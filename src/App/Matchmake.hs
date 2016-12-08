{-# LANGUAGE OverloadedStrings #-}
module App.Matchmake (wsMatchmake, disconnectGame) where

import           Data.ByteString               (ByteString)
import           Data.Monoid

import           Control.Concurrent            hiding (newChan, writeChan)
import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.STM
import           Control.Exception             (catch, finally, handle)
import           Control.Monad

import           Network.WebSockets

import           App.Common

-- | Add game to world
addGame :: Game -> TVar World -> IO ()
addGame g world = do
  atomically $
    modifyTVar world (\w@(World _ games) -> w { worldGames = g : games })

-- | Disconnect game
disconnectGame :: Game -> TVar World -> IO ()
disconnectGame game world = do
  atomically $ modifyTVar world removeGame

  where
    removeGame w = w { worldGames = filter (/= game) (worldGames w) }

-- | Find game
findGame :: Int              -- ^ Number of players required for a game
         -> Lobby            -- ^ Lobby state
         -> TVar World       -- ^ World state
         -> (Game -> IO ())    -- ^ Game loop
         -> IO (MVar (Async (), Game))   -- ^ Found game
findGame numPlayers (Lobby m) world runGame = do
  players <- atomically $ do
    w <- readTVar world
    let split = splitAtMaybe numPlayers (worldLobby w)
    case split of
      Nothing -> return Nothing
      Just (players, rest) -> do
        writeTVar world w { worldLobby = rest }
        return $ Just players

  case players of
    Nothing -> return ()
    Just p  -> do
      -- set up game and run
      (bcast, oc) <- newChan
      let g = Game p bcast oc

      -- main game thread
      a <- async (runGame g)
      putMVar m (a, g)

      -- register game
      addGame g world
  return m

-- | Generic matchmaking websockets app
wsMatchmake :: User                        -- ^ Joining user
            -> Lobby                       -- ^ Lobby state
            -> TVar World                  -- ^ World state
            -> Int                         -- ^ Player count
            -> (Game -> IO ()) -- ^ Game loop
            -> IO ()
wsMatchmake user@(User _ _ conn) lobby world numPlayers runGame = do
  m <- findGame numPlayers lobby world runGame
  (main, g@(Game _ bcast _)) <- readMVar m

  -- create client thread
  a <- async $ do
    -- make sure we tell the game when a user disconnects
    let fin = writeChan bcast (user, "DISCONNECT") >> putStrLn ("Client left " <> show user)
    flip finally fin $
      forever $ do
        d <- receiveData conn
        writeChan bcast (user, d)

  -- wait for game to finish and clean up
  wait main
  disconnectGame g world
  cancel a
