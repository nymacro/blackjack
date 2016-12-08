{-# LANGUAGE OverloadedStrings #-}
module App.Matchmake (wsMatchmake, disconnectGame) where

import           Data.ByteString               (ByteString)
import           Data.Monoid

import           Control.Concurrent            (ThreadId, forkFinally, forkIO,
                                                threadDelay, throwTo)
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
  forM_ (gameUsers game) disconnectUser
  putStrLn "Done disconnecting game"

  where
    removeGame w = w { worldGames = filter (/= game) (worldGames w) }

-- | Find match
findMatch :: Int              -- ^ Number of players required for a game
          -> TVar World       -- ^ World state
          -> STM (Maybe Game) -- ^ Found game
findMatch numPlayers world = do
  w <- readTVar world
  let split = splitAtMaybe numPlayers (worldLobby w)
  case split of
    Nothing -> return Nothing
    Just (players, rest) -> do
      let game = Game players
      writeTVar world w { worldLobby = rest }
      return $ Just game

-- | Generic matchmaking websockets app
wsMatchmake :: User                        -- ^ Joining user
            -> TVar World                  -- ^ World state
            -> Int                         -- ^ Player count
            -> ((InChan (User, ByteString), OutChan (User, ByteString)) -> Game -> IO ()) -- ^ Game loop
            -> IO ()
wsMatchmake _ world numPlayers runGame = do
  -- find match
  atomically $ findMatch numPlayers world

  -- run game or idle
  case game of
    Nothing -> do
      putStrLn "Waiting for game..."
      forever $ threadDelay 10000
    Just g -> do
      addGame g world

      (bcast, oc) <- newChan

      -- create read thread for users
      readThreads <- forM (gameUsers g) $ \user@(User _ _ conn) -> do
        async $ do
          -- make sure we tell the game when a user disconnects
          let fin = writeChan bcast (user, "DISCONNECT") >> putStrLn ("Client left " <> show user)
          flip finally fin $
            forever $ do
              d <- receiveData conn
              writeChan bcast (user, d)

      -- launch main game thread
      main <- async (runGame (bcast, oc) g)
      wait main

      putStrLn "Disconnecting game"
      disconnectGame g world

      putStrLn "Killing threads"
      mapM_ cancel readThreads

      putStrLn "Finished"

