{-# LANGUAGE OverloadedStrings #-}
module App.Matchmake (wsMatchmake, disconnectGame) where

import           Data.ByteString               (ByteString)
import           Data.Monoid

import           Control.Concurrent            (ThreadId, forkFinally, forkIO,
                                                threadDelay, throwTo)
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception             (catch, finally, handle)
import           Control.Exception.Base        (AsyncException (ThreadKilled))
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
  forM_ (gameUsers game) disconnectUser
  atomically $ modifyTVar world removeGame

  where
    removeGame w = w { worldGames = filter (/= game) (worldGames w) }

findMatch :: Int -> TVar World -> STM (Maybe Game)
findMatch numPlayers world = do
  w <- readTVar world
  let split = splitAtMaybe numPlayers (worldLobby w)
  case split of
    Nothing -> return Nothing
    Just (players, rest) -> do
      let game = Game players Running
      writeTVar world w { worldLobby = rest }
      return $ Just game

waitThread :: IO () -> IO (ThreadId, MVar ())
waitThread io = do
  mvar <- newEmptyMVar
  t <- forkFinally io (\_ -> putMVar mvar ())
  return (t, mvar)

-- matchmaking websockets app
wsMatchmake :: User                        -- ^ Joining user
            -> TVar World                  -- ^ World state
            -> Int                         -- ^ Player count
            -> ((InChan (User, ByteString), OutChan (User, ByteString)) -> Game -> IO ()) -- ^ Game loop
            -> IO ()
wsMatchmake _ world numPlayers runGame = do
  -- find match
  game <- atomically $ findMatch numPlayers world

  -- run game or idle
  case game of
    Nothing -> do
      putStrLn "Waiting to find game..."
      forever $ threadDelay 10000
    Just g -> do
      addGame g world

      (bcast, oc) <- newChan

      -- create read thread for users
      readts <- forM (gameUsers g) $ \user@(User _ conn) -> do
        waitThread $ do
          -- make sure we tell the game when a user disconnects
          let fin = writeChan bcast (user, "DISCONNECT") >> putStrLn "Client disconnected"
          flip finally fin $
            forever $ do
              d <- receiveData conn
              writeChan bcast (user, d)

      -- launch main game thread
      (_, m) <- waitThread (runGame (bcast, oc) g)
      readMVar m

      disconnectGame g world
      mapM_ (\(t, m) -> throwTo t ThreadKilled >> readMVar m) readts

