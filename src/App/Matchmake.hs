{-# LANGUAGE OverloadedStrings #-}
module App.Matchmake (wsMatchmake) where

import           Data.ByteString               (ByteString)
import           Data.Monoid

import           Control.Concurrent            (ThreadId, forkFinally, forkIO,
                                                threadDelay, throwTo)
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Exception             (finally)
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
  print game

  -- run game or idle
  case game of
    Nothing -> do
      putStrLn "Waiting"
      forever $ threadDelay 10000
    Just g -> do
      addGame g world

      (bcast, oc) <- newChan

      -- create read channel for users
      readts <- forM (gameUsers g) $ \user@(User name conn) -> do
        waitThread $ do
          forever $ do
            d <- receiveData conn
            writeChan bcast (user, d)

      -- launch main game thread
      (_, m) <- waitThread (runGame (bcast, oc) g)
      readMVar m

      mapM_ (\(t, m) -> throwTo t ThreadKilled >> readMVar m) readts

      disconnectGame g world
