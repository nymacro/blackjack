{-# LANGUAGE OverloadedStrings #-}
module App.Matchmake (wsMatchmake) where

import           Data.ByteString (ByteString)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.STM
import           Control.Exception (finally)
import           Control.Monad

import           Network.WebSockets

import           App.Common

import           Game.Blackjack

disconnectGame :: Game -> TVar World -> IO ()
disconnectGame game world = do
  forM_ (gameUsers game) disconnectUser
  atomically $ modifyTVar world removeGame

  where
    removeGame w = w { worldGames = filter (/= game) (worldGames w) }

findMatch :: TVar World -> STM (Maybe Game)
findMatch world = do
  w <- readTVar world
  let split = splitAtMaybe matchPlayers (worldLobby w)
      matchPlayers = 2
  case split of
    Nothing -> return Nothing
    Just (players, rest) -> do
      let game = Game players Running
      writeTVar world w { worldLobby = rest }
      return $ Just game

runGame :: Game -> TVar World -> IO ()
runGame game world = do
  putStrLn "Running Game"
  print game

  -- add game to world state
  atomically $
    modifyTVar world (\w@(World _ games) -> w { worldGames = game : games })

  -- run the game
  forever $ do
    forM_ (gameUsers game) $
      \(User _ conn) -> do
        sendTextData conn ("> YOUR TURN" :: ByteString)
        msg <- receiveData conn
        sendTo (const True) (gameUsers game) msg

-- matchmaking websockets app
wsMatchmake :: User -> TVar World -> IO ()
wsMatchmake _ world = do
  -- find match
  game <- atomically $ findMatch world
  print game

  -- run game or idle
  case game of
    Nothing -> do
      putStrLn "Waiting"
      forever $ threadDelay 1000 -- wait forever (or till disconnect)
    Just g -> finally (runGame g world) (disconnectGame g world)