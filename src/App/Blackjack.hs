{-# LANGUAGE OverloadedStrings #-}
module App.Blackjack (runGame) where

import           Data.ByteString               (ByteString)
import           Data.Monoid

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.STM
import           Control.Exception             (finally)
import           Control.Monad

import           Network.WebSockets

import           App.Common

import           Game.Blackjack

data BlackjackUser = BlackjackUser { bjUser  :: User
                                   , sitting :: Bool
                                   , bjCards :: Hand }

data BlackjackGame = BlackjackGame { bjUsers :: [BlackjackUser]
                                   , bjDeck  :: Deck }

toBlackjackUser :: User -> BlackjackUser
toBlackjackUser u = BlackjackUser u False []

runGame :: (InChan (User, ByteString), OutChan (User, ByteString)) -> Game -> IO ()
runGame (ic, oc) game = do
  putStrLn "Running Blackjack Game"
  print game

  -- set up blackjack game
  deck <- shuffleDeck defaultDeck

  let players = fmap toBlackjackUser (gameUsers game)
  bj <- newTVarIO $ BlackjackGame players deck

  -- main thread
  let loop = do
        finished <- atomically $ do
          (BlackjackGame users _) <- readTVar bj
          return $ any (not . sitting) users
        let final = do
              users <- bjUsers <$> readTVarIO bj
              let hands = fmap bjCards users
                  winner = pickWinner hands
              putStrLn $ "WINNER: " <> show winner
              return ()
        if not finished
          then loop
          else final
  loop
