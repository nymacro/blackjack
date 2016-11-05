{-# LANGUAGE OverloadedStrings #-}
module App.Blackjack (runGame) where

import           Data.ByteString               (ByteString)
import           Data.ByteString.Char8         (pack)
import           Data.List                     (find)
import           Data.Monoid
import           Data.Text.Encoding            (encodeUtf8)

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.STM
import           Control.Exception             (finally)
import           Control.Monad

import           Network.WebSockets

import           App.Common
import           App.Matchmake                 (disconnectGame)

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

  -- deal all players 2 cards
  -- TODO

  let apply u f bu = if bjUser bu == u
                       then f bu
                       else bu
      modUser b@(BlackjackGame users _) u f =
        b { bjUsers = fmap (apply u f) users }
      -- atomically find user
      findUser g u = do
        find (\x -> u == bjUser x) <$> (bjUsers <$> readTVarIO g)
      -- sit a user in a game
      sitUser g u = modUser g u (\x -> x { sitting = True })
      -- atomically sit user
      sitUserSTM g u = modifyTVar g $ \g' -> sitUser g' u
      -- main game loop
      loop = do
        (user@(User name conn), d) <- readChan oc
        _ <- case d of
          "sit" -> atomically $ sitUserSTM bj user
          "tap" -> do
            mu <- findUser bj user
            case mu of
              Nothing -> return ()
              Just (BlackjackUser _ True _) -> return ()
              Just (BlackjackUser _ _ hand) -> do
                card <- atomically $ do
                  (BlackjackGame users deck) <- readTVar bj
                  let Just (c, deck') = tap deck
                      giveCard = apply user (\x -> x { bjCards = c : bjCards x })
                  writeTVar bj (BlackjackGame (fmap giveCard users) deck')
                  -- bust 'em
                  when (bust (c : hand)) $ sitUserSTM bj user
                  return c
                let cardText = (pack $ show card)
                -- tell user what card they got
                sendTextData conn $ "YOU " <> cardText
                -- tell the other users what they got
                users <- bjUsers <$> readTVarIO bj
                forM_ users $ \(BlackjackUser u@(User n c) _ _) ->
                  if u /= user
                    then sendTextData c $ encodeUtf8 name <> " " <> cardText
                    else return ()
          _ -> return ()

        -- check whether all users are sitting
        finished <- atomically $ do
          (BlackjackGame users _) <- readTVar bj
          return $ all sitting users

        -- pick a winner if everyone is sitting
        let final = do
              users <- bjUsers <$> readTVarIO bj
              let hands  = fmap bjCards users
                  winner = pickWinner hands

              -- broadcast winner
              sendTo (const True) (gameUsers game) ("WINNER: " <> pack (show winner))
              return ()

        if not finished
          then loop
          else final
  loop
