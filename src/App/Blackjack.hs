{-# LANGUAGE OverloadedStrings #-}
module App.Blackjack (runGame) where

import           Data.ByteString               (ByteString)
import           Data.ByteString.Char8         (pack, unpack)
import           Data.List                     (find)
import           Data.Monoid
import           Data.Text.Encoding            (encodeUtf8)

import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.STM
import           Control.Monad

import           Network.WebSockets

import           App.Common

import           Game.Blackjack

data BlackjackUser = BlackjackUser { bjUser  :: User
                                   , sitting :: Bool
                                   , bjCards :: Hand }
                   deriving (Show)

data BlackjackGame = BlackjackGame { bjUsers :: [BlackjackUser]
                                   , bjDeck  :: Deck }
                   deriving (Show)

toBlackjackUser :: User -> BlackjackUser
toBlackjackUser u = BlackjackUser u False []

runGame :: (InChan (User, ByteString), OutChan (User, ByteString)) -> Game -> IO ()
runGame (_, oc) game = do
  putStrLn "Running Blackjack Game"
  print game

  -- set up blackjack game
  let players = toBlackjackUser <$> (gameUsers game)
  bj <- newTVarIO =<< (return . (BlackjackGame players)) =<< shuffleDeck defaultDeck

  -- shared functions
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

  -- deal all players 2 cards
  let dealAll users deck = dealAll' users deck []
      dealAll' [] deck dealt = (dealt, deck)
      dealAll' (user : users) deck dealt =
        let Just (card1, deck')  = tap deck
            Just (card2, deck'') = tap deck'
        in dealAll' users deck'' (user { bjCards = card1 : card2 : bjCards user } : dealt)
  atomically $ do
    modifyTVar bj $ \(BlackjackGame users deck) ->
      let (users', deck') = dealAll users deck
      in BlackjackGame users' deck'

  -- tell users which cards they were dealt
  u' <- bjUsers <$> readTVarIO bj
  forM_ u' $ \(BlackjackUser user@(User _ conn) _ cards) -> do
    -- other's cards
    forM_ (filter (\(BlackjackUser u _ _) -> u /= user) u') $ \(BlackjackUser (User n _) _ cards) -> do
      forM_ cards $ \c -> safeSend conn $ encodeUtf8 n <> " " <> (pack $ show c)
    -- their cards
    forM_ cards $ \c -> safeSend conn $ "YOU " <> (pack $ show c)

  -- main game loop
  let loop = do
        (user@(User name conn), d) <- readChan oc
        case d of
          "DISCONNECT" -> atomically $ sitUserSTM bj user
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
                safeSend conn $ "YOU " <> cardText
                -- tell the other users what they got
                users <- bjUsers <$> readTVarIO bj
                forM_ users $ \(BlackjackUser u@(User _ c) _ _) ->
                  if u /= user
                    then safeSend c $ encodeUtf8 name <> " " <> cardText
                    else return ()
          _ -> return ()

        -- check whether all users are sitting
        finished <- all sitting <$> bjUsers <$> readTVarIO bj

        -- pick a winner if everyone is sitting
        let final = do
              putStrLn "Game finished"
              users <- bjUsers <$> readTVarIO bj
              print users
              let hands  = fmap bjCards users
                  winner = pickWinner hands

              putStrLn $ "Winner" <> show winner

              -- broadcast winner
              sendTo (const True) (gameUsers game) ("WINNER: " <> pack (show winner))
              return ()

        if not finished
          then loop
          else final
  loop
