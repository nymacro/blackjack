{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module App.Blackjack (runGame) where

import qualified Data.ByteString.Lazy          as Lazy
import           Data.List                     (find)
import           Data.Monoid
import qualified Data.Text                     as Text
import qualified Data.UUID                     as UUID

import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.STM
import           Control.Monad

import           App.Common

import           Game.Blackjack

import           Data.Aeson
import           GHC.Generics

data BlackjackUser = BlackjackUser { bjUser  :: User
                                   , sitting :: Bool
                                   , bjCards :: Hand
                                   }
                   deriving (Show, Generic)

data BlackjackGame = BlackjackGame { bjUsers :: [BlackjackUser]
                                   , bjDeck  :: Deck
                                   }
                   deriving (Show, Generic)


instance ToJSON User where
  toJSON (User  username uuid _) = object [ "name" .= String username
                                          , "uuid" .= String (UUID.toText uuid) ]
  toEncoding (User username uuid _) = pairs ("name" .= username <>
                                             "uuid" .= UUID.toText uuid)

instance ToJSON BlackjackUser where
  toJSON (BlackjackUser u _ _) = toJSON u

instance ToJSON Rank where
  toJSON rank = String (Text.pack $ show rank)

instance ToJSON Suit where
  toJSON suit = String (Text.pack $ show suit)

instance ToJSON Card where
  toJSON (Card rank suit) = object [ "rank" .= rank
                                   , "suit" .= suit ]

data DealMessage = DealMessage { user :: User
                               , card :: Card
                               }
                 deriving (Show, Generic)

instance ToJSON DealMessage where
  toEncoding = genericToEncoding defaultOptions

data DoneMessage = DoneMessage [(Int, BlackjackUser)]
                 deriving (Show, Generic)

instance ToJSON DoneMessage where
  toEncoding = genericToEncoding defaultOptions

data BlackjackMessage a = BlackjackMessage Text.Text a
                        deriving (Show, Generic)
instance ToJSON a => ToJSON (BlackjackMessage a) where
  toJSON (BlackjackMessage typ msg) = object [ "type" .= typ
                                             , "data" .= msg ]

-- | Helper function for encoding a message to send to a client
message :: ToJSON m
        => Text.Text       -- ^ Message type
        -> m               -- ^ Message
        -> Lazy.ByteString -- ^ JSON encoded message
message typ = encode . (BlackjackMessage typ)

toBlackjackUser :: User -> BlackjackUser
toBlackjackUser u = BlackjackUser u False []

newDealer = BlackjackUser <$> newUser "Dealer" Nothing <*> return True <*> return []

-- | Main game loop for running Blackjack game
runGame :: Game -> IO ()
runGame game@(Game _ bcast oc) = do
  putStrLn "Running Blackjack Game"
  print game

  dealer <- newDealer

  -- set up blackjack game
  let players = toBlackjackUser <$> (gameUsers game)
  bj <- newTVarIO =<< (return . (BlackjackGame players)) =<< shuffleDeck (join $ replicate 6 defaultDeck)

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
        let (cards, deck') = deal2 deck
        in dealAll' users deck' (user { bjCards = cards <> bjCards user } : dealt)
      deal2 deck =
        let Just (card1, deck')  = tap deck
            Just (card2, deck'') = tap deck'
        in ([card1, card2], deck'')

  atomically $ do
    modifyTVar bj $ \(BlackjackGame users deck) ->
      let (users', deck')   = dealAll users deck
          Just (dealerCard, deck'') = tap deck'
      in BlackjackGame (dealer { bjCards = [dealerCard] } : users') deck''

  -- tell users which cards they were dealt
  u' <- bjUsers <$> readTVarIO bj
  forM_ u' $ \(BlackjackUser user@(User _ _ conn) _ cards) -> do
    -- other's cards
    forM_ (filter (\(BlackjackUser u _ _) -> u /= user) u') $ \(BlackjackUser other@(User n _ _) _ cards) -> do
      forM_ cards $ \c -> safeSendM conn $ message "deal" (DealMessage other c)
    -- their cards
    forM_ cards $ \c -> safeSendM conn $ message "deal" (DealMessage user c)

  -- main game loop
  let loop = do
        (user@(User _ _ conn), d) <- readChan oc
        case d of
          "DISCONNECT" -> atomically $ sitUserSTM bj user
          "sit" -> atomically $ sitUserSTM bj user
          "tap" -> do
            mu <- findUser bj user
            case mu of
              Nothing -> return ()
              Just (BlackjackUser _ True _) -> return ()
              Just (BlackjackUser u _ hand) -> do
                card <- atomically $ do
                  (BlackjackGame users deck) <- readTVar bj
                  let Just (c, deck') = tap deck
                      giveCard = apply user (\x -> x { bjCards = c : bjCards x })
                  writeTVar bj (BlackjackGame (fmap giveCard users) deck')
                  -- bust 'em
                  when (bust (c : hand)) $ sitUserSTM bj user
                  return c
                -- tell user what card they got
                safeSendM conn $ message "deal" (DealMessage user card)
                -- tell the other users what they got
                users <- bjUsers <$> readTVarIO bj
                forM_ users $ \(BlackjackUser u@(User _ _ c) _ _) ->
                  if u /= user
                    then safeSendM c $ message "deal" (DealMessage user card)
                    else return ()
          _ -> return ()

        -- check whether all users are sitting
        finished <- all sitting <$> bjUsers <$> readTVarIO bj

        -- pick a winner if everyone is sitting (or has busted)
        let final = do
              -- play the dealer
              Just d <- findUser bj (bjUser dealer)
              deck <- bjDeck <$> readTVarIO bj
              let newHand = playDealer (bjCards d) deck

              atomically $ modifyTVar bj $
                \s -> modUser s (bjUser dealer) (\x -> x { bjCards = newHand })

              -- Show everyone the dealer's hand
              users <- bjUsers <$> readTVarIO bj
              forM_ users $ \(BlackjackUser u@(User _ _ c) _ _) ->
                forM_ (take (length newHand - 1) newHand) $ \card ->
                  safeSendM c $ message "deal" (DealMessage (bjUser dealer) card)

              -- find the winner
              putStrLn "Game finished"
              users <- bjUsers <$> readTVarIO bj
              print users
              let winner = pickWinner_ bjCards users

              putStrLn $ "Winner " <> show winner

              -- broadcast winner[s]
              sendTo (const True) (gameUsers game) $ message "done" (DoneMessage winner)
              return ()

        if not finished
          then loop
          else final
  loop

-- | Play the dealers hand
playDealer :: Hand -> Deck -> Hand
playDealer hand deck =
  case bestValue hand of
     Nothing -> hand
     Just n  -> if n > 17
                 then hand
                 else let Just (card, deck') = tap deck
                      in playDealer (card : hand) deck'
