{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App.Common where

import           Data.ByteString    (ByteString)
import           Data.Monoid
import           Data.Text          (Text)

import           Control.Exception  (catch)
import           Control.Monad      (forM_)

import           Network.WebSockets

data User =
       User
         { userName :: Text       -- ^ User's name
         , userConn :: Connection -- ^ User's connection
         }

instance Eq User where
  (User a _) == (User b _) = a == b

noUser :: User
noUser = User undefined undefined

instance Show User where
  show (User a _) = show a

data GameState = Running
               | Finished
  deriving (Show, Eq)

data Game =
       Game
         { gameUsers :: [User]    -- ^ User's in a game
         , gameState :: GameState -- ^ State of game
         }
  deriving (Show, Eq)

data World =
       World
         { worldLobby :: [User] -- ^ User waiting lobby
         , worldGames :: [Game] -- ^ Running Games
         }
  deriving (Show, Eq)

defaultWorld :: World
defaultWorld = World [] []

-- | Send to users who match predicate
sendTo :: (User -> Bool) -> [User] -> ByteString -> IO ()
sendTo f users msg = do
  let sendUsers = filter f users
  forM_ sendUsers $ \(User _ conn) -> safeSend conn msg

-- | Try to send data to websocket connection, handling ConnectionClosed
--   exception and ignoring it.
safeSend :: WebSocketsData a => Connection -> a -> IO ()
safeSend conn msg = sendTextData conn msg `catch` (\ConnectionClosed -> return ())

-- | Disconnect user
disconnectUser :: User -> IO ()
disconnectUser (User name conn) = do
  putStrLn $ "Disconnected " <> show name
  sendClose conn ("BYE" :: ByteString)

-- | splitAt which returns a Maybe
splitAtMaybe :: Int -> [a] -> Maybe ([a], [a])
splitAtMaybe n xs =
  if length xs < n
    then Nothing
    else Just $ splitAt n xs
