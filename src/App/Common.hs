{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App.Common where

import           Data.ByteString    (ByteString)
import           Data.Monoid
import           Data.Text          (Text)
import           Data.UUID          (UUID)

import           Control.Concurrent
import           Control.Exception  (SomeException, catch, try)
import           Control.Monad      (forM_)

import           Network.WebSockets

data User =
       User
         { userName :: Text       -- ^ User's name
         , userUuid :: UUID       -- ^ User's UUID
         , userConn :: Connection -- ^ User's connection
         }

instance Eq User where
  (User a uuid _) == (User b uuid' _) = a == b && uuid == uuid'

noUser :: User
noUser = User undefined undefined undefined

instance Show User where
  show (User a _ _) = show a

data Game =
       Game
         { gameUsers :: [User]    -- ^ User's in a game
         }
  deriving (Eq)

instance Show Game where
  show = show . gameUsers

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
  forM_ sendUsers $ \(User _ _ conn) -> safeSend conn msg

-- | Try to send data to websocket connection, handling ConnectionClosed
--   exception and ignoring it.
safeSend :: WebSocketsData a => Connection -> a -> IO ()
safeSend conn msg = sendTextData conn msg `catch` (\ConnectionClosed -> return ())

-- | Disconnect user
disconnectUser :: User -> IO ()
disconnectUser (User name _ conn) = do
  putStrLn $ "Disconnected " <> show name
  sendClose conn ("" :: ByteString)
  putStrLn $ "Done disconnecting " <> show name

-- | splitAt which returns a Maybe
splitAtMaybe :: Int -> [a] -> Maybe ([a], [a])
splitAtMaybe n xs =
  if length xs < n
    then Nothing
    else Just $ splitAt n xs
