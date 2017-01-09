{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module App.Common where

import           Data.ByteString               (ByteString)
import           Data.Monoid
import           Data.Text                     (Text)
import qualified Data.UUID                     as UUID (UUID)
import qualified Data.UUID.V4                  as UUID

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi
import           Control.Exception             (catch, try)
import           Control.Monad                 (forM_)

import           Network.WebSockets

-- | User representation
-- TODO: change userConn to non-raw WebSocket connection to allow forever other
-- channels to be used for interaction, for cases where a user might actually
-- be a bot. Use green threads w/unagi-chan for this?
data User = User { userName :: Text             -- ^ User's name
                 , userUuid :: UUID.UUID        -- ^ User's UUID
                 , userConn :: Maybe Connection -- ^ User's connection
                 }

instance Eq User where
  (User a uuid _) == (User b uuid' _) = a == b && uuid == uuid'

newUser :: Text             -- ^ User's name
        -> Maybe Connection -- ^ User's connection
        -> IO User
newUser name conn = do
  uuid <- UUID.nextRandom
  return $ User name uuid conn

noUser :: User
noUser = User undefined undefined undefined

instance Show User where
  show (User a _ _) = show a

data Game =
       Game
         { gameUsers      :: [User]                     -- ^ User's in a game
         , gameInChan     :: InChan (User, ByteString)  -- ^ Input channel (broadcast)
         , gameOutChan    :: OutChan (User, ByteString) -- ^ Output channel (client output)
         , gameDisconnect :: MVar ()                     -- ^ Disconnect sync
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
sendTo :: WebSocketsData a => (User -> Bool) -> [User] -> a -> IO ()
sendTo f users msg = do
  let sendUsers = filter f users
  forM_ sendUsers $ \(User _ _ conn) -> case conn of
    Nothing -> return ()
    Just c  -> safeSend c msg

-- | Try to send data to websocket connection, handling ConnectionClosed
--   exception and ignoring it.
safeSend :: WebSocketsData a => Connection -> a -> IO ()
safeSend conn msg = sendTextData conn msg `catch` (\ConnectionClosed -> return ())

-- | Maybe send data to a connection
safeSendM :: WebSocketsData d => Maybe Connection -> d -> IO ()
safeSendM conn d = case conn of
  Just c  -> safeSend c d
  Nothing -> return ()

-- | Disconnect user
disconnectUser :: User -> IO ()
disconnectUser (User name _ conn) = do
  putStrLn $ "Disconnected " <> show name
  case conn of
    Nothing -> putStrLn "No connection associated with user"
    Just c  -> sendClose c ("" :: ByteString)
  putStrLn $ "Done disconnecting " <> show name

-- | splitAt which returns a Maybe
splitAtMaybe :: Int -> [a] -> Maybe ([a], [a])
splitAtMaybe n xs =
  if length xs < n
    then Nothing
    else Just $ splitAt n xs
