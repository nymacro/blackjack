{-# LANGUAGE OverloadedStrings #-}
module App.Common where

import           Data.ByteString
import           Data.Monoid
import           Data.Text

import           Control.Monad      (forM_)

import           Network.WebSockets

data User = User { userName :: Text
                 , userConn :: Connection }
-- this instance kinda dodgy...
instance Eq User where
  (User a _) == (User b _) = a == b

instance Show User where
  show (User a _) = show a

data GameState = Running
               | Finished
               deriving (Show, Eq)

data Game = Game { gameUsers :: [User]
                 , gameState :: GameState }
          deriving (Show, Eq)

data World = World { worldLobby :: [User]
                   , worldGames :: [Game] }
           deriving (Show, Eq)

defaultWorld :: World
defaultWorld = World [] []

sendTo :: (User -> Bool) -> [User] -> ByteString -> IO ()
sendTo f users msg = do
  let sendUsers = Prelude.filter f users
  forM_ sendUsers $ \(User _ conn) -> sendTextData conn msg

disconnectUser :: User -> IO ()
disconnectUser (User name conn) = do
  print $ name <> " disconnected"
  sendClose conn ("" :: ByteString)

splitAtMaybe :: Int -> [a] -> Maybe ([a], [a])
splitAtMaybe n xs = if Prelude.length xs < n
                      then Nothing
                      else Just $ Prelude.splitAt n xs
