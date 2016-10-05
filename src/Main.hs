{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Monad
import           Data.ByteString                      (ByteString)
import           Data.Monoid
import           Data.Text

import           Control.Concurrent                   (threadDelay)
import           Control.Concurrent.STM
import           Control.Exception

import           Network.Wai                          (Application, Middleware)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Handler.WebSockets
import           Network.Wai.Middleware.AddHeaders
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.WebSockets
import           Web.Scotty

data User = User { userName :: Text
                 , userConn :: Connection }
-- this instance kinda dodgy...
instance Eq User where
  (User a _) == (User b _) = a == b

data GameState = Running
               | Finished

data Game = Game { gameUsers :: [User]
                 , gameState :: GameState }

data World = World { worldLobby :: [User]
                   , worldGames :: [Game] }

defaultWorld :: World
defaultWorld = World [] []

restApp :: IO Application
restApp = scottyApp $ do
  middleware logStdout
  middleware $ staticPolicy $ addBase "static"

disconnectUser :: User -> IO ()
disconnectUser user@User{..} = do
  print $ userName <> " disconnected"
  sendClose userConn ("" :: ByteString)

splitAtMaybe n xs = if Prelude.length xs < n
                    then Just $ Prelude.splitAt n xs
                    else Nothing

findMatch :: TVar World -> STM (Maybe Game)
findMatch world = do
  w@World{..} <- readTVar world
  let split = splitAtMaybe 2 worldLobby
  case split of
    Nothing -> return Nothing
    Just (players, rest) -> do
      let game = Game players Running
      writeTVar world w{ worldLobby = rest }
      return $ Just game

disconnectGame :: Game -> IO ()
disconnectGame Game{..} = forM_ gameUsers disconnectUser

sendTo :: (User -> Bool) -> [User] -> ByteString -> IO ()
sendTo pred users msg = do
  let sendUsers = Prelude.filter pred users
  forM_ sendUsers $ \User{..} -> sendTextData userConn msg

runGame :: Game -> IO ()
runGame game@Game{..} = do
  forM_ gameUsers $ \user@User{..} -> do
    msg <- receiveData userConn
    sendTo (/= user) gameUsers msg
  putStrLn "running game"

wsApp :: TVar World -> ServerApp
wsApp world pending = do
  print $ pendingRequest pending

  conn <- acceptRequest pending
  name <- receiveData conn
  let user = User name conn

  -- add user to lobby
  atomically $
    modifyTVar world (\w@World{..} -> w { worldLobby = user : worldLobby })

  -- find match
  game <- atomically $
    findMatch world

  -- run game or idle
  case game of
    Nothing -> forever $ threadDelay 1000 -- wait forever!
    Just g  -> finally (runGame g) (disconnectUser user)

main :: IO ()
main = do
  world <- newTVarIO defaultWorld
  rest  <- restApp
  run 3000 $ websocketsOr defaultConnectionOptions (wsApp world) rest
