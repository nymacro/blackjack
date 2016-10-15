{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Control.Monad
import           Data.ByteString                      (ByteString)
import           Data.Monoid
import           Data.Text
import qualified Data.Text.Lazy                       as LazyText

import           Control.Concurrent                   (threadDelay)
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad.IO.Class               (MonadIO, liftIO)

import           Network.Wai                          (Application, Middleware)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Handler.WebSockets
import           Network.Wai.Middleware.AddHeaders
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.WebSockets
import           Web.Scotty

import           Control.Concurrent.Chan.Unagi

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

restApp :: TVar World -> IO Application
restApp world = scottyApp $ do
  middleware logStdout
  middleware $ staticPolicy $ addBase "static"

  get "/lobby" $ do
    state <- liftIO $ readTVarIO world
    text $ LazyText.pack $ show state

disconnectUser :: User -> IO ()
disconnectUser user@User{..} = do
  print $ userName <> " disconnected"
  sendClose userConn ("" :: ByteString)

splitAtMaybe n xs = if Prelude.length xs < n
                      then Nothing
                      else Just $ Prelude.splitAt n xs

findMatch :: TVar World -> STM (Maybe Game)
findMatch world = do
  w@World{..} <- readTVar world
  let split = splitAtMaybe matchPlayers worldLobby
      matchPlayers = 2
  case split of
    Nothing -> return Nothing
    Just (players, rest) -> do
      let game = Game players Running
      writeTVar world w{ worldLobby = rest }
      return $ Just game

disconnectGame :: Game -> TVar World -> IO ()
disconnectGame game@Game{..} world = do
  forM_ gameUsers disconnectUser
  atomically $ modifyTVar world (\w@World{..} -> w { worldGames = Prelude.filter (/= game) worldGames })

sendTo :: (User -> Bool) -> [User] -> ByteString -> IO ()
sendTo pred users msg = do
  let sendUsers = Prelude.filter pred users
  forM_ sendUsers $ \User{..} -> sendTextData userConn msg

runGame :: Game -> TVar World -> IO ()
runGame game@Game{..} world = do
  putStrLn "Running Game"
  print game

  -- add game to world state
  atomically $ modifyTVar world (\w@World{..} -> w { worldGames = game : worldGames })

  -- run the game
  forever $ do
    forM_ gameUsers $ \user@User{..} -> do
      sendTextData userConn ("> YOUR TURN" :: ByteString)
      msg <- receiveData userConn
      -- sendTo (/= user) gameUsers msg
      sendTo (const True) gameUsers msg

wsApp :: TVar World -> ServerApp
wsApp world pending = do
  let request = pendingRequest pending
      path    = requestPath request

  print request
  print path

  conn <- acceptRequest pending
  name <- receiveData conn
  let user = User name conn

  liftIO $ putStrLn $ show user <> " connected"

  -- keep connection alive
  forkPingThread conn 10

  -- add user to lobby
  atomically $
    modifyTVar world (\w@World{..} -> w { worldLobby = user : worldLobby })

  case path of
    "/chat"  -> wsChat user world
    "/match" -> wsMatchmake user world
    _ -> return ()

wsChat :: User -> TVar World -> IO ()
wsChat user world = do
  forever $ do
    msg <- receiveData (userConn user)
    w <- readTVarIO world
    sendTo (const True) (worldLobby w) msg

wsMatchmake :: User -> TVar World -> IO ()
wsMatchmake user world = do
  -- find match
  game <- atomically $
    findMatch world

  print game

  -- run game or idle
  case game of
    Nothing -> do
      putStrLn "Waiting"
      forever $ threadDelay 1000 -- wait forever!
    Just g  -> finally (runGame g world) (disconnectGame g world)

main :: IO ()
main = do
  world <- newTVarIO defaultWorld
  rest  <- restApp world
  run 3000 $ websocketsOr defaultConnectionOptions (wsApp world) rest
