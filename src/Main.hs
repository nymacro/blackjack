{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString                      (ByteString)
import           Data.Monoid
import           Data.Text
import qualified Data.Text.Lazy                       as LazyText

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.IO.Class               (MonadIO, liftIO)

import           Network.Wai                          (Application, Middleware)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Handler.WebSockets
import           Network.Wai.Middleware.AddHeaders
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.WebSockets

import           Web.Scotty

import           App.Chat
import           App.Common
import           App.Matchmake

restApp :: TVar World -> IO Application
restApp world = scottyApp $ do
  middleware logStdout
  middleware $ staticPolicy $ addBase "static"

  get "/lobby" $ do
    state <- liftIO $ readTVarIO world
    text $ LazyText.pack $ show state

wsApp :: TVar World -> ServerApp
wsApp world pending = do
  let request = pendingRequest pending
      path = requestPath request
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
    modifyTVar world (\w@(World lobby _) -> w { worldLobby = user : lobby })
  case path of
    "/chat"  -> wsChat user world
    "/match" -> wsMatchmake user world
    _        -> return ()

main :: IO ()
main = do
  world <- newTVarIO defaultWorld
  rest <- restApp world
  run 3000 $ websocketsOr defaultConnectionOptions (wsApp world) rest
