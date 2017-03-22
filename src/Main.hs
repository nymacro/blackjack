{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson                           (decode)
import qualified Data.Map                             as Map
import           Data.Monoid
import qualified Data.Text.Lazy                       as LazyText

import           Control.Concurrent.STM
import           Control.Monad.IO.Class               (liftIO)

import           Network.Wai                          (Application)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Handler.WebSockets
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Network.WebSockets

import           Web.Scotty

import           App.Common
import           App.Config
import           App.Matchmake

import qualified App.Blackjack                        as Blackjack

-- | HTTP handler
restApp ::  TVar World -> IO Application
restApp world = scottyApp $ do
  middleware logStdout
  middleware $ staticPolicy $ addBase "static"

  get "/" $ do
    file "static/blackjack.html"

  get "/lobby" $ do
    state <- liftIO $ readTVarIO world
    text $ LazyText.pack $ show state

-- | Websocket handler
wsApp :: Lobby -> TVar World -> ServerApp
wsApp lobby world pending = do
  let request = pendingRequest pending
      path    = requestPath request
  print request
  print path

  conn <- acceptRequest pending

  -- receive login config
  jsonOpts <- receiveData conn

  case (decode jsonOpts :: Maybe LoginConfig) of
    Nothing -> do
      putStrLn $ show jsonOpts
      return ()
    Just (LoginConfig name opts) -> do
      user <- newUser name (Just conn)
      putStrLn $ show user <> " connected"

      putStrLn $ show opts

      -- keep connection alive
      forkPingThread conn 10

      -- add user to lobby
      atomically $
        modifyTVar world (\w@(World lobby _) -> w { worldLobby = user : lobby })

      -- maximum number of players in a game
      let numPlayers = 3

      case path of
        "/blackjack" -> wsMatchmake user lobby world numPlayers Blackjack.runGame
        _            -> return ()

main :: IO ()
main = do
  world <- newTVarIO defaultWorld
  rest  <- restApp world
  lobby <- newLobby
  run 3000 $ websocketsOr defaultConnectionOptions (wsApp lobby world) rest
