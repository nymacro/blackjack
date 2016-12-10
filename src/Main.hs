{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson                           (decode)
import qualified Data.Map                             as Map
import           Data.Monoid
import qualified Data.Text.Lazy                       as LazyText
import qualified Data.UUID.V4                         as UUID

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
      uuid <- UUID.nextRandom
      let user = User name uuid conn
      putStrLn $ show user <> " connected"

      putStrLn $ show opts

      -- keep connection alive
      forkPingThread conn 10

      -- add user to lobby
      atomically $
        modifyTVar world (\w@(World lobby _) -> w { worldLobby = user : lobby })

      -- see if options have players
      let numPlayers = Map.lookup "players" <$> opts

      case path of
        "/blackjack" -> wsMatchmake user lobby world 3 Blackjack.runGame
        _            -> return ()

main :: IO ()
main = do
  world <- newTVarIO defaultWorld
  rest  <- restApp world
  lobby <- newLobby
  run 3000 $ websocketsOr defaultConnectionOptions (wsApp lobby world) rest
