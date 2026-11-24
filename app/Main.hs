{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson                           (decode)
import qualified Data.Text.Lazy                       as LazyText

import           Control.Concurrent.STM

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
  let req  = pendingRequest pending
      path = requestPath req
  print req
  print path

  conn <- acceptRequest pending

  -- keep the connection alive while handling the session
  withPingThread conn 10 (return ()) $ do
    -- receive login config
    jsonOpts <- receiveData conn

    case (decode jsonOpts :: Maybe LoginConfig) of
      Nothing -> do
        putStrLn $ show jsonOpts
        return ()
      Just (LoginConfig loginName opts) -> do
        user <- newUser loginName (Just conn)
        putStrLn $ show user <> " connected"

        putStrLn $ show opts

        -- add user to lobby
        atomically $
          modifyTVar world (\w@(World waiting _) -> w { worldLobby = user : waiting })

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
