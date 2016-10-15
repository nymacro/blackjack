module App.Chat (wsChat) where

import           Control.Concurrent.STM
import           Control.Monad

import           Network.WebSockets

import           App.Common

-- chat websockets app
wsChat :: User -> TVar World -> IO ()
wsChat (User _ conn) world = do
  forever $ do
    msg <- receiveData conn
    w <- readTVarIO world
    sendTo (const True) (worldLobby w) msg
