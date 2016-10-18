{-# LANGUAGE OverloadedStrings #-}
module App.Chat (wsChat) where

import           Control.Concurrent.STM
import           Control.Monad

import           Network.WebSockets

import           Data.Monoid
import           Data.Text.Encoding     (encodeUtf8)

import           App.Common

-- chat websockets app
wsChat :: User -> TVar World -> IO ()
wsChat (User name conn) world = do
    forever $ do
        msg <- receiveData conn
        w <- readTVarIO world
        sendTo (const True) (worldLobby w) (encodeUtf8 name <> ":" <> msg)
