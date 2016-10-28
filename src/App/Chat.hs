{-# LANGUAGE OverloadedStrings #-}
module App.Chat (wsChat, wsGroupChat) where

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.STM
import           Control.Monad

import           Network.WebSockets

import           Data.ByteString               (ByteString)
import           Data.Monoid
import           Data.Text.Encoding            (encodeUtf8)

import           App.Common

-- chat websockets app
wsChat :: User -> TVar World -> IO ()
wsChat (User name conn) world = do
  forever $ do
    msg <- receiveData conn
    w <- readTVarIO world
    sendTo (const True) (worldLobby w) (encodeUtf8 name <> ":" <> msg)

wsGroupChat :: (InChan (User, ByteString), OutChan (User, ByteString)) -> Game -> IO ()
wsGroupChat (bcast, oc) game = do
  forever $ do
    (u@(User name _), d) <- readChan oc
    forM_ (gameUsers game) $ \(User _ conn) -> do
      sendTextData conn (encodeUtf8 name <> ":" <> d)
