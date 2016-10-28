{-# LANGUAGE OverloadedStrings #-}
module App.Turnbased (runGame) where

import           Data.ByteString               (ByteString)
import           Data.Monoid

import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.STM
import           Control.Exception             (finally)
import           Control.Monad

import           Network.WebSockets

import           App.Common

runGame :: (InChan (User, ByteString), OutChan (User, ByteString)) -> Game -> IO ()
runGame (ic, oc) game = do
  putStrLn "Running Game"
  print game

  let (startUser:otherUsers) = gameUsers game
      loop user others = do
        (u, d) <- readChan oc
        if u /= user
          then putStrLn (show u <> " out of turn") >> loop user others
          else do
            writeChan ic (noUser, d)
            let (nextUser:rest) = others
            loop nextUser (others <> [user])

  loop startUser otherUsers
