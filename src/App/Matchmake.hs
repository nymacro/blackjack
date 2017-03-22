{-# LANGUAGE OverloadedStrings #-}
module App.Matchmake (wsMatchmake, disconnectGame, Lobby, newLobby) where

import           Data.Monoid

import           Control.Concurrent            hiding (newChan, writeChan)
import           Control.Concurrent.Async
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.STM
import           Control.Exception             (finally)
import           Control.Monad

import           Network.WebSockets

import           App.Common

-- | Lobby state. Used for synchronising players when finding a match
-- The first MVar is an indicator for whether a maximum wait thread has been
-- launched. The second MVar synchronises the running game across threads.
data Lobby = Lobby (TVar (MVar (), MVar (Async (), Game)))

unLobby :: Lobby -> (TVar (MVar (), MVar (Async (), Game)))
unLobby (Lobby l) = l

-- | Create new lobby state
newLobby :: IO Lobby
newLobby = do
  m <- newEmptyMVar
  m' <- newEmptyMVar
  t <- newTVarIO (m, m')
  return $ Lobby t

-- | Create a new lobby game and return previously filled game sync
swapLobby :: Lobby -> IO (MVar (), MVar (Async (), Game))
swapLobby (Lobby t) = do
  -- create a new MVar to replace the existing one which will be used
  -- to return/synchronise the game for waiting players
  m' <- newEmptyMVar
  m'' <- newEmptyMVar
  atomically $ swapTVar t (m', m'')

-- | Get the current lobby game sync
getLobby :: Lobby -> IO (MVar (), MVar (Async (), Game))
getLobby (Lobby t) = readTVarIO t

-- | Add game to world
addGame :: Game       -- ^ Game to add
        -> TVar World -- ^ World state
        -> IO ()
addGame g world = do
  atomically $
    modifyTVar world (\w@(World _ games) -> w { worldGames = g : games })

-- | Disconnect game
disconnectGame :: Game       -- ^ Game to disconnect
               -> TVar World -- ^ World state
               -> IO ()
disconnectGame game world = do
  atomically $ modifyTVar world removeGame

  where
    removeGame w = w { worldGames = filter (/= game) (worldGames w) }

-- | Take players from world lobby
takePlayers :: Int               -- ^ Number of players to take
            -> TVar World        -- ^ World state
            -> IO (Maybe [User]) -- ^ Maybe players
takePlayers numPlayers world =
  atomically $ do
    w <- readTVar world
    let split = splitAtMaybe numPlayers (worldLobby w)
    case split of
      Nothing -> return Nothing
      Just (players, rest) -> do
        writeTVar world w { worldLobby = rest }
        return $ Just players

-- | Return all waiting players
takeAllPlayers :: TVar World
               -> IO [User]
takeAllPlayers world =
  atomically $ do
    w <- readTVar world
    let players = worldLobby w
    writeTVar world $ w { worldLobby = [] }
    return players

-- | Find game
-- Will return existing Lobby if still waiting for players, otherwise will run
-- the game and return the new lobby to use for new players.
findGame :: Int                         -- ^ Maximum number of players for a game
         -> Lobby                       -- ^ Lobby state
         -> TVar World                  -- ^ World state
         -> (Game -> IO ())               -- ^ Game loop
         -> IO (MVar (Async (), Game))   -- ^ Found game
findGame numPlayers lobby world runGame = do
  players <- takePlayers numPlayers world

  let run p = do
        -- set up game and run
        (bcast, oc) <- newChan
        let g = Game p bcast oc

        (_, m) <- swapLobby lobby

        -- main game thread
        a <- async (runGame g)
        putMVar m (a, g)

        -- register game
        addGame g world

        return m

  case players of
    Nothing -> do
      -- try and start the maximum wait thread (but only if one does not already
      -- exist for this lobby)
      (w, l) <- getLobby lobby
      start <- tryPutMVar w ()
      when start $ do
        _ <- forkIO $ do
          putStrLn "Starting wait thread"
          threadDelay $ 10 * 1000000 -- 10 seconds
          -- only run game if we haven't started already -- although this suffers
          -- from a potential data-race on MVar l, where it may be possible a
          -- player has joined and hit the maximum player cap, but also the
          -- waiting thread has finished its delay. There probably needs to be
          -- a better abstraction of the current Lobby to prevent such a case.
          gameNotStarted <- isEmptyMVar l
          if gameNotStarted
            then do
              p <- takeAllPlayers world
              run p >> return ()
            else do
              putStrLn "Timer exceeded and game already started..."
              return ()
        return ()
      return =<< snd <$> getLobby lobby
    Just p  -> run p

-- | Generic matchmaking websockets app
wsMatchmake :: User          -- ^ Joining user
            -> Lobby         -- ^ Lobby state
            -> TVar World    -- ^ World state
            -> Int           -- ^ Player count
            -> (Game -> IO ()) -- ^ Game loop
            -> IO ()
wsMatchmake user@(User _ _ conn) lobby world numPlayers runGame = do
  m <- findGame numPlayers lobby world runGame
  (main, g@(Game _ bcast _)) <- readMVar m

  -- create client thread
  a <- async $ do
    case conn of
      Nothing -> return ()
      Just c  -> do
        -- make sure we tell the game when a user disconnects
        let fin = writeChan bcast (user, "DISCONNECT") >> putStrLn ("Client left " <> show user)
        flip finally fin $
          forever $ do
            -- start a timeout thread to ensure activity
            timer <- async $ do
              threadDelay $ 10 * 1000000 -- 10 seconds
              writeChan bcast (user, "DISCONNECT")
            flip finally (cancel timer) $ do
              d <- receiveData c
              writeChan bcast (user, d)

  -- wait for game to finish and clean up
  wait main

  disconnectGame g world
  cancel a

  return ()
