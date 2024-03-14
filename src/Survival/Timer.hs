{-# LANGUAGE OverloadedLabels #-}

module Survival.Timer (startTimer, stopTimer, restartTimer, turnTimeUp, betweenRounds) where

import CustomPrelude

import App (App (..), SurvivalApp (..))
import Survival.AppGameState (AppGame (..), AppGameState (..), AppGameStateChanMsg (AppGameStateChanged), _InGame)
import Survival.Game (
    GameState (..),
    Move (..),
    Settings (..),
    isGameOver,
    makeMove,
 )

startTimer :: (MonadUnliftIO m) => App -> (App -> m ()) -> m ()
startTimer a f = do
    t <- async $ f a
    atomically $ writeTVar (a ^. #survival % #wsGameStateTimer) $ Just t

stopTimer :: (MonadIO m) => App -> m ()
stopTimer a = maybe (pure ()) cancel =<< readTVarIO (a ^. #survival % #wsGameStateTimer)

restartTimer :: (MonadUnliftIO m) => App -> (App -> m ()) -> m ()
restartTimer a f = stopTimer a >> startTimer a f

turnTimeUp :: (MonadUnliftIO m) => App -> m ()
turnTimeUp a = do
    let secondsToGuessL = #game % _InGame % #settings % #secondsToGuess
    mSecondsToGuess <- preview secondsToGuessL <$> readTVarIO (a ^. #survival % #wsGameState)
    traverse_ (threadDelay . (* 1000000)) mSecondsToGuess

    join $ atomically $ do
        appGameState <- readTVar $ a ^. #survival % #wsGameState
        case appGameState ^. #game of
            (InGame gss) -> case makeMove gss TimeUp of
                Just (gs', events) -> do
                    let
                        gameStateType = if isGameOver gs' then InGame else BetweenRounds
                        appGameState' =
                            appGameState
                                & (#game .~ gameStateType gs')
                                & (#stateKey %~ (+ 1))
                                & (#events .~ events)
                    writeTVar (a ^. #survival % #wsGameState) appGameState'
                    writeTChan (a ^. #survival % #wsGameChan) AppGameStateChanged
                    pure $ unless (isGameOver gs') $ betweenRounds a
                Nothing -> pure $ pure ()
            _ -> pure $ pure ()

betweenRounds :: (MonadUnliftIO m) => App -> m ()
betweenRounds a = do
    threadDelay 5000000

    atomically $ do
        appGameState <- readTVar $ a ^. #survival % #wsGameState
        case appGameState ^. #game of
            (BetweenRounds gss) -> do
                let
                    appGameState' =
                        appGameState
                            & (#game .~ InGame gss)
                            & (#stateKey %~ (+ 1))
                writeTVar (a ^. #survival % #wsGameState) appGameState'
                writeTChan (a ^. #survival % #wsGameChan) AppGameStateChanged
                pure ()
            _ -> pure ()
    startTimer a turnTimeUp
