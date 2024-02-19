{-# LANGUAGE OverloadedLabels #-}

module Survival.Timer (startTimer, stopTimer, restartTimer) where

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

startTimer :: (MonadUnliftIO m) => App -> m ()
startTimer a = do
    t <- async go
    atomically $ writeTVar (a ^. #survival % #wsGameStateTimer) $ Just t
  where
    go = do
        let secondsToGuessL = #game % _InGame % #settings % #secondsToGuess
        mSecondsToGuess <- preview secondsToGuessL <$> readTVarIO (a ^. #survival % #wsGameState)
        traverse_ (threadDelay . (* 1000000)) mSecondsToGuess

        join $ atomically $ do
            appGameState <- readTVar $ a ^. #survival % #wsGameState
            case appGameState ^. #game of
                (InGame gss) -> case makeMove gss TimeUp of
                    Just (gs', events) -> do
                        let
                            appGameState' =
                                appGameState
                                    & (#game .~ InGame gs')
                                    & (#stateKey %~ (+ 1))
                                    & (#events .~ events)
                        writeTVar (a ^. #survival % #wsGameState) appGameState'
                        writeTChan (a ^. #survival % #wsGameChan) AppGameStateChanged
                        pure $ unless (isGameOver gs') go
                    Nothing -> pure $ pure ()
                _ -> pure $ pure ()

stopTimer :: (MonadIO m) => App -> m ()
stopTimer a = maybe (pure ()) cancel =<< readTVarIO (a ^. #survival % #wsGameStateTimer)

restartTimer :: (MonadUnliftIO m) => App -> m ()
restartTimer a = stopTimer a >> startTimer a
