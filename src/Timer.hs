{-# LANGUAGE OverloadedLabels #-}

module Timer (startTimer, stopTimer, restartTimer) where

import CustomPrelude

import App (App (..), AppGameState (..), AppGameStateChanMsg (AppGameStateChanged), Game (..), _InGame)
import Game (
    GameState (..),
    Move (..),
    Settings (..),
    isGameOver,
    makeMove,
 )

startTimer :: (MonadUnliftIO m) => App -> m ()
startTimer a = do
    t <- async go
    atomically $ writeTVar (a ^. #wsGameStateTimer) $ Just t
  where
    go = do
        let secondsToGuessL = #game % _InGame % #settings % #secondsToGuess
        mSecondsToGuess <- preview secondsToGuessL <$> readTVarIO (a ^. #wsGameState)
        traverse_ (threadDelay . (* 1000000)) mSecondsToGuess

        join $ atomically $ do
            appGameState <- readTVar $ a ^. #wsGameState
            case appGameState ^. #game of
                (InGame gss) -> do
                    let (gs', events) = makeMove gss TimeUp
                        appGameState' =
                            appGameState
                                & (#game .~ InGame gs')
                                & (#stateKey %~ (+ 1))
                                & (#events .~ events)
                    writeTVar (a ^. #wsGameState) appGameState'
                    writeTChan (a ^. #wsGameChan) AppGameStateChanged
                    pure $ unless (isGameOver gs') go
                _ -> pure $ pure ()

stopTimer :: (MonadIO m) => App -> m ()
stopTimer a = maybe (pure ()) cancel =<< readTVarIO (a ^. #wsGameStateTimer)

restartTimer :: (MonadUnliftIO m) => App -> m ()
restartTimer a = stopTimer a >> startTimer a
