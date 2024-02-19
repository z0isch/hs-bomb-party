{-# LANGUAGE OverloadedLabels #-}

module Classic.Timer (startTimer, stopTimer, restartTimer) where

import CustomPrelude

import App (App (..), ClassicApp (..))
import Classic.AppGameState (AppGame (..), AppGameState (..), AppGameStateChanMsg (AppGameStateChanged), _InGame)
import Classic.Game (
    GameState (..),
    Move (..),
    Settings (..),
    isGameOver,
    makeMove,
 )

startTimer :: (MonadUnliftIO m) => App -> m ()
startTimer a = do
    t <- async go
    atomically $ writeTVar (a ^. #classic % #wsGameStateTimer) $ Just t
  where
    go = do
        let secondsToGuessL = #game % _InGame % #settings % #secondsToGuess
        mSecondsToGuess <- preview secondsToGuessL <$> readTVarIO (a ^. #classic % #wsGameState)
        traverse_ (threadDelay . (* 1000000)) mSecondsToGuess

        join $ atomically $ do
            appGameState <- readTVar $ a ^. #classic % #wsGameState
            case appGameState ^. #game of
                (InGame gss) -> do
                    let (gs', events) = makeMove gss TimeUp
                        appGameState' =
                            appGameState
                                & (#game .~ InGame gs')
                                & (#stateKey %~ (+ 1))
                                & (#events .~ events)
                    writeTVar (a ^. #classic % #wsGameState) appGameState'
                    writeTChan (a ^. #classic % #wsGameChan) AppGameStateChanged
                    pure $ unless (isGameOver gs') go
                _ -> pure $ pure ()

stopTimer :: (MonadIO m) => App -> m ()
stopTimer a = maybe (pure ()) cancel =<< readTVarIO (a ^. #classic % #wsGameStateTimer)

restartTimer :: (MonadUnliftIO m) => App -> m ()
restartTimer a = stopTimer a >> startTimer a
