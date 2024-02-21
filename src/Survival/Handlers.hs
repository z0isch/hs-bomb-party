{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module Survival.Handlers (
    home,
    ws,
) where

import CustomPrelude

import App (App (..), AppM, SurvivalApp (..))
import CaseInsensitive (CaseInsensitiveText (..))
import qualified Data.Aeson as Aeson
import Lucid hiding (for_)
import Lucid.Base (makeAttribute)
import Lucid.Htmx
import qualified Network.WebSockets as WS
import OrphanInstances ()
import qualified RIO.HashMap as HashMap
import qualified RIO.Seq as S
import qualified RIO.Text.Lazy as TL
import Servant
import Servant.API.WebSocket (WebSocket)
import StateKey (StateKey)
import Survival.AppGameState (AppGame (..), AppGameState (..), AppGameStateChanMsg (..), _InGame)
import Survival.Game (
    GameState (..),
    Move (..),
    PlayerState (..),
    Settings (..),
    isGameOver,
    makeMove,
    startGame,
 )
import Survival.GameStateEvent (GameStateEvent (..), GameStateEvents (..), eventsForPlayer, isNextRound, _CorrectGuess, _WrongGuess)
import Survival.Timer (restartTimer, startTimer, stopTimer)
import Survival.Views (gameStateUI, guessInput, playerStateUI, sharedHead)
import Survival.WsMsg
import WithPlayerApi (PlayerId (..))

home ::
    ( IsElem ("survival" :> "ws" :> WebSocket) api
    ) =>
    Proxy api ->
    Maybe (Html ()) ->
    PlayerId ->
    AppM (Html ())
home api mHotreload me = do
    a <- ask
    appGameState <- liftIO $ readTVarIO $ a ^. #survival % #wsGameState
    pure $ doctypehtml_ $ html_ $ do
        head_ $ sharedHead mHotreload
        body_
            $ main_
                [ id_ "ws"
                , hxExt_ "ws,game-state-ws,morph"
                , makeAttribute "ws-connect" $ "/" <> toUrlPiece (safeLink api (Proxy @("survival" :> "ws" :> WebSocket)))
                , class_ "container mx-auto px-4 py-4"
                ]
            $ gameStateUI me (appGameState ^. #stateKey) (appGameState ^. #game) Nothing

updateGameState :: StateKey -> (AppGame -> (AppGame, GameStateEvents)) -> AppM (AppGame, GameStateEvents)
updateGameState stateKey f = do
    a <- ask
    atomically $ do
        appGameState <- readTVar $ a ^. #survival % #wsGameState
        if (appGameState ^. #stateKey) == stateKey
            then do
                let
                    (gs', events) = f $ appGameState ^. #game
                    stateKey' = stateKey + 1
                    appGameState' = appGameState{stateKey = stateKey', game = gs', events}
                writeTVar (a ^. #survival % #wsGameState) appGameState'
                (gs', events) <$ writeTChan (a ^. #survival % #wsGameChan) AppGameStateChanged
            else pure (appGameState ^. #game, appGameState ^. #events)

handleWsMsg :: PlayerId -> TChan AppGameStateChanMsg -> WsMsg -> AppM ()
handleWsMsg me chan m = do
    a <- ask
    case m of
        TypingMsg msg -> atomically $ do
            appGameState <- readTVar $ a ^. #survival % #wsGameState
            when (msg ^. #stateKey == appGameState ^. #stateKey)
                $ writeTChan chan
                $ PlayerTyping (appGameState ^. #stateKey) me (msg ^. #contents % #guess)
        SettingsMsg msg ->
            void
                $ updateGameState
                    (msg ^. #stateKey)
                    ( \case
                        InLobby settings -> (InLobby $ settings & #secondsToGuess .~ msg ^. #contents % #secondsToGuess, mempty)
                        y -> (y, mempty)
                    )
        JoinMsg msg ->
            void
                $ updateGameState
                    (msg ^. #stateKey)
                    ( \case
                        InLobby settings -> (InLobby $ settings & #players %~ HashMap.insert me Nothing, mempty)
                        x -> (x, mempty)
                    )
        LeaveMsg msg ->
            void
                $ updateGameState
                    (msg ^. #stateKey)
                    ( \case
                        InLobby settings -> (InLobby $ settings & #players %~ HashMap.delete (msg ^. #contents % #playerId), mempty)
                        x -> (x, mempty)
                    )
        NameMsg msg ->
            void
                $ updateGameState
                    (msg ^. #stateKey)
                    ( \case
                        InLobby settings ->
                            (InLobby $ settings & #players %~ HashMap.update (const $ Just $ Just $ msg ^. #contents % #name) (msg ^. #contents % #playerId), mempty)
                        x -> (x, mempty)
                    )
        StartMsg msg -> do
            (gs, _) <- updateGameState (msg ^. #stateKey) $ \case
                InLobby settings -> maybe (InLobby settings, mempty) (over _1 InGame) $ startGame settings
                x -> (x, mempty)
            traverseOf_ _InGame (const $ startTimer a) gs
        StartOverMsg msg -> do
            stopTimer a
            void $ updateGameState (msg ^. #stateKey) $ \case
                InGame gs ->
                    ( InLobby $ gs ^. #settings
                    , GameStateEvents
                        $ HashMap.fromList (gs ^.. #players % folded % #id % to (,pure GameOver))
                    )
                x -> (x, mempty)
        GuessMsg msg -> do
            (gs, events) <- atomically $ do
                appGameState <- readTVar $ a ^. #survival % #wsGameState
                let guess = msg ^. #contents % #guess
                case appGameState ^. #game of
                    InGame gs -> do
                        case makeMove gs $ Guess me guess of
                            Nothing -> pure (InGame gs, appGameState ^. #events)
                            Just (gs', events)
                                | isNextRound events -> do
                                    writeTVar (a ^. #survival % #wsGameState)
                                        $ appGameState
                                            { stateKey = (appGameState ^. #stateKey) + 1
                                            , game = InGame gs'
                                            , events
                                            }
                                    (InGame gs', events) <$ writeTChan (a ^. #survival % #wsGameChan) AppGameStateChanged
                                | otherwise -> do
                                    writeTVar (a ^. #survival % #wsGameState)
                                        $ appGameState
                                            { stateKey = appGameState ^. #stateKey
                                            , game = InGame gs'
                                            , events
                                            }
                                    (InGame gs', events) <$ case eventsForPlayer me events of
                                        Nothing -> pure ()
                                        Just evs
                                            | any (has _CorrectGuess) evs ->
                                                writeTChan (a ^. #survival % #wsGameChan)
                                                    $ PlayerGuessedCorrectly (appGameState ^. #stateKey) me guess
                                            | any (has _WrongGuess) evs ->
                                                writeTChan (a ^. #survival % #wsGameChan)
                                                    $ PlayerGuessedWrong (appGameState ^. #stateKey) me
                                            | otherwise -> pure ()
                    x -> pure (x, mempty)

            forOf_ _InGame gs $ \gsS -> do
                -- It's a new round
                when (isJust $ S.elemIndexL MyTurn =<< eventsForPlayer me events)
                    $ if isGameOver gsS
                        then stopTimer a
                        else restartTimer a

sendHtmlMsg :: (MonadIO m) => WS.Connection -> Html () -> m ()
sendHtmlMsg c = liftIO . WS.sendTextData @Text c . TL.toStrict . renderText

ws ::
    (HasCallStack) =>
    PlayerId ->
    WS.Connection ->
    AppM ()
ws me c = do
    a <- ask
    myChan <- atomically $ dupTChan (a ^. #survival % #wsGameChan)
    let
        pingThread :: Int -> AppM ()
        pingThread i = do
            threadDelay 30000000
            liftIO $ WS.sendPing c $ tshow i
            pingThread $ i + 1
        listener :: AppM ()
        listener =
            liftIO (WS.receive c) >>= \case
                WS.ControlMessage (WS.Close _ _) -> pure ()
                WS.DataMessage _ _ _ (WS.Text msgString _) -> do
                    case Aeson.eitherDecode @WsMsg msgString of
                        Left err -> logError $ "WebSocket received bad json: " <> fromString err
                        Right msg -> handleWsMsg me myChan msg
                    listener
                _ -> listener
        sender :: AppM ()
        sender = do
            join $ atomically $ do
                appGameState <- readTVar $ a ^. #survival % #wsGameState
                let
                    mGameState = appGameState ^? #game % _InGame
                    psFor pId = appGameState ^? #game % _InGame % #players % ix pId
                chanMsg <- readTChan myChan
                pure $ case chanMsg of
                    AppGameStateChanged -> do
                        logDebug $ displayPretty $ appGameState ^. #game
                        sendHtmlMsg c
                            $ gameStateUI me (appGameState ^. #stateKey) (appGameState ^. #game)
                            $ eventsForPlayer me (appGameState ^. #events)
                    PlayerGuessedCorrectly stateKey guesser guess -> case (mGameState, psFor guesser) of
                        (Just gameState, Just guesserState) -> do
                            when (stateKey == (appGameState ^. #stateKey))
                                $ sendHtmlMsg c
                                $ playerStateUI me gameState (eventsForPlayer me (appGameState ^. #events)) guesserState (getCaseInsensitiveText guess)
                        _ -> pure ()
                    PlayerGuessedWrong stateKey guesser -> case (mGameState, psFor guesser) of
                        (Just gameState, Just guesserState) -> do
                            when (stateKey == (appGameState ^. #stateKey))
                                $ sendHtmlMsg c
                                $ playerStateUI me gameState (eventsForPlayer me (appGameState ^. #events)) guesserState ""
                        _ -> pure ()
                    PlayerTyping stateKey typer guess -> do
                        when (stateKey == (appGameState ^. #stateKey) && typer /= me)
                            $ sendHtmlMsg c
                            $ guessInput guess False typer
            sender
    runConcurrently $ asum (Concurrently <$> [pingThread 0, listener, sender])
