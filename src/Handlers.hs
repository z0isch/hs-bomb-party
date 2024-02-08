{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module Handlers (
    home,
    ws,
) where

import CustomPrelude

import App (App (..), AppGameState (..), AppGameStateChanMsg (..), AppM, Game (..), StateKey)
import qualified CircularZipper as CZ
import qualified Data.Aeson as Aeson
import Game (
    GameState (..),
    Move (..),
    PlayerState (..),
    Settings (..),
    isGameOver,
    makeMove,
    startGame,
 )
import GameStateEvent (GameStateEvent (..), GameStateEvents (..), eventsForPlayer)
import Lucid hiding (for_)
import Lucid.Base (makeAttribute)
import Lucid.Htmx
import qualified Network.WebSockets as WS
import OrphanInstances ()
import qualified RIO.HashMap as HashMap
import qualified RIO.Text.Lazy as TL
import Servant
import Servant.API.WebSocket (WebSocket)
import Timer (restartTimer, startTimer, stopTimer)
import Views (gameStateUI, guessInput, sharedHead)
import WithPlayerApi (PlayerId (..))
import WsMsg

home ::
    ( IsElem ("ws" :> WebSocket) api
    ) =>
    Proxy api ->
    Maybe (Html ()) ->
    PlayerId ->
    AppM (Html ())
home api mHotreload me = do
    a <- ask
    appGameState <- liftIO $ readTVarIO $ a ^. #wsGameState
    pure $ html_ $ do
        head_ $ sharedHead mHotreload
        body_
            $ div_
                [ id_ "ws"
                , hxExt_ "ws,game-state-ws"
                , makeAttribute "ws-connect" $ "/" <> toUrlPiece (safeLink api (Proxy @("ws" :> WebSocket)))
                , class_ "container mx-auto px-4 py-4"
                ]
            $ gameStateUI me (appGameState ^. #stateKey) (appGameState ^. #game) Nothing

updateGameState :: StateKey -> (Game -> (Game, GameStateEvents)) -> AppM (StateKey, (Game, GameStateEvents))
updateGameState stateKey f = do
    a <- ask
    atomically $ do
        appGameState <- readTVar $ a ^. #wsGameState
        if (appGameState ^. #stateKey) == stateKey
            then do
                let
                    (gs', events) = f $ appGameState ^. #game
                    stateKey' = stateKey + 1
                writeTVar (a ^. #wsGameState) appGameState{stateKey = stateKey', game = gs', events}
                (stateKey', (gs', events)) <$ writeTChan (appGameState ^. #chan) (AppGameStateChanged stateKey' gs' events)
            else pure (appGameState ^. #stateKey, (appGameState ^. #game, appGameState ^. #events))

handleWsMsg :: PlayerId -> TChan AppGameStateChanMsg -> WsMsg -> AppM ()
handleWsMsg me chan m = do
    a <- ask
    case m of
        TypingMsg msg -> atomically $ do
            appGameState <- readTVar $ a ^. #wsGameState
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
            (_, (gs, _)) <- updateGameState (msg ^. #stateKey) $ \case
                InLobby settings -> maybe (InLobby settings, mempty) (over _1 InGame) $ startGame settings
                x -> (x, mempty)
            case gs of
                InGame _ -> startTimer a
                _ -> pure ()
        StartOverMsg msg -> do
            stopTimer a
            void $ updateGameState (msg ^. #stateKey) $ \case
                InGame gs ->
                    ( InLobby $ gs ^. #settings
                    , GameStateEvents
                        $ HashMap.fromList (gs ^.. #players % folded % #id % to (,pure GameOver))
                    )
                x -> (x, mempty)
            stopTimer a
        GuessMsg msg -> do
            (_, (gs, _)) <- updateGameState (msg ^. #stateKey) $ \case
                InGame gs -> let (gs', events) = makeMove gs $ Guess $ msg ^. #contents % #guess in (InGame gs', events)
                x -> (x, mempty)
            case gs of
                InGame gsS -> do
                    -- It's the next players turn
                    when (CZ.current (gsS ^. #players) ^. #tries == 0)
                        $ if isGameOver gsS
                            then stopTimer a
                            else restartTimer a
                _ -> pure ()

sendHtmlMsg :: (MonadIO m) => WS.Connection -> Html () -> m ()
sendHtmlMsg c = liftIO . WS.sendTextData @Text c . TL.toStrict . renderText

ws ::
    (HasCallStack) =>
    PlayerId ->
    WS.Connection ->
    AppM ()
ws me c = do
    a <- ask
    myChan <- atomically $ dupTChan . view #chan =<< readTVar (a ^. #wsGameState)
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
                chanMsg <- readTChan myChan
                appGameState <- readTVar $ a ^. #wsGameState
                pure $ case chanMsg of
                    AppGameStateChanged stateKey game events ->
                        sendHtmlMsg c
                            $ gameStateUI me stateKey game
                            $ eventsForPlayer me events
                    PlayerTyping stateKey typer guess ->
                        when (stateKey == (appGameState ^. #stateKey) && typer /= me)
                            $ sendHtmlMsg c
                            $ guessInput stateKey guess False typer
            sender
    runConcurrently $ asum (Concurrently <$> [pingThread 0, listener, sender])
