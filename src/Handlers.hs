{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module Handlers (
    home,
    ws,
    joinHandler,
    leave,
    settingsHandler,
    name,
    start,
    startOver,
    guessHandler,
    LeavePost (..),
    SettingsPost (..),
    NamePost (..),
    GuessPost (..),
) where

import CustomPrelude

import App (App (..), AppGameState (..), AppGameStateChanMsg (..), AppM, Game (..), StateKey)
import CaseInsensitive (CaseInsensitiveText)
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
import GameStateEvent (GameStateEvent (..), GameStateEvents (..), GameStateEventsHeader (..), eventsForPlayer)
import Lucid hiding (for_)
import Lucid.Base (makeAttribute)
import Lucid.Htmx
import qualified Network.WebSockets as WS
import OrphanInstances ()
import qualified RIO.ByteString.Lazy as BSL
import qualified RIO.HashMap as HashMap
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid
import Timer (restartTimer, startTimer, stopTimer)
import Views (gameStateUI, guessInput, sharedHead)
import Web.FormUrlEncoded (FromForm (..))
import WithPlayerApi (PlayerId (..))

type APIConstraints api =
    ( IsElem
        ( Capture "stateKey" StateKey
            :> "leave"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateKey" StateKey
            :> "join"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateKey" StateKey
            :> "start"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateKey" StateKey
            :> "settings"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateKey" StateKey
            :> "name"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateKey" StateKey
            :> "start-over"
            :> Post '[HTML] (Headers '[Header "HX-Trigger-After-Swap" GameStateEventsHeader] (Html ()))
        )
        api
    , IsElem
        ( Capture "stateKey" StateKey
            :> "guess"
            :> Post '[HTML] (Headers '[Header "HX-Trigger-After-Swap" GameStateEventsHeader] (Html ()))
        )
        api
    )

home ::
    ( APIConstraints api
    , IsElem ("ws" :> WebSocket) api
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
            $ gameStateUI api me (appGameState ^. #stateKey) (appGameState ^. #game)

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
                (stateKey', (gs', events)) <$ writeTChan (appGameState ^. #chan) AppGameStateChanged
            else pure (appGameState ^. #stateKey, (appGameState ^. #game, appGameState ^. #events))

joinHandler ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    AppM (Html ())
joinHandler api me stateKey = do
    (stateKey', (gs, _)) <- updateGameState stateKey
        $ \case
            InLobby settings -> (InLobby $ settings & #players %~ HashMap.insert me Nothing, mempty)
            x -> (x, mempty)
    pure $ gameStateUI api me stateKey' gs

newtype LeavePost = LeavePost
    {playerId :: PlayerId}
    deriving stock (Show, Generic)

instance FromForm LeavePost

leave ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    LeavePost ->
    AppM (Html ())
leave api me stateKey p = do
    (stateKey', (gs, _)) <- updateGameState stateKey
        $ \case
            InLobby settings -> (InLobby $ settings & #players %~ HashMap.delete (p ^. #playerId), mempty)
            x -> (x, mempty)
    pure $ gameStateUI api me stateKey' gs

newtype SettingsPost = SettingsPost
    {secondsToGuess :: Int}
    deriving stock (Show, Generic)

instance FromForm SettingsPost

settingsHandler ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    SettingsPost ->
    AppM (Html ())
settingsHandler api me stateKey p = do
    (stateKey', (gs, _)) <- updateGameState stateKey
        $ \case
            InLobby settings -> (InLobby $ settings & #secondsToGuess .~ p ^. #secondsToGuess, mempty)
            x -> (x, mempty)
    pure $ gameStateUI api me stateKey' gs

data NamePost = NamePost
    {playerId :: PlayerId, name :: Text}
    deriving stock (Show, Generic)

instance FromForm NamePost

name ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    NamePost ->
    AppM (Html ())
name api me stateKey p = do
    (stateKey', (gs, _)) <- updateGameState stateKey
        $ \case
            InLobby settings ->
                (InLobby $ settings & #players %~ HashMap.update (const $ Just $ Just $ p ^. #name) (p ^. #playerId), mempty)
            x -> (x, mempty)
    pure $ gameStateUI api me stateKey' gs

start ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    AppM (Html ())
start api me stateKey = do
    (stateKey', (gs, _)) <- updateGameState stateKey $ \case
        InLobby settings -> maybe (InLobby settings, mempty) (over _1 InGame) $ startGame settings
        x -> (x, mempty)
    a <- ask
    case gs of
        InGame _ -> startTimer a
        _ -> pure ()
    pure $ gameStateUI api me stateKey' gs

gameStateEventsHeaderForPlayer :: (AddHeader h GameStateEventsHeader orig new) => PlayerId -> GameStateEvents -> orig -> new
gameStateEventsHeaderForPlayer me = maybe noHeader (addHeader . GameStateEventsHeader) . eventsForPlayer me

startOver ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    AppM (Headers '[Header "HX-Trigger-After-Swap" GameStateEventsHeader] (Html ()))
startOver api me stateKey = do
    stopTimer =<< ask
    (stateKey', (gs, events)) <- updateGameState stateKey $ \case
        InGame gs ->
            ( InLobby $ gs ^. #settings
            , GameStateEvents
                $ HashMap.fromList (gs ^.. #players % folded % #id % to (,pure GameOver))
            )
        x -> (x, mempty)
    a <- ask
    stopTimer a
    pure $ gameStateEventsHeaderForPlayer me events $ gameStateUI api me stateKey' gs

newtype GuessPost = GuessPost {guess :: CaseInsensitiveText}
    deriving stock (Show, Generic)

instance FromForm GuessPost

guessHandler ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    GuessPost ->
    AppM (Headers '[Header "HX-Trigger-After-Swap" GameStateEventsHeader] (Html ()))
guessHandler api me stateKey p = do
    (stateKey', (gs, events)) <- updateGameState stateKey $ \case
        InGame gs -> let (gs', events) = makeMove gs $ Guess $ p ^. #guess in (InGame gs', events)
        x -> (x, mempty)
    let
        html = gameStateUI api me stateKey' gs
    case gs of
        InGame gsS -> do
            a <- ask
            -- It's the next players turn
            when (CZ.current (gsS ^. #players) ^. #tries == 0)
                $ if isGameOver gsS
                    then stopTimer a
                    else restartTimer a
            pure $ gameStateEventsHeaderForPlayer me events html
        _ -> pure $ noHeader html

data WsMsg = WsMsg
    { guess :: Text
    , stateKey :: StateKey
    }
    deriving stock (Show, Generic)
    deriving anyclass (Aeson.FromJSON)

data WsResponseMsg = WsResponseMsg
    { html :: Html ()
    , events :: Maybe (Seq GameStateEvent)
    , stateKey :: StateKey
    , chanMsg :: AppGameStateChanMsg
    }
    deriving stock (Generic)

wsResponseMsgKeyValues :: (Aeson.KeyValue a) => WsResponseMsg -> [a]
wsResponseMsgKeyValues msg =
    [ "html" Aeson..= (msg ^. #html % to renderText)
    , "events" Aeson..= (msg ^. #events)
    , "stateKey" Aeson..= (msg ^. #stateKey)
    , "chanMsg"
        Aeson..= (msg ^. #chanMsg % to chanMsgJSON)
    ]
  where
    chanMsgJSON :: AppGameStateChanMsg -> Text
    chanMsgJSON = \case
        PlayerTyping{} -> "PlayerTyping"
        AppGameStateChanged -> "AppGameStateChanged"

instance Aeson.ToJSON WsResponseMsg where
    toJSON = Aeson.object . wsResponseMsgKeyValues
    toEncoding =
        Aeson.pairs . fold . wsResponseMsgKeyValues

sendWsMsg :: WS.Connection -> WsResponseMsg -> IO ()
sendWsMsg c =
    WS.sendTextData @Text c
        . decodeUtf8Lenient
        . BSL.toStrict
        . Aeson.encode
ws ::
    ( HasCallStack
    , APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    WS.Connection ->
    AppM ()
ws api me c = do
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
                        Right msg -> do
                            atomically $ do
                                appGameState <- readTVar $ a ^. #wsGameState
                                when (msg ^. #stateKey == appGameState ^. #stateKey)
                                    $ writeTChan myChan
                                    $ PlayerTyping (appGameState ^. #stateKey) me (msg ^. #guess)
                    listener
                _ -> listener
        sender :: AppM ()
        sender = do
            join $ atomically $ do
                chanMsg <- readTChan myChan
                appGameState <- readTVar $ a ^. #wsGameState
                pure $ case chanMsg of
                    AppGameStateChanged -> do
                        let
                            gs = appGameState ^. #game
                            stateKey = appGameState ^. #stateKey
                        liftIO
                            $ sendWsMsg
                                c
                                WsResponseMsg
                                    { html = gameStateUI api me stateKey gs
                                    , events = eventsForPlayer me $ appGameState ^. #events
                                    , ..
                                    }
                    PlayerTyping stateKey typer guess ->
                        when (stateKey == (appGameState ^. #stateKey) && typer /= me)
                            $ liftIO
                            $ sendWsMsg
                                c
                                WsResponseMsg
                                    { events = Nothing
                                    , html = guessInput guess False typer
                                    , ..
                                    }
            sender
    runConcurrently $ asum (Concurrently <$> [pingThread 0, listener, sender])
