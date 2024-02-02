{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}

module Handlers where

import CustomPrelude

import App (App (..), AppGameState (..), AppM, Game (..), StateKey, _InGame)
import CaseInsensitive (CaseInsensitiveText)
import qualified CircularZipper as CZ
import qualified Data.Aeson as Aeson
import Data.Aeson.QQ (aesonQQ)
import qualified Data.Text.Lazy as TL
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Game (
    GameState (..),
    Move (..),
    PlayerState (..),
    Settings (..),
    isGameOver,
    makeMove,
    startGame,
 )
import GameStateEvent (GameStateEvent (..), getGameStateEvents)
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
            :> Post '[HTML] (Headers '[Header "HX-Trigger-After-Swap" GameStateEvent] (Html ()))
        )
        api
    , IsElem
        ( Capture "stateKey" StateKey
            :> "guess"
            :> Post '[HTML] (Headers '[Header "HX-Trigger-After-Swap" GameStateEvent] (Html ()))
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
                , hxExt_ "ws,transform-ws-response"
                , makeAttribute "ws-connect" $ "/" <> toUrlPiece (safeLink api (Proxy @("ws" :> WebSocket)))
                , class_ "container mx-auto px-4 py-4"
                ]
            $ gameStateUI api me (appGameState ^. #stateKey) (appGameState ^. #game)

updateGameState :: StateKey -> (Game -> Game) -> AppM Game
updateGameState stateKey f = do
    a <- ask
    logDebug . display . view #stateKey =<< readTVarIO (a ^. #wsGameState)
    liftIO $ do
        atomically $ do
            appGameState <- readTVar $ a ^. #wsGameState
            if (appGameState ^. #stateKey) == stateKey
                then do
                    let
                        gs' = f $ appGameState ^. #game
                        stateKey' = stateKey + 1
                    writeTChan (appGameState ^. #chan) (stateKey', Left gs')
                    gs' <$ writeTVar (a ^. #wsGameState) appGameState{stateKey = stateKey', game = gs'}
                else pure $ appGameState ^. #game

join ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    AppM (Html ())
join api me stateKey = do
    gs <- updateGameState stateKey
        $ \case
            InLobby settings -> InLobby $ settings & #players %~ HashMap.insert me Nothing
            x -> x
    pure $ gameStateUI api me stateKey gs

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
    gs <- updateGameState stateKey
        $ \case
            InLobby settings -> InLobby $ settings & #players %~ HashMap.delete (p ^. #playerId)
            x -> x
    pure $ gameStateUI api me stateKey gs

data SettingsPost = SettingsPost
    {secondsToGuess :: Int}
    deriving stock (Show, Generic)

instance FromForm SettingsPost

settings ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    SettingsPost ->
    AppM (Html ())
settings api me stateKey p = do
    gs <- updateGameState stateKey
        $ \case
            InLobby settings -> InLobby $ settings & #secondsToGuess .~ p ^. #secondsToGuess
            x -> x
    pure $ gameStateUI api me stateKey gs

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
    gs <- updateGameState stateKey
        $ \case
            InLobby settings -> InLobby $ settings & #players %~ HashMap.update (const $ Just $ Just $ p ^. #name) (p ^. #playerId)
            x -> x
    pure $ gameStateUI api me stateKey gs

start ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    AppM (Html ())
start api me stateKey = do
    gs <- updateGameState stateKey $ \case
        InLobby settings -> maybe (InLobby settings) InGame $ startGame settings
        x -> x
    a <- ask
    case gs of
        InGame _ -> startTimer a
        _ -> pure ()
    pure $ gameStateUI api me stateKey gs

startOver ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    AppM (Headers '[Header "HX-Trigger-After-Swap" GameStateEvent] (Html ()))
startOver api me stateKey = do
    stopTimer =<< ask
    gs <- updateGameState stateKey $ \case
        InGame gs ->
            InLobby $ gs ^. #settings
        x -> x
    pure $ addHeader GameOver $ gameStateUI api me stateKey gs

newtype GuessPost = GuessPost {guess :: CaseInsensitiveText}
    deriving stock (Show, Generic)

instance FromForm GuessPost

guess ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    GuessPost ->
    AppM (Headers '[Header "HX-Trigger-After-Swap" GameStateEvent] (Html ()))
guess api me stateKey p = do
    gs <- updateGameState stateKey $ \case
        InGame gs -> InGame $ makeMove gs $ Guess $ p ^. #guess
        x -> x
    let html = gameStateUI api me stateKey gs
    case gs of
        InGame gsS -> do
            a <- ask

            -- It's the next players turn
            if CZ.current (gsS ^. #players) ^. #tries == 0
                then
                    if isGameOver gsS
                        then do
                            stopTimer a
                            pure $ addHeader GameOver html
                        else do
                            restartTimer a
                            pure $ addHeader CorrectGuess html
                else pure $ addHeader WrongGuess html
        _ -> pure $ noHeader html

newtype WsMsg = WsMsg
    { guess :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (Aeson.FromJSON)

ws ::
    ( APIConstraints api
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
                                writeTChan myChan (appGameState ^. #stateKey, Right $ guessInput (msg ^. #guess) False False False me)
                    listener
                _ -> listener
        sender :: AppM ()
        sender = do
            mMsg <- atomically $ do
                (stateKey, msg) <- readTChan myChan
                appGameState <- readTVar $ a ^. #wsGameState
                pure $ if stateKey == (appGameState ^. #stateKey) then Just (stateKey, msg) else Nothing

            liftIO $ for_ mMsg $ \msg -> WS.sendTextData @Text c $ case msg of
                (stateKey, Left gs) ->
                    decodeUtf8Lenient
                        $ BSL.toStrict
                        $ Aeson.encode [aesonQQ|{html: #{renderText $ gameStateUI api me stateKey gs}, events: #{getGameStateEvents me gs}}|]
                (_, Right h) -> TL.toStrict $ renderText h
            sender
    runConcurrently $ asum (Concurrently <$> [pingThread 0, listener, sender])
