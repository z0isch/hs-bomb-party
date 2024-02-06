{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module Server (app) where

import CustomPrelude

import App (App (..), AppM, StateKey)
import Control.Monad.Except (ExceptT (..))
import GameStateEvent (GameStateEventsHeader)
import qualified Handlers
import Lucid hiding (for_)
import OrphanInstances ()
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid
import WithPlayerApi (PlayerId (..))
import qualified WithPlayerApi

type API =
    Get '[HTML] (Html ())
        :<|> StateChangeAPI
        :<|> "ws"
            :> WebSocket

type StateChangeAPI =
    Capture "stateKey" StateKey
        :> ( "join" :> Post '[HTML] (Html ())
                :<|> "leave"
                    :> ReqBody '[FormUrlEncoded] Handlers.LeavePost
                    :> Post '[HTML] (Html ())
                :<|> "settings"
                    :> ReqBody '[FormUrlEncoded] Handlers.SettingsPost
                    :> Post '[HTML] (Html ())
                :<|> "name"
                    :> ReqBody '[FormUrlEncoded] Handlers.NamePost
                    :> Post '[HTML] (Html ())
                :<|> "start" :> Post '[HTML] (Html ())
                :<|> "start-over" :> Post '[HTML] (Headers '[Header "HX-Trigger-After-Swap" GameStateEventsHeader] (Html ()))
                :<|> "guess"
                    :> ReqBody '[FormUrlEncoded] Handlers.GuessPost
                    :> Post '[HTML] (Headers '[Header "HX-Trigger-After-Swap" GameStateEventsHeader] (Html ()))
           )

api :: Proxy API
api = Proxy

stateChangeApi :: Proxy StateChangeAPI
stateChangeApi = Proxy

totalApi :: Proxy ("static" :> Raw :<|> WithPlayerApi.API API)
totalApi = Proxy

totalApiServer :: App -> Maybe (Html ()) -> Server ("static" :> Raw :<|> WithPlayerApi.API API)
totalApiServer a mHotReload =
    hoistServer
        totalApi
        ( Servant.Handler . ExceptT . handleRIOServerErrors . fmap Right . runRIO a . logErrors
        )
        $ static
        :<|> WithPlayerApi.withPlayerApi
            api
            (server mHotReload)
  where
    static = serveDirectoryWebApp $ a ^. #staticDir
    logErrors = handleAny $ \e -> logError (displayShow e) >> throwM e
    -- Lift thrown ServerErrors into Left
    handleRIOServerErrors = handle @IO @ServerError (pure . Left)

server :: Maybe (Html ()) -> PlayerId -> ServerT API AppM
server mHotReload playerId =
    Handlers.home api mHotReload playerId
        :<|> stateChangeServer playerId
        :<|> Handlers.ws api playerId

stateChangeServer :: PlayerId -> ServerT StateChangeAPI AppM
stateChangeServer playerId stateId =
    Handlers.joinHandler stateChangeApi playerId stateId
        :<|> Handlers.leave stateChangeApi playerId stateId
        :<|> Handlers.settingsHandler stateChangeApi playerId stateId
        :<|> Handlers.name stateChangeApi playerId stateId
        :<|> Handlers.start stateChangeApi playerId stateId
        :<|> Handlers.startOver stateChangeApi playerId stateId
        :<|> Handlers.guessHandler stateChangeApi playerId stateId

app :: App -> Maybe (Html ()) -> Application
app a = serve totalApi . totalApiServer a
