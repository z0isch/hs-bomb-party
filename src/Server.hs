{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module Server (app) where

import CustomPrelude

import App (App (..), AppM)
import qualified Classic.Handlers
import Control.Monad.Except (ExceptT (..))
import Lucid hiding (for_)
import OrphanInstances ()
import Servant
import Servant.API.WebSocket (WebSocket)
import Servant.HTML.Lucid
import qualified Survival.Handlers
import WithPlayerApi (PlayerId (..))
import qualified WithPlayerApi

type API =
    Get '[HTML] (Html ())
        :<|> "ws"
            :> WebSocket
        :<|> "survival"
            :> ( Get '[HTML] (Html ())
                    :<|> "ws"
                        :> WebSocket
               )

api :: Proxy API
api = Proxy

survivalApi ::
    Proxy
        ( "survival"
            :> ( Get '[HTML] (Html ())
                    :<|> "ws"
                        :> WebSocket
               )
        )
survivalApi = Proxy

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
    Classic.Handlers.home api mHotReload playerId
        :<|> Classic.Handlers.ws playerId
        :<|> Survival.Handlers.home survivalApi mHotReload playerId
        :<|> Survival.Handlers.ws playerId

app :: App -> Maybe (Html ()) -> Application
app a = serve totalApi . totalApiServer a
