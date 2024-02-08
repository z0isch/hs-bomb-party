{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module App (AppM, App (..), Game (..), AppGameState (..), StateKey (..), AppGameStateChanMsg (..), _InGame, _InLobby) where

import CustomPrelude

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Game (GameState, Settings)
import GameStateEvent (GameStateEvents)
import Optics.TH (makePrisms)
import qualified RIO
import Servant (FromHttpApiData, ToHttpApiData)
import WithPlayerApi (PlayerId)

data Game = InLobby Settings | InGame GameState
    deriving stock (Show)
makePrisms ''Game

type AppM = RIO App

newtype StateKey = StateKey {getStateKey :: Int}
    deriving newtype (Eq, Show, Num, Display, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)

data AppGameStateChanMsg
    = AppGameStateChanged
    | PlayerTyping StateKey PlayerId Text

data AppGameState = AppGameState
    { stateKey :: StateKey
    , game :: Game
    , events :: GameStateEvents
    }
    deriving (Generic)

data App = App
    { wsGameState :: TVar AppGameState
    , wsGameStateTimer :: TVar (Maybe (Async ()))
    , wsGameChan :: TChan AppGameStateChanMsg
    , logFunction :: LogFunc
    , staticDir :: FilePath
    }
    deriving (Generic)

instance HasLogFunc App where
    logFuncL = RIO.lens (view #logFunction) (flip $ set #logFunction)
