{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Classic.AppGameState (AppGame (..), AppGameState (..), AppGameStateChanMsg (..), _InGame, _InLobby) where

import CustomPrelude

import Classic.Game (GameState, Settings)
import Classic.GameStateEvent (GameStateEvents)
import Optics.TH (makePrisms)
import StateKey (StateKey)
import WithPlayerApi (PlayerId)

data AppGame = InLobby Settings | InGame GameState
    deriving stock (Show)
makePrisms ''AppGame

data AppGameStateChanMsg
    = AppGameStateChanged
    | PlayerTyping StateKey PlayerId Text

data AppGameState = AppGameState
    { stateKey :: StateKey
    , game :: AppGame
    , events :: GameStateEvents
    }
    deriving (Generic)
