{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Survival.AppGameState (AppGame (..), AppGameState (..), AppGameStateChanMsg (..), _InGame, _InLobby, _BetweenRounds) where

import CustomPrelude

import CaseInsensitive (CaseInsensitiveText)
import Optics.TH (makePrisms)
import StateKey (StateKey)
import Survival.Game (GameState, Settings)
import Survival.GameStateEvent (GameStateEvents)
import WithPlayerApi (PlayerId)

data AppGame
    = InLobby Settings
    | InGame GameState
    | BetweenRounds GameState
    deriving stock (Show)
makePrisms ''AppGame

data AppGameStateChanMsg
    = AppGameStateChanged
    | PlayerTyping StateKey PlayerId Text
    | PlayerGuessedCorrectly StateKey PlayerId CaseInsensitiveText
    | PlayerGuessedWrong StateKey PlayerId

data AppGameState = AppGameState
    { stateKey :: StateKey
    , game :: AppGame
    , events :: GameStateEvents
    }
    deriving (Generic)
