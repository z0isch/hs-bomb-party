{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}

module App (AppM, App (..), ClassicApp (..)) where

import CustomPrelude

import qualified Classic.AppGameState
import qualified RIO

type AppM = RIO App

data ClassicApp = ClassicApp
    { wsGameState :: TVar Classic.AppGameState.AppGameState
    , wsGameStateTimer :: TVar (Maybe (Async ()))
    , wsGameChan :: TChan Classic.AppGameState.AppGameStateChanMsg
    }
    deriving (Generic)
data App = App
    { classic :: ClassicApp
    , logFunction :: LogFunc
    , staticDir :: FilePath
    }
    deriving (Generic)

instance HasLogFunc App where
    logFuncL = RIO.lens (view #logFunction) (flip $ set #logFunction)
