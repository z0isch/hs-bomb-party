{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}

module App (AppM, App (..), ClassicApp (..), SurvivalApp (..)) where

import CustomPrelude

import qualified Classic.AppGameState
import qualified RIO
import qualified Survival.AppGameState

type AppM = RIO App

data ClassicApp = ClassicApp
    { wsGameState :: TVar Classic.AppGameState.AppGameState
    , wsGameStateTimer :: TVar (Maybe (Async ()))
    , wsGameChan :: TChan Classic.AppGameState.AppGameStateChanMsg
    }
    deriving (Generic)

data SurvivalApp = SurvivalApp
    { wsGameState :: TVar Survival.AppGameState.AppGameState
    , wsGameStateTimer :: TVar (Maybe (Async ()))
    , wsGameChan :: TChan Survival.AppGameState.AppGameStateChanMsg
    }
    deriving (Generic)

data App = App
    { classic :: ClassicApp
    , survival :: SurvivalApp
    , logFunction :: LogFunc
    , staticDir :: FilePath
    }
    deriving (Generic)

instance HasLogFunc App where
    logFuncL = RIO.lens (view #logFunction) (flip $ set #logFunction)
