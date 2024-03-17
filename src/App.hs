{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}

module App (AppM, App (..), ClassicApp (..), SurvivalApp (..), withSqlConnection, sqlConnectionPoolEnv) where

import CustomPrelude

import qualified Classic.AppGameState
import Control.Concurrent (getNumCapabilities)
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import qualified Database.PostgreSQL.Simple
import qualified RIO
import qualified RIO.Text as T
import qualified Survival.AppGameState
import System.Environment (lookupEnv)

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
    , sqlConnectionPool :: Pool Database.PostgreSQL.Simple.Connection
    }
    deriving (Generic)

instance HasLogFunc App where
    logFuncL = RIO.lens (view #logFunction) (flip $ set #logFunction)

sqlConnectionPoolEnv :: IO (Pool Database.PostgreSQL.Simple.Connection)
sqlConnectionPoolEnv = do
    numCapabilities <- getNumCapabilities
    connectionString <-
        lookupEnv "DB_CONNECTION_STRING" >>= \case
            Nothing -> throwString "No connection string"
            Just c -> pure $ encodeUtf8 $ T.pack c
    newPool
        $ defaultPoolConfig
            (Database.PostgreSQL.Simple.connectPostgreSQL connectionString)
            Database.PostgreSQL.Simple.close
            60
            (max 10 numCapabilities)

withSqlConnection :: (Database.PostgreSQL.Simple.Connection -> IO b) -> AppM b
withSqlConnection f = do
    pool <- asks sqlConnectionPool
    liftIO $ withResource pool f
