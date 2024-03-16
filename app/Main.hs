module Main (main) where

import CustomPrelude

import App (App (..), ClassicApp (..), SurvivalApp (..))
import qualified Classic.AppGameState
import qualified Classic.Game
import qualified Data.Binary as Binary
import Network.Wai.Handler.Warp
import qualified RIO.Text as T
import Server (app)
import qualified Survival.AppGameState
import qualified Survival.Game
import System.Environment (lookupEnv)
import System.Random (newStdGen)

main :: IO ()
main = do
    -- wordsFile <- fromMaybe "words.txt" <$> lookupEnv "WORDS_FILE"
    -- wordsSet <- T.lines <$> readFileUtf8 wordsFile

    -- givenLettersFile <- fromMaybe "histogram.csv" <$> lookupEnv "GIVEN_LETTERS_FILE"
    -- -- Have at least 200 words per given letter set
    -- givenLettersSet <- take 1490 . fmap (T.takeWhile (/= ',')) . T.lines <$> readFileUtf8 givenLettersFile

    lettersMap <- Binary.decodeFile . fromMaybe "letters-map" =<< lookupEnv "LETTERS_MAP_FILE"

    staticDir <- fromMaybe "static" <$> lookupEnv "STATIC_DIR"

    dbConnectionString <-
        lookupEnv "DB_CONNECTION_STRING" >>= \case
            Nothing -> throwString "No connection string"
            Just c -> pure $ encodeUtf8 $ T.pack c

    stdGen <- newStdGen

    wsGameState <-
        newTVarIO
            Classic.AppGameState.AppGameState
                { stateKey = 0
                , game =
                    Classic.AppGameState.InLobby
                        $ Classic.Game.initialSettings
                            stdGen
                            lettersMap
                , events = mempty
                , ..
                }

    wsGameChan <- newBroadcastTChanIO
    wsGameStateTimer <- newTVarIO Nothing
    let classic = ClassicApp{..}

    stdGen <- newStdGen

    wsGameState <-
        newTVarIO
            Survival.AppGameState.AppGameState
                { stateKey = 0
                , game =
                    Survival.AppGameState.InLobby
                        $ Survival.Game.initialSettings
                            stdGen
                            lettersMap
                , events = mempty
                , ..
                }

    wsGameChan <- newBroadcastTChanIO
    wsGameStateTimer <- newTVarIO Nothing
    let survival = SurvivalApp{..}

    logOptions' <- logOptionsHandle stderr False
    let logOptions = setLogUseTime True logOptions'

    portEnv <- lookupEnv "APP_PORT"
    let port = fromMaybe 8080 $ readMaybe @Int =<< portEnv

    withLogFunc logOptions $ \logFunction -> run port $ app App{..} Nothing
