module Main (main) where

import CustomPrelude

import App (App (..), AppGameState (..), Game (..))
import CaseInsensitive (CaseInsensitiveText (..))
import Game (initialSettings)
import Network.Wai.Handler.Warp
import qualified RIO.HashSet as HashSet
import qualified RIO.Text as T
import Server (app)
import System.Environment (lookupEnv)
import System.Random (newStdGen)

main :: IO ()
main = do
    wordsFile <- fromMaybe "words.txt" <$> lookupEnv "WORDS_FILE"
    wordsSet <- HashSet.fromList . fmap CaseInsensitiveText . T.lines <$> readFileUtf8 wordsFile

    givenLettersFile <- fromMaybe "histogram.csv" <$> lookupEnv "GIVEN_LETTERS_FILE"
    -- Have at least 100 words per given letter set
    givenLettersSet <- take 2211 . fmap (CaseInsensitiveText . T.takeWhile (/= ',')) . T.lines <$> readFileUtf8 givenLettersFile

    staticDir <- fromMaybe "static" <$> lookupEnv "STATIC_DIR"

    stdGen <- newStdGen

    wsGameState <-
        newTVarIO
            AppGameState
                { stateKey = 0
                , game =
                    InLobby
                        $ initialSettings
                            stdGen
                            wordsSet
                            givenLettersSet
                , events = mempty
                , ..
                }

    wsGameChan <- newBroadcastTChanIO
    wsGameStateTimer <- newTVarIO Nothing

    logOptions' <- logOptionsHandle stderr False
    let logOptions = setLogUseTime True logOptions'

    portEnv <- lookupEnv "APP_PORT"
    let port = fromMaybe 8080 $ readMaybe @Int =<< portEnv

    withLogFunc logOptions $ \logFunction -> run port $ app App{..} Nothing
