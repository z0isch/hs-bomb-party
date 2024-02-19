module Main (main) where

import CustomPrelude

import App (App (..), ClassicApp (..))
import CaseInsensitive (CaseInsensitiveText (..))
import qualified Classic.AppGameState
import qualified Classic.Game
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
            Classic.AppGameState.AppGameState
                { stateKey = 0
                , game =
                    Classic.AppGameState.InLobby
                        $ Classic.Game.initialSettings
                            stdGen
                            wordsSet
                            givenLettersSet
                , events = mempty
                , ..
                }

    wsGameChan <- newBroadcastTChanIO
    wsGameStateTimer <- newTVarIO Nothing
    let classic = ClassicApp{..}

    logOptions' <- logOptionsHandle stderr False
    let logOptions = setLogUseTime True logOptions'

    portEnv <- lookupEnv "APP_PORT"
    let port = fromMaybe 8080 $ readMaybe @Int =<< portEnv

    withLogFunc logOptions $ \logFunction -> run port $ app App{..} Nothing
