{-# LANGUAGE QuasiQuotes #-}

module DevelMain (update) where

import CustomPrelude

import App (App (..), ClassicApp (..), SurvivalApp (..))
import qualified Classic.AppGameState
import qualified Classic.Game
import qualified Data.Binary as Binary
import qualified Data.HashSet as HashSet
import Lucid (Html, script_, src_)
import Network.HTTP.Types (status400)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ControlMessage (..), Message (..), acceptRequest, defaultConnectionOptions, receive, sendTextData, withPingThread)
import RIO.File (withBinaryFileDurable)
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Rapid (createRef, rapid, restart, start)
import Servant.Server
import Server (app)
import qualified Survival.AppGameState
import qualified Survival.Game
import System.Random (mkStdGen)
import Text.Shakespeare.Text (st)

update :: IO ()
update = do
    rapid 0 $ \r -> do
        reloadChan <- createRef @Text r "reloadChan" newChan
        -- Entire map is too big so let's restrict it to these given letters
        lettersMap <- createRef @Text r "lettersMap" $ (`Map.restrictKeys` Set.fromList ["tha", "the", "acc"]) <$> Binary.decodeFile "letters-map"

        wsGameState <- createRef @Text r "classic.wsGameState" $ do
            newTVarIO
                $ Classic.AppGameState.AppGameState
                    { game =
                        Classic.AppGameState.InLobby
                            $ Classic.Game.initialSettings
                                (mkStdGen 0)
                                lettersMap
                    , stateKey = 0
                    , events = mempty
                    , ..
                    }

        wsGameChan <- createRef @Text r "classic.wsGameStateChan" newBroadcastTChanIO

        wsGameStateTimer <- createRef @Text r "classic.wsGameStateTimer" $ newTVarIO Nothing
        let classic = ClassicApp{..}

        wsGameState <- createRef @Text r "survival.wsGameState" $ do
            newTVarIO
                $ Survival.AppGameState.AppGameState
                    { game =
                        Survival.AppGameState.InLobby
                            $ Survival.Game.initialSettings
                                (mkStdGen 0)
                                lettersMap
                    , stateKey = 0
                    , events = mempty
                    , ..
                    }

        wsGameChan <- createRef @Text r "survival.wsGameStateChan" newBroadcastTChanIO

        wsGameStateTimer <- createRef @Text r "survival.wsGameStateTimer" $ newTVarIO Nothing
        let survival = SurvivalApp{..}

        start r "hotreload" $ run 8081 $ hotReloadServer reloadChan

        restart r "webserver" $ do
            writeChan reloadChan ()
            withBinaryFileDurable "log" ReadWriteMode $ \h -> do
                logOptions <- logOptionsHandle h True
                withLogFunc logOptions
                    $ \logFunction -> do
                        let staticDir = "static"
                        run 8080
                            $ app App{..}
                            $ Just
                            $ hotreloadJs "ws://localhost:8081"

hotReloadServer :: Chan () -> Application
hotReloadServer reloadChan = websocketsOr defaultConnectionOptions hotreloader backup
  where
    hotreloader pc = do
        c <- acceptRequest pc
        myChan <- dupChan reloadChan
        withPingThread c 30 (pure ()) $ do
            let
                handleClose =
                    receive c >>= \case
                        ControlMessage (Close _ _) -> pure ()
                        _ -> handleClose
                hotreload = do
                    _ <- readChan myChan
                    sendTextData @Text c "hotreload"
                    hotreload
            race_ handleClose hotreload
    backup _ resp = resp $ responseLBS status400 [] "Not a WebSocket request"

hotreloadJs :: Text -> Html ()
hotreloadJs uri = do
    script_ [src_ "https://unpkg.com/idiomorph@0.3.0"] ("" :: String)
    script_
        [st|
(function () {
  let timeout = 1000;
  const resetBackoff = () => {
    timeout = 1000;
  };

  const backOff = () => {
    if (timeout > 10 * 1000) {
      return;
    }

    timeout = timeout * 2;
  };

  function connectHotReload() {
    const socket = new WebSocket("#{uri}");

    socket.onmessage = async (e) => {
      Idiomorph.morph(document.documentElement,await (await fetch(location.href)).text())
      if(htmx) htmx.process(document.documentElement)
    };

    socket.onopen = () => {
      resetBackoff();
    };

    socket.onclose = () => {
      const timeoutId = setTimeout(function () {
        clearTimeout(timeoutId);
        backOff();

        connectHotReload();
      }, timeout);
    };
  }

  connectHotReload();
})();
|]
