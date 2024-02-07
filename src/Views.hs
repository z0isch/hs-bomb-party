{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

module Views (APIConstraints, gameStateUI, guessInput, sharedHead) where

import CustomPrelude

import App (Game (..), StateKey)
import CaseInsensitive (CaseInsensitiveChar (..), CaseInsensitiveText (..))
import CircularZipper (CircularZipper (..))
import qualified CircularZipper as CZ
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import Game (
    GameState (..),
    PlayerState (..),
    Settings (..),
    isGameOver,
    isPlayerAlive,
    isPlayerTurn,
    totalLettersL,
 )
import GameStateEvent (GameStateEvent)
import Lucid hiding (for_)
import qualified Lucid
import Lucid.Base (makeAttribute)
import Lucid.Htmx
import Lucid.Htmx.Servant (hxPostSafe_)
import qualified RIO.ByteString.Lazy as BSL
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import qualified RIO.Text as T
import Servant
import Servant.HTML.Lucid
import Text.Shakespeare.Text (st)
import WithPlayerApi (PlayerId (..))

type APIConstraints api =
    ( IsElem
        ( Capture "stateKey" StateKey
            :> "leave"
            :> Post '[HTML] NoContent
        )
        api
    , IsElem
        ( Capture "stateKey" StateKey
            :> "join"
            :> Post '[HTML] NoContent
        )
        api
    , IsElem
        ( Capture "stateKey" StateKey
            :> "start"
            :> Post '[HTML] NoContent
        )
        api
    , IsElem
        ( Capture "stateKey" StateKey
            :> "settings"
            :> Post '[HTML] NoContent
        )
        api
    , IsElem
        ( Capture "stateKey" StateKey
            :> "name"
            :> Post '[HTML] NoContent
        )
        api
    , IsElem
        ( Capture "stateKey" StateKey
            :> "start-over"
            :> Post '[HTML] NoContent
        )
        api
    , IsElem
        ( Capture "stateKey" StateKey
            :> "guess"
            :> Post '[HTML] NoContent
        )
        api
    )

sharedHead :: Maybe (Html ()) -> Html ()
sharedHead mHotreload = head_ $ do
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport_", content_ "width=device-width, initial-scale=1.0"]
    link_ [rel_ "icon", href_ "data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>💣</text></svg>"]
    link_ [href_ "/static/css/index.css", rel_ "stylesheet"]
    script_ [src_ "https://cdn.tailwindcss.com"] ("" :: String)
    script_
        [ src_ "https://unpkg.com/htmx.org@1.9.10"
        , integrity_ "sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC"
        , crossorigin_ "anonymous"
        ]
        ("" :: String)
    script_ [src_ "https://unpkg.com/hyperscript.org@0.9.12"] ("" :: String)
    script_ [src_ "https://unpkg.com/htmx.org/dist/ext/ws.js"] ("" :: String)
    script_ [src_ "/static/js/index.js"] ("" :: String)
    title_ "BombParty"
    sequenceA_ mHotreload

gameStateUI ::
    ( APIConstraints api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    Game ->
    Maybe (Seq GameStateEvent) ->
    Html ()
gameStateUI api me stateKey game events = div_
    [ id_ "gameState"
    , makeAttribute "data-state-key" (tshow stateKey)
    , makeAttribute "data-game-state-events" (decodeUtf8Lenient $ BSL.toStrict $ Aeson.encode events)
    ]
    $ do
        case game of
            InLobby settings -> do
                h1_ "Settings"
                div_ $ do
                    label_ [Lucid.for_ "secondsToGuess"] "Seconds per  guess"
                    input_
                        [ id_ "secondsToGuess"
                        , name_ "secondsToGuess"
                        , class_ "border-2 caret-blue-900"
                        , autocomplete_ "off"
                        , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateKey" StateKey :> "settings" :> Post '[HTML] NoContent)) stateKey
                        , type_ "number"
                        , value_ $ tshow $ settings ^. #secondsToGuess
                        ]
                h1_ "Players"
                ul_ $ for_ (HashMap.toList $ settings ^. #players) $ \(pId, mName) -> li_ $ do
                    button_
                        [ type_ "button"
                        , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-red-500 text-white hover:bg-red-600 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                        , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateKey" StateKey :> "leave" :> Post '[HTML] NoContent)) stateKey
                        , hxTarget_ "#gameState"
                        , hxVals_ [st|{"playerId":"#{UUID.toText $ getPlayerId pId}"}|]
                        ]
                        "x"
                    let isMe = pId == me
                    input_
                        $ [ class_ $ "w-1/3 border-2 caret-blue-900" <> (if isMe then "" else " bg-slate-300")
                          , name_ "name"
                          , value_ $ fromMaybe (T.pack $ show $ getPlayerId pId) mName
                          , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateKey" StateKey :> "name" :> Post '[HTML] NoContent)) stateKey
                          , hxVals_ [st|{"playerId":"#{UUID.toText $ getPlayerId pId}"}|]
                          , autocomplete_ "off"
                          ]
                        <> [disabled_ "" | not isMe]
                unless (me `HashMap.member` (settings ^. #players))
                    $ button_
                        [ type_ "button"
                        , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-blue-600 text-white hover:bg-blue-700 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                        , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateKey" StateKey :> "join" :> Post '[HTML] NoContent)) stateKey
                        , hxTarget_ "#gameState"
                        ]
                        "Join Game"
                unless (null $ settings ^. #players)
                    $ button_
                        [ type_ "button"
                        , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-green-500 text-white hover:bg-green-600 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                        , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateKey" StateKey :> "start" :> Post '[HTML] NoContent)) stateKey
                        , hxTarget_ "#gameState"
                        ]
                        "Start Game"
            InGame gs -> do
                button_
                    [ type_ "button"
                    , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-red-500 text-white hover:bg-red-600 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                    , tabindex_ "-1"
                    , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateKey" StateKey :> "start-over" :> Post '[HTML] NoContent)) stateKey
                    , hxTarget_ "#gameState"
                    ]
                    "Start A New Game"
                unless (isGameOver gs)
                    $ div_
                        [ id_ "given-letters"
                        , class_ "text-2xl font-mono"
                        ]
                    $ toHtml (gs ^. #givenLetters)
                ul_
                    [ id_ "player-states"
                    , class_ "space-y-3"
                    ]
                    $ do
                        traverse_ (playerStateUI api me stateKey gs) $ playerFirst me $ gs ^. #players

playerStateUI ::
    ( IsElem
        ( Capture "stateKey" StateKey
            :> "guess"
            :> Post '[HTML] NoContent
        )
        api
    ) =>
    Proxy api ->
    PlayerId ->
    StateKey ->
    GameState ->
    PlayerState ->
    Html ()
playerStateUI api me stateKey gs ps = do
    li_
        [ id_ $ "player-state-" <> UUID.toText (getPlayerId me)
        , class_ $ "p-2 rounded-lg " <> bg <> " " <> outline
        ]
        $ do
            h1_ $ maybe (toHtml $ T.pack $ show $ getPlayerId $ ps ^. #id) toHtml $ ps ^. #name
            if isPlayerAlive ps
                then
                    if isGameOver gs
                        then h1_ [class_ "text-center text-2xl"] "✨✨✨✨✨WINNER✨✨✨✨✨"
                        else do
                            letterUI ps
                            div_ [id_ $ "player-state-lives-" <> UUID.toText (getPlayerId me)] $ replicateM_ (ps ^. #lives) $ toHtml ("❤️" :: String)
                            form_
                                [ id_ $ "player-state-form-" <> UUID.toText (getPlayerId me)
                                , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateKey" StateKey :> "guess" :> Post '[HTML] NoContent)) stateKey
                                , hxTarget_ "#gameState"
                                ]
                                $ do
                                    guessInput
                                        (maybe "" getCaseInsensitiveText $ ps ^. #lastUsedWord)
                                        (ps ^. #id == me && isPlayerTurn (gs ^. #players) ps)
                                        (ps ^. #id)
                else h1_ [class_ "text-center text-2xl"] "☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️"
  where
    outline =
        if isPlayerTurn (gs ^. #players) ps
            then if isGameOver gs then "bg-emerald-600" else "border-4 border-blue-700"
            else "border-2"
    bg =
        if isPlayerAlive ps
            then if isGameOver gs then "bg-green-600" else ""
            else "bg-red-600"

playerInputId :: PlayerId -> Text
playerInputId playerId = "input-" <> UUID.toText (getPlayerId playerId)

guessInput :: Text -> Bool -> PlayerId -> Html ()
guessInput v enabled playerId = do
    input_
        ( [ id_ $ playerInputId playerId
          , name_ "guess"
          , class_
                $ "border-2 caret-blue-900"
                <> ( if enabled
                        then ""
                        else " bg-slate-300"
                   )
          , value_ v
          , autocomplete_ "off"
          ]
            <> [disabled_ "" | not enabled]
            <> [autofocus_ | enabled]
            <> [makeAttribute "ws-send" "" | enabled]
            <> [hxTrigger_ "keyup changed delay:50ms" | enabled]
            <> [makeAttribute "_" "on WrongGuess from elsewhere add .shake" | enabled]
        )

letterUI :: PlayerState -> Html ()
letterUI ps = for_ [(CaseInsensitiveChar 'A') .. (CaseInsensitiveChar 'Z')] $ \l -> do
    let
        isFree = l `HashSet.member` (ps ^. #freeLetters)
        weight = if l `HashSet.member` (ps ^. totalLettersL) then " font-extrabold" else " font-extralight"
        color = if isFree then " text-rose-600" else ""
    span_
        [class_ $ "tracking-widest" <> weight <> color]
        $ toHtml l

playerFirst :: PlayerId -> CircularZipper PlayerState -> [PlayerState]
playerFirst pId cz = CZ.current playerCurrent : CZ.rights playerCurrent <> CZ.lefts playerCurrent
  where
    playerCurrent = fromMaybe cz $ CZ.findRight ((== pId) . view #id) cz
