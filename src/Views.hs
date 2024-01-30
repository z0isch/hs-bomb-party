{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Views where

import CustomPrelude

import App (Game)
import CaseInsensitive (CaseInsensitiveChar (..))
import CircularZipper (CircularZipper (..))
import qualified CircularZipper as CZ
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Game (
    GameState (..),
    PlayerState (..),
    Settings (..),
    isGameOver,
    isPlayerAlive,
    isPlayerTurn,
 )
import Lucid hiding (for_)
import qualified Lucid
import Lucid.Base (makeAttribute)
import Lucid.Htmx
import Lucid.Htmx.Servant (hxPostSafe_)
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import qualified RIO.Text as T
import Servant
import Servant.HTML.Lucid
import Text.Shakespeare.Text (st)
import WithPlayerApi (PlayerId (..))

sharedHead :: Maybe (Html ()) -> Html ()
sharedHead mHotreload = head_ $ do
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport_", content_ "width=device-width, initial-scale=1.0"]
    link_ [rel_ "icon", href_ "data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>💣</text></svg>"]
    script_ [src_ "https://cdn.tailwindcss.com"] ("" :: String)
    script_
        [ src_ "https://unpkg.com/htmx.org@1.9.10"
        , integrity_ "sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC"
        , crossorigin_ "anonymous"
        ]
        ("" :: String)
    script_ [src_ "https://unpkg.com/hyperscript.org@0.9.12"] ("" :: String)
    script_ [src_ "https://unpkg.com/htmx.org/dist/ext/ws.js"] ("" :: String)
    style_ [] shakeAnimationCss
    title_ "BombParty"
    sequenceA_ mHotreload

gameStateUI ::
    ( IsElem
        ( Capture "stateId" UUID
            :> "leave"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateId" UUID
            :> "join"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateId" UUID
            :> "settings"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateId" UUID
            :> "name"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateId" UUID
            :> "start"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateId" UUID
            :> "start-over"
            :> Post '[HTML] (Html ())
        )
        api
    , IsElem
        ( Capture "stateId" UUID
            :> "guess"
            :> Post '[HTML] (Html ())
        )
        api
    ) =>
    Proxy api ->
    PlayerId ->
    UUID ->
    Game ->
    Html ()
gameStateUI api me stateId game = div_ [id_ "gameState"] $ do
    case game of
        Left settings -> do
            h1_ "Settings"
            div_ $ do
                label_ [Lucid.for_ "secondsToGuess"] "Seconds per  guess"
                input_
                    [ id_ "secondsToGuess"
                    , name_ "secondsToGuess"
                    , class_ "border-2 caret-blue-900"
                    , autocomplete_ "off"
                    , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateId" UUID :> "settings" :> Post '[HTML] (Html ()))) stateId
                    , type_ "number"
                    , value_ $ tshow settings.secondsToGuess
                    ]
            h1_ "Players"
            ul_ $ for_ (HashMap.toList settings.players) $ \(pId, mName) -> li_ $ do
                button_
                    [ type_ "button"
                    , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-red-500 text-white hover:bg-red-600 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                    , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateId" UUID :> "leave" :> Post '[HTML] (Html ()))) stateId
                    , hxTarget_ "#gameState"
                    , hxVals_ [st|{"playerId":"#{UUID.toText $ getPlayerId pId}"}|]
                    ]
                    "x"
                let isMe = pId == me
                input_
                    $ [ class_ $ "w-1/3 border-2 caret-blue-900" <> (if isMe then "" else " bg-slate-300")
                      , name_ "name"
                      , value_ $ fromMaybe (T.pack $ show $ getPlayerId pId) mName
                      , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateId" UUID :> "name" :> Post '[HTML] (Html ()))) stateId
                      , hxVals_ [st|{"playerId":"#{UUID.toText $ getPlayerId pId}"}|]
                      , autocomplete_ "off"
                      ]
                    <> [disabled_ "" | not isMe]
            unless (me `HashMap.member` settings.players)
                $ button_
                    [ type_ "button"
                    , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-blue-600 text-white hover:bg-blue-700 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                    , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateId" UUID :> "join" :> Post '[HTML] (Html ()))) stateId
                    , hxTarget_ "#gameState"
                    ]
                    "Join Game"
            unless (null settings.players)
                $ button_
                    [ type_ "button"
                    , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-green-500 text-white hover:bg-green-600 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                    , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateId" UUID :> "start" :> Post '[HTML] (Html ()))) stateId
                    , hxTarget_ "#gameState"
                    ]
                    "Start Game"
        Right gs -> do
            button_
                [ type_ "button"
                , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-red-500 text-white hover:bg-red-600 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                , tabindex_ "-1"
                , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateId" UUID :> "start-over" :> Post '[HTML] (Html ()))) stateId
                , hxTarget_ "#gameState"
                ]
                "Start A New Game"
            unless (isGameOver gs)
                $ div_
                    [ id_ "given-letters"
                    , class_ "text-2xl font-mono"
                    ]
                $ toHtml gs.givenLetters
            ul_
                [ id_ "player-states"
                , class_ "space-y-3"
                ]
                $ do
                    traverse_ (playerStateUI api me stateId gs) $ playerFirst me gs.players

playerStateUI ::
    ( IsElem
        ( Capture "stateId" UUID
            :> "guess"
            :> Post '[HTML] (Html ())
        )
        api
    ) =>
    Proxy api ->
    PlayerId ->
    UUID ->
    GameState ->
    PlayerState ->
    Html ()
playerStateUI api me stateId gs ps = do
    li_
        [ id_ $ "player-state-" <> UUID.toText (getPlayerId me)
        , class_ $ "p-2 rounded-lg " <> bg <> " " <> outline
        ]
        $ do
            h1_ $ maybe (toHtml $ T.pack $ show $ getPlayerId ps.id) toHtml ps.name
            if isPlayerAlive ps
                then
                    if isGameOver gs
                        then h1_ [class_ "text-center text-2xl"] "✨✨✨✨✨WINNER✨✨✨✨✨"
                        else do
                            letterUI ps.letters
                            div_ [id_ $ "player-state-lives-" <> UUID.toText (getPlayerId me)] $ replicateM_ ps.lives $ toHtml ("❤️" :: String)
                            form_
                                [ id_ $ "player-state-form-" <> UUID.toText (getPlayerId me)
                                , hxPostSafe_ $ safeLink api (Proxy @(Capture "stateId" UUID :> "guess" :> Post '[HTML] (Html ()))) stateId
                                , hxTarget_ "#gameState"
                                ]
                                $ do
                                    guessInput "" (me == ps.id) (isPlayerTurn gs.players ps) (ps.tries > 0) ps.id
                else h1_ [class_ "text-center text-2xl"] "☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️"
  where
    outline =
        if isPlayerTurn gs.players ps
            then if isGameOver gs then "bg-emerald-600" else "border-4 border-blue-700"
            else "border-2"
    bg =
        if isPlayerAlive ps
            then if isGameOver gs then "bg-green-600" else ""
            else "bg-red-600"

playerInputId :: Bool -> PlayerId -> Text
playerInputId isMe playerId = "input-" <> UUID.toText (getPlayerId playerId) <> "-" <> if isMe then "me" else "other"

guessInput :: Text -> Bool -> Bool -> Bool -> PlayerId -> Html ()
guessInput v isMe isMyTurn invaldGuess playerId = do
    input_
        ( [ id_ $ playerInputId isMe playerId
          , name_ "guess"
          , class_
                $ "border-2 caret-blue-900"
                <> ( if invaldGuess
                        then " shake"
                        else ""
                   )
                <> ( if isActivePlayersTurn
                        then ""
                        else " bg-slate-300"
                   )
          , value_ v
          , autocomplete_ "off"
          ]
            <> [disabled_ "" | not isActivePlayersTurn]
            <> [autofocus_ | isActivePlayersTurn]
            <> [makeAttribute "ws-send" "" | isActivePlayersTurn]
            <> [hxTrigger_ "keyup changed delay:200ms" | isActivePlayersTurn]
        )
  where
    isActivePlayersTurn = isMe && isMyTurn

letterUI :: HashSet CaseInsensitiveChar -> Html ()
letterUI ls = for_ [(CaseInsensitiveChar 'A') .. (CaseInsensitiveChar 'Z')] $ \l -> do
    let weight = if l `HashSet.member` ls then "font-extrabold" else "font-extralight"
    span_ [class_ $ "tracking-widest " <> weight] $ toHtml l

playerFirst :: PlayerId -> CircularZipper PlayerState -> [PlayerState]
playerFirst pId cz = CZ.current playerCurrent : CZ.rights playerCurrent <> CZ.lefts playerCurrent
  where
    playerCurrent = fromMaybe cz $ CZ.findRight ((== pId) . (.id)) cz

shakeAnimationCss :: Text
shakeAnimationCss =
    [st|
@keyframes shake {
  0% {
    margin-left: 0rem;
  }
  25% {
    margin-left: 0.5rem;
  }
  75% {
    margin-left: -0.5rem;
  }
  100% {
    margin-left: 0rem;
  }
}

.shake {
  animation: shake 0.2s ease-in-out 0s 2;
}
|]
