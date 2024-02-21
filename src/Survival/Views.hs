{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

module Survival.Views (gameStateUI, playerStateUI, guessInput, sharedHead) where

import CustomPrelude

import CaseInsensitive (CaseInsensitiveChar (..), CaseInsensitiveText (..))
import qualified Data.Aeson as Aeson
import qualified Data.UUID as UUID
import Lucid hiding (for_)
import qualified Lucid
import Lucid.Base (makeAttribute)
import Lucid.Htmx
import qualified RIO.ByteString.Lazy as BSL
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import qualified RIO.Text as T
import StateKey (StateKey)
import Survival.AppGameState (AppGame (..))
import Survival.Game (
    GameState (..),
    PlayerState (..),
    Settings (..),
    isGameOver,
    isPlayerActive,
    isPlayerAlive,
    totalLettersL,
 )
import Survival.GameStateEvent (GameStateEvent (..))
import Text.Shakespeare.Text (st)
import WithPlayerApi (PlayerId (..))

sharedHead :: Maybe (Html ()) -> Html ()
sharedHead mHotreload = head_ $ do
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport_", content_ "width=device-width, initial-scale=1.0"]
    link_ [rel_ "icon", href_ "data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>💣</text></svg>"]
    link_ [href_ "/static/css/output.css", rel_ "stylesheet"]
    script_
        [ src_ "https://unpkg.com/htmx.org@1.9.10"
        , integrity_ "sha384-D1Kt99CQMDuVetoL1lrYwg5t+9QdHe7NLX/SoJYkXDFfX37iInKRy5xLSi8nO7UC"
        , crossorigin_ "anonymous"
        ]
        ("" :: String)
    script_ [src_ "https://unpkg.com/hyperscript.org@0.9.12"] ("" :: String)
    script_ [src_ "https://unpkg.com/htmx.org/dist/ext/ws.js"] ("" :: String)
    script_ [src_ "https://unpkg.com/idiomorph/dist/idiomorph-ext.min.js"] ("" :: String)
    script_ [src_ "/static/js/survival/index.js"] ("" :: String)
    title_ "BombParty"
    sequenceA_ mHotreload

gameStateUI ::
    PlayerId ->
    StateKey ->
    AppGame ->
    Maybe (Seq GameStateEvent) ->
    Html ()
gameStateUI me stateKey game events =
    div_
        [ id_ "gameState"
        , makeAttribute "data-state-key" (tshow stateKey)
        , makeAttribute "data-game-state-events" (decodeUtf8Lenient $ BSL.toStrict $ Aeson.encode events)
        , hxSwapOob_ "morph"
        ]
        $ do
            case game of
                InLobby settings -> do
                    h1_ [class_ "text-4xl"] "Survival"
                    a_ [class_ "text-sm text-blue-600 underline", href_ "/"] "Classic"
                    hr_ [class_ "my-3 h-0.5 border-t-0 bg-neutral-100 opacity-100"]
                    h2_ [class_ "text-2xl"] "Settings"
                    div_ $ do
                        label_ [Lucid.for_ "secondsToGuess"] "Seconds per guess"
                        input_
                            [ id_ "secondsToGuess"
                            , name_ "secondsToGuess"
                            , class_ "border-2 caret-blue-900"
                            , autocomplete_ "off"
                            , type_ "number"
                            , value_ $ tshow $ settings ^. #secondsToGuess
                            , makeAttribute "ws-send" ""
                            , hxVals_ [st|{"tag":"Settings"}|]
                            ]
                    h2_ [class_ "text-2xl"] "Players"
                    ul_ $ for_ (HashMap.toList $ settings ^. #players) $ \(pId, mName) -> li_ $ do
                        button_
                            [ type_ "button"
                            , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-red-500 text-white hover:bg-red-600 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                            , makeAttribute "ws-send" ""
                            , hxVals_ [st|{"tag":"Leave", "playerId":"#{UUID.toText $ getPlayerId pId}"}|]
                            ]
                            "x"
                        let isMe = pId == me
                        input_
                            $ [ class_ $ "w-1/3 border-2 caret-blue-900" <> (if isMe then "" else " bg-slate-300")
                              , name_ "name"
                              , value_ $ fromMaybe (T.pack $ show $ getPlayerId pId) mName
                              , makeAttribute "ws-send" ""
                              , hxVals_ [st|{"tag":"Name", "playerId":"#{UUID.toText $ getPlayerId pId}"}|]
                              , autocomplete_ "off"
                              ]
                            <> [disabled_ "" | not isMe]
                    unless (me `HashMap.member` (settings ^. #players))
                        $ button_
                            [ type_ "button"
                            , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-blue-600 text-white hover:bg-blue-700 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                            , makeAttribute "ws-send" ""
                            , hxVals_ [st|{"tag":"Join"}|]
                            ]
                            "Join Game"
                    unless (null $ settings ^. #players)
                        $ button_
                            [ type_ "button"
                            , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-green-500 text-white hover:bg-green-600 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                            , makeAttribute "ws-send" ""
                            , hxVals_ [st|{"tag":"Start"}|]
                            ]
                            "Start Game"
                InGame gs -> do
                    button_
                        [ type_ "button"
                        , class_ "py-2 px-3 inline-flex items-center gap-x-2 text-sm font-semibold rounded-lg border border-transparent bg-red-500 text-white hover:bg-red-600 disabled:opacity-50 disabled:pointer-events-none dark:focus:outline-none dark:focus:ring-1 dark:focus:ring-gray-600"
                        , tabindex_ "-1"
                        , makeAttribute "ws-send" ""
                        , hxVals_ [st|{"tag":"StartOver"}|]
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
                        $ for_ (playerFirst me $ gs ^. #players)
                        $ \ps -> playerStateUI me gs events ps $ maybe "" getCaseInsensitiveText $ ps ^. #lastUsedWord

classNames :: [Text] -> Attribute
classNames = class_ . T.intercalate " "

playerStateUI ::
    PlayerId ->
    GameState ->
    Maybe (Seq GameStateEvent) ->
    PlayerState ->
    Text ->
    Html ()
playerStateUI me gs events ps v = do
    li_
        [ id_ $ "player-state-" <> UUID.toText (getPlayerId $ ps ^. #id)
        , classNames ["p-2 rounded-lg border-2", bg, border]
        , makeAttribute "data-game-state-events" (decodeUtf8Lenient $ BSL.toStrict $ Aeson.encode events)
        ]
        $ do
            h1_ $ maybe (toHtml $ T.pack $ show $ getPlayerId $ ps ^. #id) toHtml $ ps ^. #name
            if isGameOver gs
                then
                    if isPlayerAlive ps
                        then h1_ [class_ "text-center text-2xl"] "✨✨✨✨✨WINNER✨✨✨✨✨"
                        else h1_ [class_ "text-center text-2xl"] "☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️"
                else do
                    letterUI ps
                    div_ [id_ $ "player-state-lives-" <> UUID.toText (getPlayerId me)] $ replicateM_ (ps ^. #lives) $ toHtml ("❤️" :: String)
                    form_
                        [ id_ $ "player-state-form-" <> UUID.toText (getPlayerId me)
                        , makeAttribute "ws-send" ""
                        , hxVals_ [st|{"tag":"Guess"}|]
                        ]
                        $ do
                            guessInput
                                v
                                (ps ^. #id == me && isPlayerActive gs (ps ^. #id))
                                (ps ^. #id)
  where
    border
        | T.null bg && ps ^. #id == me = "border-blue-500"
        | otherwise = ""
    bg
        | isGameOver gs = if isPlayerAlive ps then "bg-green-600" else "bg-red-600"
        | isPlayerAlive ps = if isJust $ ps ^. #lastUsedWord then "bg-green-100" else ""
        | otherwise = "bg-red-100"

playerInputId :: PlayerId -> Text
playerInputId playerId = "input-" <> UUID.toText (getPlayerId playerId)

guessInput :: Text -> Bool -> PlayerId -> Html ()
guessInput v enabled playerId = div_
    [ id_ $ "guessInput-" <> UUID.toText (getPlayerId playerId)
    , hxSwapOob_ "morph"
    ]
    $ do
        let
            percent = min 100 $ 100 * T.length v `div` 11
            bgColor = if T.length v > 10 then "bg-yellow-500" else "bg-blue-600"
            updateLetterCount =
                [st|on keyup from ##{playerInputId playerId}
                put target.value.length into me
                if target.value.length > 0 
                    then set @style to `width: ${Math.min(100,100 * (target.value.length / 11))}%`
                    else set @style to 'display:none' 
                end
                if target.value.length is greater than 10
                    then add .bg-yellow-500 
                    else remove .bg-yellow-500 
                end
                |]
        div_ [class_ "w-full bg-gray-200 rounded-full h-4 mb-1"]
            $ div_
                [ class_ $ "h-4 text-xs font-medium text-blue-100 text-center p-0.5 leading-none rounded-full " <> bgColor
                , makeAttribute "_" updateLetterCount
                , style_ $ if percent > 0 then [st|width:#{percent}%|] else "display:none"
                ]
            $ toHtml
            $ show
            $ T.length v
        let
            glow =
                [ [st|on keyup
                if target.value.length is greater than 10
                    then add .glow 
                    else remove .glow 
                end|]
                ]
            wrongGuessHandler = [[st|on WrongGuess from elsewhere add .shake wait for animationend then remove .shake end|] | enabled]
        input_
            ( [ id_ $ playerInputId playerId
              , name_ "guess"
              , class_
                    $ "border-2 caret-blue-900"
                    <> ( if enabled
                            then ""
                            else " bg-slate-300"
                       )
                    <> (if T.length v > 10 then " glow" else "")
              , value_ v
              , autocomplete_ "off"
              , hxVals_ [st|{"tag":"Typing"}|]
              , makeAttribute "_" $ T.unlines $ glow <> wrongGuessHandler
              ]
                <> [disabled_ "" | not enabled]
                <> [autofocus_ | enabled]
                <> [makeAttribute "ws-send" "" | enabled]
                <> [hxTrigger_ "keyup changed delay:50ms" | enabled]
            )

letterUI :: PlayerState -> Html ()
letterUI ps = for_ [(CaseInsensitiveChar 'A') .. (CaseInsensitiveChar 'Z')] $ \l -> do
    let
        isFree = l `HashSet.member` (ps ^. #freeLetters)
        weight = if l `HashSet.member` (ps ^. totalLettersL) then " font-extrabold" else " font-extralight"
        color = if isFree then " text-rose-600" else ""
        playerID = ps ^. #id
    span_
        ( [class_ $ "tracking-widest" <> weight <> color]
            <> [makeAttribute "_" [st|on FreeLetterAward[playerID == '#{playerID}' and char == '#{l}'] from elsewhere add .grow wait for animationend then remove .grow|]]
        )
        $ toHtml l

playerFirst :: PlayerId -> HashMap PlayerId PlayerState -> [PlayerState]
playerFirst pId players = catMaybes $ players ^? ix pId : (Just <$> HashMap.elems (HashMap.delete pId players))
