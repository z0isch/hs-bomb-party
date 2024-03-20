{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}

module Survival.Views (homeUI, gameStateUI, PlayerStateUIFor (..), playerStateUI, guessInput) where

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
import qualified RIO.List as L
import qualified RIO.Text as T
import StateKey (StateKey)
import Survival.AppGameState (AppGame (..), AppGameState (..))
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

hyperscript :: Text -> Attribute
hyperscript = makeAttribute "_"

classNames :: [Text] -> Attribute
classNames = class_ . T.intercalate " "

styles :: [Text] -> Attribute
styles = style_ . T.intercalate ";"

homeUI ::
    Maybe (Html ()) ->
    Text ->
    PlayerId ->
    AppGameState ->
    Html ()
homeUI mHotreload wsUrl me appGameState = doctypehtml_ $ html_ $ do
    head_ $ head_ $ do
        meta_ [charset_ "UTF-8"]
        meta_ [name_ "viewport_", content_ "width=device-width, initial-scale=1.0"]
        link_ [rel_ "icon", href_ "data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>💣</text></svg>"]
        link_ [href_ "https://fonts.googleapis.com/css?family=Press+Start+2P", rel_ "stylesheet"]
        link_ [href_ "https://unpkg.com/nes.css@2.3.0/css/nes.min.css", rel_ "stylesheet"]
        link_ [href_ "/static/css/base.css", rel_ "stylesheet"]
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
    body_ [style_ "background:lightslategrey"]
        $ main_
            [ id_ "ws"
            , hxExt_ "ws,game-state-ws,morph"
            , makeAttribute "ws-connect" $ "/" <> wsUrl
            , styles ["margin-left:10px", "margin-right:10px"]
            ]
        $ gameStateUI me (appGameState ^. #stateKey) (appGameState ^. #game) Nothing

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
        $ case game of
            InLobby settings -> lobbyUI me settings
            InGame gs -> gameUI me events gs
            BetweenRounds gs -> betweenRoundsUI me gs

lobbyUI ::
    PlayerId ->
    Settings ->
    Html ()
lobbyUI me settings = do
    h1_ [] "Survival"
    a_ [href_ "/"] "Classic"
    hr_ []
    div_ [classNames ["nes-container", "with-title", "is-centered", "is-dark"]] $ do
        p_ [class_ "title"] "Settings"
        form_
            [ makeAttribute "ws-send" ""
            , hxVals_ [st|{"tag":"Settings"}|]
            , hxTrigger_ "change"
            ]
            $ do
                label_ [Lucid.for_ "secondsToGuess"] "Seconds per guess"
                input_
                    [ id_ "secondsToGuess"
                    , name_ "secondsToGuess"
                    , autocomplete_ "off"
                    , type_ "number"
                    , min_ "1"
                    , value_ $ tshow $ settings ^. #secondsToGuess
                    , class_ "nes-input"
                    ]
                label_ [Lucid.for_ "freeLetterAwardLength"] "Free Letter Award Length"
                input_
                    [ id_ "freeLetterAwardLength"
                    , name_ "freeLetterAwardLength"
                    , autocomplete_ "off"
                    , type_ "number"
                    , min_ "1"
                    , value_ $ tshow $ settings ^. #freeLetterAwardLength
                    , class_ "nes-input"
                    ]
    br_ []
    div_ [classNames ["nes-container", "with-title", "is-centered", "is-dark"]] $ do
        p_ [class_ "title"] "Players"
        for_ (HashMap.toList $ settings ^. #players) $ \(pId, mName) -> p_ $ div_ [style_ "display:flex"] $ do
            let isMe = pId == me
            button_
                [ type_ "button"
                , classNames ["nes-btn", "is-error"]
                , makeAttribute "ws-send" ""
                , hxVals_ [st|{"tag":"Leave", "playerId":"#{UUID.toText $ getPlayerId pId}"}|]
                ]
                "✖"
            input_
                $ [ name_ "name"
                  , value_ $ fromMaybe (T.pack $ show $ getPlayerId pId) mName
                  , makeAttribute "ws-send" ""
                  , hxVals_ [st|{"tag":"Name", "playerId":"#{UUID.toText $ getPlayerId pId}"}|]
                  , autocomplete_ "off"
                  , class_ "nes-input"
                  ]
                <> [disabled_ "" | not isMe]
    br_ []
    unless (me `HashMap.member` (settings ^. #players))
        $ button_
            [ type_ "button"
            , classNames ["nes-btn", "is-primary"]
            , makeAttribute "ws-send" ""
            , hxVals_ [st|{"tag":"Join"}|]
            ]
            "Join Game"
    unless (null $ settings ^. #players)
        $ button_
            [ type_ "button"
            , classNames ["nes-btn", "is-success"]
            , makeAttribute "ws-send" ""
            , hxVals_ [st|{"tag":"Start"}|]
            ]
            "Start Game"

betweenRoundsUI :: PlayerId -> GameState -> Html ()
betweenRoundsUI me gs = do
    div_ [style_ "text-align:center"] $ do
        div_
            [ classNames ["nes-balloon", "from-left"]
            , hyperscript
                [st|
                            on htmx:load from window
                                for x in [5,4,3,2,1]
                                    put x into #countdown
                                    wait 1s
                                end
                            end
                            |]
            ]
            $ div_
                [ id_ "countdown"
                , style_ "font-size:5em"
                ]
                ""
        h1_
            $ toHtml
            $ "Round "
            <> ( gs
                    ^. #round
                    % to (tshow . (+ 1))
               )
        for_ (gs ^. #examples) $ \(givenLetters, examples) ->
            div_ [classNames ["nes-container", "is-dark"]] $ do
                span_ [style_ "font-size:1.5em"] $ toHtml [st|Previous: #{getCaseInsensitiveText givenLetters} - |]
                let
                    oedLink :: CaseInsensitiveText -> Html ()
                    oedLink (CaseInsensitiveText example) =
                        a_
                            [ href_ [st|https://www.oed.com/search/dictionary/?q=#{example}|]
                            , target_ "_blank"
                            , style_ "font-size:1.5em"
                            ]
                            $ toHtml example
                htmlIntercalate ", " $ oedLink <$> examples
        hr_ []
        div_
            [ id_ "player-states"
            , styles ["display:flex", "flex-flow:row wrap"]
            ]
            $ do
                let (mMine, pss) = playerFirst me $ gs ^. #players
                for_ mMine $ \mine ->
                    div_ [style_ "width:100%"]
                        $ playerStateUI me gs mine PlayerStateUIInBetweenRounds
                for_ pss $ \ps ->
                    p_
                        $ playerStateUI me gs ps PlayerStateUIInBetweenRounds

htmlIntercalate :: Html () -> [Html ()] -> Html ()
htmlIntercalate sep = go
  where
    go [] = pure ()
    go [x] = x
    go (x : xs) = x >> sep >> go xs

gameUI :: PlayerId -> Maybe (Seq GameStateEvent) -> GameState -> Html ()
gameUI me events gs = do
    button_
        [ type_ "button"
        , classNames ["nes-btn", "is-error"]
        , tabindex_ "-1"
        , makeAttribute "ws-send" ""
        , hxVals_ [st|{"tag":"StartOver"}|]
        ]
        "Start A New Game"
    hr_ []
    unless (isGameOver gs) $ do
        div_ [classNames ["nes-balloon", "from-left"]]
            $ div_
                [ id_ "given-letters"
                , style_ "font-size:5em"
                ]
            $ toHtml (gs ^. #givenLetters)
    div_
        [ id_ "player-states"
        , styles ["display:flex", "flex-flow:row wrap"]
        ]
        $ do
            let (mMine, pss) = playerFirst me $ gs ^. #players
            for_ mMine $ \mine ->
                div_ [style_ "width:100%"]
                    $ playerStateUI me gs mine
                    $ PlayerStateUIInRound events
                    $ maybe "" getCaseInsensitiveText
                    $ mine
                    ^. #lastUsedWord
            for_ pss $ \ps ->
                p_
                    $ playerStateUI me gs ps
                    $ PlayerStateUIInRound events
                    $ maybe "" getCaseInsensitiveText
                    $ ps
                    ^. #lastUsedWord

data PlayerStateUIFor
    = PlayerStateUIInRound (Maybe (Seq GameStateEvent)) Text
    | PlayerStateUIInBetweenRounds

playerStateUI ::
    PlayerId ->
    GameState ->
    PlayerState ->
    PlayerStateUIFor ->
    Html ()
playerStateUI me gs ps playerStateUIFor = do
    div_
        [ id_ $ "player-state-" <> UUID.toText (getPlayerId $ ps ^. #id)
        , classNames ["nes-container", "with-title", "is-centered", "is-dark"]
        , styles [bg, "margin-bottom:20px", "margin-right:2px;"]
        , makeAttribute "data-game-state-events" (decodeUtf8Lenient $ BSL.toStrict $ Aeson.encode events)
        ]
        $ do
            p_ [class_ "title"] $ maybe (toHtml $ T.pack $ show $ getPlayerId $ ps ^. #id) toHtml $ ps ^. #name
            if isGameOver gs
                then
                    if isPlayerAlive ps
                        then do
                            h1_ [classNames ["text-center", "text-2xl"]] "✨✨✨✨✨WINNER✨✨✨✨✨"
                            for_ previousWord $ \w ->
                                input_
                                    [ class_ "nes-input"
                                    , style_ "font-size: 2em"
                                    , value_ $ getCaseInsensitiveText w
                                    , disabled_ ""
                                    ]
                        else h1_ [classNames ["text-center", "text-2xl"]] "☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️☠️"
                else do
                    letterUI ps
                    section_ [id_ $ "player-state-lives-" <> UUID.toText (getPlayerId me)]
                        $ replicateM_ (ps ^. #lives)
                        $ term "i" [classNames ["nes-icon", "is-medium heart"]] ""
                    val
  where
    events = case playerStateUIFor of
        PlayerStateUIInBetweenRounds -> Nothing
        PlayerStateUIInRound es _ -> es
    val = case playerStateUIFor of
        PlayerStateUIInBetweenRounds -> for_ previousWord $ \w ->
            input_
                [ class_ "nes-input"
                , style_ "font-size: 2em"
                , value_ $ getCaseInsensitiveText w
                , disabled_ ""
                ]
        PlayerStateUIInRound _ v -> form_
            [ id_ $ "player-state-form-" <> UUID.toText (getPlayerId me)
            , makeAttribute "ws-send" ""
            , hxVals_ [st|{"tag":"Guess"}|]
            ]
            $ do
                guessInput
                    (gs ^. #settings)
                    v
                    (ps ^. #id == me && isPlayerActive gs (ps ^. #id))
                    (ps ^. #id)
    previousWord = case playerStateUIFor of
        PlayerStateUIInBetweenRounds -> join $ ps ^. #wordsUsedStack % to L.headMaybe
        PlayerStateUIInRound _ _ -> ps ^. #lastUsedWord
    bg :: Text
    bg = case playerStateUIFor of
        PlayerStateUIInBetweenRounds
            | gs ^. #round == 0 -> ""
            | otherwise -> if isJust previousWord then "background-color:darkolivegreen" else "background-color:darkred"
        PlayerStateUIInRound _ _
            | isGameOver gs -> if isPlayerAlive ps then "background-color:darkgoldenrod" else "background-color:darkred"
            | isPlayerAlive ps -> if isJust previousWord then "background-color:darkolivegreen" else ""
            | otherwise -> "background-color:darkred"

playerInputId :: PlayerId -> Text
playerInputId playerId = "input-" <> UUID.toText (getPlayerId playerId)

guessInput :: Settings -> Text -> Bool -> PlayerId -> Html ()
guessInput settings v enabled playerId = div_
    [ id_ $ "guessInput-" <> UUID.toText (getPlayerId playerId)
    , hxSwapOob_ "morph"
    ]
    $ do
        let
            freeLetterAwardLength = settings ^. #freeLetterAwardLength
            percent = min 100 $ 100 * T.length v `div` freeLetterAwardLength
            updateLetterCount =
                [st|on keyup from ##{playerInputId playerId}
                put target.value.length into me
                set @value to `${Math.min(100,Math.round(100 * (target.value.length / #{freeLetterAwardLength})))}`
                if target.value.length is greater than (#{freeLetterAwardLength} - 1)
                    then add .is-warning remove .is-primary
                    else add .is-primary remove .is-warning
                end
                |]
        progress_
            [ classNames ["nes-progress", if T.length v > settings ^. #freeLetterAwardLength then "is-warning" else "is-primary"]
            , max_ "100"
            , value_ $ textDisplay percent
            , hyperscript updateLetterCount
            , style_ "height:25px"
            ]
            $ toHtml
            $ show
            $ T.length v
        let
            wrongGuessHandler = [[st|on WrongGuess from elsewhere add .shake wait for animationend then remove .shake end|] | enabled]
        hr_ []
        input_
            ( [ id_ $ playerInputId playerId
              , name_ "guess"
              , class_ "nes-input"
              , style_ "font-size: 2em"
              , value_ v
              , autocomplete_ "off"
              , hxVals_ [st|{"tag":"Typing"}|]
              , hyperscript $ T.unlines wrongGuessHandler
              ]
                <> [disabled_ "" | not enabled]
                <> [autofocus_ | enabled]
                <> [makeAttribute "ws-send" "" | enabled]
                <> [hxTrigger_ "keyup changed delay:50ms" | enabled]
            )

letterUI :: PlayerState -> Html ()
letterUI ps = h2_ $ for_ [(CaseInsensitiveChar 'A') .. (CaseInsensitiveChar 'Z')] $ \l -> do
    let
        isFree = l `HashSet.member` (ps ^. #freeLetters)
        weight = if l `HashSet.member` (ps ^. totalLettersL) then "is-primary" else "is-disabled"
        color = if isFree then "is-warning" else ""
        playerID = ps ^. #id
    span_
        ( [classNames ["nes-text", weight, color]]
            <> [hyperscript [st|on FreeLetterAward[playerID == '#{playerID}' and char == '#{l}'] from elsewhere add .grow wait for animationend then remove .grow|]]
        )
        $ toHtml l

playerFirst :: PlayerId -> HashMap PlayerId PlayerState -> (Maybe PlayerState, [PlayerState])
playerFirst pId players = (players ^? ix pId, HashMap.elems (HashMap.delete pId players))
