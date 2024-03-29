{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Classic.Game (
    GameState (..),
    Settings (..),
    Move (..),
    PlayerState (..),
    initialPlayerState,
    initialSettings,
    startGame,
    makeMove,
    isGameOver,
    isPlayerAlive,
    isPlayerTurn,
    totalLettersL,
    mkValidWords,
) where

import CustomPrelude

import CaseInsensitive (CaseInsensitiveChar (..), CaseInsensitiveText (..), caseInsensitiveLetters)
import qualified CaseInsensitive
import CircularZipper (CircularZipper (..), currentL, findRight)
import qualified CircularZipper as CZ
import Classic.GameStateEvent (GameStateEvent, GameStateEvents (..))
import qualified Classic.GameStateEvent as GameStateEvent
import Control.Monad (replicateM)
import Control.Monad.RWS (
    MonadState,
    MonadWriter (..),
    RWS,
    execRWS,
    get,
 )
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import RIO.List.Partial ((!!))
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import qualified RIO.Text as T
import System.Random (Random, StdGen, randomR)
import WithPlayerApi (PlayerId (..))

data Settings = Settings
    { validWords :: Map CaseInsensitiveText (Set CaseInsensitiveText)
    , stdGen :: StdGen
    , players :: HashMap PlayerId (Maybe Text)
    , secondsToGuess :: Int
    }
    deriving (Show, Generic)

data GameState = GameState
    { players :: CircularZipper PlayerState
    , givenLetters :: (CaseInsensitiveText, Int)
    , validWords :: Set CaseInsensitiveText
    , examples :: Maybe (CaseInsensitiveText, [CaseInsensitiveText])
    , alreadyUsedWords :: HashSet CaseInsensitiveText
    , settings :: Settings
    , round :: Natural
    }
    deriving (Show, Generic)

data PlayerState = PlayerState
    { id :: PlayerId
    , name :: Maybe Text
    , letters :: HashSet CaseInsensitiveChar
    , lives :: Int
    , tries :: Int
    , lastUsedWord :: Maybe CaseInsensitiveText
    , wordsUsedStack :: [CaseInsensitiveText]
    , freeLetters :: HashSet CaseInsensitiveChar
    }
    deriving (Show, Generic)

totalLettersL :: Getter PlayerState (HashSet CaseInsensitiveChar)
totalLettersL = to $ \p -> (p ^. #freeLetters) `HashSet.union` (p ^. #letters)

initialPlayerState :: PlayerId -> Maybe Text -> PlayerState
initialPlayerState playerId name =
    PlayerState
        { id = playerId
        , name
        , letters = mempty
        , lives = 3
        , tries = 0
        , lastUsedWord = Nothing
        , wordsUsedStack = []
        , freeLetters = mempty
        }

initialSettings :: StdGen -> Map CaseInsensitiveText (Set CaseInsensitiveText) -> Settings
initialSettings stdGen validWords = Settings{players = mempty, secondsToGuess = 7, ..}

startGame :: Settings -> Maybe (GameState, GameStateEvents)
startGame s = case HashMap.toList (s ^. #players) of
    [] -> Nothing
    (p : ps) ->
        let
            (givenLetters, stdGen) = randomGivenLetters (s ^. #stdGen) (s ^. #validWords % to Map.keys)
            players = CZ.fromNonEmpty $ fmap (uncurry initialPlayerState) $ p :| ps
            settings = s{stdGen}
            events = GameStateEvents $ HashMap.singleton (view #id $ CZ.current players) $ pure GameStateEvent.MyTurn
         in
            Just
                ( GameState
                    { alreadyUsedWords = mempty
                    , round = 0
                    , givenLetters = (givenLetters, 0)
                    , validWords = fromMaybe mempty $ Map.lookup givenLetters $ s ^. #validWords
                    , examples = Nothing
                    , ..
                    }
                , events
                )

tellCurrentPlayer :: (MonadWriter GameStateEvents m, MonadState GameState m) => GameStateEvent -> m ()
tellCurrentPlayer = tellEvents (castOptic currentPlayerL)

tellEvents :: (MonadWriter GameStateEvents m, MonadState GameState m) => Fold GameState PlayerState -> GameStateEvent -> m ()
tellEvents l e = do
    ids <- getIds <$> get
    traverse_ tellEvent ids
  where
    getIds = toListOf $ l % #id
    tellEvent = tell . GameStateEvents . (`HashMap.singleton` pure e)

data Move = Guess CaseInsensitiveText | TimeUp

currentPlayerL :: Lens' GameState PlayerState
currentPlayerL = #players % currentL

makeMove :: GameState -> Move -> (GameState, GameStateEvents)
makeMove gs = (\x -> execRWS x () gs) . runMove
  where
    runMove :: Move -> RWS a GameStateEvents GameState ()
    runMove = \case
        Guess guess
            | isValidGuess gs guess -> do
                tellCurrentPlayer GameStateEvent.CorrectGuess
                when (CaseInsensitive.length guess >= 11) $ awardFreeLetter guess
                zoom currentPlayerL $ do
                    #tries .= 0
                    #wordsUsedStack %= (guess :)
                    #lastUsedWord .= Just guess
                    #letters %= HashSet.union (caseInsensitiveLetters guess)
                    let hasUsedAllLetters = totalLettersL % to ((== 26) . HashSet.size)
                    whenM (use hasUsedAllLetters) $ do
                        #lives += 1
                        #letters .= mempty
                        #freeLetters .= mempty
                #alreadyUsedWords %= HashSet.insert guess
                pickNewGivenLetters
                #players %= nextPlayer
                currentPlayerL % #lastUsedWord .= Nothing
                #round += 1
                tellCurrentPlayer GameStateEvent.MyTurn
            | otherwise -> do
                tellCurrentPlayer GameStateEvent.WrongGuess
                currentPlayerL % #tries += 1
        TimeUp -> do
            tellCurrentPlayer GameStateEvent.TimeUp
            zoom currentPlayerL $ do
                #lives -= 1
                #tries .= 0
                #lastUsedWord .= Nothing
            #players %= nextPlayer
            currentPlayerL % #lastUsedWord .= Nothing
            timesLetersUsed <- #givenLetters % _2 <%= (+ 1)
            givenLettersUsedTooManyTimes <-
                use
                    $ #players
                    % to ((<= timesLetersUsed) . length . filter isPlayerAlive . toList)
            when givenLettersUsedTooManyTimes pickNewGivenLetters
            use (to isGameOver) >>= \case
                False -> do
                    #round += 1
                    tellCurrentPlayer GameStateEvent.MyTurn
                True -> do
                    tellCurrentPlayer GameStateEvent.IWin
                    tellEvents (#players % folding CZ.others) GameStateEvent.ILose
                    tellEvents (#players % folded) GameStateEvent.GameOver

genRandom :: (Random a) => (a, a) -> (MonadState GameState m) => m a
genRandom r = #settings % #stdGen %%= randomR r

pickNewGivenLetters :: (MonadState GameState m) => m ()
pickNewGivenLetters = do
    pickExamples
    currentGivenLetters <- use $ #givenLetters % _1
    allButCurrent <- use $ #settings % #validWords % to (Map.delete currentGivenLetters)
    i <- genRandom (0, length allButCurrent - 1)
    let (givenLetters, _) = Map.elemAt i allButCurrent
    #givenLetters .= (givenLetters, 0)
    #validWords .= fromMaybe mempty (Map.lookup givenLetters allButCurrent)

pickExamples :: (MonadState GameState m) => m ()
pickExamples = do
    currentGivenLetters <- use $ #givenLetters % _1
    validWords <- use $ #validWords % to (filter ((>= 11) . CaseInsensitive.length) . toList)
    unless (null validWords) $ do
        randomIdxs <- replicateM 3 $ genRandom (0, length validWords - 1)
        #examples .= Just (currentGivenLetters, (validWords !!) <$> randomIdxs)

awardFreeLetter ::
    (MonadState GameState m, MonadWriter GameStateEvents m) =>
    CaseInsensitiveText ->
    m ()
awardFreeLetter guess = do
    totalLetters <- use $ currentPlayerL % totalLettersL
    pID <- use $ currentPlayerL % #id
    let
        allLetters = HashSet.fromList $ CaseInsensitiveChar <$> ['A' .. 'Z']
        openLetters =
            foldr
                (flip HashSet.difference)
                allLetters
                [totalLetters, caseInsensitiveLetters guess]
    i <- genRandom (0, length openLetters - 1)
    let letter = toList openLetters !! i
    currentPlayerL % #freeLetters %= HashSet.insert letter
    tellEvents (#players % folded) $ GameStateEvent.FreeLetterAward letter pID

nextPlayer :: CircularZipper PlayerState -> CircularZipper PlayerState
nextPlayer z = fromMaybe z $ findRight isPlayerAlive z

randomGivenLetters :: StdGen -> [CaseInsensitiveText] -> (CaseInsensitiveText, StdGen)
randomGivenLetters stdGen givenLettersSet = let (i, stdGen') = randomR (0, length givenLettersSet - 1) stdGen in (givenLettersSet !! i, stdGen')

isGameOver :: GameState -> Bool
isGameOver gs = (== 1) $ length $ filter isPlayerAlive $ toList $ gs ^. #players

isPlayerAlive :: PlayerState -> Bool
isPlayerAlive ps = ps ^. #lives > 0

isValidGuess :: GameState -> CaseInsensitiveText -> Bool
isValidGuess gs g =
    ((gs ^. #givenLetters % _1) `CaseInsensitive.isInfixOf` g)
        && not (g `HashSet.member` (gs ^. #alreadyUsedWords))
        && (g `Set.member` (gs ^. #validWords))

isPlayerTurn :: CircularZipper PlayerState -> PlayerState -> Bool
isPlayerTurn z ps = CZ.current z ^. #id == ps ^. #id

mkValidWords :: [Text] -> [Text] -> Map CaseInsensitiveText (Set CaseInsensitiveText)
mkValidWords ws givenLetters =
    Map.fromListWith (<>)
        $ fmap (\givenLetter -> (CaseInsensitiveText givenLetter, Set.fromList (CaseInsensitiveText <$> filter (\w -> givenLetter `T.isInfixOf` w) ws))) givenLetters
