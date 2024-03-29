{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}

module Survival.Game (
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
    isPlayerActive,
    totalLettersL,
) where

import CustomPrelude

import CaseInsensitive (CaseInsensitiveChar (..), CaseInsensitiveText, caseInsensitiveLetters)
import qualified CaseInsensitive
import Control.Monad (replicateM)
import Control.Monad.RWS (
    MonadState,
    MonadWriter (..),
    RWS,
    execRWS,
    get,
    gets,
 )
import Data.Monoid (All (..))
import qualified RIO.HashMap as HashMap
import qualified RIO.HashSet as HashSet
import RIO.List.Partial ((!!))
import qualified RIO.Map as Map
import qualified RIO.Set as Set
import Survival.GameStateEvent (GameStateEvent, GameStateEvents (..))
import qualified Survival.GameStateEvent as GameStateEvent
import System.Random (Random, StdGen, randomR)
import WithPlayerApi (PlayerId (..))

data Settings = Settings
    { validWords :: Map CaseInsensitiveText (Set CaseInsensitiveText)
    , stdGen :: StdGen
    , players :: HashMap PlayerId (Maybe Text)
    , secondsToGuess :: Int
    , freeLetterAwardLength :: Int
    }
    deriving (Show, Generic)

data GameState = GameState
    { players :: HashMap PlayerId PlayerState
    , givenLetters :: CaseInsensitiveText
    , validWords :: Set CaseInsensitiveText
    , alreadyUsedWords :: HashSet CaseInsensitiveText
    , examples :: Maybe (CaseInsensitiveText, [CaseInsensitiveText])
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
    , wordsUsedStack :: [Maybe CaseInsensitiveText]
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
initialSettings stdGen validWords =
    Settings
        { players = mempty
        , secondsToGuess = 7
        , freeLetterAwardLength = 11
        , ..
        }

startGame :: Settings -> Maybe (GameState, GameStateEvents)
startGame s =
    let
        (givenLetters, stdGen) = randomGivenLetters (s ^. #stdGen) (s ^. #validWords % to Map.keys)
        players = HashMap.mapWithKey initialPlayerState (s ^. #players)
        settings = s{stdGen}
        events = GameStateEvents $ fmap (pure . const GameStateEvent.MyTurn) players
     in
        Just
            ( GameState
                { round = 0
                , givenLetters = givenLetters
                , alreadyUsedWords = mempty
                , examples = Nothing
                , validWords = fromMaybe mempty $ Map.lookup givenLetters $ s ^. #validWords
                , ..
                }
            , events
            )

data Move = Guess PlayerId CaseInsensitiveText | TimeUp

genRandom :: (Random a) => (a, a) -> (MonadState GameState m) => m a
genRandom r = #settings % #stdGen %%= randomR r

pickNewGivenLetters :: (MonadState GameState m) => m ()
pickNewGivenLetters = do
    pickExamples
    currentGivenLetters <- use #givenLetters
    allButCurrent <- use $ #settings % #validWords % to (Map.delete currentGivenLetters)
    i <- genRandom (0, length allButCurrent - 1)
    let (givenLetters, _) = Map.elemAt i allButCurrent
    #givenLetters .= givenLetters
    #validWords .= fromMaybe mempty (Map.lookup givenLetters allButCurrent)

pickExamples :: (MonadState GameState m) => m ()
pickExamples = do
    currentGivenLetters <- use #givenLetters
    freeLetterAwardLength <- use $ #settings % #freeLetterAwardLength
    validWords <- use $ #validWords % to (filter ((>= freeLetterAwardLength) . CaseInsensitive.length) . toList)
    unless (null validWords) $ do
        randomIdxs <- replicateM 3 $ genRandom (0, length validWords - 1)
        #examples .= Just (currentGivenLetters, (validWords !!) <$> randomIdxs)

tellEvents :: (MonadWriter GameStateEvents m, MonadState GameState m) => Fold GameState PlayerState -> GameStateEvent -> m ()
tellEvents l e = do
    ids <- getIds <$> get
    traverse_ tellEvent ids
  where
    getIds = toListOf $ l % #id
    tellEvent = tell . GameStateEvents . (`HashMap.singleton` pure e)

tellPlayer :: (MonadState GameState m, MonadWriter GameStateEvents m) => PlayerId -> GameStateEvent -> m ()
tellPlayer playerId = tellEvents (castOptic $ #players % ix playerId)

allPlayersHaveAnswered :: (MonadState GameState m) => m Bool
allPlayersHaveAnswered = getAll <$> gets (foldOf hasAnsweredFold)
  where
    hasAnsweredFold = #players % folded % #lastUsedWord % to (All . isJust)

makeMove :: GameState -> Move -> Maybe (GameState, GameStateEvents)
makeMove gs mv
    | validMove = (\x -> Just $ execRWS x () gs) $ runMove mv
    | otherwise = Nothing
  where
    validMove :: Bool
    validMove
        | isGameOver gs = False
        | otherwise = True
    runMove :: Move -> RWS a GameStateEvents GameState ()
    runMove = \case
        Guess playerId guess
            | isValidGuess gs guess -> do
                tellPlayer playerId GameStateEvent.CorrectGuess
                when (CaseInsensitive.length guess >= gs ^. #settings % #freeLetterAwardLength) $ awardFreeLetter playerId guess
                void $ zoomMaybe (#players % ix playerId) $ do
                    #wordsUsedStack %= (Just guess :)
                    #lastUsedWord .= Just guess
                    #letters %= HashSet.union (caseInsensitiveLetters guess)
                    let hasUsedAllLetters = totalLettersL % to ((== 26) . HashSet.size)
                    whenM (use hasUsedAllLetters) $ do
                        #lives += 1
                        #letters .= mempty
                        #freeLetters .= mempty
                        tell $ GameStateEvent.singletonEvent playerId GameStateEvent.UsedAllLetters
                #alreadyUsedWords %= HashSet.insert guess
                whenM allPlayersHaveAnswered $ do
                    pickNewGivenLetters
                    tellEvents (#players % folded) GameStateEvent.MyTurn
                    #players % traversed % #lastUsedWord .= Nothing
                    #round += 1
            | otherwise -> do
                tellPlayer playerId GameStateEvent.WrongGuess
        TimeUp -> do
            zoomMany (#players % traversed) $ do
                whenM (use $ #lastUsedWord % to isNothing) $ do
                    playerId <- use #id
                    tell $ GameStateEvent.singletonEvent playerId GameStateEvent.TimeUp
                    #lives %= \l -> if l > 0 then l - 1 else l
                    #wordsUsedStack %= (Nothing :)
            use (to isGameOver) >>= \case
                False -> do
                    pickNewGivenLetters
                    tellEvents (#players % folded) GameStateEvent.MyTurn
                    #players % traversed % #lastUsedWord .= Nothing
                    #round += 1
                True -> do
                    zoomMany (#players % traversed) $ do
                        playerId <- use #id
                        tell $ GameStateEvent.singletonEvent playerId GameStateEvent.GameOver
                        use #lives >>= \case
                            0 -> tell $ GameStateEvent.singletonEvent playerId GameStateEvent.ILose
                            _ -> tell $ GameStateEvent.singletonEvent playerId GameStateEvent.IWin

randomGivenLetters :: StdGen -> [CaseInsensitiveText] -> (CaseInsensitiveText, StdGen)
randomGivenLetters stdGen givenLettersSet = let (i, stdGen') = randomR (0, length givenLettersSet - 1) stdGen in (givenLettersSet !! i, stdGen')

awardFreeLetter ::
    (MonadState GameState m, MonadWriter GameStateEvents m) =>
    PlayerId ->
    CaseInsensitiveText ->
    m ()
awardFreeLetter pId guess = do
    totalLetters <- fromMaybe mempty <$> preuse (#players % ix pId % totalLettersL)
    let
        allLetters = HashSet.fromList $ CaseInsensitiveChar <$> ['A' .. 'Z']
        openLetters =
            foldr
                (flip HashSet.difference)
                allLetters
                [totalLetters, caseInsensitiveLetters guess]
    i <- genRandom (0, length openLetters - 1)
    let letter = toList openLetters !! i
    #players % ix pId % #freeLetters %= HashSet.insert letter
    tellEvents (#players % folded) $ GameStateEvent.FreeLetterAward letter pId

isGameOver :: GameState -> Bool
isGameOver gs = (<= 1) $ length $ filter isPlayerAlive $ toList $ gs ^. #players

isPlayerAlive :: PlayerState -> Bool
isPlayerAlive ps = ps ^. #lives > 0

isValidGuess :: GameState -> CaseInsensitiveText -> Bool
isValidGuess gs g =
    ((gs ^. #givenLetters) `CaseInsensitive.isInfixOf` g)
        && not (g `HashSet.member` (gs ^. #alreadyUsedWords))
        && (g `Set.member` (gs ^. #validWords))

isPlayerActive :: GameState -> PlayerId -> Bool
isPlayerActive gs playerId = fromMaybe False $ gs ^? #players % ix playerId % #lastUsedWord % to isNothing
