module Survival.GameStateEvent (GameStateEvents (..), GameStateEvent (..), singletonEvent, eventsForPlayer) where

import CaseInsensitive (CaseInsensitiveChar)
import CustomPrelude
import Data.Aeson (ToJSON (..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import Data.Coerce (coerce)
import qualified RIO.HashMap as HashMap
import WithPlayerApi (PlayerId)

newtype GameStateEvents = GameStateEvents (HashMap PlayerId (Seq GameStateEvent))

instance Monoid GameStateEvents where
    mempty = coerce @(HashMap PlayerId (Seq GameStateEvent)) mempty

instance Semigroup GameStateEvents where
    (<>) =
        coerce $ HashMap.unionWith @PlayerId @(Seq GameStateEvent) (<>)

singletonEvent :: PlayerId -> GameStateEvent -> GameStateEvents
singletonEvent playerId = GameStateEvents . HashMap.singleton playerId . pure

eventsForPlayer :: PlayerId -> GameStateEvents -> Maybe (Seq GameStateEvent)
eventsForPlayer = coerce $ HashMap.lookup @PlayerId @(Seq GameStateEvent)

data GameStateEvent
    = IWin
    | ILose
    | MyTurn
    | TimeUp
    | GameOver
    | WrongGuess
    | CorrectGuess
    | FreeLetterAward CaseInsensitiveChar PlayerId
    | UsedAllLetters
    deriving stock (Show, Eq)

instance ToJSON GameStateEvent where
    toJSON = toJSON . Aeson.object . (: []) . toPair
    toEncoding = toEncoding . Aeson.object . (: []) . toPair

toPair :: GameStateEvent -> Aeson.Pair
toPair = \case
    IWin -> ("IWin", Aeson.Null)
    ILose -> ("ILose", Aeson.Null)
    MyTurn -> ("MyTurn", Aeson.Null)
    TimeUp -> ("TimeUp", Aeson.Null)
    GameOver -> ("GameOver", Aeson.Null)
    WrongGuess -> ("WrongGuess", Aeson.Null)
    CorrectGuess -> ("CorrectGuess", Aeson.Null)
    FreeLetterAward c pID -> ("FreeLetterAward", Aeson.object [("char", toJSON c), ("playerID", toJSON pID)])
    UsedAllLetters -> ("UsedAllLetters", Aeson.Null)