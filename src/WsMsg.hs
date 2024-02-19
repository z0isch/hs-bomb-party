{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module WsMsg (
    WsMsg (..),
    WsMsg' (..),
    TypingMsgContent (..),
    SettingsMsgContent (..),
    LeaveMsgContent (..),
    NameMsgContent (..),
    GuessMsgContent (..),
) where

import CustomPrelude hiding ((.=))

import CaseInsensitive (CaseInsensitiveText)
import Data.Aeson
import Data.Aeson.Types (Parser)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import StateKey (StateKey)
import WithPlayerApi (PlayerId)

data WsMsg
    = TypingMsg (WsMsg' TypingMsgContent "Typing")
    | SettingsMsg (WsMsg' SettingsMsgContent "Settings")
    | JoinMsg (WsMsg' () "Join")
    | LeaveMsg (WsMsg' LeaveMsgContent "Leave")
    | NameMsg (WsMsg' NameMsgContent "Name")
    | StartMsg (WsMsg' () "Start")
    | StartOverMsg (WsMsg' () "StartOver")
    | GuessMsg (WsMsg' GuessMsgContent "Guess")
    deriving stock (Show, Generic)

instance FromJSON WsMsg where
    parseJSON v =
        asum
            [ TypingMsg <$> parseJSON v
            , SettingsMsg <$> parseJSON v
            , JoinMsg <$> parseJSON v
            , LeaveMsg <$> parseJSON v
            , NameMsg <$> parseJSON v
            , StartMsg <$> parseJSON v
            , StartOverMsg <$> parseJSON v
            , GuessMsg <$> parseJSON v
            ]

data WsMsg' a (b :: Symbol) = WsMsg'
    { stateKey :: StateKey
    , contents :: a
    }
    deriving stock (Show, Generic)

instance (FromJSON a, KnownSymbol b) => FromJSON (WsMsg' a b) where
    parseJSON = withObject "WsMsg" $ \v -> do
        stateKey <- v .: "stateKey"
        tag <- v .: "tag"
        guard $ symbolVal (Proxy @b) == tag
        contents <- parseJSON $ Object v
        pure $ WsMsg'{..}

newtype TypingMsgContent = TypingMsgContent
    { guess :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON)

newtype SettingsMsgContent = SettingsMsgContent
    { secondsToGuess :: Int
    }
    deriving stock (Show, Generic)

stringIntP :: String -> Parser Int
stringIntP = maybe (fail "Not an Int") pure . readMaybe @Int

instance FromJSON SettingsMsgContent where
    parseJSON = withObject "SettingsMsgContent" $ \v -> do
        secondsToGuess <- stringIntP =<< v .: "secondsToGuess"
        pure SettingsMsgContent{..}

newtype LeaveMsgContent = LeaveMsgContent
    { playerId :: PlayerId
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON)

data NameMsgContent = NameMsgContent
    { playerId :: PlayerId
    , name :: Text
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON)

newtype GuessMsgContent = GuessMsgContent
    { guess :: CaseInsensitiveText
    }
    deriving stock (Show, Generic)
    deriving anyclass (FromJSON)
