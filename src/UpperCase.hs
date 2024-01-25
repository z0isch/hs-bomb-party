{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module UpperCase (UpperCase (..), fromText) where

import Data.Hashable (Hashable)
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Lucid (ToHtml)
import Web.HttpApiData (FromHttpApiData)
import Web.Internal.HttpApiData (FromHttpApiData (..))

newtype UpperCase = UpperCase {getUpperCase :: Text}
    deriving (Eq, Show)
    deriving newtype (Hashable, ToHtml)

instance IsString UpperCase where
    fromString = fromText . T.pack

fromText :: Text -> UpperCase
fromText = UpperCase . T.toUpper

instance FromHttpApiData UpperCase where
    parseQueryParam = fmap fromText . parseQueryParam @Text
