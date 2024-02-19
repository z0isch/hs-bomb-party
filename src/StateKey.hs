{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module StateKey (StateKey (..)) where

import CustomPrelude

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Servant (FromHttpApiData, ToHttpApiData)

newtype StateKey = StateKey {getStateKey :: Int}
    deriving newtype (Eq, Show, Num, Display, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)
