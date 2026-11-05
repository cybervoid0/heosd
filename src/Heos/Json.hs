module Heos.Json (heosJson, YesNo (..), TrueFalse (..)) where

import Data.Aeson (FromJSON (parseJSON), Options (..), defaultOptions, withText)
import Data.Text qualified as T

-- field names are the device's keys, except a leading underscore
-- that keeps a Haskell keyword out of the way: _type -> "type"
heosJson :: Options
heosJson = defaultOptions{fieldLabelModifier = dropWhile (== '_')}

-- The device spells a boolean two ways, depending on the field: "yes"/"no"
-- in browse replies, "true"/"false" elsewhere. Each is its own type, so a
-- field cannot accept the other spelling by accident.

newtype YesNo = YesNo Bool deriving (Show, Eq)
instance FromJSON YesNo where
  parseJSON = withText "yes/no" $ \case
    "yes" -> pure (YesNo True)
    "no" -> pure (YesNo False)
    other -> fail ("expected yes or no, got " <> T.unpack other)

newtype TrueFalse = TrueFalse Bool deriving (Show, Eq)
instance FromJSON TrueFalse where
  parseJSON = withText "true/false" $ \case
    "true" -> pure (TrueFalse True)
    "false" -> pure (TrueFalse False)
    other -> fail ("expected true or false, got " <> T.unpack other)
