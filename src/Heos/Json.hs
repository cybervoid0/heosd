module Heos.Json (yesNo, trueFalse) where

import Data.Aeson.Types (Parser)
import Data.Text (Text)
import Data.Text qualified as T

yesNo :: Text -> Parser Bool
yesNo txt = maybe (fail ("expected yes or no, got " <> T.unpack txt)) pure (go txt)
 where
  go = \case
    "yes" -> Just True
    "no" -> Just False
    _ -> Nothing

trueFalse :: Text -> Parser Bool
trueFalse txt = maybe (fail ("expected true or false, got " <> T.unpack txt)) pure (go txt)
 where
  go = \case
    "true" -> Just True
    "false" -> Just False
    _ -> Nothing
