module Heos.Json (heosJson) where

import Data.Aeson (Options (..), defaultOptions)

{- config loading, defaults -}

-- field names are the device's keys, except a leading underscore
-- that keeps a Haskell keyword out of the way: _type -> "type"
heosJson :: Options
heosJson = defaultOptions{fieldLabelModifier = dropWhile (== '_')}
