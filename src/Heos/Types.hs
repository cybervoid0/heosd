module Heos.Types (UrlResponse (..), UrlParam (..), UrlPair (..), makeParamKey, ParamKey (text), renderProblem, KeyFailure (..)) where

{- PlayerId, NowPlaying, MediaItem, HeosError -}

import Control.Monad (unless, when)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import Data.Text qualified as T

data KeyFailure = EmptyKey | IllegalKey Text deriving (Show, Eq)
renderProblem :: KeyFailure -> Text
renderProblem EmptyKey = "Empty key"
renderProblem (IllegalKey key) = "Illegal symbols in key: " <> key

newtype ParamKey = ParamKey {text :: Text} deriving (Show, Eq)
makeParamKey :: Text -> Either KeyFailure ParamKey
makeParamKey txt = do
  when (T.null txt) $ Left EmptyKey
  unless (T.all isAlphaNum txt) $ Left (IllegalKey txt)
  pure $ ParamKey txt

data UrlPair = UrlPair {key :: ParamKey, val :: Text} deriving (Show, Eq)

--   Sometimes the device sends a bare token instead of a key/value pair, e.g.
--   "command under process&sid=10&search=Ok Computer&scid=3"
data UrlParam = UrlFlag Text | UrlKeyValue UrlPair | UrlFailure Text KeyFailure deriving (Show, Eq)

data UrlResponse = UrlResponse {address :: Text, params :: [UrlParam]} deriving (Show, Eq)
