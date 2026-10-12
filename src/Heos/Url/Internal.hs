module Heos.Url.Internal where

import Control.Monad (unless, when)
import Data.Char (isAlphaNum)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T

data KeyFailure = EmptyKey | IllegalKey Text deriving (Show, Eq)
renderProblem :: KeyFailure -> Text
renderProblem EmptyKey = "Empty key"
renderProblem (IllegalKey key) = "Illegal symbols in key: " <> key

newtype ParamKey = ParamKey {text :: Text}
  deriving stock (Show)
  deriving newtype (Eq)
makeParamKey :: Text -> Either KeyFailure ParamKey
makeParamKey txt = do
  when (T.null txt) $ Left EmptyKey
  unless (T.all (\c -> isAlphaNum c || c == '_') txt) $ Left (IllegalKey txt)
  pure $ ParamKey txt

-- | For keys written in the source code. A bad literal is a programmer's
--   mistake, and the tests that render every command will catch it at once.
makeKey :: Text -> ParamKey
makeKey k = either (error . T.unpack . renderProblem) id (makeParamKey k)

-- | A value is always plain text on the wire. A list such as "qid=4,5,6" is
--   just a value with commas in it: turning it into a list, or a list into it,
--   is the job of whoever knows what the value means, not of this module.
data UrlPair = UrlPair {key :: ParamKey, val :: Text} deriving (Show, Eq)

-- | Makes UrlPair from key and value text args
makePair :: Text -> Text -> UrlPair
makePair key = UrlPair (makeKey key)

-- | Renders key-value pair
joinPair :: UrlPair -> Text
joinPair (UrlPair key val) = key.text <> "=" <> renderValue val

--   Sometimes the device sends a bare token instead of a key/value pair, e.g.
--   "command under process&sid=10&search=Ok Computer&scid=3"
data UrlParam = UrlFlag Text | UrlKeyValue UrlPair | UrlFailure Text KeyFailure deriving (Show, Eq)
lookupParam :: Text -> [UrlParam] -> Maybe Text
lookupParam k ps = listToMaybe [p.val | UrlKeyValue p <- ps, p.key.text == k]

data UrlResponse = UrlResponse {address :: Text, params :: [UrlParam]} deriving (Show, Eq)

-- | Build a HEOS command URL.
--   Values are escaped, keys are not: makeParamKey already guarantees there is
--   nothing in a key that would need escaping.
renderCommandUrl :: Text -> [UrlPair] -> Text
renderCommandUrl address [] = address
renderCommandUrl address pairs = address <> "?" <> renderQuery pairs

-- | Build the query string from a list of key/value pairs.
renderQuery :: [UrlPair] -> Text
renderQuery pairs = T.intercalate "&" (joinPair <$> pairs)

-- | Build the query string from a list of all available UrlParam cases (not only key-value pairs)
renderUrlParams :: [UrlParam] -> Text
renderUrlParams params = T.intercalate "&" (go <$> params)
 where
  go (UrlFlag flag) = flag
  go (UrlKeyValue p) = joinPair p
  go (UrlFailure txt _) = txt

-- | Escape the characters the HEOS specification reserves.
renderValue :: Text -> Text
renderValue = T.concatMap replace
 where
  replace = \case
    '&' -> "%26"
    '%' -> "%25"
    '=' -> "%3D"
    '\r' -> " "
    '\n' -> " "
    c -> T.singleton c

-- Split a command URL into its address and its parameters.
-- Not needed for the responses parsing
decodeCommandUrl :: Text -> UrlResponse
decodeCommandUrl txt = case query of
  "" -> UrlResponse address []
  q -> UrlResponse address (decodeQuery $ T.drop 1 q)
 where
  (address, query) = T.breakOn "?" txt

-- | Split "key1=val1&key2=val2" into parameters. Empty chunks are dropped,
--   so stray separators do not produce empty flags.
-- Used for parsing parameters in `message` field
decodeQuery :: Text -> [UrlParam]
decodeQuery = map splitPair . filter (not . T.null) . T.splitOn "&"

-- | Parse a single chunk. No "=" makes it a flag; a bad key makes it a
--   UrlFailure carrying the original chunk, so nothing is lost.
splitPair :: Text -> UrlParam
splitPair txt = case T.breakOn "=" txt of
  (key, "") -> UrlFlag key
  (key, val) -> case makeParamKey key of
    Left failure -> UrlFailure txt failure
    Right validKey -> UrlKeyValue $ UrlPair validKey (decodeValue $ T.drop 1 val)

-- | Turn escape sequences back into the characters they stand for.
decodeValue :: Text -> Text
decodeValue = T.pack . go . T.unpack
 where
  go = \case
    ('%' : '2' : '6' : rest) -> '&' : go rest
    ('%' : '3' : 'D' : rest) -> '=' : go rest
    ('%' : '2' : '5' : rest) -> '%' : go rest
    (c : rest) -> c : go rest
    [] -> []
