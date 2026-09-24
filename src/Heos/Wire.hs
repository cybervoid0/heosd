module Heos.Wire (renderCommandUrl, renderQuery, renderValue, decodeCommandUrl, decodeQuery, splitPair, decodeValue) where

import Data.Text (Text)
import Data.Text qualified as T
import Heos.Types (UrlPair (..), UrlParam (..), UrlResponse (..), makeParamKey, text)

-- | Build a HEOS command URL.
--   Values are escaped, keys are not: makeParamKey already guarantees there is
--   nothing in a key that would need escaping.
renderCommandUrl :: Text -> [UrlPair] -> Text
renderCommandUrl address [] = address
renderCommandUrl address pairs = address <> "?" <> renderQuery pairs

-- | Build the query string from a list of key/value pairs.
renderQuery :: [UrlPair] -> Text
renderQuery pairs = T.intercalate "&" (joinPair <$> pairs)
 where
  joinPair (UrlPair key val) = key.text <> "=" <> renderValue val

-- | Escape the characters the HEOS specification reserves.
renderValue :: Text -> Text
renderValue = T.concatMap replace
 where
  replace char = case char of
    '&' -> "%26"
    '%' -> "%25"
    '=' -> "%3D"
    c -> T.singleton c

-- | Split a command URL into its address and its parameters.
decodeCommandUrl :: Text -> UrlResponse
decodeCommandUrl txt = case query of
  "" -> UrlResponse address []
  q -> UrlResponse address (decodeQuery $ T.drop 1 q)
 where
  (address, query) = T.breakOn "?" txt

-- | Split "key1=val1&key2=val2" into parameters. Empty chunks are dropped,
--   so stray separators do not produce empty flags.
decodeQuery :: Text -> [UrlParam]
decodeQuery = (splitPair <$>) . filter (/= "") . T.splitOn "&"

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
  go ('%' : '2' : '6' : rest) = '&' : go rest
  go ('%' : '3' : 'D' : rest) = '=' : go rest
  go ('%' : '2' : '5' : rest) = '%' : go rest
  go (c : rest) = c : go rest
  go [] = []
