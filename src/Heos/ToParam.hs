module Heos.ToParam (ToParam (..), (.=), (.=?)) where

import Data.Text (Text)
import Data.Text qualified as T
import Heos.Url (UrlPair, makePair)

-- | How a value looks on the wire. Only the value: the key is added by (.=).
class ToParam a where
  toParam :: a -> Text

instance ToParam Text where
  toParam = id
instance ToParam Int where
  toParam = T.pack . show

-- | Lists go comma-separated: qid=4,5,6, pid=3,1,4
instance (ToParam a) => ToParam [a] where
  toParam = T.intercalate "," . map toParam

-- | A parameter: key and value.
(.=) :: (ToParam a) => Text -> a -> UrlPair
k .= v = makePair k (toParam v)

-- | An optional parameter: Nothing leaves it out of the command entirely.
(.=?) :: (ToParam a) => Text -> Maybe a -> [UrlPair]
k .=? v = maybe [] (\x -> [k .= x]) v