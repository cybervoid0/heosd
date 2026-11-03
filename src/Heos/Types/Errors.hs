module Heos.Types.Errors where

import Control.Exception (Exception)
import Data.Text (Text)

newtype ProtocolError = ProtocolError Text deriving (Show)
instance Exception ProtocolError