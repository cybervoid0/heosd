module Heos.Config where

import Data.Text (Text)
import Data.Yaml
import GHC.Generics (Generic)

data Config = Config
  {ipAddress :: Text}