-- | Every type of the protocol in one import. They live by subject, one
--   module per command group of the spec: 'Heos.Types.System',
--   'Heos.Types.Player', 'Heos.Types.Browse'; the wire spellings of
--   booleans sit with the other JSON rules in 'Heos.Json'. This module only
--   re-exports them.
module Heos.Types (
  module Heos.Types.System,
  module Heos.Types.Player,
  module Heos.Types.Browse,
  YesNo (..),
  TrueFalse (..),
) where

import Heos.Json (TrueFalse (..), YesNo (..))
import Heos.Types.Browse
import Heos.Types.Player
import Heos.Types.System
