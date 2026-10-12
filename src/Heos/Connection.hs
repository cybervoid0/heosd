module Heos.Connection (
  splitLines,
  withConnection,
  Connection,
  subscribe,
  Waiting (..),
  sendCommand,
) where

import Heos.Connection.Internal (
  Connection,
  Waiting (..),
  sendCommand,
  splitLines,
  subscribe,
  withConnection,
 )
