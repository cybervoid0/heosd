module Heos.Connection (
  splitLines,
  withConnection,
  Connection,
  subscribe,
  sendCommand,
) where

import Heos.Connection.Internal (
  Connection,
  sendCommand,
  splitLines,
  subscribe,
  withConnection,
 )
