module Heos.Connection (
  splitLines,
  withConnection,
  Connection,
  subscribe,
  sendRaw,
) where

import Heos.Connection.Internal (
  Connection,
  sendRaw,
  splitLines,
  subscribe,
  withConnection,
 )
