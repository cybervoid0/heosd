module Heos.Envelope (
  -- * Raw envelope, as decoded from JSON
  Envelope (..),
  HeosHeader (..),
  HeosResult (..),

  -- * Classified envelope
  IncomingEnvelope (..),
  HeosOutcome (..),
  HeosError (..),
  HeosEvent (..),
  classifyEnvelope,
) where

import Heos.Envelope.Internal
