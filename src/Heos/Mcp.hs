module Heos.Mcp where

{- MCP tools on top of Service -}
{-
Keep the number of tools small. Do not mirror the protocol one to one. A
reasonable set:

heos_status     - what is playing, volume, state. No arguments, answers from
                  cache instantly.
heos_play_music - the main one. A single call: {query, kind?, mode?}, where
                  mode is "replace queue / play next / append". It does the
                  search, the choice and the playback itself. That saves the
                  agent a whole round of reasoning: instead of "search, look at
                  the results, decide, play" it is one call.
heos_transport  - {action: play|pause|next|prev}.
heos_volume     - {level?, delta?, mute?}.
heos_search     - separate, for when the agent wants to show options rather
                  than play immediately.

Arguments are flat, no nested structures. Responses are short: the model pays
tokens for every character you hand back, so do not return the amplifier's raw
JSON. Errors are plain language the model can act on: "the amplifier is not
responding, it is probably switched off" beats ECONNREFUSED.
-}
