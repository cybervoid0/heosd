# heosd

A daemon for controlling Denon and Marantz receivers over the
[HEOS CLI protocol](docs/HEOS_CLI_ProtocolSpecification-Version-1.17.pdf),
written in Haskell. It exposes the receiver as an MCP server, so an AI agent
can turn it on, pick a source, set the volume and play music.

## Features

- **Zero-config discovery**: finds HEOS devices on the local network over
  SSDP. No mDNS, avahi or other system services needed.
- **One long-lived connection**: keeps a single TCP session to the receiver,
  with heartbeats and automatic reconnect.
- **Typed commands**: every HEOS command is a Haskell value, rendered to the
  wire format with the protocol's escaping rules.
- **Events**: device notifications (volume, playback, source changes) are
  picked up as they happen and keep the daemon's view of the receiver current.
- **MCP server**: a small set of tools an agent can call without knowing
  anything about HEOS.

## Building

Requires GHC 9.12 and cabal 3.14 or newer (both available via
[GHCup](https://www.haskell.org/ghcup/)).

```sh
cabal build
cabal test
```

## License

[MIT](LICENSE)
