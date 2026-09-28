module Heos.Command (
  Command (..),
  ServiceOption (..),
  Credentials (..),
  Range (..),
  Qid (..),
  QuickSelectId (..),
  InputName (..),
  AddCriteria (..),
  renderCommand,
)
where

import Data.ByteString (ByteString)

-- import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Heos.Media (ContainerId (..), MediaId (..), SearchCriteriaId (..), SourceId (..))
import Heos.Player (GroupId, OnOff, PlayState, PlayerId, RepeatMode)
import Heos.ToParam (ToParam (..), (.=), (.=?))
import Heos.Url (UrlPair, renderCommandUrl)

-- Two parameters, not one value, so no ToParam: see AccountSignIn in commandParts.
data Credentials = Credentials {login :: Text, password :: Text} deriving (Eq)
instance Show Credentials where
  show (Credentials login _) = "Login=" <> T.unpack login <> " Password=<hidden>"

data Range = Range {start :: Int, end :: Int} deriving (Show, Eq)
instance ToParam Range where
  toParam (Range start end) = toParam [start, end]

newtype Qid = Qid Int deriving (Show, Eq)
instance ToParam Qid where
  toParam (Qid qid) = toParam qid

newtype QuickSelectId = QuickSelectId Int deriving (Show, Eq) -- 1..6
instance ToParam QuickSelectId where
  toParam (QuickSelectId n) = toParam n

newtype InputName = InputName Text deriving (Show, Eq) -- "inputs/aux_in_1", "inputs/hdmi_arc_1", ...
instance ToParam InputName where
  toParam (InputName name) = name

-- aid in 4.4.11 / 4.4.12
data AddCriteria = AddPlayNow | AddPlayNext | AddToEnd | AddReplaceAndPlay deriving (Show, Eq)
instance ToParam AddCriteria where
  toParam = \case
    AddPlayNow -> "1"
    AddPlayNext -> "2"
    AddToEnd -> "3"
    AddReplaceAndPlay -> "4"

data ServiceOption
  = AddTrackToLibrary SourceId MediaId -- 1
  | AddAlbumToLibrary SourceId ContainerId -- 2
  | AddStationToLibrary SourceId MediaId -- 3
  | AddPlaylistToLibrary SourceId ContainerId Text -- 4
  | RemoveTrackFromLibrary SourceId MediaId -- 5
  | RemoveAlbumFromLibrary SourceId ContainerId -- 6
  | RemoveStationFromLibrary SourceId MediaId -- 7
  | RemovePlaylistFromLibrary SourceId ContainerId -- 8
  | ThumbsUp SourceId PlayerId -- 11
  | ThumbsDown SourceId PlayerId -- 12
  | CreateStation SourceId Text (Maybe SearchCriteriaId) (Maybe Range) -- 13
  | AddNowPlayingToFavorites PlayerId -- 19, from the now playing screen
  | AddStationToFavorites SourceId MediaId Text -- 19, from the browse screen
  | RemoveFromFavorites MediaId -- 20
  deriving (Show, Eq)

data Command
  = RegisterForChangeEvents OnOff -- 4.1.1
  | AccountCheck -- 4.1.2
  | AccountSignIn Credentials -- 4.1.3
  | AccountSignOut -- 4.1.4
  | HeartBeat -- 4.1.5
  | Reboot -- 4.1.6
  | PrettifyJson OnOff -- 4.1.7
  | GetPlayers -- 4.2.1
  | GetPlayerInfo PlayerId -- 4.2.2
  | GetPlayState PlayerId -- 4.2.3
  | SetPlayState PlayerId PlayState -- 4.2.4
  | GetNowPlayingMedia PlayerId -- 4.2.5
  | GetVolume PlayerId -- 4.2.6
  | SetVolume PlayerId Int -- 4.2.7
  | VolumeUp PlayerId Int -- 4.2.8
  | VolumeDown PlayerId Int -- 4.2.9
  | GetMute PlayerId -- 4.2.10
  | SetMute PlayerId OnOff -- 4.2.11
  | ToggleMute PlayerId -- 4.2.12
  | GetPlayMode PlayerId -- 4.2.13
  | SetPlayMode PlayerId RepeatMode OnOff -- 4.2.14
  | GetQueue PlayerId (Maybe Range) -- 4.2.15
  | PlayQueueItem PlayerId Qid -- 4.2.16
  | RemoveFromQueue PlayerId [Qid] -- 4.2.17
  | SaveQueue PlayerId Text -- 4.2.18
  | ClearQueue PlayerId -- 4.2.19
  | MoveQueue PlayerId [Qid] Qid -- 4.2.20
  | PlayNext PlayerId -- 4.2.21
  | PlayPrevious PlayerId -- 4.2.22
  | SetQuickSelect PlayerId QuickSelectId -- 4.2.23
  | PlayQuickSelect PlayerId QuickSelectId -- 4.2.24
  | GetQuickSelects PlayerId (Maybe QuickSelectId) -- 4.2.25
  | CheckUpdate PlayerId -- 4.2.26
  | GetGroups -- 4.3.1
  | GetGroupInfo GroupId -- 4.3.2
  | SetGroup PlayerId [PlayerId] -- 4.3.3, leader and members; no members ungroups
  | GetGroupVolume GroupId -- 4.3.4
  | SetGroupVolume GroupId Int -- 4.3.5
  | GroupVolumeUp GroupId Int -- 4.3.6
  | GroupVolumeDown GroupId Int -- 4.3.7
  | GetGroupMute GroupId -- 4.3.8
  | SetGroupMute GroupId OnOff -- 4.3.9
  | ToggleGroupMute GroupId -- 4.3.10
  | GetMusicSources -- 4.4.1
  | GetSourceInfo SourceId -- 4.4.2
  | BrowseSource SourceId -- 4.4.3
  | BrowseContainer SourceId ContainerId (Maybe Range) -- 4.4.4
  | GetSearchCriteria SourceId -- 4.4.5
  | Search SourceId Text SearchCriteriaId (Maybe Range) -- 4.4.6
  | PlayStation PlayerId SourceId (Maybe ContainerId) MediaId Text -- 4.4.7
  | PlayPreset PlayerId Int -- 4.4.8
  | PlayInput PlayerId (Maybe PlayerId) InputName -- 4.4.9, the Maybe is spid (another speaker as source)
  | PlayUrl PlayerId Text -- 4.4.10
  | AddContainerToQueue PlayerId SourceId ContainerId AddCriteria -- 4.4.11
  | AddTrackToQueue PlayerId SourceId ContainerId MediaId AddCriteria -- 4.4.12
  -- 4.4.13 Get HEOS Playlists: done with BrowseSource / BrowseContainer
  | RenamePlaylist SourceId ContainerId Text -- 4.4.14
  | DeletePlaylist SourceId ContainerId -- 4.4.15
  -- 4.4.16 Get HEOS History: done with BrowseSource / BrowseContainer
  | RetrieveMetadata SourceId ContainerId -- 4.4.17
  -- 4.4.18 Get Service Options: obsolete, left out
  | SetServiceOption ServiceOption -- 4.4.19
  deriving (Show, Eq)

-- Convert all existing HEOS commands with arguments to an intermediate data representation for further rendering
commandParts :: Command -> (Text, [UrlPair])
commandParts = \case
  -- System commands
  RegisterForChangeEvents onOff -> ("system/register_for_change_events", ["enable" .= onOff])
  AccountCheck -> ("system/check_account", [])
  AccountSignIn (Credentials un pw) -> ("system/sign_in", ["un" .= un, "pw" .= pw])
  AccountSignOut -> ("system/sign_out", [])
  HeartBeat -> ("system/heart_beat", [])
  Reboot -> ("system/reboot", [])
  PrettifyJson onOff -> ("system/prettify_json_response", ["enable" .= onOff])
  -- Player commands
  GetPlayers -> ("player/get_players", [])
  GetPlayerInfo p -> ("player/get_player_info", [pid p])
  GetPlayState p -> ("player/get_play_state", [pid p])
  SetPlayState p state -> ("player/set_play_state", [pid p, "state" .= state])
  GetNowPlayingMedia p -> ("player/get_now_playing_media", [pid p])
  GetVolume p -> ("player/get_volume", [pid p])
  SetVolume p level -> ("player/set_volume", [pid p, "level" .= level])
  VolumeUp p step -> ("player/volume_up", [pid p, "step" .= step])
  VolumeDown p step -> ("player/volume_down", [pid p, "step" .= step])
  GetMute p -> ("player/get_mute", [pid p])
  SetMute p onOff -> ("player/set_mute", [pid p, "state" .= onOff])
  ToggleMute p -> ("player/toggle_mute", [pid p])
  GetPlayMode p -> ("player/get_play_mode", [pid p])
  SetPlayMode p repeatMode shuffle -> ("player/set_play_mode", [pid p, "repeat" .= repeatMode, "shuffle" .= shuffle])
  GetQueue p range -> ("player/get_queue", [pid p] <> ("range" .=? range))
  PlayQueueItem p qid -> ("player/play_queue", [pid p, "qid" .= qid])
  RemoveFromQueue p qids -> ("player/remove_from_queue", [pid p, "qid" .= qids])
  SaveQueue p name -> ("player/save_queue", [pid p, "name" .= name])
  ClearQueue p -> ("player/clear_queue", [pid p])
  MoveQueue p sqids dqid -> ("player/move_queue_item", [pid p, "sqid" .= sqids, "dqid" .= dqid])
  PlayNext p -> ("player/play_next", [pid p])
  PlayPrevious p -> ("player/play_previous", [pid p])
  SetQuickSelect p qsid -> ("player/set_quickselect", [pid p, "id" .= qsid])
  PlayQuickSelect p qsid -> ("player/play_quickselect", [pid p, "id" .= qsid])
  GetQuickSelects p qsid -> ("player/get_quickselects", [pid p] <> ("id" .=? qsid))
  CheckUpdate p -> ("player/check_update", [pid p])
  -- Group commands
  GetGroups -> ("group/get_groups", [])
  GetGroupInfo g -> ("group/get_group_info", [gid g])
  SetGroup leader members -> ("group/set_group", ["pid" .= (leader : members)])
  GetGroupVolume g -> ("group/get_volume", [gid g])
  SetGroupVolume g level -> ("group/set_volume", [gid g, "level" .= level])
  GroupVolumeUp g step -> ("group/volume_up", [gid g, "step" .= step])
  GroupVolumeDown g step -> ("group/volume_down", [gid g, "step" .= step])
  GetGroupMute g -> ("group/get_mute", [gid g])
  SetGroupMute g onOff -> ("group/set_mute", [gid g, "state" .= onOff])
  ToggleGroupMute g -> ("group/toggle_mute", [gid g])
  -- Browse commands
  GetMusicSources -> ("browse/get_music_sources", [])
  GetSourceInfo s -> ("browse/get_source_info", [sid s])
  BrowseSource s -> ("browse/browse", [sid s])
  BrowseContainer s c range -> ("browse/browse", [sid s, cid c] <> ("range" .=? range))
  GetSearchCriteria s -> ("browse/get_search_criteria", [sid s])
  Search s search scid range -> ("browse/search", [sid s, "search" .= search, "scid" .= scid] <> ("range" .=? range))
  PlayStation p s c m name ->
    ("browse/play_stream", [pid p, sid s] <> ("cid" .=? c) <> [mid m, "name" .= name])
  PlayPreset p preset -> ("browse/play_preset", [pid p, "preset" .= preset])
  PlayInput p spid input -> ("browse/play_input", [pid p] <> ("spid" .=? spid) <> ["input" .= input])
  PlayUrl p url -> ("browse/play_stream", [pid p, "url" .= url])
  AddContainerToQueue p s c aid ->
    ("browse/add_to_queue", [pid p, sid s, cid c, "aid" .= aid])
  AddTrackToQueue p s c m aid ->
    ("browse/add_to_queue", [pid p, sid s, cid c, mid m, "aid" .= aid])
  RenamePlaylist s c name -> ("browse/rename_playlist", [sid s, cid c, "name" .= name])
  DeletePlaylist s c -> ("browse/delete_playlist", [sid s, cid c])
  RetrieveMetadata s c -> ("browse/retrieve_metadata", [sid s, cid c])
  SetServiceOption opt -> ("browse/set_service_option", serviceOptionParams opt)

-- | Parameters of 4.4.19, in the order the spec examples use.
serviceOptionParams :: ServiceOption -> [UrlPair]
serviceOptionParams = \case
  AddTrackToLibrary s m -> [sid s, option 1, mid m]
  AddAlbumToLibrary s c -> [sid s, option 2, cid c]
  AddStationToLibrary s m -> [sid s, option 3, mid m]
  AddPlaylistToLibrary s c name -> [sid s, option 4, cid c, "name" .= name]
  RemoveTrackFromLibrary s m -> [sid s, option 5, mid m]
  RemoveAlbumFromLibrary s c -> [sid s, option 6, cid c]
  RemoveStationFromLibrary s m -> [sid s, option 7, mid m]
  RemovePlaylistFromLibrary s c -> [sid s, option 8, cid c]
  ThumbsUp s p -> [sid s, option 11, pid p]
  ThumbsDown s p -> [sid s, option 12, pid p]
  CreateStation s name scid range ->
    [sid s, option 13, "name" .= name] <> ("scid" .=? scid) <> ("range" .=? range)
  AddNowPlayingToFavorites p -> [option 19, pid p]
  AddStationToFavorites s m name -> [sid s, option 19, mid m, "name" .= name]
  RemoveFromFavorites m -> [option 20, mid m]

-- HELPERS --
option :: Int -> UrlPair
option n = "option" .= n

pid :: PlayerId -> UrlPair
pid p = "pid" .= p

sid :: SourceId -> UrlPair
sid s = "sid" .= s

cid :: ContainerId -> UrlPair
cid c = "cid" .= c

gid :: GroupId -> UrlPair
gid g = "gid" .= g

mid :: MediaId -> UrlPair
mid m = "mid" .= m

-- | Convert Sequence + Command to HEOS-style URL-encoded String
renderCommand :: Int -> Command -> ByteString
renderCommand seqNo cmd =
  encodeUtf8 (renderCommandUrl ("heos://" <> path) (pairs <> ["SEQUENCE" .= seqNo]) <> "\r\n")
 where
  (path, pairs) = commandParts cmd