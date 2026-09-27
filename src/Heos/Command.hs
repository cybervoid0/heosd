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
  GetPlayerInfo pid -> ("player/get_player_info", ["pid" .= pid])
  GetPlayState pid -> ("player/get_play_state", ["pid" .= pid])
  SetPlayState pid state -> ("player/set_play_state", ["pid" .= pid, "state" .= state])
  GetNowPlayingMedia pid -> ("player/get_now_playing_media", ["pid" .= pid])
  GetVolume pid -> ("player/get_volume", ["pid" .= pid])
  SetVolume pid level -> ("player/set_volume", ["pid" .= pid, "level" .= level])
  VolumeUp pid step -> ("player/volume_up", ["pid" .= pid, "step" .= step])
  VolumeDown pid step -> ("player/volume_down", ["pid" .= pid, "step" .= step])
  GetMute pid -> ("player/get_mute", ["pid" .= pid])
  SetMute pid onOff -> ("player/set_mute", ["pid" .= pid, "state" .= onOff])
  ToggleMute pid -> ("player/toggle_mute", ["pid" .= pid])
  GetPlayMode pid -> ("player/get_play_mode", ["pid" .= pid])
  SetPlayMode pid repeatMode shuffle -> ("player/set_play_mode", ["pid" .= pid, "repeat" .= repeatMode, "shuffle" .= shuffle])
  GetQueue pid range -> ("player/get_queue", ["pid" .= pid] <> ("range" .=? range))
  PlayQueueItem pid qid -> ("player/play_queue", ["pid" .= pid, "qid" .= qid])
  RemoveFromQueue pid qids -> ("player/remove_from_queue", ["pid" .= pid, "qid" .= qids])
  SaveQueue pid name -> ("player/save_queue", ["pid" .= pid, "name" .= name])
  ClearQueue pid -> ("player/clear_queue", ["pid" .= pid])
  MoveQueue pid sqids dqid -> ("player/move_queue_item", ["pid" .= pid, "sqid" .= sqids, "dqid" .= dqid])
  PlayNext pid -> ("player/play_next", ["pid" .= pid])
  PlayPrevious pid -> ("player/play_previous", ["pid" .= pid])
  SetQuickSelect pid qsid -> ("player/set_quickselect", ["pid" .= pid, "id" .= qsid])
  PlayQuickSelect pid qsid -> ("player/play_quickselect", ["pid" .= pid, "id" .= qsid])
  GetQuickSelects pid qsid -> ("player/get_quickselects", ["pid" .= pid] <> ("id" .=? qsid))
  CheckUpdate pid -> ("player/check_update", ["pid" .= pid])
  -- Group commands
  GetGroups -> ("group/get_groups", [])
  GetGroupInfo gid -> ("group/get_group_info", ["gid" .= gid])
  SetGroup leader members -> ("group/set_group", ["pid" .= (leader : members)])
  GetGroupVolume gid -> ("group/get_volume", ["gid" .= gid])
  SetGroupVolume gid level -> ("group/set_volume", ["gid" .= gid, "level" .= level])
  GroupVolumeUp gid step -> ("group/volume_up", ["gid" .= gid, "step" .= step])
  GroupVolumeDown gid step -> ("group/volume_down", ["gid" .= gid, "step" .= step])
  GetGroupMute gid -> ("group/get_mute", ["gid" .= gid])
  SetGroupMute gid onOff -> ("group/set_mute", ["gid" .= gid, "state" .= onOff])
  ToggleGroupMute gid -> ("group/toggle_mute", ["gid" .= gid])
  -- Browse commands
  GetMusicSources -> ("browse/get_music_sources", [])
  GetSourceInfo sid -> ("browse/get_source_info", ["sid" .= sid])
  BrowseSource sid -> ("browse/browse", ["sid" .= sid])
  BrowseContainer sid cid range -> ("browse/browse", ["sid" .= sid, "cid" .= cid] <> ("range" .=? range))
  GetSearchCriteria sid -> ("browse/get_search_criteria", ["sid" .= sid])
  Search sid search scid range -> ("browse/search", ["sid" .= sid, "search" .= search, "scid" .= scid] <> ("range" .=? range))
  PlayStation pid sid cid mid name ->
    ("browse/play_stream", ["pid" .= pid, "sid" .= sid] <> ("cid" .=? cid) <> ["mid" .= mid, "name" .= name])
  PlayPreset pid preset -> ("browse/play_preset", ["pid" .= pid, "preset" .= preset])
  PlayInput pid spid input -> ("browse/play_input", ["pid" .= pid] <> ("spid" .=? spid) <> ["input" .= input])
  PlayUrl pid url -> ("browse/play_stream", ["pid" .= pid, "url" .= url])
  AddContainerToQueue pid sid cid aid ->
    ("browse/add_to_queue", ["pid" .= pid, "sid" .= sid, "cid" .= cid, "aid" .= aid])
  AddTrackToQueue pid sid cid mid aid ->
    ("browse/add_to_queue", ["pid" .= pid, "sid" .= sid, "cid" .= cid, "mid" .= mid, "aid" .= aid])
  RenamePlaylist sid cid name -> ("browse/rename_playlist", ["sid" .= sid, "cid" .= cid, "name" .= name])
  DeletePlaylist sid cid -> ("browse/delete_playlist", ["sid" .= sid, "cid" .= cid])
  RetrieveMetadata sid cid -> ("browse/retrieve_metadata", ["sid" .= sid, "cid" .= cid])
  SetServiceOption opt -> ("browse/set_service_option", serviceOptionParams opt)

-- | Parameters of 4.4.19, in the order the spec examples use.
serviceOptionParams :: ServiceOption -> [UrlPair]
serviceOptionParams = \case
  AddTrackToLibrary sid mid -> ["sid" .= sid, option 1, "mid" .= mid]
  AddAlbumToLibrary sid cid -> ["sid" .= sid, option 2, "cid" .= cid]
  AddStationToLibrary sid mid -> ["sid" .= sid, option 3, "mid" .= mid]
  AddPlaylistToLibrary sid cid name -> ["sid" .= sid, option 4, "cid" .= cid, "name" .= name]
  RemoveTrackFromLibrary sid mid -> ["sid" .= sid, option 5, "mid" .= mid]
  RemoveAlbumFromLibrary sid cid -> ["sid" .= sid, option 6, "cid" .= cid]
  RemoveStationFromLibrary sid mid -> ["sid" .= sid, option 7, "mid" .= mid]
  RemovePlaylistFromLibrary sid cid -> ["sid" .= sid, option 8, "cid" .= cid]
  ThumbsUp sid pid -> ["sid" .= sid, option 11, "pid" .= pid]
  ThumbsDown sid pid -> ["sid" .= sid, option 12, "pid" .= pid]
  CreateStation sid name scid range ->
    ["sid" .= sid, option 13, "name" .= name] <> ("scid" .=? scid) <> ("range" .=? range)
  AddNowPlayingToFavorites pid -> [option 19, "pid" .= pid]
  AddStationToFavorites sid mid name -> ["sid" .= sid, option 19, "mid" .= mid, "name" .= name]
  RemoveFromFavorites mid -> [option 20, "mid" .= mid]
 where
  -- A bare numeric literal would be ambiguous for (.=), so the type is fixed here.
  option :: Int -> UrlPair
  option n = "option" .= n

renderCommand :: Command -> ByteString
renderCommand cmd = encodeUtf8 (renderCommandUrl ("heos://" <> path) pairs <> "\r\n")
 where
  (path, pairs) = commandParts cmd