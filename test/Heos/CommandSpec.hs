module Heos.CommandSpec where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Heos.Command
import Test.Hspec
import Test.QuickCheck (property)

spec :: Spec
spec = do
  -- Each case is the "Example:" line of the HEOS CLI spec 1.17 for that
  -- section. Where the spec has a typo (quotes around values, a missing pid,
  -- "&?" in a URL), the expected line is the corrected one.
  describe "renderCommand matches the spec examples" $
    forM_ specExamples $ \(section, cmd, expected) ->
      it (section <> "  " <> T.unpack expected) $
        wire cmd `shouldBe` expected <> "\r\n"

  describe "framing" $ do
    it "every command starts with heos://" $
      forM_ specExamples $ \(_, cmd, _) ->
        wire cmd `shouldSatisfy` T.isPrefixOf "heos://"

    it "every command ends with exactly one \\r\\n" $
      forM_ specExamples $ \(_, cmd, _) -> do
        wire cmd `shouldSatisfy` T.isSuffixOf "\r\n"
        T.count "\r\n" (wire cmd) `shouldBe` 1

    it "a command without parameters has no ?" $
      wire GetPlayers `shouldBe` "heos://player/get_players\r\n"

  describe "optional parameters" $ do
    it "Nothing leaves the parameter out entirely" $ do
      wire (GetQueue (PlayerId 1) Nothing) `shouldBe` "heos://player/get_queue?pid=1\r\n"
      wire (BrowseContainer (SourceId 2) (ContainerId "TopAlbums") Nothing)
        `shouldBe` "heos://browse/browse?sid=2&cid=TopAlbums\r\n"
      wire (Search (SourceId 2) "U2" (SearchCriteriaId 1) Nothing)
        `shouldBe` "heos://browse/search?sid=2&search=U2&scid=1\r\n"
      wire (GetQuickSelects (PlayerId 1) Nothing) `shouldBe` "heos://player/get_quickselects?pid=1\r\n"

    it "Just puts the parameter in its place" $ do
      wire (Search (SourceId 2) "U2" (SearchCriteriaId 1) (Just (Range 0 50)))
        `shouldBe` "heos://browse/search?sid=2&search=U2&scid=1&range=0,50\r\n"
      wire (GetQuickSelects (PlayerId 1) (Just (QuickSelectId 3)))
        `shouldBe` "heos://player/get_quickselects?pid=1&id=3\r\n"

    it "an optional parameter in the middle keeps the order around it" $
      wire (PlayStation (PlayerId 1) (SourceId 2) Nothing (MediaId "15376") "Q95")
        `shouldBe` "heos://browse/play_stream?pid=1&sid=2&mid=15376&name=Q95\r\n"

  describe "values" $ do
    it "on/off and booleans" $ do
      toParam On `shouldBe` "on"
      toParam Off `shouldBe` "off"
      toParam True `shouldBe` "on"
      toParam False `shouldBe` "off"

    it "play states" $
      map toParam [Play, Pause, Stop] `shouldBe` ["play", "pause", "stop"]

    it "repeat modes" $
      map toParam [RepeatAll, RepeatOne, RepeatOff] `shouldBe` ["on_all", "on_one", "off"]

    it "add criteria are numbered 1 to 4" $
      map toParam [AddPlayNow, AddPlayNext, AddToEnd, AddReplaceAndPlay] `shouldBe` ["1", "2", "3", "4"]

    it "a range is start,end" $
      toParam (Range 0 10) `shouldBe` "0,10"

    it "a list is comma-separated, a one-item list has no comma" $ do
      toParam [Qid 4, Qid 5, Qid 6] `shouldBe` "4,5,6"
      toParam [Qid 4] `shouldBe` "4"

    it "negative player ids keep their sign" $
      wire (GetVolume (PlayerId (-1245882071))) `shouldBe` "heos://player/get_volume?pid=-1245882071\r\n"

    it "a player id renders as its number, for any Int" $
      property $ \(n :: Int) -> toParam (PlayerId n) == T.pack (show n)

  describe "escaping" $ do
    it "reserved characters in a value are escaped" $
      wire (SaveQueue (PlayerId 1) "Rock & Roll = 100%")
        `shouldBe` "heos://player/save_queue?pid=1&name=Rock %26 Roll %3D 100%25\r\n"

    it "a password with reserved characters cannot split into extra parameters" $
      wire (AccountSignIn (Credentials "me" "a&pw=b"))
        `shouldBe` "heos://system/sign_in?un=me&pw=a%26pw%3Db\r\n"

    it "a URL with its own query string stays one value" $
      wire (PlayUrl (PlayerId 1) "http://host/play?id=1&fmt=mp3")
        `shouldBe` "heos://browse/play_stream?pid=1&url=http://host/play?id%3D1%26fmt%3Dmp3\r\n"

    it "commas inside a single value are sent as is" $
      wire (SaveQueue (PlayerId 1) "great, playlist")
        `shouldBe` "heos://player/save_queue?pid=1&name=great, playlist\r\n"

    -- A line break inside a value ends the command early, and whatever
    -- follows is read by the device as a second command. The spec does not
    -- say how to escape it, so it needs a decision: reject such values, strip
    -- them, or escape them some other way.
    it "a line break in a value cannot inject a second command" $ do
      pendingWith "renderValue does not handle \\r or \\n yet"
      T.count "\r\n" (wire (SaveQueue (PlayerId 1) "x\r\nheos://system/reboot")) `shouldBe` 1

  describe "credentials" $ do
    it "show hides the password" $ do
      let shown = show (AccountSignIn (Credentials "me" "secret"))
      shown `shouldNotSatisfy` ("secret" `T.isInfixOf`) . T.pack
      shown `shouldSatisfy` ("me" `T.isInfixOf`) . T.pack

    it "the wire carries the real password" $
      wire (AccountSignIn (Credentials "me" "secret")) `shouldSatisfy` T.isInfixOf "pw=secret"

wire :: Command -> Text
wire = decodeUtf8 . renderCommand

specExamples :: [(String, Command, Text)]
specExamples =
  [ ("4.1.1", RegisterForChangeEvents True, "heos://system/register_for_change_events?enable=on")
  , ("4.1.2", AccountCheck, "heos://system/check_account")
  , ("4.1.3", AccountSignIn (Credentials "user@gmail.com" "12345"), "heos://system/sign_in?un=user@gmail.com&pw=12345")
  , ("4.1.4", AccountSignOut, "heos://system/sign_out")
  , ("4.1.5", HeartBeat, "heos://system/heart_beat")
  , ("4.1.6", Reboot, "heos://system/reboot")
  , ("4.1.7", PrettifyJson True, "heos://system/prettify_json_response?enable=on")
  , ("4.2.1", GetPlayers, "heos://player/get_players")
  , ("4.2.2", GetPlayerInfo (PlayerId 1), "heos://player/get_player_info?pid=1")
  , ("4.2.3", GetPlayState (PlayerId 1), "heos://player/get_play_state?pid=1")
  , ("4.2.4", SetPlayState (PlayerId 1) Play, "heos://player/set_play_state?pid=1&state=play")
  , ("4.2.5", GetNowPlayingMedia (PlayerId 1), "heos://player/get_now_playing_media?pid=1")
  , ("4.2.6", GetVolume (PlayerId 1), "heos://player/get_volume?pid=1")
  , ("4.2.7", SetVolume (PlayerId 2) 30, "heos://player/set_volume?pid=2&level=30")
  , ("4.2.8", VolumeUp (PlayerId 2) 5, "heos://player/volume_up?pid=2&step=5")
  , ("4.2.9", VolumeDown (PlayerId 2) 5, "heos://player/volume_down?pid=2&step=5")
  , ("4.2.10", GetMute (PlayerId 1), "heos://player/get_mute?pid=1")
  , ("4.2.11", SetMute (PlayerId 3) Off, "heos://player/set_mute?pid=3&state=off")
  , ("4.2.12", ToggleMute (PlayerId 3), "heos://player/toggle_mute?pid=3")
  , ("4.2.13", GetPlayMode (PlayerId 1), "heos://player/get_play_mode?pid=1")
  , ("4.2.14", SetPlayMode (PlayerId 1) RepeatAll Off, "heos://player/set_play_mode?pid=1&repeat=on_all&shuffle=off")
  , ("4.2.15", GetQueue (PlayerId 1) (Just (Range 0 10)), "heos://player/get_queue?pid=1&range=0,10")
  , ("4.2.16", PlayQueueItem (PlayerId 2) (Qid 9), "heos://player/play_queue?pid=2&qid=9")
  , ("4.2.17", RemoveFromQueue (PlayerId 1) [Qid 4, Qid 5, Qid 6], "heos://player/remove_from_queue?pid=1&qid=4,5,6")
  , ("4.2.18", SaveQueue (PlayerId 1) "great playlist", "heos://player/save_queue?pid=1&name=great playlist")
  , ("4.2.19", ClearQueue (PlayerId 1), "heos://player/clear_queue?pid=1")
  , ("4.2.20", MoveQueue (PlayerId 2) [Qid 2, Qid 4] (Qid 6), "heos://player/move_queue_item?pid=2&sqid=2,4&dqid=6")
  , ("4.2.21", PlayNext (PlayerId 1), "heos://player/play_next?pid=1")
  , ("4.2.22", PlayPrevious (PlayerId 1), "heos://player/play_previous?pid=1")
  , ("4.2.23", SetQuickSelect (PlayerId 1) (QuickSelectId 2), "heos://player/set_quickselect?pid=1&id=2")
  , ("4.2.24", PlayQuickSelect (PlayerId 1) (QuickSelectId 2), "heos://player/play_quickselect?pid=1&id=2")
  , ("4.2.25", GetQuickSelects (PlayerId 1) Nothing, "heos://player/get_quickselects?pid=1")
  , ("4.2.26", CheckUpdate (PlayerId 1), "heos://player/check_update?pid=1")
  , ("4.3.1", GetGroups, "heos://group/get_groups")
  , ("4.3.2", GetGroupInfo (GroupId 1), "heos://group/get_group_info?gid=1")
  , ("4.3.3", SetGroup (PlayerId 3) [PlayerId 1, PlayerId 4], "heos://group/set_group?pid=3,1,4")
  , ("4.3.3 ungroup", SetGroup (PlayerId 3) [], "heos://group/set_group?pid=3")
  , ("4.3.4", GetGroupVolume (GroupId 1), "heos://group/get_volume?gid=1")
  , ("4.3.5", SetGroupVolume (GroupId 1) 30, "heos://group/set_volume?gid=1&level=30")
  , ("4.3.6", GroupVolumeUp (GroupId 1) 5, "heos://group/volume_up?gid=1&step=5")
  , ("4.3.7", GroupVolumeDown (GroupId 1) 5, "heos://group/volume_down?gid=1&step=5")
  , ("4.3.8", GetGroupMute (GroupId 1), "heos://group/get_mute?gid=1")
  , ("4.3.9", SetGroupMute (GroupId 1) Off, "heos://group/set_mute?gid=1&state=off")
  , ("4.3.10", ToggleGroupMute (GroupId 1), "heos://group/toggle_mute?gid=1")
  , ("4.4.1", GetMusicSources, "heos://browse/get_music_sources")
  , ("4.4.2", GetSourceInfo (SourceId 1), "heos://browse/get_source_info?sid=1")
  , ("4.4.3", BrowseSource (SourceId 1), "heos://browse/browse?sid=1")
  , ("4.4.4", BrowseContainer (SourceId 2) (ContainerId "TopAlbums") (Just (Range 0 100)), "heos://browse/browse?sid=2&cid=TopAlbums&range=0,100")
  , ("4.4.5", GetSearchCriteria (SourceId 3), "heos://browse/get_search_criteria?sid=3")
  , ("4.4.6", Search (SourceId 2) "U2" (SearchCriteriaId 1) Nothing, "heos://browse/search?sid=2&search=U2&scid=1")
  , ("4.4.7", PlayStation (PlayerId 1) (SourceId 2) (Just (ContainerId "CID-55")) (MediaId "15376") "Q95", "heos://browse/play_stream?pid=1&sid=2&cid=CID-55&mid=15376&name=Q95")
  , ("4.4.8", PlayPreset (PlayerId 1) 2, "heos://browse/play_preset?pid=1&preset=2")
  , ("4.4.9", PlayInput (PlayerId 1234) Nothing (InputName "inputs/aux_in_1"), "heos://browse/play_input?pid=1234&input=inputs/aux_in_1")
  , ("4.4.9 spid", PlayInput (PlayerId 1234) (Just (PlayerId 9876)) (InputName "inputs/aux_in_1"), "heos://browse/play_input?pid=1234&spid=9876&input=inputs/aux_in_1")
  , ("4.4.10", PlayUrl (PlayerId 1) "http://10.110.25.159:49152/web/138.mp3", "heos://browse/play_stream?pid=1&url=http://10.110.25.159:49152/web/138.mp3")
  , ("4.4.11", AddContainerToQueue (PlayerId 1) (SourceId 5) (ContainerId "Artist/All") AddPlayNext, "heos://browse/add_to_queue?pid=1&sid=5&cid=Artist/All&aid=2")
  , ("4.4.12", AddTrackToQueue (PlayerId 1) (SourceId 8) (ContainerId "Artists/All") (MediaId "9") AddPlayNow, "heos://browse/add_to_queue?pid=1&sid=8&cid=Artists/All&mid=9&aid=1")
  , ("4.4.14", RenamePlaylist (SourceId 11) (ContainerId "234") "new name", "heos://browse/rename_playlist?sid=11&cid=234&name=new name")
  , ("4.4.15", DeletePlaylist (SourceId 11) (ContainerId "234"), "heos://browse/delete_playlist?sid=11&cid=234")
  , ("4.4.17", RetrieveMetadata (SourceId 2) (ContainerId "Alb.184664171"), "heos://browse/retrieve_metadata?sid=2&cid=Alb.184664171")
  , ("4.4.19 opt 1", SetServiceOption (AddTrackToLibrary (SourceId 2) (MediaId "Tra.174684187")), "heos://browse/set_service_option?sid=2&option=1&mid=Tra.174684187")
  , ("4.4.19 opt 2", SetServiceOption (AddAlbumToLibrary (SourceId 2) (ContainerId "Alb.174684186")), "heos://browse/set_service_option?sid=2&option=2&cid=Alb.174684186")
  , ("4.4.19 opt 3", SetServiceOption (AddStationToLibrary (SourceId 2) (MediaId "as.6513639")), "heos://browse/set_service_option?sid=2&option=3&mid=as.6513639")
  , ("4.4.19 opt 4", SetServiceOption (AddPlaylistToLibrary (SourceId 2) (ContainerId "LIBPLAYLIST-pp.175573149") "Lupe Fiasco"), "heos://browse/set_service_option?sid=2&option=4&cid=LIBPLAYLIST-pp.175573149&name=Lupe Fiasco")
  , ("4.4.19 opt 5", SetServiceOption (RemoveTrackFromLibrary (SourceId 2) (MediaId "Tra.174684187")), "heos://browse/set_service_option?sid=2&option=5&mid=Tra.174684187")
  , ("4.4.19 opt 6", SetServiceOption (RemoveAlbumFromLibrary (SourceId 2) (ContainerId "LIBALBUM-Alb.174684186")), "heos://browse/set_service_option?sid=2&option=6&cid=LIBALBUM-Alb.174684186")
  , ("4.4.19 opt 7", SetServiceOption (RemoveStationFromLibrary (SourceId 2) (MediaId "as.6513639")), "heos://browse/set_service_option?sid=2&option=7&mid=as.6513639")
  , ("4.4.19 opt 8", SetServiceOption (RemovePlaylistFromLibrary (SourceId 2) (ContainerId "LIBPLAYLIST-mp.186017722")), "heos://browse/set_service_option?sid=2&option=8&cid=LIBPLAYLIST-mp.186017722")
  , ("4.4.19 opt 11", SetServiceOption (ThumbsUp (SourceId 1) (PlayerId (-409995282))), "heos://browse/set_service_option?sid=1&option=11&pid=-409995282")
  , ("4.4.19 opt 12", SetServiceOption (ThumbsDown (SourceId 1) (PlayerId (-409995282))), "heos://browse/set_service_option?sid=1&option=12&pid=-409995282")
  , ("4.4.19 opt 13 scid", SetServiceOption (CreateStation (SourceId 1) "Love" (Just (SearchCriteriaId 1)) Nothing), "heos://browse/set_service_option?sid=1&option=13&name=Love&scid=1")
  , ("4.4.19 opt 13 range", SetServiceOption (CreateStation (SourceId 1) "Love" Nothing (Just (Range 0 10))), "heos://browse/set_service_option?sid=1&option=13&name=Love&range=0,10")
  , ("4.4.19 opt 19 now playing", SetServiceOption (AddNowPlayingToFavorites (PlayerId (-409995282))), "heos://browse/set_service_option?option=19&pid=-409995282")
  , ("4.4.19 opt 19 browse", SetServiceOption (AddStationToFavorites (SourceId 3) (MediaId "sas.6513639") "Folk Radio"), "heos://browse/set_service_option?sid=3&option=19&mid=sas.6513639&name=Folk Radio")
  , ("4.4.19 opt 20", SetServiceOption (RemoveFromFavorites (MediaId "sas.6513639")), "heos://browse/set_service_option?option=20&mid=sas.6513639")
  ]
