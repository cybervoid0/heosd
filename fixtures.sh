#!/usr/bin/env bash
set -eu

HOST=Marantz-MODEL-60n.local
PORT=1255
OUT=test/fixtures/streams
PID=-1245882071

#mkdir -p "$OUT"

ask () {   # ask <имя файла> <команда> [сколько ждать]
  { printf 'heos://system/prettify_json_response?enable=off\r\n'; sleep 1
    printf 'heos://%s\r\n' "$2"; sleep "${3:-2}"
  } | nc -c -w 3 "$HOST" "$PORT" | tail -n +2 > "$OUT/$1.txt"
  echo "$1"
}

ask heart_beat          "system/heart_beat"
ask check_account       "system/check_account"
ask get_players         "player/get_players"
ask get_music_sources   "browse/get_music_sources"
ask get_player_info     "player/get_player_info?pid=$PID"
ask now_playing         "player/get_now_playing_media?pid=$PID"
ask get_queue           "player/get_queue?pid=$PID"
ask search_criteria     "browse/get_search_criteria?sid=10"
ask search_ok_computer  "browse/search?sid=10&search=Ok Computer&scid=3" 4
ask search_nothing      "browse/search?sid=10&search=qwertyuiopasdfgh&scid=3" 4
ask error_bad_pid       "player/get_play_state?pid=999999999"
