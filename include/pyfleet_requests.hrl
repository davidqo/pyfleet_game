%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Февр. 2015 17:29
%%%-------------------------------------------------------------------
-author("davidqo").

-record(pyfleet_player_request, {
  player_ref,
  player,
  sequrity_data,
  body
}).

-record(pyfleet_player_response, {
  player_ref,
  player,
  sequrity_data,
  body
}).

-record(pyfleet_player_game_notification, {
  player_ref,
  player,
  sequrity_data,
  body
}).
