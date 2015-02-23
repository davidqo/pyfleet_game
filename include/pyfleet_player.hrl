%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Февр. 2015 18:05
%%%-------------------------------------------------------------------
-author("davidqo").

-record(player_game_status, {
  last_game_report
}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++

-record(player_status, {
  general = normal,
  game = not_joined :: not_joined | #player_game_status{}
}).
%%+++++++++++++++++++++++++++++++++++++++++++++++++++++