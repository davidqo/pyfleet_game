-module(pyfleet_resource).
-export([
    init/1,
    to_html/2
]).

-include_lib("webmachine/include/webmachine.hrl").
-include_lib("webmachine/include/wm_reqstate.hrl").

-record(state, {
  player_manager_ref,
  %%not used yet:
  security_data
}).

-spec init(list()) -> {ok, term()}.
init(Options) ->
  PlayerManagerRef = proplists:get_value(player_manager_ref, Options),
  {ok, #state{player_manager_ref = PlayerManagerRef}}.

-spec to_html(wrq:reqdata(), term()) -> {iodata(), wrq:reqdata(), term()}.

to_html(ReqData = #wm_reqdata{path_tokens = ["users", User], wm_state = #wm_reqstate{reqdata = #wm_reqdata{req_qs = []}}}, State = #state{player_manager_ref = PlayerManagerRef}) ->
  {ok, PlayerRef} = pyfleet_player_manager:relogin(PlayerManagerRef, User, dummy_security_data),
  EncodedPlayerRef = player_ref_encode(PlayerRef),
  case pyfleet_player:join_game(PlayerRef, 1) of
    {ok, _} ->
      ok;
    _ ->
      pyfleet_player:create_game(PlayerRef, [])
  end,
  ReqData2 = wrq:set_resp_header("Location", "?player_ref=" ++ EncodedPlayerRef, ReqData),
  {{halt, 302}, ReqData2, State};
to_html(#wm_reqdata{path_tokens = ["users", _User], wm_state = #wm_reqstate{reqdata = #wm_reqdata{req_qs = ReqQs}}} = ReqData, State = #state{}) ->
  PlayerRefEncoded = proplists:get_value("player_ref", ReqQs),
  io:format("PlayerRefEncoded = ~p~n", [PlayerRefEncoded]),
  PlayerRef = player_ref_decode(PlayerRefEncoded),
  {ok, PollingPageBegin} = pyfleet_utils:read_web_page("polling_page_begin.html"),
  {ok, Status} = pyfleet_player:get_status(PlayerRef),
  StatusInfo = lists:flatten(io_lib:format("<p>Player status: ~p</p>", [Status])),
  {ok, PollingPageEnd} = pyfleet_utils:read_web_page("polling_page_end.html"),
  PollingPage = [PollingPageBegin, StatusInfo, PollingPageEnd],
  {PollingPage, ReqData, State#state{}};
to_html(#wm_reqdata{path = "/"} = ReqData, State) ->
  ReqData2 = wrq:set_resp_header("Location", "login", ReqData),
  {{halt, 302}, ReqData2, State};
to_html(#wm_reqdata{path = "/login", wm_state = #wm_reqstate{reqdata = #wm_reqdata{req_qs = []}}} = ReqData, State) ->
  {ok, LoginPage} = pyfleet_utils:read_web_page("login_page.html"),
  {LoginPage, ReqData, State};
to_html(#wm_reqdata{path = "/login", wm_state = #wm_reqstate{reqdata = #wm_reqdata{req_qs = Arguments}}} = ReqData, State = #state{player_manager_ref = PlayerManagerRef}) ->
  PlayerName = proplists:get_value("login", Arguments),
  io:format("Http user login: ~p~n", [PlayerName]),
  Ret = pyfleet_player_manager:async_login(PlayerManagerRef, PlayerName, dummy_security_data),
  io:format("Http user logging in res: ~p~n", [Ret]),
  ReqData2 = wrq:set_resp_header("Location", ["users/", PlayerName], ReqData),
  {{halt, 302}, ReqData2, State#state{}};
to_html(#wm_reqdata{} = ReqData, State) ->
  {ok, ResourceUnknownPage} = pyfleet_utils:read_web_page("resource_unknown.html"),
  {ResourceUnknownPage, ReqData, State}.

player_ref_encode(PlayerRef) ->
  pid_to_list(PlayerRef).

player_ref_decode(PlayerRefEncoded) ->
  list_to_pid(PlayerRefEncoded).

