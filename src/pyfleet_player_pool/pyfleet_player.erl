%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. фев 2015 14:05
%%%-------------------------------------------------------------------
-module(pyfleet_player).
-author("davidqo").

-behaviour(gen_server).

-include("../../include/pyfleet_player.hrl").
-include("../../include/pyfleet_game_manager.hrl").
-include("../../include/pyfleet_requests.hrl").

%% API
-export([async_login/6, start_link/5, create_game/2, join_game/2, get_status/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(state, {
  name :: string(),
  security_data :: term(),
  game_manager_ref :: pid(),
  game_ref :: pid(),
  player_manager_ref :: pid(),
  api_sessions = [] :: [pid()],
  status = #player_status{} :: #player_status{}
}).

-define(SESSION_EXPIRED_TIMEOUT, 30000).

%%%===================================================================
%%% API
%%%===================================================================

async_login(PlayerSupRef, PlayerManagerRef, ApiRef, GameManagerRef, Username, SecurityData) ->
  io:format("Pyfleet player. Async login. Player sup ref: ~p, Player manager ref: ~p, Api ref: ~p, Username: ~p~n", [PlayerSupRef, PlayerManagerRef, ApiRef, Username]),
  supervisor:start_child(PlayerSupRef, [PlayerManagerRef, ApiRef, GameManagerRef, Username, SecurityData]).

create_game(PlayerRef, Options) ->
  gen_server:call(PlayerRef, {create_game, Options}).
%%--------------------------------------------------------------------

join_game(PlayerRef, GameId) ->
  gen_server:call(PlayerRef, {join_game, GameId}).
%%--------------------------------------------------------------------

get_status(PlayerRef) ->
  gen_server:call(PlayerRef, status).
%%--------------------------------------------------------------------

%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(PlayerManagerRef :: pid(), ApiRef :: pid(), GameManagerRef :: pid(), Username :: string(), SecurityData :: term()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(PlayerManagerRef, ApiRef, GameManagerRef, Username, SecurityData) ->
  io:format("Pyfleet player. start_link. Player manager ref: ~p, Api ref: ~p, Username:~p~n", [PlayerManagerRef, ApiRef, Username]),
  gen_server:start_link(?MODULE, [PlayerManagerRef, ApiRef, GameManagerRef, Username, SecurityData], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([PlayerManagerRef, ApiRef, GameManagerRef, Username, SecurityData]) ->
  io:format("Pyfleet player. Init player session. Player manager ref: ~p, username: ~p~n", [PlayerManagerRef, Username]),
  self() ! {login, ApiRef},
  {ok, #state{name = Username, security_data = SecurityData, player_manager_ref = PlayerManagerRef, game_manager_ref = GameManagerRef}, ?SESSION_EXPIRED_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
        State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_call(#pyfleet_player_request{body = Body}, _, State = #state{name = User, game_ref = undefined}) ->
  io:format("Pyfleet player. User ~p session received request ~p, but not currently joined in any game~n", [User, Body]),
  {reply, ok, State, ?SESSION_EXPIRED_TIMEOUT};
handle_call(#pyfleet_player_request{body = keepalive}, _, State = #state{name = User}) ->
  io:format("Pyfleet player. User ~p session received keepalive~n", [User]),
  {reply, ok, State, ?SESSION_EXPIRED_TIMEOUT};
handle_call({create_game, Options}, _, State = #state{game_manager_ref = GameManagerRef, name = User, game_ref = undefined, status = #player_status{} = PlayerStatus}) ->
  io:format("Pyfleet player. User ~p creates game..~n", [User]),
  case gen_server:call(GameManagerRef, #pyfleet_create_game{game_type = pyfleet, player_name = User, player_ref = self(), options = Options}) of
    {ok, GameRef} = Result ->
      {reply, Result, State#state{game_ref = GameRef, status = PlayerStatus#player_status{game = #player_game_status{last_game_report = waiting_for_start}}}, ?SESSION_EXPIRED_TIMEOUT};
    Result ->
      {reply, Result, State, ?SESSION_EXPIRED_TIMEOUT}
  end;
handle_call({join_game, GameId}, _, State = #state{game_manager_ref = GameManagerRef, name = User, game_ref = undefined, status = #player_status{} = PlayerStatus}) ->
  io:format("Pyfleet player. User ~p join to game ~p..~n", [User, GameId]),
  case gen_server:call(GameManagerRef, #pyfleet_join_game{game_id = GameId, player_name = User, player_ref = self()}) of
    {ok, GameRef} = Result ->
      {reply, Result, State#state{game_ref = GameRef, status = PlayerStatus#player_status{game = #player_game_status{last_game_report = waiting_for_start}}}, ?SESSION_EXPIRED_TIMEOUT};
    Result ->
      {reply, Result, State, ?SESSION_EXPIRED_TIMEOUT}
  end;
handle_call(status, _, State = #state{name = User, status = #player_status{} = PlayerStatus}) ->
  io:format("User ~p requests his status: ~p..~n", [User, PlayerStatus]),
  {reply, {ok, PlayerStatus}, State, ?SESSION_EXPIRED_TIMEOUT};
handle_call(Request, _From, State) ->
  io:format("Unexpected request: ~p ~p~n", [Request, State]),
  {reply, ok, State, ?SESSION_EXPIRED_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).

handle_cast(_Request, State) ->
  io:format("Pyfleet player. Unknown cast~n", []),
  {noreply, State, ?SESSION_EXPIRED_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info({login, ApiRef}, State = #state{player_manager_ref = PlayerManagerRef, name = Username, security_data = _}) ->
  io:format("Pyfleet player. Logging in.. TODO: request to DB~n", []),
  io:format("Pyfleet player. Done!~n"),
  notify_logged_in(PlayerManagerRef, ApiRef, Username, self()),
  {noreply, State, ?SESSION_EXPIRED_TIMEOUT};
handle_info(timeout, State = #state{name = User}) ->
  io:format("Pyfleet player. User ~p session is expired~n", [User]),
  {stop, {shutdown, session_expired}, State};
handle_info({tick, Info}, State = #state{status = #player_status{} = PlayerStatus}) ->
  {noreply, State#state{status = PlayerStatus#player_status{game = Info}}, ?SESSION_EXPIRED_TIMEOUT};
handle_info({'DOWN', _, _, _, _}, State) ->
  io:format("Pyfleet player. Game is down..~n", []),
  {noreply, State#state{game_ref = undefined}, ?SESSION_EXPIRED_TIMEOUT};
handle_info(Info, State) ->
  io:format("Unexpected info: ~p~n", [Info]),
  {noreply, State, ?SESSION_EXPIRED_TIMEOUT}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
        State :: #state{}) -> term()).
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
        Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

notify_logged_in(PyfleetManagerRef, ApiRef, Username, UserRef) ->
  gen_server:cast(PyfleetManagerRef, {logged_in, Username, UserRef, ApiRef}).
%%++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
