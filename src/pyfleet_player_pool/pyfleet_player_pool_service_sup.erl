%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. фев 2015 14:54
%%%-------------------------------------------------------------------
-module(pyfleet_player_pool_service_sup).
-author("davidqo").

-behaviour(supervisor).

%% API
-export([
  start_link/1,
  start_as_child/2
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(GameManagerPid :: pid()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(GameManagerPid) ->
    supervisor:start_link(?MODULE, [GameManagerPid]).

-spec(start_as_child(ParentServiceSup :: pid(), GameManagerPid :: pid()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_as_child(ParentServiceSup, GameManagerPid) ->
  io:format("Create pyfleet player pool. Parent service sup: ~p~n", [ParentServiceSup]),
  ChildSpec = {pyfleet_player_pool_service_sup, {pyfleet_player_pool_service_sup, start_link, [GameManagerPid]},
    transient, 2000, supervisor, [pyfleet_player_pool_service_sup]},
  supervisor:start_child(ParentServiceSup, ChildSpec).
%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([GameManagerPid]) ->
    SupFlags = {rest_for_one, 1000, 3600},
    ServiceSup = self(),
    Child = {'pyfleet_player_pool_sup', {'pyfleet_player_pool_sup', start_link, [ServiceSup, GameManagerPid]},
        transient, 2000, supervisor, ['pyfleet_player_pool_sup']},
    {ok, {SupFlags, [Child]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
