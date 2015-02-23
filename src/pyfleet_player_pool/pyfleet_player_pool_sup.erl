%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. фев 2015 14:54
%%%-------------------------------------------------------------------
-module(pyfleet_player_pool_sup).
-author("davidqo").

-behaviour(supervisor).

%% API
-export([
  start_link/2
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
-spec(start_link(ServiceSup :: pid(), GameManagerPid :: pid()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ServiceSup, GameManagerPid) ->
  io:format("Start pyfleet player manager supervisor~n", []),
  supervisor:start_link(?MODULE, [ServiceSup, GameManagerPid]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([ServiceSup, GameManagerPid]) ->
    io:format("Init pyfleet player manager supervisor~n", []),
    SupFlags = {one_for_all, 0, 3600},
    PyfleetPlayerPoolSup = self(),
    Child = {'pyfleet_player_manager', {'pyfleet_player_manager', start_link, [ServiceSup, PyfleetPlayerPoolSup, GameManagerPid]},
        transient, 2000, worker, ['pyfleet_player_manager']},
    {ok, {SupFlags, [Child]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
