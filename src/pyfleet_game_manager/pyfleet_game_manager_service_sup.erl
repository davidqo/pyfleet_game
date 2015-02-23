%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. фев 2015 14:54
%%%-------------------------------------------------------------------
-module(pyfleet_game_manager_service_sup).
-author("davidqo").

-behaviour(supervisor).

%% API
-export([start_link/0, start_as_child/1]).

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
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link(?MODULE, []).

-spec(start_as_child(ParentServiceSup :: pid()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_as_child(ParentServiceSup) ->
  ChildSpec = {pyfleet_game_manager_service_sup, {pyfleet_game_manager_service_sup, start_link, []},
    transient, 2000, supervisor, [pyfleet_game_manager_service_sup]},
  supervisor:start_child(ParentServiceSup, ChildSpec).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = {rest_for_one, 1000, 3600},
    PyfleetGameManagerServiceSup = self(),
    Child = {'pyfleet_game_manager_sup', {'pyfleet_game_manager_sup', start_link, [PyfleetGameManagerServiceSup]},
        transient, 2000, supervisor, ['pyfleet_game_manager_sup']},
    {ok, {SupFlags, [Child]}}.

%%%= ==================================================================
%%% Internal functions
%%%===================================================================
