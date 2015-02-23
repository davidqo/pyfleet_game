%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. фев 2015 14:54
%%%-------------------------------------------------------------------
-module(pyfleet_game_manager_sup).
-author("davidqo").

-behaviour(supervisor).

%% API
-export([start_link/1]).

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
-spec(start_link(PyfleetGameManagerServiceSup :: pid()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(PyfleetGameManagerServiceSup) ->
    supervisor:start_link(?MODULE, [PyfleetGameManagerServiceSup]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([PyfleetGameManagerServiceSup]) ->

    SupFlags = {one_for_all, 0, 3600},
    PyfleetGameManagerSup = self(),
    Child = {'pyfleet_game_manager', {'pyfleet_game_manager', start_link, [PyfleetGameManagerServiceSup, PyfleetGameManagerSup]},
        transient, 2000, worker, ['pyfleet_game_manager']},
    {ok, {SupFlags, [Child]}}.

%%%= ==================================================================
%%% Internal functions
%%%===================================================================
