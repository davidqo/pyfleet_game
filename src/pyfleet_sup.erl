-module(pyfleet_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  io:format("Pyfleet app. Starting..~n", []),
  {ok, ServiceSupervisorPid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  io:format("Pyfleet app. Starting pyfleet game manager..~n", []),
  {ok, GameManagerPid} = pyfleet_game_manager_service_sup:start_as_child(ServiceSupervisorPid),
  io:format("Done! Pyfleet game manager is running with pid ~p..~n", [GameManagerPid]),
  {ok, ServiceSupervisorPid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {rest_for_one, 10, 10}, []} }.

