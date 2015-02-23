-module(pyfleet_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  application:ensure_all_started(pyfleet),
  application:start(pyfleet).

start(_StartType, _StartArgs) ->
  pyfleet_sup:start_link().

stop(_State) ->
    ok.
