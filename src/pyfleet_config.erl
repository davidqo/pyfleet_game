-module(pyfleet_config).

-export([
    dispatch/1,
    web_config/1
]).

-spec dispatch(PlayerManagerRef :: pid()) -> [webmachine_dispatcher:route()].
dispatch(PlayerManagerRef) ->
    lists:flatten([
        {['*'], pyfleet_resource, [{player_manager_ref, PlayerManagerRef}]}
    ]).

web_config(PlayerManagerRef) ->
    {ok, App} = application:get_application(?MODULE),
    {ok, Ip} = application:get_env(App, web_ip),
    {ok, Port} = application:get_env(App, web_port),
    [
        {ip, Ip},
        {port, Port},
        {log_dir, "priv/log"},
        {dispatch, dispatch(PlayerManagerRef)}
    ].