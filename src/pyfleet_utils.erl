%%%-------------------------------------------------------------------
%%% @author davidqo
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Февр. 2015 12:59
%%%-------------------------------------------------------------------
-module(pyfleet_utils).
-author("davidqo").

%% API
-export([priv_dir/0, web_pages_dir/0, read_web_page/1]).

priv_dir() ->
  case code:priv_dir(pyfleet) of
    {error, _} ->
      case file:get_cwd() of
        {ok, Cwd} ->
          filename:join(Cwd, "priv");
        _ ->
          throw(cannot_find_priv)
      end;
    PrivDir ->
      PrivDir
  end.
%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

web_pages_dir() ->
  filename:join(priv_dir(), "www").
%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-spec read_web_page(PageFilename :: string()) -> {ok, Content :: binary()} | {error, term()}.
read_web_page(PageFilename) ->
  Path = filename:join(web_pages_dir(), PageFilename),
  file:read_file(Path).
%%+++++++++++++++++++++++++++++++++++++++++++++++++++++++++