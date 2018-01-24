%%%-------------------------------------------------------------------
%% @doc magic_man public API
%% @end
%%%-------------------------------------------------------------------

-module(file_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  file:make_dir("store"),  
  {ok, PortC} = application:get_env(port), 
  {Port, _} = string:to_integer(PortC),
  Dispatch = cowboy_router:compile([
    {'_', [{"/store/:id", file_store, []}]}, 
    {'_', [{"/ping", health_check, []}] }
  ]),

  {ok, _} = cowboy:start_clear(http, [{port, Port}],  #{
    env => #{dispatch => Dispatch} }
  ),
  file_server_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
