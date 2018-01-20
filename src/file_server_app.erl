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
  application:start(sasl),	
  application:start(crypto),
  application:start(cowlib),

  application:start(ranch),
  application:start(cowboy),

  file:make_dir("store"),  
  Dispatch = cowboy_router:compile([
    {'_', [{"/store/:id", file_store, []}]}, 
    {'_', [{"/ping", health_check, []}] }
  ]),

  {ok, _} = cowboy:start_clear(http, [{port, 8080}],  #{
    env => #{dispatch => Dispatch} }
  ),
  magic_man_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
