%%%-------------------------------------------------------------------
%% @doc http_service public API
%% @end
%%%-------------------------------------------------------------------

-module(http_service_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', []}]),
    {ok, Port} = application:get_env(http_service, port),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [{env, [{dispatch, Dispatch}]}]),
    Ret = http_service_sup:start_link(),
    http_service_dispatch:start(),
    http_service_handler:load(),
    Ret.

stop(_State) ->
    cowboy:stop_listener(http),
    http_service_dispatch:stop(),
    ok.

%% internal functions
get_paths() ->
    lists:flatten([Service:paths()
                   || Service <- application:get_env(http_service, services, [])]).
