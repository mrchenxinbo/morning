%%%-------------------------------------------------------------------
%% @doc morning public API
%% @end
%%%-------------------------------------------------------------------

-module(morning_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    morning_token:start(),
    morning_sup:start_link().
    



stop(_State) ->
    ok.

%% internal functions
