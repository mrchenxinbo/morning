%%%-------------------------------------------------------------------
%% @doc morning_template public API
%% @end
%%%-------------------------------------------------------------------

-module(morning_template_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    morning_template_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
