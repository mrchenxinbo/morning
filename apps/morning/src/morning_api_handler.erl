%%%-------------------------------------------------------------------
%% @doc morning public API
%% @end
%%%-------------------------------------------------------------------

-module(morning_api_handler).


-export([login/1, handle/2]).


login({User, Password, Type})->
    Expire = time_util:erlang_system_time(seconds)+3600*12,
    #{token => <<"YWMt39RfMMOqEeKYE_GW7tu81ABCDT71lGijyjG4VUIC2AwZGzUjVbPp_4qRD5k">>, expire_in => Expire}.


handle(login, {Uid, Passward, Channel})->
    ok.
