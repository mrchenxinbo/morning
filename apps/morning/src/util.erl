%%%-------------------------------------------------------------------
%% @doc morning util API
%% @end
%%%-------------------------------------------------------------------

-module(util).

-define(DEFAULT_TIMEOUT, 2000).
-export([to_list/1, get_redis_timeout/1, partition_n/2]).


to_list(V) when is_list(V)->
    V;
to_list(V) when is_binary(V)->
    binary_to_list(V);
to_list(V) when is_integer(V)->
    integer_to_list(V);
to_list(V) when is_atom(V)->
    atom_to_list(V);
to_list(V) ->
    V.


get_redis_timeout(Name) ->
    {ok, Props} = application:get_env(morning, Name),
    DefaultTimeOut = application:get_env(morning, redis_timeout, ?DEFAULT_TIMEOUT),
    proplists:get_value(redis_timeout, Props, DefaultTimeOut).

partition_n(L, N) ->
    lists:reverse(partition_n(L, N, [])).
    
partition_n([], _N, Acc) ->
    Acc;
partition_n(L, N, Acc) ->
    try
        {L1, L2} = lists:split(N, L),
        partition_n(L2, N, [L1 | Acc])
    catch
        error:badarg ->
            [L | Acc]
    end.