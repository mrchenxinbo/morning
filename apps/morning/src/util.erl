%%%-------------------------------------------------------------------
%% @doc morning util API
%% @end
%%%-------------------------------------------------------------------

-module(util).

-export([to_list/1]).


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