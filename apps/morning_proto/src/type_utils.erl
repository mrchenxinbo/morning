% /*
%  * @Author: mrchenxinbo 
%  * @Date: 2023-12-07 14:49:45 
%  * @Last Modified by:   mrchenxinbo 
%  * @Last Modified time: 2023-12-07 14:49:45 
%  */
-module(type_utils).
-export([to_integer/1, to_integer/2, to_float/1, to_float/2, to_binary/1, to_binary/2,
         to_enum/3]).

to_integer(undefined) ->
    undefined;
to_integer(V) ->
    to_integer(V, 0).

to_integer(V, D) ->
    try
        to_integer_internal(V)
    catch
        _:_ ->
            D
    end.

to_integer_internal(V) when is_float(V) ->
    erlang:round(V);
to_integer_internal(V) when is_list(V) ->
    list_to_integer(V);
to_integer_internal(V) when is_binary(V) ->
    binary_to_integer(V);
to_integer_internal(V) when is_integer(V) ->
    V.

to_float(undefined) ->
    undefined;
to_float(V) ->
    to_float(V, 0.0).

to_float(V, D) ->
    try
        to_float_internal(V)
    catch
        _:_ ->
            D
    end.

to_float_internal(V) when is_integer(V) ->
    erlang:float(V);
to_float_internal(V) when is_list(V) ->
    list_to_float(V);
to_float_internal(V) when is_binary(V) ->
    binary_to_float(V);
to_float_internal(V) when is_float(V) ->
    V.

to_binary(undefined) ->
    undefined;
to_binary(V) ->
    to_binary(V, <<"">>).

to_binary(V, D) ->
    try
        to_binary_internal(V)
    catch
        _:_ ->
            D
    end.

to_binary_internal(V) when is_integer(V) ->
    integer_to_binary(V);
to_binary_internal(V) when is_float(V) ->
    float_to_binary(V);
to_binary_internal(V) when is_atom(V) ->
    atom_to_binary(V, latin1);
to_binary_internal(V) ->
    iolist_to_binary(V).

to_enum(V, D, E) ->
    proplists:get_value(V, E, proplists:get_value(D, E, undefined)).

%%
%% Tests
%%
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

int_test() ->
    %% undefined is special because of gbp
    ?assertEqual(undefined, to_integer(undefined)),
    ?assertEqual(0, to_integer("abc")),
    ?assertEqual(11, to_integer(10.5)),
    ?assertEqual(10, to_integer(10.4)),
    ?assertEqual(100, to_integer("abc", 100)),
    ?assertEqual(0, to_integer("0")),
    ?assertEqual(1, to_integer("1")),
    ?assertEqual(0, to_integer("2.0")),
    ?assertEqual(1, to_integer("1")),
    ?assertEqual(1, to_integer(<<"1">>)),
    ?assertEqual(1, to_integer(1)).

float_test() ->
    %% undefined is special because of gbp
    ?assertEqual(undefined, to_float(undefined)),
    ?assertEqual(10.0, to_float(10)),
    ?assertEqual(0.0, to_float("abc")),
    ?assertEqual(10.0, to_float("abc", 10.0)),
    ?assertEqual(10.0, to_float("2", 10.0)),
    ?assertEqual(1.0, to_float("1.0")),
    ?assertEqual(1.0, to_float(<<"1.0">>)),
    ?assertEqual(1.0, to_float(1.0)),
    ?assertEqual(0.0, to_float(null)).

bianry_test() ->
    %% undefined is special because of gbp
    ?assertEqual(undefined, to_binary(undefined)),
    ?assertEqual(<<"1.00000000000000000000e+00">>, to_binary(1.0)),
    ?assertEqual(<<"1">>, to_binary(1)),
    ?assertEqual(<<"1">>, to_binary("1")),
    ?assertEqual(<<"ok">>, to_binary(ok)).

enum_test() ->
    E = [{<<"txt">>, text}, {<<"img">>, image}],
    ?assertEqual(text, to_enum(<<"txt">>, <<"txt">>, E)),
    ?assertEqual(text, to_enum(<<"txtx">>, <<"txt">>, E)),
    ?assertEqual(image, to_enum(<<"img">>, <<"txt">>, E)).

-endif.
