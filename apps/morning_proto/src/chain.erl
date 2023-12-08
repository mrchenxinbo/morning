-module(chain).

-export([apply/2]).

apply(Operand, []) ->
    Operand;
apply(Operand, [{M, F, A} | Rest]) ->
    chain:apply(
        erlang:apply(M, F, [Operand | A]), Rest);
apply(Operand, [{F, A} | Rest]) ->
    chain:apply(
        erlang:apply(F, [Operand | A]), Rest).
