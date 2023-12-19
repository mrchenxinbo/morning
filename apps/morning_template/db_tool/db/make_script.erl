#!/usr/bin/env escript
-module(make_script).
-export([main/1]).

main(A)->
    Files = filelib:wildcard("*.erl"),
    lists:foreach(fun(Filename) -> c:c(Filename, [{outdir, "ebin"}]) end, Files).