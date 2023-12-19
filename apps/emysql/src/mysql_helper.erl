%% Author: Administrator
%% Description: TODO: Add description to mysql_helper
-module(mysql_helper).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([string_to_atom/1, string_to_term/1, pack_value_by_type/1, pack_value/1, pack_where/1, pack_limit/1, pack_orderby/1, unpack_row/2, pack_update_columns/1]).
-export([unpack_value_by_type/1, row_to_kv/2, pack_kv/2]).
%%
%% API Functions
%%

unpack_value_by_type({Val, Type}) when Type == term_varchar;
    Type == term_char ->
    string_to_term(Val);
unpack_value_by_type({Val, _}) ->
    Val.

string_to_term("") ->
    [];
string_to_term(Val) when is_binary(Val) ->
    string_to_term(binary_to_list(Val));
string_to_term(Val) ->
    {ok, Term} = util:string_to_term(Val),
    Term.

string_to_atom(Val) when is_list(Val) ->
    list_to_atom(Val).

pack_value_by_type({Val, blob}) ->
    pack_value(term_to_binary(Val));
pack_value_by_type({Val, Type}) when Type == term_varchar;
    Type == term_char ->
    pack_value(util:term_to_string(Val));
pack_value_by_type({Val, Type}) when Type == time ->
    pack_time(Val);
pack_value_by_type({Val, _Type}) ->
    pack_value(Val).

pack_value(undefined) ->
    "null";
pack_value(true) ->
    "TRUE";
pack_value(false) ->
    "FALSE";
pack_value(Val) when is_atom(Val) ->
    pack_value(atom_to_list(Val));
pack_value(Val) when is_integer(Val) ->
    integer_to_list(Val);
pack_value(Val) when is_float(Val) ->
    float_to_list(Val);
pack_value({MegaSec, Sec, MicroSec}) when is_integer(MegaSec) andalso is_integer(Sec) andalso is_integer(MicroSec) ->
    pack_datetime({MegaSec, Sec, MicroSec});
pack_value({{_, _, _}, {_, _, _}} = Val) ->
    pack_datetime(Val);
pack_value(V) when is_binary(V) ->
    quote(binary_to_list(V));
pack_value(V) when is_list(V) ->%string
    quote(V).

pack_time(undefined) ->
    "null";
pack_time({0, 0, 0}) ->
    "'00:00:00'";
pack_time({H, M, S}) ->
    [format_time(X) || X <- [H, M, S]].

pack_datetime(undefined) ->
    "null";
pack_datetime(0) ->
    "null";
pack_datetime({0, 0, 0}) ->
    "'0000-00-00 00:00:00'";
pack_datetime({{Y, M, D}, {H, I, S}}) ->
    [format_time(X) || X <- [Y, M, D, H, I, S]];
pack_datetime({_, _, _} = Now) ->
    {{Y, M, D}, {H, I, S}} = calendar:now_to_local_time(Now),
    "'" ++ string:join([format_time(X) || X <- [Y, M, D]], "-") ++ " " ++ string:join([format_time(X) || X <- [H, I, S]], ":") ++ "'".

format_time(Val) when Val < 10 ->
    "0" ++ integer_to_list(Val);
format_time(Val) ->
    integer_to_list(Val).

pack_where(Conditions) ->
    Sql = pack_kv(Conditions, []),
    case length(Sql) of
        0 ->
            "";
        _Any ->
            " WHERE " ++ string:join(Sql, " AND ")
    end.

pack_kv([], Sql) ->
    lists:reverse(Sql);
pack_kv([{ColumeName, '=', Value} | Tail], Sql) ->
    New = "`" ++ atom_to_list(ColumeName) ++ "`" ++ "=" ++ pack_value_by_type(Value),
    pack_kv(Tail, [New | Sql]);
pack_kv([{ColumeName, '!=', Value} | Tail], Sql) ->
    New = "`" ++ atom_to_list(ColumeName) ++ "`" ++ "!=" ++ pack_value_by_type(Value),
    pack_kv(Tail, [New | Sql]);
pack_kv([{ColumeName, '>', Value} | Tail], Sql) ->
    New = "`" ++ atom_to_list(ColumeName) ++ "`" ++ ">" ++ pack_value_by_type(Value),
    pack_kv(Tail, [New | Sql]);
pack_kv([{ColumeName, '<', Value} | Tail], Sql) ->
    New = "`" ++ atom_to_list(ColumeName) ++ "`" ++ "<" ++ pack_value_by_type(Value),
    pack_kv(Tail, [New | Sql]);
pack_kv([{ColumeName, '>=', Value} | Tail], Sql) ->
    New = "`" ++ atom_to_list(ColumeName) ++ "`" ++ ">=" ++ pack_value_by_type(Value),
    pack_kv(Tail, [New | Sql]);
pack_kv([{ColumeName, '<=', Value} | Tail], Sql) ->
    New = "`" ++ atom_to_list(ColumeName) ++ "`" ++ "<=" ++ pack_value_by_type(Value),
    pack_kv(Tail, [New | Sql]);
pack_kv([{ColumeName, in, {Value, Type}} | Tail], Sql) when is_list(Value) ->
    New = "`" ++ atom_to_list(ColumeName) ++ "`" ++ " IN " ++ pack_set_by_type({Value, Type}),
    pack_kv(Tail, [New | Sql]);
pack_kv([{ColumeName, not_in, {Value, Type}} | Tail], Sql) when is_list(Value) ->
    New = "`" ++ atom_to_list(ColumeName) ++ "`" ++ " NOT IN " ++ pack_set_by_type({Value, Type}),
    pack_kv(Tail, [New | Sql]);
pack_kv([{ColumeName, like, Value} | Tail], Sql) ->
    New = "`" ++ atom_to_list(ColumeName) ++ "`" ++ " LIKE " ++ pack_value_by_type(Value),
    pack_kv(Tail, [New | Sql]).

pack_set_by_type({Values, _Type}) ->
    "(" ++ string:join(lists:map(fun pack_value/1, Values), ", ") ++ ")".

pack_update_columns(Columns) ->
    KV = pack_kv(Columns, []),
    case length(KV) of
        0 ->
            "";
        _Any ->
            " SET " ++ string:join(KV, ", ")
    end.

pack_limit(undefined) ->
    "";
pack_limit(Limit) when is_integer(Limit) ->
    " LIMIT 0," ++ integer_to_list(Limit);
pack_limit({From, Count}) ->
    " LIMIT " ++ integer_to_list(From) ++ "," ++ integer_to_list(Count).

pack_orderby(undefined) ->
    "";
pack_orderby({Column, asc}) ->
    " ORDER BY " ++ atom_to_list(Column) ++ " ASC";
pack_orderby({Column, desc}) ->
    " ORDER BY " ++ atom_to_list(Column) ++ " DESC".

unpack_row(Module, RowColumnDataList) ->
    list_to_tuple([Module | RowColumnDataList]).
row_to_kv(FieldList, RowRecord) ->
    [_ | List] = tuple_to_list(RowRecord),
    lists:zip(FieldList, List).
%%
%% Local Functions
%%

quote(String) when is_list(String) ->
    [39 | lists:reverse([39 | quote(String, [])])];    %% 39 is $'
quote(Bin) when is_binary(Bin) ->
    list_to_binary(quote(binary_to_list(Bin))).

quote([], Acc) ->
    Acc;
quote([0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\, $\\ | Acc]);
quote([39 | Rest], Acc) ->        %% 39 is $'
    quote(Rest, [39, $\\ | Acc]);    %% 39 is $'
quote([34 | Rest], Acc) ->        %% 34 is $"
    quote(Rest, [34, $\\ | Acc]);    %% 34 is $"
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).
