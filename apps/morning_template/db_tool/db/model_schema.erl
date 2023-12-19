-module(model_schema).

-include("model_define.hrl").

-compile(export_all).

get_index_name(Index) when is_list(Index)->
	string:join([atom_to_list(X)||X<-Index], "_");
get_index_name(Index) when is_atom(Index)->
	atom_to_list(Index);
get_index_name({unique, Index}) ->
	"unique_"++get_index_name(Index);
get_index_name({fulltext, Index}) ->
	"fulltext_"++get_index_name(Index).

create_index([])->
    [];
create_index(Indexs)->
    create_index(Indexs, []).

create_index([], IndexSQLList) ->
    ",\r\n  " ++ string:join(IndexSQLList, ",\r\n  ");
create_index([Index|Indexs], IndexSQLList) when is_list(Index) ->
    create_index(Indexs, ["KEY "++get_index_name(Index)++"(" ++ string:join(["`"++atom_to_list(X)++"`"||X<-Index], ",") ++ ")"|IndexSQLList]);
create_index([Index|Indexs], IndexSQLList) when is_atom(Index) ->
    create_index(Indexs, ["KEY "++get_index_name(Index)++"(`" ++ atom_to_list(Index) ++ "`)"|IndexSQLList]);
create_index([{unique, Index}|Indexs], IndexSQLList) when is_list(Index) ->
    create_index(Indexs, ["UNIQUE KEY "++get_index_name({unique, Index})++"(" ++ string:join(["`"++atom_to_list(X)++"`"||X<-Index], ",") ++ ")" |IndexSQLList]);
create_index([{unique, Index}|Indexs], IndexSQLList) when is_atom(Index) ->
    create_index(Indexs, ["UNIQUE KEY "++get_index_name({unique, Index})++"(`" ++ atom_to_list(Index) ++ "`)" |IndexSQLList]);
create_index([{fulltext, Index}|Indexs], IndexSQLList) when is_list(Index) ->
    create_index(Indexs, ["FULLTEXT KEY "++get_index_name({fulltext, Index})++"(" ++ string:join(["`"++atom_to_list(X)++"`"||X<-Index], ",") ++ ")" |IndexSQLList]);
create_index([{fulltext, Index}|Indexs], IndexSQLList) when is_atom(Index) ->
    create_index(Indexs, ["FULLTEXT KEY "++get_index_name({fulltext, Index})++"(`" ++ atom_to_list(Index) ++ "`)" |IndexSQLList]).


add_index(Index) when is_list(Index) ->
    "ADD INDEX "++get_index_name(Index)++"(" ++ string:join(["`"++atom_to_list(X)++"`"||X<-Index], ",") ++ ")";
add_index(Index) when is_atom(Index) ->
    "ADD INDEX "++get_index_name(Index)++"(`" ++ atom_to_list(Index) ++ "`)";
add_index({unique, Index}) when is_list(Index) ->
    "ADD UNIQUE INDEX "++get_index_name({unique, Index})++"(" ++ string:join(["`"++atom_to_list(X)++"`"||X<-Index], ",") ++ ")";
add_index({unique, Index}) when is_atom(Index) ->
    "ADD UNIQUE INDEX "++get_index_name({unique, Index})++"(`" ++ atom_to_list(Index) ++ "`)";
add_index({fulltext, Index}) when is_list(Index) ->
    "ADD FULLTEXT INDEX "++get_index_name({fulltext, Index})++"(" ++ string:join(["`"++atom_to_list(X)++"`"||X<-Index], ",") ++ ")";
add_index({fulltext, Index}) when is_atom(Index) ->
    "ADD FULLTEXT INDEX "++get_index_name({fulltext, Index})++"(`" ++ atom_to_list(Index) ++ "`)".


create_columns(ModelAttrList, PrimaryKeys)->
	SQLList = create_columns(ModelAttrList, PrimaryKeys, []),
	string:join(SQLList, ",\r\n  ").


create_columns([],_PrimaryKeys, SQL)->
	lists:reverse(SQL);
create_columns([ModelAttr|Tail], PrimaryKeys, SQL)->
	ColumnDefSQL = create_column_sql(ModelAttr, PrimaryKeys),
	create_columns(Tail, PrimaryKeys, [ColumnDefSQL|SQL]).

create_column_sql(#model_attr{name		=	Name,
							  type		=	Type,
							  length	=	Length,
							  float		=	Float,
							  cannull	=	Cannull,
							  default	=	Default,
							  autoinc	=	Autoinc,
							  info		=	Info}, PrimaryKeys)->
	create_column_sql(Name, Type, Length, Float, Cannull, Default, Autoinc, Info, PrimaryKeys).

create_column_sql(Name, Type, Length, Float, Cannull, Default, Autoinc, Info, PrimaryKeys) ->
	string:join([
				 "`"++atom_to_list(Name)++"`",
				 create_column_type(Type, Length, Float),
				 case lists:member(unsigned, Autoinc) of
					 true ->
						 "UNSIGNED";
					 false ->
						 ""
				 end,
				 if Cannull == notnull ->
						"NOT NULL "
							++
							case lists:member(Name, PrimaryKeys) of
								true -> "";
								false ->
									if Type == blob; Type == longblob; Type == tinyblob; Type == text ->"";
									   true->
										   "DEFAULT "++
											   if Type==datetime;
												  Type==date;
												  Type==time ->
													  if Default=="";
														 Default==undefined;
														 Default=={0,0,0};
														 Default=={{0,0,0},{0,0,0}} ->
															 "0";
														 true ->
															 pack_value(Default) end;
												  true->pack_value(Default)
											   end
									end
							end;
					true->"NULL" 
				 end,
				 case lists:member(auto, Autoinc) of
					 true ->
						 "AUTO_INCREMENT";
					 false ->
						 ""
				 end,
				 %create_column_options(Options),
				 "COMMENT '" ++ xmerl_ucs:to_utf8(Info) ++ "'"
				], " ").

create_column_type(Type, Length, Float) when    Type==float;
                                                Type==decimal;
                                                Type==double
                                                ->
	atom_to_list(Type)++"("++integer_to_list(Length)++","++integer_to_list(Float)++")";
create_column_type(Type, _Length, _Float) when  Type==blob;
												Type==longblob;
                                                Type==tinyblob;
                                                Type==datetime;
                                                Type==date;
                                                Type==time;
                                                Type==timestamp
                                                ->
	atom_to_list(Type);
create_column_type(term_varchar, Length, _Float)->
	"varchar("++integer_to_list(Length)++")";
create_column_type(term_char, Length, _Float)->
	"char("++integer_to_list(Length)++")";
create_column_type(text, _Length, _Float)->
	"text CHARACTER SET utf8 COLLATE utf8_general_ci";
create_column_type(Type, Length, _Float) ->
	atom_to_list(Type)++"("++integer_to_list(Length)++")".


create_primary([]) ->
	[];
create_primary(PrimaryKeys)->
	List = lists:map(fun(Column)->"`"++atom_to_list(Column)++"`" end, PrimaryKeys),
	string:join(List, ", ").


pack_value(undefined) ->
	"null";
pack_value(V) when is_binary(V) ->
    pack_value(binary_to_list(V));
pack_value(V) when is_list(V) ->
    "'" ++ escape_sql(V) ++ "'";
pack_value({MegaSec, Sec, MicroSec}) when is_integer(MegaSec) andalso is_integer(Sec) andalso is_integer(MicroSec) ->
    pack_datetime({MegaSec, Sec, MicroSec});
pack_value({{_, _, _}, {_, _, _}} = Val) ->
    pack_datetime(Val);
pack_value(Val) when is_integer(Val) ->
    integer_to_list(Val);
pack_value(Val) when is_float(Val) ->
    float_to_list(Val);
pack_value(true) ->
    "TRUE";
pack_value(false) ->
    "FALSE".

escape_sql(Value) ->
    escape_sql1(Value, []).

escape_sql1([], Acc) ->
    lists:reverse(Acc);
escape_sql1([$'|Rest], Acc) ->
    escape_sql1(Rest, [$', $'|Acc]);
escape_sql1([C|Rest], Acc) ->
    escape_sql1(Rest, [C|Acc]).

pack_datetime(undefined)->
    "0";
pack_datetime(0)->
	"0";
pack_datetime({{Y,M,D},{H,I,S}})->
    [format_time(X)||X<-[Y,M,D,H,I,S]];
pack_datetime({_,_,_}=Now)->
	{{Y,M,D},{H,I,S}} =calendar:now_to_local_time(Now),
	[format_time(X)||X<-[Y,M,D,H,I,S]].

format_time(Val) when Val < 10 ->
	"0" ++ integer_to_list(Val);
format_time(Val) ->
	integer_to_list(Val).
