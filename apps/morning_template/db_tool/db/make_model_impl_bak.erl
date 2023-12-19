-module(make_model_impl).

-define(MODELPREFIX, "model_").
-define(PROTOPREFIX, "proto_").
-include("model_define.hrl").
-define(MODEL_CONFIG_ROOT, "config/").
-define(MODEL_CONFIG_PREFIX, "mdf_").
-define(MODEL_SRC_ROOT, "src/").

-export([run/0]).

run()->
	io:format("Start auto-coding model modules.~n"),
	create_model_files(),
	io:format("Complete auto-coding model modules.~n"),
	init:stop().

create_model_files()->
    ModelConfigFiles = filelib:wildcard(?MODEL_CONFIG_ROOT ++ ?MODEL_CONFIG_PREFIX ++ "*.config"),
	lists:foreach(
		fun(Model)->
			HeaderLength = length(?MODEL_CONFIG_ROOT)+length(?MODEL_CONFIG_PREFIX),
			NameLength = length(Model)-HeaderLength-length(".config"),
			ModName = string:substr(Model, HeaderLength+1, NameLength),
			create_model(Model, ModName)
		end, ModelConfigFiles).

create_model(ModFile, DBMod) ->
	case file:consult(ModFile) of
        {ok, Models} ->
            lists:foreach(fun(Model)-> inner_create_model(Model, DBMod) end, Models);
		Error ->
			io:format("~n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nConfig File ~p,~n Reason:~p~n~n", [ModFile, Error])
	end.

inner_create_model(#model_define{type=Type}, _DBMod) when Type =:= ?TABLE_TYPE_LOG ->
	nothing;
inner_create_model(#proto_define{name=Name, attrs=ModelAttrList, keys=KeyList, type=Type}, _DBMod)->
	RECORD_NAME = atom_to_list(Name),
	case file:open(?MODEL_SRC_ROOT++?PROTOPREFIX++RECORD_NAME++".erl", [write]) of
		{ok, File} ->
			ETSTABLENAME = string:to_upper(RECORD_NAME)++"_ETS",
			ModelAttrNames = [Proto#model_attr.name || Proto <- ModelAttrList, Proto#model_attr.flag =:= ?MODEL_FLAG_SERVER],
			ColumnListString = string:join(["`"++atom_to_list(Attr)++"`" || Attr <- ModelAttrNames], ","),
			TABLE_TYPE = atom_to_list(Type),
			KEY_POS = "["++string:join(["#"++RECORD_NAME++"."++atom_to_list(K)||K<-KeyList], ",")++"]",
			GET_CODE =string:join(["get_"++atom_to_list(Attr)++"(Record)->\r\n\t"++
								       "Record#"++RECORD_NAME++"."++atom_to_list(Attr)++".\r\n"||Attr<-ModelAttrNames], "\r\n"),
			SETORBAG_CODE = case Type of
								set ->
									"[{_,Info}|_]->Info";
								bag ->
									"Infos->[ Info || {_,Info} <- Infos]"
							end,
			KEY_ARG_LIST = string:join([string:to_upper(atom_to_list(K))||K<-KeyList], ","),
			INIT_FUN = 	"?INIT_ETS("++RECORD_NAME++", ?"++ETSTABLENAME++", "++KEY_POS++", []).",
			KVList =    [
						 	{"?ETSTABLENAME",           ETSTABLENAME},
                            {"?PROTOPREFIX",            ?PROTOPREFIX},
                            {"?RECORD_NAME",            RECORD_NAME},
                            {"?ColumnList",             ColumnListString},
                            {"?TABLE_TYPE",             TABLE_TYPE},
                            {"?KEY_POS",     			KEY_POS},
							{"?KEY_ARG_LIST",			KEY_ARG_LIST},
							{"?ETS_KEY_ARG",			case length(KeyList) of 1-> KEY_ARG_LIST;_->"{"++KEY_ARG_LIST++"}" end},
                            {"?GET_CODE",       		GET_CODE},
							{"?SETORBAG_CODE",			SETORBAG_CODE},
							{"?INIT_FUN", 				INIT_FUN}
                        ],

            Code = val_replace(KVList, 'PROTO_IMPL_FORMAT'()),

			io:format(File, Code, []),
			file:close(File);
		{error, Reason} ->
			io:format("~n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nOpen ~s.erl Error,~n Reason:~p~n~n", [?PROTOPREFIX++RECORD_NAME, Reason])
	end,
	io:format("Complete auto-coding ~s.erl.~n", [?PROTOPREFIX++RECORD_NAME]);
inner_create_model(#callback_proto_define{name=Name, attrs=ModelAttrList, keys=KeyList, type=Type, init_args=InitArgs}, DBMod)->
	RECORD_NAME = atom_to_list(Name),
	case file:open(?MODEL_SRC_ROOT++?PROTOPREFIX++RECORD_NAME++".erl", [write]) of
		{ok, File} ->
			ETSTABLENAME = string:to_upper(RECORD_NAME)++"_ETS",
			ModelAttrNames = [Proto#proto_attr.name || Proto <- ModelAttrList, Proto#proto_attr.flag =:= ?MODEL_FLAG_SERVER],
			ColumnListString = string:join(["`"++atom_to_list(Attr)++"`" || Attr <- ModelAttrNames], ","),
			TABLE_TYPE = atom_to_list(Type),
			KEY_POS = "["++string:join(["#"++RECORD_NAME++"."++atom_to_list(K)||K<-KeyList], ",")++"]",
			GET_CODE =string:join(["get_"++atom_to_list(Attr)++"(Record)->\r\n\t"++
								       "Record#"++RECORD_NAME++"."++atom_to_list(Attr)++".\r\n"||Attr<-ModelAttrNames], "\r\n"),
			SETORBAG_CODE = case Type of
								set ->
									"[{_,Info}|_]->Info";
								bag ->
									"Infos->[ Info || {_,Info} <- Infos]"
							end,
			KEY_ARG_LIST = string:join([string:to_upper(atom_to_list(K))||K<-KeyList], ","),
			INIT_FUN = 	"?INIT_ETS_WITH_CALLBACK("++RECORD_NAME++", ?"++ETSTABLENAME++", "++DBMod++", init_"++RECORD_NAME++", "++term_to_string(InitArgs)++").",
			KVList =    [
						 	{"?ETSTABLENAME",           ETSTABLENAME},
                            {"?PROTOPREFIX",            ?PROTOPREFIX},
                            {"?RECORD_NAME",            RECORD_NAME},
                            {"?ColumnList",             ColumnListString},
                            {"?TABLE_TYPE",             TABLE_TYPE},
                            {"?KEY_POS",     			KEY_POS},
							{"?KEY_ARG_LIST",			KEY_ARG_LIST},
							{"?ETS_KEY_ARG",			case length(KeyList) of 1-> KEY_ARG_LIST;_->"{"++KEY_ARG_LIST++"}" end},
                            {"?GET_CODE",       		GET_CODE},
							{"?SETORBAG_CODE",			SETORBAG_CODE},
							{"?INIT_FUN", 				INIT_FUN}
                        ],

            Code = val_replace(KVList, 'PROTO_IMPL_FORMAT'()),

			io:format(File, Code, []),
			file:close(File);
		{error, Reason} ->
			io:format("~n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nOpen ~s.erl Error,~n Reason:~p~n~n", [?PROTOPREFIX++RECORD_NAME, Reason])
	end,
	io:format("Complete auto-coding ~s.erl.~n", [?PROTOPREFIX++RECORD_NAME]);
inner_create_model(#model_define{name=Name, type = Type, attrs=ModelAttrList, primary_keys=KeyList}, _DBMod) when Type =:= ?TABLE_TYPE_PROTO ->
	RECORD_NAME = atom_to_list(Name),
	ModelAttrNames = [Proto#model_attr.name || Proto <- ModelAttrList, Proto#model_attr.flag =:= ?MODEL_FLAG_SERVER],
	case ModelAttrNames =/= [] of
		true ->
			case file:open(?MODEL_SRC_ROOT++?PROTOPREFIX++RECORD_NAME++".erl", [write]) of
				{ok, File} ->
					TABLE_TYPE = case length(KeyList)>1 of true -> "bag"; _ -> "set" end,
					ETSTABLENAME = string:to_upper(RECORD_NAME)++"_ETS",
					ColumnListString = string:join(["`"++atom_to_list(Attr)++"`" || Attr <- ModelAttrNames], ","),
					KEY_POS = "["++string:join(["#"++RECORD_NAME++"."++atom_to_list(K)||K<-KeyList], ",")++"]",
					COUNT_FIELD = case length(KeyList)>0 of true -> atom_to_list(hd(KeyList)); _ -> "id" end,
					GET_CODE =string:join(["get_"++atom_to_list(Attr)++"(Record)->\r\n\t"++
										       "Record#"++RECORD_NAME++"."++atom_to_list(Attr)++".\r\n"||Attr<-ModelAttrNames], "\r\n"),
					SETORBAG_CODE = case TABLE_TYPE of
										"set" ->
											"[{_,Info}|_]->Info";
										"bag" ->
											"Infos->[ Info || {_,Info} <- Infos]"
									end,
					KEY_ARG_LIST = string:join([string:to_upper(atom_to_list(K))||K<-KeyList], ","),
					INIT_FUN = 	"?INIT_ETS("++RECORD_NAME++", ?"++ETSTABLENAME++", "++KEY_POS++", select_all()).",
					TypeArgList = [{ModelAttr#model_attr.name, ModelAttr#model_attr.type} || ModelAttr <- ModelAttrList],
					TermVarcharChange = string:join([
		                                    atom_to_list(CName)++"=mysql_helper:string_to_term(Record#"++RECORD_NAME++"."++atom_to_list(CName)++")"
		                                    ||
		                                    {CName, CType}<-TypeArgList,
		                                                    (CType==term_varchar) or (CType==term_char)]
		                                    ++
		                                    [
		                                    atom_to_list(CName)++"=mysql_helper:string_to_atom(Record#"++RECORD_NAME++"."++atom_to_list(CName)++")"
		                                    ||
		                                    {CName, CType}<-TypeArgList,
		                                                    (CType==atom_varchar) or (CType==atom_char)],
		                                ",\r\n\t\t\t\t"),
					KVList =    [
								 	{"?ETSTABLENAME",           ETSTABLENAME},
		                            {"?PROTOPREFIX",            ?PROTOPREFIX},
		                            {"?RECORD_NAME",            RECORD_NAME},
		                            {"?ColumnList",             ColumnListString},
		                            {"?TABLE_TYPE",             TABLE_TYPE},
		                            {"?KEY_POS",     			KEY_POS},
		                            {"?COUNT_FIELD",     		COUNT_FIELD},
									{"?KEY_ARG_LIST",			KEY_ARG_LIST},
									{"?ETS_KEY_ARG",			case length(KeyList) of 1-> KEY_ARG_LIST;_->"{"++KEY_ARG_LIST++"}" end},
		                            {"?GET_CODE",       		GET_CODE},
									{"?SETORBAG_CODE",			SETORBAG_CODE},
		                            {"?TermVarcharChange",      TermVarcharChange},
									{"?INIT_FUN", 				INIT_FUN}
		                        ],
		
		            Code = val_replace(KVList, 'PROTO_SQL_IMPL_FORMAT'()),
		
					io:format(File, Code, []),
					file:close(File);
				{error, Reason} ->
					io:format("~n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nOpen ~s.erl Error,~n Reason:~p~n~n", [?PROTOPREFIX++RECORD_NAME, Reason])
			end;
		_ ->
			nothing
	end,
	io:format("Complete auto-coding ~s.erl.~n", [?PROTOPREFIX++RECORD_NAME]);
inner_create_model(#model_define{name=Name, attrs=ModelAttrList, primary_keys=PrimaryKeys}, _DBMod)->
	RECORD_NAME = atom_to_list(Name),
	case file:open(?MODEL_SRC_ROOT++?MODELPREFIX++RECORD_NAME++".erl", [write]) of
		{ok, File} ->
			TypeArgList = [{ModelAttr#model_attr.name, ModelAttr#model_attr.type} || ModelAttr <- ModelAttrList],
			ModelAttrNames = [NameString || {NameString, _} <- TypeArgList],
			ColumnListString = string:join(["`"++atom_to_list(Attr)++"`" || Attr <- ModelAttrNames], ","),
			MODULEARGSLIST = [string:to_upper(atom_to_list(Attr))||Attr<-ModelAttrNames],
			TABLEARGSSTRING = string:join(["`"++atom_to_list(ModelAttr#model_attr.name)++"`" || ModelAttr <- ModelAttrList], ", "),
			SQL_INSERT0 = pack_insert0(TABLEARGSSTRING, TypeArgList),
			SQL_DELETE0 = pack_delete0(PrimaryKeys, TypeArgList),
			SQL_UPDATE0 = pack_update0(PrimaryKeys, ModelAttrNames, TypeArgList),
			SQL_INSERTORUPDATE0 = pack_insert_or_update0(TABLEARGSSTRING, PrimaryKeys, ModelAttrNames, TypeArgList),
			SQL_SELECT0 = pack_select0(PrimaryKeys, TypeArgList),
			PRIMARYKEYS = string:to_upper(lists:flatten(io_lib:format("~w", [PrimaryKeys]))),
			PRIMARYKEY_ARGS = string:join([string:to_upper(atom_to_list(Key)) || Key <- PrimaryKeys], ", "),
			ColumnDefine = io_lib:format("~p", [TypeArgList]),
			TermVarcharChange = string:join([
                                    atom_to_list(CName)++"=mysql_helper:string_to_term(Record#"++RECORD_NAME++"."++atom_to_list(CName)++")"
                                    ||
                                    {CName, CType}<-TypeArgList,
                                                    (CType==term_varchar) or (CType==term_char)]
                                    ++
                                    [
                                    atom_to_list(CName)++"=mysql_helper:string_to_atom(Record#"++RECORD_NAME++"."++atom_to_list(CName)++")"
                                    ||
                                    {CName, CType}<-TypeArgList,
                                                    (CType==atom_varchar) or (CType==atom_char)],
                                ",\r\n\t\t\t\t"),
			THIS = "{"++RECORD_NAME++", "++string:join(MODULEARGSLIST, ", ") ++ "}",
            ModelColumnsPlace = string:join(["column_place("++atom_to_list(ColumnName)++")->\r\n\t#"++RECORD_NAME++"."++atom_to_list(ColumnName)||ColumnName<-ModelAttrNames], ";\r\n")++".\r\n",
            GET_ATTR_FUNS=string:join(["get_"++atom_to_list(Attr)++"(Record)->\r\n\t"++
                                            "Record#"++RECORD_NAME++"."++atom_to_list(Attr)++".\r\n"||Attr<-ModelAttrNames], "\r\n"),
            ValuesOfInsertSqlString = pack_values_of_insert0(TypeArgList),
			KVList =    [
                            {"?MODELPREFIX",            ?MODELPREFIX},
                            {"?RECORD_NAME",            RECORD_NAME},
                            {"?ColumnList",             ColumnListString},
                            {"?THIS",                   THIS},
                            {"?SQL_INSERT0",            SQL_INSERT0},
                            {"?SQL_DELETE0",            SQL_DELETE0},
                            {"?SQL_UPDATE0",            SQL_UPDATE0},
                            {"?SQL_INSERTORUPDATE0",    SQL_INSERTORUPDATE0},
                            {"?SQL_SELECT0",            SQL_SELECT0},
                            {"?PRIMARYKEYS",            PRIMARYKEYS},
							{"?PRIMARYKEY_ARGS",		PRIMARYKEY_ARGS},
                            {"?ColumnDefine",           ColumnDefine},
                            {"?TermVarcharChange",      TermVarcharChange},
                            {"?ModelColumnsPlace",      ModelColumnsPlace},
                            {"?GET_ATTR_FUNS",          GET_ATTR_FUNS},
							{"?TABLEARGS_STRING",		TABLEARGSSTRING},
                            {"?SQL_TYPE_INSERT",        "INSERT INTO"},
                            {"?SQL_TYPE_UPDATE",        "REPLACE INTO"},
							{"?ValuesOfInsertSqlString", ValuesOfInsertSqlString}
                        ],

            Code = val_replace(KVList, xmerl_ucs:to_utf8('MODULE_IMPL_FORMAT'())),

			io:format(File, Code, []),
			file:close(File);
		{error, Reason} ->
			io:format("~n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nOpen ~s.erl Error,~n Reason:~p~n~n", [?MODELPREFIX++RECORD_NAME, Reason])
	end,
	io:format("Complete auto-coding ~s.erl.~n", [?MODELPREFIX++RECORD_NAME]);
inner_create_model(Arg, _DBMod)->
	io:format("~n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nConfig File Error,~n Reason:~p~n~n", [Arg]).

pack_insert0(TABLEARGSSTRING, TypeArgList)->
	"("++TABLEARGSSTRING++") VALUES " ++ pack_values_of_insert0(TypeArgList) ++ ";".
	
pack_values_of_insert0(TypeArgList)->
	Values = string:join(["\"++mysql_helper:pack_value_by_type("++value_format(Value)++")++\"" || Value <- TypeArgList], ", "),
	"("++Values++")".
	
pack_delete0(PrimaryKeys, TypeArgList)->
	Conditions=[{Key, '=', proplists:lookup(Key, TypeArgList)}||Key<-PrimaryKeys],
	pack_where(Conditions).

pack_update0(PrimaryKeys, ModelAttrNames, TypeArgList)->
	Conditions=[{Key, '=', proplists:lookup(Key, TypeArgList)}||Key<-PrimaryKeys],
	Columns=[{Key, '=', proplists:lookup(Key, TypeArgList)}||Key<-(ModelAttrNames--PrimaryKeys)],
	pack_update_columns(Columns)++pack_where(Conditions).

pack_insert_or_update0(TABLEARGSSTRING, PrimaryKeys, ModelAttrNames, TypeArgList)->
	Columns=[{Key, '=', proplists:lookup(Key, TypeArgList)}||Key<-(ModelAttrNames--PrimaryKeys)],
	KV = pack_kv(Columns, []),
	Values = string:join(["\"++mysql_helper:pack_value_by_type("++value_format(Value)++")++\"" || Value <- TypeArgList], ", "),
	"("++TABLEARGSSTRING++") VALUES("++Values++") ON DUPLICATE KEY UPDATE "++string:join(KV, ", ").

pack_select0(PrimaryKeys, TypeArgList)->
	Conditions=[{Key, '=', proplists:lookup(Key, TypeArgList)}||Key<-PrimaryKeys],
	pack_where(Conditions).

pack_update_columns(Columns)->
	KV = pack_kv(Columns, []),
	case length(KV) of
		0 ->
			"";
		_Any ->
			" SET "++string:join(KV, ", ")
	end.

pack_where(Conditions)->
	KV = pack_kv(Conditions, []),
	case length(KV) of
		0 ->
			"";
		_Any ->
			" WHERE "++string:join(KV, " AND ")
	end.

pack_kv([], Sql)->
	Sql;
pack_kv([{ColumeName, '=', Value}|Tail], Sql)->
	New = "`"++atom_to_list(ColumeName)++"`"++"="++"\"++mysql_helper:pack_value_by_type("++value_format(Value)++")++\"",
	pack_kv(Tail, [New|Sql]);
pack_kv([{ColumeName, '!=', Value}|Tail], Sql)->
	New = "`"++atom_to_list(ColumeName)++"`"++"!="++"\"++mysql_helper:pack_value_by_type("++value_format(Value)++")++\"",
	pack_kv(Tail, [New|Sql]);
pack_kv([{ColumeName, '>', Value}|Tail], Sql) ->
	New = "`"++atom_to_list(ColumeName)++"`"++">"++"\"++mysql_helper:pack_value_by_type("++value_format(Value)++")++\"",
	pack_kv(Tail, [New|Sql]);
pack_kv([{ColumeName, '<', Value}|Tail], Sql) ->
	New = "`"++atom_to_list(ColumeName)++"`"++"<"++"\"++mysql_helper:pack_value_by_type("++value_format(Value)++")++\"",
	pack_kv(Tail, [New|Sql]);
pack_kv([{ColumeName, '>=', Value}|Tail], Sql) ->
	New = "`"++atom_to_list(ColumeName)++"`"++">="++"\"++mysql_helper:pack_value_by_type("++value_format(Value)++")++\"",
	pack_kv(Tail, [New|Sql]);
pack_kv([{ColumeName, '<=', Value}|Tail], Sql) ->
	New = "`"++atom_to_list(ColumeName)++"`"++"<="++"\"++mysql_helper:pack_value_by_type("++value_format(Value)++")++\"",
	pack_kv(Tail, [New|Sql]);
pack_kv([{ColumeName, in, Value}|Tail], Sql) when is_list(Value) ->
	New = "`"++atom_to_list(ColumeName)++"`"++" IN "++"\"++mysql_helper:pack_set("++value_format(Value)++")++\"",
	pack_kv(Tail, [New|Sql]);
pack_kv([{ColumeName, not_in, Value}|Tail], Sql) when is_list(Value) ->
	New = "`"++atom_to_list(ColumeName)++"`"++" NOT IN "++"\"++mysql_helper:pack_set("++value_format(Value)++")++\"",
	pack_kv(Tail, [New|Sql]);
pack_kv([{ColumeName, like, Value}|Tail], Sql) ->
    New = "`"++atom_to_list(ColumeName)++"`"++" LIKE "++"\"++mysql_helper:pack_value_by_type("++value_format(Value)++")++\"",
    pack_kv(Tail, [New|Sql]).

val_replace(KVList, Code)->
    string_replace(KVList, Code).

string_replace([], Code)->
    %io:format("~s~n", [Code]),
    Code;
string_replace([{K, V}|List], Code)->
    CodeBins = binary:split(list_to_binary(Code), list_to_binary(K), [global]),
    string_replace(List, string:join([binary_to_list(X) || X <- CodeBins], V)).

value_format({Value, Type})->
    "{"++string:to_upper(atom_to_list(Value))++", "++atom_to_list(Type)++"}".

term_to_string(Term)->
	lists:flatten(io_lib:format("~w", [Term])).



'PROTO_IMPL_FORMAT'()->
"-module(?PROTOPREFIX?RECORD_NAME).
-include(\"model_def.hrl\").
-include(\"ets_file.hrl\").
-define(?ETSTABLENAME, ets_?RECORD_NAME).

-compile(export_all).
-export([create/0, init/0]).
-behaviour(ets_operater_mod).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% behavior ets_operater_mod callback functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create()->
	?NEW_ETS(??ETSTABLENAME, [named_table, ?TABLE_TYPE]).

init()->
	?INIT_FUN

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% record operation functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(?KEY_ARG_LIST)->
	case ets:lookup(?GET_ENABLE_TABLE(??ETSTABLENAME),?ETS_KEY_ARG) of
		[]->[];
		?SETORBAG_CODE
	end.

foldl(Fun, Acc)->
	ets:foldl(Fun, Acc, ?GET_ENABLE_TABLE(??ETSTABLENAME)).

all()->
	ets:tab2list(?GET_ENABLE_TABLE(??ETSTABLENAME)).
	
?GET_CODE

".

'PROTO_SQL_IMPL_FORMAT'()->
"-module(?PROTOPREFIX?RECORD_NAME).
-include(\"model_def.hrl\").
-include(\"ets_file.hrl\").
-define(?ETSTABLENAME, ets_?RECORD_NAME).

-compile(export_all).
-export([create/0, init/0]).
-behaviour(ets_operater_mod).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% behavior ets_operater_mod callback functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create()->
	?NEW_ETS(??ETSTABLENAME, [named_table, ?TABLE_TYPE]).

init()->
	?INIT_FUN

select_all() ->
	AllCount = mysql_client:select_count(?RECORD_NAME, ?COUNT_FIELD),
	Columns = \"?ColumnList\",
	PageSize = ?SELECT_PAGE_SIZE,
	if
		AllCount > PageSize ->
			Pages = erlang:max(1, math_util:even_div(AllCount, PageSize)),
			lists:foldl(fun(Page, AccTmp) ->
								Where =  mysql_helper:pack_orderby({?COUNT_FIELD, asc}) ++ mysql_helper:pack_limit({(Page-1) * PageSize, PageSize}) ++ \";\",
								case mysql_client:select_columns(?RECORD_NAME, Columns, Where) of
									{ok, RowList}->
										AccTmp ++ unpack_row(RowList, []);
									_ ->
										AccTmp
								end
						end, [], lists:seq(1, Pages));
		true ->
			case mysql_client:select_columns(?RECORD_NAME, Columns, \";\") of
				{ok, RowList}->
					unpack_row(RowList, []);
				_ ->
					[]
			end
	end.

unpack_row([], Records)->
    lists:reverse(Records);
unpack_row([Row|Left], Records)->
    Record = mysql_helper:unpack_row(?RECORD_NAME, Row),
    unpack_row(Left,
				[Record#?RECORD_NAME{
					?TermVarcharChange
	    		}|Records]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% record operation functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(?KEY_ARG_LIST)->
	case ets:lookup(?GET_ENABLE_TABLE(??ETSTABLENAME),?ETS_KEY_ARG) of
		[]->[];
		?SETORBAG_CODE
	end.

foldl(Fun, Acc)->
	ets:foldl(Fun, Acc, ?GET_ENABLE_TABLE(??ETSTABLENAME)).

ms_select(MatchSpec) ->
	ets:select(?GET_ENABLE_TABLE(??ETSTABLENAME), MatchSpec).

all()->
	ets:tab2list(?GET_ENABLE_TABLE(??ETSTABLENAME)).

?GET_CODE

".

'MODULE_IMPL_FORMAT'()->
"-module(?MODELPREFIX?RECORD_NAME).
-include(\"model_def.hrl\").
-compile(export_all).

%% @doc		查询操作
%% @args	FieldList = [id,name]   要查询的字段
%%			Conditions = [{power, '>', 5000}, {level, '>', 65}]   查询条件
%% @return	{ok, AllRows} | error
select(FieldList, Conditions)->
    FormatCond = where_condition_format(Conditions),
	Columns=string:join([atom_to_list(Key) || Key<-FieldList], \",\"),
	Where = mysql_helper:pack_where(FormatCond),
	mysql_client:select_columns(?RECORD_NAME, Columns, Where).

%% @doc		插入操作
%% @args	Record | RecordList   要插入的表结构
%% @return	{ok, AffectedNum, InsertId} | error
insert(?THIS)->
	mysql_client:insert(?RECORD_NAME, \"?SQL_INSERT0\");
insert([])->
	error;
insert(InsertList) when is_list(InsertList)->
	ValueSql = pack_bash_insert(InsertList),
	mysql_client:insert(?RECORD_NAME, ValueSql).

%% @doc		更新操作
%% @args	Record | RecordList   要更新的表结构
%% @return	{ok, AffectedNum} | error
update(?THIS)->
	mysql_client:update(?RECORD_NAME, \"?SQL_UPDATE0\");
update([])->
	error;
update(UpdateList) when is_list(UpdateList)->
	ValueSql = pack_bash_insert(UpdateList),
	mysql_client:update_list(?RECORD_NAME, ValueSql).

%% @doc		插入或更新操作
%% @args	Record | RecordList   要更新的表结构
%% @return	{ok, AffectedNum, InsertId} | error
insert_or_update(?THIS)->
	mysql_client:insert(?RECORD_NAME, \"?SQL_INSERTORUPDATE0\").

%% @doc		更新操作
%% @args	FieldValueList = [{id, Value}]   要更新的字段
%%			Conditions = [{power, '>', 5000}, {level, '>', 65}]   更新条件
%% @return	{ok, AffectedNum} | error
update_fields(FieldValueList, Conditions)->
    FormatCond = where_condition_format(Conditions),
	Columns=[{Key, '=', {Value, get_column_datatype(Key)}}||{Key, Value}<-FieldValueList],
	SQL = mysql_helper:pack_update_columns(Columns)++mysql_helper:pack_where(FormatCond),
	mysql_client:update(?RECORD_NAME, SQL).

%% @doc		删除操作
%% @args	Record  数据表结构
%%		or	Conditions = [{power, '>', 5000}, {level, '>', 65}]   删除条件
%% @return	{ok, AffectedNum} | error
delete(?THIS) when is_tuple(?THIS)->
    mysql_client:delete(?RECORD_NAME, \"?SQL_DELETE0\");
delete([])->
	error;
delete(Conditions) when is_list(Conditions)->
    FormatCond = where_condition_format(Conditions),
	SQL = mysql_helper:pack_where(FormatCond),
    mysql_client:delete(?RECORD_NAME, SQL).

read_by_record(?THIS=Record) when is_record(Record, ?RECORD_NAME)->
	case mysql_client:select(?RECORD_NAME, \"?SQL_SELECT0\") of
		{ok,[]}->
			[];
		{ok,[RowList]}->
            unpack_rows([RowList])
	end.

%% @doc		读取操作
%% @args	Key  主键
%% @return	AllRows | []
read(?PRIMARYKEY_ARGS)->
	case mysql_client:select(?RECORD_NAME, \"?SQL_SELECT0\") of
		{ok,RowList}->
            unpack_rows(RowList);
		_ ->
			[]
	end.

%% @doc		插入或更新操作
%% @args	Record 要更新的表结构
%% @return	{ok, AffectedNum, InsertId} | {ok, AffectedNum} | error
write(?THIS)->
    case read_by_record(?THIS) of
		[] ->
			insert(?THIS);
		_Any ->
			update(?THIS)
	end.

%% @doc		查询操作
%% @args	Conditions = [{power, '>', 5000}, {level, '>', 65}]   查询条件
%% @return	AllRows | []
find(Conditions)->
	find([], Conditions, undefined, undefined).

%% @doc		查询操作
%% @args	FieldList = [id,name] | []  要查询的字段
%%			Conditions = [{power, '>', 5000}, {level, '>', 65}]   查询条件
%%			Limit = {0,100} | {5, 20},
%%			OrderBy = {hpmax, asc} | {hpmax, desc},
%% @return	AllRows | []
find(Conditions, Limit, OrderBy)->
	find([], Conditions, Limit, OrderBy).
find(FieldList, Conditions, Limit, OrderBy)->
	FormatCond = where_condition_format(Conditions),
	Where = mysql_helper:pack_where(FormatCond)
			++mysql_helper:pack_orderby(OrderBy)
			++mysql_helper:pack_limit(Limit),
	case FieldList of
		[] ->
			case mysql_client:select(?RECORD_NAME, Where) of
				{ok,RowList}->unpack_rows(RowList);
				_->[]
			end;
		_ ->
			Columns=string:join([atom_to_list(Key) || Key<-FieldList], \",\"),
			case mysql_client:select_columns(?RECORD_NAME, Columns, Where) of
				{ok,RowList}->RowList;
				_->[]
			end
	end.

%% @doc		查询操作
%% @return	AllRows | []
all()->
	case mysql_client:select(?RECORD_NAME, \";\") of
		{ok,RowList}->unpack_rows(RowList);
		_->[]
	end.

where_condition_format(Conditions)->
    where_condition_format(Conditions, []).

where_condition_format([], Ret)->
	Ret;
where_condition_format([{Column, Con, Val}|Conditions], Ret)->
    where_condition_format(Conditions,[{Column, Con, {Val, get_column_datatype(Column)}} | Ret]).

get_column_datatype(Column)->
    proplists:get_value(Column, column_datatypes()).

column_datatypes()->
    ?ColumnDefine.

?ModelColumnsPlace

unpack_rows(RowList)->
    unpack_row(RowList, []).

unpack_row([], Records)->
    lists:reverse(Records);
unpack_row([Row|Left], Records)->
    Record = mysql_helper:unpack_row(?RECORD_NAME, Row),
    unpack_row(Left,
				[Record#?RECORD_NAME{
					?TermVarcharChange
	    		}|Records]).

?GET_ATTR_FUNS

get_bash_insert_value_list(?THIS) ->
	\"?ValuesOfInsertSqlString\".

pack_bash_insert(InsertList) ->
	SqlValueListString = 
		lists:foldl(fun(InsertRecord, ValueList) ->
							case ValueList =:= \"\" of
								true ->
									[get_bash_insert_value_list(InsertRecord)];
								false ->
									[get_bash_insert_value_list(InsertRecord) | ValueList]
							end
					end, [], lists:reverse(InsertList)),
	ValueString = string:join(SqlValueListString, \",\"),
	\"(?TABLEARGS_STRING) VALUES\" ++ ValueString ++ \";\".
".

