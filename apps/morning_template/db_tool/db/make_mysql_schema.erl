-module(make_mysql_schema).

-define(DEFFILE, "schema/schema.sql").
-define(DEFPROTOFILE, "schema/proto_schema.sql").
-define(DEFLOGFILE, "schema/log_schema.sql").
-define(MODEL_CONFIG_ROOT, "config/").
-define(MODEL_CONFIG_PREFIX, "mdf_").
-include("model_define.hrl").

-define(SCHEMA_FORMAT,
"
SET FOREIGN_KEY_CHECKS=0;
-- ----------------------------
-- Table structure for `~s`
-- ----------------------------
DROP TABLE IF EXISTS `~s`;
CREATE TABLE `~s` (
  ~s,
  PRIMARY KEY (~s)~s
) ENGINE=~s DEFAULT CHARSET=utf8 COMMENT='~s';
").

-define(SCHEMA_FORMAT_WITHOUT_PK,
"
SET FOREIGN_KEY_CHECKS=0;
-- ----------------------------
-- Table structure for `~s`
-- ----------------------------
DROP TABLE IF EXISTS `~s`;
CREATE TABLE `~s` (
  ~s
  ~s
) ENGINE=~s DEFAULT CHARSET=utf8 COMMENT='~s';
").

-define(SCHEMA_FORMAT_WITHOUT_PK_INDEX,
"
SET FOREIGN_KEY_CHECKS=0;
-- ----------------------------
-- Table structure for `~s`
-- ----------------------------
DROP TABLE IF EXISTS `~s`;
CREATE TABLE `~s` (
  ~s
) ENGINE=~s DEFAULT CHARSET=utf8 COMMENT='~s';
").

-export([run/0]).

run()->
	io:format("Start auto-coding sql schema.~n"),
	DbPrefix = get_argument('pre'),
	create_model_files(DbPrefix),
	io:format("Complete auto-coding sql schema.~n"),
	init:stop().

create_model_files(DbPrefix)->
    ModelConfigFiles = filelib:wildcard(?MODEL_CONFIG_ROOT ++ ?MODEL_CONFIG_PREFIX ++ "*.config"),

    file:delete(?DEFFILE),
	file:delete(?DEFPROTOFILE),
	file:delete(?DEFLOGFILE),
	case file:open(?DEFFILE, [write]) of
		{ok, File} ->
			case file:open(?DEFPROTOFILE, [write]) of
				{ok, ProtoFile} ->
					case file:open(?DEFLOGFILE, [write]) of
						{ok, LogFile} ->
							lists:foreach(fun(Model)-> create_model(Model, File, LogFile, ProtoFile, DbPrefix) end, ModelConfigFiles),
							file:close(File),
							file:close(ProtoFile),
							file:close(LogFile);
						{error, Reason1} ->
							io:format("~n!!!!!!!!!!!!!!create_model_files!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nOpen schema.sql Error,~n Reason:~p~n~n", [Reason1])
					end
			end;
		{error, Reason} ->
			io:format("~n!!!!!!!!!!!!!!create_model_files!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nOpen schema.sql Error,~n Reason:~p~n~n", [Reason])
	end.

create_model(ModFile, File, LogFile, ProtoFile, DbPrefix) ->
	case file:consult(ModFile) of
        {ok, Models} ->
            lists:foreach(fun(Model)-> inner_create_model(Model, File, LogFile, ProtoFile, DbPrefix) end, Models);
		Error ->
			io:format("~n!!!!!!!!!!!!!!create_model!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nConfig File ~p,~n Reason:~p~n~n", [ModFile, Error])
	end.

inner_create_model(#proto_define{}, _File, _LogFile, _ProtoFile, _DbPrefix)->
	nothing;
inner_create_model(#callback_proto_define{}, _File, _LogFile, _ProtoFile, _DbPrefix)->
	nothing;
inner_create_model(#model_define{name=Name,
								 info=Info,
								 type=Type,
								 attrs=ModelAttrList,
								 primary_keys=PrimaryKeys,
								 indexs = Indexs,
								 engine = Engine,
								 extra = _Extra
								}, File, LogFile, ProtoFile, DbPrefix)->
	AttrNames = get_attr_names(ModelAttrList),
	IsPKExist = is_in_attrnames(PrimaryKeys, AttrNames),
	IsInExist = is_in_attrnames(Indexs, AttrNames),
	if
		not IsPKExist ->
			exit(list_to_atom(atom_to_list(Name) ++ " primary_key_not_found"));
		not IsInExist ->
			exit(list_to_atom(atom_to_list(Name) ++ " index_not_found"));
		true ->
			nothing
	end,
	MODULENAME = case Type of
					 ?TABLE_TYPE_LOG -> DbPrefix ++ "log_" ++ atom_to_list(Name);
					 _ -> DbPrefix ++ atom_to_list(Name)
				 end,
	ColumnsSQL = model_schema:create_columns(ModelAttrList, PrimaryKeys),
	PrimarySQL = model_schema:create_primary(PrimaryKeys),
	IndexSQL = model_schema:create_index(Indexs),
	EngineSql = case Engine of
					myisam ->
						"MyISAM";
					_ ->
						"InnoDB"
				end,
	WriteFile = case Type of
					?TABLE_TYPE_PROTO -> ProtoFile;
					?TABLE_TYPE_LOG -> LogFile;
					_ -> File
				end,
	case {PrimarySQL, IndexSQL} of
		{[], []} ->
			io:format(WriteFile, ?SCHEMA_FORMAT_WITHOUT_PK_INDEX, [MODULENAME, MODULENAME, MODULENAME, ColumnsSQL, EngineSql, xmerl_ucs:to_utf8(Info)]);
		{[], I} when I =/= [] ->
			io:format(WriteFile, ?SCHEMA_FORMAT_WITHOUT_PK, [MODULENAME, MODULENAME, MODULENAME, ColumnsSQL, IndexSQL, EngineSql, xmerl_ucs:to_utf8(Info)]);
		_ ->
			io:format(WriteFile, ?SCHEMA_FORMAT, [MODULENAME, MODULENAME, MODULENAME, ColumnsSQL, PrimarySQL, IndexSQL, EngineSql, xmerl_ucs:to_utf8(Info)])
	end;
inner_create_model(Arg, _, _, _, _)->
	io:format("~n!!!!!!!!!!!!!!!!inner_create_model!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nConfig File Error,~n Reason:~p~n~n", [Arg]).

get_attr_names(ModelAttrs)->
	[AttrName||#model_attr{name=AttrName}<-ModelAttrs].

is_in_attrnames([], _) ->
	true;
is_in_attrnames([L1|Tail], L2) when erlang:is_list(L1) ->
	case is_in_attrnames(L1, L2) of
		true ->
			is_in_attrnames(Tail, L2);
		false ->
			false
	end;
is_in_attrnames([{_, L1}|Tail], L2) ->
	case lists:member(L1, L2) of
		true ->
			is_in_attrnames(Tail, L2);
		false ->
			false
	end;
is_in_attrnames([L1|Tail], L2) ->
	case lists:member(L1, L2) of
		true ->
			is_in_attrnames(Tail, L2);
		false ->
			false
	end.

get_argument(Input) ->
	case init:get_argument(Input) of
		error-> [];
		{ok, [[ArgString | _]]}-> 
			ArgString
	end.