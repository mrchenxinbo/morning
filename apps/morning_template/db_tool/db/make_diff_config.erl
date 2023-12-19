-module(make_diff_config).

-define(VERSION_MODEL_CONFIG_ROOT, "config_release/").

-define(MODEL_CONFIG_PREFIX, "mdf_").

-define(SCHEMA_FILE_ROOT, "schema/").

-define(ALTER_ADD_ATTR_AFTER,"ALTER TABLE `~s` ADD ~s AFTER `~s`;~n").

-define(ALTER_ADD_ATTR_FIRST,"ALTER TABLE `~s` ADD ~s FIRST;~n").

-define(ALTER_DEL_ATTR,"ALTER TABLE `~s` DROP `~s`;~n").

-define(ALTER_CHG_ATTR,"ALTER TABLE `~s` CHANGE `~s` ~s;~n").

-define(ALTER_TABLE_DEL_INDEX,"ALTER TABLE `~s` DROP INDEX `~s`;~n").

-define(ALTER_TABLE_ADD_INDEX,"ALTER TABLE `~s` ~s;~n").

-define(ALTER_TABLE_CHANGE_ENGINE,"ALTER TABLE `~s` ENGINE=~s;~n").

-define(ALTER_TABLE_CHANGE_COMMENT,"ALTER TABLE `~s` COMMENT='~s';~n").

-define(ALTER_TABLE_DROP_PK,"ALTER TABLE `~s` DROP PRIMARY KEY;~n").

-define(ALTER_TABLE_ADD_PK,"ALTER TABLE `~s` ADD PRIMARY KEY (~s);~n").

-define(ALTER_TABLE,
"
-- ----------------------------
-- ALTER TABLE `~s`
-- ----------------------------
").

-define(SCHEMA_FORMAT_DEL,
"
-- ----------------------------
-- DELETE TABLE `~s`
-- ----------------------------
DROP TABLE IF EXISTS `~s`;
").

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

-define(NOT_FOUND_PRIMARY_KEY, not_found_primary_key).
-define(NOT_FOUND_INDEX, not_found_index).

-include("model_define.hrl").

-export([run/0]).

run() ->
	io:format("Start auto-coding model defines.~n"),
	DbPrefix = get_argument('pre'),
	OldVersionArg = get_argument('old'),
	NewVersionArg = get_argument('new'),
	case OldVersionArg =:= [] orelse NewVersionArg =:= [] of
		true ->
			ReleaseVersionStrings = [V||"config_release/release_"++V<-filelib:wildcard("config_release/release_*")],
			ReleaseVersions = [list_to_integer(V)||V<-ReleaseVersionStrings],
			LastVersion = lists:max(ReleaseVersions),
			OldVersion = integer_to_list(LastVersion-1),
			NewVersion = integer_to_list(LastVersion);
		_ ->
			OldVersion = OldVersionArg,
			NewVersion = NewVersionArg
	end,
	io:format("~nOldVersion=~p, NewVersion=~p.~n", [OldVersion, NewVersion]),
	DiscUpdateFile = ?SCHEMA_FILE_ROOT++"update_"++OldVersion++"_"++NewVersion++".sql",
	create_diff_files(OldVersion, NewVersion, DiscUpdateFile, ?TABLE_TYPE_DISC, DbPrefix),
	ProtoUpdateFile = ?SCHEMA_FILE_ROOT++"proto_"++OldVersion++"_"++NewVersion++".sql",
	create_diff_files(OldVersion, NewVersion, ProtoUpdateFile, ?TABLE_TYPE_PROTO, DbPrefix),
	LogUpdateFile = ?SCHEMA_FILE_ROOT++"log_"++OldVersion++"_"++NewVersion++".sql",
	create_diff_files(OldVersion, NewVersion, LogUpdateFile, ?TABLE_TYPE_LOG, DbPrefix),
	io:format("Complete auto-coding model defines.~n"),
	init:stop().

get_argument(Input) ->
	case init:get_argument(Input) of
		error-> [];
		{ok, [[ArgString | _]]}-> 
			ArgString
	end.

create_diff_files(OldVersion, NewVersion, UpdateFile, TableType, DbPrefix)->
	try
		OldModels = get_all_models(OldVersion, TableType),
		NewModels = get_all_models(NewVersion, TableType),
		OldModelNames = get_model_names(OldModels),
		NewModelNames = get_model_names(NewModels),
		
		AddModels = NewModelNames--OldModelNames,
		DelModels = OldModelNames--NewModelNames,
		ChangedModels = get_model_names(NewModels -- OldModels) -- AddModels,
		
		case file:open(UpdateFile, [write]) of
			{ok, File} ->
				add_models(File, AddModels, NewVersion, TableType, DbPrefix),
				del_models(File, DelModels, OldVersion, TableType, DbPrefix),
				change_models(File, ChangedModels, OldVersion, NewVersion, TableType, DbPrefix),
				file:close(File),
				case file:read_file(UpdateFile) of
					{ok,<<>>} ->
						file:delete(UpdateFile);
					_ ->
						nothing
				end;
			{error, Reason} ->
				io:format("~n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nOpen ~p Error,~n Reason:~p~n~n", [UpdateFile, Reason]),
				throw(Reason)
		end
	catch
		throw:{ErrorType, Name, NewVersion} ->
			do_exit(ErrorType, Name, NewVersion);
		_:R ->
			io:format("~n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-ERRO;~p ~p~n~n", [R, erlang:get_stacktrace()]),
			del_dir(NewVersion),
			file:delete(UpdateFile)
	end.

add_models(File, AddModels, NewVersion, TableType, DbPrefix)->
	Models = get_models(AddModels, NewVersion, TableType),
	lists:foreach(
	  fun(#model_define{name=Name,
						info=Info,
						type=Type,
						attrs=ModelAttrList,
						primary_keys=PrimaryKeys,
						indexs = Indexs,
						engine = Engine
					   })->
			  AttrNames = get_attr_names(ModelAttrList),
			  IsPKExist = is_in_attrnames(PrimaryKeys, AttrNames),
			  IsInExist = is_in_attrnames(Indexs, AttrNames),
			  if
				  not IsPKExist ->
					  throw({?NOT_FOUND_PRIMARY_KEY, Name, NewVersion});
				  not IsInExist ->
					  throw({?NOT_FOUND_INDEX, Name, NewVersion});
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
			  case {PrimarySQL, IndexSQL} of
				  {[], []} ->
					  io:format(File, ?SCHEMA_FORMAT_WITHOUT_PK_INDEX, [MODULENAME, MODULENAME, MODULENAME, ColumnsSQL, EngineSql, xmerl_ucs:to_utf8(Info)]);
				  {[], I} when I =/= [] ->
					  io:format(File, ?SCHEMA_FORMAT_WITHOUT_PK, [MODULENAME, MODULENAME, MODULENAME, ColumnsSQL, IndexSQL, EngineSql, xmerl_ucs:to_utf8(Info)]);
				  _ ->
					  io:format(File, ?SCHEMA_FORMAT, [MODULENAME, MODULENAME, MODULENAME, ColumnsSQL, PrimarySQL, IndexSQL, EngineSql, xmerl_ucs:to_utf8(Info)])
			  end
	  end, Models).

del_models(File, DelModels, OldVersion, TableType, DbPrefix)->
	Models = get_models(DelModels, OldVersion, TableType),
	lists:foreach(
	  fun(#model_define{name=Name,type=Type}) ->
			  MODULENAME = case Type of
					 ?TABLE_TYPE_LOG -> DbPrefix ++ "log_" ++ atom_to_list(Name);
					 _ -> DbPrefix ++ atom_to_list(Name)
				 end,
			  io:format(File, ?SCHEMA_FORMAT_DEL, [MODULENAME, MODULENAME])
	  end, Models).

change_models(File, ChangedModels, OldVersion, NewVersion, TableType, DbPrefix)->
	OldModels = get_models(ChangedModels, OldVersion, TableType),
	NewModels = get_models(ChangedModels, NewVersion, TableType),
	ModelDiffs = lists:map(
				   fun(#model_define{name=Name,
									 info=NewInfo,
									 type=Type,
									 attrs=Attrs,
									 primary_keys=PK,
									 indexs=Indexs,
									 engine=NewEngine})->
						   #model_define{attrs=OldAttrs,
										 info=OldInfo,
										 primary_keys=OldPK,
										 indexs=OldIndexs,
										 engine=OldEngine} = lists:keyfind(Name, #model_define.name, OldModels),
						   NewAttrNames = get_attr_names(Attrs),
						   OldAttrNames = get_attr_names(OldAttrs),
						   IsPKExist = is_in_attrnames(PK, NewAttrNames),
						   IsInExist = is_in_attrnames(Indexs, NewAttrNames),
						   if
							   not IsPKExist ->
								   throw({?NOT_FOUND_PRIMARY_KEY, Name, NewVersion});
							   not IsInExist ->
								   throw({?NOT_FOUND_INDEX, Name, NewVersion});
							   true ->
								   nothing
						   end,
						   AddAttrs = NewAttrNames--OldAttrNames,
						   AddAforeAttrs = get_afore_attr_name(AddAttrs, NewAttrNames, []),
						   AddPK = if PK == OldPK ->
										  {false, [], OldPK};
									  true ->
										  {true, PK, OldPK}
								   end,
						   DelIndexs = OldIndexs -- Indexs,
						   DelAttrs = OldAttrNames--NewAttrNames,
						   IsDelTable = if DelAttrs == OldAttrNames ->
											   NewAttrs = Attrs,
											   true;
										   true ->
											   NewAttrs = [],
											   false
										end,
						   AddIndexs = Indexs -- OldIndexs,
						   Engine = case OldEngine of
										NewEngine ->
											undefined;
										_ ->
											NewEngine
									end,
						   Info = case OldInfo of
									  NewInfo ->
										  OldInfo;
									  _ ->
										  NewInfo
								  end,
						   #model_diff{name=Name,
									   info=Info,
									   type=Type,
									   add_attrs=AddAttrs,
									   add_afore_attrs=AddAforeAttrs,
									   del_attrs=OldAttrNames--NewAttrNames,
									   del_table=IsDelTable,
									   new_attrs=NewAttrs,
									   chg_attrs=get_attr_names(Attrs--OldAttrs)--AddAttrs,
									   new_primary_keys=AddPK,
									   del_indexs=DelIndexs,
									   add_indexs=AddIndexs,
									   engine=Engine}
				   end, NewModels),
	do_change(File, ModelDiffs, OldModels, NewModels, DbPrefix).

do_change(File, ModelDiffs, _OldModels, NewModels, DbPrefix)->
	lists:foreach(
	  fun(#model_diff{name=Name,
					  info=Info,
					  type=Type,
					  add_attrs=AddAttrs,
					  add_afore_attrs=AddAforeAttrs,
					  del_attrs=DelAttrs,
					  del_table=DelTable,
					  chg_attrs=ChgAttrs,
					  new_primary_keys=NewPKTuple,
					  del_indexs=DelIndexs,
					  add_indexs=AddIndexs,
					  engine=Engine}) when DelTable =/= true ->
			  MODULENAME = case Type of
					 ?TABLE_TYPE_LOG -> DbPrefix ++ "log_" ++ atom_to_list(Name);
					 _ -> DbPrefix ++ atom_to_list(Name)
				 end,
			  io:format(File, ?ALTER_TABLE, [MODULENAME]),
			  lists:foreach(
				fun(Index)->
						io:format(File, ?ALTER_TABLE_DEL_INDEX, [MODULENAME, model_schema:get_index_name(Index)])
				end, DelIndexs),
			  case NewPKTuple of
				  {true, NewPK, OldPK} ->
					  IsNeedDropOldPK = is_old_pk_in_del_attr(OldPK, DelAttrs),
					  if
						  not IsNeedDropOldPK ->
							  io:format(File, ?ALTER_TABLE_DROP_PK, [MODULENAME]);
						  true ->
							  nothing
					  end;
				  {_, NewPK, OldPK} ->
					  nothing
			  end,
			  lists:foreach(
				fun(AttrName)->
						io:format(File, ?ALTER_DEL_ATTR, [MODULENAME, AttrName])
				end, DelAttrs),
			  {IsAutoInc, _} = lists:foldl(
								 fun(AttrName, {AccInc, AccIndex})->
										 case lists:nth(AccIndex, AddAforeAttrs) of
											 {'$after', AttrAfore} ->
												 {IsAutoInc, AddColumn} = create_column_sql_diff(get_attr(Name, AttrName, NewModels), NewPK),
												 io:format(File, ?ALTER_ADD_ATTR_AFTER, [MODULENAME, AddColumn, AttrAfore]),
												 if AccInc ->
														{AccInc, AccIndex + 1};
													true ->
														{IsAutoInc, AccIndex + 1}
												 end;
											 {'$first', _} ->
												 {IsAutoInc, AddColumn} = create_column_sql_diff(get_attr(Name, AttrName, NewModels), NewPK),
												 io:format(File, ?ALTER_ADD_ATTR_FIRST, [MODULENAME, AddColumn]),
												 if AccInc ->
														{AccInc, AccIndex + 1};
													true ->
														{IsAutoInc, AccIndex + 1}
												 end
										 end
								 end, {false, 1}, AddAttrs),
			  case {NewPK, IsAutoInc} of
				  {[], _} ->
					  nothing;
				  {_, true} ->
					  nothing;
				  _ ->
					  io:format(File, ?ALTER_TABLE_ADD_PK, [MODULENAME, model_schema:create_primary(NewPK)])
			  end,
			  CurPk = case NewPK =:= [] of true -> OldPK; _ -> NewPK end,
			  lists:foreach(
				fun(AttrName)->
						io:format(File, ?ALTER_CHG_ATTR, [MODULENAME, AttrName, model_schema:create_column_sql(get_attr(Name, AttrName, NewModels), CurPk)])
				end, ChgAttrs),
			  lists:foreach(
				fun(Index)->
						io:format(File, ?ALTER_TABLE_ADD_INDEX, [MODULENAME, model_schema:add_index(Index)])
				end, AddIndexs),
			  case Engine of
				  undefined ->
					  nothing;
				  Engine ->
					  EngineStr = case Engine of
									  myisam ->
										  "MyISAM";
									  _ ->
										  "InnoDB"
								  end,
					  io:format(File, ?ALTER_TABLE_CHANGE_ENGINE, [MODULENAME, EngineStr])
			  end,
			  case Info of
				  undefined ->
					  nothing;
				  _ ->
					  io:format(File, ?ALTER_TABLE_CHANGE_COMMENT, [MODULENAME, xmerl_ucs:to_utf8(Info)])
			  end;
		 (#model_diff{name=Name,
					  info=Info,
					  type=Type,
					  del_table=DelTable,
					  new_attrs=ModelAttrList,
					  new_primary_keys=PrimaryKeys,
					  add_indexs=Indexs,
					  engine=Engine}) when DelTable ->
			  MODULENAME = case Type of
					 ?TABLE_TYPE_LOG -> DbPrefix ++ "log_" ++ atom_to_list(Name);
					 _ -> DbPrefix ++ atom_to_list(Name)
				 end,
			  case PrimaryKeys of
				  {true, NewPK, _OldPK} ->
					  nothing;
				  {_, NewPK, _OldPK} ->
					  nothing
			  end,
			  ColumnsSQL = model_schema:create_columns(ModelAttrList, NewPK),
			  PrimarySQL = model_schema:create_primary(NewPK),
			  IndexSQL = model_schema:create_index(Indexs),
			  EngineSql = case Engine of
							  myisam ->
								  "MyISAM";
							  _ ->
								  "InnoDB"
						  end,
			  io:format(File, ?SCHEMA_FORMAT, [MODULENAME, MODULENAME, MODULENAME, ColumnsSQL, PrimarySQL, IndexSQL, EngineSql, xmerl_ucs:to_utf8(Info)])
	  end, ModelDiffs).


get_models(ModelNames, Version, TableType)->
	lists:filter(
	  fun(#model_define{name=Name})->
			  lists:member(Name, ModelNames)
	  end, get_all_models(Version, TableType)).

get_all_models(Version, TableType)->
	ModelConfigFiles = filelib:wildcard(?VERSION_MODEL_CONFIG_ROOT ++ "release_" ++ Version ++ "/" ++ ?MODEL_CONFIG_PREFIX ++ "*.config"),
	AllModels = lists:foldl(fun(Item, Acc)->
									{ok, ModelsAndProtos} = file:consult(Item),
									ModelsAndProtos++Acc
							end, [], ModelConfigFiles),
	[Model||#model_define{type=Type} = Model <- AllModels, Type =:= TableType].

get_model_names(Models)->
	[Name || #model_define{name=Name} <- Models].

get_attr_names(ModelAttrs)->
	[AttrName||#model_attr{name=AttrName}<-ModelAttrs].

get_attr(ModelName, AttrName, Models)->
	Model = lists:keyfind(ModelName, #model_define.name, Models),
	lists:keyfind(AttrName, #model_attr.name, Model#model_define.attrs).

get_afore_attr_name([AddNames|Tail], AttrNames, Attrs) ->
	Nth = where_is(AddNames, AttrNames, 1),
	if
		Nth > 1 ->
			Afore = lists:nth(Nth - 1, AttrNames),
			get_afore_attr_name(Tail, AttrNames, [{'$after', Afore}|Attrs]);
		Nth =:= 1, length(AttrNames) > 1 ->
			get_afore_attr_name(Tail, AttrNames, [{'$first', AddNames}|Attrs]);
		true ->
			get_afore_attr_name(Tail, AttrNames, [{'$new', AddNames}|Attrs])
	end;
get_afore_attr_name([], _, Attrs) ->
	lists:reverse(Attrs).


where_is(C, [C|_], Index) ->
	Index;
where_is(C, [_|T], Index) ->
	where_is(C, T, Index + 1);
where_is(_, [], Index) ->
	Index.


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

is_old_pk_in_del_attr(OldPK, DelAttrNames) ->
	(OldPK -- DelAttrNames) =:= [].


do_exit(?NOT_FOUND_PRIMARY_KEY, Name, NewVersion) ->
	del_dir(NewVersion),
	exit(list_to_atom(atom_to_list(Name) ++ " primary_key_not_found"));
do_exit(?NOT_FOUND_INDEX, Name, NewVersion) ->
	del_dir(NewVersion),
	exit(list_to_atom(atom_to_list(Name) ++ " index_not_found"));
do_exit(_, _, _) ->
	nothing.

del_dir(Version) ->
	Dir = "config_release/release_" ++ Version,
	case filelib:is_dir(Dir) of
		true ->
			[file:delete(V) || V<-filelib:wildcard(Dir ++ "/*"), filelib:is_regular(V)],
			file:del_dir(Dir);
		_ ->
			nothing
	end.

create_column_sql_diff(#model_attr{name		=	Name,
								   type		=	Type,
								   length	=	Length,
								   float		=	Float,
								   cannull	=	Cannull,
								   default	=	Default,
								   autoinc	=	Autoinc,
								   info		=	Info}, PrimaryKeys)->
	case lists:member(auto, Autoinc) of
		true ->
			ColumnsSQL = string:join([
									  "`"++atom_to_list(Name)++"`",
									  model_schema:create_column_type(Type, Length, Float),
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
														 if Type == blob; Type==tinyblob ->"";
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
																				  model_schema:pack_value(Default) end;
																	   true->
																		   model_schema:pack_value(Default)
																	end
														 end
												 end;
										 true->"NULL " 
									  end,
									  "PRIMARY KEY AUTO_INCREMENT ",
									  %create_column_options(Options),
									  "COMMENT '"++ xmerl_ucs:to_utf8(Info) ++"'"
									 ], " "),
			{true, ColumnsSQL};
		false ->
			ColumnsSQL = string:join([
									  "`"++atom_to_list(Name)++"`",
									  model_schema:create_column_type(Type, Length, Float),
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
														 if Type == blob; Type==tinyblob ->"";
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
																				  model_schema:pack_value(Default) end;
																	   true->
																		   model_schema:pack_value(Default)
																	end
														 end
												 end;
										 true->"NULL" 
									  end,
									  "",
									  %create_column_options(Options),
									  "COMMENT '"++ xmerl_ucs:to_utf8(Info) ++"'"
									 ], " "),
			{false, ColumnsSQL}
	end.