-module(make_model_def).

-include("model_define.hrl").
-define(DEFFILE, "include/model_def.hrl").
-define(DEFLOGFILE, "include/model_log_def.hrl").
-define(INCLUDE_PATH, "include/").

-define(MODULE_DEF_FORMAT, "~s~n-record(~s, {\n\t\t\t~s\n\t\t}). %%@~s~n~n").
-define(MODEL_CONFIG_ROOT, "config/").
-define(MODEL_CONFIG_PREFIX, "mdf_").
-define(MODEL_DEF_FILE_PREFIX, "model_").

-export([run/0]).

run()->
	io:format("Start auto-coding model defines.~n"),
	create_model_files(),
	io:format("Complete auto-coding model defines.~n"),
	init:stop().

create_model_files()->
    ModelConfigFileNameList = filelib:wildcard(?MODEL_CONFIG_ROOT ++ ?MODEL_CONFIG_PREFIX ++ "*.config"),
	io:format("======~p~n", [ModelConfigFileNameList]),
	case file:open(?DEFFILE, [write]) of
		{ok, HrlFile} ->
			{ok, LogFile} =  file:open(?DEFLOGFILE, [write]),
			lists:foreach(fun(ModelConfigFileName)-> create_models(ModelConfigFileName, HrlFile, LogFile) end, ModelConfigFileNameList),
			file:close(HrlFile),
			file:close(LogFile);
		{error, Reason} ->
			io:format("~n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nOpen model_def.hrl Error,~n Reason:~p~n~n", [Reason])
	end.

create_models(ModelConfigFileName, HrlFile, LogFile) ->
 	HeaderLength = length(?MODEL_CONFIG_ROOT)+length(?MODEL_CONFIG_PREFIX),
    NameLength = length(ModelConfigFileName)-HeaderLength-length(".config"),
    ModHrlFileName = string:substr(ModelConfigFileName, HeaderLength+1, NameLength),
    HrlFilePathName = ?INCLUDE_PATH ++ ?MODEL_DEF_FILE_PREFIX ++ ModHrlFileName ++ ".hrl",
	case file:open(HrlFilePathName, [write]) of
		{ok, ModHrlFile} ->
			case file:consult(ModelConfigFileName) of
	            {ok, Models} ->
		            inner_create_models(Models, ModHrlFile, HrlFile, LogFile);
				Error ->
					io:format("~n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nConfig File ~p,~n Reason:~p~n~n", [ModelConfigFileName, Error])
			end,
			file:close(ModHrlFile);
		{error, Reason} ->
			io:format("~n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nOpen ~p Error,~n Reason:~p~n~n", [HrlFilePathName, Reason])
	end.

inner_create_models([], _ModHrlFile, _HrlFile, _LogFile) ->
    ok;
inner_create_models([#model_define{name=Name, info = Info, type = Type, attrs=ModelAttrList, primary_keys=PK, indexs=Indexs} | Tail ],
					ModHrlFile, HrlFile, LogFile) when Type =:= ?TABLE_TYPE_LOG ->
	[{LastAttr, LastInfo} | TmpAttrList] = 
		lists:map(fun(ModelAttr) ->
						  AttrName = ModelAttr#model_attr.name,
						  AttrAutoinc = ModelAttr#model_attr.autoinc,
						  TmpAttr = case lists:member(AttrName, PK) andalso lists:member(auto, AttrAutoinc) of
										true ->
											atom_to_list(ModelAttr#model_attr.name) ++ "=null";
										_ ->
											atom_to_list(ModelAttr#model_attr.name) ++ 
												case ModelAttr#model_attr.default of undefined->"";TheDefault->"="++io_lib:format("~w", [TheDefault]) end
									end,
						  TmpInfo = io_lib:format("~s", [xmerl_ucs:to_utf8(ModelAttr#model_attr.info)]),
						  {TmpAttr, TmpInfo}
				  end, lists:reverse(ModelAttrList)),
	LastAttrInfo = LastAttr ++ get_tab_str(LastAttr,0) ++ "%% " ++ LastInfo,
	NewTmpAttrList = [TmpAttr ++ "," ++ get_tab_str(TmpAttr, 1) ++ "%% " ++ TmpInfo || {TmpAttr, TmpInfo} <- TmpAttrList],
	DefAttrs = string:join(lists:reverse(NewTmpAttrList) ++ [LastAttrInfo], "\n\t\t\t"),
	EtsType = case Type of ?TABLE_TYPE_PROTO -> if length(PK)>1 -> "bag"; true -> "set" end; _ -> "set" end,
	Infos = "disc,"++ EtsType ++",key:"++io_lib:format("~w",[PK])++case Indexs of []->"";Indexs->",index:"++io_lib:format("~w",[[Ind||{_,Ind}<-Indexs]++[Ind||Ind<-Indexs,is_atom(Ind)]]) end,
	DesStr = "%% " ++ xmerl_ucs:to_utf8(Info),
	io:format(LogFile, ?MODULE_DEF_FORMAT, [DesStr, atom_to_list(Name), DefAttrs, Infos]),
	io:format(ModHrlFile, ?MODULE_DEF_FORMAT, [DesStr, atom_to_list(Name), DefAttrs, Infos]),
	inner_create_models(Tail, ModHrlFile, HrlFile, LogFile);
inner_create_models([#model_define{name=Name, info = Info, type = Type, attrs=ModelAttrList, primary_keys=PK, indexs=Indexs} | Tail ],
                    ModHrlFile, HrlFile, LogFile) ->
	List = 
		lists:foldl(fun(ModelAttr, ModelAcc) ->
						case ModelAttr#model_attr.flag of
							?MODEL_FLAG_SERVER ->
								AttrName = ModelAttr#model_attr.name,
								AttrAutoinc = ModelAttr#model_attr.autoinc,
								TmpAttr = case lists:member(AttrName, PK) andalso lists:member(auto, AttrAutoinc) of
											  true ->
												  atom_to_list(ModelAttr#model_attr.name) ++ "=null";
											  _ ->
												  atom_to_list(ModelAttr#model_attr.name) ++ 
													  case ModelAttr#model_attr.default of undefined->"";TheDefault->"="++io_lib:format("~w", [TheDefault]) end
										  end,
								TmpInfo = io_lib:format("~s", [xmerl_ucs:to_utf8(ModelAttr#model_attr.info)]),
								[{TmpAttr, TmpInfo} | ModelAcc];
							_ ->
								ModelAcc
						end
				end, [], ModelAttrList),
	case List of
		[{LastAttr, LastInfo} | TmpAttrList] ->
			LastAttrInfo = LastAttr ++ get_tab_str(LastAttr,0) ++ "%% " ++ LastInfo,
			NewTmpAttrList = [TmpAttr ++ "," ++ get_tab_str(TmpAttr, 1) ++ "%% " ++ TmpInfo || {TmpAttr, TmpInfo} <- TmpAttrList],
			DefAttrs = string:join(lists:reverse(NewTmpAttrList) ++ [LastAttrInfo], "\n\t\t\t"),
			EtsType = case Type of ?TABLE_TYPE_PROTO -> if length(PK)>1 -> "bag"; true -> "set" end; _ -> "set" end,
			Infos = atom_to_list(Type)++","++ EtsType ++",key:"++io_lib:format("~w",[PK])++case Indexs of []->"";Indexs->",index:"++io_lib:format("~w",[[Ind||{_,Ind}<-Indexs]++[Ind||Ind<-Indexs,is_atom(Ind)]]) end,
			DesStr = "%% " ++ xmerl_ucs:to_utf8(Info),
			io:format(HrlFile, ?MODULE_DEF_FORMAT, [DesStr, atom_to_list(Name), DefAttrs, Infos]),
			io:format(ModHrlFile, ?MODULE_DEF_FORMAT, [DesStr, atom_to_list(Name), DefAttrs, Infos]),
			inner_create_models(Tail, ModHrlFile, HrlFile, LogFile);
		_ ->
			inner_create_models(Tail, ModHrlFile, HrlFile, LogFile)
	end;
inner_create_models([#proto_define{name=Name, attrs=ModelAttrList, keys=KeyList, type=Type} | Tail ],
                    ModHrlFile, HrlFile, LogFile) ->
	{TmpAttrList, TmpAttrInfoList} = 
		lists:foldl(fun(ModelAttr, {ModelAcc, InfoAcc}) ->
						case ModelAttr#proto_attr.flag of
							?MODEL_FLAG_SERVER ->
								TmpAttr = atom_to_list(ModelAttr#proto_attr.name) ++ 
											  case ModelAttr#proto_attr.default of undefined->"";TheDefault->"="++io_lib:format("~w", [TheDefault]) end,
								TmpInfo = io_lib:format("~s", [xmerl_ucs:to_utf8(ModelAttr#proto_attr.info)]),
								{[TmpAttr | ModelAcc], [TmpInfo | InfoAcc]};
							_ ->
								{ModelAcc, InfoAcc}
						end
				end, {[], []}, ModelAttrList),
	DefAttrs = string:join(lists:reverse(TmpAttrList), ", "),
	Infos = "proto,"++atom_to_list(Type)++",key:"++io_lib:format("~w",[KeyList]),
	DesStr = "%% " ++ atom_to_list(Name) ++ " {" ++ string:join(lists:reverse(TmpAttrInfoList), ", ") ++ "}",
	io:format(HrlFile, ?MODULE_DEF_FORMAT, [DesStr, atom_to_list(Name), DefAttrs, Infos]),
	io:format(ModHrlFile, ?MODULE_DEF_FORMAT, [DesStr, atom_to_list(Name), DefAttrs, Infos]),
	inner_create_models(Tail, ModHrlFile, HrlFile, LogFile);
inner_create_models([#callback_proto_define{name=Name, attrs=ModelAttrList, keys=KeyList, type=Type} | Tail ],
                    ModHrlFile, HrlFile, LogFile) ->
	{TmpAttrList, TmpAttrInfoList} = 
		lists:foldl(fun(ModelAttr, {ModelAcc, InfoAcc}) ->
						case ModelAttr#proto_attr.flag of
							?MODEL_FLAG_SERVER ->
								TmpAttr = atom_to_list(ModelAttr#proto_attr.name) ++ 
											  case ModelAttr#proto_attr.default of undefined->"";TheDefault->"="++io_lib:format("~w", [TheDefault]) end,
								TmpInfo = io_lib:format("~s", [xmerl_ucs:to_utf8(ModelAttr#proto_attr.info)]),
								{[TmpAttr | ModelAcc], [TmpInfo | InfoAcc]};
							_ ->
								{ModelAcc, InfoAcc}
						end
				end, {[], []}, ModelAttrList),
	DefAttrs = string:join(lists:reverse(TmpAttrList), ", "),
	Infos = "proto,"++atom_to_list(Type)++",key:"++io_lib:format("~w",[KeyList]),
	DesStr = "%% " ++ atom_to_list(Name) ++ " {" ++ string:join(lists:reverse(TmpAttrInfoList), ", ") ++ "}",
	io:format(HrlFile, ?MODULE_DEF_FORMAT, [DesStr, atom_to_list(Name), DefAttrs, Infos]),
	io:format(ModHrlFile, ?MODULE_DEF_FORMAT, [DesStr, atom_to_list(Name), DefAttrs, Infos]),
	inner_create_models(Tail, ModHrlFile, HrlFile, LogFile);
inner_create_models([Arg|Tail], ModHrlFile, HrlFile, LogFile)->
	io:format("~n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-  ERROR  -!!!!!!!!!!!!!!!!!!!!!!!!!!!!!~nConfig File Error,~n Reason:~p~n~n", [Arg]),
	inner_create_models(Tail, ModHrlFile, HrlFile, LogFile).

get_tab_str(Name, ValN) ->
	TabNum = case lists:last(Name) of
				 "[]" ->
					 (length(Name) + ValN + 1) div 4;
				 _ ->
					 (length(Name) + ValN) div 4
			 end,
	lists:concat(lists:duplicate((7-TabNum), "\t")).

