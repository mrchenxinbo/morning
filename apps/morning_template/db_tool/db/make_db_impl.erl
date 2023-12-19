-module(make_db_impl).

-define(HRL_INCLUDE_DIR,"./include/").
-define(ERL_SRC_CREATE_DIR,"./src_db_mods/").

%%recordname:				dbname
%%field:					fieldsname
%%type:						set/bag
%%stroe_type:				disc_split/disc/ram/proto
%%args:						key:[xxx,yyy]/index:[xxx,yyy]
%%inittype:					normal/callback
-record(db_record,{recordname,field,type,stroe_type,args,inittype}).

-define(TYPES,[set,bag]).
-define(STROE_TYPES,[disc_split,disc,ram,proto]).
-define(INIT_TYPE,[normal,callback]).
-define(BLANK,$\ ).
-define(ENTER,$\n).
-define(TAB,$\t).
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
	lists:foreach(fun(ModelConfigFileName)-> create_db(ModelConfigFileName) end, ModelConfigFileNameList).

create_db(ModelConfigFileName)->
    HeaderLength = length(?MODEL_CONFIG_ROOT)+length(?MODEL_CONFIG_PREFIX),
    NameLength = length(ModelConfigFileName)-HeaderLength-length(".config"),
    ModFileName = string:substr(ModelConfigFileName, HeaderLength+1, NameLength),
    HrlFileName = ?MODEL_DEF_FILE_PREFIX ++ ModFileName ++ ".hrl",
	gen_db_code(HrlFileName, ModFileName).

gen_db_code(HrlFile, ModFileName)->
	File = ?HRL_INCLUDE_DIR ++ HrlFile,
	OutMod = list_to_atom(ModFileName),
	OutFile = ?ERL_SRC_CREATE_DIR ++ erlang:atom_to_list(OutMod)++".erl",
	case file:open(File,[read]) of
		{ok, F}->
			AllRecord = read_all_record(F),
 			case write_to_file(AllRecord,OutFile,OutMod) of
				{error, exsit}->
					io:format("OutFile ~p is exist !!! not create again~n",[OutFile]);
				{error, Reason}->
					io:format("open file ~p error: ~p ~n",[OutFile,Reason]);
				ok->
					%io:format("~p ~n",[AllRecord]),
					io:format("Complete auto-coding ~p ~n",[OutFile]),	
					file:close(F)
			end;
		{error,Reason}-> io:format("open file ~p error: ~p ~n",[File,Reason])
	end.
	
write_to_file(AllRecord,OutFile,OutMod)->	
    case file:open(OutFile, [write]) of
        {ok,F}->
            write_record_to_file(F,OutMod,AllRecord),
            file:close(F),
            ok;
        {error,Reason}->
            {error,Reason}
    end.

	
write_record_to_file(F,OutMod,AllRecord)->
	BinData = get_file_str(OutMod,AllRecord),
	Bin = xmerl_ucs:to_utf8(BinData),
	file:write(F, Bin).
	
get_file_str(OutMod,AllRecord)->	
		"%%\n%% 工具生成代码,如果想加函数,请写到自定义区域.\n"++
		"%%\n%% create by gen_db_code,you can edit it, especially use db index\n%%\n"++
		"-module(" ++ erlang:atom_to_list(OutMod) ++ ").\n\n"++
		"-include(\"model_def.hrl\").\n\n"++
		make_ets_init_callback_export_str(AllRecord)++
		"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
		"%% 						behaviour export\n"
		"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
		"-export([start/0,create_mnesia_table/1,create_mnesia_split_table/2,delete_role_from_db/1,tables_info/0]).\n\n"++
		"-behaviour(db_operater_mod).\n"	
		"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
		"%% 				behaviour functions\n"
		"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n"	
		"start()->\n"
		"	db_operater_mod:start_module(?MODULE,[]).\n\n"++
		make_records_create_str(AllRecord)				++
		"delete_role_from_db(_RoleId)->\n"
		"\ttodo.\n\n"
		"tables_info()->\n\t"++
		term_to_string(lists:map(fun(#db_record{recordname = Name,stroe_type = Type})->{Name,Type} end, AllRecord))++
		".\n\n"
		++
		"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"
		"%% 				behaviour functions end\n"
		"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n"
		++make_ets_init_callback_str(AllRecord)++
		"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"++
		"%% 自动生成代码结束,下面是自定义区域,可以随意增删修改\n"++
		"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n\n".
		
make_ets_init_callback_export_str(AllRecord)->
	case filter_records_by_store_type([proto],AllRecord) of
		[]->
			"";
		ProtoRecords ->
			CallBackRecords = [Record|| #db_record{inittype=callback} = Record<-ProtoRecords],
			case CallBackRecords of
				[] ->
					"";
				CallBackRecords ->
					"-export(["
					++string:join(lists:map(
					  	fun(#db_record{recordname=RecordName})->
							"init_"++atom_to_list(RecordName)++"/3"
						end, CallBackRecords), ",\n")
					++"]).\r\n"
			end
	end.

make_ets_init_callback_str(AllRecord)->
	case filter_records_by_store_type([proto],AllRecord) of
		[]->
			"";
		ProtoRecords ->
			CallBackRecords = [Record|| #db_record{inittype=callback} = Record<-ProtoRecords],
			case CallBackRecords of
				[] ->
					"";
				CallBackRecords ->
					"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n"++
					"%% ETS数据初始化回调函数\n"++
					"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n\n"++
					string:join(lists:map(
					  	fun(#db_record{recordname=RecordName})->
							"init_"++atom_to_list(RecordName)++"(ETS_NAME, "++string:to_upper(atom_to_list(RecordName))++"s, Args)->\r\n\t"++
								 "lists:foreach(fun(Term)-> \r\n\t\ttodo \r\n\tend, "++string:to_upper(atom_to_list(RecordName))++"s).\r\n"
						end, CallBackRecords), "")
			end
	end.

filter_records_by_store_type(StaorTypes,AllRecord)->
	lists:filter(fun(ReCord)->#db_record{stroe_type=Stroe_type} = ReCord,lists:member(Stroe_type, StaorTypes) end,AllRecord).

make_records_create_str(AllRecord)->
	RamReCords = filter_records_by_store_type([ram],AllRecord),
	DiscRecords = filter_records_by_store_type([disc,proto],AllRecord),
	DiscSpiltRecords = filter_records_by_store_type([disc_split],AllRecord),
	make_ram_records_create(RamReCords)++make_disc_records_create(DiscRecords)++make_disc_split_records_create(DiscSpiltRecords).

make_disc_records_create(_)->
	"create_mnesia_table(disc)->\n\tnothing.\n\n".

make_ram_records_create([])->
	"create_mnesia_table(ram)->\n\tnothing;\n\n";
make_ram_records_create(RamReCords)->
	"create_mnesia_table(ram)->\n"++
	lists:foldl(fun(#db_record{recordname=RocordName,type=Type,args=Args},AccStrTmp)-> 
				ReCordStr = atom_to_list(RocordName),
				TypeStr = atom_to_list(Type),
				{index,TableIndex} = lists:keyfind(index,1,Args),
				AccStrTmp++
				if
					AccStrTmp=/=[]->
						",\n";
					true->
						[]
				end ++  
				"\tdb_tools:create_table_ram("++ReCordStr++",record_info(fields,"++ReCordStr++"),"++
				term_to_string(TableIndex)	++
					","++TypeStr++")"
			end,[], RamReCords)++";\n\n".
									  
make_disc_split_records_create([])->
	"create_mnesia_split_table(_,_)->\n\tnothing.\n\n";
make_disc_split_records_create(RamReCords)->
	lists:foldl(fun(#db_record{recordname=RocordName,type=Type,args=Args},AccStrTmp)-> 
				{index,TableIndex} = lists:keyfind(index,1,Args),		
				ReCordStr = atom_to_list(RocordName),
				TypeStr = atom_to_list(Type),
				{index,TableIndex} = lists:keyfind(index,1,Args),
				AccStrTmp++
				if
					AccStrTmp=/=[]->
						";\n";
					true->
						[]
				end++
				"create_mnesia_split_table("++ReCordStr++",TrueTabName)->\n\tdb_tools:create_table_disc(TrueTabName,record_info(fields,"++
				ReCordStr++"),"++	
				term_to_string(TableIndex)	++
				","++TypeStr++")"
			end,[], RamReCords)++".\n\n".
	
read_all_record(F)->
	read_all_record_loop(F,[],[]).

read_all_record_loop(F,LastData,AllRecords)->
	case file:read_line(F) of
		eof->
			AllRecords;
		{ok,OriData}->
			case clear_blank_edge(OriData) of
				[]->
					read_all_record_loop(F,LastData,AllRecords);
				OriDataWithArgs->	
					[OriRecordData|_]  = string:tokens(OriDataWithArgs,"%"),
					NewRecordData = clear_blank_edge(OriRecordData),
					case has_str(NewRecordData,"-record(")  of
						false->				%%not record start
							case has_str(NewRecordData,").") of
								false->		%%not end
									read_all_record_loop(F,LastData++NewRecordData,AllRecords);
								_->		%%record end!
									read_all_record_loop(F,[],AllRecords++[parse_to_db_record(LastData++OriDataWithArgs)])
							end;
%% 							read_all_record_loop(F,LastData,AllRecords);
						_->				%%is record start
							case has_str(NewRecordData, ").") of
								false->		%%not end
									read_all_record_loop(F,NewRecordData,AllRecords);
								_->		%%has end
									read_all_record_loop(F,[],AllRecords++[parse_to_db_record(LastData++OriDataWithArgs)])
							end
					end
			end
	end.
		
parse_to_db_record(RecordStr)->
	#db_record{
			   recordname = get_record_name(RecordStr),
			   field = get_record_fileds(RecordStr),
			   type = get_record_type(RecordStr),
			   stroe_type = get_stroe_type(RecordStr),
			   args = get_record_args(RecordStr),
			   inittype = get_inittype(RecordStr)
			  }.

get_record_name(RecordStr)->
	Start = string:str(RecordStr, "(")+1,
	Lenth = string:str(RecordStr, ",")-Start,
	erlang:list_to_atom(string:strip(string:substr(RecordStr, Start, Lenth),both,?BLANK)).

get_record_fileds(RecordStr)->
	Start = string:str(RecordStr, "{")+1,
	Lenth = string:str(RecordStr, "}")-Start,
	RecordFieldsStr = string:strip(string:substr(RecordStr, Start, Lenth),both,?BLANK), 
	lists:map(fun(FiledStr)-> 
                    Field = hd(string:tokens(string:join(string:tokens(FiledStr," \t\r\n"), ""),"=")),
                    erlang:list_to_atom(Field) end,
            string:tokens(RecordFieldsStr,",")). 
	
get_record_type(RecordStr)->
	[_|ArgsL] = string:tokens(RecordStr,"@"),
	[Args] = ArgsL,
	[Type|_] = lists:filter(fun(Term)->has_str(Args,erlang:atom_to_list(Term))  end,?TYPES),
	Type.

get_stroe_type(RecordStr)->
	[_|ArgsL] = string:tokens(RecordStr,"@"),
	[Args] = ArgsL,
	[Type|_] = lists:filter(fun(Term)->has_str(Args,erlang:atom_to_list(Term))  end,?STROE_TYPES),
	Type.

get_record_args(RecordStr)->
	[{key,get_record_keys(RecordStr)}]
	++
	[{index,get_indexes(RecordStr)}].
		
get_indexes(RecordStr)->
	[_|ArgsL] = string:tokens(RecordStr,"@"),
	[Args] = ArgsL,
	case string:str(Args,"index:") of
		0->
			[];
		EtsIndex->
			EtsStrAll = string:substr(Args, EtsIndex+6),
			[EtsStr|_] = string:tokens(EtsStrAll,"]"),
			string_to_term(EtsStr++"]")
	end.

get_inittype(RecordStr)->
	[_|ArgsL] = string:tokens(RecordStr,"@"),
	[Args] = ArgsL,
	case lists:filter(fun(Term)->has_str(Args,erlang:atom_to_list(Term))  end,?INIT_TYPE) of
		[]->
			normal;
		[callback]->
			callback
	end.

get_record_keys(RecordStr)->
	[_|ArgsL] = string:tokens(RecordStr,"@"),
	[Args] = ArgsL,
	case string:str(Args,"key:") of
		0->
			[FiledKey|_] = get_record_fileds(RecordStr),
			[FiledKey];
		EtsIndex->
			EtsStrAll = string:substr(Args, EtsIndex+4),
			[EtsStr|_] = string:tokens(EtsStrAll,"]"),
			string_to_term(EtsStr++"]")
	end.

has_str(Str,Strpart)->
	string:str(Str,Strpart)=/=0.

clear_blank_edge(OriData)->
	string:strip(string:strip(string:strip(OriData,both,?ENTER),both,?BLANK),both,?TAB).

term_to_string(Term)->
	lists:flatten(io_lib:format("~w", [Term])).

string_to_term(String)->
	case erl_scan:string(String++".") of
		{ok,Tokens,_}->
			case erl_parse:parse_term(Tokens) of
				{ok,Term}->
					Term;
				_->
					{}				
			end;
		_->
			{}
	end.

%% upper_first_char([First|Left])->
%% 	[string:to_upper(First) | Left].
