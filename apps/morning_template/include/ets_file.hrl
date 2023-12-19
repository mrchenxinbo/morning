-define(IMPORT_CACHE_ETS, import_cache_ets).
-define(PROTO_MODULE_PREFIX, "proto_").
-define(SELECT_PAGE_SIZE, 5000).

-define(INIT_ETS(RecordName, EtsName, EtsKeyPosOrPoses, EtsData),
		begin
			ets_file:version_check(RecordName, 1),
			case ets:lookup(ets_flag, ets_flag) of
				[{ets_flag,0}] ->
					ets_file:init_ets(EtsName, EtsKeyPosOrPoses, EtsData),
					ets_file:init_ets(list_to_atom(atom_to_list(EtsName) ++ "2"), EtsKeyPosOrPoses, EtsData);
				[{ets_flag,1}] ->
					ets_file:init_ets(list_to_atom(atom_to_list(EtsName) ++ "2"), EtsKeyPosOrPoses, EtsData);
				[{ets_flag,2}] ->
					ets_file:init_ets(EtsName, EtsKeyPosOrPoses, EtsData)
			end
		end).

-define(INIT_ETS_WITH_CALLBACK(RecordName, EtsName, Module, Fun, Args),
		case ets:lookup(ets_flag, ets_flag) of
			[{ets_flag,0}] ->
				ets:delete_all_objects(EtsName),
				Module:Fun(EtsName, ets_file:read_cache(RecordName), Args),
				ets:delete_all_objects(list_to_atom(atom_to_list(EtsName) ++ "2")),
				Module:Fun(list_to_atom(atom_to_list(EtsName) ++ "2"), ets_file:read_cache(RecordName), Args);
			[{ets_flag,1}] ->
				ets:delete_all_objects(list_to_atom(atom_to_list(EtsName) ++ "2")),
				Module:Fun(list_to_atom(atom_to_list(EtsName) ++ "2"), ets_file:read_cache(RecordName), Args);
			[{ets_flag,2}] ->
				ets:delete_all_objects(EtsName),
				Module:Fun(EtsName, ets_file:read_cache(RecordName), Args)
		end).

-define(NEW_ETS(EtsName, Operations),
			ets:new(EtsName, Operations),
			ets:new(list_to_atom(atom_to_list(EtsName) ++ "2"), Operations)
		).

-define(GET_ENABLE_TABLE(EtsName),
			case ets:lookup(ets_flag, ets_flag) of
				[{ets_flag,0}] ->
					EtsName;
				[{ets_flag,1}] ->
					EtsName;
				[{ets_flag,2}] ->
					list_to_atom(atom_to_list(EtsName) ++ "2")
			end).

-define(TO_FLOAT_RECORD_LIST,[
		{role_icon, [id,icon_name]}
	]).