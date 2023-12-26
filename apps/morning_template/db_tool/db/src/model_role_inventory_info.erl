-module(model_role_inventory_info).
-include("model_def.hrl").
-compile(export_all).

%% @doc		查询操作
%% @args	FieldList = [id,name]   要查询的字段
%%			Conditions = [{power, '>', 5000}, {level, '>', 65}]   查询条件
%% @return	{ok, AllRows} | error
select(FieldList, Conditions)->
    FormatCond = where_condition_format(Conditions),
	Columns=string:join([atom_to_list(Key) || Key<-FieldList], ","),
	Where = mysql_helper:pack_where(FormatCond),
	mysql_client:select_columns(role_inventory_info, Columns, Where).

%% @doc		插入操作
%% @args	Record | RecordList   要插入的表结构
%% @return	{ok, AffectedNum, InsertId} | error
insert({role_inventory_info, ID, UID, PROTO_ID, COUNT, UPDATE_TS})->
	mysql_client:insert(role_inventory_info, "(`id`, `uid`, `proto_id`, `count`, `update_ts`) VALUES ("++mysql_helper:pack_value_by_type({ID, int})++", "++mysql_helper:pack_value_by_type({UID, int})++", "++mysql_helper:pack_value_by_type({PROTO_ID, int})++", "++mysql_helper:pack_value_by_type({COUNT, int})++", "++mysql_helper:pack_value_by_type({UPDATE_TS, bigint})++");");
insert([])->
	error;
insert(InsertList) when is_list(InsertList)->
	ValueSql = pack_bash_insert(InsertList),
	mysql_client:insert(role_inventory_info, ValueSql).

insert_auto({role_inventory_info, ID, UID, PROTO_ID, COUNT, UPDATE_TS})->
		mysql_client:insert(role_inventory_info, "( `uid`, `proto_id`, `count`, `update_ts`) VALUES ("++mysql_helper:pack_value_by_type({UID, int})++", "++mysql_helper:pack_value_by_type({PROTO_ID, int})++", "++mysql_helper:pack_value_by_type({COUNT, int})++", "++mysql_helper:pack_value_by_type({UPDATE_TS, bigint})++");").

%% @doc		更新操作
%% @args	Record | RecordList   要更新的表结构
%% @return	{ok, AffectedNum} | error
update({role_inventory_info, ID, UID, PROTO_ID, COUNT, UPDATE_TS})->
	mysql_client:update(role_inventory_info, " SET `update_ts`="++mysql_helper:pack_value_by_type({UPDATE_TS, bigint})++", `count`="++mysql_helper:pack_value_by_type({COUNT, int})++", `proto_id`="++mysql_helper:pack_value_by_type({PROTO_ID, int})++", `uid`="++mysql_helper:pack_value_by_type({UID, int})++" WHERE `id`="++mysql_helper:pack_value_by_type({ID, int})++"");
update([])->
	error;
update(UpdateList) when is_list(UpdateList)->
	ValueSql = pack_bash_insert(UpdateList),
	mysql_client:update_list(role_inventory_info, ValueSql).

%% @doc		插入或更新操作
%% @args	Record | RecordList   要更新的表结构
%% @return	{ok, AffectedNum, InsertId} | error
insert_or_update({role_inventory_info, ID, UID, PROTO_ID, COUNT, UPDATE_TS})->
	mysql_client:insert(role_inventory_info, "(`id`, `uid`, `proto_id`, `count`, `update_ts`) VALUES("++mysql_helper:pack_value_by_type({ID, int})++", "++mysql_helper:pack_value_by_type({UID, int})++", "++mysql_helper:pack_value_by_type({PROTO_ID, int})++", "++mysql_helper:pack_value_by_type({COUNT, int})++", "++mysql_helper:pack_value_by_type({UPDATE_TS, bigint})++") ON DUPLICATE KEY UPDATE `update_ts`="++mysql_helper:pack_value_by_type({UPDATE_TS, bigint})++", `count`="++mysql_helper:pack_value_by_type({COUNT, int})++", `proto_id`="++mysql_helper:pack_value_by_type({PROTO_ID, int})++", `uid`="++mysql_helper:pack_value_by_type({UID, int})++"").

%% @doc		更新操作
%% @args	FieldValueList = [{id, Value}]   要更新的字段
%%			Conditions = [{power, '>', 5000}, {level, '>', 65}]   更新条件
%% @return	{ok, AffectedNum} | error
update_fields(FieldValueList, Conditions)->
    FormatCond = where_condition_format(Conditions),
	Columns=[{Key, '=', {Value, get_column_datatype(Key)}}||{Key, Value}<-FieldValueList],
	SQL = mysql_helper:pack_update_columns(Columns)++mysql_helper:pack_where(FormatCond),
	mysql_client:update(role_inventory_info, SQL).

%% @doc		删除操作
%% @args	Record  数据表结构
%%		or	Conditions = [{power, '>', 5000}, {level, '>', 65}]   删除条件
%% @return	{ok, AffectedNum} | error
delete({role_inventory_info, ID, UID, PROTO_ID, COUNT, UPDATE_TS}) when is_tuple({role_inventory_info, ID, UID, PROTO_ID, COUNT, UPDATE_TS})->
    mysql_client:delete(role_inventory_info, " WHERE `id`="++mysql_helper:pack_value_by_type({ID, int})++"");
delete([])->
	error;
delete(Conditions) when is_list(Conditions)->
    FormatCond = where_condition_format(Conditions),
	SQL = mysql_helper:pack_where(FormatCond),
    mysql_client:delete(role_inventory_info, SQL).

read_by_record({role_inventory_info, ID, UID, PROTO_ID, COUNT, UPDATE_TS}=Record) when is_record(Record, role_inventory_info)->
	case mysql_client:select(role_inventory_info, " WHERE `id`="++mysql_helper:pack_value_by_type({ID, int})++"") of
		{ok,[]}->
			[];
		{ok,[RowList]}->
            unpack_rows([RowList])
	end.

%% @doc		读取操作
%% @args	Key  主键
%% @return	AllRows | []
read(ID)->
	Columns = string:join(["`" ++ atom_to_list(Key) ++ "`" || Key <- record_info(fields, role_inventory_info)], ","),
	case mysql_client:select_columns(role_inventory_info, Columns, " WHERE `id`="++mysql_helper:pack_value_by_type({ID, int})++"") of
		{ok,RowList}->
            unpack_rows(RowList);
		_ ->
			[]
	end.

%% @doc		插入或更新操作
%% @args	Record 要更新的表结构
%% @return	{ok, AffectedNum, InsertId} | {ok, AffectedNum} | error
write({role_inventory_info, ID, UID, PROTO_ID, COUNT, UPDATE_TS})->
    case read_by_record({role_inventory_info, ID, UID, PROTO_ID, COUNT, UPDATE_TS}) of
		[] ->
			insert({role_inventory_info, ID, UID, PROTO_ID, COUNT, UPDATE_TS});
		_Any ->
			update({role_inventory_info, ID, UID, PROTO_ID, COUNT, UPDATE_TS})
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
		[] -> Fields = record_info(fields, role_inventory_info);
		_ ->  Fields = FieldList
	end,
	Columns = string:join(["`" ++ atom_to_list(Key) ++ "`" || Key <- Fields], ","),
	case mysql_client:select_columns(role_inventory_info, Columns, Where) of
		{ok, RowList} -> unpack_rows(RowList, FieldList);
		_ -> []
	end.

%% @doc		查询操作
%% @return	AllRows | []
all()->
	Columns=string:join(["`" ++ atom_to_list(Key) ++ "`" || Key<-record_info(fields, role_inventory_info)], ","),
	case mysql_client:select_columns(role_inventory_info, Columns, ";") of
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
    [{id,int},{uid,int},{proto_id,int},{count,int},{update_ts,bigint}].

column_place(id)->
	#role_inventory_info.id;
column_place(uid)->
	#role_inventory_info.uid;
column_place(proto_id)->
	#role_inventory_info.proto_id;
column_place(count)->
	#role_inventory_info.count;
column_place(update_ts)->
	#role_inventory_info.update_ts.


unpack_rows(RowList)->
	unpack_rows(RowList, []).
unpack_rows(RowList, []) ->
	unpack_row(RowList, [], [], []);
unpack_rows(RowList, FieldList) ->
	Fileds = record_info(fields, role_inventory_info),
	AllFileds = lists:zip(lists:seq(2, length(Fileds)+1), Fileds),
	unpack_row(RowList, AllFileds, FieldList, []).

unpack_row([], _AllFileds, _FieldList, Records)->
    lists:reverse(Records);
unpack_row([Row|Left], AllFileds, FieldList, Records)->
	case FieldList of
		[] ->
			Record = mysql_helper:unpack_row(role_inventory_info, Row);
		_ ->
			Record = lists:foldl(fun({Filed, Value}, AccTmp) ->
								 case lists:keyfind(Filed, 2, AllFileds) of
									 {Index, _} -> setelement(Index, AccTmp, Value);
									 _ -> AccTmp
								 end
						 end, #role_inventory_info{}, lists:zip(FieldList, Row))
	end,
	unpack_row(Left, AllFileds, FieldList,
				[Record#role_inventory_info{
					
	    		}|Records]).

get_id(Record)->
	Record#role_inventory_info.id.

get_uid(Record)->
	Record#role_inventory_info.uid.

get_proto_id(Record)->
	Record#role_inventory_info.proto_id.

get_count(Record)->
	Record#role_inventory_info.count.

get_update_ts(Record)->
	Record#role_inventory_info.update_ts.


get_bash_insert_value_list({role_inventory_info, ID, UID, PROTO_ID, COUNT, UPDATE_TS}) ->
	"("++mysql_helper:pack_value_by_type({ID, int})++", "++mysql_helper:pack_value_by_type({UID, int})++", "++mysql_helper:pack_value_by_type({PROTO_ID, int})++", "++mysql_helper:pack_value_by_type({COUNT, int})++", "++mysql_helper:pack_value_by_type({UPDATE_TS, bigint})++")".

pack_bash_insert(InsertList) ->
	SqlValueListString = 
		lists:foldl(fun(InsertRecord, ValueList) ->
							case ValueList =:= "" of
								true ->
									[get_bash_insert_value_list(InsertRecord)];
								false ->
									[get_bash_insert_value_list(InsertRecord) | ValueList]
							end
					end, [], lists:reverse(InsertList)),
	ValueString = string:join(SqlValueListString, ","),
	"(`id`, `uid`, `proto_id`, `count`, `update_ts`) VALUES" ++ ValueString ++ ";".
