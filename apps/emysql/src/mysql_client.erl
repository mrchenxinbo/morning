%% @doc 为mysql使用者提供基础的方法
%% @version 0.1
%% @author Administrator
-module(mysql_client).

-export([insert/2, delete/2, delete_all/1, update/2, update_list/2, select/2, select_with_fields/2, select_columns/3,
		 transaction/1,transaction/2, select/1, update/1, select_count/2, select_count/4]).
-export([insert_log/2, update_log/1, update_log/2, select_log_columns/3, select_log/1, delete_log/2]).

-include("logger.hrl").

%%% @doc 插入操作
%%% TbName atom() 要插入的表名
%%% ValueSql list() 插入的value语句
%%% @spec insert(TbName, ValueSql) -> {ok, AffectedNum, InsertId} | error
%%% AffectedNum integer() 执行成功后受影响的数目
insert(TbName, ValueSql) when is_atom(TbName) ->
	WpoolId = mysql_util:get_w_pool_id(),
	RealTbName = mysql_util:get_tbprefix() ++ atom_to_list(TbName),
	RealSql = "INSERT INTO " ++ RealTbName ++ " " ++ ValueSql,
	Result = emysql:execute(WpoolId, RealSql),
 	case emysql:result_type(Result) of
		ok ->
			{ok, emysql:affected_rows(Result), emysql:insert_id(Result)};
		error ->
			case loop_error(error, 1, WpoolId, RealSql) of
				{ok, Result} ->
					{ok, emysql:affected_rows(Result), emysql:insert_id(Result)};
				_ ->
					
					?ERROR_MSG("insert RealSql : ~p, error : ~p~n", [RealSql, Result]),
					error
			end
	end;
insert(_, _) ->
	error.

loop_error(ok, Result, _WpoolId, _RealSql) ->
	{ok, Result};
loop_error(error, 5, _WpoolId, _RealSql) ->
	error;
loop_error(error, Num, WpoolId, RealSql) ->
	Result = emysql:execute(WpoolId, RealSql),
	case emysql:result_type(Result) of
		ok ->
			loop_error(ok, Result, WpoolId, RealSql);
		error ->
			loop_error(error, Num + 1, WpoolId, RealSql)
	end.

%%% @doc 删除操作
%%% TbName atom() 表名
%%% Sql string() 删除的sql语句
%%% @spec delete(TbName, Sql) -> {ok, AffectedNum} | error
%%% AffectedNum integer() 执行成功后受影响的数目
delete(TbName, Where) when is_atom(TbName) ->
	WpoolId = mysql_util:get_w_pool_id(),
	RealTbName = mysql_util:get_tbprefix() ++ atom_to_list(TbName),
	RealSql = "DELETE FROM " ++ RealTbName ++ Where,
	Result = emysql:execute(WpoolId, RealSql),
	case emysql:result_type(Result) of
		ok ->
			{ok, emysql:affected_rows(Result)};
		error ->
			case loop_error(error, 1, WpoolId, RealSql) of
				{ok, Result} ->
					{ok, emysql:affected_rows(Result)};
				_ ->
					
					?ERROR_MSG("delete RealSql : ~p, error : ~p~n", [RealSql, Result]),
					error
			end
	end;
delete(_, _) ->
	error.

%%% @doc 清空表操作
%%% TbName atom() 表名
%%% @spec delete_all(TbName) -> {ok, AffectedNum} | error
%%% AffectedNum integer() 执行成功后受影响的数目
delete_all(TbName) when is_atom(TbName) ->
	Sql = "delete from "++atom_to_list(TbName),
	delete(TbName, Sql);
delete_all(_) ->
	error.

%%% @doc 更新操作
%%% Sql string() 要更新的sql语句
%%% @spec update(Sql) -> {ok, AffectedNum} | error
%%% AffectedNum integer() 执行成功后受影响的数目
update(Sql) ->
	WpoolId = mysql_util:get_w_pool_id(),
	Result = emysql:execute(WpoolId, Sql),
	case emysql:result_type(Result) of
		ok ->
			{ok, emysql:affected_rows(Result)};
		error ->
			case loop_error(error, 1, WpoolId, Sql) of
				{ok, Result} ->
					{ok, emysql:affected_rows(Result)};
				_ ->
					db_event:set_sql_log(Sql),
					?ERROR_MSG("update Sql : ~p, error : ~p~n", [Sql, Result]),
					error
			end
	end.

%%% @doc 更新操作
%%% TbName atom() 表名
%%% Sql string() 要更新的sql语句
%%% @spec update(TbName, Sql) -> {ok, AffectedNum} | error
%%% AffectedNum integer() 执行成功后受影响的数目
update(TbName, Sql) when is_atom(TbName) ->
	WpoolId = mysql_util:get_w_pool_id(),
	RealTbName = mysql_util:get_tbprefix() ++ atom_to_list(TbName),
	RealSql = "UPDATE " ++ RealTbName ++ " " ++ Sql,
	Result = emysql:execute(WpoolId, RealSql),
	case emysql:result_type(Result) of
		ok ->
			{ok, emysql:affected_rows(Result)};
		error ->
			case loop_error(error, 1, WpoolId, RealSql) of
				{ok, Result} ->
					{ok, emysql:affected_rows(Result)};
				_ ->
					
					?ERROR_MSG("update RealSql : ~p, error : ~p~n", [RealSql, Result]),
					error
			end
	end;
update(_, _) ->
	error.

%%% @doc 更新操作
%%% TbName atom() 表名
%%% Sql string() 要更新的sql语句
%%% @spec update(TbName, Sql) -> {ok, AffectedNum} | error
%%% AffectedNum integer() 执行成功后受影响的数目
update_list(TbName, Sql) when is_atom(TbName) ->
	WpoolId = mysql_util:get_w_pool_id(),
	RealTbName = mysql_util:get_tbprefix() ++ atom_to_list(TbName),
	RealSql = "REPLACE INTO " ++ RealTbName ++ " " ++ Sql,
	Result = emysql:execute(WpoolId, RealSql),
	case emysql:result_type(Result) of
		ok ->
			{ok, emysql:affected_rows(Result)};
		error ->
			case loop_error(error, 1, WpoolId, RealSql) of
				{ok, Result} ->
					{ok, emysql:affected_rows(Result)};
				_ ->
					
					?ERROR_MSG("update_list RealSql : ~p, error : ~p~n", [RealSql, Result]),
					error
			end
	end;
update_list(_, _) ->
	error.

%%% @doc 查询操作
%%% Sql string() 要查询的sql语句
%%% @spec select(Sql) -> {ok, AllRows} | error
%%% AllRows list() 执行成功后查询出的所有数据列表
select(Sql) ->
	RpoolId = mysql_util:get_r_pool_id(),
	Result = emysql:execute(RpoolId, Sql),
	case emysql:result_type(Result) of
		result ->
			{ok, emysql:result_rows(Result)};
		error ->
			?ERROR_MSG("select Sql : ~p, error : ~p~n", [Sql, Result]),
			error
	end.

%%% @doc 查询操作
%%% TbName atom() 表名
%%% Sql string() 要查询的sql语句
%%% @spec select(TbName, Where) -> {ok, AllRows} | error
%%% AllRows list() 执行成功后查询出的所有数据列表
select(TbName, Where) when is_atom(TbName) ->
	RpoolId = mysql_util:get_r_pool_id(),
	RealTbName = mysql_util:get_tbprefix() ++ atom_to_list(TbName),
	RealSql = "SELECT * FROM " ++ RealTbName ++ Where,
	Result = emysql:execute(RpoolId, RealSql),
	case emysql:result_type(Result) of
		result ->
			{ok, emysql:result_rows(Result)};
		error ->
			?ERROR_MSG("select RealSql : ~p, error : ~p~n", [RealSql, Result]),
			error
	end;
select(_, _) ->
	error.

%%% @doc 查询操作
%%% TbName atom() 表名
%%% Sql string() 要查询的sql语句
%%% @spec select(TbName, Where) -> {ok, AllRows} | error
%%% AllRows list() 执行成功后查询出的所有数据列表
select_with_fields(TbName, Where) when is_atom(TbName) ->
	RpoolId = mysql_util:get_r_pool_id(),
	RealTbName = mysql_util:get_tbprefix() ++ atom_to_list(TbName),
	RealSql = "SELECT * FROM " ++ RealTbName ++ Where,
	Result = emysql:execute(RpoolId, RealSql),
	case emysql:result_type(Result) of
		result ->
			{ok, emysql:field_names(Result), emysql:result_rows(Result)};
		error ->
			?ERROR_MSG("select RealSql : ~p, error : ~p~n", [RealSql, Result]),
			error
	end;
select_with_fields(_, _) ->
	error.


%%% @doc 查询操作
%%% TbName atom() 表名
%%% Columns string() 要查询的字段
%%% Where string() 要查询的where语句
%%% @spec select_count(TbName, Columns, Where) -> {ok, AllRows} | error
%%% AllRows list() 执行成功后查询出的所有数据列表
select_columns(TbName, Columns, Where) when is_atom(TbName) ->
	RpoolId = mysql_util:get_r_pool_id(),
	RealTbName = mysql_util:get_tbprefix() ++ atom_to_list(TbName),
	RealSql = "SELECT " ++ Columns ++ " FROM " ++ RealTbName ++ " " ++ Where,
	Result = emysql:execute(RpoolId, RealSql),
	case emysql:result_type(Result) of
		result ->
			{ok, emysql:result_rows(Result)};
		error ->
			?ERROR_MSG("select RealSql : ~p, error : ~p~n", [RealSql, Result]),
			error
	end;
select_columns(_, _, _) ->
	error.

%%% @doc 查询数据条数
%%% TbName atom() 表名
%%% @spec select_count(TbName) -> Count
%%% Count int 执行成功后查询出的所有数据条数
select_count(TbName, CountField) ->
	case select_columns(TbName, "count("++ atom_to_list(CountField) ++")", ";") of
		{ok,RowList}->
			case RowList of
				[[undefined]]->
					 0;
				[[AllCount]]->
					AllCount
			end;
		_->
			0
	end.

%%% @doc 查询数据条数
%%% TbName atom() 表名
%%% @spec select_count(TbName) -> Count
%%% Where string() 要查询的where语句
%%% Count int 执行成功后查询出的所有数据条数
select_count(TbName, CountField, Conditions, Module) ->
    FormatCond = Module:where_condition_format(Conditions),
    Where = mysql_helper:pack_where(FormatCond),
    case select_columns(TbName, "count("++ atom_to_list(CountField) ++")", Where) of
        {ok,RowList}->
            case RowList of
                [[undefined]]->
                     0;
                [[AllCount]]->
                    AllCount
            end;
        _->
            0
    end.


%% 日志专用
insert_log(TbName, ValueSql) when is_list(TbName) ->
	WpoolId = mysql_util:get_log_pool_id(),
	RealTbName = mysql_util:get_logprefix() ++ TbName,
	RealSql = "INSERT INTO " ++ RealTbName ++ " " ++ ValueSql,
	Result = emysql:execute(WpoolId, RealSql),
	case emysql:result_type(Result) of
		ok ->
			{ok, emysql:affected_rows(Result), emysql:insert_id(Result)};
		error ->
			?ERROR_MSG("insert_log RealSql : ~p, error : ~p~n", [RealSql, Result]),
			error
	end;
insert_log(_, _) ->
	error.

%% 日志专用
update_log(Sql) ->
	WpoolId = mysql_util:get_log_pool_id(),
	Result = emysql:execute(WpoolId, Sql),
	case emysql:result_type(Result) of
		ok ->
			{ok, emysql:affected_rows(Result)};
		error ->
			?ERROR_MSG("update_log Sql : ~p, error : ~p~n", [Sql, Result]),
			error
	end.
%% 日志专用
update_log(TbName, Sql) when is_list(TbName) ->
	WpoolId = mysql_util:get_log_pool_id(),
	RealTbName = mysql_util:get_logprefix() ++ TbName,
	RealSql = "UPDATE " ++ RealTbName ++ " " ++ Sql,
	Result = emysql:execute(WpoolId, RealSql),
	case emysql:result_type(Result) of
		ok ->
			{ok, emysql:affected_rows(Result)};
		error ->
			?ERROR_MSG("update_log RealSql : ~p, error : ~p~n", [RealSql, Result]),
			error
	end;
update_log(_, _) ->
	error.

%% 日志专用
select_log_columns(TbName, Columns, Where) when is_list(TbName) ->
	RealTbName = mysql_util:get_logprefix() ++ TbName,
	Sql = "SELECT " ++ Columns ++ " FROM " ++ RealTbName ++ " " ++ Where,
	select_log(Sql);
select_log_columns(_, _, _) ->
	error.

select_log(Sql) when is_list(Sql) ->
	RpoolId = mysql_util:get_log_pool_id(),
	Result = emysql:execute(RpoolId, Sql),
	case emysql:result_type(Result) of
		result ->
			{ok, emysql:result_rows(Result)};
		error ->
			?ERROR_MSG("select_log_columns Sql : ~p, error : ~p~n", [Sql, Result]),
			error
	end;
select_log(_) ->
	error.

delete_log(TbName, Where) when is_atom(TbName) ->
    RealTbName = mysql_util:get_logprefix() ++ atom_to_list(TbName),
    delete_log(RealTbName, Where);
delete_log(RealTbName, Where) when is_list(RealTbName) ->
    WpoolId = mysql_util:get_log_pool_id(),
    RealSql = "DELETE FROM " ++ RealTbName ++ Where,
    Result = emysql:execute(WpoolId, RealSql),
    case emysql:result_type(Result) of
        ok ->
            {ok, emysql:affected_rows(Result)};
        error ->
            ?ERROR_MSG("delete_log RealSql : ~p, error : ~p~n", [RealSql, Result]),
            error
    end;
delete_log(_, _) ->
    error.

%%% @doc 事物操作
%%% Fun::function() 要要操作的sql语句级成的函数，需要注意控制返回值，才能回滚
%%% @spec transaction(Fun) -> {ok, Result} | {error, {Reason, {rollback_result, Result}}}
%%% AffectedNum integer() 执行成功后受影响的数目
transaction(Fun,TimeOut) when erlang:is_function(Fun)->
	todo;
transaction(_, _) ->
	error.

%%% @doc 事物操作
%%% Fun::function() 要要操作的sql语句级成的函数，需要注意控制返回值，才能回滚
%%% @spec transaction(Fun) -> {atomic, Result} | {error, {Reason, {rollback_result, Result}}}
%%% AffectedNum integer() 执行成功后受影响的数目
transaction(Fun) when erlang:is_function(Fun)->
   transaction(Fun,5000).


