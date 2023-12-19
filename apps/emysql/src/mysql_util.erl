%% vim: ts=4 sw=4 et
%% Copyright (c) 2009
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% Mike Oxford <moxford@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% @deprecated Please use the functions in {@link emysql} instead.
-module(mysql_util).

-include("emysql.hrl").

%% Query data
-export([
	affected_rows/1,
	field_names/1,
	insert_id/1,
	result_type/1
]).

%% Conversion routines
-export([
         as_dict/1,
         as_json/1,
         as_proplist/1,
         as_record/3,
         as_record/4
]).

-export([get_pool/0,
		 escape/1,
		 keep_alive/0]).

-export([get_r_conf/0, get_r_pool_id/0, get_w_conf/0, get_w_pool_id/0, get_log_conf/0, get_log_pool_id/0,
		 get_tbprefix/0, get_logprefix/0, get_node_by_poolid/1]).

affected_rows(P) -> emysql:affected_rows(P).
field_names(R) -> emysql:field_names(R).
insert_id(P) -> emysql:insert_id(P).
result_type(R) -> emysql:result_type(R).

as_dict(Res) -> emysql:as_dict(Res).
as_json(Res) -> emysql:as_json(Res).
as_proplist(Res) -> emysql:as_proplist(Res).
as_record(Res, RecName, Fields) -> emysql:as_record(Res, RecName, Fields).
as_record(Res, RecName, Fields, Fun) -> emysql:as_record(Res, RecName, Fields, Fun).

get_pool()->
	?DEF_POOLID.

keep_alive()->
	ok.

%% Escape character that will confuse an SQL engine
escape(S) when is_list(S) ->
    [escape(C) || C <- S];
escape(S) when is_binary(S) ->
    escape(binary_to_list(S));
escape($%) -> "\\%";
escape($_) -> "\\_";
escape($\0) -> "\\0";
escape($\n) -> "\\n";
escape($\t) -> "\\t";
escape($\b) -> "\\b";
escape($\r) -> "\\r";
escape($')  -> "\\'";
escape($")  -> "\\\"";
escape($\\) -> "\\\\";
escape(C)  -> C.

%% 得到数据库表前缀---"zc_hdsg_"
get_tbprefix() ->
	get_conf(tbprefix).

get_logprefix() ->
	get_conf(logprefix).

get_r_conf()->
	PoolId = get_r_pool_id(),
	PoolSize = get_conf(r_pool_size),
	ReadHost = get_conf(r_host),
	ReadPort = get_conf(r_port),
	ReadUser = get_conf(r_user),
	ReadPwd = get_conf(r_pwd),
	ReadDB = get_conf(r_database),
	Encoding = get_conf(r_encoding),
	RRunNode = get_conf(r_runnode),
	[PoolId, PoolSize, ReadHost, ReadPort, ReadUser, ReadPwd, ReadDB,Encoding,RRunNode].

-compile({inline,[get_r_pool_id/0]}).
get_r_pool_id() ->
    ?READ_POOL_ID.

get_w_conf()->
	PoolId = get_w_pool_id(),
	PoolSize = get_conf(w_pool_size),
	WHost = get_conf(w_host),
	WPort = get_conf(w_port),
	WUser = get_conf(w_user),
	WPwd = get_conf(w_pwd),
	WDB = get_conf(w_database),
	Encoding = get_conf(w_encoding),
	WRunNode = get_conf(w_runnode),
	[PoolId, PoolSize, WHost, WPort, WUser, WPwd, WDB,Encoding,WRunNode].

-compile({inline,[get_w_pool_id/0]}).
get_w_pool_id() ->
	?WRITE_POOL_ID.

get_log_conf()->
	PoolId = get_log_pool_id(),
	PoolSize = get_conf(log_pool_size),
	LogHost = get_conf(log_host),
	LogPort = get_conf(log_port),
	LogUser = get_conf(log_user),
	LogPwd = get_conf(log_pwd),
	LogDB = get_conf(log_database),
	Encoding = get_conf(log_encoding),
	LogRunNode = get_conf(log_runnode),
	[PoolId, PoolSize, LogHost, LogPort, LogUser, LogPwd, LogDB,Encoding,LogRunNode].

-compile({inline,[get_log_pool_id/0]}).
get_log_pool_id() ->
	?LOG_POOL_ID.

get_conf(Key) when is_atom(Key)->
	Configs = application:get_env(morning, mysql,[]),
	case lists:keyfind(Key, 1, Configs) of
		false->
			[];
		{_, Val}-> 
			Val
	end.

get_node_by_poolid(PoolId) ->
	Nodes = 
		case PoolId of
			?WRITE_POOL_ID -> get_conf(w_runnode);
			?READ_POOL_ID -> get_conf(r_runnode);
			?LOG_POOL_ID -> get_conf(log_runnode);
			_ -> [db]
		end,
	case Nodes of
		[Node | _] -> get_db_node(Node);
		_ -> get_db_node(db)
	end.

get_db_node(RunNode) ->
	lists:foldl(fun(Node, Acc)->
						Index = string:str(atom_to_list(Node), atom_to_list(RunNode)),
						if Index =/= 0 ->
							   Node;
						   true ->
							   Acc
						end
				end, [], node_util:get_all_nodes()).
