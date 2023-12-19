%% Copyright (c) 2009
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
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
%% @private
-module(emysql_sup).
-behaviour(supervisor).

-export([start_link/0 ,init/1]).
-export([add_pool/8, start_pool/0, stop_pool/0]).
-include("emysql.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    {ok, {{one_for_one, 10, 10}, [
        {emysql_statements, {emysql_statements, start_link, []}, permanent, 5000, worker, [emysql_statements]},
        {emysql_conn_mgr, {emysql_conn_mgr, start_link, []}, permanent, 5000, worker, [emysql_conn_mgr]}
    ]}}.

add_pool(PoolId, PoolSize, WHost, WPort, WUser, WPwd, WDB, WEncoding)->
    emysql:add_pool(PoolId, PoolSize, WUser, WPwd, WHost, WPort, WDB, WEncoding).

start_pool() ->
    {ok, MysqlParmas} = application:get_env(morning, mysql),
    Host = proplists:get_value(host, MysqlParmas),
    DataBase = proplists:get_value(database, MysqlParmas),
    User = proplists:get_value(user, MysqlParmas),
    Password = proplists:get_value(password, MysqlParmas),
    Port = proplists:get_value(port, MysqlParmas),
    QuerryTimeout = proplists:get_value(query_timeout, MysqlParmas),
    PoolSize = proplists:get_value(pool_size, MysqlParmas),
    % Config =
    %     [{host, Host},
    %     {database, DataBase},
    %     {user, User},
    %     {password, Password},
    %     {port, Port},
    %     {query_timeout, QuerryTimeout}],
    add_pool(?READ_POOL_ID, PoolSize, Host, Port, User, Password, DataBase, utf8),
    add_pool(?WRITE_POOL_ID, PoolSize, Host, Port, User, Password, DataBase, utf8).

stop_pool()->
    lists:foreach(
		   fun (Pool) -> emysql:remove_pool(Pool#pool.pool_id) end,
		   emysql_conn_mgr:pools()).