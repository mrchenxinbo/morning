% /*
%  * @Author: mrchenxinbo 
%  * @Date: 2023-12-04 17:04:23 
%  * @Last Modified by:   mrchenxinbo 
%  * @Last Modified time: 2023-12-04 17:04:23 
%  */
%%%-------------------------------------------------------------------
%% @doc morning top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(morning_db_mysql).

-export([config_init/0, get_worker/0,query/1]).
-include("logger.hrl").

config_init() ->
    {ok, MysqlParmas} = application:get_env(morning, mysql),
    Host = proplists:get_value(host, MysqlParmas),
    DataBase = proplists:get_value(database, MysqlParmas),
    User = proplists:get_value(user, MysqlParmas),
    Password = proplists:get_value(password, MysqlParmas),
    Port = proplists:get_value(port, MysqlParmas),
    QuerryTimeout = proplists:get_value(query_timeout, MysqlParmas),
    PoolSize = proplists:get_value(pool_size, MysqlParmas),
    Config =
        [{host, Host},
         {database, DataBase},
         {user, User},
         {password, Password},
         {port, Port},
         {query_timeout, QuerryTimeout}],
    config_init_spec(PoolSize, Config).

config_init_spec(PoolSize, Config) ->
    Opts = [db_mysql, PoolSize, [mysql], {mysql, start_link, [Config]}],
    {db_mysql, {cuesport, start_link, Opts}, permanent, infinity, supervisor, [mysql]}.

get_worker()->
    morning_db_mysql_sup:get_random_pid(<<"db_mysql">>).   

query(Sql)->
    case mysql:query(get_worker(), list_to_binary(Sql)) of
        ok->
            {ok, []};
        {ok, _Query, Result}->
            {ok, Result};    
        {error, _, _}->
            {error, db_error};
        {error, ERROR}->
            ?ERROR_MSG("db sql querry error ==:~p~n", [ERROR]),
            {error, db_error}
    end.





