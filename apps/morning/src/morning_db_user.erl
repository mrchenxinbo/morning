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

-module(morning_db_user).

-export([user_info_read_by_uid/1, user_info_read_by_unionid/1, user_info_write/4, user_info_del_by_uid/1, user_info_update_by_uid/1,user_info_password/1]).

-include("morning.hrl").


user_info_read_by_uid(Uid)->
    Sql = "SELECT uid, nickname, unionid, channel, create_ts, update_ts FROM morning_user_info WHERE uid = "++Uid, 
    case morning_db_mysql:querry(Sql) of
        {ok, [[Uid, Nickname, Unionid, Channel, Create_ts, Update_ts]]}->
            UserInfo = #user_info{uid = integer_to_binary(Uid), nickname = Nickname, unionid = Unionid, channel = Channel},
            {ok, UserInfo};
        {ok, [[]]}->
            {ok, []};
        {error, Error}->
            {error, Error}
    end.

user_info_read_by_unionid(Unionid)->
    Sql = "SELECT uid, nickname, unionid, channel, create_ts, update_ts FROM morning_user_info WHERE unionid = "++Unionid, 
    case morning_db_mysql:querry(Sql) of
        {ok, [[Uid, Nickname, Unionid, Channel, Create_ts, Update_ts]]}->
            UserInfo = #user_info{uid = integer_to_binary(Uid), nickname = Nickname, unionid = Unionid, channel = Channel},
            {ok, UserInfo};
        {ok, [[]]}->
            {ok, []};
        {error, Error}->
            {error, Error}
    end.

user_info_write(Nickname, Password, Unionid, Channel)->
    Sql =
    "INSERT INTO morning_user_info (nickname, password, unionid, channel, create_ts, update_ts) VALUES('"++Nickname++"','"++Password++"','"++Unionid++"',"++Channel++","++util:to_list(time_util:erlang_system_time(seconds))++","++ util:to_list(time_util:erlang_system_time(seconds))++"); SELECT LAST_INSERT_ID();",
    case morning_db_mysql:querry(Sql) of
        {ok, [[Uid]]}->
            {ok, integer_to_binary(Uid)};
        _->
            {error, <<>>}   
    end.


user_info_del_by_uid(Uid)->
    Sql = "DELETE FROM morning_user_info WHERE uid="++Uid++";",
    morning_db_mysql:querry(Sql).

user_info_update_by_uid(Uid)->
    Sql = "UPDATE morning_user_info SET update_ts="++util:to_list(time_util:erlang_system_time(seconds))++" WHERE uid="++Uid++";",
    morning_db_mysql:querry(Sql).

user_info_password(Uid)->
    Sql = "SELECT uid, password FROM morning_user_info WHERE uid = "++Uid, 
    case morning_db_mysql:querry(Sql) of
        {ok, [[UidB, P]]}->
            {ok, {integer_to_binary(UidB), P}};
        {ok, []}->
            {error, not_register};
        _->
            {error, not_password}
    end.






