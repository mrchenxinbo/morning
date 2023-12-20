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

-export([user_info_read_by_uid/1, user_info_read_by_unionid/2, user_info_write/4, user_info_del_by_uid/1, user_info_update_by_uid/1,user_info_password/2]).

-include("morning.hrl").
-include("model_def.hrl").


user_info_read_by_uid(Uid)->
    case model_morning_user_info:select([uid, nickname, unionid, channel, create_ts, update_ts], [{uid, '=', Uid}]) of
        {ok, [[UidDb, Nickname, Unionid, Channel, Create_ts, Update_ts]]}->
            UserInfo = #user_info{uid = integer_to_binary(UidDb), nickname = Nickname, unionid = Unionid, channel = Channel},
            {ok, UserInfo};
        {ok, [[]]}->
            {ok, []};
        {error, Error}->
            {error, Error}
    end.

user_info_read_by_unionid(Unionid, Channel)->
    case model_morning_user_info:select([uid, nickname, unionid, channel, create_ts, update_ts], [{unionid, '=', Unionid}, {channel, '=', Channel}]) of
        {ok, [[Uid, Nickname, UnionidB, Channel, Create_ts, Update_ts]]}->
            UserInfo = #user_info{uid = integer_to_binary(Uid), nickname = Nickname, unionid = UnionidB, channel = Channel},
            {ok, UserInfo};
        {ok, []}->
            {ok, []};
        {error, Error}->
            {error, Error}
    end.

user_info_write(Nickname, Password, Unionid, Channel)->
    Record = #morning_user_info{nickname = Nickname, unionid = Unionid, password = Password, channel = Channel, create_ts = time_util:erlang_system_time(seconds), update_ts = time_util:erlang_system_time(seconds)},
    case model_morning_user_info:insert_auto(Record) of
        {ok, _, Uid}->
            {ok, integer_to_binary(Uid)};
        _->
            {error, <<>>}   
    end.


user_info_del_by_uid(Uid)->
    model_morning_user_info:delete([{uid, '=', Uid}]).

user_info_update_by_uid(Uid)->
    model_morning_user_info:update_fields([{update_ts, time_util:erlang_system_time(seconds)}], [{uid, '=', Uid}]).

user_info_password(Unionid, Channel)->
    case model_morning_user_info:select([uid, password], [{unionid, '=', Unionid}, {channel, '=', Channel}]) of
        {ok, [[UidB, P]]}->
            {ok, {integer_to_binary(UidB), P}};
        {ok, []}->
            {error, not_register};
        _->
            {error, not_password}
    end.






