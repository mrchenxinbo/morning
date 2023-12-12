% /*
%  * @Author: mrchenxinbo 
%  * @Date: 2023-12-11 21:08:20 
%  * @Last Modified by:   mrchenxinbo 
%  * @Last Modified time: 2023-12-11 21:08:20 
%  */
%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------

-module(morning_token).

-export([get_token/2, check_token/2, start/0]).

-define(TOKEN, <<"morning:">>).
-define(STORE_TOKEN, token_ets_cache).
-record(token_info, {uid, token, expire}).
-include("logger.hrl").
-include("pb_ClientCmdConstants.hrl").


start()->
    ets:new(?STORE_TOKEN, [named_table, public, {keypos, #token_info.uid}]).

check_token(UidBinary, Token)->
    Now = time_util:erlang_system_time(seconds),
    case ets:lookup(?STORE_TOKEN, UidBinary) of
        [#token_info{token = Token, expire = Expire}]->
            if
                Now <  Expire->
                    true;
                true ->
                    ?INFO_MSG("token check=timeout====~p~n", [UidBinary]),
                    ets:delete(?STORE_TOKEN, UidBinary),  
                    {error, 'ERROR_TOKEN_TIMEOUT'}
            end;
        [_|_R]->
            ?INFO_MSG("token check= not right====~p~n", [UidBinary]),
            {error, 'ERROR_TOKEN_NOT_TRUE'};
        []->
            Key = get_key(UidBinary),
            case morning_redis:q([get, Key]) of
                {ok, Token}->
                    {ok, ExpireB}= morning_redis:q([ttl, Key]), 
                    Expire = binary_to_integer(ExpireB),
                    if
                        Now < Expire ->
                            TokenInfoR = #token_info{uid=UidBinary, token = Token, expire = binary_to_integer(ExpireB)},
                            ets:insert(?STORE_TOKEN, TokenInfoR),
                            true;
                        true ->
                            ?INFO_MSG("token check=timeout====~p~n", [UidBinary]),
                            {error, 'ERROR_TOKEN_TIMEOUT'}
                    end; 
                _->
                    ?INFO_MSG("token check= null====~p~n", [UidBinary]),
                    {error, 'ERROR_TOKEN_NOT_EXIST'}
            end
    end.
    

get_token(UserBinary, Type)->
    Base = "YWMt39RfMMOqEeKYE_GW7tu81ABCDT71lGijyjG4VUIC2AwZGzUjVbPp_4qRD5k",
    Now = time_util:erlang_system_time(nano_seconds),
    User = util:to_list(UserBinary),
    Token = base64:encode(integer_to_list(Now)++Base++User++integer_to_list(Type)),
    Ttl = application:get_env(morning, token_ttl, 7200),
    TokenKey = get_key(UserBinary),
    
    morning_redis:q([set, TokenKey, Token]),
    morning_redis:q([expire, TokenKey, Ttl]),
    Expire = time_util:erlang_system_time(seconds)+Ttl,

    TokenInfoR = #token_info{uid=UserBinary, token = Token, expire = Expire},
    
    ets:insert(?STORE_TOKEN, TokenInfoR),
    {Token, Expire}.

    
get_key(UserBinary)->
    <<?TOKEN/binary,UserBinary/binary >>.