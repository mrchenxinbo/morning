%%%-------------------------------------------------------------------
%% @doc morning public API
%% @end
%%%-------------------------------------------------------------------

-module(morning_api_handler).


-export([handle/1, login_request_other/2, user_login_or_register/3]).

-define(TYPE_PASSWORD,3).
-define(TYPE_WX,2).
-define(TYPE_DY,1).

-include("pb_ClientCmdConstants.hrl").
-include("pb_Login.hrl").
-include("logger.hrl").
-include("morning.hrl").

handle(#'LoginReq'{account = UserBinary, loginPass = PasswordBinary, loginType = LoginType}=Info)->
    ?INFO_MSG("32323232=====~p~n", [Info]),
    Return = user_login_or_register(UserBinary, PasswordBinary, LoginType),
    case Return of
        {ok, {Uid, RToken, RExpire}}->
            {ok, #'LoginResp'{uid = Uid, token = RToken, expire_in = RExpire}};
        {error, ErrorStatus} ->
            {error, ErrorStatus}
    end;
handle(Proto)->
    {error, 'ERROR_UNKNOW_HANDLER'}.


user_login_or_register(User, Password, Type)->
    case Type of
        ?TYPE_PASSWORD ->
            case morning_db_user:user_info_password(util:to_list(User), Type) of
                {ok, {UidB, Password}}->
                    morning_db_user:user_info_update_by_uid(util:to_list(User)),
                    {Token, Expire} = morning_token:get_token(UidB, Type),        
                    {ok, {UidB, Token, Expire}};
                {error, not_register}->
                    {ok, UidB} = morning_db_user:user_info_write("", util:to_list(Password), util:to_list(User), util:to_list(Type)),
                    {Token, Expire} = morning_token:get_token(UidB, Type),        
                    {ok, {UidB, Token, Expire}};
                _->
                    {error, 'ERROR_SERVER'}
            end;
        ?TYPE_WX->
            case login_request_other(?TYPE_WX, {util:to_list(Password)}) of
                {ok, #{<<"session_key">> := Session_key, <<"unionid">> := Unionid}}->
                    case morning_db_user:user_info_read_by_unionid(util:to_list(Unionid), Type) of
                        {ok, #user_info{uid = UidB, nickname = Nickname, unionid = Unionid, channel = Channel}} ->
                            morning_db_user:user_info_update_by_uid(UidB),
                            {Token, Expire} = morning_token:get_token(UidB, Type),        
                            {ok, {UidB, Token, Expire}};
                        {ok, []}->
                            {ok, UidB} = morning_db_user:user_info_write("", "", util:to_list(Unionid), util:to_list(Type)),
                            {Token, Expire} = morning_token:get_token(UidB, Type),        
                            {ok, {UidB, Token, Expire}};
                        {error, _}->
                            {error, 'ERROR_SERVER'}     
                    end;
                _->
                    {error, 'ERROR_SERVER'} 
            end;
        ?TYPE_DY->
            {error, 'ERROR_UNKNOW_HANDLER'};
        _->
            {error, 'ERROR_PARAMA'}
    end.









% WX  code2Session API
login_request_other(?TYPE_WX, {Code})->
    Appid = "wx20d4e73ce97f2c32",
    Secret = "594f2734780923281629f8eacbee32d2",
    Url = "https://api.weixin.qq.com/sns/jscode2session?",
    Pramas = ["appid", "=", Appid, "&", "secret", "=", Secret, "&", "js_code", "=", Code, "&", "grant_type", "=", "authorization_code"], 
    Url1 = Url++lists:concat(Pramas),
    Timeout = 5000,
    case ibrowse:send_req(Url1, [{"content-type", "application/json"}], get, [], [], Timeout) of
        {ok, "200", _Headers, JsonStr} ->
            Map = jsx:decode(iolist_to_binary(JsonStr), [{return_maps, true}]), 
            ?INFO_MSG("login_request_other INFO ~p~n", [Map]),
            case maps:get(<<"errcode">>, Map, not_found) of
                not_found->
                    {ok, Map};
                _->
                    {error, maps:get(<<"errmsg">>, Map, <<>>)}  
            end;
        Other->
            ?ERROR_MSG("login_request_other Error ~p~n", [Other]),
            {error, <<"request exception ">>}
    end;
login_request_other(_Type, _Pramas)->
    {error, <<"error paramas">>}.