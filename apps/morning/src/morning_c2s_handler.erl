% /*
%  * @Author: mrchenxinbo 
%  * @Date: 2023-12-26 15:32:57 
%  * @Last Modified by:   mrchenxinbo 
%  * @Last Modified time: 2023-12-26 15:32:57 
%  */
%%%-------------------------------------------------------------------
%% @doc morning public API
%% @end
%%%-------------------------------------------------------------------

-module(morning_c2s_handler).

-export([handle/2, login_request_other/2, user_login_or_register/3]).
-export([test_mission/3]).

-define(TYPE_PASSWORD,3).
-define(TYPE_WX,2).
-define(TYPE_DY,1).

-include("pb_ClientCmdConstants.hrl").
-include("pb_Login.hrl").
-include("pb_Mission.hrl").
-include("logger.hrl").
-include("morning.hrl").
-include("model_def.hrl").

%%login
handle(_Uid, #'LoginReq'{account = UserBinary, loginPass = PasswordBinary, loginType = LoginType}=Info)->
    ?INFO_MSG("32323232=====~p~n", [Info]),
    Return = user_login_or_register(UserBinary, PasswordBinary, LoginType),
    case Return of
        {ok, {Uid, RToken, RExpire}}->
            Res = #'LoginResp'{uid = Uid, token = RToken, expire_in = RExpire},
            {ok, make_login_stage_info(Res, Uid)};
        {error, ErrorStatus} ->
            {error, ErrorStatus}
    end;

%%Mission
handle(Uid, #'MissionReq'{mission = Mission, score = Score}=Info)->
    Res = 
    case model_role_stage_info:read(Uid) of
        [#role_stage_info{max_score = Max_score}]->
            Updatefields = [{mission, Mission},{max_score, max(Score, Max_score)}, {update_ts, time_util:erlang_system_time(seconds)}],
            model_role_stage_info:update_fields(Updatefields, [{uid, '=', Uid}]),
            #'MissionResp'{mission=Mission, score=Score, max_score= max(Score, Max_score)};
        _->
            Record = #role_stage_info{uid=Uid, mission=Mission, score=Score, max_score = Score, update_ts=time_util:erlang_system_time(seconds)},
            model_role_stage_info:insert(Record),
            #'MissionResp'{mission=Mission, score=Score, max_score= Score}
    end,
    {ok, Res};

handle(_Uid, Proto)->
    {error, 'ERROR_UNKNOW_HANDLER'}.


make_login_stage_info(Res, Uid)->
    case model_role_stage_info:read(Uid) of
        [#role_stage_info{mission=Mission, max_score = Max_score}]->
            Res#'LoginResp'{mission=Mission, max_score = Max_score};
        _->
            Res#'LoginResp'{mission=0, max_score = 0}
    end.














test_mission(Uid,M,S)->
    R= #'MissionReq'{mission = M, score = S},
    handle(Uid,R).


user_login_or_register(User, Password, Type)->
    case Type of
        ?TYPE_PASSWORD ->
            case morning_db_user:user_info_password(util:to_list(User), Type) of
                {ok, {UidB, Password}}->
                    morning_db_user:user_info_update_by_uid(UidB),
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