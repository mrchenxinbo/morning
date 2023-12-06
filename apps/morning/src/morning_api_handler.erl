%%%-------------------------------------------------------------------
%% @doc morning public API
%% @end
%%%-------------------------------------------------------------------

-module(morning_api_handler).


-export([handle/2, login_request_other/2, get_token/2]).

-define(TYPE_PASSWORD,1).
-define(TYPE_WX,2).
-define(TYPE_DY,3).


handle(login, {User, Password, ?TYPE_PASSWORD})->
    {ok, get_token(User, ?TYPE_PASSWORD)};

handle(login, {User, Password, ?TYPE_WX})->
    case login_request_other(?TYPE_WX, {Password}) of
        {ok, R} ->
            {ok, get_token(Password, ?TYPE_WX)};
        {error, Reason}->
            {error, Reason}
    end;
handle(login, {User, Password, Type})->
    login_request_other(Type, Password).


get_token(User, Type)->
    Base = "YWMt39RfMMOqEeKYE_GW7tu81ABCDT71lGijyjG4VUIC2AwZGzUjVbPp_4qRD5k",
    Now = time_util:erlang_system_time(nano_seconds),
    Token = base64:encode(integer_to_list(Now)++Base++User++integer_to_list(Type)),
    Expire = time_util:erlang_system_time(seconds)+3600*12,
    #{token => Token, expire_in => Expire}.
















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
            case maps:get(<<"errcode">>, Map, not_found) of
                not_found->
                    {ok, Map};
                _->
                    {error, maps:get(<<"errmsg">>, Map, <<>>)}  
            end;
        Other->
            {error, <<"request exception ">>}
    end;
login_request_other(_Type, _Pramas)->
    {error, <<"error paramas">>}.