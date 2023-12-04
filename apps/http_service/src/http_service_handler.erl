% /*
%  * @Author: mrchenxinbo 
%  * @Date: 2023-12-04 11:33:53 
%  * @Last Modified by:   mrchenxinbo 
%  * @Last Modified time: 2023-12-04 11:33:53 
%  */
-module(http_service_handler).

-export([load/0]).

-export([paths/0]).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
%%%===================================================================
%%% API
%%%===================================================================

paths() ->
    [{"/api/users/:uid/:type", ?MODULE, []},
     {"/api/users/user_login/:type", ?MODULE, []}
     ].
init(_Type, Req, []) ->
    {ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
    ok.

load()->
    http_service_dispatch:add_routers(paths()),
    http_service_dispatch:reload().

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {<<"/api/", Path/binary>>, _} = cowboy_req:path(Req),
    Req3 =
        try
            {ok, R} = handle_request(Req2, Method, binary:split(Path, <<"/">>, [global])),
            R
        catch
            Type:Error ->
                % ?ERROR_MSG("handle http request Type=~p, Error=~p, S=~p",
                %             [Type, Error, erlang:get_stacktrace()]),
                
                http_reply_error(Req2, 403, <<"uncath, exception">>)
        end,
    {ok, Req3, State}.


handle_request(Req, <<"POST">>, [<<"users">>, <<"user_login">>, Type])->
    {ok, Data, _} = cowboy_req:body(Req),
    #{<<"user">> := User, <<"password">> := Password} = jsx:decode(Data, [return_maps]), 
    R = morning_api_handler:login({User, Password, Type}),
    http_reply(Req, R);

handle_request(Req, Method, [<<"users">>, UserId|_] = Path)->
    
    case cowboy_req:parse_header(<<"authorization">>, Req) of
         {ok, {_, HttpToken}, _}->
            case check_token(UserId, HttpToken) of
                true->
                    handle_request_do(Req, Method, Path);
                false->
                    http_reply_error(Req, 403, <<"token not right">>)    
            end;
        _->
            http_reply_error(Req, 403, <<"token is null">>)
    end;
handle_request(Req, Method, Path)->
    http_reply_error(Req, 403, <<"request is illeagal">>).





%%=====request_do=====================
handle_request_do(Req, _Method, _Path)->
    http_reply_error(Req, 403, <<"unkown request">>).

http_reply_error(Req, Code, Reason)->
    cowboy_req:reply(Code,
    [{<<"Content-Type">>, <<"application/json">>}],
    jsx:encode([{result, <<"error">>}, {reason, Reason}]),
    Req).

http_reply(Req, Data)->
    cowboy_req:reply(200,
    [{<<"Content-Type">>, <<"application/json">>}],
    jsx:encode(Data),
    Req).

check_token(User, Token)->
    true.