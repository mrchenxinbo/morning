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

-include("logger.hrl").
-include("pb_ClientCmdConstants.hrl").

%%%===================================================================
%%% API
%%%===================================================================

paths() ->
    [{"/api/users/:uid/:cmd", ?MODULE, []},
     {"/api/users/user_login/:cmd", ?MODULE, []}
     ].
init(_Type, Req, []) ->
    {ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
    ok.

load()->
    http_service_dispatch:add_routers(paths()),
    http_service_dispatch:reload().

handle(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    {<<"/api/", Path/binary>>, _} = cowboy_req:path(Req),
    ?INFO_MSG("handle http request Req=~p~n",
                            [Req1]),
    Req2 =
        try
            {ok, R} = handle_request(Req1, Method, binary:split(Path, <<"/">>, [global])),
            R
        catch
            Type:Error ->
                ?ERROR_MSG("handle http request Type=~p, Error=~p, S=~p",
                            [Type, Error, erlang:get_stacktrace()]),
                Decodedata1 = morning_msg:packet_http_data('ERROR_SERVER', <<>>),
                http_reply(Req, Decodedata1)
        end,
   {ok, Req2, State}.


handle_request(Req, <<"POST">>, [<<"users">>, <<"user_login">>, CMD])->
    {ok, Data, _} = cowboy_req:body(Req),
    case morning_api_handler:handle(morning_msg:decode_msg(binary_to_integer(CMD), Data)) of
        {ok, R}->
            Decodedata = morning_msg:encode_msg(binary_to_integer(CMD), R), 
            Decodedata1 = morning_msg:packet_http_data('OK', Decodedata),
            http_reply(Req, Decodedata1);
        {error, ErrStatus}->
            Decodedata1 = morning_msg:packet_http_data(ErrStatus, <<>>),
            http_reply(Req, Decodedata1)
    end;
    
handle_request(Req, <<"POST">>, [<<"users">>, UserId, CMD] = Path)->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
         {ok, {_, HttpToken}, _}->
            case morning_token:check_token(UserId, HttpToken) of
                true->


                    {ok, Data, _} = cowboy_req:body(Req),
                    case morning_api_handler:handle(morning_msg:decode_msg(binary_to_integer(CMD), Data)) of
                        {ok, R}->
                            Decodedata = morning_msg:encode_msg(binary_to_integer(CMD), R), 
                            Decodedata1 = morning_msg:packet_http_data('OK', Decodedata),
                            http_reply(Req, Decodedata1);
                        {error, ErrStatus}->
                            Decodedata1 = morning_msg:packet_http_data(ErrStatus, <<>>),
                            http_reply(Req, Decodedata1)
                    end;
                {error, ErrStatus}->
                    Decodedata1 = morning_msg:packet_http_data(ErrStatus, <<>>),
                    http_reply(Req, Decodedata1)
            end;
        _->
            Decodedata1 = morning_msg:packet_http_data('ERROR_PARAMA', <<>>),
            http_reply(Req, Decodedata1)
    end;

handle_request(Req, Method, Path)->
    Decodedata1 = morning_msg:packet_http_data('ERROR_PARAMA', <<>>),
    http_reply(Req, Decodedata1).





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
    [{<<"Content-Type">>, <<"application/x-protobuf">>}],
    Data,
    Req).

check_token(User, Token)->
    true.
