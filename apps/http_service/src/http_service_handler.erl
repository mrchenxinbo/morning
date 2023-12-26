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
-include("pb_Login.hrl").
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
                            Encodedata = morning_msg:packet_http_data('ERROR_SERVER', <<>>),
                http_reply(Req, Encodedata)
        end,
   {ok, Req2, State}.


handle_request(Req, <<"POST">>, [<<"users">>, <<"user_login">>, CMD])->
    {ok, Data, _} = cowboy_req:body(Req),
    InitData = 
    case cowboy_req:parse_header(<<"content-type">>, Req) of
        {ok, {<<"application">>,<<"x-protobuf">>,[]},  _Req}->
            Data;
        {ok, {<<"application">>,<<"x-protobuf-wx">>,[]},  _Req}-> 
            parse_wx(Data);
        _->
            Data
    end,
    ?INFO_MSG("handle login InitDataInitData=======~p~n", [InitData]),
    DecodeData = morning_msg:decode_msg(binary_to_integer(CMD), InitData),
    case is_record(DecodeData, 'LoginReq') of
         true->
            case morning_c2s_handler:handle(undefined, DecodeData) of
                {ok, R}->
                    ?INFO_MSG("handle login sucess=======~p~n", [R]),
                    Encodedata = morning_msg:encode_msg(binary_to_integer(CMD), R), 
                    Encodedata1 = morning_msg:packet_http_data('OK', Encodedata),
                    http_reply(Req, Encodedata1);
                {error, ErrStatus}->
                    ?ERROR_MSG("handle login error=======error_code: ~p re_data:~p~n", [ErrStatus, DecodeData]),
                    Encodedata1 = morning_msg:packet_http_data(ErrStatus, <<>>),
                    http_reply(Req, Encodedata1)
            end;
        false->
            Encodedata1 = morning_msg:packet_http_data('ERROR_PARAMA', <<>>),
            http_reply(Req, Encodedata1)
    end;
    
handle_request(Req, <<"POST">>, [<<"users">>, UserId, CMD] = Path)->
    case cowboy_req:parse_header(<<"authorization">>, Req) of
         {ok, {_, HttpToken}, _}->
            ?INFO_MSG("handle check token =======UserId:~p~n", [{UserId, CMD}]),
            case morning_token:check_token(UserId, HttpToken) of
                true->
                    ?INFO_MSG("handle check token sucess =======UserId:~p~n", [{UserId, CMD}]),
                    {ok, Data, _} = cowboy_req:body(Req),
                    InitData = 
                    case cowboy_req:parse_header(<<"content-type">>, Req) of
                        {ok, {<<"application">>,<<"x-protobuf">>,[]},  _Req}->
                            Data;
                        {ok, {<<"application">>,<<"x-protobuf-wx">>,[]},  _Req}-> 
                            parse_wx(Data);
                        _->
                            Data
                    end,
                    case morning_c2s_handler:handle(binary_to_integer(UserId), morning_msg:decode_msg(binary_to_integer(CMD), InitData)) of
                        {ok, R}->
                            ?INFO_MSG("handle req  sucess=======UserId:~p~n", [{UserId, CMD}]),
                            Encodedata = morning_msg:encode_msg(binary_to_integer(CMD), R), 
                            Encodedata1 = morning_msg:packet_http_data('OK', Encodedata),
                            http_reply(Req, Encodedata1);
                        {error, ErrStatus}->
                            ?ERROR_MSG("handle do request fail =======UserId:~p~n", [{UserId, CMD}]),
                            Encodedata = morning_msg:packet_http_data(ErrStatus, <<>>),
                            http_reply(Req, Encodedata)
                    end;
                {error, ErrStatus}->
                    ?ERROR_MSG("handle check token fail =======UserId:~p~n", [{UserId, CMD}]),
                    Encodedata = morning_msg:packet_http_data(ErrStatus, <<>>),
                    http_reply(Req, Encodedata)
            end;
        _->
            ?ERROR_MSG("handle get token fail =======UserId:~p~n", [{UserId, CMD}]),
            Encodedata = morning_msg:packet_http_data('ERROR_PARAMA', <<>>),
            http_reply(Req, Encodedata)
    end;

handle_request(Req, Method, Path)->
    Encodedata = morning_msg:packet_http_data('ERROR_PARAMA', <<>>),
    http_reply(Req, Encodedata).





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

parse_wx(Data)->
    MapData = jsx:decode(Data, [return_maps]),
    Keys = maps:keys(MapData),
    IntegerKeys = lists:sort([binary_to_integer(K)||K<-Keys]),
    SortedKeys = [integer_to_binary(IntK)||IntK<-IntegerKeys],
    Values = [maps:get(Key, MapData) || Key <- SortedKeys],
    Bytes = [Value || Value <- Values],
    list_to_binary(Bytes).
