% /*
%  * @Author: mrchenxinbo 
%  * @Date: 2023-12-08 12:00:17 
%  * @Last Modified by:   mrchenxinbo 
%  * @Last Modified time: 2023-12-08 12:00:17 
%  */
%%%-------------------------------------------------------------------
%% @doc morning top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(morning_msg).

-compile(export_all).

-include("pb_ClientCmdConstants.hrl").
-include("pb_Login.hrl").

%%=================api===========================
decode_msg(CmdValue, Binary)->
    CmdSymbol = cmd_value_to_symbol(CmdValue),
    decode(CmdSymbol, Binary).

encode_msg(CmdValue, Data) when is_integer(CmdValue)->
    CmdSymbol = cmd_value_to_symbol(CmdValue),
    encode(CmdSymbol, Data);
encode_msg(CmdSymbol, Data)->
    encode(CmdSymbol, Data).


%%============decode info  start=====================    
decode('LOGIN_MSG', Binary)->
    decode_do(pb_Login, 'LoginReq', Binary).



%%============encode info  start=====================
encode('LOGIN_MSG', Data)->
    encode_do(pb_Login, 'LoginResp', Data).









%%=================intenal fun===================
decode_do(Module, Proto, Data)->
    Module:decode_msg(Data, Proto).

encode_do(Module, Proto, Data)->
    Module:encode_msg(Data, Proto).

packet_http_data(Status, Binary)->
    Value = pb_ClientCmdConstants:enum_value_by_symbol_StatusCode(Status),
    <<Value:32, Binary/binary>>.

cmd_symbol_to_value(Symbol)->
    pb_ClientCmdConstants:enum_value_by_symbol_ClientCmd(Symbol).

cmd_value_to_symbol(Value)->
    pb_ClientCmdConstants:enum_symbol_by_value_ClientCmd(Value).

test(Json)->
    Data = jsx:encode(Json),
    {ok, File} = file:open("output.json", [write]),
    io:format(File, "~s", [Data]),
    file:close(File).
    


