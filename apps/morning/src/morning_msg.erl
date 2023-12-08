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

-include("pb_messagebody.hrl").

%%=================api===========================
decode_login(Binary)->
    decode_do(pb_messagebody, 'LoginReq', Binary).
encode_login(Data)->
    decode_do(pb_messagebody, 'LoginReq', Data).
    






%%=================intenal fun===================

decode_do(Module, Proto, Data)->
    Module:decode_msg(Data, Proto).

encode_do(Module, Proto, Data)->
    Module:encode_msg(Data, Proto).

