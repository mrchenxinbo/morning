% /*
%  * @Author: mrchenxinbo 
%  * @Date: 2023-12-04 11:33:53 
%  * @Last Modified by:   mrchenxinbo 
%  * @Last Modified time: 2023-12-04 11:33:53 
%  */
-module(morning_template_http_handler).

-export([load/0]).

-export([paths/0]).
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([template_data_to_db/2, template_db_to_data/1]).

-include("logger.hrl").
-include("pb_ClientCmdConstants.hrl").

%%%===================================================================
%%% API
%%%===================================================================

paths() ->
    [{"/template/:data_table", ?MODULE, []}].
init(_Type, Req, []) ->
    {ok, Req, undefined}.

terminate(_Reason, _Req, _State) ->
    ok.

load()->
    http_service_dispatch:add_routers(paths()),
    http_service_dispatch:reload().

handle(Req, State) ->
    {Method, Req1} = cowboy_req:method(Req),
    {<<"/template/", Path/binary>>, _} = cowboy_req:path(Req),
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


handle_request(Req, <<"POST">>, [Table])->
	{ok, Data, _} = cowboy_req:body(Req),
	Maplist = jsx:decode(Data, [return_maps]),
	TableAtom = list_to_atom(binary_to_list(Table)),
	template_data_to_db(TableAtom, Maplist),
	http_reply(Req);
handle_request(Req, <<"GET">>, [Table])->
	TableAtom = list_to_atom(binary_to_list(Table)),
	Data = template_db_to_data(TableAtom),			
	http_reply(Req, Data).









template_data_to_db(TbName, MapList) ->
	mysql_client:delete(TbName, " WHERE `id` > 0;"),
	[M|_]= MapList,
	Columns =
	maps:fold(fun(K, V,Acc1)->
		case Acc1 of
			""->
				"`"++util:to_list(K)++"`";
			_->
				Acc1++" , `"++util:to_list(K)++"`"
		end 
		end, "", M),

	MapOpFun =
	fun(Map)->
		maps:fold(fun(K, V, Acc2)->
			case Acc2 of
				""->
					"'"++util:to_list(V)++"'";
				_->
					Acc2++" , '"++util:to_list(V)++"'"
			end 
		end, "", Map)
	end,
	ValueStrs = 
	lists:foldl(fun(Map, Acc)->
			ValueStr = MapOpFun(Map),
			case Acc of
				""->
					Acc++" ("++ValueStr++") ";
				_->
					Acc++", ("++ValueStr++") "
			end
		end, "", MapList),
	ValueSql = "("++Columns++") VALUES "++ValueStrs++"; ",
	mysql_client:insert(TbName, ValueSql);


template_data_to_db(TbName, [M|Last])->
	template_data_to_db(TbName, M),
	template_data_to_db(TbName, Last);
template_data_to_db(TbName, [])->	
	nothing;
template_data_to_db(TbName, _Other)->
	nothing.

template_db_to_data(TbName)->
	case mysql_client:select_with_fields(TbName, " WHERE `id` > 0;") of
		{ok, Fields, Values}->
			Data =
			lists:map(fun(Value)->
					lists:zip(Fields, Value)
				end, Values),
			% jsx:encode(Data);
			Data;
		error->
			error	
	end.



% [#{<<"id">> =>1, time=>100, type=>3, paramas=>"1,2,3,2", missionid=>2},
% #{<<"id">> =>2, time=>100, type=>3, paramas=>"1,2,3,222", missionid=>33},
% #{<<"id">> =>3, time=>100, type=>3, paramas=>"1,2,3,222", missionid=>2},
% #{<<"id">> =>4, time=>100, type=>3, paramas=>"1,2,3,222", missionid=>2}].

% INSERT INTO morning_one.stage_level_config
% (id, `time`, `type`, paramas, missionid)
% VALUES(0, 0, 0, '', 0);

% INSERT INTO stage_level_config (`id` , `missionid` , `paramas` , `time` , `type`) VALUES (`1` , `2` , `1,2,3,222` , `100` , `3`)


http_reply_error(Req, Code, Reason)->
    cowboy_req:reply(Code,
    [{<<"Content-Type">>, <<"application/json">>}],
    jsx:encode([{result, <<"error">>}, {reason, Reason}]),
    Req).

http_reply(Req)->
    cowboy_req:reply(200,
    [{<<"Content-Type">>, <<"application/json">>}],
    jsx:encode([{result, <<"ok">>}]),
    Req).

http_reply(Req, Data)->
	cowboy_req:reply(200,
	[{<<"Content-Type">>, <<"application/json">>}],
	jsx:encode([{result, <<"ok">>}, {data, Data}]),
	Req).