-module(proto_item_config).
-include("model_def.hrl").
-include("ets_file.hrl").
-define(ITEM_CONFIG_ETS, ets_item_config).

-compile(export_all).
-export([create/0, init/0]).
-behaviour(ets_operater_mod).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% behavior ets_operater_mod callback functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create()->
	?NEW_ETS(?ITEM_CONFIG_ETS, [named_table, set]).

init()->
	?INIT_ETS(item_config, ?ITEM_CONFIG_ETS, [#item_config.id], select_all()).

select_all() ->
	AllCount = mysql_client:select_count(item_config, id),
	Columns = "`id`,`type`,`icon`",
	PageSize = ?SELECT_PAGE_SIZE,
	RecordList = if
					AllCount > PageSize ->
						Pages = erlang:max(1, math_util:even_div(AllCount, PageSize)),
						lists:foldl(fun(Page, AccTmp) ->
											Where =  mysql_helper:pack_orderby({id, asc}) ++ mysql_helper:pack_limit({(Page-1) * PageSize, PageSize}) ++ ";",
											case mysql_client:select_columns(item_config, Columns, Where) of
												{ok, RowList}->
													AccTmp ++ unpack_row(RowList, []);
												_ ->
													AccTmp
											end
									end, [], lists:seq(1, Pages));
					true ->
						case mysql_client:select_columns(item_config, Columns, ";") of
							{ok, RowList}->
								unpack_row(RowList, []);
							_ ->
								[]
						end
				end,
	record_to_float(RecordList).

unpack_row([], Records)->
    lists:reverse(Records);
unpack_row([Row|Left], Records)->
    Record = mysql_helper:unpack_row(item_config, Row),
    unpack_row(Left,
				[Record#item_config{
					
	    		}|Records]).

record_to_float(RecordList) ->
	case lists:keyfind(item_config,1,?TO_FLOAT_RECORD_LIST) of
		false ->
			RecordList;
		{_,FloatFields} ->
			FieldList = record_info(fields,item_config),
			FieldIndex = lists:zip(FieldList,lists:seq(2,length(FieldList) + 1)),
			lists:foldr(fun(Record,Acc)->
						NewRecord = lists:foldl(fun(Field,Acc1)->
											{_,Index} = lists:keyfind(Field,1,FieldIndex),
											OldValue = element(Index,Acc1),
											case string:to_float(OldValue) of
												{error,_} ->
													{NewValue,[]} = string:to_integer(OldValue); 
												{NewValue,[]} ->
													nothing
											end,
											setelement(Index,Acc1,NewValue)
										end,Record,FloatFields),
						[NewRecord|Acc]
					end,[],RecordList)
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% record operation functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get(ID)->
	case ets:lookup(?GET_ENABLE_TABLE(?ITEM_CONFIG_ETS),ID) of
		[]->[];
		[{_,Info}|_]->Info
	end.

foldl(Fun, Acc)->
	ets:foldl(Fun, Acc, ?GET_ENABLE_TABLE(?ITEM_CONFIG_ETS)).

ms_select(MatchSpec) ->
	ets:select(?GET_ENABLE_TABLE(?ITEM_CONFIG_ETS), MatchSpec).

all()->
	ets:tab2list(?GET_ENABLE_TABLE(?ITEM_CONFIG_ETS)).

get_id(Record)->
	Record#item_config.id.

get_type(Record)->
	Record#item_config.type.

get_icon(Record)->
	Record#item_config.icon.


