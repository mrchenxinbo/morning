-module(morning_redis).

-export([q/1, q/2, q/3, qp/2, qp/3, qp_n_check/2, qp_n/2, q_noreply/2]).

-include("logger.hrl").

-export([q_do/2, q_do/3, qp_do/2, qp_do/3, q_noreply_do/2]).

-define(DEFAULT_TIMEOUT, 5000).

qp_n_check(Name, QP) ->
    QPRet = filter_queries(QP),
    qp_n(Name, QPRet).

qp_n(_Name, []) ->
    [];
qp_n(Name, P) when erlang:length(P) < 1000 ->
    qp(Name, P);
qp_n(Name, P) ->
    QueriesList = utils:partition_n(P, 1000),
    lists:flatmap(fun(Queries) -> qp(Name, Queries) end, QueriesList).
q(Q) ->
    q(morning_redis, Q).

q(Name, Q) ->
    {Time, Value} = timer:tc(?MODULE, q_do, [Name, Q]),
    Value.

q_do(Name, Q) ->
    try request_q(Name, Q) of
        {error, no_connection} ->
            ?ERROR_MSG("redis op: input = ~p, error reason:~p", [Q, no_connection]),
            {error, no_connection};
        {error, Reason} ->
            ?ERROR_MSG("redis op: input = ~p, error reason:~p", [Q, Reason]),
            {error, Reason};
        {ok, Value} ->
            ?DEBUG("redis op: ~p => ~p~n", [Q, Value]),
            {ok, Value}
    catch
        Class:Exception ->
            ?ERROR_MSG("redis op ~p:~p input = ~w~n       stack = ~p~n",
                       [Class, Exception, Q, erlang:get_stacktrace()]),
            {error, {Class, Exception}}
    end.

q(Name, Q, TimeOut) ->
    {Time, Value} = timer:tc(?MODULE, q_do, [Name, Q, TimeOut]),
    Value.


q_do(Name, Q, TimeOut) ->
    try request_q(Name, Q, TimeOut) of
        {error, no_connection} ->
            ?ERROR_MSG("redis op: input = ~p, error reason:~p", [Q, no_connection]),
            {error, no_connection};
        {error, Reason} ->
            ?ERROR_MSG("redis op: input = ~p, error reason:~p", [Q, Reason]),
            {error, Reason};
        {ok, Value} ->
            ?DEBUG("redis op: ~p => ~p~n", [Q, Value]),
            {ok, Value}
    catch
        Class:Exception ->
            ?ERROR_MSG("redis op ~p:~p input = ~w~n       stack = ~p~n",
                       [Class, Exception, Q, erlang:get_stacktrace()]),
            {error, {Class, Exception}}
    end.

qp(Name, []) ->
    [];
qp(Name, Q) ->
    {Time, Value} = timer:tc(?MODULE, qp_do, [Name, Q]),
    Value.

qp_do(Name, QP) ->
    try request_qp(Name, QP) of
        RetList when is_list(RetList) ->
            qp_check(QP, RetList),
            RetList;
        {error, no_connection} ->
            ?ERROR_MSG("redis op: input = ~w, error reason:~w", [QP, no_connection]),
            {error, no_connection};
        {error, Reason} ->
            ?ERROR_MSG("redis op: input = ~w, error reason:~w", [QP, Reason]),
            {error, Reason}
    catch
        Class:Exception ->
            ?ERROR_MSG("redis op ~p:~p input = ~w~n       stack = ~p~n",
                       [Class, Exception, QP, erlang:get_stacktrace()]),
            {error, {Class, Exception}}
    end.

qp(Name, [], TimeOut) ->
    [];
qp(Name, Q, TimeOut) ->
    {Time, Value} = timer:tc(?MODULE, qp_do, [Name, Q, TimeOut]),
    Value.

qp_do(Name, QP, TimeOut) ->
    try request_qp(Name, QP, TimeOut) of
        RetList when is_list(RetList) ->
            qp_check(QP, RetList),
            RetList;
        {error, no_connection} ->
            ?ERROR_MSG("redis op: input = ~w, error reason:~w", [QP, no_connection]),
            {error, no_connection};
        {error, Reason} ->
            ?ERROR_MSG("redis op: input = ~w, error reason:~w", [QP, Reason]),
            {error, Reason}
    catch
        Class:Exception ->
            ?ERROR_MSG("redis op ~p:~p input = ~w~n       stack = ~p~n",
                       [Class, Exception, QP, erlang:get_stacktrace()]),
            {error, {Class, Exception}}
    end.

q_noreply(Name, Q) ->
    {Time, Value} = timer:tc(?MODULE, q_noreply_do, [Name, Q]),
    Value.

q_noreply_do(Name, Q) ->
    request_q_noreply(Name, Q),
    ?DEBUG("redis op : ~p without waiting reply and ignore any errors~n", [Q]),
    ok.

qp_check(QP, RetList) ->
    lists:foreach(fun({Q, Ret}) ->
                     case Ret of
                         {error, no_connection} ->
                             ?ERROR_MSG("error reason:no_connection, input = ~p~n", [Q]),
                             {error, no_connection};
                         {error, Reason} ->
                             ?ERROR_MSG("error reason:~p, Q = ~p~n", [Q, Reason]),
                             {error, Reason};
                         {ok, Value} ->
                             ?DEBUG("redis op: ~p => ~p~n", [Q, Value]),
                             {ok, Value}
                     end
                  end,
                  lists:zip(QP, RetList)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
filter_queries(PList) ->
    lists:filter(fun(P) -> P /= [] end, PList).

% maybe_double_write_q(Name, QP, RetList) ->
%     ProcessModule = application:get_env(message_store, double_process_module, undefined),
%     case ProcessModule of
%         undefined ->
%             ok;
%         _ ->
%             spawn(fun() -> ProcessModule:process_rules(Name, QP, RetList) end)
%     end.

% maybe_double_write_qp(Name, QP, RetList) ->
%     ProcessModule = application:get_env(message_store, double_process_module, undefined),
%     case ProcessModule of
%         undefined ->
%             ok;
%         _ ->
%             spawn(fun() ->
%                      {FilterQP, FilterRetList} = filter_bad_queries(QP, RetList),
%                      ProcessModule:process_rules(Name, FilterQP, FilterRetList)
%                   end)
%     end.

filter_bad_queries(PList, RList) ->
    PRList = lists:zip(PList, RList),
    FilterPRList = [{Query, Value} || {Query, {ok, Value}} <- PRList],
    lists:unzip(FilterPRList).

request_q(Name, Q) ->
    request_q(Name, Q, util:get_redis_timeout(Name)).

request_q(Name, Q, TimeOut) ->
    Client = cuesport:get_worker(Name),
    Result = eredis:q(Client, Q, TimeOut),
    case Result of
        {error, no_connection} ->
            ignore;
        {error, <<"MOVED ", _/binary>>} ->
            %% reconnect
            Client ! {tcp_closed, fake_socket};
        {error, <<"ASK ", _/binary>>} ->
            Client ! {tcp_closed, fake_socket};
        {error, _Other} ->
            Client ! {tcp_closed, fake_socket};
        _Result ->
            ignore
    end,
    Result.

request_qp(Name, Q) ->
    request_qp(Name, Q, util:get_redis_timeout(Name)).

request_qp(Name, Q, TimeOut) ->
    Client = cuesport:get_worker(Name),
    eredis:qp(Client, Q, TimeOut).

request_q_noreply(Name, Q) ->
    Client = cuesport:get_worker(Name),
    eredis:q_noreply(Client, Q).

get_orig_op([OrigOp | _]) when is_atom(OrigOp) ->
    OrigOp;
get_orig_op([OrigOp | _]) ->
    erlang:list_to_atom(OrigOp).
