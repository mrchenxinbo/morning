%%%-------------------------------------------------------------------
%% @doc morning top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(morning_tool).

-compile(export_all).


%%=================api===========================
get_state(Pid) ->
    get_state(Pid, 5000).

get_state(Pid, Timeout) ->
    PidReal = ensure_pid(Pid),
    sys:get_state(PidReal, Timeout).

%%=================intenal fun============
ensure_pid(List) when is_list(List) ->
    list_to_pid(List);
ensure_pid(Pid) when is_pid(Pid) ->
    Pid;
ensure_pid(Bin) when is_binary(Bin) ->
    ensure_pid(binary_to_list(Bin)).