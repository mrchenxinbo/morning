% /*
%  * @Author: mrchenxinbo 
%  * @Date: 2023-12-04 17:04:23 
%  * @Last Modified by:   mrchenxinbo 
%  * @Last Modified time: 2023-12-04 17:04:23 
%  */
%%%-------------------------------------------------------------------
%% @doc morning top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(morning_db_mysql_sup).

%% API
-export([start_link/0, init/1, start/0, stop/0]).
-export([get_pid/1, get_random_pid/1]).

% -include("logger.hrl").

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpec = [morning_db_mysql:config_init()],
    SupFlags = {one_for_one, erlang:length(ChildSpec) * 10, 1},
    % ?DEBUG("ChildSpec:~p ", [ChildSpec]),
    {ok, {SupFlags, ChildSpec}}.

stop() ->
    supervisor:terminate_child(morning_db_mysql_sup, db_mysql),
    supervisor:delete_child(morning_db_mysql_sup, db_mysql).

start() ->
    % ChildSpec = morning_db_mysql:config_init(),
    % supervisor:start_child(morning_db_mysql_sup, ChildSpec).
    start_link().

get_pid(ShardNames) ->
    cuesport:get_worker(ShardNames).

%%
%% Return odbc connection process id and the number of sharded table,
%% and undefined PID means no available connection for this sharding.
%%

get_random_pid(<<"db_mysql">>) ->
    get_pid(db_mysql).

