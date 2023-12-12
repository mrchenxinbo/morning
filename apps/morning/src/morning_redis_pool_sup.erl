% /*
%  * @Author: mrchenxinbo 
%  * @Date: 2023-12-11 19:32:33 
%  * @Last Modified by:   mrchenxinbo 
%  * @Last Modified time: 2023-12-11 19:32:33 
%  */
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
-module(morning_redis_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, connect/1, connect/5, connect/6, disconnect/1, get_default_opt/1,
         init_redis/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec connection redis
%% @end
%%--------------------------------------------------------------------
connect(#{type := redis,
          name := Name,
          hosts := Hosts,
          db := DB,
          password := PassWord,
          pool_size := PoolSize}) ->
    [{Host, Port}] = Hosts,
    ChildSpec = init_childspec(Name, Host, Port, DB, PassWord, PoolSize),
    supervisor:start_child(morning_redis_pool_sup, ChildSpec);

% connect(#{type := redis_cluster,
%           name := Name,
%           hosts := Hosts,
%           db := DB,
%           password := PassWord,
%           pool_size := PoolSize,
%           tls_options := TlsOptions}) ->
%     eredis_cluster:connect(Name,
%                            Hosts,
%                            [{password, PassWord},
%                             {pool_size, PoolSize},
%                             {database, DB},
%                             {tls, TlsOptions}]
%                            ++ [{socket_options, socket_opts()}]),
%     application:set_env(redis_choose, Name, eredis_cluster);
connect(Name) ->
    Options = get_default_opt(Name),
    connect(Options).

%%--------------------------------------------------------------------
%% @doc
%% @spec connection redis
%% @end
%%--------------------------------------------------------------------
connect(Type, Name, Hosts, DB, PoolSize) ->
    connect(Type, Name, Hosts, DB, "", PoolSize).

connect(Type, Name, Hosts, DB, PassWord, PoolSize) ->
    connect(#{type => Type,
              name => Name,
              hosts => Hosts,
              db => DB,
              password => PassWord,
              pool_size => PoolSize}).

%%--------------------------------------------------------------------
%% @doc
%% @spec disconnect redis
%% @end
%%--------------------------------------------------------------------
disconnect(Name) ->
    supervisor:terminate_child(easemob_redis_pool_sup, Name),
    supervisor:delete_child(easemob_redis_pool_sup, Name).

    % case application:get_env(redis_choose, Name, eredis) of
    %     eredis ->
    %         supervisor:terminate_child(easemob_redis_pool_sup, Name),
    %         supervisor:delete_child(easemob_redis_pool_sup, Name);
    %     eredis_cluster ->
    %         eredis_cluster:disconnect(Name),
    %         ok
    % end.

%%--------------------------------------------------------------------
%% @doc
%% @spec get default redis opt for Name
%% @end
%%--------------------------------------------------------------------
get_default_opt(Name) ->
    {ok, Config} = application:get_env(morning, Name),
    Hosts = get_host_and_port_list(Config),
    RedisDb = proplists:get_value(db, Config, 0),
    PassWord = proplists:get_value(password, Config, ""),
    Type = proplists:get_value(type, Config),
    TlsOptions = proplists:get_value(tls, Config, []),
    PoolSize = proplists:get_value(pool_size, Config, 0),
        % case proplists:get_value(single_cpu_pool_size, Config, undefined) of
        %     undefined ->
        %         proplists:get_value(pool_size, Config, 0);
        %     SingleCpuPoolSize ->
        %         adaptive_poolsize(SingleCpuPoolSize)
        % end,
    #{name => Name,
      type => Type,
      hosts => Hosts,
      db => RedisDb,
      password => PassWord,
      pool_size => PoolSize,
      tls_options => TlsOptions}.

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    Childs = redis_child_spec(),
    SupFlags = {one_for_one, erlang:length(Childs) * 10, 1},
    {ok, {SupFlags, Childs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
init_redis() ->
    {ok, RedisList} = application:get_env(morning, redis),
    [connect(RedisName) || RedisName <- RedisList].

redis_child_spec() ->
    {ok, RedisList} = application:get_env(morning, redis),
    redis_child_spec(RedisList, []).

redis_child_spec([RedisName | Last], Ret) ->
    Options = get_default_opt(RedisName),
    ChildSpec = init_childspec(Options),
    redis_child_spec(Last, ChildSpec ++ Ret);
redis_child_spec([], Ret) ->
    Ret.

init_childspec(#{
                 name := Name,
                 hosts := Hosts,
                 db := DB,
                 password := PassWord,
                 pool_size := PoolSize}) ->
    [{Host, Port}] = Hosts,
    Spec = init_childspec(Name, Host, Port, DB, PassWord, PoolSize),
    [Spec].
% init_childspec(#{type := redis_cluster} = Options) ->
%     connect(Options),
%     [].

socket_opts() ->
    [{keepalive, true}].

init_childspec(RedisName, Host, Port, DB, PassWord, PoolSize) ->
    RedisOpts =
        [RedisName,
         PoolSize,
         [eredis, eredis_client, eredis_parser],
         {eredis,
          start_link,
          [[{host, Host},
            {port, Port},
            {database, DB},
            {password, PassWord},
            {socket_options, socket_opts()}]]}],
    {RedisName, {cuesport, start_link, RedisOpts}, permanent, infinity, supervisor, [eredis]}.

get_host_and_port_list(Config) ->
    case proplists:get_value(host_and_port, Config, []) of
        [] ->
            RedisHost = proplists:get_value(host, Config, ""),
            RedisPort = proplists:get_value(port, Config, 6379),
            [{RedisHost, RedisPort}];
        HostPortList ->
            HostPortList
    end.

adaptive_poolsize(OriValue) ->
    Schedulers = erlang:system_info(schedulers),
    OriValue * Schedulers.
