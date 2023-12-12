%%%-------------------------------------------------------------------
%% @doc morning top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(morning_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    ChildSpecs = [init_db_mysql_sup(),
                  init_redis_pool_sup()   
            ],
    % ChildSpecs= [],
    SupFlags = #{strategy => one_for_one,
                 intensity => length(ChildSpecs),
                 period => 1},
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
init_db_mysql_sup() ->
    {morning_db_mysql_sup,
     {morning_db_mysql_sup, start_link, []},
     permanent,
     infinity,
     supervisor,
     [morning_db_mysql_sup]}.

init_redis_pool_sup() ->
    {morning_redis_pool_sup,
        {morning_redis_pool_sup, start_link, []},
        permanent,
        infinity,
        supervisor,
        [morning_redis_pool_sup]}.