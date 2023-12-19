%%
%% 工具生成代码,如果想加函数,请写到自定义区域.
%%
%% create by gen_db_code,you can edit it, especially use db index
%%
-module(base).

-include("model_def.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 						behaviour export
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-export([start/0,create_mnesia_table/1,create_mnesia_split_table/2,delete_role_from_db/1,tables_info/0]).

-behaviour(db_operater_mod).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				behaviour functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start()->
	db_operater_mod:start_module(?MODULE,[]).

create_mnesia_table(ram)->
	nothing;

create_mnesia_table(disc)->
	nothing.

create_mnesia_split_table(_,_)->
	nothing.

delete_role_from_db(_RoleId)->
	todo.

tables_info()->
	[{morning_user_info,disc},{stage_level_config,proto}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 				behaviour functions end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 自动生成代码结束,下面是自定义区域,可以随意增删修改
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


