%% 用户信息表
-record(morning_user_info, {
			uid=null,					%% uid
			nickname=[],				%% 昵称
			unionid=[],					%% unionid 唯一id
			password=[],				%% 密码
			channel=0,					%% 渠道
			create_ts=0,				%% 创建时间
			update_ts=0					%% 更新时间
		}). %%@disc,set,key:[uid],index:[unionid,channel]

%% 关卡表
-record(stage_level_config, {
			id=0,						%% id
			time=0,						%% 次数
			type=0,						%% 类型
			paramas=[],					%% 参数
			missionid=0					%% 任务id
		}). %%@proto,set,key:[id]

