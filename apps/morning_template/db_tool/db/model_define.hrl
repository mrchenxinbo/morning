%% 数据表配置
-record(model_define, {
					   name,			%% 表名
					   info="", 		%% 表描述
					   type, 			%% 类型 (disc:实体表, proto:模板表, log:log表)
					   attrs, 			%% 字段列表 ([model_attr])
					   primary_keys, 	%% 主键列表
					   indexs=[], 		%% 索引列表
					   engine=innodb,	%% 表属性
					   extra			%% 扩展
					  }).
%% 表字段配置
-record(model_attr, {
					 name, 				%% 字段名
					 flag, 				%% 标识 (主要针对模数据  s:服务端  c:客户端)
					 type,				%% 字段类型
					 length,			%% 长度
					 float,				%% 小数位
					 cannull="",		%% 是否可空
					 default,			%% 默认值
					 autoinc=[],		%% 无符号、自增等 [unsigned, auto]
					 info=""			%% 描述
					}).
%% 数据表发生改变时，对比信息
-record(model_diff, {name, info="", type, add_attrs, add_afore_attrs, del_attrs, del_table, new_attrs, chg_attrs, new_primary_keys, del_indexs, add_indexs, engine}).
%% 纯模板表  无须mysql库
-record(proto_define, {name, attrs, keys, type=set}).
-record(proto_attr, {name, flag, default, info=""}).
%% 纯模板表  无须mysql库 (带init回调函数)
-record(callback_proto_define, {name, attrs, keys, type=set, init_args}).

%%数据库表前缀
-define(MODEL_FLAG_SERVER, s).
-define(MODEL_FLAG_CLIENT, c).
-define(TABLE_TYPE_DISC, disc).			%%实体表
-define(TABLE_TYPE_PROTO, proto).		%%模板表
-define(TABLE_TYPE_LOG, log).			%%log表
