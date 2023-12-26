
SET FOREIGN_KEY_CHECKS=0;
-- ----------------------------
-- Table structure for `morning_user_info`
-- ----------------------------
DROP TABLE IF EXISTS `morning_user_info`;
CREATE TABLE `morning_user_info` (
  `uid` int(11)  NOT NULL  AUTO_INCREMENT COMMENT 'uid',
  `nickname` varchar(100)  NOT NULL DEFAULT ''  COMMENT '昵称',
  `unionid` varchar(100)  NOT NULL DEFAULT ''  COMMENT 'unionid 唯一id',
  `password` varchar(100)  NULL  COMMENT '密码',
  `channel` int(3)  NOT NULL DEFAULT 0  COMMENT '渠道',
  `create_ts` bigint(20)  NOT NULL DEFAULT 0  COMMENT '创建时间',
  `update_ts` bigint(20)  NOT NULL DEFAULT 0  COMMENT '更新时间',
  PRIMARY KEY (`uid`),
  KEY channel(`channel`),
  KEY unionid(`unionid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='用户信息表';

SET FOREIGN_KEY_CHECKS=0;
-- ----------------------------
-- Table structure for `role_stage_info`
-- ----------------------------
DROP TABLE IF EXISTS `role_stage_info`;
CREATE TABLE `role_stage_info` (
  `uid` int(11)  NOT NULL   COMMENT 'uid',
  `mission` int(6)  NOT NULL DEFAULT 0  COMMENT '当前关卡',
  `score` int(11)  NOT NULL DEFAULT 0  COMMENT '分数',
  `max_score` int(11)  NOT NULL DEFAULT 0  COMMENT '最大分数',
  `update_ts` bigint(20)  NOT NULL DEFAULT 0  COMMENT '更新时间',
  PRIMARY KEY (`uid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='关卡信息表';

SET FOREIGN_KEY_CHECKS=0;
-- ----------------------------
-- Table structure for `role_inventory_info`
-- ----------------------------
DROP TABLE IF EXISTS `role_inventory_info`;
CREATE TABLE `role_inventory_info` (
  `id` int(11)  NOT NULL  AUTO_INCREMENT COMMENT 'id',
  `uid` int(11)  NOT NULL DEFAULT 0  COMMENT 'uid',
  `proto_id` int(11)  NOT NULL DEFAULT 0  COMMENT '当前关卡',
  `count` int(11)  NOT NULL DEFAULT 0  COMMENT '分数',
  `update_ts` bigint(20)  NOT NULL DEFAULT 0  COMMENT '更新时间',
  PRIMARY KEY (`id`),
  KEY uid(`uid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='背包信息表';
