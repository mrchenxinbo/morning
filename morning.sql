
-- morning_one.morning_user_info definition

CREATE TABLE `morning_user_info` (
  `uid` int(11)  NOT NULL DEFAULT 0 AUTO_INCREMENT COMMENT 'uid',
  `nickname` varchar(100)  NOT NULL DEFAULT ''  COMMENT '昵称',
  `unionid` varchar(100)  NOT NULL DEFAULT ''  COMMENT 'unionid 唯一id',
  `password` varchar(100)  NULL  COMMENT '密码',
  `channel` int(3)  NOT NULL DEFAULT 0  COMMENT '渠道',
  `create_ts` bigint(20)  NOT NULL DEFAULT 0  COMMENT '创建时间',
  `update_ts` bigint(20)  NOT NULL DEFAULT 0  COMMENT '更新时间',
  PRIMARY KEY (`uid`),
  UNIQUE KEY `morning_user_info_unionid_IDX` (`unionid`, `channel`) USING BTREE,
  KEY `morning_user_info_uid_IDX` (`uid`) USING BTREE
) ENGINE=InnoDB  AUTO_INCREMENT=1000000 DEFAULT CHARSET=utf8 COMMENT='用户信息表';




INSERT INTO morning_user_info INSERT INTO morning_user_info (`uid`, `nickname`, `unionid`, `password`, `channel`, `create_ts`, `update_ts`) VALUES ('null','aa', '1234', '', 3, 1, 2);