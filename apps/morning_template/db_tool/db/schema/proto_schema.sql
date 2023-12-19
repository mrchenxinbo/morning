
SET FOREIGN_KEY_CHECKS=0;
-- ----------------------------
-- Table structure for `stage_level_config`
-- ----------------------------
DROP TABLE IF EXISTS `stage_level_config`;
CREATE TABLE `stage_level_config` (
  `id` int(5)  NOT NULL   COMMENT 'id',
  `time` int(6)  NOT NULL DEFAULT 0  COMMENT '次数',
  `type` int(5)  NOT NULL DEFAULT 0  COMMENT '类型',
  `paramas` varchar(250)  NOT NULL DEFAULT ''  COMMENT '参数',
  `missionid` int(5)  NOT NULL DEFAULT 0  COMMENT '任务id',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='关卡表';
