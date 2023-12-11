-- morning_one.morning_user_info definition

CREATE TABLE `morning_user_info` (
  `uid` bigint NOT NULL AUTO_INCREMENT,
  `nickname` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `unionid` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL,
  `password` varchar(100) DEFAULT NULL,
  `channel` int DEFAULT NULL,
  `create_ts` bigint DEFAULT NULL,
  `update_ts` bigint DEFAULT NULL,
  PRIMARY KEY (`uid`),
  UNIQUE KEY `morning_user_info_uid_IDX` (`uid`) USING BTREE,
  KEY `morning_user_info_unionid_IDX` (`unionid`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=1000022 DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci;