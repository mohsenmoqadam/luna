USE luna_dev_db; 

DROP TABLE IF EXISTS `luna_dev_db`.`chat_meta`;
CREATE TABLE `luna_dev_db`.`chat_meta` (
	  `cid` BIGINT NOT NULL AUTO_INCREMENT
	, `cra` DATETIME(6) NOT NULL
	, `mda` DATETIME(6) DEFAULT NULL
	, `starter_id` BIGINT NOT NULL
	, `follower_id` BIGINT NOT NULL
	, `last_message_sequence` BIGINT NOT NULL DEFAULT 0
	, `last_event_sequence` BIGINT NOT NULL DEFAULT 0
	, `pinned_messages` JSON DEFAULT NULL
	, `starter_start_sequence` BIGINT NOT NULL DEFAULT 0
	, `starter_delivered_sequence` BIGINT NOT NULL DEFAULT 0
	, `starter_seen_sequence` BIGINT NOT NULL DEFAULT 0
	, `starter_is_muted` BOOL NOT NULL DEFAULT FALSE
	, `starter_is_blocked` BOOL NOT NULL DEFAULT FALSE
	, `starter_is_deleted` BOOL NOT NULL DEFAULT FALSE
	, `starter_auto_delete` INT DEFAULT NULL
	, `follower_start_sequence` BIGINT NOT NULL DEFAULT 0
	, `follower_delivered_sequence` BIGINT NOT NULL DEFAULT 0
	, `follower_seen_sequence` BIGINT NOT NULL DEFAULT 0
	, `follower_is_muted` BOOL NOT NULL DEFAULT FALSE
	, `follower_is_blocked` BOOL NOT NULL DEFAULT FALSE
	, `follower_is_deleted` BOOL NOT NULL DEFAULT FALSE 
	, `follower_auto_delete` INT DEFAULT NULL
	, `kivi` JSON DEFAULT NULL 
	, PRIMARY KEY (cid)
	, KEY (starter_id, follower_id)
) 
AUTO_INCREMENT=5
DEFAULT CHARSET=utf8mb4 
COLLATE utf8mb4_unicode_ci
PARTITION BY KEY(cid) PARTITIONS 10;

DROP TABLE IF EXISTS `luna_dev_db`.`chat_message`;
CREATE TABLE `luna_dev_db`.`chat_message`( 
	 `cid` BIGINT NOT NULL
	, `cra` DATETIME(6) NOT NULL 
	, `mda` DATETIME(6) DEFAULT NULL
	, `type` ENUM('MESSAGE', 'CREATE', 'DELETE', 'MUTE', 'UNMUTE', 'BLOCK', 'UNBLOCK', 'PIN', 'UNPIN')
	, `sequence` BIGINT NOT NULL
	, `reply_sequence` BIGINT DEFAULT NULL
	, `writer` ENUM('STARTER', 'FOLLOWER')
	, `body` TEXT NOT NULL
	, `objects` JSON DEFAULT NULL
	, `actions` JSON DEFAULT NULL
	, `is_deleted_by_starter` BOOLEAN DEFAULT FALSE
	, `is_deleted_by_follower` BOOLEAN DEFAULT FALSE
	, `auto_delete` INT DEFAULT NULL
	, `kivi` JSON DEFAULT NULL 
	, `version` INT DEFAULT 0
	, PRIMARY KEY (`cid`, `sequence`, type)
	, KEY(`cid`, `sequence`, )
) 
DEFAULT CHARSET=utf8mb4 
COLLATE utf8mb4_unicode_ci
PARTITION BY KEY(cid) PARTITIONS 10;

DROP TABLE IF EXISTS `luna_dev_db`.`chat_storage`;
CREATE TABLE `luna_dev_db`.`chat_storage`( 
	 `cid` BIGINT NOT NULL
	, `uid` BIGINT NOT NULL
	, `cra` DATETIME(6) NOT NULL 
	, `type` ENUM('LINK', 'FILE')
	, `sequence` BIGINT NOT NULL
	, `mime` VARCHAR(64) DEFAULT NULL 
	, `body` VARCHAR(512) NOT NULL
	, `oid` VARCHAR(256) DEFAULT NULL
	, INDEX (`uid`, `cid`)             -- DEL CHAT
	, INDEX (`uid`, `cid`, `type`)     -- GET
	, INDEX (`uid`, `cid`, `sequence`) -- DEL | SET
) 
DEFAULT CHARSET=utf8mb4 
COLLATE utf8mb4_unicode_ci
PARTITION BY KEY(cid) PARTITIONS 10;