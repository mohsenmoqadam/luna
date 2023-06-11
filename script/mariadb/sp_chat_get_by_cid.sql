DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_get_by_cid`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_get_by_cid`(
	  IN in_cid BIGINT
)
BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: INVALID CID
	-- 3: DONE	
	
	DECLARE v_cid BIGINT;
	DECLARE v_cra DATETIME(6);
	DECLARE v_mda DATETIME(6);
	DECLARE v_starter_id BIGINT;
	DECLARE v_follower_id BIGINT;
	DECLARE v_last_message_sequence BIGINT;
	DECLARE v_last_event_sequence BIGINT;
	DECLARE v_pinned_messages JSON;
	DECLARE v_starter_start_sequence BIGINT;
	DECLARE v_starter_delivered_sequence BIGINT;
	DECLARE v_starter_seen_sequence BIGINT;
	DECLARE v_starter_is_muted BOOLEAN;
	DECLARE v_starter_is_blocked BOOLEAN;
	DECLARE v_starter_is_deleted BOOLEAN;
	DECLARE v_starter_auto_delete INT;
	DECLARE v_follower_start_sequence BIGINT;
	DECLARE v_follower_delivered_sequence BIGINT;
	DECLARE v_follower_seen_sequence BIGINT;
	DECLARE v_follower_is_muted BOOLEAN;
	DECLARE v_follower_is_blocked BOOLEAN;
	DECLARE v_follower_is_deleted BOOLEAN;
	DECLARE v_follower_auto_delete INT;
	DECLARE v_kivi JSON;

	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
		ROLLBACK;
		SELECT 1 AS 'RC';
	END;
   
	SET v_cid = NULL;

	-- Make result.
	SELECT cid
	     , cra
		 , mda
		 , starter_id
		 , follower_id
		 , last_message_sequence
		 , last_event_sequence
		 , pinned_messages
		 , starter_start_sequence
		 , starter_delivered_sequence
		 , starter_seen_sequence
		 , starter_is_muted
		 , starter_is_blocked
		 , starter_is_deleted
		 , starter_auto_delete
		 , follower_start_sequence
		 , follower_delivered_sequence
		 , follower_seen_sequence
		 , follower_is_muted
		 , follower_is_blocked
		 , follower_is_deleted 
		 , follower_auto_delete
		 , kivi 	 
	INTO   v_cid
	     , v_cra
		 , v_mda
		 , v_starter_id
		 , v_follower_id
		 , v_last_message_sequence
		 , v_last_event_sequence
		 , v_pinned_messages
		 , v_starter_start_sequence
		 , v_starter_delivered_sequence
		 , v_starter_seen_sequence
		 , v_starter_is_muted
		 , v_starter_is_blocked
		 , v_starter_is_deleted
		 , v_starter_auto_delete
		 , v_follower_start_sequence
		 , v_follower_delivered_sequence
		 , v_follower_seen_sequence
		 , v_follower_is_muted
		 , v_follower_is_blocked
		 , v_follower_is_deleted 
		 , v_follower_auto_delete
		 , v_kivi	 
	FROM luna_dev_db.chat_meta WHERE cid = in_cid;  
	
	-- Return result.
	IF (ISNULL(v_cid)) THEN
		SELECT 2 AS 'RC';
	ELSE
		SELECT 3 AS 'RC'
			 , v_cid AS 'CID'
			 , v_cra AS 'CRA'
			 , v_mda AS 'MDA'
			 , v_starter_id AS 'STARTER_ID'
			 , v_follower_id AS 'FOLLOWER_IID'
			 , v_last_message_sequence AS 'LAST_MESSAGE_SEQUENCE'
			 , v_last_event_sequence AS 'LAST_EVENT_SEQUENCE'
			 , v_pinned_messages AS 'PINNED_MESSAGES'
			 , v_starter_start_sequence AS 'STARTER_START_SEQUENCE'
			 , v_starter_delivered_sequence AS 'STARTER_DELIVERED_SEQUENCE'
			 , v_starter_seen_sequence AS 'STARTER_SEEN_SEQUENCE'
			 , v_starter_is_muted AS 'STARTER_IS_MUTED'
			 , v_starter_is_blocked AS 'STARTER_IS_BLOCKED'
			 , v_starter_is_deleted AS 'STARTER_IS_DELETED'
			 , v_starter_auto_delete AS 'STARTER_AUTO_DELETE'
			 , v_follower_start_sequence AS 'FOLLOWER_START_SEQUENCE'
			 , v_follower_delivered_sequence AS 'FOLLOWER_DELIVERED_SEQUENCE'
			 , v_follower_seen_sequence AS 'FOLLOWER_SEEN_SEQUENCE'
			 , v_follower_is_muted AS 'FOLLOWER_IS_MUTED'
			 , v_follower_is_blocked AS 'FOLLOWER_IS_BLOCKED'
			 , v_follower_is_deleted  AS 'FOLLOWER_IS_DELETED'
			 , v_follower_auto_delete AS 'FOLLOWER_AUTO_DELETE'
 			 , v_kivi AS 'KIVI';
	END IF;	
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_get_by_cid`(6);
