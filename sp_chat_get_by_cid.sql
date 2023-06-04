DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_get_by_cid`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_get_by_cid`(
	  IN cid_ BIGINT
)
BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: INVALID CID
	-- 3: DONE	
	 
	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
		ROLLBACK;
		SELECT 1 AS 'RC';
	END;
   
	SET @cid = NULL;

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
	FROM luna_dev_db.chat_meta WHERE cid = cid_ 
	INTO   @cid
	     , @cra
		 , @mda
		 , @starter_id
		 , @follower_id
		 , @last_message_sequence
		 , @last_event_sequence
		 , @pinned_messages
		 , @starter_start_sequence
		 , @starter_delivered_sequence
		 , @starter_seen_sequence
		 , @starter_is_muted
		 , @starter_is_blocked
		 , @starter_is_deleted
		 , @starter_auto_delete
		 , @follower_start_sequence
		 , @follower_delivered_sequence
		 , @follower_seen_sequence
		 , @follower_is_muted
		 , @follower_is_blocked
		 , @follower_is_deleted 
		 , @follower_auto_delete
		 , @kivi;  
	
	-- Return result.
	IF (ISNULL(@cid)) THEN
		SELECT 2 AS 'RC';
	ELSE
		SELECT 3 AS 'RC'
			 , @cid AS 'CID'
			 , @cra AS 'CRA'
			 , @mda AS 'MDA'
			 , @starter_id AS 'STARTER_ID'
			 , @follower_id AS 'FOLLOWER_IID'
			 , @last_message_sequence AS 'LAST_MESSAGE_SEQUENCE'
			 , @last_event_sequence AS 'LAST_EVENT_SEQUENCE'
			 , @pinned_messages AS 'PINNED_MESSAGES'
			 , @starter_start_sequence AS 'STARTER_START_SEQUENCE'
			 , @starter_delivered_sequence AS 'STARTER_DELIVERED_SEQUENCE'
			 , @starter_seen_sequence AS 'STARTER_SEEN_SEQUENCE'
			 , @starter_is_muted AS 'STARTER_IS_MUTED'
			 , @starter_is_blocked AS 'STARTER_IS_BLOCKED'
			 , @starter_is_deleted AS 'STARTER_IS_DELETED'
			 , @starter_auto_delete AS 'STARTER_AUTO_DELETE'
			 , @follower_start_sequence AS 'FOLLOWER_START_SEQUENCE'
			 , @follower_delivered_sequence AS 'FOLLOWER_DELIVERED_SEQUENCE'
			 , @follower_seen_sequence AS 'FOLLOWER_SEEN_SEQUENCE'
			 , @follower_is_muted AS 'FOLLOWER_IS_MUTED'
			 , @follower_is_blocked AS 'FOLLOWER_IS_BLOCKED'
			 , @follower_is_deleted  AS 'FOLLOWER_IS_DELETED'
			 , @follower_auto_delete AS 'FOLLOWER_AUTO_DELETE'
 			 , @kivi AS 'KIVI';
	END IF;	
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_get_by_cid`(5);
