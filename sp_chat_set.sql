DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_set`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_set`(
	  IN cid_ BIGINT
	, IN cra_ DATETIME
	, IN mda_ DATETIME
    , IN starter_id_ BIGINT
	, IN follower_id_ BIGINT
	, IN last_message_sequence_ BIGINT
	, IN last_event_sequence_ BIGINT
	, IN pinned_messages_ JSON
	, IN starter_start_sequence_ BIGINT
	, IN starter_delivered_sequence_ BIGINT
	, IN starter_seen_sequence_ BIGINT
	, IN starter_is_muted_ BOOL 
	, IN starter_is_blocked_ BOOL
	, IN starter_is_deleted_ BOOL
	, IN starter_auto_delete_ INT
	, IN follower_start_sequence_ BIGINT
	, IN follower_delivered_sequence_ BIGINT
	, IN follower_seen_sequence_ BIGINT
	, IN follower_is_muted_ BOOL
	, IN follower_is_blocked_ BOOL
	, IN follower_is_deleted_ BOOL 
	, IN follower_auto_delete_ INT
	, IN kivi_ JSON 
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
	SELECT cid FROM luna_dev_db.chat_meta WHERE cid = cid_ AND starter_id = starter_id_ AND follower_id = follower_id_ INTO @cid;
	IF (ISNULL(@cid)) THEN
		SELECT 2 AS 'RC';
	ELSE
		-- TRANSACTION: START
		START TRANSACTION;
		UPDATE luna_dev_db.chat_meta  
		SET cra = cra_
		  , mda = mda_
		  , starter_id = starter_id_
		  , follower_id = follower_id_
		  , last_message_sequence = last_message_sequence_
		  , last_event_sequence = last_event_sequence_
		  , pinned_messages = pinned_messages_
		  , starter_start_sequence = starter_start_sequence_
		  , starter_delivered_sequence = starter_delivered_sequence_
		  , starter_seen_sequence = starter_seen_sequence_
		  , starter_is_muted = starter_is_muted_
		  , starter_is_blocked = starter_is_blocked_
		  , starter_is_deleted = starter_is_deleted_
		  , starter_auto_delete = starter_auto_delete_
		  , follower_start_sequence = follower_start_sequence_
		  , follower_delivered_sequence = follower_delivered_sequence_
		  , follower_seen_sequence = follower_seen_sequence_
		  , follower_is_muted = follower_is_muted_
		  , follower_is_blocked = follower_is_blocked_
		  , follower_is_deleted = follower_is_deleted_ 
		  , follower_auto_delete = follower_auto_delete_
		  , kivi = kivi_
		WHERE cid = cid_;
		SELECT 3 AS 'RC';
	END IF;
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_set`(6, "2023-06-03 17:24:29", "2023-06-03 17:24:29", 1, 2, 1, 1, "[]", 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, "{}");
