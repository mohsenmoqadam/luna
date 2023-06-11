DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_set`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_set`(
	  IN in_cid BIGINT
	, IN in_starter_id BIGINT
	, IN in_follower_id BIGINT
	, IN in_last_message_sequence BIGINT
	, IN in_last_event_sequence BIGINT
	, IN in_pinned_messages JSON
	, IN in_starter_start_sequence BIGINT
	, IN in_starter_delivered_sequence BIGINT
	, IN in_starter_seen_sequence BIGINT
	, IN in_starter_is_muted BOOL 
	, IN in_starter_is_blocked BOOL
	, IN in_starter_is_deleted BOOL
	, IN in_starter_auto_delete INT
	, IN in_follower_start_sequence BIGINT
	, IN in_follower_delivered_sequence BIGINT
	, IN in_follower_seen_sequence BIGINT
	, IN in_follower_is_muted BOOL
	, IN in_follower_is_blocked BOOL
	, IN in_follower_is_deleted BOOL 
	, IN in_follower_auto_delete INT
	, IN in_kivi JSON 
)
BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: INVALID CID
	-- 3: DONE	
	
	DECLARE v_cid BIGINT DEFAULT NULL;
	DECLARE v_cra DATETIME(6);
	DECLARE v_mda DATETIME(6);
	
	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
		ROLLBACK;
		SELECT 1 AS 'RC';
	END;
   
	SET v_mda = NOW(6);

	-- Make result.
	SELECT cid, cra INTO v_cid, v_cra FROM luna_dev_db.chat_meta WHERE cid = in_cid;
	IF (ISNULL(v_cid)) THEN
		SELECT 2 AS 'RC';
	ELSE
		UPDATE luna_dev_db.chat_meta  
		SET starter_id = in_starter_id
		  , follower_id = in_follower_id
		  , last_message_sequence = in_last_message_sequence
		  , last_event_sequence = in_last_event_sequence
		  , pinned_messages = in_pinned_messages
		  , starter_start_sequence = in_starter_start_sequence
		  , starter_delivered_sequence = in_starter_delivered_sequence
		  , starter_seen_sequence = in_starter_seen_sequence
		  , starter_is_muted = in_starter_is_muted
		  , starter_is_blocked = in_starter_is_blocked
		  , starter_is_deleted = in_starter_is_deleted
		  , starter_auto_delete = in_starter_auto_delete
		  , follower_start_sequence = in_follower_start_sequence
		  , follower_delivered_sequence = in_follower_delivered_sequence
		  , follower_seen_sequence = in_follower_seen_sequence
		  , follower_is_muted = in_follower_is_muted
		  , follower_is_blocked = in_follower_is_blocked
		  , follower_is_deleted = in_follower_is_deleted 
		  , follower_auto_delete = in_follower_auto_delete
		  , kivi = in_kivi
		WHERE cid = v_cid;
		SELECT 3 AS 'RC', v_cra AS 'CRA', v_mda AS 'MDA';
	END IF;
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_set`(6, 1, 2, 1, 1, "{}", 1, 1, 1, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, "{}");
