DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_del_pinned_messages`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_del_pinned_messages`(
	  IN in_cid BIGINT
	, IN in_last_message_sequence BIGINT
	, IN in_last_event_sequence BIGINT	
	, IN in_unpinner ENUM('STARTER', 'FOLLOWER')
)
BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: DONE	
	
	DECLARE v_cra DATETIME(6);
	DECLARE v_mda DATETIME(6);
	DECLARE v_last_message_sequence BIGINT;
	DECLARE v_last_event_sequence BIGINT;

	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
		ROLLBACK;
		SELECT 1 AS 'RC';
	END;
   
	-- Be careful that the latest version of chat_meta is available in the cache layer and we trust it,
	-- Therefore, in order to reduce the processing load, we do not check the correctness of the information again.

	SET v_cra = NOW(6);
	SET v_mda = v_cra;
	SET v_last_message_sequence = in_last_message_sequence + 1;
	SET v_last_event_sequence = in_last_event_sequence + 1;

	-- TRANSACTION: START
	START TRANSACTION;	
	-- Update chat_meta
	UPDATE luna_dev_db.chat_meta 
	SET pinned_messages = "[]"
	  , last_message_sequence = v_last_message_sequence
	  , last_event_sequence = v_last_event_sequence
	  , mda = v_mda 
	WHERE cid = in_cid;
	-- Save PIN message
	INSERT INTO `luna_dev_db`.`chat_message` (cid, cra, type, sequence, writer, body)
	VALUES (in_cid, v_cra, 'UNPIN', v_last_message_sequence, in_unpinner, "");
	-- TRANSACTION: END
	COMMIT;		
		
	-- Return result.
	SELECT 2 AS 'RC', v_mda AS 'MDA', v_last_message_sequence AS 'LAST_MESSAGE_SEQUENCE', v_last_event_sequence AS 'LAST_EVENT_SEQUENCE';
		
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_del_pinned_messages`(5, 7, 1, 'FOLLOWER');
