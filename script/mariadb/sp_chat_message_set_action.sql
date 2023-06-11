DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_message_set_action`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_message_set_action`(
	  IN in_cid BIGINT
	, IN in_starter_id BIGINT
	, IN in_follower_id BIGINT
	, IN in_writer BIGINT
	, IN in_message_sequence BIGINT  
	, IN in_last_event_sequence BIGINT
	, IN in_actions JSON
)

BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: INVALID ROLE
	-- 3: DONE
	
	DECLARE v_mda DATETIME(6);
	DECLARE v_last_event_sequence BIGINT;
	DECLARE v_writer ENUM('STARTER', 'FOLLOWER');
	DECLARE v_role BOOL;

	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
		ROLLBACK;
		SELECT 1 AS 'RC';
	END;
   
	-- Be careful that the latest version of chat_meta is available in the cache layer and we trust it,
	-- Therefore, in order to reduce the processing load, we do not check the correctness of the information again.
	  
	SET v_mda = NOW(6);
	SET v_last_event_sequence = in_last_event_sequence + 1;
	
	SELECT writer FROM `luna_dev_db`.`chat_message` 
	WHERE cid = in_cid AND sequence = in_message_sequence
	INTO v_writer;
	
	IF (v_writer = 'STARTER' AND in_writer = in_starter_id) THEN
		SET v_role = FALSE;
	ELSEIF (v_writer = 'FOLLOWER' AND in_writer = in_follower_id) THEN
		SET v_role = FALSE;
	ELSE
		SET v_role = TRUE;
	END IF;

	IF (v_role) THEN
		-- TRANSACTION: START
		START TRANSACTION;
		-- Update chat_meta
		UPDATE luna_dev_db.chat_meta 
		SET mda = mda
		  , last_event_sequence = v_last_event_sequence 	
		WHERE cid = in_cid;
		-- Save actions
		UPDATE `luna_dev_db`.`chat_message`
		SET actions = in_actions
		WHERE cid = in_cid AND sequence = in_message_sequence;	
		-- TRANSACTION: END
		COMMIT;	
		SELECT 3 AS 'RC', v_mda AS 'MDA', v_last_event_sequence AS 'LAST_EVENT_SEQUENCE'; 
	ELSE
		SELECT 2 AS 'RC';
	END IF;
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_message_set_action`(6, 1, "{}");
