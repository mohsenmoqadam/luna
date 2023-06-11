DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_del_starter`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_del_starter`(
	  IN in_cid BIGINT
)
BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: INVALID CID
	-- 3: ALREADY SET
	-- 4: DONE	

	DECLARE v_cra DATETIME(6);
	DECLARE v_mda DATETIME(6);
	DECLARE v_cid BIGINT DEFAULT NULL;
	DECLARE v_starter_id BIGINT;
	DECLARE v_last_message_sequence BIGINT;
	DECLARE v_starter_is_deleted BOOLEAN;
	DECLARE v_rc INT;
	 
	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
		ROLLBACK;
		SELECT 1 AS 'RC';
	END;
   
	SET v_cra = NOW(6);
	SET v_mda = v_cra;

	-- Getting some meta if exists.
	SELECT cid, starter_id, starter_is_deleted, last_message_sequence 
	INTO v_cid, v_starter_id, v_starter_is_deleted, v_last_message_sequence 
	FROM `luna_dev_db`.`chat_meta` 
	WHERE cid = in_cid;
	
	IF (ISNULL(v_cid)) THEN
		SELECT 2 AS 'RC';
	ELSEIF (v_starter_is_deleted) THEN
		SELECT 3 AS 'RC';
	ELSE
		SET v_last_message_sequence = v_last_message_sequence + 1;
		-- TRANSACTION: START
		START TRANSACTION;
		-- Update chat_meta
		UPDATE `luna_dev_db`.`chat_meta`
		SET starter_is_deleted = TRUE
		  , starter_start_sequence = v_last_message_sequence
		  , starter_delivered_sequence = v_last_message_sequence
		  , starter_seen_sequence = v_last_message_sequence
		  , last_message_sequence = v_last_message_sequence
		  , mda = v_mda
		WHERE cid = in_cid;
		-- Save DELETE message
		INSERT INTO `luna_dev_db`.`chat_message` (cid, cra, type, sequence, writer, body)
		VALUES (in_cid, v_cra, 'DELETE', v_last_message_sequence, 'STARTER', "");
		-- Delete all objects
		DELETE FROM `luna_dev_db`.`chat_storage`
		WHERE cid = in_cid AND uid = v_starter_id;
		-- TRANSACTION: END
		COMMIT;
		-- Return RC and changes
		SELECT 4 AS RC
		     , v_mda as 'MDA' 
			 , v_last_message_sequence AS 'STARTER_START_SEQUENCE'
		     , v_last_message_sequence AS 'STARTER_DELIVERED_SEQUENCE'
		     , v_last_message_sequence AS 'STARTER_SEEN_SEQUENCE'
		     , v_last_message_sequence AS 'LAST_MESSAGE_SEQUENCE';
	END IF;
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_del_starter`(5, 1);
