DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_set_follower_muted_state`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_set_follower_muted_state`(
	  IN in_cid BIGINT
	, IN in_is_muted BOOL
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
	DECLARE v_follower_id BIGINT DEFAULT NULL;
	DECLARE v_follower_is_muted BOOLEAN;
	DECLARE v_last_message_sequence BIGINT;
	DECLARE v_action_type VARCHAR(8);

	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
		ROLLBACK;
		SELECT 1 AS 'RC';
	END;
   
	-- Be careful that the latest version of chat_meta is available in the cache layer and we trust it,
	-- Therefore, in order to reduce the processing load, we do not check the correctness of the information again.

   	SET v_cra = NOW(6);
   	SET v_mda = v_cra;
	
   	-- Getting some meta if exists.
	SELECT cid, follower_id, follower_is_muted, last_message_sequence 
	INTO v_cid, v_follower_id, v_follower_is_muted, v_last_message_sequence 
	FROM `luna_dev_db`.`chat_meta` 
	WHERE cid = in_cid;

	-- MAIN BUSINESS
	IF(ISNULL(v_cid)) THEN
		SELECT 2 AS 'RC';
	ELSEIF(v_follower_is_muted = in_is_muted) THEN
		SELECT 3 AS 'RC';
	ELSE
		SET v_last_message_sequence = v_last_message_sequence + 1;
		IF (in_is_muted = TRUE) THEN
			SET v_action_type = 'MUTE';
		ELSE
			SET v_action_type = 'UNMUTE';
		END IF;
		-- TRANSACTION: START
		START TRANSACTION;	
		-- Update chat_meta
		UPDATE luna_dev_db.chat_meta 
			SET follower_is_muted = in_is_muted
			  , last_message_sequence = v_last_message_sequence
			  , mda = v_mda 
		WHERE cid = in_cid;
		-- Save DELETE message
		INSERT INTO `luna_dev_db`.`chat_message` (cid, cra, type, sequence, writer, body)
		VALUES (in_cid, v_cra, v_action_type, v_last_message_sequence, 'FOLLOWER', "");
		-- TRANSACTION: END
		COMMIT;		
		-- Return result.
		SELECT 4 AS 'RC', v_mda AS 'MDA', v_last_message_sequence AS 'LAST_MESSAGE_SEQUENCE';
	END IF;		
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_set_follower_muted_state`(6, FALSE);
