DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_add`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_add`(
	  IN in_starter_id BIGINT
	, IN in_follower_id BIGINT
	, IN in_kivi TEXT
)
BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: SAME ID AS BOTH SIDES
	-- 3: ALREADY EXIST 
	-- 4: ALREADY EXIST BUT BOTH SIDES HAD BEEN DELETED
	-- 5: ALREADY EXIST BUT STARTER HAD BEEN DELETED
	-- 6: ALREADY EXIST BUT FOLLOWER HAD BEEN DELETED
	-- 7: NEW CHAT
	 
	DECLARE v_rc INT DEFAULT NULL;
	DECLARE v_cra DATETIME(6) DEFAULT NULL;
	DECLARE v_cid BIGINT DEFAULT NULL;
	DECLARE v_starter_id BIGINT DEFAULT NULL;
	DECLARE v_follower_id BIGINT DEFAULT NULL;
	DECLARE v_already_exist BOOLEAN DEFAULT TRUE;
	DECLARE v_starter_is_deleted BOOLEAN DEFAULT NULL;
	DECLARE v_follower_is_deleted BOOLEAN DEFAULT NULL;
	DECLARE v_last_message_sequence BIGINT DEFAULT NULL;
	DECLARE v_last_event_sequence BIGINT DEFAULT NULL;
	DECLARE v_action_creator VARCHAR(16) DEFAULT NULL;

	-- DECLARE EXIT HANDLER FOR SQLEXCEPTION
	-- BEGIN
	--  	ROLLBACK;
	-- 	SELECT 1 AS 'RC';
	-- END;
   
	SET v_cra = NOW(6);

	-- Creating a new chat with the same ID as the starter and follower is NOT permitted!
	IF (in_starter_id = in_follower_id) THEN
		SELECT 2 AS 'RC';
	ELSE		
		-- Getting some meta if exists.
		SELECT cid, starter_id, follower_id, starter_is_deleted, follower_is_deleted, last_message_sequence 
		INTO v_cid, v_starter_id, v_follower_id, v_starter_is_deleted, v_follower_is_deleted, v_last_message_sequence
		FROM `luna_dev_db`.`chat_meta` 
  		WHERE (starter_id = in_starter_id AND follower_id = in_follower_id) OR (starter_id = in_follower_id AND follower_id = in_starter_id);
		
		-- TRANSACTION: START
		START TRANSACTION;
		-- Chat does not already exist.
		IF (ISNULL(v_cid)) THEN
			SET v_last_message_sequence = 1;
			INSERT INTO `luna_dev_db`.`chat_meta` (cra, mda, starter_id, follower_id, last_message_sequence ,kivi)
			VALUES (v_cra, v_cra, in_starter_id, in_follower_id, v_last_message_sequence, in_kivi);
			SELECT LAST_INSERT_ID() INTO v_cid;
			-- Insert CREATE action message
			INSERT INTO `luna_dev_db`.`chat_message` (cid, cra, type, sequence, writer, body)
			VALUES (v_cid, v_cra, 'CREATE', v_last_message_sequence, 'STARTER', "");
			SET v_rc = 7;
		-- Chat already exists, so we must reconfigure it properly.
		ELSEIF(v_starter_is_deleted = FALSE AND v_follower_is_deleted = FALSE) THEN 
			UPDATE luna_dev_db.chat_meta SET kivi = in_kivi WHERE cid = v_cid;
			SET v_rc = 3;
		ELSE
			IF (in_starter_id = v_starter_id) THEN
				SET v_action_creator = 'STARTER';
			ELSE
				SET v_action_creator = 'FOLLOWER';
			END IF;
			-- Both sides of the chat have already deleted it.
			IF (v_starter_is_deleted = TRUE AND v_follower_is_deleted = TRUE) THEN 
				SET v_last_message_sequence = v_last_message_sequence + 1;
				UPDATE `luna_dev_db`.`chat_meta`
				SET kivi = in_kivi
				  , last_message_sequence = v_last_message_sequence
				  , starter_start_sequence = v_last_message_sequence
				  , starter_delivered_sequence = v_last_message_sequence
				  , starter_seen_sequence = v_last_message_sequence
				  , starter_is_muted = FALSE
				  , starter_is_blocked = FALSE
				  , starter_is_deleted = FALSE
				  , starter_auto_delete = NULL
				  , follower_start_sequence = v_last_message_sequence
				  , follower_delivered_sequence = v_last_message_sequence
				  , follower_seen_sequence = v_last_message_sequence
				  , follower_is_muted = FALSE
				  , follower_is_blocked = FALSE
				  , follower_is_deleted = FALSE
				  , follower_auto_delete = NULL
				WHERE cid = v_cid;
				-- Insert CREATE action message
				INSERT INTO `luna_dev_db`.`chat_message` (cid, cra, type, sequence, writer, body)
				VALUES (v_cid, v_cra, 'CREATE', v_last_message_sequence, v_action_creator, "");	
				SET v_rc = 4;
			-- Only the starter has already deleted the chat, so it will see the timeline as the other side.
			ELSEIF (v_starter_is_deleted) THEN
				SET v_last_message_sequence = v_last_message_sequence + 1;
				UPDATE `luna_dev_db`.`chat_meta`
				SET kivi = in_kivi
				  , last_message_sequence = v_last_message_sequence
				  , starter_start_sequence = follower_start_sequence 
				  , starter_delivered_sequence = follower_start_sequence
				  , starter_seen_sequence = follower_start_sequence
				  , starter_is_deleted = FALSE
				WHERE cid = v_cid;
			    -- Insert CREATE action message
				INSERT INTO `luna_dev_db`.`chat_message` (cid, cra, type, sequence, writer, body)
				VALUES (v_cid, v_cra, 'CREATE', v_last_message_sequence, v_action_creator, "");
				SET v_rc = 5;
			-- Only the follower has already deleted the chat, so it will see the timeline as the other side.
			ELSEIF (v_follower_is_deleted) THEN
				SET v_last_message_sequence = v_last_message_sequence + 1;
				UPDATE `luna_dev_db`.`chat_meta`
				SET kivi = in_kivi
				  , last_message_sequence = v_last_message_sequence
				  , follower_start_sequence = starter_start_sequence
				  , follower_delivered_sequence = starter_start_sequence
				  , follower_seen_sequence = starter_start_sequence
				  , follower_is_deleted = FALSE
				WHERE cid = v_cid;
				-- Insert CREATE action message
				INSERT INTO `luna_dev_db`.`chat_message` (cid, cra, type, sequence, writer, body)
				VALUES (v_cid, v_cra, 'CREATE', v_last_message_sequence, v_action_creator, "");
				SET v_rc = 6;
			END IF;
		END IF;
		
		-- TRANSACTION: END
		COMMIT;
		
		-- And finally, we return the last state of chat meta.
		SELECT v_rc AS 'RC'
		     , cid AS 'CID'
			 , cra AS 'CRA'
			 , mda AS 'MDA'
			 , starter_id AS 'STARTER_ID'
			 , follower_id AS 'FOLLOWER_ID'
			 , last_message_sequence AS 'LAST_MESSAGE_SEQUENCE'
			 , last_event_sequence AS 'LAST_EVENT_SEQUENCE'
			 , pinned_messages AS 'PINNED_MESSAGES'
			 , starter_start_sequence AS 'STARTER_START_SEQUENCE'
			 , starter_delivered_sequence AS 'STARTER_DELIVERED_SEQUENCE'
			 , starter_seen_sequence AS 'STARTER_SEEN_SEQUENCE'
			 , starter_is_muted AS 'STARTER_IS_MUTED'
			 , starter_is_blocked AS 'STARTER_IS_BLOCKED'
			 , starter_is_deleted AS 'STARTER_IS_DELETED'
			 , starter_auto_delete AS 'STARTER_AUTO_DELETE'
			 , follower_start_sequence AS 'FOLLOWER_START_SEQUENCE'
			 , follower_delivered_sequence AS 'FOLLOWER_DELIVERED_SEQUENCE'
			 , follower_seen_sequence AS 'FOLLOWER_SEEN_SEQUENCE'
			 , follower_is_muted AS 'FOLLOWER_IS_MUTED'
			 , follower_is_blocked AS 'FOLLOWER_IS_BLOCKED'
			 , follower_is_deleted AS 'FOLLOWER_IS_DELETED'
			 , follower_auto_delete AS 'FOLLOWER_AUTO_DELETE' 
			 , kivi AS 'KIVI'
		FROM `luna_dev_db`.`chat_meta` 
		WHERE cid = v_cid;
	END IF;
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_add`(1, 5, null);
