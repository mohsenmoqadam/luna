DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_add`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_add`(
	  IN starter_id_ BIGINT
	, IN follower_id_ BIGINT
	, IN kivi_ TEXT
)
BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: ERROR: SAME STARTER-ID AND FOLLOWER-ID
	-- 3: DONE
	 
	DECLARE EXIT HANDLER FOR SQLEXCEPTION
    BEGIN
		SELECT 1 AS 'RC';
    END;
   
   	SET @cra = NOW();
   	SET @already_exist = TRUE;
	SET @cid = NULL;
	SET @starter_is_deleted = TRUE;
	SET @follower_is_deleted = TRUE;
	SET @starter_id = NULL;
	SET @follower_id = NULL;
	SET @last_message_sequence = NULL;
  	
	-- Creating a new chat with the same ID as the starter and follower is NOT permitted!
	IF (starter_id_ = follower_id_) THEN
		SELECT 2 AS 'RC';
	ELSE
		-- Getting some meta if exists.
		SELECT cid, starter_id, follower_id, starter_is_deleted, follower_is_deleted, last_message_sequence FROM `luna_dev_db`.`chat_meta` 
  		WHERE (starter_id = starter_id_ AND follower_id = follower_id_) OR (starter_id = follower_id_ AND follower_id = starter_id_)
		INTO @cid, @starter_id, @follower_id, @starter_is_deleted, @follower_is_deleted, @last_message_sequence;
		
		-- Chat does not already exist.
		IF (ISNULL(@cid)) THEN
			INSERT INTO `luna_dev_db`.`chat_meta` (cra, starter_id, follower_id, kivi)
			VALUES (@cra, starter_id_, follower_id_, kivi_);
			SELECT LAST_INSERT_ID() INTO @cid;
			SET @already_exist = FALSE;
			SET @starter_id = starter_id_;
			SET @follower_id = follower_id_;
			SET @last_message_sequence = 0;
			INSERT INTO `luna_dev_db`.`chat_uid2cid` (uid, cid, mda)
			VALUES (@starter_id, @cid, @cra), (@follower_id, @cid, @cra);
		-- Chat already exists, so we must reconfigure it properly.
		ELSE
			-- Both sides of the chat have already deleted it.
			IF (@starter_is_deleted = TRUE AND @follower_is_deleted = TRUE) THEN 
				UPDATE `luna_dev_db`.`chat_meta`
				SET kivi = kivi_
				  , last_message_sequence = @last_message_sequence + 1	
				  , starter_start_sequence = @last_message_sequence + 1
				  , starter_delivered_sequence = @last_message_sequence + 1
				  , starter_seen_sequence = @last_message_sequence + 1
				  , starter_badge = 0
				  , starter_is_muted = FALSE
				  , starter_is_blocked = FALSE
				  , starter_is_deleted = FALSE
				  , starter_auto_delete = NULL
				  , follower_start_sequence = @last_message_sequence + 1
				  , follower_delivered_sequence = @last_message_sequence + 1
				  , follower_seen_sequence = @last_message_sequence + 1
				  , follower_badge = 0
				  , follower_is_muted = FALSE
				  , follower_is_blocked = FALSE
				  , follower_is_deleted = FALSE
				  , follower_auto_delete = NULL
				WHERE cid = @cid;
				SET @already_exist = FALSE;
				INSERT INTO `luna_dev_db`.`chat_uid2cid` (uid, cid, mda)
				VALUES (@starter_id, @cid, @cra), (@follower_id, @cid, @cra);
			-- Only the starter has already deleted the chat, so it will see the timeline as the other side.
			ELSEIF (@starter_is_deleted) THEN
				UPDATE `luna_dev_db`.`chat_meta`
				SET kivi = kivi_
				   , starter_start_sequence = follower_start_sequence 
				   , starter_delivered_sequence = follower_start_sequence
				   , starter_seen_sequence = follower_start_sequence
				   , starter_badge = 0
				   , starter_is_deleted = FALSE
				WHERE cid = @cid;
				INSERT INTO `luna_dev_db`.`chat_uid2cid` (uid, cid, mda)
				VALUES (@starter_id, @cid, @cra);
			-- Only the follower has already deleted the chat, so it will see the timeline as the other side.
			ELSEIF (@follower_is_deleted) THEN
				UPDATE `luna_dev_db`.`chat_meta`
				SET kivi = kivi_
				   , follower_start_sequence = starter_start_sequence
				   , follower_delivered_sequence = starter_start_sequence
				   , follower_seen_sequence = starter_start_sequence
				   , follower_badge = 0 
				   , follower_is_deleted = FALSE
				WHERE cid = @cid;
				INSERT INTO `luna_dev_db`.`chat_uid2cid` (uid, cid, mda)
				VALUES (@follower_id, @cid, @cra);
			END IF;
		END IF;
		
		-- When creating a chat for the first time (or when both sides of the chat have already deleted it), we add CREATE action on the timeline.
		IF (@already_exist = FALSE) THEN
			SELECT last_message_sequence FROM `luna_dev_db`.`chat_meta` WHERE cid = @cid INTO @last_message_sequence;
			SET @new_last_message_sequence = @last_message_sequence + 1;
			IF (starter_id_ = @starter_id) THEN
				SET @writer = starter_id_;
			END IF;
			IF (starter_id_ = @follower_id) THEN
				SET @writer = follower_id_;
			END IF;
			INSERT INTO `luna_dev_db`.`chat_message` (cid, cra, type, sequence, writer, body)
			VALUES (@cid, @cra, 'CREATE', @new_last_message_sequence, @writer, "");
			UPDATE `luna_dev_db`.`chat_meta` 
			SET last_message_sequence = @new_last_message_sequence
			WHERE cid = @cid;
		END IF;
	
		-- And finally, we return the last state of chat meta..
		SELECT 3 AS 'RC'
		     , @already_exist AS 'ALREADR_EXIST'
			 , cid AS 'CID'
			 , cra AS 'CRA'
			 , starter_id AS 'STARTER_ID'
			 , follower_id AS 'FOLLOWER_ID'
			 , last_message_sequence AS 'LAST_MESSAGE_SEQUENCE'
			 , last_event_sequence AS 'LAST_EVENT_SEQUENCE'
			 , pinned_messages AS 'PINNED_MESSAGES'
			 , starter_start_sequence AS 'STARTER_START_SEQUENCE'
			 , starter_delivered_sequence AS 'STARTER_DELIVERED_SEQUENCE'
			 , starter_seen_sequence AS 'STARTER_SEEN_SEQUENCE'
			 , starter_badge AS 'STARTER_BADGE'
			 , starter_is_muted AS 'STARTER_IS_MUTED'
			 , starter_is_blocked AS 'STARTER_IS_BLOCKED'
			 , starter_is_deleted AS 'STARTER_IS_DELETED'
			 , starter_auto_delete AS 'STARTER_AUTO_DELETE'
			 , follower_start_sequence AS 'FOLLOWER_START_SEQUENCE'
			 , follower_delivered_sequence AS 'FOLLOWER_DELIVERED_SEQUENCE'
			 , follower_seen_sequence AS 'FOLLOWER_SEEN_SEQUENCE'
			 , follower_badge AS 'FOLLOWER_BADGE'
			 , follower_is_muted AS 'FOLLOWER_IS_MUTED'
			 , follower_is_blocked AS 'FOLLOWER_IS_BLOCKED'
			 , follower_is_deleted AS 'FOLLOWER_IS_DELETED'
			 , follower_auto_delete AS 'FOLLOWER_AUTO_DELETE' 
			 , kivi AS 'KIVI'
		FROM `luna_dev_db`.`chat_meta` 
		WHERE cid = @cid;
	END IF;
END
%
DELIMITER ;

-- EXAMPLE:
CALL `luna_dev_db`.`sp_chat_add`(2, 1, "{}");
