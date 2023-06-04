DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_del`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_del`(
	  IN cid_ BIGINT
	, IN uid_ BIGINT
)
BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: INVALID CID
	-- 3: STARTER HAS ALREADY BEEN DELETED
	-- 4: FOLLOWER HAS ALREADY BEEN DELETED
	-- 5: STARTER DELETE THE CHAT
	-- 6: FOLLOWER DELETE THE CHAT
	 
	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
		ROLLBACK;
		SELECT 1 AS 'RC';
	END;
   
   	SET @mda = NOW();
   	SET @cid = NULL;
	SET @starter_is_deleted = TRUE;
	SET @follower_is_deleted = TRUE;
	SET @starter_id = NULL;
	SET @follower_id = NULL;
	SET @last_message_sequence = NULL;
  	SET @rc = NULL;
  
	-- Getting some meta if exists.
	SELECT cid, starter_id, follower_id, starter_is_deleted, follower_is_deleted, last_message_sequence FROM `luna_dev_db`.`chat_meta` 
	WHERE (cid = cid_) AND (starter_id = uid_ OR follower_id = uid_)
	INTO @cid, @starter_id, @follower_id, @starter_is_deleted, @follower_is_deleted, @last_message_sequence;
	
	-- Invalid CID
	IF (ISNULL(@cid)) THEN
		SELECT 2 AS 'RC';
	ELSE
		-- TRANSACTION: START
		START TRANSACTION;
		-- Starter has already been deleted.
		IF (@starter_id = uid_ AND @starter_is_deleted = TRUE) THEN	
			SET @rc = 3;	
		-- Follower has already been deleted.
		ELSEIF (@follower_id = uid_ AND @follower_is_deleted = TRUE) THEN
			SET @rc = 4;	
		-- One side of the chat wants to block other side.
		ELSE
			-- Starter wants to delete the chat.
			IF (@starter_id = uid_ ) THEN
				SET @new_last_message_sequence = @last_message_sequence + 1;
				-- Update chat_meta
				UPDATE `luna_dev_db`.`chat_meta`
				SET starter_is_deleted = TRUE
				  , starter_start_sequence = @last_message_sequence
				  , starter_delivered_sequence = @last_message_sequence
				  , starter_seen_sequence = @last_message_sequence
				  , last_message_sequence = @new_last_message_sequence
				  , mda = @mda
				WHERE cid = @cid;
				-- Update chat_uid2cid
				UPDATE `luna_dev_db`.`chat_uid2cid`
				SET mda = @mda
				WHERE cid = @cid;
				-- Insert DELETE action message
				INSERT INTO `luna_dev_db`.`chat_message` (cid, cra, type, sequence, writer, body)
				VALUES (@cid, @cra, 'DELETE', @new_last_message_sequence, @starter_id, "");
			    -- Upddate RC
				SET @rc = 5;
			-- Follower wants to delete the chat.
			ELSEIF(@follower_id = uid_) THEN
				SET @new_last_message_sequence = @last_message_sequence + 1;
				-- Update chat_meta
				UPDATE `luna_dev_db`.`chat_meta`
				SET follower_is_deleted = TRUE
				  , follower_start_sequence = @last_message_sequence
				  , follower_delivered_sequence = @last_message_sequence
				  , follower_seen_sequence = @last_message_sequence
				  , last_message_sequence = @new_last_message_sequence
				  , mda = @mda
				WHERE cid = @cid;
				-- Update chat_uid2cid
				UPDATE `luna_dev_db`.`chat_uid2cid`
				SET mda = @mda
				WHERE cid = @cid;
				-- Insert BLOCK action message
				INSERT INTO `luna_dev_db`.`chat_message` (cid, cra, type, sequence, writer, body)
				VALUES (@cid, @cra, 'DELETE', @new_last_message_sequence, @follower_id, "");
			    -- Upddate RC
				SET @rc = 6;
			END IF;
			
			-- TRANSACTION: END
			COMMIT;	
		END IF;
		-- Since the chat meta has been updated, we send it agian for updating the cache.
		SELECT @rc AS 'RC'
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
		WHERE cid = @cid;
	END IF;	
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_del`(11, 1);
