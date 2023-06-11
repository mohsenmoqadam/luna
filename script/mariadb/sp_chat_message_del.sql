DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_message_del`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_message_del`(
	  IN in_cid BIGINT
	, IN in_starter_id BIGINT
	, IN in_follower_id BIGINT
	, IN in_writer_id BIGINT
	, IN in_message_sequence BIGINT
	, IN in_last_event_sequence BIGINT	
	, IN in_del_type ENUM('ONE', 'EVERYONE')  
)

BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: INVALID ROLE
	-- 3: DONE
	
	DECLARE v_cra DATETIME(6);
	DECLARE v_mda DATETIME(6);
	DECLARE v_last_event_sequence BIGINT;
	DECLARE v_writer ENUM('STARTER', 'FOLLOWER');
	DECLARE v_role BOOL;
	DECLARE v_objects_cnt INT;

	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
		ROLLBACK;
		SELECT 1 AS 'RC';
	END;
   
	-- Be careful that the latest version of chat_meta is available in the cache layer and we trust it,
	-- Therefore, in order to reduce the processing load, we do not check the correctness of the information again.
      
	SET v_cra = NOW(6);
   	SET v_mda = v_cra;
   	SET v_last_event_sequence = in_last_event_sequence + 1;
   	SET v_objects_cnt = 0;
	
   	SELECT writer FROM `luna_dev_db`.`chat_message`
   	WHERE cid = in_cid AND sequence = in_message_sequence AND type = 'MESSAGE' INTO v_writer; 
   	IF (v_writer = 'STARTER' AND in_writer_id = in_starter_id) THEN
		SET v_role = TRUE;
	ELSEIF (v_writer = 'FOLLOWER' AND in_writer_id = in_follower_id) THEN
		SET v_role = TRUE;
	ELSE
		SET v_role = FALSE;
	END IF;
	
	
	-- TRANSACTION: START
	START TRANSACTION;
	-- Update chat_meta
	UPDATE luna_dev_db.chat_meta 
	SET mda = v_mda 
	  , last_event_sequence = v_last_event_sequence 
	WHERE cid = in_cid;
	-- Update message
	IF (in_del_type = 'ONE' AND in_writer_id = in_starter_id) THEN
		SET v_role = TRUE;
		UPDATE luna_dev_db.chat_message
		SET mda = v_mda
		  , is_deleted_by_starter = TRUE
		WHERE cid = in_cid AND sequence = in_message_sequence;
		-- Upddate chat_storage
		SELECT `luna_dev_db`.`sf_storage_del`(in_cid, in_starter_id, in_message_sequence) INTO v_objects_cnt;
	ELSEIF (in_del_type = 'ONE' AND in_writer_id = in_follower_id) THEN
		SET v_role = TRUE;
		UPDATE luna_dev_db.chat_message
		SET mda = v_mda
		  , is_deleted_by_follower = TRUE
		WHERE cid = in_cid AND sequence = in_message_sequence;
		-- Upddate chat_storage
		SELECT `luna_dev_db`.`sf_storage_del`(in_cid, in_follower_id, in_message_sequence) INTO v_objects_cnt;
	ELSEIF (in_del_type = 'EVERYONE' AND v_role = TRUE) THEN
		SET v_role = TRUE;
		UPDATE luna_dev_db.chat_message
		SET mda = v_mda
		  , is_deleted_by_starter = TRUE
		  , is_deleted_by_follower = TRUE
		WHERE cid = in_cid AND sequence = in_message_sequence;
		-- Upddate chat_storage
		SELECT `luna_dev_db`.`sf_storage_del`(in_cid, in_starter_id, in_message_sequence) INTO v_objects_cnt;
		SELECT `luna_dev_db`.`sf_storage_del`(in_cid, in_follower_id, in_message_sequence) INTO v_objects_cnt;
	ELSE
		SET v_role = FALSE;
	END IF;
	-- TRANSACTION: END
	COMMIT;
	
	IF (v_role) THEN
		SELECT 3 AS 'RC', v_mda AS 'MDA', v_last_event_sequence AS 'LAST_EVENT_SEQUENCE';
	ELSE
		SELECT 2 AS 'RC';
	END IF;
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_message_del`(6, 1, 3, 3, 13, 6, 'EVERYONE');
