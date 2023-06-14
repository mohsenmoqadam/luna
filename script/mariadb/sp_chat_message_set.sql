DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_message_set`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_message_set`(
	  IN in_cid BIGINT
	, IN in_starter_id BIGINT
	, IN in_follower_id BIGINT
	, IN in_writer_id BIGINT
	, IN in_message_sequence BIGINT
	, IN in_last_event_sequence BIGINT  
	, IN in_version INT
	, IN in_body TEXT
	, IN in_objects JSON
	, IN in_auto_delete INT
	, IN in_kivi JSON
)

BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: INVALID ROLE
	-- 3: INVALID VERSION
	-- 4: DONE
	
	DECLARE v_cra DATETIME(6);
	DECLARE v_mda DATETIME(6);
	DECLARE v_last_event_sequence BIGINT;
	DECLARE v_writer ENUM('STARTER', 'FOLLOWER');
	DECLARE v_role BOOL;
	DECLARE v_version INT;
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
	
   	SELECT writer, version INTO v_writer, v_version
   	FROM `luna_dev_db`.`chat_message`
   	WHERE cid = in_cid AND sequence = in_message_sequence AND type = 'MESSAGE';  
   
   	IF (v_writer = 'STARTER' AND in_writer_id = in_starter_id) THEN
		SET v_role = TRUE;
	ELSEIF (v_writer = 'FOLLOWER' AND in_writer_id = in_follower_id) THEN
		SET v_role = TRUE;
	ELSE
		SET v_role = FALSE;
	END IF;
	
	IF (v_role) THEN
		IF (in_version = v_version) THEN
			SET v_version = in_version + 1;
			-- TRANSACTION: START
			START TRANSACTION;
			-- Update chat_meta
			UPDATE luna_dev_db.chat_meta 
			SET mda = v_mda 
			  , last_event_sequence = v_last_event_sequence 
			WHERE cid = in_cid;
			-- Update message
			UPDATE luna_dev_db.chat_message
			SET mda = v_mda
			  , body = in_body
			  , objects = in_objects
			  , auto_delete = in_auto_delete 
			  , kivi = in_kivi
			  , version = v_version + 1 
			WHERE cid = in_cid AND sequence = in_message_sequence;
			-- Upddate chat_storage
			IF (ISNULL(in_objects)) THEN
				SELECT `luna_dev_db`.`sf_storage_del`(in_cid, in_starter_id, in_message_sequence) INTO v_objects_cnt;
				SELECT `luna_dev_db`.`sf_storage_del`(in_cid, in_follower_id, in_message_sequence) INTO v_objects_cnt;
			ELSE
			 	SELECT `luna_dev_db`.`sf_storage_set`(in_cid, in_starter_id, in_follower_id, in_message_sequence, v_cra, in_objects) INTO v_objects_cnt;
			END IF;
			-- TRANSACTION: END
			COMMIT;
			SELECT 4 AS 'RC', v_mda AS 'MDA', v_last_event_sequence AS 'LAST_EVENT_SEQUENCE', v_version AS 'LAST_VERSION';
		ELSE
			SELECT 3 AS 'RC';
		END IF;
	ELSE
		SELECT 2 AS 'RC';
	END IF;
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_message_set`(6, 1, 3, 3, 2, 1, "u-body", null, 0, null);
