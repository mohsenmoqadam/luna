DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_message_add`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_message_add`(
	  IN in_cid BIGINT
	, IN in_starter_id BIGINT
	, IN in_follower_id BIGINT
	, IN in_writer ENUM('STARTER', 'FOLLOWER')
	, IN in_last_message_sequence BIGINT  
	, IN in_reply_sequence BIGINT
	, IN in_body TEXT
	, IN in_objects JSON
	, IN in_auto_delete INT
	, IN in_kivi JSON
)

BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: DONE
	
	DECLARE v_cra DATETIME(6);
	DECLARE v_mda DATETIME(6);
	DECLARE v_last_message_sequence BIGINT;
	DECLARE v_writer VARCHAR(16);
	DECLARE v_objects_cnt INT;

	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
	 	ROLLBACK;
		SELECT 1 AS 'RC';
	END;
   
	-- Be careful that the latest version of chat_meta is available in the cache layer and we trust it,
	-- Therefore, in order to reduce the processing load, we do not check the correctness of the information again.
	-- So we suppose:
	--                1. CID is valid Exist (and has not been deleted by the writer).
	--                2. The writer isn't blocked.
	--                3. The writer_id_ is one of starter_id or follower_id
	--                4. The message_sequence_ is already incremented and updated in the cache layer.
      
	SET v_cra = NOW(6);
   	SET v_mda = v_cra;
   	SET v_last_message_sequence = in_last_message_sequence + 1;
   	
   	-- TRANSACTION: START
	START TRANSACTION;
	-- Update chat_meta
	UPDATE luna_dev_db.chat_meta 
	SET mda = v_mda 
	  , last_message_sequence = v_last_message_sequence 
	WHERE cid = in_cid;
	-- Save message
	INSERT INTO `luna_dev_db`.`chat_message` (cid, cra, mda, type, sequence, reply_sequence, writer, body, objects, auto_delete, kivi, version)
	VALUES (in_cid, v_cra, v_mda, 'MESSAGE', v_last_message_sequence, in_reply_sequence, in_writer, in_body, in_objects, in_auto_delete, in_kivi, 0);
	-- Upddate chat_storage
	IF (!ISNULL(in_objects)) THEN
		SELECT `luna_dev_db`.`sf_storage_add`(in_cid, in_starter_id, in_follower_id, v_last_message_sequence, v_cra, in_objects) INTO v_objects_cnt;
	END IF;
	-- TRANSACTION: END
	COMMIT;
	
	SELECT 2 AS 'RC', v_mda AS 'CHAT_MDA', v_cra AS 'MESSAGE_CRA', v_last_message_sequence AS 'LAST_MESSAGE_SEQUENCE'; 
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_message_add`(5, 1, 5, 'STARTER', 14, 0, "body", '[{"type": "FILE", "body":"--BODY-U--", "mime":"image/png", "oid": "--OID-U--"}, {"type": "LINK", "body": "https://example.com/u-any..."}]', 0, '{}');
-- CALL `luna_dev_db`.`sp_chat_message_add`(5, 1, 3, 'STARTER', 15, 0, "body", '[{"type": "FILE", "body":"--BODY-U--", "mime":"image/png", "oid": "--OID-U--"}, {"type": "LINK", "body": "https://example.com/u-any..."}]', 0, null);
-- CALL `luna_dev_db`.`sp_chat_message_add`(5, 1, 3, 'FOLLOWER', 16, 0, "body", null, 0, null);
