DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_message_set`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_message_set`(
	  IN cid_ BIGINT
	, IN starter_id_ BIGINT
	, IN follower_id_ BIGINT
	, IN message_sequence_ BIGINT
	, IN last_event_sequence_ BIGINT  
	, IN body_ TEXT
	, IN objects_ JSON
	, IN auto_delete_ INT
	, IN kivi_ JSON
)

BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: DONE
	 
	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
		ROLLBACK;
		SELECT 1 AS 'RC';
	END;
   
	-- Be careful that the latest version of chat_meta is available in the cache layer and we trust it,
	-- Therefore, in order to reduce the processing load, we do not check the correctness of the information again.
      
	SET @cra = NOW();
   	SET @mda = @cra;
   	SET @objects_cnt = 0;
	
   	-- TRANSACTION: START
	START TRANSACTION;
	-- Update chat_meta
	UPDATE luna_dev_db.chat_meta 
	SET mda = @mda 
	  , last_event_sequence = last_event_sequence_ 
	WHERE cid = cid_;
	-- Update chat_uid2cid 
	UPDATE luna_dev_db.chat_uid2cid SET mda = @mda WHERE cid = cid_;
	-- Update message
	UPDATE luna_dev_db.chat_message
	SET mda = @mda
	  , body = body_
	  , objects = objects_
	  , auto_delete = auto_delete_ 
	  , kivi = kivi_
	WHERE cid = cid_ AND sequence = message_sequence_;
	-- Upddate chat_storage
	SELECT `luna_dev_db`.`sf_storage_set`(cid_, starter_id_, follower_id_, message_sequence_, @cra, objects_) INTO @objects_cnt;
	-- TRANSACTION: END
	COMMIT;
	
	SELECT 2 AS 'RC', @cra AS 'CRA', @objects_cnt AS 'OBJECT_COUNT'; 
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_message_set`(6, 1, 2, 3, 1, "body", '[{"type": "FILE", "body":"--BODY--", "mime":"image/png", "oid": "--OID--"}, {"type": "LINK", "body": "https://example.com/any..."}]', 0, '{}');
