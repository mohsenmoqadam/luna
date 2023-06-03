DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_add_message`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_add_message`(
	  IN cid_ BIGINT
	, IN writer_ BIGINT
	, IN message_sequence_ BIGINT  
	, IN reply_sequence_ BIGINT
	, IN body_ TEXT
	, IN objects_ JSON
	, IN auto_delete_ INT
	, IN kivi_ TEXT
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
    -- So we suppose:
    --                1. CID is valid Exist (and has not been deleted by the writer).
    --                2. The writer isn't blocked.
    --                3. The writer_id_ is one of starter_id or follower_id
    --                4. The message_sequence_ is already incremented and updated in the cache layer.
      
	SET @rc = NULL;
   	SET @cra = NOW();
   	SET @mda = @cra;
	
   	-- TRANSACTION: START
	START TRANSACTION;
	-- Update chat_meta
	UPDATE luna_dev_db.chat_meta 
	SET mda = @mda 
	  , last_message_sequence = message_sequence_ 
	WHERE cid = cid_;
	-- Update chat_uid2cid 
	UPDATE luna_dev_db.chat_uid2cid SET mda = @mda WHERE cid = cid_;
	-- Save message
	INSERT INTO `luna_dev_db`.`chat_message` (cid, cra, mda, type, sequence, reply_sequence, writer, body, objects, auto_delete, kivi)
	VALUES (cid_, @cra, @mda, 'MESSAGE', message_sequence_, reply_sequence_, writer_, body_, objects_, auto_delete_, kivi_);
	-- TRANSACTION: END
	COMMIT;
	
	SELECT 2 AS 'RC', @cra AS 'CRA'; 
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_add_message`(6, 1, 5, 0, "body", '[{"type": "FILE", "body":"--BODY--", "mime":"image/png", "oid": "--OID--"}, {"type": "LINK", "body": "https://example.com/any..."}]', 0, '{}');
