DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_message_del_by_follower`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_message_del_by_follower`(
	  IN cid_ BIGINT
	, IN follower_id_ BIGINT
	, IN message_sequence_ BIGINT
	, IN last_event_sequence_ BIGINT
)

BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: DONE
	 
	-- DECLARE EXIT HANDLER FOR SQLEXCEPTION
	-- BEGIN
	-- 	ROLLBACK;
	-- 	SELECT 1 AS 'RC';
	-- END;
   
	-- Be careful that the latest version of chat_meta is available in the cache layer and we trust it,
	-- Therefore, in order to reduce the processing load, we do not check the correctness of the information again.
	
	-- We get follower_id_ as input because we trust to cache and don't want to run one more query to get it.
	  
	SET @cra = NOW();
   	SET @mda = @cra;
	
   	-- TRANSACTION: START
	START TRANSACTION;
	-- Update chat_meta
	UPDATE luna_dev_db.chat_meta 
	SET mda = @mda 
	  , last_event_sequence = last_event_sequence_ 
	WHERE cid = cid_;
	-- Update chat_uid2cid 
	UPDATE luna_dev_db.chat_uid2cid SET mda = @mda WHERE cid = cid_;
	-- Update chat_message 
	UPDATE luna_dev_db.chat_message 
	SET is_deleted_by_follower = TRUE
	WHERE cid = cid_ AND sequence = message_sequence_; 
	-- Update chat_storage 
	SELECT `luna_dev_db`.`sf_storage_del`(cid_, follower_id_, message_sequence_) INTO @tmp;
	-- TRANSACTION: END
	COMMIT;
	
	SELECT 2 AS 'RC', @cra AS 'CRA'; 
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_message_del_by_follower`(6, 2, 3, 2);
