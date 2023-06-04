DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_message_set_action`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_message_set_action`(
	  IN cid_ BIGINT
	, IN message_sequence_ BIGINT  
	, IN actions_ JSON
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
	  
	SET @mda = NOW();
	
   	-- TRANSACTION: START
	START TRANSACTION;
	-- Update chat_meta
	UPDATE luna_dev_db.chat_meta 
	SET mda = @mda  
	WHERE cid = cid_;
	-- Save actions
	UPDATE `luna_dev_db`.`chat_message`
	SET actions = actions_
	WHERE cid = cid_ AND sequence = message_sequence_;	
	-- TRANSACTION: END
	COMMIT;
	
	SELECT 2 AS 'RC', @mda AS 'MDA'; 
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_message_set_action`(6, 1, "{}");
