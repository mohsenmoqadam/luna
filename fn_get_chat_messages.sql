USE luna_dev_db;

DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`fn_get_chat_messages`%
CREATE PROCEDURE `luna_dev_db`.`fn_get_chat_messages`(
      IN cid_ BIGINT
    , IN from_message_sequence_ BIGINT
	, IN length_ INT 
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
	
	SELECT 2 AS 'RC'
	     , cra AS 'CRA'
	     , mda AS 'MDA'
	     , type AS 'TYPE'
	     , sequence AS 'SEQUENCE'
	     , reply_sequence AS 'REPLY_SEQUENCE'
	     , writer AS 'WRITER'
	     , body AS 'BODY'
	     , objects AS 'OBJECTS'
	     , actions AS 'ACTIONS'
	     , is_deleted_by_starter AS 'IS_DELETED_BY_STARTER'
	     , is_deleted_by_follower AS 'IS_DELETED_BY_FOLLOWER'
	     , auto_delete AS 'AUTO_DELETE'
	     , kivi AS 'KIVI'
	     , version AS 'VERSION'
	FROM luna_dev_db.chat_message
	WHERE cid = cid_ AND sequence >= from_message_sequence_
	ORDER BY sequence DESC
	LIMIT length_; 
	
END
%
DELIMITER ;

-- EXAMPLE
-- CALL `luna_dev_db`.`fn_get_chat_messages`(6, 0, 5);

