USE luna_dev_db;

DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_get_chat_storage`%
CREATE PROCEDURE `luna_dev_db`.`sp_get_chat_storage`(
      IN cid_ BIGINT
    , IN uid_ BIGINT  
    , IN type_ ENUM('FILE', 'LINK')  
    , IN from_message_sequence_ BIGINT
	, IN length_ INT 
)
BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: DONE	
	 
	--  DECLARE EXIT HANDLER FOR SQLEXCEPTION
	-- BEGIN
	-- 	ROLLBACK;
	-- 	SELECT 1 AS 'RC';
	-- END;

	-- Be careful that the latest version of chat_meta is available in the cache layer and we trust it,
	-- Therefore, in order to reduce the processing load, we do not check the correctness of the information again.
	
	SELECT 2 AS 'RC'
	     , cra AS 'CRA'
	     , type AS 'TYPE'
	     , sequence AS 'SEQUENCE'
	     , mime AS 'WRITER'
	     , body AS 'BODY'
	     , oid AS 'OBJECTS'
	FROM `luna_dev_db`.`chat_storage`
	WHERE uid = uid_ AND cid = cid_ AND type = type_ AND sequence >= from_message_sequence_
	ORDER BY sequence DESC
	LIMIT length_; 
	
END
%
DELIMITER ;

-- EXAMPLE
-- CALL `luna_dev_db`.`sp_get_chat_storage`(6, 1, 'FILE', 1, 10);

