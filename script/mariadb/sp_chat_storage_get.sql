USE luna_dev_db;

DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_storage_get`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_storage_get`(
      IN in_cid BIGINT
    , IN in_uid BIGINT  
    , IN in_type ENUM('FILE', 'LINK')  
    , IN in_from_message_sequence BIGINT
	, IN in_length INT 
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
	     , sequence AS 'SEQUENCE'
	     , mime AS 'WRITER'
	     , body AS 'BODY'
	     , oid AS 'OBJECTS'
	FROM `luna_dev_db`.`chat_storage`
	WHERE uid = in_uid AND cid = in_cid AND type = in_type AND sequence >= in_from_message_sequence
	LIMIT in_length; 
	
END
%
DELIMITER ;

-- EXAMPLE
-- CALL `luna_dev_db`.`sp_chat_storage_get`(5, 1, 'FILE', 1, 10);

