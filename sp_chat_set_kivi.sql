DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_set_kivi`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_set_kivi`(
	  IN cid_ BIGINT
	, IN kivi_ JSON
)
BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: INVALID CID
	-- 3: DONE	
	 
	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
		ROLLBACK;
		SELECT 1 AS 'RC';
	END;
   
   	SET @mda = NOW();
   	SET @cid = NULL;
	SET @rc = NULL;
  
	-- Getting some meta if exists.
	SELECT cid FROM `luna_dev_db`.`chat_meta` 
	WHERE cid = cid_ 
	INTO @cid;
	
	-- Invalid CID
	IF (ISNULL(@cid)) THEN
		-- Return result.
		SELECT 2 AS 'RC';
	ELSE
		-- TRANSACTION: START
		START TRANSACTION;
		UPDATE luna_dev_db.chat_meta SET kivi = kivi_ WHERE cid = cid_;
		-- TRANSACTION: END
		COMMIT;
		-- Return result.
		SELECT 3 AS 'RC';
	END IF;	
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_set_kivi`(5, '{"key": "value"}');
