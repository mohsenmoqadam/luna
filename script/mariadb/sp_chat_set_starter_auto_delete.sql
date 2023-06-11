DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_set_starter_auto_delete`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_set_starter_auto_delete`(
	  IN in_cid BIGINT
	, IN in_auto_delete INT
)
BEGIN
	-- RC:
	-- 1: EXCEPTION
	-- 2: DONE	
	
	DECLARE v_mda DATETIME(6);

	DECLARE EXIT HANDLER FOR SQLEXCEPTION
	BEGIN
		ROLLBACK;
		SELECT 1 AS 'RC';
	END;
   
	-- Be careful that the latest version of chat_meta is available in the cache layer and we trust it,
	-- Therefore, in order to reduce the processing load, we do not check the correctness of the information again.

   	SET v_mda = NOW(6);
   
	-- Update chat_meta
	UPDATE luna_dev_db.chat_meta 
	SET starter_auto_delete = in_auto_delete 
	  , mda = v_mda 
	WHERE cid = in_cid;
	
	-- Return result.
	SELECT 2 AS 'RC', v_mda AS 'MDA';
		
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_set_starter_auto_delete`(6, 60);
