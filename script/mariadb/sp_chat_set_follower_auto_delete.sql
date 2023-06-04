DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_set_follower_auto_delete`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_set_follower_auto_delete`(
	  IN cid_ BIGINT
	, IN auto_delete_ INT
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
		SET follower_auto_delete = auto_delete_ 
		  , mda = @mda 
	WHERE cid = cid_;
	-- Update chat_uid2cid 
	UPDATE luna_dev_db.chat_uid2cid SET mda = @mda WHERE cid = cid_;
	-- TRANSACTION: END
	COMMIT;

	-- Return result.
	SELECT 2 AS 'RC', @mda AS 'MDA';
		
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_set_follower_auto_delete`(6, 60);
