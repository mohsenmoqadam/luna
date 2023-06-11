DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_chat_set_starter_delivered_sequence`%
CREATE PROCEDURE `luna_dev_db`.`sp_chat_set_starter_delivered_sequence`(
	  IN in_cid BIGINT  
	, IN in_starter_delivered_sequence BIGINT
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
	SET mda = v_mda
	  , starter_delivered_sequence = in_starter_delivered_sequence
	WHERE cid = in_cid;
	
	SELECT 2 AS 'RC', v_mda AS 'MDA'; 
END
%
DELIMITER ;

-- EXAMPLE:
-- CALL `luna_dev_db`.`sp_chat_set_starter_delivered_sequence`(6, 1);
