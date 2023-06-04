DELIMITER %
DROP FUNCTION IF EXISTS `luna_dev_db`.`sf_storage_del`%
CREATE FUNCTION `luna_dev_db`.`sf_storage_del`(
	  IN cid_ BIGINT	
	, IN uid_ BIGINT
	, IN sequence_ BIGINT
)
RETURNS INT
BEGIN
	DELETE FROM `luna_dev_db`.`chat_storage`
	WHERE cid = cid_ AND uid = uid_ AND sequence = sequence_;
	RETURN 0;
END
%
DELIMITER ;

-- EXAMPLE:
-- SET @cra = NOW();
-- SELECT `luna_dev_db`.`sf_storage_del`(6, 1, 3);  