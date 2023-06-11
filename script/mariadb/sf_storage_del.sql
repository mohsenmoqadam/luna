DELIMITER %
DROP FUNCTION IF EXISTS `luna_dev_db`.`sf_storage_del`%
CREATE FUNCTION `luna_dev_db`.`sf_storage_del`(
	  IN in_cid BIGINT	
	, IN in_uid BIGINT
	, IN in_sequence BIGINT
)
RETURNS INT
BEGIN
	DELETE FROM `luna_dev_db`.`chat_storage`
	WHERE cid = in_cid AND uid = in_uid AND sequence = in_sequence;
	RETURN 0;
END
%
DELIMITER ;

-- EXAMPLE:
-- SET @cra = NOW();
-- SELECT `luna_dev_db`.`sf_storage_del`(6, 1, 3);  