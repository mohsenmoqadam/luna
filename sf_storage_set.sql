DELIMITER %
DROP FUNCTION IF EXISTS `luna_dev_db`.`sf_storage_set`%
CREATE FUNCTION `luna_dev_db`.`sf_storage_set`(
	  IN cid_ BIGINT	
	, IN starter_ BIGINT
	, IN follower_ BIGINT   
	, IN sequence_ BIGINT
	, IN cra_ DATETIME
	, IN objects_ JSON
)
RETURNS INT
BEGIN
	SET @objects_cnt = NULL;
   	
	SELECT JSON_LENGTH(objects_) INTO @objects_cnt;
	IF (@objects_cnt = NULL) THEN 
		SET @objects_cnt = 0; 
	END IF;

	DELETE FROM `luna_dev_db`.`chat_storage`
	WHERE cid = cid_ AND (uid = starter_ OR uid = follower_) AND sequence = sequence_;

	FOR i IN 0..(@objects_cnt - 1)
	DO
		SELECT JSON_EXTRACT(objects_, CONCAT('$[',i,']')) INTO @object;
		SELECT JSON_VALUE(@object, '$.type') INTO @object_type;
		IF (@object_type = 'FILE') THEN
			SELECT JSON_VALUE(@object, '$.body') INTO @object_body;
			SELECT JSON_VALUE(@object, '$.mime') INTO @object_mime;
			SELECT JSON_VALUE(@object, '$.oid') INTO @object_oid;
			INSERT INTO `luna_dev_db`.`chat_storage` (`uid`, `cid`, `cra`, `type`, `sequence`, `mime`, `body`, `oid`) 
			VALUES (starter_, cid_, cra_, @object_type, sequence_, @object_mime, @object_body, @object_oid)
			     , (follower_, cid_, cra_, @object_type, sequence_, @object_mime, @object_body, @object_oid);
		ELSEIF (@object_type = 'LINK') THEN
			SELECT JSON_VALUE(@object, '$.body') INTO @object_body;
			INSERT INTO `luna_dev_db`.`chat_storage` (`uid`, `cid`, `cra`, `type`, `sequence`, `body`) 
			VALUES (starter_, cid_, cra_, @object_type, sequence_, @object_body)
		         , (follower_, cid_, cra_, @object_type, sequence_, @object_body);
		END IF;
	END FOR;
	RETURN @objects_cnt;
END
%
DELIMITER ;

-- EXAMPLE:
-- SET @cra = NOW();
-- SELECT `luna_dev_db`.`sf_storage_set`(6, 1, 2, 3, @cra, '[{"type": "FILE", "body":"--BODY-S--", "mime":"image/png", "oid": "--OID-S--"}, {"type": "LINK", "body": "https://example.com/s-any..."}]');  