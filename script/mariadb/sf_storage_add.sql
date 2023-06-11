DELIMITER %
DROP FUNCTION IF EXISTS `luna_dev_db`.`sf_storage_add`%
CREATE FUNCTION `luna_dev_db`.`sf_storage_add`(
	  IN in_cid BIGINT	
	, IN in_starter BIGINT
	, IN in_follower BIGINT   
	, IN in_sequence BIGINT
	, IN in_cra DATETIME
	, IN in_objects JSON
)
RETURNS INT
BEGIN
	DECLARE v_objects_cnt INT DEFAULT 0;
	DECLARE v_object JSON;
	DECLARE v_object_type VARCHAR(8);
	DECLARE v_object_body VARCHAR(128);
	DECLARE v_object_mime VARCHAR(128);
	DECLARE v_object_oid VARCHAR(128);
	
	SELECT JSON_LENGTH(in_objects) INTO v_objects_cnt;
	FOR i IN 0..(v_objects_cnt - 1)
	DO
		SELECT JSON_EXTRACT(in_objects, CONCAT('$[',i,']')) INTO v_object;
		SELECT JSON_VALUE(v_object, '$.type') INTO v_object_type;
		IF (v_object_type = 'FILE') THEN
			SELECT JSON_VALUE(v_object, '$.body') INTO v_object_body;
			SELECT JSON_VALUE(v_object, '$.mime') INTO v_object_mime;
			SELECT JSON_VALUE(v_object, '$.oid') INTO v_object_oid;
			INSERT INTO `luna_dev_db`.`chat_storage` (`uid`, `cid`, `cra`, `type`, `sequence`, `mime`, `body`, `oid`) 
			VALUES (in_starter, in_cid, in_cra, v_object_type, in_sequence, v_object_mime, v_object_body, v_object_oid)
			     , (in_follower, in_cid, in_cra, v_object_type, in_sequence, v_object_mime, v_object_body, v_object_oid);
		ELSEIF (v_object_type = 'LINK') THEN
			SELECT JSON_VALUE(v_object, '$.body') INTO v_object_body;
			INSERT INTO `luna_dev_db`.`chat_storage` (`uid`, `cid`, `cra`, `type`, `sequence`, `body`) 
			VALUES (in_starter, in_cid, in_cra, v_object_type, in_sequence, v_object_body)
		         , (in_follower, in_cid, in_cra, v_object_type, in_sequence, v_object_body);
		END IF;
	END FOR;
	RETURN v_objects_cnt;
END
%
DELIMITER ;

-- EXAMPLE:
-- SET @cra = NOW(6);
-- SELECT `luna_dev_db`.`sf_storage_set`(6, 1, 2, 3, @cra, '[{"type": "FILE", "body":"--BODY-U--", "mime":"image/png", "oid": "--OID-U--"}, {"type": "LINK", "body": "https://example.com/u-any..."}]');  

