DELIMITER %
DROP TRIGGER IF EXISTS `luna_dev_db`.`tr_storage_add`%
CREATE TRIGGER `luna_dev_db`.`tr_storage_add`
AFTER INSERT ON `luna_dev_db`.`chat_message` 
FOR EACH ROW
BEGIN
	SET @objects_cnt = NULL;
   	SELECT JSON_LENGTH(NEW.objects) INTO @objects_cnt;
	IF (@objects_cnt = NULL) THEN 
		SET @objects_cnt = 0; 
	END IF;
	IF (NEW.type = 'MESSAGE') THEN
		FOR i IN 0..(@objects_cnt - 1)
		DO
			SELECT JSON_EXTRACT(NEW.objects, CONCAT('$[',i,']')) INTO @object;
			SELECT JSON_VALUE(@object, '$.type') INTO @object_type;
			IF (@object_type = 'FILE') THEN
				SELECT JSON_VALUE(@object, '$.body') INTO @object_body;
				SELECT JSON_VALUE(@object, '$.mime') INTO @object_mime;
				SELECT JSON_VALUE(@object, '$.oid') INTO @object_oid;
				INSERT INTO `luna_dev_db`.`chat_storage` (`cid`, `cra`, `type`, `sequence`, `mime`, `body`, `oid`) 
				VALUES (NEW.cid, NEW.cra, @object_type, NEW.sequence, @object_mime, @object_body, @object_oid);
			ELSEIF (@object_type = 'LINK') THEN
				SELECT JSON_VALUE(@object, '$.body') INTO @object_body;
				INSERT INTO `luna_dev_db`.`chat_storage` (`cid`, `cra`, `type`, `sequence`, `body`) 
				VALUES (NEW.cid, NEW.cra, @object_type, NEW.sequence, @object_body);
			END IF;
		END FOR;	
	END IF;
END
%
DELIMITER ;