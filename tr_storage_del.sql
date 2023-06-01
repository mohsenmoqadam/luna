DELIMITER %
DROP TRIGGER IF EXISTS `luna_dev_db`.`tr_storage_del`%
CREATE TRIGGER `luna_dev_db`.`tr_storage_del`
AFTER DELETE ON `luna_dev_db`.`chat_message` 
FOR EACH ROW
BEGIN
	IF (OLD.type = 'MESSAGE') THEN
		DELETE FROM `luna_dev_db`.`chat_storage` WHERE cid = OLD.cid AND sequence = OLD.sequence;	
	END IF;
END
%
DELIMITER ;