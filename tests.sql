DELIMITER %
DROP PROCEDURE IF EXISTS `luna_dev_db`.`sp_set_chat_message`%
CREATE PROCEDURE `luna_dev_db`.`sp_set_chat_message`(
	IN objects JSON 
)
BEGIN
	DECLARE EXIT HANDLER FOR SQLEXCEPTION
    BEGIN
            ROLLBACK;
             SELECT 1 AS 'RC';
    END;
   	
   	
	
	END
%
DELIMITER ;

-- EXAMPLE
CALL `luna_dev_db`.`sp_set_chat_message`('[{"type": "FILE", "body": "file1", "mime": "image/png", "oid": "--OID1--"}, {"type": "LINK", "body": "http://any.com"}]');

SET @j = '[{"type": "FILE", "body": "file1", "mime": "image/png", "oid": "--OID1--"}, {"type": "LINK", "body": "http://any.com"}]';
SELECT JSON_EXTRACT(@j, '$[0]') INTO @e;
SELECT JSON_VALUE(@e, '$.type') INTO @t;
SELECT @t;
SELECT JSON_LENGTH(''); -- NULL
SELECT JSON_LENGTH('{}'); -- 0
SELECT JSON_LENGTH('[]'); -- 0
SELECT JSON_LENGTH(NULL); -- NULL

DROP TABLE IF EXISTS test;
CREATE TEMPORARY TABLE test (a INT);

SELECT JSON_LENGTH(@j) INTO @l; 


SET @j = '[{"type": "FILE", "body": "file1", "mime": "image/png", "oid": "--OID1--"}, {"type": "LINK", "body": "http://any.com"}]';
DROP TABLE IF EXISTS test;
CREATE TEMPORARY TABLE test 
( type ENUM('FILE', 'LINK')
, body VARCHAR(128)
, mime VARCHAR(128)
, oid VARCHAR(128)
);
SELECT JSON_LENGTH(@j) INTO @l;
DELIMITER %
FOR i IN 0..(@l-1)
DO
	SELECT JSON_EXTRACT(@j, CONCAT('$[',i,']')) INTO @e;
	SELECT JSON_VALUE(@e, '$.type') INTO @t;
	IF (@t = 'FILE') THEN
		SELECT JSON_VALUE(@e, '$.body') INTO @b;
		SELECT JSON_VALUE(@e, '$.mime') INTO @m;
		SELECT JSON_VALUE(@e, '$.oid') INTO  @o;
		INSERT INTO test (type, body, mime, oid) VALUES (@t, @b, @m, @o);
	ELSE
		SELECT JSON_VALUE(@e, '$.body') INTO @b;
		INSERT INTO test (type, body) VALUES (@t, @b);
	END IF;
	
END FOR;
%
DELIMITER ;

SELECT * FROM test;



SET @o1 = '[{"type": "FILE", "body": "file1", "mime": "image/png", "oid": "--OID1--"}, {"type": "LINK", "body": "http://any.com"}]';
SET @cra = NOW();
INSERT INTO luna_dev_db.chat_message (cid, cra, type, `sequence`, writer, body, objects)
VALUES (1, @cra, 'MESSAGE', 1, 'STARTER', "B1", @o1);




