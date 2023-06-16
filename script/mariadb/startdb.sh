#!/bin/bash

if [ ! -f /var/lib/mysql/ibdata1 ];
then
    mysql_install_db
    chown mysql:mysql -R /var/lib/mysql/
    chmod 777 -R /var/lib/mysql/
fi

/usr/sbin/mysqld --user=mysql
