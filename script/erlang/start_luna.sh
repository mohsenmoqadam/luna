#!/bin/bash

RESULT=" "

if [ "$1" = "container" ]
then
    while [ "$RESULT" != "EXITED" ];
    do
	echo "Waiting for the MariaDB to init..."
	sleep 1
	RESULT=`supervisorctl status mariadb-init | tr -s ' ' | cut -d ' ' -f 2`
    done
    echo "Starting LUNA..."
    /srv/bin/luna foreground
fi
