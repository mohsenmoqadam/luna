#!/bin/bash

RESULT=" "

if [ "$2" = "container" ]
then
    while [ "$RESULT" != "EXITED" ];
    do
	echo "Waiting for the MariaDB to init..."
	sleep 1
	RESULT=`supervisorctl status mariadb-init | tr -s ' ' | cut -d ' ' -f 2`
    done
    IP=`ip -4 -o addr show eth0 | tr -s ' ' | cut -d' ' -f 4 | cut -d "/" -f 1`
    sed -i 's/luna_ip/'$IP'/g' /srv/releases/$1/vm.args
    sed -i 's/luna_ip/'$IP'/g' /srv/releases/$1/sys.config
    echo "Starting LUNA..."
    /srv/bin/luna foreground
fi
