#!/bin/bash

DB_IP="127.0.0.1"
DB_PORT=3305
DB_USER="luna_dev_user"
DB_PASS="luna_dev_pass"

FILES="./*.sql"

NOCOLOR='\033[0m'
RED='\033[0;31m'
GREEN='\033[0;32m'
ORANGE='\033[0;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'
LIGHTGRAY='\033[0;37m'
DARKGRAY='\033[1;30m'
LIGHTRED='\033[1;31m'
LIGHTGREEN='\033[1;32m'
YELLOW='\033[1;33m'
LIGHTBLUE='\033[1;34m'
LIGHTPURPLE='\033[1;35m'
LIGHTCYAN='\033[1;36m'
WHITE='\033[1;37m'

table="tables.sql"
default="defaults.sql"
init="init.sql"

#read -p "Are you want to create tables again [ Y:Yes ]? " is_create_again
#case $is_create_again in
#    Y)
#        printf "Running $RED$table$NOCOLOR ... \t" | expand -t 70
#        mysql -h $DB_IP -P $DB_PORT -u $DB_USER --password=$DB_PASS < $table
#        printf "[$GREEN Done $NOCOLOR]\n"
#        ;;
#    *)
#        ;;
#esac

#read -p "Are you want to insert defaults [ Y:Yes ]? " is_insert_defaults
#case $is_insert_defaults in
#    Y)
#        printf "Running $RED$default$NOCOLOR ... \t" | expand -t 70
#        mysql -h $DB_IP -P $DB_PORT -u $DB_USER --password=$DB_PASS < $default
#        printf "[$GREEN Done $NOCOLOR]\n"
#        ;;
#    *)
#        ;;
#esac

#=== Init DB
printf "Running $RED$init$NOCOLOR ... \t" | expand -t 70
mysql -h $DB_IP -P $DB_PORT -u $DB_USER --password=$DB_PASS < $init
if [ $? -eq 0 ] 
then
    printf "[$GREEN Done $NOCOLOR]\n"
else
    printf "[$RED Error $NOCOLOR]\n"
fi

#=== Create tables
printf "Running $RED$table$NOCOLOR ... \t" | expand -t 70
mysql -h $DB_IP -P $DB_PORT -u $DB_USER --password=$DB_PASS < $table
if [ $? -eq 0 ] 
then
    printf "[$GREEN Done $NOCOLOR]\n"
else
    printf "[$RED Error $NOCOLOR]\n"
fi

#=== Insert defaults
printf "Running $RED$default$NOCOLOR ... \t" | expand -t 70
mysql -h $DB_IP -P $DB_PORT -u $DB_USER --password=$DB_PASS < $default
if [ $? -eq 0 ] 
then
    printf "[$GREEN Done $NOCOLOR]\n"
else
    printf "[$RED Error $NOCOLOR]\n"
fi

for f in $FILES
do
    name=${f##*/}
    if [ "$name" != "$table" ] && [ "$name" != "$default" ] && [ "$name" != "$init" ]
    then
        printf "Running $ORANGE$name$NOCOLOR ... \t" | expand -t 35
        mysql -h $DB_IP -P $DB_PORT -u $DB_USER --password=$DB_PASS < $f
	retVal=$?
	if [ $? -eq 0 ] 
	then
	    printf "[$GREEN Done $NOCOLOR]\n"
	else
	    printf "[$RED Error $NOCOLOR]\n"
	fi
    fi
done

echo ""
