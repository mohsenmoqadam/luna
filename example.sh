# Example:
docker build . -t luna
docker run -v /tmp/luna/mariadb:/var/lib/mysql/ -v /tmp/luna/log:/srv/log --net erlang --ip 172.18.0.2 -d luna
erl -name test@172.18.0.1 -remsh luna@172.18.0.2 -setcookie luna-cookie
