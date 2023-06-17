FROM debian:bullseye AS build

RUN echo "nameserver 178.22.122.100" > /etc/resolv.conf \
    && apt-get update \
    && apt-get install build-essential git erlang -y

WORKDIR /tmp
RUN echo "nameserver 178.22.122.100" > /etc/resolv.conf \
    && git clone https://github.com/erlang/rebar3.git
WORKDIR /tmp/rebar3
RUN ./bootstrap \
    && cp ./rebar3 /bin

WORKDIR /tmp
RUN echo "nameserver 178.22.122.100" > /etc/resolv.conf \
    && git clone https://github.com/mohsenmoqadam/luna.git
WORKDIR /tmp/luna
RUN make rel-prod

FROM debian:bullseye AS prod
WORKDIR /srv

COPY --from=build /tmp/luna/_build/prod/rel/luna/luna-1.0.0.tar.gz . 
RUN tar -zxvf luna-1.0.0.tar.gz

RUN echo "nameserver 178.22.122.100" > /etc/resolv.conf \
    && apt-get update \
    && apt-get install -y curl \
    && curl -LsS -O https://downloads.mariadb.com/MariaDB/mariadb_repo_setup \
    && chmod 777 ./mariadb_repo_setup \
    && ./mariadb_repo_setup --mariadb-server-version=10.10.5 \
    && apt-get install -y --no-install-recommends mariadb-server mariadb-backup \
    && apt-get install -y supervisor 

RUN mkdir /srv/mariadb
COPY supervisord.conf /etc/supervisor/supervisord.conf    
COPY script/mariadb/*.sql /srv/mariadb/
COPY script/mariadb/*.sh /srv/mariadb/
COPY script/mariadb/50-server.cnf /etc/mysql/mariadb.conf.d/50-server.cnf
COPY script/erlang/start_luna.sh /srv/bin/start_luna.sh
ARG MARIADB_MYSQL_SOCKET_DIRECTORY='/var/run/mysqld'
RUN mkdir -p $MARIADB_MYSQL_SOCKET_DIRECTORY \
    && chown root:mysql $MARIADB_MYSQL_SOCKET_DIRECTORY \
    && chmod 774 $MARIADB_MYSQL_SOCKET_DIRECTORY
RUN echo "nameserver 178.22.122.100" > /etc/resolv.conf \
    && apt-get install nano -y    
#CMD ["/usr/bin/supervisord"]