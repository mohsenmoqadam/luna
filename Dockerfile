#=== Prepare DEV Environment
FROM debian:bullseye AS dev
WORKDIR /tmp
RUN echo "nameserver 178.22.122.100\nnameserver 185.51.200.2" > /etc/resolv.conf \
    && apt-get update \
    && apt-get install build-essential git erlang -y \
    && git clone https://github.com/erlang/rebar3.git \
    && cd /tmp/rebar3 \
    && ./bootstrap \
    && cp ./rebar3 /bin \
    && cd /tmp \
    && git clone https://github.com/mohsenmoqadam/luna.git \
    && cd /tmp/luna \
    && make rel-dock

#=== Prepare DOCK Environment
FROM debian:bullseye AS dock
WORKDIR /srv
RUN echo "nameserver 178.22.122.100\nnameserver 185.51.200.2" > /etc/resolv.conf \
    && apt-get update \
    && apt-get install -y curl \
    && curl -LsS -O https://downloads.mariadb.com/MariaDB/mariadb_repo_setup \
    && chmod 777 ./mariadb_repo_setup \
    && ./mariadb_repo_setup --mariadb-server-version=10.10.5 \
    && apt-get install -y --no-install-recommends mariadb-server mariadb-backup \
    && apt-get install -y supervisor
RUN mkdir /srv/script    
COPY --from=dev /tmp/luna/_build/dock/rel/luna/luna-1.0.0.tar.gz .
COPY --from=dev /tmp/luna/script /srv/script
COPY --from=dev /tmp/luna/Version . 
RUN tar -zxvf luna-1.0.0.tar.gz \
    && rm luna-1.0.0.tar.gz \ 
    && mv script/supervisord/supervisord.conf /etc/supervisor/supervisord.conf \
    && mv script/mariadb/50-server.cnf /etc/mysql/mariadb.conf.d/50-server.cnf \
    && sed -i 's/luna-version/'`cat Version`'/g' /etc/supervisor/supervisord.conf \
    && mkdir -p /var/run/mysqld \
    && chown root:mysql /var/run/mysqld \
    && chmod 774 /var/run/mysqld \
    && mkdir -p /srv/log/mariadb \
    && chown root:mysql /srv/log/mariadb \
    && chmod 774 /srv/log/mariadb \
    && mkdir -p /srv/log/luna \
    && chmod 777 /srv/log/luna  
CMD ["/usr/bin/supervisord"]