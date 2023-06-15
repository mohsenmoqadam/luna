FROM debian:bullseye AS build

RUN apt-get update -y
RUN apt-get install build-essential git erlang -y

WORKDIR /tmp
RUN git clone https://github.com/erlang/rebar3.git
WORKDIR /tmp/rebar3
RUN ./bootstrap
RUN cp ./rebar3 /bin

WORKDIR /tmp
RUN git clone https://github.com/mohsenmoqadam/luna.git
WORKDIR /tmp/luna
RUN make rel-prod


