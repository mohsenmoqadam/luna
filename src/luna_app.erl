-module(luna_app).

-behaviour(application).

-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    ok = application:ensure_started(bingo),
    luna_rgst:init(),
    luna_db_sup:start_link(),
    luna_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
