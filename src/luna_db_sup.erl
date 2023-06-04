-module(luna_db_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    {ok, Workers} = luna_conf:get('db.workers'),
    SupFlags = #{ strategy => one_for_one
		, intensity => 1
		, period => 5
		},    
    WC = [ #{ id => list_to_atom( "luna_db_worker_"
				++ integer_to_list(I)
				)
	    , start => {luna_db_worker, start_link, [I]}
	    , restart => permanent
	    , shutdown => 5000
	    , type => worker
	    , modules => [luna_db_worker]
	    } || I <- lists:seq(1, Workers)
	 ],
    WP = #{ id => luna_db_pool
	  , start => {luna_db_pool, start_link, []}
	  , restart => permanent
	  , shutdown => 5000
	  , type => worker
	  , modules => [luna_db_poll]
	  },
    {ok, {SupFlags, WC ++ [WP]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
