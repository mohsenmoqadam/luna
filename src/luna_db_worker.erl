-module(luna_db_worker).

-behaviour(gen_server).

%% API
-export([ start_link/1
	]).

%% gen_server callbacks
-export([ init/1
	, handle_call/3
	, handle_cast/2
	, handle_info/2
	, terminate/2
	, code_change/3
	, format_status/2
	]).

-include("luna.hrl").

-record( state
       , { index     :: non_neg_integer()
	 , db_ip     :: string()
	 , db_port   :: non_neg_integer()
	 , db_name   :: string()
	 , db_user   :: string()
	 , db_pass   :: string()
	 , conn      :: pid()
	 , conn_mntr :: reference()
	 , conn_retry_interval :: non_neg_integer()
	 }
       ).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Index) -> gen_server:start_link(?MODULE, [Index], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([Index]) ->
    process_flag(trap_exit, true),
    {ok, #{ 'db.ip' := DbIp
	  , 'db.port' := DbPort 
	  , 'db.name' := DbName
	  , 'db.user' := DbUser
	  , 'db.pass' := DbPass
	  , 'db.conn.retry.interval' := DbConnRetryInterval
	  }
    } = luna_conf:get('db.conf'),
    gen_server:cast(self(), connect),
    {ok, #state{ index = Index
	       , db_ip = DbIp
	       , db_port = DbPort
	       , db_name = DbName
	       , db_user = DbUser
	       , db_pass = DbPass
	       , conn_retry_interval = DbConnRetryInterval
	       }
    }.

handle_call( {do_query, _Query, _Params}
	   , _From
	   , #state{conn = undefined} = State
	   ) ->
    {reply, {error, db_conn_not_exist}, State};
handle_call( {do_query, Query, Params}
	   , _From
	   , #state{conn = Conn} = State
	   ) ->
    {reply, mysql:query(Conn, Query, Params), State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast( connect
	   , #state{ db_ip = DbIP
		   , db_port = DbPort
		   , db_name = DbName
		   , db_user = DbUser
		   , db_pass = DbPass
		   , conn_retry_interval = DbConnRetryInterval
		   } = State
	   ) ->
    NewState = case mysql:start_link( [ {host, DbIP}
				      , {port, DbPort}
				      , {user, DbUser}
				      , {keep_alive, true}
				      , {password, DbPass}
				      , {database, DbName}
				      ]
				    ) of
		   {ok, Conn} ->
		       ?LOG_INFO("DB connection established: ~p", [Conn]),
		       luna_db_pool:add_worker(self()),
		       ConnMntr = erlang:monitor(process, Conn),
		       State#state{conn = Conn, conn_mntr = ConnMntr};
		   {error, _} ->
		       erlang:send_after(DbConnRetryInterval, self(), do_connect),
		       State#state{conn = undefined, conn_mntr = undefined}
	       end,
    {noreply, NewState};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info( {'DOWN', ConnMntr, process, Conn, Reason}
	   , #state{ conn = Conn
		   , conn_mntr = ConnMntr
		   , conn_retry_interval = ConnRetryInterval
		   } = State
	   ) ->
    ?LOG_ERROR("DB connection ~p killed with reason: ~p", [Conn, Reason]),
    luna_db_pool:del_worker(self()),
    erlang:demonitor(ConnMntr),
    erlang:send_after(ConnRetryInterval, self(), do_connect),
    NewState = State#state{conn = undefined, conn_mntr = undefined},
    {noreply, NewState};

handle_info(do_connect, State) ->
    gen_server:cast(self(), connect),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    luna_db_pool:del_worker(self()),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
    
    
