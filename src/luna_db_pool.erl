-module(luna_db_pool).

-behaviour(gen_server).

-export( [ start_link/0
	 , add_worker/1
	 , del_worker/1
	 , do/2
	 ]
       ).

-export( [ init/1
	 , handle_call/3
	 , handle_cast/2
	 , handle_info/2
	 , terminate/2
	 , code_change/3
	 , format_status/2
	 ]
       ).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_worker(WorkerPid) ->
    gen_server:call(?SERVER, {add_worker, WorkerPid}).

del_worker(WorkerPid) ->
    gen_server:call(?SERVER, {del_worker, WorkerPid}).    

do(Query, Params) ->
    {ok, {WorkerCnt, WorkerPids}} = luna_conf:get('db.active.workers'),
    Worker = maps:get(rand:uniform(WorkerCnt), WorkerPids),
    gen_server:call(Worker, {do_query, Query, Params}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call({add_worker, WorkerPid}, _From, State) ->
    {ok, {Cnt, ActiveWorkers}} = luna_conf:get('db.active.workers'),
    NewCnt = Cnt + 1,
    NewActiveWorkers = {NewCnt, ActiveWorkers#{NewCnt => WorkerPid}},
    ok = luna_conf:set('db.active.workers', NewActiveWorkers),
    {reply, ok, State};
handle_call({del_worker, WorkerPid}, _From, State) ->
    {ok, {_, ActiveWorkers0}} = luna_conf:get('db.active.workers'),
    {_, NewCnt, NewActiveWorkers} = 
	maps:fold( fun(_, WP, {WP, Cnt, AW}) ->
			   {WP, Cnt, AW};
		      (_, V, {WP, Cnt, AW}) ->
			   NC = Cnt + 1,
			   {WP, NC, AW#{NC => V}}
		   end
		 , {WorkerPid, 0, #{}}
		 , ActiveWorkers0),
    ok = luna_conf:set('db.active.workers', {NewCnt, NewActiveWorkers}),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================
