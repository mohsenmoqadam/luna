-module(luna_rgst).

-export([ init/0
	, set/2
	, del/1
	, get/1
	]).

-define(SCOPE, chat).

%%%===================================================================
%%% Public Function: init/0
%%%===================================================================
-spec init() -> ok | {error, term()}.
init() ->
    try 
	syn:add_node_to_scopes([?SCOPE])
    catch 
	_:Any -> 
	    {error, Any} 
    end.

%%%===================================================================
%%% Public Function: set/2
%%%===================================================================
-spec set(term(), pid()) -> ok | {error, term()}.
set(Name, Pid) -> 
    try 
	ok = syn:register(?SCOPE, Name, Pid)
    catch 
	_:Any -> 
	    {error, Any} 
    end.

%%%===================================================================
%%% Public Function: del/1
%%%===================================================================
-spec del(term()) -> ok | {error, term()}.
del(Name) ->
    try 
	syn:unregister(?SCOPE, Name)
    catch 
	_:Any -> 
	    {error, Any} 
    end.

%%%===================================================================
%%% Public Function: get/1
%%%===================================================================
-spec get(term()) -> {ok, pid()} | {error, undefined} | {error, term}.
get(Name) ->
    try 
	case syn:lookup(?SCOPE, Name) of
	    {Pid, _Meta} -> {ok, Pid};
	    undefined -> {error, undefined} 
	end
    catch _:Any -> 
	    {error, Any} 
    end.
