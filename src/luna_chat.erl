-module(luna_chat).

-behaviour(gen_server).

-export( [ add/3
	 , del/2
	 , block/2
	 , unblock/2
	 , mute/2
	 , unmute/2
	 , set/1 +++++++++++++++++++++++++++++++++++++++++++
	 , set_kivi/2
	 , set_auto_del/3
	 , set_delivered/3
	 , set_seen/3
	 , add_pin_message/3
	 , del_pin_message/2
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

-import( luna_db_chat
       , [ sp_chat_add/3
	 , sp_chat_get_by_cid/1
	 , sp_chat_del_starter/1
	 , sp_chat_del_follower/1
	 , sp_chat_set_starter_blocked_state/2
	 , sp_chat_set_follower_blocked_state/2
	 , sp_chat_set_starter_muted_state/2
	 , sp_chat_set_follower_muted_state/2
	 , sp_chat_set_starter_auto_delete/2
	 , sp_chat_set_follower_auto_delete/2
	 , sp_chat_set_starter_delivered_sequence/2
	 , sp_chat_set_follower_delivered_sequence/2
	 , sp_chat_set_starter_seen_sequence/2
	 , sp_chat_set_follower_seen_sequence/2 
	 , sp_chat_set_pinned_messages/5
	 , sp_chat_del_pinned_messages/4
	 , sp_chat_set_kivi/2
	 ]
       ).

-include("luna.hrl").

%%%===================================================================
%%% API
%%%===================================================================
-spec add(non_neg_integer(), non_neg_integer(), map()) ->
	  {ok, new, luna_chat_meta()} | 
	  {ok, already_exist, luna_chat_meta()} |
	  {ok, starter_had_been_deleted, luna_chat_meta()} |
	  {ok, follower_had_been_deleted, luna_chat_meta()} |
	  {ok, both_sides_had_been_deleted, luna_chat_meta()} |
	  {error, same_starter_and_follower} |
	  {error, invalid_params} |
	  {error, server_internal_error}.
add(StarterID, FollowerID, KiVi) when is_integer(StarterID)
				      , is_integer(FollowerID) 
				      , is_map(KiVi) ->
    try 
	case sp_chat_add(StarterID, FollowerID, KiVi) of
	    {error, ?DBE_EXCEPTION} -> 
		{error, server_internal_error};
	    {error, ?DBE_CHAT_SAME_ID_AS_SIDES} -> 
		{error, same_starter_and_follower};
	    { ok
	    , ?DBI_CHAT_ALREADY_EXIST
	    , LunaChatMeta
	    } -> {ok, already_exist, LunaChatMeta};
	    { ok
	    , ?DBI_CHAT_ALREADY_EXIST_BOTH_SIDES_HAD_BEEN_DELETED
	    , LunaChatMeta
	    } -> {ok, both_sides_had_been_deleted, LunaChatMeta};
	    { ok
	    , ?DBI_CHAT_ALREADY_EXIST_STARTER_HAD_BEEN_DELETED
	    , LunaChatMeta
	    } -> {ok, starter_had_been_deleted, LunaChatMeta};
	    { ok
	    , ?DBI_CHAT_ALREADY_EXIST_FOLLOWER_HAD_BEEN_DELETED
	    , LunaChatMeta
	    } -> {ok, follower_had_been_deleted, LunaChatMeta};
	    { ok
	    , ?DBI_CHAT_NEW
	    , LunaChatMeta
	    } -> {ok, new, LunaChatMeta}
	end
    catch 
	_:_ -> {error, server_internal_error}
    end;
add(_, _, _) -> {error, invalid_params}.

%%%===================================================================
-spec del(non_neg_integer(), non_neg_integer()) ->
	  {ok, done} | 
	  {error, invalid_uid} | 
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
del(CID, UID) when is_integer(CID), is_integer(UID) ->
    try
	case luna_rgst:get(CID) of
	    {ok, Pid} -> 
		gen_server:call(Pid, {del, UID});
	    {error, undefined} -> 
		case gen_server:start(?MODULE, [CID], []) of
		    {ok, Pid} -> gen_server:call(Pid, {del, UID});
		    _ -> {error, invalid_cid}
		end
	end
    catch
	_:_ -> {error, server_internal_error}
    end.

%%%===================================================================
-spec block(non_neg_integer(), non_neg_integer()) ->
	  {ok, done} | 
	  {error, invalid_uid} | 
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
block(CID, UID) when is_integer(CID), is_integer(UID) ->
    try
	case luna_rgst:get(CID) of
	    {ok, Pid} -> 
		gen_server:call(Pid, {set_blocked_state, UID, true});
	    {error, undefined} -> 
		case gen_server:start(?MODULE, [CID], []) of
		    {ok, Pid} -> gen_server:call(Pid, {set_blocked_state, UID, true});
		     _ -> {error, invalid_cid}
		end		     
	end
    catch
	_:_ -> {error, server_internal_error}
    end.

%%%===================================================================
-spec unblock(non_neg_integer(), non_neg_integer()) ->
	  {ok, done} | 
	  {error, invalid_uid} | 
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
unblock(CID, UID) when is_integer(CID), is_integer(UID) ->
    try
	case luna_rgst:get(CID) of
	    {ok, Pid} -> 
		gen_server:call(Pid, {set_blocked_state, UID, false});
	    {error, undefined} -> 
		case gen_server:start(?MODULE, [CID], []) of
		    {ok, Pid} -> gen_server:call(Pid, {set_blocked_state, UID, false});
		     _ -> {error, invalid_cid}
		end
	end
    catch
	_:_ -> {error, server_internal_error}
    end.

%%%===================================================================
-spec mute(non_neg_integer(), non_neg_integer()) ->
	  {ok, done} | 
	  {error, invalid_uid} | 
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
mute(CID, UID) when is_integer(CID), is_integer(UID) ->
    try
	case luna_rgst:get(CID) of
	    {ok, Pid} -> 
		gen_server:call(Pid, {set_muted_state, UID, true});
	    {error, undefined} -> 
		case gen_server:start(?MODULE, [CID], []) of
		    {ok, Pid} -> gen_server:call(Pid, {set_muted_state, UID, true});
		    _ -> {error, invalid_cid}
		end
	end
    catch
	_:_ -> {error, server_internal_error}
    end.

%%%===================================================================
-spec unmute(non_neg_integer(), non_neg_integer()) ->
	  {ok, done} | 
	  {error, invalid_uid} | 
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
unmute(CID, UID) when is_integer(CID), is_integer(UID) ->
    try
	case luna_rgst:get(CID) of
	    {ok, Pid} -> 
		gen_server:call(Pid, {set_muted_state, UID, false});
	    {error, undefined} -> 
		case gen_server:start(?MODULE, [CID], []) of
		    {ok, Pid} ->
			gen_server:call(Pid, {set_muted_state, UID, false});
		    _ ->
			{error, invalid_cid}
		end
	end
    catch
	_:_ -> {error, server_internal_error}
    end.

%%%===================================================================
-spec set_kivi(non_neg_integer(), map()) ->
	  {ok, done} |  
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
set_kivi(CID, KiVi) when is_integer(CID)
			 , is_map(KiVi) ->
    try
	case luna_rgst:get(CID) of
	    {ok, Pid} -> 
		gen_server:call(Pid, {set_kivi, KiVi});
	    {error, undefined} -> 
		case gen_server:start(?MODULE, [CID], []) of
		    {ok, Pid} ->
			gen_server:call(Pid, {set_kivi, KiVi});
		    _ ->
			{error, invalid_cid}
		end
	end
    catch
	_:_ -> {error, server_internal_error}
    end.

%%%===================================================================
-spec set_auto_del(non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
	  {ok, done} | 
	  {error, invalid_uid} | 
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
set_auto_del(CID, UID, AutoDel) when is_integer(CID)
				     , is_integer(UID)
				     , is_integer(AutoDel) ->
    try
	case luna_rgst:get(CID) of
	    {ok, Pid} -> 
		gen_server:call(Pid, {set_auto_del, UID, AutoDel});
	    {error, undefined} -> 
		case gen_server:start(?MODULE, [CID], []) of
		    {ok, Pid} ->
			gen_server:call(Pid, {set_auto_del, UID, AutoDel});
		    _ ->
			{error, invalid_cid}
		end
	end
    catch
	_:_ -> {error, server_internal_error}
    end.

%%%===================================================================
-spec set_delivered(non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
	  {ok, done} | 
	  {error, invalid_cid} | 
	  {error, invalid_uid} | 
	  {error, invalid_seq} |
	  {error, invalid_params} |
	  {error, server_internal_error}. 
set_delivered(CID, UID, Sequence) when is_integer(CID)
				       , is_integer(UID)
				       , is_integer(Sequence) ->
    try
	case luna_rgst:get(CID) of
	    {ok, Pid} -> 
		gen_server:call(Pid, {set_delivered, UID, Sequence});
	    {error, undefined} -> 
		case gen_server:start(?MODULE, [CID], []) of
		    {ok, Pid} ->
			gen_server:call(Pid, {set_delivered, UID, Sequence});
		    _ ->
			{error, invalid_cid}
		end
	end
    catch
	_:_ -> {error, server_internal_error}
    end.

%%%===================================================================
-spec set_seen(non_neg_integer(), non_neg_integer(), non_neg_integer()) ->
	  {ok, done} | 
	  {error, invalid_uid} |
	  {error, invalid_cid} | 
	  {error, invalid_seq} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
set_seen(CID, UID, Sequence) when is_integer(CID)
				  , is_integer(UID)
				  , is_integer(Sequence) ->
    try
	case luna_rgst:get(CID) of
	    {ok, Pid} -> 
		gen_server:call(Pid, {set_seen, UID, Sequence});
	    {error, undefined} -> 
		case gen_server:start(?MODULE, [CID], []) of
		    {ok, Pid} ->
			gen_server:call(Pid, {set_seen, UID, Sequence});
		    _ ->
			{error, invalid_cid}
		end
	end
    catch
	_:_ -> {error, server_internal_error}
    end.

%%%===================================================================
-spec add_pin_message(non_neg_integer(), non_neg_integer(), map()) ->
	  {ok, done} | 
	  {error, invalid_uid} |
	  {error, invalid_cid} | 
	  {error, invalid_pin} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
add_pin_message(CID, UID, PinnedMessages) when is_integer(CID)
					      , is_integer(UID)
					      , is_map(PinnedMessages) ->
    try
	case luna_rgst:get(CID) of
	    {ok, Pid} -> 
		gen_server:call(Pid, {add_pin_message, UID, PinnedMessages});
	    {error, undefined} -> 
		case gen_server:start(?MODULE, [CID], []) of
		    {ok, Pid} ->
			gen_server:call(Pid, {add_pin_message, UID, PinnedMessages});
		    _ ->
			{error, invalid_cid}
		end
	end
    catch
	_:_ -> {error, server_internal_error}
    end.

%%%===================================================================
-spec del_pin_message(non_neg_integer(), non_neg_integer()) ->
	  {ok, done} | 
	  {error, invalid_uid} |
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
del_pin_message(CID, UID) when is_integer(CID)
			       , is_integer(UID) ->
    try
	case luna_rgst:get(CID) of
	    {ok, Pid} -> 
		gen_server:call(Pid, {del_pin_message, UID});
	    {error, undefined} -> 
		case gen_server:start(?MODULE, [CID], []) of
		    {ok, Pid} ->
			gen_server:call(Pid, {del_pin_message, UID});
		    _ ->
			{error, invalid_cid}
		end
	end
    catch
	_:_ -> {error, server_internal_error}
    end.
    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([CID]) ->
    case sp_chat_get_by_cid(CID) of
	{ok, LunaChatMeta} -> 
	    ok = luna_rgst:set(CID, self()),
	    {ok, #luna_chat_state{chat_meta = LunaChatMeta}};
	{error, ?DBE_INVALID_CID} ->
	    {stop, normal}
    end.

%%%===================================================================
handle_call( {del, UID}
	   , _From
	   , #luna_chat_state{chat_meta = LCM} = State
	   ) ->
    try
	DBR = case {LCM, UID} of
		  { #luna_chat_meta{ starter_id = UID 
				   , starter_is_deleted = true }
		  , UID 
		  } -> {starter, {error, ?DBE_INVALID_CID}};
		  { #luna_chat_meta{ cid = CID
				   , starter_id = UID }
		  , UID
		  } -> {starter, sp_chat_del_starter(CID)};
		  { #luna_chat_meta{ follower_id = UID 
				   , follower_is_deleted = true }
		  , UID } -> {follower, {error, ?DBE_INVALID_CID}};
		  { #luna_chat_meta{ cid = CID
				   , follower_id = UID }
		  , UID
		  } -> {follower, sp_chat_del_follower(CID)};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{reply, {error, server_internal_error}, State};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{reply, {error, invalid_cid}, State};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{reply, {error, invalid_uid}, State};
	    {_, {error, ?DBE_ALREADY_SET}} ->
		{reply, {ok, done}, State};
	    {starter, {ok, MDA, LMeS, SStS, SDeS, SSeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = LMeS
					 , starter_is_deleted = true
					 , starter_start_sequence = SStS
					 , starter_delivered_sequence = SDeS
					 , starter_seen_sequence = SSeS
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}};
	    {follower, {ok, MDA, LMeS, SStS, SDeS, SSeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = LMeS
					 , follower_is_deleted = true
					 , follower_start_sequence = SStS
					 , follower_delivered_sequence = SDeS
					 , follower_seen_sequence = SSeS
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}}
	end
    catch
	_:_ -> {reply, {error, server_internal_error}, State}
    end;
%%%===================================================================
handle_call( {set_blocked_state, UID, BlockedState}
	   , _From
	   , #luna_chat_state{chat_meta = LCM} = State
	   ) ->
    try
	DBR = case {LCM, UID} of
		  {#luna_chat_meta{ starter_id = UID 
				  , starter_is_deleted = true
				  }, UID } ->
		      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ starter_id = UID 
				  , starter_is_blocked = BlockedState
				  }, UID } ->
		      {starter, {error, ?DBE_ALREADY_SET}};
		  {#luna_chat_meta{ cid = CID
				  , starter_id = UID
				  }, UID} ->
		      {starter, sp_chat_set_starter_blocked_state(CID, BlockedState)};
		  {#luna_chat_meta{ follower_id = UID 
				  , follower_is_deleted = true
				  }, UID } ->
		      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ follower_id = UID 
				  , follower_is_blocked = BlockedState
				  }, UID } ->
		      {follower, {error, ?DBE_ALREADY_SET}};
		  {#luna_chat_meta{ cid = CID
				  , follower_id = UID
				  }, UID } ->
		      {follower, sp_chat_set_follower_blocked_state(CID, BlockedState)};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{reply, {error, server_internal_error}, State};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{reply, {error, invalid_cid}, State};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{reply, {error, invalid_uid}, State};
	    {_, {error, ?DBE_ALREADY_SET}} ->		
		{reply, {ok, done}, State};
	    {starter, {ok, MDA, LMeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = LMeS
					 , starter_is_blocked = BlockedState
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}};
	    {follower, {ok, MDA, LMeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = LMeS
					 , follower_is_blocked = BlockedState
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}}
	end
    catch
	_:_ -> {reply, {error, server_internal_error}, State}
    end;
%%%===================================================================
handle_call( {set_muted_state, UID, MutedState}
	   , _From
	   , #luna_chat_state{chat_meta = LCM} = State
	   ) ->
    try
	DBR = case {LCM, UID} of
		  {#luna_chat_meta{ starter_id = UID
                                  , starter_is_deleted = true
                                  }, UID } ->
                      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ starter_id = UID 
				  , starter_is_muted = MutedState
				  }, UID } ->
		      {starter, {error, ?DBE_ALREADY_SET}};
		  {#luna_chat_meta{ cid = CID
				  , starter_id = UID
				  }, UID} ->
		      {starter, sp_chat_set_starter_muted_state(CID, MutedState)};
		  {#luna_chat_meta{ follower_id = UID
                                  , follower_is_deleted = true
                                  }, UID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ follower_id = UID 
				  , follower_is_muted = MutedState
				  }, UID } ->
		      {follower, {error, ?DBE_ALREADY_SET}};
		  {#luna_chat_meta{ cid = CID
				  , follower_id = UID
				  }, UID } ->
		      {follower, sp_chat_set_follower_muted_state(CID, MutedState)};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{reply, {error, server_internal_error}, State};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{reply, {error, invalid_cid}, State};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{reply, {error, invalid_uid}, State};
	    {_, {error, ?DBE_ALREADY_SET}} ->		
		{reply, {ok, done}, State};
	    {starter, {ok, MDA, LMeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = LMeS
					 , starter_is_muted = MutedState
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}};
	    {follower, {ok, MDA, LMeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = LMeS
					 , follower_is_muted = MutedState
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}}
	end
    catch
	_:_ -> {reply, {error, server_internal_error}, State}
    end;
%%%===================================================================
handle_call( {set_kivi, KiVi}
	   , _From
	   , #luna_chat_state{chat_meta = LCM} = State
	   ) ->
    try
	case sp_chat_set_kivi(LCM#luna_chat_meta.cid, KiVi) of
	    {error, ?DBE_EXCEPTION} -> 
		{reply, {error, server_internal_error}, State};
	    {ok, MDA} ->
		NLCM = LCM#luna_chat_meta{mda = MDA},
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}}
	end
    catch
	_:_ -> {reply, {error, server_internal_error}, State}
    end;
%%%===================================================================
handle_call( {set_auto_del, UID, AutoDel}
	   , _From
	   , #luna_chat_state{chat_meta = LCM} = State
	   ) ->
    try
	DBR = case {LCM, UID} of
		  {#luna_chat_meta{ starter_id = UID
                                  , starter_is_deleted = true
                                  }, UID } ->
                      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ starter_id = UID 
				  , starter_auto_delete = AutoDel
				  }, UID } ->
		      {starter, {error, ?DBE_ALREADY_SET}};
		  {#luna_chat_meta{ cid = CID
				  , starter_id = UID
				  }, UID} ->
		      {starter, sp_chat_set_starter_auto_delete(CID, AutoDel)};
		  {#luna_chat_meta{ follower_id = UID
                                  , follower_is_deleted = true
                                  }, UID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ follower_id = UID 
				  , follower_auto_delete = AutoDel
				  }, UID } ->
		      {follower, {error, ?DBE_ALREADY_SET}};
		  {#luna_chat_meta{ cid = CID
				  , follower_id = UID
				  }, UID } ->
		      {follower, sp_chat_set_follower_auto_delete(CID, AutoDel)};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{reply, {error, server_internal_error}, State};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{reply, {error, invalid_cid}, State};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{reply, {error, invalid_uid}, State};
	    {_, {error, ?DBE_ALREADY_SET}} ->		
		{reply, {ok, done}, State};
	    {starter, {ok, MDA}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , starter_auto_delete = AutoDel
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}};
	    {follower, {ok, MDA}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , follower_auto_delete = AutoDel
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}}
	end
    catch
	_:_ -> {reply, {error, server_internal_error}, State}
    end;
%%%===================================================================
handle_call( {set_delivered, UID, NS}
	   , _From
	   , #luna_chat_state{chat_meta = LCM} = State
	   ) ->
    try
	DBR = case {LCM, UID} of
		  {#luna_chat_meta{ starter_id = UID
                                  , starter_is_deleted = true
                                  }, UID } ->
                      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ starter_id = UID
                                  , starter_start_sequence = SStS
				  }, UID } when NS < SStS ->
                      {starter, {error, ?DBE_INVALID_SEQ}};
		  {#luna_chat_meta{ starter_id = UID 
				  , starter_delivered_sequence = SDeS 
				  }, UID } when NS =< SDeS ->
		      {starter, {error, ?DBE_ALREADY_SET}};
		  {#luna_chat_meta{ cid = CID
				  , starter_id = UID
				  }, UID} ->
		      {starter, sp_chat_set_starter_delivered_sequence(CID, NS)};
		  {#luna_chat_meta{ follower_id = UID
				  , follower_is_deleted = true
                                  }, UID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ follower_id = UID
                                  , follower_start_sequence = FStS
				  }, UID } when NS < FStS ->
                      {follower, {error, ?DBE_INVALID_SEQ}};
		  {#luna_chat_meta{ follower_id = UID 
				  , follower_delivered_sequence = FDeS
				  }, UID } when NS =< FDeS ->
		      {follower, {error, ?DBE_ALREADY_SET}};
		  {#luna_chat_meta{ cid = CID
				  , follower_id = UID
				  }, UID } ->
		      {follower, sp_chat_set_follower_delivered_sequence(CID, NS)};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{reply, {error, server_internal_error}, State};
	    {_, {error, ?DBE_INVALID_SEQ}} ->
		{reply, {error, invalid_seq}, State};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{reply, {error, invalid_cid}, State};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{reply, {error, invalid_uid}, State};
	    {_, {error, ?DBE_ALREADY_SET}} ->		
		{reply, {ok, done}, State};
	    {starter, {ok, MDA}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , starter_delivered_sequence = NS
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}};
	    {follower, {ok, MDA}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , follower_delivered_sequence = NS
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}}
	end
    catch
	_:_ -> {reply, {error, server_internal_error}, State}
    end;
%%%===================================================================
handle_call( {set_seen, UID, NS}
	   , _From
	   , #luna_chat_state{chat_meta = LCM} = State
	   ) ->
    try
	DBR = case {LCM, UID} of
		  {#luna_chat_meta{ starter_id = UID
                                  , starter_is_deleted = true
                                  }, UID } ->
                      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ starter_id = UID
                                  , starter_start_sequence = SStS
				  }, UID } when NS < SStS ->
                      {starter, {error, ?DBE_INVALID_SEQ}};
		  {#luna_chat_meta{ starter_id = UID 
				  , starter_delivered_sequence = SDeS 
				  }, UID } when NS > SDeS ->
		      {starter, {error, ?DBE_INVALID_SEQ}};
		  {#luna_chat_meta{ starter_id = UID 
				  , starter_seen_sequence = SSeS 
				  }, UID } when NS =< SSeS ->
		      {starter, {error, ?DBE_ALREADY_SET}};
		  {#luna_chat_meta{ cid = CID
				  , starter_id = UID
				  }, UID} ->
		      {starter, sp_chat_set_starter_seen_sequence(CID, NS)};
		  {#luna_chat_meta{ follower_id = UID
				  , follower_is_deleted = true
                                  }, UID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ follower_id = UID
                                  , follower_start_sequence = FStS
				  }, UID } when NS < FStS ->
                      {follower, {error, ?DBE_INVALID_SEQ}};
		  {#luna_chat_meta{ follower_id = UID
                                  , follower_delivered_sequence = FDeS
                                  }, UID } when NS > FDeS ->
                      {follower, {error, ?DBE_INVALID_SEQ}};
		  {#luna_chat_meta{ follower_id = UID 
				  , follower_seen_sequence = FSeS
				  }, UID } when NS =< FSeS ->
		      {follower, {error, ?DBE_ALREADY_SET}};
		  {#luna_chat_meta{ cid = CID
				  , follower_id = UID
				  }, UID } ->
		      {follower, sp_chat_set_follower_seen_sequence(CID, NS)};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{reply, {error, server_internal_error}, State};
	    {_, {error, ?DBE_INVALID_SEQ}} ->
		{reply, {error, invalid_seq}, State};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{reply, {error, invalid_cid}, State};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{reply, {error, invalid_uid}, State};
	    {_, {error, ?DBE_ALREADY_SET}} ->		
		{reply, {ok, done}, State};
	    {starter, {ok, MDA}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , starter_seen_sequence = NS
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}};
	    {follower, {ok, MDA}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , follower_seen_sequence = NS
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}}
	end
    catch
	_:_ -> {reply, {error, server_internal_error}, State}
    end;
%%%===================================================================
handle_call( {add_pin_message, UID, #{items := PinnedMessageList0}}
	   , _From
	   , #luna_chat_state{chat_meta = LCM} = State
	   ) ->
    PinnedMessages = #{items => lists:usort(PinnedMessageList0)},
    ?LOG_ERROR("~p:~p", [LCM#luna_chat_meta.pinned_messages, PinnedMessages]),
    try
	DBR = case {LCM, UID} of
		  {#luna_chat_meta{ starter_id = UID
                                  , starter_is_deleted = true
                                  }, UID } ->
                      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ starter_id = UID
                                  , pinned_messages = OldPinnedMessages
                                  }, UID } when OldPinnedMessages =:= PinnedMessages ->
                      {starter, {error, ?DBE_ALREADY_SET}};
		  {#luna_chat_meta{ cid = CID
				  , last_message_sequence = LMeS
				  , last_event_sequence = LEvS
				  , starter_id = UID
				  , starter_start_sequence = SStS
				  , starter_delivered_sequence = SDeS
				  }, UID} ->
		      case is_valid_pins(LMeS, SStS, SDeS, PinnedMessages) of
			  false -> 
			      {starter, {error, ?DBE_INVALID_SEQ}};
			  true ->
			      {starter, sp_chat_set_pinned_messages(CID, LMeS, LEvS, 'STARTER', PinnedMessages)}
			  end;
		  {#luna_chat_meta{ follower_id = UID
                                  , follower_is_deleted = true
                                  }, UID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ follower_id = UID
                                  , pinned_messages = OldPinnedMessages
                                  }, UID } when OldPinnedMessages =:= PinnedMessages ->
                      {follower, {error, ?DBE_ALREADY_SET}};
		  {#luna_chat_meta{ cid = CID
				  , last_message_sequence = LMeS
				  , last_event_sequence = LEvS
				  , follower_id = UID
				  , follower_start_sequence = FStS
				  , follower_delivered_sequence = FDeS
				  }, UID} ->
		      case is_valid_pins(LMeS, FStS, FDeS, PinnedMessages) of
			  false -> 
			      {follower, {error, ?DBE_INVALID_SEQ}};
			  true ->
			      {follower, sp_chat_set_pinned_messages(CID, LMeS, LEvS, 'FOLLOWER', PinnedMessages)}
		      end;
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{reply, {error, server_internal_error}, State};
	    {_, {error, ?DBE_INVALID_SEQ}} ->
		{reply, {error, invalid_seq}, State};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{reply, {error, invalid_cid}, State};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{reply, {error, invalid_uid}, State};
	    {_, {error, ?DBE_ALREADY_SET}} ->   
                {reply, {ok, done}, State};		
	    {starter, {ok, MDA, NLMeS, NLEvS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = NLMeS
					 , last_event_sequence = NLEvS
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}};
	    {follower, {ok, MDA, NLMeS, NLEvS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = NLMeS
					 , last_event_sequence = NLEvS
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}}
	end
    catch
	_:_ -> {reply, {error, server_internal_error}, State}
    end;
%%%===================================================================
handle_call( {del_pin_message, UID}
	   , _From
	   , #luna_chat_state{chat_meta = LCM} = State
	   ) ->
    try
	DBR = case {LCM, UID} of
		  {#luna_chat_meta{ starter_id = UID
                                  , starter_is_deleted = true
                                  }, UID } ->
                      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , starter_id = UID
				  , last_message_sequence = LMeS
				  , last_event_sequence = LEvS
				  }, UID} ->
		      {starter, sp_chat_del_pinned_messages(CID, LMeS, LEvS, 'STARTER')};
		  {#luna_chat_meta{ follower_id = UID
                                  , follower_is_deleted = true
                                  }, UID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , follower_id = UID
				  , last_message_sequence = LMeS
				  , last_event_sequence = LEvS
				  }, UID} -> 
		      {follower, sp_chat_del_pinned_messages(CID, LMeS, LEvS, 'FOLLOWER')};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{reply, {error, server_internal_error}, State};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{reply, {error, invalid_cid}, State};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{reply, {error, invalid_uid}, State};
	    {starter, {ok, MDA, NLMeS, NLEvS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = NLMeS
					 , last_event_sequence = NLEvS
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}};
	    {follower, {ok, MDA, NLMeS, NLEvS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = NLMeS
					 , last_event_sequence = NLEvS
					 },
		{reply, {ok, done}, State#luna_chat_state{chat_meta = NLCM}}
	end
    catch
	_:_ -> {reply, {error, server_internal_error}, State}
    end;
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
is_valid_pins(LMeS, StS, DeS, PinnedMessages0) when is_map(PinnedMessages0) ->
    case maps:get(items, PinnedMessages0, []) of
	[] -> false;
	PinnedMessages1 ->
	    is_valid_pins(LMeS, StS, DeS, PinnedMessages1)
	end;
is_valid_pins(_LMeS, _StS, _DeS, []) -> true;
is_valid_pins(LMeS, StS, DeS, [E|R]) when E >= StS, E =< LMeS, E =< DeS ->
    is_valid_pins(LMeS, StS, DeS, R);
is_valid_pins(_LMeS, _StS, _DeS, _PinnedMessages) -> false.

