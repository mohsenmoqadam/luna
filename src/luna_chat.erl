-module(luna_chat).

-behaviour(gen_server).

-export( [ init/1
	 , handle_call/3
	 , handle_cast/2
	 , handle_info/2
	 , terminate/2
	 , code_change/3
	 , format_status/2
	 ]
       ).

-export( [ add/2
	 , add/3
	 , del/2
	 , get/2
	 , block/2
	 , unblock/2
	 , mute/2
	 , unmute/2
	 , set_kivi/2
	 , set_auto_del/3
	 , set_delivered/3
	 , set_seen/3
	 , add_message/4
	 , add_message/5
	 , add_message/6
	 , set_message/5
	 , set_message/6
	 , set_message/7
	 , del_message/4
	 , set_message_action/4
	 , add_pin_message/3
	 , del_pin_message/2
	 , get_messages/4
	 , get_medias/5
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
	 , sp_chat_message_add/10
	 , sp_chat_message_set/11
	 , sp_chat_message_del/7
	 , sp_chat_message_set_action/7
	 , sp_chat_message_get/3
	 , sp_chat_storage_get/5
	 ]
       ).

-include("luna.hrl").

%%%=== API Function: add/2,3 =========================================
%%% It creates a new chat and returns the chat's meta.  
add(StarterID, FollowerID) ->
    add(StarterID, FollowerID, null).
-spec add(starter_id(), follower_id(), map()) ->
	  {ok, new, map()} | 
	  {ok, already_exist, map()} |
	  {ok, starter_had_been_deleted, map()} |
	  {ok, follower_had_been_deleted, map()} |
	  {ok, both_sides_had_been_deleted, map()} |
	  {error, same_starter_and_follower} |
	  {error, invalid_params} |
	  {error, server_internal_error}.
add(StarterID, FollowerID, KiVi) 
  when is_integer(StarterID) andalso 
       is_integer(FollowerID) andalso 
       (is_map(KiVi) orelse KiVi =:= null) ->
    try 
	case sp_chat_add(StarterID, FollowerID, KiVi) of
	    {error, ?DBE_EXCEPTION} -> 
		{error, server_internal_error};
	    {error, ?DBE_CHAT_SAME_ID_AS_SIDES} -> 
		{error, same_starter_and_follower};
	    { ok
	    , ?DBI_CHAT_ALREADY_EXIST
	    , LunaChatMeta
	    } -> {ok, already_exist, m(LunaChatMeta, any)};
	    { ok
	    , ?DBI_CHAT_ALREADY_EXIST_BOTH_SIDES_HAD_BEEN_DELETED
	    , LunaChatMeta
	    } -> { ok
		 , both_sides_had_been_deleted
		 , m(LunaChatMeta, any)
		 };
	    { ok
	    , ?DBI_CHAT_ALREADY_EXIST_STARTER_HAD_BEEN_DELETED
	    , LunaChatMeta
	    } -> { ok
		 , starter_had_been_deleted
		 , m(LunaChatMeta, any)
		 };
	    { ok
	    , ?DBI_CHAT_ALREADY_EXIST_FOLLOWER_HAD_BEEN_DELETED
	    , LunaChatMeta
	    } -> { ok
		 , follower_had_been_deleted
		 , m(LunaChatMeta, any)
		 };
	    { ok
	    , ?DBI_CHAT_NEW
	    , LunaChatMeta
	    } -> {ok, new, m(LunaChatMeta, any)}
	end
    catch 
	_:_ -> {error, server_internal_error}
    end;
add(_, _, _) -> {error, invalid_params}.

%%%=== API Function: del/2 ===========================================
%%% It deletes a chat for a user whose identification is: UID.
-spec del(cid(), uid()) ->
	  {ok, luna_date()} | 
	  {error, invalid_uid} | 
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
del(CID, UID) 
  when is_integer(CID) andalso is_integer(UID) ->
    call(CID, {del, UID});
del(_, _) -> {error, invalid_params}.

%%%=== API Function: get/1 ===========================================
%%% It returns chat meta.
-spec get(cid(), uid()) ->
	  {ok, map()} |
	  {error, invalid_cid} |
	  {error, invalid_uid} |
	  {error, invalid_params} |
	  {error, server_internal_error}. 
get(CID, UID) 
  when is_integer(CID) andalso is_integer(UID) ->
    call(CID, {get, UID});
get(_, _) -> {error, invalid_params}.

%%%=== API Function: block/2 =========================================
%%% It blocks a user whose identification is: UID.
-spec block(cid(), uid()) ->
	  {ok, done} | 
	  {ok, already_set} | 
	  {error, invalid_uid} | 
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
block(CID, UID) 
  when is_integer(CID) andalso is_integer(UID) ->
    call(CID, {set_blocked_state, UID, true});
block(_, _) -> {error, invalid_params}.

%%%=== API Function: unblock/2 =======================================
%%% It unblocks a user whose identification is: UID.
-spec unblock(cid(), uid()) ->
	  {ok, done} | 
	  {ok, already_set} |
	  {error, invalid_uid} | 
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
unblock(CID, UID) when is_integer(CID), is_integer(UID) ->
    call(CID, {set_blocked_state, UID, false});
unblock(_, _) -> {error, invalid_params}.

%%%=== API Function: mute/2 ==========================================
%%% It mutes a user whose identification is: UID.
-spec mute(non_neg_integer(), non_neg_integer()) ->
	  {ok, done} | 
	  {error, invalid_uid} | 
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
mute(CID, UID) 
  when is_integer(CID) andalso is_integer(UID) ->
    call(CID, {set_muted_state, UID, true});
mute(_, _) -> {error, invalid_params}.

%%%=== API Function: unmute/2 ========================================
%%% It unmutes a user whose identification is: UID.
-spec unmute(non_neg_integer(), non_neg_integer()) ->
	  {ok, done} | 
	  {error, invalid_uid} | 
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
unmute(CID, UID) 
  when is_integer(CID) andalso is_integer(UID) ->
    call(CID, {set_muted_state, UID, false});
unmute(_, _) -> {error, invalid_params}.

%%%=== API Function: set_kivi/2 ======================================
%%% It sets the chat's KiVi (Key-value storage).
-spec set_kivi(non_neg_integer(), map()) ->
	  {ok, done} |  
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
set_kivi(CID, KiVi) 
  when is_integer(CID) andalso is_map(KiVi) ->
    call(CID, {set_kivi, KiVi});
set_kivi(_, _) -> {error, invalid_params}.

%%%=== API Function: set_auto_del/3 ==================================
%%% It sets an auto-delete for a user whose identification is: UID.
-spec set_auto_del( non_neg_integer()
		  , non_neg_integer()
		  , non_neg_integer()
		  ) ->
	  {ok, done} | 
	  {error, invalid_uid} | 
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
set_auto_del(CID, UID, AutoDel) 
  when is_integer(CID) andalso 
       is_integer(UID) andalso 
       is_integer(AutoDel) ->
    call(CID, {set_auto_del, UID, AutoDel});
set_auto_del(_, _, _) -> {error, invalid_params}.

%%%=== API Function: set_delivered/3 =================================
%%% It sets the last delivered message sequence.
-spec set_delivered( non_neg_integer()
		   , non_neg_integer()
		   , non_neg_integer()
		   ) ->
	  {ok, done} | 
	  {error, invalid_cid} | 
	  {error, invalid_uid} | 
	  {error, invalid_seq} |
	  {error, invalid_params} |
	  {error, server_internal_error}. 
set_delivered(CID, UID, Sequence) 
  when is_integer(CID) andalso 
       is_integer(UID) andalso 
       is_integer(Sequence) ->
    call(CID, {set_delivered, UID, Sequence});
set_delivered(_, _, _) -> {error, invalid_params}.

%%%=== API Function: set_seen/3 ======================================
%%% It sets the last seen message sequence.
-spec set_seen( non_neg_integer()
	      , non_neg_integer()
	      , non_neg_integer()
	      ) ->
	  {ok, done} | 
	  {error, invalid_uid} |
	  {error, invalid_cid} | 
	  {error, invalid_seq} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
set_seen(CID, UID, Sequence) 
  when is_integer(CID) andalso 
       is_integer(UID) andalso 
       is_integer(Sequence) ->
    call(CID, {set_seen, UID, Sequence});
set_seen(_, _, _) -> {error, invalid_params}.

%%%=== API Function: add_message/4,5,6 ===============================
%%% It adds a new message to the chat.
add_message(CID, WID, ReplySequence, Body) ->
    add_message(CID, WID, ReplySequence, Body, null, null).
add_message(CID, WID, ReplySequence, Body, Objects) ->
    add_message(CID, WID, ReplySequence, Body, Objects, null).
-spec add_message( non_neg_integer(), non_neg_integer()
		 , non_neg_integer(), binary(), map(), map() ) ->
	  {ok, done} | 
	  {error, invalid_uid} |
	  {error, invalid_cid} | 
	  {error, invalid_seq} |
	  {error, invalid_body} |
	  {error, invalid_objects} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
add_message(_, _, _, <<"">>, _, _) -> {error, invalid_body};
add_message(CID, WID, ReplySequence, Body, Objects, KiVi) 
  when Objects =:= #{} orelse 
       Objects =:= null ->
    add_message_(CID, WID, ReplySequence, Body, null, KiVi);
add_message( CID, WID, ReplySequence, Body
	   , #{<<"items">> := ObjectList}, KiVi ) ->
    add_message( CID, WID, ReplySequence, Body
	       , #{items => ObjectList}, KiVi );
add_message( CID, WID, ReplySequence, Body
	   , #{items := ObjectList} = Objects, KiVi ) ->
    case is_valid_opjects(ObjectList) of
	true -> add_message_( CID, WID, ReplySequence, Body
			    , Objects, KiVi );
	false -> {error, invalid_objects}
    end;
add_message(_, _, _, _, _, _) ->  {error, invalid_params}.
add_message_(CID, WID, ReplySequence, Body, Objects, KiVi) 
  when ( is_integer(CID) andalso 
	 is_integer(WID) andalso
	 is_binary(Body) 
       ) andalso
       ( is_map(KiVi) orelse
	 KiVi =:= null
       ) andalso
       ( is_integer(ReplySequence) orelse 
	 ReplySequence =:= null
       ) ->
    call( CID
	, {add_message, WID, ReplySequence, Body, Objects, KiVi }
	);
add_message_(_, _, _, _, _, _) ->  {error, invalid_params}.

%%%=== API Function: set_message/4,5,6 ===============================
%%% It changes one message of the chat.
set_message(CID, WID, Sequence, Version, Body) ->
    set_message(CID, WID, Sequence, Version, Body, null, null).
set_message(CID, WID, Sequence, Version, Body, Objects) ->
    set_message(CID, WID, Sequence, Version, Body, Objects, null).
-spec set_message( non_neg_integer(), non_neg_integer()
		 , non_neg_integer(), non_neg_integer()
		 , binary(), map(), map() ) ->
	  {ok, done} | 
	  {error, invalid_uid} |
	  {error, invalid_cid} | 
	  {error, invalid_seq} |
	  {error, invalid_role} |
	  {error, invalid_version} |
	  {error, invalid_body} |
	  {error, invalid_objects} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
set_message(_, _, _, _, <<"">>, _, _) -> {error, invalid_body};
set_message(CID, WID, Sequence, Version, Body, Objects, KiVi) 
  when Objects =:= #{} orelse 
       Objects =:= null ->
    set_message_(CID, WID, Sequence, Version, Body, null, KiVi);
set_message( CID, WID, Sequence, Body
	   , Version, #{<<"items">> := ObjectList}, KiVi ) ->
    set_message( CID, WID, Sequence, Body
	       , Version, #{items => ObjectList}, KiVi );
set_message( CID, WID, Sequence, Body
	   , Version, #{items := ObjectList} = Objects, KiVi ) ->
    case is_valid_opjects(ObjectList) of
	true -> set_message_( CID, WID, Sequence, Version
			    , Body, Objects, KiVi );
	false -> {error, invalid_objects}
    end;
set_message(_, _, _, _, _, _, _) ->  {error, invalid_params}.
set_message_(CID, WID, Sequence, Version, Body, Objects, KiVi) 
  when ( is_integer(CID) andalso 
	 is_integer(WID) andalso
	 is_integer(Sequence) andalso
	 is_integer(Version) andalso
	 is_binary(Body) 
       ) andalso
       ( is_map(KiVi) orelse
	 KiVi =:= null
       ) ->
    call( CID
        , {set_message, WID, Sequence, Version, Body, Objects, KiVi}
        );
set_message_(_, _, _, _, _, _, _) ->  {error, invalid_params}.

%%%=== API Function: del_message/4 ===================================
%%% It deletes one message from the chat.
-spec del_message( non_neg_integer(), non_neg_integer()
		 , non_neg_integer(), one | everyone ) ->
	  {ok, done} | 
	  {error, invalid_uid} |
	  {error, invalid_cid} | 
	  {error, invalid_seq} |
	  {error, invalid_role} |  
	  {error, invalid_params} |
	  {error, server_internal_error}. 
del_message(CID, WID, Sequence, DelType) 
  when ( is_integer(CID) andalso 
	 is_integer(WID) andalso
	 is_integer(Sequence)
       ) andalso
       ( DelType =:= one orelse
	 DelType =:= everyone
       ) ->
    call(CID, {del_message, WID, Sequence, DelType});
del_message(_, _, _, _) ->  {error, invalid_params}.

%%%=== API Function: set_message_action/4 ============================
%%% It changes the action of one message.
-spec set_message_action( non_neg_integer(), non_neg_integer()
			, non_neg_integer(), one | everyone ) ->
	  {ok, done} | 
	  {error, invalid_uid} |
	  {error, invalid_cid} | 
	  {error, invalid_seq} |
	  {error, invalid_role} |  
	  {error, invalid_params} |
	  {error, server_internal_error}. 
set_message_action(CID, WID, Sequence, Action) 
  when ( is_integer(CID) andalso
	 is_integer(WID) andalso
	 is_integer(Sequence)
       ) andalso
       ( is_map(Action) orelse
	 Action =:= null
       ) ->
    call(CID, {set_message_action, WID, Sequence, Action});
set_message_action(_, _, _, _) ->  {error, invalid_params}.

%%%=== API Function: add_pin_message/3 ===============================
%%% It adds a list of message sequences to the chat pin list.
-spec add_pin_message(non_neg_integer(), non_neg_integer(), map()) ->
	  {ok, done} | 
	  {error, invalid_uid} |
	  {error, invalid_cid} | 
	  {error, invalid_pin} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
add_pin_message(_, _, #{items := []}) -> {error, invalid_pin};
add_pin_message(CID, UID, #{<<"items">> := PinnedMessageList}) ->
    add_pin_message(CID, UID, #{items => PinnedMessageList}); 
add_pin_message( CID, UID
	       , #{items := PinnedMessageList} = PinnedMessages ) 
  when is_integer(CID) andalso
       is_integer(UID) andalso
       is_list(PinnedMessageList) ->
    call(CID, {add_pin_message, UID, PinnedMessages});
add_pin_message(_, _, _) -> {error, invalid_params}.

%%%=== API Function: del_pin_message/2 ===============================
%%% It deletes the chat pin list
-spec del_pin_message(non_neg_integer(), non_neg_integer()) ->
	  {ok, done} | 
	  {error, invalid_uid} |
	  {error, invalid_cid} | 
	  {error, invalid_params} |
	  {error, server_internal_error}. 
del_pin_message(CID, UID) 
  when is_integer(CID) andalso
       is_integer(UID) ->
    call(CID, {del_pin_message, UID});
del_pin_message(_, _) -> {error, invalid_params}.

%%%=== API Function: get_messages/4 ==================================
%%% It returns messages of the chat.
-spec get_messages( non_neg_integer(), non_neg_integer()
		  , non_neg_integer(), one | everyone ) ->
	  {ok, done} | 
	  {error, invalid_uid} |
	  {error, invalid_cid} | 
	  {error, invalid_seq} |
	  {error, invalid_role} |  
	  {error, invalid_params} |
	  {error, server_internal_error}. 
get_messages(CID, UID, From, Size) 
  when  is_integer(CID) andalso 
	is_integer(UID) andalso
	is_integer(From) andalso 
	is_integer(Size) ->
    call(CID, {get_messages, UID, From, Size});
get_messages(_, _, _, _) ->  {error, invalid_params}.

%%%=== API Function: get_medias/4 ====================================
%%% It returns medias of the chat.
-spec get_medias( non_neg_integer(), non_neg_integer()
		, non_neg_integer(), non_neg_integer()
		, 'FILE' | 'LINK' ) ->
	  {ok, done} | 
	  {error, invalid_uid} |
	  {error, invalid_cid} | 
	  {error, invalid_seq} |
	  {error, invalid_role} |  
	  {error, invalid_params} |
	  {error, server_internal_error}. 
get_medias(CID, UID, Type, From, Size) 
  when  is_integer(CID) andalso 
	is_integer(UID) andalso 
	is_integer(From) andalso
	is_integer(Size) andalso
	(Type =:= 'FILE' orelse Type =:= 'LINK') ->
    call(CID, {get_medias, UID, Type, From, Size});
get_medias(_, _, _, _, _) ->  {error, invalid_params}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init([CID]) ->
    case sp_chat_get_by_cid(CID) of
	{ok, LunaChatMeta} -> 
	    {ok, Timeout} = luna_conf:get('chat.timeout'),
	    {ok, Hibernate} = luna_conf:get('chat.hibernate'),
	    ok = luna_rgst:set(CID, self()),
	    {ok, #luna_chat_state{ chat_meta = LunaChatMeta
				 , timeout = Timeout*60*1000
				 , hibernate = Hibernate*60*1000
				 }, Timeout*60*60};
	{error, ?DBE_INVALID_CID} ->
	    {stop, normal}
    end.

%%%===================================================================
handle_call( {del, UID}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
	   ) ->
    try
	DBR = case {LCM, UID} of
		  { #luna_chat_meta{ starter_id = UID 
				   , starter_is_deleted = true 
				   }
		  , UID 
		  } -> {starter, {error, ?DBE_INVALID_CID}};
		  { #luna_chat_meta{ cid = CID
				   , starter_id = UID 
				   }
		  , UID
		  } -> {starter, sp_chat_del_starter(CID)};
		  { #luna_chat_meta{ follower_id = UID 
				   , follower_is_deleted = true 
				   }
		  , UID } -> {follower, {error, ?DBE_INVALID_CID}};
		  { #luna_chat_meta{ cid = CID
				   , follower_id = UID 
				   }
		  , UID
		  } -> {follower, sp_chat_del_follower(CID)};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_ALREADY_SET}} ->
		{ reply
		, {ok, done}
		, cht(State)
		, Timeout
		};
	    {starter, {ok, MDA, LMeS, SStS, SDeS, SSeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = LMeS
					 , starter_is_deleted = true
					 , starter_start_sequence = SStS
					 , starter_delivered_sequence = SDeS
					 , starter_seen_sequence = SSeS
					 },
		{ reply
		, {ok, m(NLCM, 'STARTER')}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		};
	    {follower, {ok, MDA, LMeS, SStS, SDeS, SSeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = LMeS
					 , follower_is_deleted = true
					 , follower_start_sequence = SStS
					 , follower_delivered_sequence = SDeS
					 , follower_seen_sequence = SSeS
					 },
		{ reply
		, {ok, m(NLCM, 'FOLLOWER')}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;

%%%===================================================================
handle_call({get, UID}, _From, #luna_chat_state{ chat_meta = LCM
					       , timeout = Timeout
					       } = State) ->
    try
	DBR = case {LCM, UID} of
		  {#luna_chat_meta{ starter_id = UID 
				  , starter_is_deleted = true
				  }, UID } ->
		      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{starter_id = UID}, UID} ->
		      {starter, LCM};
		  {#luna_chat_meta{ follower_id = UID 
				  , follower_is_deleted = true
				  }, UID } ->
		      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{follower_id = UID}, UID } ->
		      {follower, LCM};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_INVALID_CID}} ->
		{reply, {error, invalid_cid}, cht(State), Timeout};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{reply, {error, invalid_uid}, cht(State), Timeout};
	    {_, LCM} ->
		{reply, {ok, m(LCM, any)}, cht(State), Timeout}
	end
    catch
	_:_ -> 
	    { reply
	    , {error, server_internal_error}
	    , cht(State)
	    , Timeout
	    }
    end;
%%%===================================================================
handle_call( {set_blocked_state, UID, BlockedState}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout 
			     } = State
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
		      { starter
		      , sp_chat_set_starter_blocked_state( CID
							 , BlockedState
							 )
		      };
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
		      { follower
		      , sp_chat_set_follower_blocked_state( CID
							  , BlockedState
							  )
		      };
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_ALREADY_SET}} ->		
		{ reply
		, {ok, already_set}
		, cht(State)
		, Timeout
		};
	    {starter, {ok, MDA, LMeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = LMeS
					 , starter_is_blocked = BlockedState
					 },
		{ reply
		, {ok, m(NLCM, 'STARTER')}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		};
	    {follower, {ok, MDA, LMeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = LMeS
					 , follower_is_blocked = BlockedState
					 },
		{ reply
		, {ok, m(NLCM, 'FOLLOWER')}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;
%%%===================================================================
handle_call( {set_muted_state, UID, MutedState}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
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
		      { starter
		      , sp_chat_set_starter_muted_state( CID
						       , MutedState
						       )
		      };
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
		      { follower
		      , sp_chat_set_follower_muted_state( CID
							, MutedState
							)
		      };
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_ALREADY_SET}} ->		
		{ reply
		, {ok, done}
		, cht(State)
		, Timeout
		};
	    {starter, {ok, MDA, LMeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = LMeS
					 , starter_is_muted = MutedState
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		};
	    {follower, {ok, MDA, LMeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = LMeS
					 , follower_is_muted = MutedState
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;
%%%===================================================================
handle_call( {set_kivi, KiVi}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
	   ) ->
    try
	case sp_chat_set_kivi(LCM#luna_chat_meta.cid, KiVi) of
	    {error, ?DBE_EXCEPTION} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {ok, MDA} ->
		NLCM = LCM#luna_chat_meta{mda = MDA},
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;
%%%===================================================================
handle_call( {set_auto_del, UID, AutoDel}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
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
		      { starter
		      , sp_chat_set_starter_auto_delete( CID
						       , AutoDel
						       )
		      };
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
		      { follower
		      , sp_chat_set_follower_auto_delete( CID
							, AutoDel
							)
		      };
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_ALREADY_SET}} ->		
		{ reply
		, {ok, done}
		, cht(State)
		, Timeout
		};
	    {starter, {ok, MDA}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , starter_auto_delete = AutoDel
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		};
	    {follower, {ok, MDA}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , follower_auto_delete = AutoDel
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;
%%%===================================================================
handle_call( {set_delivered, UID, NS}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
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
		      { starter
		      , sp_chat_set_starter_delivered_sequence( CID
							      , NS
							      )
		      };
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
		      { follower
		      , sp_chat_set_follower_delivered_sequence( CID
							       , NS
							       )
		      };
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_SEQ}} ->
		{ reply
		, {error, invalid_seq}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_ALREADY_SET}} ->		
		{ reply
		, {ok, done}
		, State
		, Timeout
		};
	    {starter, {ok, MDA}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , starter_delivered_sequence = NS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		};
	    {follower, {ok, MDA}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , follower_delivered_sequence = NS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;
%%%===================================================================
handle_call( {set_seen, UID, NS}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
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
		      { starter
		      , sp_chat_set_starter_seen_sequence(CID, NS)
		      };
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
		      { follower
		      , sp_chat_set_follower_seen_sequence(CID, NS)
		      };
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_SEQ}} ->
		{ reply
		, {error, invalid_seq}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_ALREADY_SET}} ->		
		{ reply
		, {ok, done}
		, cht(State)
		, Timeout
		};
	    {starter, {ok, MDA}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , starter_seen_sequence = NS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		};
	    {follower, {ok, MDA}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , follower_seen_sequence = NS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;
%%%===================================================================
handle_call( {add_message, WID, ReplySequence, Body, Objects, KiVi}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
	   ) ->
    try
	DBR = case {LCM, WID} of
		  {#luna_chat_meta{ starter_id = WID
                                  , starter_is_deleted = true
                                  }, WID } ->
                      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , last_message_sequence = LMeS
				  , starter_id = WID
				  , starter_auto_delete = SAuD
				  , starter_start_sequence = SStS
				  , starter_delivered_sequence = SDeS
				  , follower_id = FID
				  }, WID} 
		    when ReplySequence =:= null orelse 
			 (ReplySequence >= SStS andalso 
			  ReplySequence =< SDeS) ->
		      { starter
		      , sp_chat_message_add( CID, WID, FID, 'STARTER' 
					   , LMeS, ReplySequence, Body
					   , Objects, SAuD, KiVi 
					   )
		      };
		  {#luna_chat_meta{starter_id = WID}, WID}  ->
		      {starter, {error, ?DBE_INVALID_SEQ}};
		  {#luna_chat_meta{ follower_id = WID
                                  , follower_is_deleted = true
                                  }, WID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , last_message_sequence = LMeS
				  , follower_id = WID
				  , follower_auto_delete = FAuD
				  , follower_start_sequence = FStS
				  , follower_delivered_sequence = FDeS
				  , starter_id = SID
				  }, WID} 
		    when ReplySequence =:= null orelse 
			 (ReplySequence >= FStS andalso 
			  ReplySequence =< FDeS) ->
		      { follower
		      , sp_chat_message_add( CID, SID, WID, 'FOLLOWER' 
					   , LMeS, ReplySequence, Body
					   , Objects, FAuD, KiVi 
					   )
		      };
		  {#luna_chat_meta{follower_id = WID}, WID}  ->
		      {follower, {error, ?DBE_INVALID_SEQ}};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_SEQ}} ->
		{ reply
		, {error, invalid_seq}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {starter, {ok, MDA, NLMeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = NLMeS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		};
	    {follower, {ok, MDA, NLMeS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = NLMeS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;
%%%===================================================================
handle_call( { set_message, WID, Sequence, Version
	     , Body, Objects, KiVi }
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
	   ) ->
    try
	DBR = case {LCM, WID} of
		  {#luna_chat_meta{ starter_id = WID
                                  , starter_is_deleted = true
                                  }, WID } ->
                      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , last_event_sequence = LEvS
				  , starter_id = WID
				  , starter_auto_delete = SAuD
				  , starter_start_sequence = SStS
				  , starter_delivered_sequence = SDeS
				  , follower_id = FID
				  }, WID} when Sequence >= SStS 
					       , Sequence =< SDeS ->
		      { starter
		      , sp_chat_message_set( CID, WID, FID, WID
					   , Sequence, LEvS, Version
					   , Body, Objects
					   , SAuD, KiVi 
					   )
		      };
		  {#luna_chat_meta{starter_id = WID}, WID}  ->
		      {starter, {error, ?DBE_INVALID_SEQ}};
		  {#luna_chat_meta{ follower_id = WID
                                  , follower_is_deleted = true
                                  }, WID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , last_event_sequence = LEvS
				  , follower_id = WID
				  , follower_auto_delete = FAuD
				  , follower_start_sequence = FStS
				  , follower_delivered_sequence = FDeS
				  , starter_id = SID
				  }, WID} when Sequence >= FStS
					       , Sequence =< FDeS ->
		      { follower
		      , sp_chat_message_set( CID, SID, WID, WID
					   , Sequence, LEvS, Version
					   , Body, Objects, FAuD, KiVi 
					   )
		      };
		  {#luna_chat_meta{follower_id = WID}, WID}  ->
		      {follower, {error, ?DBE_INVALID_SEQ}};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_SEQ}} ->
		{ reply
		, {error, invalid_seq}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_ROL}} ->
		{ reply
		, {error, invalid_role}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {starter, {ok, MDA, NLEvS, _LMeV}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_event_sequence = NLEvS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		};
	    {follower, {ok, MDA, NLEvS, _LMeV}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_event_sequence = NLEvS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;
%%%===================================================================
handle_call( {del_message, WID, Sequence, DelType}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
	   ) ->
    try
	DBR = case {LCM, WID} of
		  {#luna_chat_meta{ starter_id = WID
                                  , starter_is_deleted = true
                                  }, WID } ->
                      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , last_event_sequence = LEvS
				  , starter_id = WID
				  , starter_start_sequence = SStS
				  , starter_delivered_sequence = SDeS
				  , follower_id = FID
				  }, WID} 
		    when Sequence >= SStS 
			 , Sequence =< SDeS ->
		      { starter
		      , sp_chat_message_del(CID, WID, FID, WID
					   , Sequence, LEvS, DelType
					   )
		      };
		  {#luna_chat_meta{starter_id = WID}, WID}  ->
		      {starter, {error, ?DBE_INVALID_SEQ}};
		  {#luna_chat_meta{ follower_id = WID
                                  , follower_is_deleted = true
                                  }, WID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , last_event_sequence = LEvS
				  , follower_id = WID
				  , follower_start_sequence = FStS
				  , follower_delivered_sequence = FDeS
				  , starter_id = SID
				  }, WID} 
		    when Sequence >= FStS
			 , Sequence =< FDeS ->
		      { follower
		      , sp_chat_message_del( CID, SID, WID, WID
					   , Sequence, LEvS, DelType
					   )
		      };
		  {#luna_chat_meta{follower_id = WID}, WID}  ->
		      {follower, {error, ?DBE_INVALID_SEQ}};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_SEQ}} ->
		{ reply
		, {error, invalid_seq}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_ROL}} ->
		{ reply
		, {error, invalid_role}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {starter, {ok, MDA, NLEvS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_event_sequence = NLEvS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		};
	    {follower, {ok, MDA, NLEvS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_event_sequence = NLEvS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;

%%%===================================================================
handle_call( {set_message_action, WID, Sequence, Action}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
	   ) ->
    try
	DBR = case {LCM, WID} of
		  {#luna_chat_meta{ starter_id = WID
                                  , starter_is_deleted = true
                                  }, WID } ->
                      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , last_event_sequence = LEvS
				  , starter_id = WID
				  , starter_start_sequence = SStS
				  , starter_delivered_sequence = SDeS
				  , follower_id = FID
				  }, WID} when Sequence >= SStS 
					       , Sequence =< SDeS ->
		      { starter
		      , sp_chat_message_set_action( CID, WID, FID
						  , WID, Sequence
						  , LEvS, Action
						  )
		      };
		  {#luna_chat_meta{starter_id = WID}, WID}  ->
		      {starter, {error, ?DBE_INVALID_SEQ}};
		  {#luna_chat_meta{ follower_id = WID
                                  , follower_is_deleted = true
                                  }, WID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , last_event_sequence = LEvS
				  , follower_id = WID
				  , follower_start_sequence = FStS
				  , follower_delivered_sequence = FDeS
				  , starter_id = SID
				  }, WID} when Sequence >= FStS
					       , Sequence =< FDeS ->
		      { follower
		      , sp_chat_message_set_action( CID, SID, WID
						  , WID, Sequence
						  , LEvS, Action
						  )
		      };
		  {#luna_chat_meta{follower_id = WID}, WID}  ->
		      {follower, {error, ?DBE_INVALID_SEQ}};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_SEQ}} ->
		{ reply
		, {error, invalid_seq}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_ROL}} ->
		{ reply
		, {error, invalid_role}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {starter, {ok, MDA, NLEvS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_event_sequence = NLEvS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		};
	    {follower, {ok, MDA, NLEvS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_event_sequence = NLEvS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;
%%%===================================================================
handle_call( {add_pin_message, UID, #{items := PinnedMessageList0}}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
	   ) ->
    PinnedMessages = #{items => lists:usort(PinnedMessageList0)},
    try
	DBR = case {LCM, UID} of
		  {#luna_chat_meta{ starter_id = UID
                                  , starter_is_deleted = true
                                  }, UID } ->
                      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ starter_id = UID
                                  , pinned_messages = OldPinnedMessages
                                  }, UID }
		    when OldPinnedMessages =:= PinnedMessages ->
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
			      { starter
			      , sp_chat_set_pinned_messages( CID
							   , LMeS
							   , LEvS
							   , 'STARTER'
							   , PinnedMessages
							   )
			      }
		      end;
		  {#luna_chat_meta{ follower_id = UID
                                  , follower_is_deleted = true
                                  }, UID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ follower_id = UID
                                  , pinned_messages = OldPinnedMessages
                                 }, UID } 
		    when OldPinnedMessages =:= PinnedMessages ->
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
			      { follower
			      , sp_chat_set_pinned_messages( CID
							   , LMeS
							   , LEvS
							   , 'FOLLOWER'
							   , PinnedMessages
							   )
			      }
		      end;
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_SEQ}} ->
		{ reply
		, {error, invalid_seq}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_ALREADY_SET}} ->   
                { reply
		, {ok, done}
		, cht(State)
		, Timeout
		};		
	    {starter, {ok, MDA, NLMeS, NLEvS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = NLMeS
					 , last_event_sequence = NLEvS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		};
	    {follower, {ok, MDA, NLMeS, NLEvS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = NLMeS
					 , last_event_sequence = NLEvS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;
%%%===================================================================
handle_call( {del_pin_message, UID}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
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
		      { starter
		      , sp_chat_del_pinned_messages( CID
						   , LMeS
						   , LEvS
						   , 'STARTER'
						   )
		      };
		  {#luna_chat_meta{ follower_id = UID
                                  , follower_is_deleted = true
                                  }, UID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , follower_id = UID
				  , last_message_sequence = LMeS
				  , last_event_sequence = LEvS
				  }, UID} -> 
		      { follower
		      , sp_chat_del_pinned_messages( CID
						   , LMeS
						   , LEvS
						   , 'FOLLOWER'
						   )
		      };
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {starter, {ok, MDA, NLMeS, NLEvS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = NLMeS
					 , last_event_sequence = NLEvS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		};
	    {follower, {ok, MDA, NLMeS, NLEvS}} ->
		NLCM = LCM#luna_chat_meta{ mda = MDA
					 , last_message_sequence = NLMeS
					 , last_event_sequence = NLEvS
					 },
		{ reply
		, {ok, done}
		, cht(State#luna_chat_state{chat_meta = NLCM})
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;
%%%===================================================================
handle_call( {get_messages, UID, From, Len}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
	   ) ->
    try
	DBR = case {LCM, UID} of
		  {#luna_chat_meta{ starter_id = UID
                                  , starter_is_deleted = true
                                  }, UID } ->
                      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID 
				  , starter_id = UID
				  , starter_start_sequence = SStS
				  }, UID} when From >= SStS ->
		      {starter, sp_chat_message_get(CID, From, Len)};
		  {#luna_chat_meta{ starter_id = UID
                                  }, UID}  ->
		      {starter, {error, ?DBE_INVALID_SEQ}};
		  {#luna_chat_meta{ follower_id = UID
                                  , follower_is_deleted = true
                                  }, UID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , follower_id = UID
				  , follower_start_sequence = FStS
				  }, UID} when From >= FStS ->
		      {follower, sp_chat_message_get(CID, From, Len)};
		  {#luna_chat_meta{ follower_id = UID
                                  }, UID}  ->
		      {follower, {error, ?DBE_INVALID_SEQ}};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_SEQ}} ->
		{ reply
		, {error, invalid_seq}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {starter, {ok, Messages}} ->
		{ reply
		, {ok, m(Messages, 'STARTER')}
		, cht(State)
		, Timeout
		};
	    {follower, {ok, Messages}} -> 
                { reply
		, {ok, m(Messages, 'FOLLOWER')}
		, cht(State)
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;
%%%===================================================================
handle_call( {get_medias, UID, Type, From, Len}
	   , _From
	   , #luna_chat_state{ chat_meta = LCM
			     , timeout = Timeout
			     } = State
	   ) ->
    try
	DBR = case {LCM, UID} of
		  {#luna_chat_meta{ starter_id = UID
                                  , starter_is_deleted = true
                                  }, UID } ->
                      {starter, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , starter_id = UID
				  , starter_start_sequence = SStS
				  }, UID} when From >= SStS ->
		      { starter
		      , sp_chat_storage_get(CID, UID, Type, From, Len)
		      };
		  {#luna_chat_meta{starter_id = UID}, UID}  ->
		      {starter, {error, ?DBE_INVALID_SEQ}};
		  {#luna_chat_meta{ follower_id = UID
                                  , follower_is_deleted = true
                                  }, UID } ->
                      {follower, {error, ?DBE_INVALID_CID}};
		  {#luna_chat_meta{ cid = CID
				  , follower_id = UID
				  , follower_start_sequence = FStS
				  }, UID} when From >= FStS ->
		      { follower
		      , sp_chat_storage_get(CID, UID, Type, From, Len)
		      };
		  {#luna_chat_meta{follower_id = UID}, UID}  ->
		      {follower, {error, ?DBE_INVALID_SEQ}};
		  _Else -> {unknown, {error, ?DBE_INVALID_UID}}
	      end,
	case DBR of
	    {_, {error, ?DBE_EXCEPTION}} -> 
		{ reply
		, {error, server_internal_error}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_SEQ}} ->
		{ reply
		, {error, invalid_seq}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_CID}} ->
		{ reply
		, {error, invalid_cid}
		, cht(State)
		, Timeout
		};
	    {_, {error, ?DBE_INVALID_UID}} ->
		{ reply
		, {error, invalid_uid}
		, cht(State)
		, Timeout
		};
	    {_, {ok, Messages}} ->
		{ reply
		, {ok, m(Messages, any)}
		, cht(State)
		, Timeout
		}
	end
    catch
	_:_ -> { reply
	       , {error, server_internal_error}
	       , cht(State)
	       , Timeout
	       }
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info( timeout
	   , #luna_chat_state{hibernate = Hibernate} = State ) ->
    HTiR = erlang:send_after(Hibernate, self(), stop),
    { noreply
    , State#luna_chat_state{hibernate_timer = HTiR}
    , hibernate
    };
handle_info( stop
	   , #luna_chat_state{hibernate_timer = HTiR} = State
	   ) when is_reference(HTiR) ->
    {stop, normal, State};
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
call(CID, Request) ->
    try
	case luna_rgst:get(CID) of
	    {ok, Pid} -> 
		gen_server:call(Pid, Request);
	    {error, undefined} -> 
		case gen_server:start(?MODULE, [CID], []) of
		    {ok, Pid} ->
			gen_server:call(Pid, Request);
		    _ ->
			{error, invalid_cid}
		end
	end
    catch
	_:_ -> {error, server_internal_error}
    end.

cht(#luna_chat_state{hibernate_timer = HTiR} = State) 
  when is_reference(HTiR) ->
    erlang:cancel_timer(HTiR), 
    State#luna_chat_state{hibernate_timer = undefined};
cht(State) -> State.

is_valid_pins(LMeS, StS, DeS, #{items := PinnedMessages}) ->
    is_valid_pins_(LMeS, StS, DeS, PinnedMessages);
is_valid_pins(_, _, _, _) -> false.
is_valid_pins_(_LMeS, _StS, _DeS, []) -> true;
is_valid_pins_(LMeS, StS, DeS, [E|R]) 
  when E >= StS, E =< LMeS, E =< DeS ->
    is_valid_pins_(LMeS, StS, DeS, R);
is_valid_pins_(_LMeS, _StS, _DeS, _PinnedMessages) -> false.

is_valid_opjects([]) -> true;
is_valid_opjects( [ #{ type := Type 
		     , body := Body
		     , mime := Mime
		     , oid := OID
		     } | R ] ) when Type =:= <<"FILE">>
				    , is_binary(Body)
				    , is_binary(Mime)
				    , is_binary(OID) ->
    is_valid_opjects(R);
is_valid_opjects( [ #{ <<"type">> := Type 
		     , <<"body">> := Body
		     , <<"mime">> := Mime
		     , <<"oid">> := OID
		     } | R ] ) when Type =:= <<"FILE">>
				    , is_binary(Body)
				    , is_binary(Mime)
				    , is_binary(OID) ->
    is_valid_opjects(R);
is_valid_opjects( [ #{ type := Type 
		     , body := Body
		     } | R ] ) when Type =:= <<"LINK">>
				    , is_binary(Body) ->
    is_valid_opjects(R);
is_valid_opjects( [ #{ <<"type">> := Type 
		     , <<"body">> := Body
		     } | R ] ) when Type =:= <<"LINK">>
				    , is_binary(Body) ->
    is_valid_opjects(R);
is_valid_opjects(_) -> false.

m(L, M) when is_list(L) ->
    m(L, M, []);
m(#luna_chat_message{} = LCM, 'FOLLOWER') ->
    #{ cra => LCM#luna_chat_message.cra
     , mda => LCM#luna_chat_message.mda
     , type => LCM#luna_chat_message.type
     , sequence => LCM#luna_chat_message.sequence
     , reply_sequence => LCM#luna_chat_message.reply_sequence
     , writer => LCM#luna_chat_message.writer
     , body => LCM#luna_chat_message.body
     , objects => LCM#luna_chat_message.objects
     , actions => LCM#luna_chat_message.actions
     , is_deleted_by_starter => LCM#luna_chat_message.is_deleted_by_starter
     , auto_delete => LCM#luna_chat_message.auto_delete
     , kivi => LCM#luna_chat_message.kivi
     , version => LCM#luna_chat_message.version
     };
m(#luna_chat_message{} = LCM, 'STARTER') ->
    #{ cra => LCM#luna_chat_message.cra
     , mda => LCM#luna_chat_message.mda
     , type => LCM#luna_chat_message.type
     , sequence => LCM#luna_chat_message.sequence
     , reply_sequence => LCM#luna_chat_message.reply_sequence
     , writer => LCM#luna_chat_message.writer
     , body => LCM#luna_chat_message.body
     , objects => LCM#luna_chat_message.objects
     , actions => LCM#luna_chat_message.actions
     , is_deleted_by_follower => LCM#luna_chat_message.is_deleted_by_follower
     , auto_delete => LCM#luna_chat_message.auto_delete
     , kivi => LCM#luna_chat_message.kivi
     , version => LCM#luna_chat_message.version
     };
m(#luna_chat_meta{} = LCM, _) ->
    #{ cid => LCM#luna_chat_meta.cid
     , cra => LCM#luna_chat_meta.cra
     , mda => LCM#luna_chat_meta.mda
     , starter_id => LCM#luna_chat_meta.starter_id
     , follower_id => LCM#luna_chat_meta.follower_id
     , last_message_sequence => LCM#luna_chat_meta.last_message_sequence
     , last_event_sequence => LCM#luna_chat_meta.last_event_sequence
     , pinned_messages => LCM#luna_chat_meta.pinned_messages
     , starter_start_sequence => LCM#luna_chat_meta.starter_start_sequence
     , starter_delivered_sequence => LCM#luna_chat_meta.starter_delivered_sequence
     , starter_seen_sequence => LCM#luna_chat_meta.starter_seen_sequence
     , starter_is_muted => LCM#luna_chat_meta.starter_is_muted
     , starter_is_blocked => LCM#luna_chat_meta.starter_is_blocked
     , starter_is_deleted => LCM#luna_chat_meta.starter_is_deleted
     , starter_auto_delete => LCM#luna_chat_meta.starter_auto_delete
     , follower_start_sequence => LCM#luna_chat_meta.follower_start_sequence
     , follower_delivered_sequence => LCM#luna_chat_meta.follower_delivered_sequence
     , follower_seen_sequence => LCM#luna_chat_meta.follower_seen_sequence
     , follower_is_muted => LCM#luna_chat_meta.follower_is_muted
     , follower_is_blocked => LCM#luna_chat_meta.follower_is_blocked
     , follower_is_deleted => LCM#luna_chat_meta.follower_is_deleted
     , follower_auto_delete => LCM#luna_chat_meta.follower_auto_delete
     , kivi => LCM#luna_chat_meta.kivi
     };    
m(#luna_chat_object{type = 'FILE'} = LCO, _) ->
    #{ cra => LCO#luna_chat_object.cra
     , type => LCO#luna_chat_object.type
     , sequence => LCO#luna_chat_object.sequence
     , mime => LCO#luna_chat_object.mime
     , body => LCO#luna_chat_object.body
     , oid => LCO#luna_chat_object.oid
     };
m(#luna_chat_object{type = 'LINK'} = LCO, _) ->
    #{ cra => LCO#luna_chat_object.cra
     , type => LCO#luna_chat_object.type
     , sequence => LCO#luna_chat_object.sequence
     , body => LCO#luna_chat_object.body
     }.

m([], _M, []) -> [];
m([], _M, Acc) -> Acc;
m([E|R], M, Acc) ->
    m(R, M, Acc ++ [m(E, M)]).
