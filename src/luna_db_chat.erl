-module(luna_db_chat).

-export([ sp_chat_add/3
	, sp_chat_get_by_cid/1
	, sp_chat_del_starter/1
	, sp_chat_del_follower/1
	, sp_chat_set/1
	, sp_chat_set_kivi/2
	, sp_chat_set_pinned_messages/2
	, sp_chat_set_starter_blocked_state/2
	, sp_chat_set_follower_blocked_state/2
	, sp_chat_set_starter_muted_state/2
	, sp_chat_set_follower_muted_state/2
	, sp_chat_set_starter_delivered_sequence/2
	, sp_chat_set_follower_delivered_sequence/2
	, sp_chat_set_starter_seen_sequence/2
	, sp_chat_set_follower_seen_sequence/2
	, sp_chat_set_starter_auto_delete/2
	, sp_chat_set_follower_auto_delete/2
	]
       ).

-export([ sp_chat_message_add/10
	]).

-import( luna_utils
       , [ json/1
	 , date/1
	 , bool/1
	 ]
       ).

-include("luna.hrl").

%% =================================================================================
%% SP: luna_dev_db.sp_chat_add
%% =================================================================================
sp_chat_add(StarterID, FollowerID, KiVi) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_add(?, ?, ?)">>,
	Params = [StarterID, FollowerID, json(KiVi)],
	case luna_db_pool:do(Query, Params) of 
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2]]} -> {error, ?DBE_CHAT_SAME_ID_AS_SIDES};
	    { ok
	    , _
	    , [[ RC, CID, CRA, MDA, StarterId, FollowerId, LastMessageSequence
	       , LastEventSequence, PinnedMessages, StarterStartSequence
	       , StarterDeliveredSequence, StarterSeenSequence, StarterIsMuted
	       , StarterIsBlocked, StarterIsDeleted, StarterAutoDelete
	       , FollowerStartSequence, FollowerDeliveredSequence
	       , FollowerSeenSequence, FollowerIsMuted, FollowerIsBlocked
	       , FollowerIsDeleted, FollowerAutoDelete, KiVi
	       ]]
	    } ->
		{ ok
		, case RC of
		      3 -> ?DBI_CHAT_ALREADY_EXIST;
		      4 -> ?DBI_CHAT_ALREADY_EXIST_BOTH_SIDES_HAD_BEEN_DELETED;
		      5 -> ?DBI_CHAT_ALREADY_EXIST_STARTER_HAD_BEEN_DELETED;
		      6 -> ?DBI_CHAT_ALREADY_EXIST_FOLLOWER_HAD_BEEN_DELETED;
		      7 -> ?DBI_CHAT_NEW
		  end
		, #luna_chat{ cid = CID
			    , cra = date(CRA)
			    , mda = date(MDA)
			    , starter_id = StarterId
			    , follower_id = FollowerId 
			    , last_message_sequence = LastMessageSequence
			    , last_event_sequence = LastEventSequence 
			    , pinned_messages = json(PinnedMessages) 
			    , starter_start_sequence = StarterStartSequence
			    , starter_delivered_sequence = StarterDeliveredSequence
			    , starter_seen_sequence = StarterSeenSequence
			    , starter_is_muted = StarterIsMuted
			    , starter_is_blocked = StarterIsBlocked
			    , starter_is_deleted = StarterIsDeleted
			    , starter_auto_delete = StarterAutoDelete
			    , follower_start_sequence = FollowerStartSequence
			    , follower_delivered_sequence = FollowerDeliveredSequence
			    , follower_seen_sequence = FollowerSeenSequence
			    , follower_is_muted = FollowerIsMuted
			    , follower_is_blocked = FollowerIsBlocked
			    , follower_is_deleted = FollowerIsDeleted
			    , follower_auto_delete = FollowerAutoDelete
			    , kivi = json(KiVi)
			    }
		}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_get_by_cid
%% =================================================================================
sp_chat_get_by_cid(CID) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_get_by_cid(?)">>,
	Params = [CID],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2]]} -> {error, ?DBE_INVALID_CID};
	    {ok, _, [[ 3, CID, CRA, MDA, StarterId, FollowerId, LastMessageSequence
		     , LastEventSequence, PinnedMessages, StarterStartSequence
		     , StarterDeliveredSequence, StarterSeenSequence, StarterIsMuted
		     , StarterIsBlocked, StarterIsDeleted, StarterAutoDelete
		     , FollowerStartSequence, FollowerDeliveredSequence
		     , FollowerSeenSequence, FollowerIsMuted, FollowerIsBlocked
		     , FollowerIsDeleted, FollowerAutoDelete, KiVi 
		     ]]} -> 
		{ok, #luna_chat{ cid = CID
			       , cra = date(CRA)
			       , mda = date(MDA)
			       , starter_id = StarterId
			       , follower_id = FollowerId 
			       , last_message_sequence = LastMessageSequence
			       , last_event_sequence = LastEventSequence 
			       , pinned_messages = json(PinnedMessages) 
			       , starter_start_sequence = StarterStartSequence
			       , starter_delivered_sequence = StarterDeliveredSequence
			       , starter_seen_sequence = StarterSeenSequence
			       , starter_is_muted = StarterIsMuted
			       , starter_is_blocked = StarterIsBlocked
			       , starter_is_deleted = StarterIsDeleted
			       , starter_auto_delete = StarterAutoDelete
			       , follower_start_sequence = FollowerStartSequence
			       , follower_delivered_sequence = FollowerDeliveredSequence
			       , follower_seen_sequence = FollowerSeenSequence
			       , follower_is_muted = FollowerIsMuted
			       , follower_is_blocked = FollowerIsBlocked
			       , follower_is_deleted = FollowerIsDeleted
			       , follower_auto_delete = FollowerAutoDelete
			       , kivi = json(KiVi) }}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_del_starter
%% =================================================================================
sp_chat_del_starter(CID) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_del_starter(?)">>,
	Params = [CID],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2]]} -> {error, ?DBE_INVALID_CID};
	    {ok, _, [[3]]} -> {error, ?DBE_ALREADY_SET};
	    { ok
	    , _
	    , [[ 4, MDA, StarterStartSequence, StarterDeliveredSequence
	       , StarterSeenSequence, LastMessageSequence ]]
	    } ->
		{ ok, date(MDA)
		, LastMessageSequence, StarterStartSequence
		, StarterDeliveredSequence, StarterSeenSequence
		}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_del_follower
%% =================================================================================
sp_chat_del_follower(CID) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_del_follower(?)">>,
	Params = [CID],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2]]} -> {error, ?DBE_INVALID_CID};
	    {ok, _, [[3]]} -> {error, ?DBE_ALREADY_SET};
	    { ok
	    , _
	    , [[ 4, MDA, FollowerStartSequence, FollowerDeliveredSequence
	       , FollowerSeenSequence, LastMessageSequence ]]
	    } ->
		{ ok, date(MDA)
		, LastMessageSequence, FollowerStartSequence
		, FollowerDeliveredSequence, FollowerSeenSequence
		}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_set
%% =================================================================================
sp_chat_set(#luna_chat{ cid = CID
		      , starter_id = StarterId
		      , follower_id = FollowerId 
		      , last_message_sequence = LastMessageSequence
		      , last_event_sequence = LastEventSequence 
		      , pinned_messages = PinnedMessages 
		      , starter_start_sequence = StarterStartSequence
		      , starter_delivered_sequence = StarterDeliveredSequence
		      , starter_seen_sequence = StarterSeenSequence
		      , starter_is_muted = StarterIsMuted
		      , starter_is_blocked = StarterIsBlocked
		      , starter_is_deleted = StarterIsDeleted
		      , starter_auto_delete = StarterAutoDelete
		      , follower_start_sequence = FollowerStartSequence
		      , follower_delivered_sequence = FollowerDeliveredSequence
		      , follower_seen_sequence = FollowerSeenSequence
		      , follower_is_muted = FollowerIsMuted
		      , follower_is_blocked = FollowerIsBlocked
		      , follower_is_deleted = FollowerIsDeleted
		      , follower_auto_delete = FollowerAutoDelete
		      , kivi = KiVi } = LunaChat) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_set(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>,
	Params = [ CID, StarterId, FollowerId, LastMessageSequence
		 , LastEventSequence, json(PinnedMessages), StarterStartSequence
		 , StarterDeliveredSequence, StarterSeenSequence, StarterIsMuted
		 , StarterIsBlocked, StarterIsDeleted, StarterAutoDelete
		 , FollowerStartSequence, FollowerDeliveredSequence
		 , FollowerSeenSequence, FollowerIsMuted, FollowerIsBlocked
		 , FollowerIsDeleted, FollowerAutoDelete, json(KiVi) 
		 ],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2]]} -> {error, ?DBE_INVALID_CID};
	    {ok, _, [[3, CRA, MDA]]} -> 
		{ok, LunaChat#luna_chat{mda = date(MDA), cra = date(CRA)}}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_set_kivi
%% =================================================================================
sp_chat_set_kivi(CID, KiVi) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_set_kivi(?, ?)">>,
	Params = [CID, json(KiVi)],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2, MDA]]} -> {ok, date(MDA)}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_set_pinned_messages
%% =================================================================================
sp_chat_set_pinned_messages(CID, PinnedMessages) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_set_pinned_messages(?, ?)">>,
	Params = [CID, json(PinnedMessages)],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2, MDA]]} -> {ok, date(MDA)}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_set_starter_blocked_state
%% =================================================================================
sp_chat_set_starter_blocked_state(CID, StarterBlockedState) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_set_starter_blocked_state(?, ?)">>,
	Params = [CID, bool(StarterBlockedState)],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2]]} -> {error, ?DBE_INVALID_CID};
	    {ok, _, [[3]]} -> {error, ?DBE_ALREADY_SET};
	    { ok
	    , _
	    , [[4, MDA, LastMessageSequence]]
	    } ->
		{ok, date(MDA), LastMessageSequence}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_set_follower_blocked_state
%% =================================================================================
sp_chat_set_follower_blocked_state(CID, FollowerBlockedState) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_set_follower_blocked_state(?, ?)">>,
	Params = [CID, bool(FollowerBlockedState)],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2]]} -> {error, ?DBE_INVALID_CID};
	    {ok, _, [[3]]} -> {error, ?DBE_ALREADY_SET};
	    { ok
	    , _
	    , [[4, MDA, LastMessageSequence]]
	    } ->
		{ok, date(MDA), LastMessageSequence}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_set_starter_muted_state
%% =================================================================================
sp_chat_set_starter_muted_state(CID, StarterMutedState) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_set_starter_muted_state(?, ?)">>,
	Params = [CID, bool(StarterMutedState)],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2]]} -> {error, ?DBE_INVALID_CID};
	    {ok, _, [[3]]} -> {error, ?DBE_ALREADY_SET};
	    {ok, _, [[4, MDA, LastMessageSequence]]} -> 
		{ok, date(MDA), LastMessageSequence}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_set_follower_muted_state
%% =================================================================================
sp_chat_set_follower_muted_state(CID, FollowerMutedState) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_set_follower_muted_state(?, ?)">>,
	Params = [CID, bool(FollowerMutedState)],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2]]} -> {error, ?DBE_INVALID_CID};
	    {ok, _, [[3]]} -> {error, ?DBE_ALREADY_SET};
	    {ok, _, [[4, MDA, LastMessageSequence]]} ->
		{ok, date(MDA), LastMessageSequence}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_set_starter_delivered_sequence
%% =================================================================================
sp_chat_set_starter_delivered_sequence(CID, StarterDelivedState) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_set_starter_delivered_sequence(?, ?)">>,
	Params = [CID, StarterDelivedState],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2, MDA]]} -> {ok, date(MDA)}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_set_follower_delivered_sequence
%% =================================================================================
sp_chat_set_follower_delivered_sequence(CID, FollowerDeliveredState) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_set_follower_delivered_sequence(?, ?)">>,
	Params = [CID, FollowerDeliveredState],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2, MDA]]} -> {ok, date(MDA)}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_set_starter_seen_sequence
%% =================================================================================
sp_chat_set_starter_seen_sequence(CID, StarterSeenState) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_set_starter_seen_sequence(?, ?)">>,
	Params = [CID, StarterSeenState],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2, MDA]]} -> {ok, date(MDA)}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_set_follower_seen_sequence
%% =================================================================================
sp_chat_set_follower_seen_sequence(CID, FollowerSeenState) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_set_follower_seen_sequence(?, ?)">>,
	Params = [CID, FollowerSeenState],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2, MDA]]} -> {ok, date(MDA)}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_set_starter_auto_delete
%% =================================================================================
sp_chat_set_starter_auto_delete(CID, StarterAutoDelete) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_set_starter_auto_delete(?, ?)">>,
	Params = [CID, StarterAutoDelete],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2, MDA]]} -> {ok, date(MDA)}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_set_follower_auto_delete
%% =================================================================================
sp_chat_set_follower_auto_delete(CID, FollowerAutoDelete) ->
    try
	Query = <<"CALL luna_dev_db.sp_chat_set_follower_auto_delete(?, ?)">>,
	Params = [CID, FollowerAutoDelete],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2, MDA]]} -> {ok, date(MDA)}
	end
    catch
	_:Eny -> {error, Eny}
    end.

%% =================================================================================
%% SP: luna_dev_db.sp_chat_message_add
%% Object: #{objects => [ #{ type => 'FILE'
%%                         , body => <<"body">>
%%                         , mime => <<"image/png">>
%%                         , oid => <<"oid">>
%%                         }
%%                      , #{ type => <<"LINK">>
%%                         , body => <<"body-">>
%%                         }
%%                      ]
%%          }
%% =================================================================================
sp_chat_message_add( CID, StarterId, FollowerId, Writer, MessageSequence
		   , ReplySequence, Body, Objects0, AutoDelete, KiVi ) ->
    try
	Objects = maps:get(objects, Objects0, []),
	Query = <<"CALL luna_dev_db.sp_chat_message_add(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>,
	Params = [ CID, StarterId, FollowerId, Writer, MessageSequence
		 , ReplySequence, Body, json(Objects), AutoDelete, json(KiVi) ],
	case luna_db_pool:do(Query, Params) of
	    {ok, _, [[1]]} -> {error, ?DBE_EXCEPTION};
	    {ok, _, [[2, CRA, ObjectCount]]} -> {ok, date(CRA), ObjectCount}
	end.
    catch
	_:Eny -> {error, Eny}
    end.
