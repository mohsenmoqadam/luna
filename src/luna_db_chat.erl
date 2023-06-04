-module(luna_db_chat).

-export([ sp_chat_add/3
	]
       ).

-import( luna_utils
       , [ json/1
	 , date/1
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
	{ ok
	, _
	, [[ RC, CID, CRA, MDA, StarterId, FollowerId, LastMessageSequence
	   , LastEventSequence, PinnedMessages, StarterStartSequence
	   , StarterDeliveredSequence, StarterSeenSequence, StarterIsMuted
	   , StarterIsBlocked, StarterIsDeleted, StarterAutoDelete
	   , FollowerStartSequence, FollowerDeliveredSequence
	   , FollowerSeenSequence, FollowerIsMuted, FollowerIsBlocked
	   , FollowerIsDeleted, FollowerAutoDelete, KiKi
	   ]]
	} = luna_db_pool:do(Query, Params),
	{ ok
	, RC
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
		    , kivi = json(KiKi)
		    }
	}
    catch
	_:Eny -> {error, Eny}
    end.
