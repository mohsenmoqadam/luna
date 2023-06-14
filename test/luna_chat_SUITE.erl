
-module(luna_chat_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("luna.hrl").

-define(ID, rand:uniform(1000000000)).

suite() -> 
    [{timetrap,{seconds,60}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(luna),
    [{k, v}| Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [ add
    , get   
    , del
    , block
    , unblock
    , mute
    , unmute
    , set_kivi
    , set_auto_del
    ].

add(_Config) ->
    %% New chat without KiVi
    StarterId1 = ?ID,
    FollowerId1 = ?ID,
    { ok
    , new
    , #{ cid := CID1, cra := CRA1, mda := MDA1
       , starter_id := StarterId1
       , follower_id := FollowerId1
       , last_message_sequence := 1
       , last_event_sequence := 0
       , pinned_messages := null 
       , starter_start_sequence := 0
       , starter_delivered_sequence := 0 
       , starter_seen_sequence := 0
       , starter_is_muted := false
       , starter_is_blocked := false
       , starter_is_deleted := false
       , starter_auto_delete := null
       , follower_start_sequence := 0 
       , follower_delivered_sequence := 0 
       , follower_seen_sequence := 0
       , follower_is_muted := false
       , follower_is_blocked := false
       , follower_is_deleted := false
       , follower_auto_delete := null
       , kivi := null
       }
    } = luna_chat:add(StarterId1, FollowerId1),
    ?assert(is_integer(CID1)),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRA1)))),
    ?assertEqual(CRA1, MDA1),

    %% New chat with KiVi
    StarterId2 = ?ID,
    FollowerId2 = ?ID,
    { ok
    , new
    , #{ cid := CID2, cra := CRA2, mda := MDA2
       , starter_id := StarterId2
       , follower_id := FollowerId2
       , last_message_sequence := 1
       , last_event_sequence := 0
       , pinned_messages := null 
       , starter_start_sequence := 0
       , starter_delivered_sequence := 0 
       , starter_seen_sequence := 0
       , starter_is_muted := false
       , starter_is_blocked := false
       , starter_is_deleted := false
       , starter_auto_delete := null
       , follower_start_sequence := 0 
       , follower_delivered_sequence := 0 
       , follower_seen_sequence := 0
       , follower_is_muted := false
       , follower_is_blocked := false
       , follower_is_deleted := false
       , follower_auto_delete := null
       , kivi := #{<<"k">> := <<"v">>}
       }
    } = luna_chat:add(StarterId2, FollowerId2, #{k => v}),
    ?assert(is_integer(CID2)),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRA2)))),
    ?assertEqual(CRA2, MDA2),

    %% Already exist chat (StarterId1, FollowerId1) and without KiVi 
    { ok
    , already_exist
    , #{ cid := CID1, cra := CRA1, mda := MDA1
       , starter_id := StarterId1
       , follower_id := FollowerId1
       , last_message_sequence := 1
       , last_event_sequence := 0
       , pinned_messages := null 
       , starter_start_sequence := 0
       , starter_delivered_sequence := 0 
       , starter_seen_sequence := 0
       , starter_is_muted := false
       , starter_is_blocked := false
       , starter_is_deleted := false
       , starter_auto_delete := null
       , follower_start_sequence := 0 
       , follower_delivered_sequence := 0 
       , follower_seen_sequence := 0
       , follower_is_muted := false
       , follower_is_blocked := false
       , follower_is_deleted := false
       , follower_auto_delete := null
       , kivi := null
       }
    } = luna_chat:add(StarterId1, FollowerId1), 
    
    %% Already exist chat (FollowerId1, StarterId1) and without KiVi 
    { ok
    , already_exist
    , #{ cid := CID1, cra := CRA1, mda := MDA1
       , starter_id := StarterId1
       , follower_id := FollowerId1
       , last_message_sequence := 1
       , last_event_sequence := 0
       , pinned_messages := null 
       , starter_start_sequence := 0
       , starter_delivered_sequence := 0 
       , starter_seen_sequence := 0
       , starter_is_muted := false
       , starter_is_blocked := false
       , starter_is_deleted := false
       , starter_auto_delete := null
       , follower_start_sequence := 0 
       , follower_delivered_sequence := 0 
       , follower_seen_sequence := 0
       , follower_is_muted := false
       , follower_is_blocked := false
       , follower_is_deleted := false
       , follower_auto_delete := null
       , kivi := null
       }
    } = luna_chat:add(FollowerId1, StarterId1),

    %% Already exist chat (StarterId1, FollowerId1) and with KiVi 
    { ok
    , already_exist
    , #{ cid := CID1, cra := CRA1, mda := MDA_S1_F1
       , starter_id := StarterId1
       , follower_id := FollowerId1
       , last_message_sequence := 1
       , last_event_sequence := 0
       , pinned_messages := null 
       , starter_start_sequence := 0
       , starter_delivered_sequence := 0 
       , starter_seen_sequence := 0
       , starter_is_muted := false
       , starter_is_blocked := false
       , starter_is_deleted := false
       , starter_auto_delete := null
       , follower_start_sequence := 0 
       , follower_delivered_sequence := 0 
       , follower_seen_sequence := 0
       , follower_is_muted := false
       , follower_is_blocked := false
       , follower_is_deleted := false
       , follower_auto_delete := null
       , kivi := #{<<"k">> := <<"v">>}
       }
    } = luna_chat:add(StarterId1, FollowerId1, #{k => v}),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA_S1_F1)))),

    %% Already exist chat (FollowerId1, StarterId1) and with KiVi 
    { ok
    , already_exist
    , #{ cid := CID1, cra := CRA1, mda := MDA_F1_S1
       , starter_id := StarterId1
       , follower_id := FollowerId1
       , last_message_sequence := 1
       , last_event_sequence := 0
       , pinned_messages := null 
       , starter_start_sequence := 0
       , starter_delivered_sequence := 0 
       , starter_seen_sequence := 0
       , starter_is_muted := false
       , starter_is_blocked := false
       , starter_is_deleted := false
       , starter_auto_delete := null
       , follower_start_sequence := 0 
       , follower_delivered_sequence := 0 
       , follower_seen_sequence := 0
       , follower_is_muted := false
       , follower_is_blocked := false
       , follower_is_deleted := false
       , follower_auto_delete := null
       , kivi := #{<<"k">> := <<"v">>}
       }
    } = luna_chat:add(FollowerId1, StarterId1, #{k => v}),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA_F1_S1)))),
   
    %% Same ID error without KiVi
    SameId = ?ID,
    {error, same_starter_and_follower} = luna_chat:add(SameId, SameId),
    
    %% Same ID error with KiVi
    {error, same_starter_and_follower} = luna_chat:add(SameId, SameId, #{<<"k">> => <<"v">>}),
    
    %% Invalid Params
    {error, invalid_params} = luna_chat:add(a, b),
    {error, invalid_params} = luna_chat:add(a, b, []),
    {error, invalid_params} = luna_chat:add(SameId, a),
    {error, invalid_params} = luna_chat:add(SameId, a, []),
    {error, invalid_params} = luna_chat:add(b, SameId),
    {error, invalid_params} = luna_chat:add(b, SameId, []),
    
    ok.

get(_Config) ->
    %% Add chat
    StarterId = ?ID,
    FollowerId = ?ID,
    {ok, new, #{cid := CID} = ChatMeta} = luna_chat:add(StarterId, FollowerId),
    
    %% Get (normal)
    {ok, ChatMeta} = luna_chat:get(CID, StarterId),
    {ok, ChatMeta} = luna_chat:get(CID, FollowerId),
    #{ cid := CID
     , cra := CRA
     , mda := MDA
     , starter_id := StarterId1
     , follower_id := FollowerId1
     , last_message_sequence := LMeS
     , last_event_sequence := LEvS
     , pinned_messages := PM
     , starter_start_sequence := SStS
     , starter_delivered_sequence := SDeS 
     , starter_seen_sequence := SSeS
     , starter_is_muted := SIsM
     , starter_is_blocked := SIsB
     , starter_is_deleted := SIsD
     , starter_auto_delete := SAuD
     , follower_start_sequence := FStS 
     , follower_delivered_sequence := FDeS 
     , follower_seen_sequence := FSeS
     , follower_is_muted := FIsM
     , follower_is_blocked := FIsB
     , follower_is_deleted := FIsD
     , follower_auto_delete := FAuD
     , kivi := KiVi
     } = ChatMeta,
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRA)))),
    ?assert(is_integer(CID)),
    ?assert(is_integer(LMeS)),
    ?assert(is_integer(LEvS)),
    ?assert(is_integer(SStS)),
    ?assert(is_integer(SDeS)),
    ?assert(is_integer(SSeS)),
    ?assert(is_integer(FStS)),
    ?assert(is_integer(FDeS)),
    ?assert(is_integer(FSeS)),
    ?assert(is_boolean(SIsM)),
    ?assert(is_boolean(SIsB)),
    ?assert(is_boolean(SIsD)),
    ?assert(is_boolean(FIsM)),
    ?assert(is_boolean(FIsB)),
    ?assert(is_boolean(FIsD)),
    ?assert(PM =:= null),
    ?assert(SAuD =:= null),
    ?assert(FAuD =:= null),
    ?assert(KiVi =:= null),

    %% Get (error)
    {error, invalid_cid} = luna_chat:get(?ID, 1),
    {error, invalid_uid} = luna_chat:get(CID, ?ID),
    {error, invalid_params} = luna_chat:get(a, FollowerId1),
    {error, invalid_params} = luna_chat:get(CID, a),

    ok.

del(_Config) ->
    %% Add chat
    StarterId = ?ID,
    FollowerId = ?ID,
    {ok, new, #{cid := CID}} = luna_chat:add(StarterId, FollowerId),

    %% Del (normal)
    {ok, MDA1, LMeS1, FollowerId} = luna_chat:del(CID, StarterId),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA1)))),
    ?assert(LMeS1 =:= 2),
    {error, invalid_cid} = luna_chat:get(CID, StarterId),
    {ok, #{ mda := MDA2
	  , last_event_sequence := 0
	  , last_message_sequence := 2 
	  , starter_is_deleted := true
	  , starter_start_sequence := 2
	  , starter_delivered_sequence := 2
	  , starter_seen_sequence := 2
	  , follower_is_deleted := false
	  , follower_start_sequence := 0
	  , follower_delivered_sequence := 0 
	  , follower_seen_sequence := 0
	  }
    } = luna_chat:get(CID, FollowerId),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA2)))),
    {ok, MDA3, LMeS2, StarterId} = luna_chat:del(CID, FollowerId),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA3)))),
    ?assert(LMeS2 =:= 3),
    {error, invalid_cid} = luna_chat:get(CID, FollowerId),

    %% Del (error)
    {error, invalid_uid} = luna_chat:del(CID, ?ID),
    {error, invalid_cid} = luna_chat:del(?ID, StarterId),
    {error, invalid_cid} = luna_chat:del(CID, StarterId),
    {error, invalid_cid} = luna_chat:del(CID, FollowerId),
    {error, invalid_params} = luna_chat:del(a, b),
    
    ok.

block(_Config) ->
    %% Add chat 
    StarterId = ?ID,
    FollowerId = ?ID,
    {ok, new, #{cid := CID}} = luna_chat:add(StarterId, FollowerId),
    
    %% Block (normal)
    {ok, MDA1, LMeS1, FollowerId} = luna_chat:block(CID, StarterId),
    ?assert(LMeS1 =:= 2),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA1)))),
    {ok, #{ mda := MDA1
          , last_event_sequence := 0
          , last_message_sequence := LMeS1
          , starter_is_blocked := true
          , follower_is_blocked := false
          }
    } = luna_chat:get(CID, FollowerId),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA1)))),
    {ok, MDA2, LMeS2, StarterId} = luna_chat:block(CID, FollowerId),
    ?assert(LMeS2 =:= 3),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA2)))),
    {ok, #{ mda := MDA2
          , last_event_sequence := 0
          , last_message_sequence := LMeS2
          , starter_is_blocked := true
          , follower_is_blocked := true
          }
    } = luna_chat:get(CID, FollowerId),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA2)))),
    
    %% Block (error)
    {error, invalid_uid} = luna_chat:block(CID, ?ID),
    {error, invalid_cid} = luna_chat:block(?ID, StarterId),
    {ok, already_set} = luna_chat:block(CID, StarterId),
    {ok, already_set} = luna_chat:block(CID, FollowerId),
    {error, invalid_params} = luna_chat:block(a, b),
    
    ok.

unblock(_Config) ->
    %% Add chat 
    StarterId = ?ID,
    FollowerId = ?ID,
    {ok, new, #{cid := CID}} = luna_chat:add(StarterId, FollowerId),
    {ok, _, _, _} = luna_chat:block(CID, StarterId),
    {ok, _, _, _} = luna_chat:block(CID, FollowerId),

    %% Unblock (normal)
    {ok, MDA1, LMeS1, FollowerId} = luna_chat:unblock(CID, StarterId),
    ?assert(LMeS1 =:= 4),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA1)))),
    {ok, #{ mda := MDA1
          , last_event_sequence := 0
          , last_message_sequence := LMeS1
          , starter_is_blocked := false
          , follower_is_blocked := true
          }
    } = luna_chat:get(CID, FollowerId),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA1)))),
    {ok, MDA2, LMeS2, StarterId} = luna_chat:unblock(CID, FollowerId),
    ?assert(LMeS2 =:= 5),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA2)))),
    {ok, #{ mda := MDA2
          , last_event_sequence := 0
          , last_message_sequence := LMeS2
          , starter_is_blocked := false
          , follower_is_blocked := false
          }
    } = luna_chat:get(CID, FollowerId),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA2)))),
    
    %% Unlock (error)
    {error, invalid_uid} = luna_chat:unblock(CID, ?ID),
    {error, invalid_cid} = luna_chat:unblock(?ID, StarterId),
    {ok, already_set} = luna_chat:unblock(CID, StarterId),
    {ok, already_set} = luna_chat:unblock(CID, FollowerId),
    {error, invalid_params} = luna_chat:unblock(a, b),
    
    ok.  

mute(_Config) ->
    %% Add chat 
    StarterId = ?ID,
    FollowerId = ?ID,
    {ok, new, #{cid := CID}} = luna_chat:add(StarterId, FollowerId),
    
    %% Mute (normal)
    {ok, MDA1, LMeS1, FollowerId} = luna_chat:mute(CID, StarterId),
    ?assert(LMeS1 =:= 2),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA1)))),
    {ok, #{ mda := MDA1
          , last_event_sequence := 0
          , last_message_sequence := LMeS1
          , starter_is_muted := true
          , follower_is_muted := false
          }
    } = luna_chat:get(CID, FollowerId),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA1)))),
    {ok, MDA2, LMeS2, StarterId} = luna_chat:mute(CID, FollowerId),
    ?assert(LMeS2 =:= 3),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA2)))),
    {ok, #{ mda := MDA2
          , last_event_sequence := 0
          , last_message_sequence := LMeS2
          , starter_is_muted := true
          , follower_is_muted := true
          }
    } = luna_chat:get(CID, FollowerId),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA2)))),
    
    %% Mute (error)
    {error, invalid_uid} = luna_chat:mute(CID, ?ID),
    {error, invalid_cid} = luna_chat:mute(?ID, StarterId),
    {ok, already_set} = luna_chat:mute(CID, StarterId),
    {ok, already_set} = luna_chat:mute(CID, FollowerId),
    {error, invalid_params} = luna_chat:mute(a, b),
    
    ok.

unmute(_) ->
    %% Add chat 
    StarterId = ?ID,
    FollowerId = ?ID,
    {ok, new, #{cid := CID}} = luna_chat:add(StarterId, FollowerId),
    {ok, _, _, _} = luna_chat:mute(CID, StarterId),
    {ok, _, _, _} = luna_chat:mute(CID, FollowerId),

    %% Unmute (normal)
    {ok, MDA1, LMeS1, FollowerId} = luna_chat:unmute(CID, StarterId),
    ?assert(LMeS1 =:= 4),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA1)))),
    {ok, #{ mda := MDA1
          , last_event_sequence := 0
          , last_message_sequence := LMeS1
          , starter_is_muted := false
          , follower_is_muted := true
          }
    } = luna_chat:get(CID, FollowerId),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA1)))),
    {ok, MDA2, LMeS2, StarterId} = luna_chat:unmute(CID, FollowerId),
    ?assert(LMeS2 =:= 5),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA2)))),
    {ok, #{ mda := MDA2
          , last_event_sequence := 0
          , last_message_sequence := LMeS2
          , starter_is_muted := false
          , follower_is_muted := false
          }
    } = luna_chat:get(CID, FollowerId),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA2)))),
    
    %% Block (error)
    {error, invalid_uid} = luna_chat:unmute(CID, ?ID),
    {error, invalid_cid} = luna_chat:unmute(?ID, StarterId),
    {ok, already_set} = luna_chat:unmute(CID, StarterId),
    {ok, already_set} = luna_chat:unmute(CID, FollowerId),
    {error, invalid_params} = luna_chat:unmute(a, b),
    
    ok.

set_kivi(_) ->   
    %% Add chat
    StarterId = ?ID,
    FollowerId = ?ID,
    KiVi = #{<<"K1">> => <<"V1">>},
    {ok, new, #{cid := CID}} = luna_chat:add(StarterId, FollowerId),

    %% Set kivi (normal)
    {ok, MDA, StarterId, FollowerId} = luna_chat:set_kivi(CID, KiVi),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA)))),
    {ok, #{kivi := #{<<"K1">> := <<"V1">>}}} = luna_chat:get(CID, StarterId),
    {ok, #{kivi := #{<<"K1">> := <<"V1">>}}} = luna_chat:get(CID, FollowerId),

    %% Set kivi (error) 
    {error, invalid_cid} = luna_chat:set_kivi(?ID, #{}),
    {error, invalid_params} = luna_chat:set_kivi(a, b),

    ok.

set_auto_del(_) ->   
    %% Add chat
    StarterId = ?ID,
    FollowerId = ?ID,
    StarterAutoDel = 1000,
    FollowerAutoDel = 1001,
    {ok, new, #{cid := CID}} = luna_chat:add(StarterId, FollowerId),

    %% Set auto del (normal)
    {ok, #{starter_auto_delete := null}} = luna_chat:get(CID, StarterId),
    {ok, #{follower_auto_delete := null}} = luna_chat:get(CID, FollowerId),

    {ok, MDA1, StarterId, FollowerId} = luna_chat:set_auto_del(CID, StarterId, StarterAutoDel),
    {ok, MDA2, StarterId, FollowerId} = luna_chat:set_auto_del(CID, FollowerId, FollowerAutoDel),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA1)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA2)))),
    
    {ok, #{starter_auto_delete := StarterAutoDel}} = luna_chat:get(CID, StarterId),
    {ok, #{follower_auto_delete := FollowerAutoDel}} = luna_chat:get(CID, FollowerId),
    {ok, _, _, _} = luna_chat:del(CID, StarterId),
    
    %% Set auto del (error)
    {error, invalid_cid} = luna_chat:set_auto_del(CID, StarterId, StarterAutoDel),
    {error, invalid_uid} = luna_chat:set_auto_del(CID, ?ID, StarterAutoDel),
    {error, invalid_cid} = luna_chat:set_auto_del(?ID, StarterId, StarterAutoDel),
    {error, invalid_params} = luna_chat:set_auto_del(a, b, c),

    ok.
