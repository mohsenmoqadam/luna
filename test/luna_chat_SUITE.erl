
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
    [ add_simplex
    , get_simplex    
    , del_simplex
    , block_simplex
    , unblock_simplex
    , mute_simplex
    , unmute_simplex
    , set_kivi
    , set_auto_del
    ].

add_simplex(_Config) ->
    %%=== New chat without KiVi
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

    %%=== New chat with KiVi
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

    %%=== Already exist chat (StarterId1, FollowerId1) and without KiVi 
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
    
    %%=== Already exist chat (FollowerId1, StarterId1) and without KiVi 
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

    %%=== Already exist chat (StarterId1, FollowerId1) and with KiVi 
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

    %%=== Already exist chat (FollowerId1, StarterId1) and with KiVi 
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
   
    %%=== Same ID error without KiVi
    SameId = ?ID,
    {error, same_starter_and_follower} = luna_chat:add(SameId, SameId),
    
    %%=== Same ID error with KiVi
    {error, same_starter_and_follower} = luna_chat:add(SameId, SameId, #{k => v}),
    
    %%=== Invalid Params
    {error, invalid_params} = luna_chat:add(a, b),
    {error, invalid_params} = luna_chat:add(a, b, []),
    {error, invalid_params} = luna_chat:add(SameId, a),
    {error, invalid_params} = luna_chat:add(SameId, a, []),
    {error, invalid_params} = luna_chat:add(b, SameId),
    {error, invalid_params} = luna_chat:add(b, SameId, []),
    
    ok.

get_simplex(_Config) ->
    %%=== New chat without KiVi
    StarterId1 = ?ID,
    FollowerId1 = ?ID,

    {ok, new, #{cid := CID} = CM1} = luna_chat:add(StarterId1, FollowerId1),
    
    {ok, CM1} = luna_chat:get(CID, StarterId1),
    {ok, CM1} = luna_chat:get(CID, FollowerId1),

    {error, invalid_cid} = luna_chat:get(?ID, 1),
    {error, invalid_uid} = luna_chat:get(CID, ?ID),
    {error, invalid_params} = luna_chat:get(a, FollowerId1),
    {error, invalid_params} = luna_chat:get(CID, a),
    
    ok.

del_simplex(_Config) ->
    %%=== New chat without KiVi
    StarterId1 = ?ID,
    FollowerId1 = ?ID,

    { ok
    , new
    , #{ cid := CID
       , cra := CRA
       }
    } = luna_chat:add(StarterId1, FollowerId1),
    
    { ok
    , #{ cid := CID
       , cra := CRA
       , mda := MDA_S1
       , last_message_sequence := 2
       , starter_is_deleted := true
       , starter_start_sequence := 2
       , starter_delivered_sequence := 2
       , starter_seen_sequence := 2 
       }
    } = luna_chat:del(CID, StarterId1),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA_S1)))),

    { ok
    , #{ cid := CID
       , cra := CRA
       , mda := MDA_F1
       , last_message_sequence := 3
       , follower_is_deleted := true
       , follower_start_sequence := 3
       , follower_delivered_sequence := 3
       , follower_seen_sequence := 3
       }
    } = luna_chat:del(CID, FollowerId1),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA_F1)))),

    {error, invalid_uid} = luna_chat:del(CID, ?ID),
    {error, invalid_cid} = luna_chat:del(?ID, StarterId1),
    {error, invalid_cid} = luna_chat:del(CID, StarterId1),
    {error, invalid_cid} = luna_chat:del(CID, FollowerId1),
    {error, invalid_params} = luna_chat:del(a, b),
    
    ok.

block_simplex(_Config) ->
    %%=== New chat without KiVi
    StarterId1 = ?ID,
    FollowerId1 = ?ID,

    { ok
    , new
    , #{ cid := CID
       , cra := CRA
       }
    } = luna_chat:add(StarterId1, FollowerId1),
    
    { ok
    , #{ cid := CID
       , cra := CRA
       , mda := MDA_S1
       , last_message_sequence := 2 
       , starter_is_blocked := true
       }
    } = luna_chat:block(CID, StarterId1),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA_S1)))),

    { ok
    , #{ cid := CID
       , cra := CRA
       , mda := MDA_F1
       , last_message_sequence := 3
       , follower_is_blocked := true
       }
    } = luna_chat:block(CID, FollowerId1),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA_F1)))),

    {error, invalid_uid} = luna_chat:block(CID, ?ID),
    {error, invalid_cid} = luna_chat:block(?ID, StarterId1),
    {ok, already_set} = luna_chat:block(CID, StarterId1),
    {ok, already_set} = luna_chat:block(CID, FollowerId1),
    {error, invalid_params} = luna_chat:block(a, b),
    
    ok.

unblock_simplex(_) ->
    %%=== New chat without KiVi
    StarterId1 = ?ID,
    FollowerId1 = ?ID,

    { ok
    , new
    , #{ cid := CID
       , cra := CRA
       }
    } = luna_chat:add(StarterId1, FollowerId1),
    
    {ok, already_set} = luna_chat:unblock(CID, StarterId1),
    {ok, already_set} = luna_chat:unblock(CID, FollowerId1),

    {ok, #{cid := CID}} = luna_chat:block(CID, StarterId1),
    {ok, #{cid := CID}} = luna_chat:block(CID, FollowerId1),
    
    
    { ok
    , #{ cid := CID
       , cra := CRA
       , mda := MDA_S1
       , last_message_sequence := 4 
       , starter_is_blocked := false
       }
    } = luna_chat:unblock(CID, StarterId1),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA_S1)))),

    { ok
    , #{ cid := CID
       , cra := CRA
       , mda := MDA_F1
       , last_message_sequence := 5
       , follower_is_blocked := false
       }
    } = luna_chat:unblock(CID, FollowerId1),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA_F1)))),

    {error, invalid_uid} = luna_chat:unblock(CID, ?ID),
    {error, invalid_cid} = luna_chat:unblock(?ID, StarterId1),
    {error, invalid_params} = luna_chat:unblock(a, b),
    
    ok.

mute_simplex(_) ->
    %%=== New chat without KiVi
    StarterId1 = ?ID,
    FollowerId1 = ?ID,

    { ok
    , new
    , #{ cid := CID
       , cra := CRA
       }
    } = luna_chat:add(StarterId1, FollowerId1),
        
    { ok
    , #{ cid := CID
       , cra := CRA
       , mda := MDA_S1
       , last_message_sequence := 2 
       , starter_is_muted := true
       }
    } = luna_chat:mute(CID, StarterId1),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA_S1)))),

    { ok
    , #{ cid := CID
       , cra := CRA
       , mda := MDA_F1
       , last_message_sequence := 3
       , follower_is_muted := true
       }
    } = luna_chat:mute(CID, FollowerId1),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA_F1)))),

    {error, invalid_uid} = luna_chat:mute(CID, ?ID),
    {error, invalid_cid} = luna_chat:mute(?ID, StarterId1),
    {ok, already_set} = luna_chat:mute(CID, StarterId1),
    {ok, already_set} = luna_chat:mute(CID, FollowerId1),
    {error, invalid_params} = luna_chat:mute(a, b),
    
    ok.

unmute_simplex(_) ->
    %%=== New chat without KiVi
    StarterId1 = ?ID,
    FollowerId1 = ?ID,

    { ok
    , new
    , #{ cid := CID
       , cra := CRA
       } 
    } = luna_chat:add(StarterId1, FollowerId1),

    {ok, already_set} = luna_chat:unmute(CID, StarterId1),
    {ok, already_set} = luna_chat:unmute(CID, FollowerId1),
    
    {ok, #{cid := CID}} = luna_chat:mute(CID, StarterId1),
    {ok, #{cid := CID}} = luna_chat:mute(CID, FollowerId1),

    { ok
    , #{ cid := CID
       , cra := CRA
       , mda := MDA_S1
       , last_message_sequence := 4 
       , starter_is_muted := false
       }
    } = luna_chat:unmute(CID, StarterId1),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA_S1)))),

    { ok
    , #{ cid := CID
       , cra := CRA
       , mda := MDA_F1
       , last_message_sequence := 5
       , follower_is_muted := false
       }
    } = luna_chat:unmute(CID, FollowerId1),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA_F1)))),

    {error, invalid_uid} = luna_chat:unmute(CID, ?ID),
    {error, invalid_cid} = luna_chat:unmute(?ID, StarterId1),
    {error, invalid_params} = luna_chat:unmute(a, b),
    
    ok.

set_kivi(_) ->   
    %%=== New chat without KiVi
    StarterId1 = ?ID,
    FollowerId1 = ?ID,
    KiVi = #{<<"K1">> => <<"V1">>},

    {ok, new, #{cid := CID}} = luna_chat:add(StarterId1, FollowerId1),

    {ok, done} = luna_chat:set_kivi(CID, KiVi),
    {ok, #{kivi := #{<<"K1">> := <<"V1">>}}} = luna_chat:get(CID, StarterId1),
    {ok, #{kivi := #{<<"K1">> := <<"V1">>}}} = luna_chat:get(CID, FollowerId1),

    {error, invalid_cid} = luna_chat:set_kivi(?ID, #{}),
    {error, invalid_params} = luna_chat:set_kivi(a, b),

    ok.

set_auto_del(_) ->   
    %%=== New chat without KiVi
    StarterId1 = ?ID,
    FollowerId1 = ?ID,
    StarterAutoDel = 1000,
    FollowerAutoDel = 1001,

    {ok, new, #{cid := CID}} = luna_chat:add(StarterId1, FollowerId1),

    {ok, #{starter_auto_delete := null}} = luna_chat:get(CID, StarterId1),
    {ok, #{follower_auto_delete := null}} = luna_chat:get(CID, FollowerId1),

    {ok, done} = luna_chat:set_auto_del(CID, StarterId1, StarterAutoDel),
    {ok, done} = luna_chat:set_auto_del(CID, FollowerId1, FollowerAutoDel),

    {ok, _} = luna_chat:del(CID, FollowerId1),

    {ok, #{starter_auto_delete := StarterAutoDel}} = luna_chat:get(CID, StarterId1),
    {error,invalid_cid} = luna_chat:get(CID, FollowerId1),

    {error, invalid_cid} = luna_chat:set_auto_del(CID, FollowerId1, StarterAutoDel),
    {error, invalid_cid} = luna_chat:set_auto_del(?ID, StarterId1, StarterAutoDel),
    {error, invalid_params} = luna_chat:set_auto_del(a, b, c),

    ok.
