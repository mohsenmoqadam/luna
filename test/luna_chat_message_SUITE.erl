-module(luna_chat_message_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("luna.hrl").

-define(ID, rand:uniform(1000000000)).
-define(STR(Len), luna_test_utils:get_rand_string(Len)).

suite() -> 
    [{timetrap,{seconds,60}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(luna),
    [{k, v} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() ->
    [ add_message 
    , get_message
    , get_medias
    , set_delivered
    , set_seen
    , set_message
    , del_message
    , set_message_action
    , pin_message
    ].

add_message(_) ->
    StarterID = ?ID,
    FollowerID = ?ID,
    File = #{ type => 'FILE'
	    , body => ?STR(16)
	    , mime => ?STR(16)
	    , oid => ?STR(64)
	    },
    Link = #{ type => 'LINK'
	    , body => ?STR(64)
	    },
    {ok, new, #{cid := CID}} = luna_chat:add(StarterID, FollowerID),

    %% Add (normal)
    {ok, MAD1, CRA1, 2, FollowerID} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MAD1)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRA1)))),
    {ok, MAD2, CRA2, 3, FollowerID} = luna_chat:add_message(CID, StarterID, null, <<"">>, #{items => [File]}),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MAD2)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRA2)))),
    {ok, MAD3, CRA3, 4, StarterID} = luna_chat:add_message(CID, FollowerID, null, ?STR(16), #{items => [File, Link]}),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MAD3)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRA3)))),      
    {ok, MAD4, CRA4, 5, FollowerID} = luna_chat:add_message(CID, StarterID, null, ?STR(16), null, #{a => b}),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MAD4)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRA4)))),
    {ok, MAD5, CRA5, 6, StarterID} = luna_chat:add_message(CID, FollowerID, null, ?STR(16), #{items => [File, Link]}, #{a => b}),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MAD5)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRA5)))),

    %% Add (error)
    {error, invalid_body} = luna_chat:add_message(CID, StarterID, null, <<"">>),
    {error, invalid_params} = luna_chat:add_message(CID, StarterID, null, ?STR(16), #{items_ => [File]}),
    {error, invalid_params} = luna_chat:add_message(CID, StarterID, null, atom, #{items => [File]}),
    {error, invalid_uid} = luna_chat:add_message(CID, ?ID, null, ?STR(16), #{items => [File]}),
    {error, invalid_cid} = luna_chat:add_message(?ID, StarterID, null, ?STR(16), #{items => [File]}),
    {error, invalid_seq} = luna_chat:add_message(CID, StarterID, 1, ?STR(16), #{items => [File]}),

    %% Check after error
    {ok, _, _, 7, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16), null),

    ok.

get_message(_) ->
    StarterID = ?ID,
    FollowerID = ?ID,
    File = #{ type => 'FILE'
	    , body => <<"--FILE-BODY--">>
	    , mime => <<"--FILE-MIME--">>
	    , oid => <<"--FILE-OID--">>
	    },
    Link = #{ type => 'LINK'
	    , body => <<"--LIINK-BODY--">>
	    },
    {ok, new, #{cid := CID}} = luna_chat:add(StarterID, FollowerID),

    %% Add some messages
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, FollowerID, null, <<"--BODY-5--">>, null, #{<<"K">> => <<"V">>}),
    {ok, _, _, S1, _} = luna_chat:add_message(CID, StarterID, null, <<"--BODY-6--">>, #{items => [File, Link]}, #{<<"K">> => <<"V">>}),

    %% Get (normal)
    {ok, [ #{ actions := null
	    , auto_delete := null
	    , body := <<"--BODY-5--">>
	    , cra := CRA5
	    , mda := MDA5
	    , objects := null
	    , reply_sequence := null
	    , sequence := 5
	    , type := 'MESSAGE'
	    , version := 0
	    , writer := 'FOLLOWER'
	    , is_deleted_by_follower := false
	    , kivi := #{<<"K">> := <<"V">>}
	    }
	 , #{ actions := null
	    , auto_delete := null
	    , body := <<"--BODY-6--">>
	    , cra := CRA6
	    , mda := MDA6
	    , objects := #{ items := [ #{ <<"body">> := <<"--FILE-BODY--">>
					, <<"mime">> := <<"--FILE-MIME--">>
					, <<"oid">> := <<"--FILE-OID--">>
					, <<"type">> := <<"FILE">>
					}
				     , #{ <<"body">> := <<"--LIINK-BODY--">>
					, <<"type">> := <<"LINK">>
					}
				     ]
			  }
	    , reply_sequence := null
	    , sequence := 6
	    , type := 'MESSAGE'
	    , version := 0
	    , writer := 'STARTER'
	    , is_deleted_by_follower := false
	    , kivi := #{<<"K">> := <<"V">>}
	    }
	 ]
    } = luna_chat:get_messages(CID, StarterID, 5, S1),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRA5)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA5)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRA6)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA6)))),

    %% get (error)
    {error, invalid_cid} = luna_chat:get_messages(?ID, ?ID, 1, 5),
    {error, invalid_uid} = luna_chat:get_messages(CID, ?ID, 1, 5),
    {error, invalid_seq} = luna_chat:get_messages(CID, StarterID, -1, 10),
    {error, invalid_params} = luna_chat:get_messages(CID, StarterID, a, 10),
    {error, invalid_params} = luna_chat:get_messages(CID, a, 1, 10),
    {error, invalid_params} = luna_chat:get_messages(a, StarterID, 1, 10),

    ok.

get_medias(_) ->
    StarterID = ?ID,
    FollowerID = ?ID,
    File = #{ type => 'FILE'
	    , body => <<"--FILE-BODY--">>
	    , mime => <<"--FILE-MIME--">>
	    , oid => <<"--FILE-OID--">>
	    },
    Link = #{ type => 'LINK'
	    , body => <<"--LIINK-BODY--">>
	    },
    {ok, new, #{cid := CID}} = luna_chat:add(StarterID, FollowerID),

    %% Add some messages
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16), #{items => [File, Link]}, null),
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16), #{items => [File, Link]}, null),
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16), #{items => [File]}, null),
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16), #{items => [Link]}, null),
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16), #{items => [File]}, null),
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16), #{items => [Link]}, null),

    %% Get (normal)
    {ok, [ #{ body := <<"--FILE-BODY--">>
	    , cra := CRAF3
	    , mime := <<"--FILE-MIME--">>
	    , oid := <<"--FILE-OID--">>
	    , sequence := 3
	    , type := 'FILE'
	    }
	 , #{ body := <<"--FILE-BODY--">>
	    , cra := CRAF4
	    , mime := <<"--FILE-MIME--">>
	    , oid := <<"--FILE-OID--">>
	    , sequence := 4
	    , type := 'FILE'
	    }
	 , #{ body := <<"--FILE-BODY--">>
	    , cra := CRAF6
	    , mime := <<"--FILE-MIME--">>
	    , oid := <<"--FILE-OID--">>
	    , sequence := 6
	    , type := 'FILE'
	    } 
	 ]} = luna_chat:get_medias(CID, StarterID, 'FILE', 3, 3),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRAF3)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRAF4)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRAF6)))),

    {ok, [ #{ body := <<"--LIINK-BODY--">>
	    , cra := CRAL3
	    , sequence := 3
	    , type := 'LINK'
	    }
	 , #{ body := <<"--LIINK-BODY--">>,
	      cra := CRAL5
	    , sequence := 5
	    , type := 'LINK'
	    }
	 , #{ body := <<"--LIINK-BODY--">>
	    , cra := CRAL7 
	    , sequence := 7
            , type := 'LINK'
	    }
	 ]} = luna_chat:get_medias(CID, StarterID, 'LINK', 3, 3),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRAL3)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRAL5)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(CRAL7)))),

    %% get (error)
    {error, invalid_cid} = luna_chat:get_medias(?ID, ?ID, 'FILE', 1, 5),
    {error, invalid_cid} = luna_chat:get_medias(?ID, ?ID, 'LINK', 1, 5),
    {error, invalid_uid} = luna_chat:get_medias(CID, ?ID, 'FILE', 1, 5),
    {error, invalid_uid} = luna_chat:get_medias(CID, ?ID, 'LINK', 1, 5),
    {error, invalid_seq} = luna_chat:get_medias(CID, StarterID, 'FILE', -1, 10),
    {error, invalid_seq} = luna_chat:get_medias(CID, StarterID, 'LINK', -1, 10),
    {error, invalid_params} = luna_chat:get_medias(CID, StarterID, 'FILE', a, 10),
    {error, invalid_params} = luna_chat:get_medias(CID, StarterID, 'LINK', a, 10),
    {error, invalid_params} = luna_chat:get_medias(CID, a, [], 1, 10),
    {error, invalid_params} = luna_chat:get_medias(a, StarterID, <<"a">>, 1, 10),

    ok.

set_delivered(_) ->
    StarterID = ?ID,
    FollowerID = ?ID,
    {ok, new, #{cid := CID}} = luna_chat:add(StarterID, FollowerID),

    %% Add some messages
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),
    {ok, _, _, FollowerSeq, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, StarterSeq, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),

    %% Set delivered (normal)
    {ok, StarterMDA, FollowerID} = luna_chat:set_delivered(CID, StarterID, FollowerSeq),
    {ok, FollowerMDA, StarterID} = luna_chat:set_delivered(CID, FollowerID, StarterSeq),
    ?assert(is_tuple(ec_date:parse(binary_to_list(StarterMDA)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(FollowerMDA)))),

    %% Set delivered (error)
    {error, already_set} = luna_chat:set_delivered(CID, StarterID, FollowerSeq),
    {error, already_set} = luna_chat:set_delivered(CID, FollowerID, StarterSeq),
    {error, invalid_cid} = luna_chat:set_delivered(?ID, StarterID, StarterSeq),
    {error, invalid_uid} = luna_chat:set_delivered(CID, ?ID, StarterSeq),
    {error, invalid_seq} = luna_chat:set_delivered(CID, StarterID, StarterSeq + 1),
    {error, invalid_seq} = luna_chat:set_delivered(CID, FollowerID, StarterSeq + 1),
    {error, invalid_params} = luna_chat:set_delivered(a, StarterID, StarterSeq),
    {error, invalid_params} = luna_chat:set_delivered(CID, a, StarterSeq),
    {error, invalid_params} = luna_chat:set_delivered(CID, StarterID, a),

    ok.

set_seen(_) ->
    StarterID = ?ID,
    FollowerID = ?ID,
    {ok, new, #{cid := CID}} = luna_chat:add(StarterID, FollowerID),

    %% Add some messages
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),
    {ok, _, _, FollowerSeq1, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, StarterSeq1, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),
    {ok, _, _, FollowerSeq2, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, StarterSeq2, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),

    %% Set seen (normal)
    {ok, _, _} = luna_chat:set_delivered(CID, StarterID, FollowerSeq1),
    {ok, _, _} = luna_chat:set_delivered(CID, FollowerID, StarterSeq1),
    {ok, StarterMDA, FollowerID} = luna_chat:set_seen(CID, StarterID, FollowerSeq1),
    {ok, FollowerMDA, StarterID} = luna_chat:set_seen(CID, FollowerID, StarterSeq1),
    ?assert(is_tuple(ec_date:parse(binary_to_list(StarterMDA)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(FollowerMDA)))),

    %% Set seen (error)
    {error, invalid_seq} = luna_chat:set_seen(CID, StarterID, FollowerSeq2),
    {error, invalid_seq} = luna_chat:set_seen(CID, FollowerID, StarterSeq2),
    {error, already_set} = luna_chat:set_seen(CID, StarterID, FollowerSeq1),
    {error, already_set} = luna_chat:set_seen(CID, FollowerID, StarterSeq1),
    {error, invalid_cid} = luna_chat:set_seen(?ID, StarterID, StarterSeq2),
    {error, invalid_uid} = luna_chat:set_seen(CID, ?ID, StarterSeq2),
    {error, invalid_seq} = luna_chat:set_seen(CID, StarterID, StarterSeq2 + 1),
    {error, invalid_seq} = luna_chat:set_seen(CID, FollowerID, StarterSeq2 + 1),
    {error, invalid_params} = luna_chat:set_seen(a, StarterID, StarterSeq2),
    {error, invalid_params} = luna_chat:set_seen(CID, a, StarterSeq2),
    {error, invalid_params} = luna_chat:set_seen(CID, StarterID, a),

    ok.

set_message(_) ->
    StarterID = ?ID,
    FollowerID = ?ID,
    {ok, new, #{cid := CID}} = luna_chat:add(StarterID, FollowerID),

    %% Add some messages
    {ok, _, _, 2, _} = luna_chat:add_message( CID
					    , StarterID
					    , null
					    , <<"--CAPTION-1--">>
					    , #{ items => [ #{ type => 'FILE'
							     , body => <<"--BODY-1--">>
							     , mime => <<"--MIME-1--">>
							     , oid => <<"--OID-1--">>
							     }
							  , #{ type => 'LINK'
							     , body => <<"--BODY-1--">>
							     }
							  ] 
					       }
					    , #{<<"K1">> => <<"V1">>}
					    ),
    {ok, _, _, 3, _} = luna_chat:add_message( CID
					    , FollowerID
					    , null
					    , <<"--CAPTION-1--">>
					    , #{ items => [ #{ type => 'FILE'
							     , body => <<"--BODY-1--">>
							     , mime => <<"--MIME-1--">>
							     , oid => <<"--OID-1--">>
							     }
							  , #{ type => 'LINK'
							     , body => <<"--BODY-1--">>
							     }
							  ]
					       }
					    , #{<<"K1">> => <<"V1">>} 
					    ),

    %% Set (normal)
    {ok, MDA1, 1, 1, FollowerID} = luna_chat:set_message( CID
							, StarterID
							, 2
							, 0
							, <<"--CAPTION-2--">>
							, #{ items => [ #{ type => 'FILE'
									 , body => <<"--BODY-2--">>
									 , mime => <<"--MIME-2--">>
									 , oid => <<"--OID-2--">>
									 }
								      , #{ type => 'LINK'
									 , body => <<"--BODY-2--">>
									 }
								      ]
							   }
							, #{<<"K2">> => <<"V2">>} 
							),
    {ok, MDA2, 2, 1, StarterID} = luna_chat:set_message( CID
						       , FollowerID
						       , 3
						       , 0
						       , <<"--CAPTION-2--">>
						       , null
						       , #{<<"K2">> => <<"V2">>} 
						       ),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA1)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA2)))),

    { ok
    , [ #{ body := <<"--CAPTION-2--">>
	 , kivi := #{<<"K2">> := <<"V2">>}
	 , sequence := 2
	 , writer := 'STARTER'
	 , version := 1
	 , objects := #{ items := [ #{ <<"body">> := <<"--BODY-2--">>
				     , <<"mime">> := <<"--MIME-2--">>
				     , <<"oid">> := <<"--OID-2--">>
				     , <<"type">> := <<"FILE">>
				     }
				  , #{ <<"body">> := <<"--BODY-2--">>
				     , <<"type">> := <<"LINK">>
				     }
				  ]
		       }
	 }
      ]
    } = luna_chat:get_messages(CID, StarterID, 2, 1),
    { ok
    , [ #{ body := <<"--CAPTION-2--">>
	 , kivi := #{<<"K2">> := <<"V2">>}
	 , sequence := 3
	 , writer := 'FOLLOWER'
	 , version := 1
	 , objects := null
	 }
      ]
    } = luna_chat:get_messages(CID, FollowerID, 3, 1),

    {ok, [ #{ body := <<"--BODY-2--">>
	    }
	 ]
    } = luna_chat:get_medias(CID, StarterID, 'LINK', 1, 5),

    {ok, [ #{ body := <<"--BODY-2--">>
	    , mime := <<"--MIME-2--">>
	    , oid := <<"--OID-2--">>
	    }
	 ]
    } = luna_chat:get_medias(CID, StarterID, 'FILE', 1, 5),

    %% Set (error)
    {error, invalid_uid} = luna_chat:set_message(CID, ?ID, 3, 0, <<"--CAPTION--">>, null, null),
    {error, invalid_cid} = luna_chat:set_message(?ID, StarterID, 3, 0, <<"--CAPTION--">>, null, null),
    {error, invalid_seq} = luna_chat:set_message(CID, StarterID, 5, 0, <<"--CAPTION--">>, null, null),
    {error, invalid_seq} = luna_chat:set_message(CID, StarterID, -1, 0, <<"--CAPTION--">>, null, null),
    {error, invalid_role} = luna_chat:set_message(CID, StarterID, 3, 0, <<"--CAPTION--">>, null, null),
    {error, invalid_version} = luna_chat:set_message(CID, StarterID, 2, 0, <<"--CAPTION--">>, null, null),
    {error, invalid_body} = luna_chat:set_message(CID, StarterID, 2, 0, <<"">>, null, null),
    {error, invalid_objects} = luna_chat:set_message(CID, StarterID, 2, 0, <<"ANY">>, #{items => a}, #{}),
    {error, invalid_params} = luna_chat:set_message(CID, StarterID, 2, a, <<"ANY">>, null, #{}),
    {error, invalid_params} = luna_chat:set_message(CID, StarterID, a, 0, <<"ANY">>, null, null),
    {error, invalid_params} = luna_chat:set_message(CID, a, 2, 0, <<"ANY">>, null, null),
    {error, invalid_params} = luna_chat:set_message(a, StarterID, 2, 0, <<"ANY">>, null, null),

    ok.

del_message(_) ->
    StarterID = ?ID,
    FollowerID = ?ID,
    {ok, new, #{cid := CID}} = luna_chat:add(StarterID, FollowerID),

    %% Add some messages
    {ok, _, _, 2, _} = luna_chat:add_message( CID
					    , StarterID
					    , null
					    , <<"--CAPTION-1--">>
					    , #{ items => [ #{ type => 'FILE'
							     , body => <<"--BODY-1--">>
							     , mime => <<"--MIME-1--">>
							     , oid => <<"--OID-1--">>
							     }
							  , #{ type => 'LINK'
							     , body => <<"--BODY-1--">>
							     }
							  ] 
					       }
					    , #{<<"K1">> => <<"V1">>}
					    ),
    {ok, _, _, 3, _} = luna_chat:add_message( CID
					    , FollowerID
					    , null
					    , <<"--CAPTION-1--">>
					    , #{ items => [ #{ type => 'FILE'
							     , body => <<"--BODY-1--">>
							     , mime => <<"--MIME-1--">>
							     , oid => <<"--OID-1--">>
							     }
							  , #{ type => 'LINK'
							     , body => <<"--BODY-1--">>
							     }
							  ]
					       }
					    , #{<<"K1">> => <<"V1">>} 
					    ),

    {ok, _, _} = luna_chat:set_delivered(CID, StarterID, 3),
    {ok, _, _} = luna_chat:set_delivered(CID, FollowerID, 3),

    %% Del (normal)
    {ok, MDA1, 1, FollowerID} = luna_chat:del_message(CID, StarterID, 2, one),
    {ok, MDA2, 2, StarterID} = luna_chat:del_message(CID, FollowerID, 3, everyone),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA1)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA2)))),

    {ok, []} = luna_chat:get_messages(CID, StarterID, 2, 1),
    {ok, []} = luna_chat:get_messages(CID, FollowerID, 3, 1),

    {ok, []} = luna_chat:get_medias(CID, StarterID, 'LINK', 1, 5),

    {ok, []} = luna_chat:get_medias(CID, StarterID, 'FILE', 1, 5),

    %% Set (error)
    {error, invalid_uid} = luna_chat:del_message(CID, ?ID, 3, one),
    {error, invalid_uid} = luna_chat:del_message(CID, ?ID, 3, everyone),
    {error, invalid_cid} = luna_chat:del_message(?ID, StarterID, 3, one),
    {error, invalid_cid} = luna_chat:del_message(?ID, StarterID, 3, everyone),
    {error, invalid_seq} = luna_chat:del_message(CID, StarterID, 5, one),
    {error, invalid_seq} = luna_chat:del_message(CID, StarterID, 5, everyone),
    {error, invalid_seq} = luna_chat:del_message(CID, StarterID, -1, one),
    {error, invalid_seq} = luna_chat:del_message(CID, StarterID, -1, everyone),
    {error, invalid_role} = luna_chat:del_message(CID, StarterID, 3, everyone),
    {error, invalid_params} = luna_chat:del_message(CID, StarterID, 2, a),
    {error, invalid_params} = luna_chat:del_message(CID, StarterID, a, 0),
    {error, invalid_params} = luna_chat:del_message(CID, a, 2, 0),
    {error, invalid_params} = luna_chat:del_message(a, StarterID, 2, 0),

    ok.

set_message_action(_) ->
    StarterID = ?ID,
    FollowerID = ?ID,
    {ok, new, #{cid := CID}} = luna_chat:add(StarterID, FollowerID),

    %% Add some messages
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),
    {ok, _, _} = luna_chat:set_delivered(CID, StarterID, 5),
    {ok, _, _} = luna_chat:set_delivered(CID, FollowerID, 4),

    %% Set Action (normal)
    {ok, StarterMDA, 1, FollowerID} = luna_chat:set_message_action(CID, StarterID, 5, #{a => b}),
    {ok, FollowerMDA, 2, StarterID} = luna_chat:set_message_action(CID, FollowerID, 4, #{a => b}),
    ?assert(is_tuple(ec_date:parse(binary_to_list(StarterMDA)))),
    ?assert(is_tuple(ec_date:parse(binary_to_list(FollowerMDA)))),
    {ok, [#{actions := #{<<"a">> := <<"b">>}}]} = luna_chat:get_messages(CID, StarterID, 4, 1),
    {ok, [#{actions := #{<<"a">> := <<"b">>}}]} = luna_chat:get_messages(CID, StarterID, 5, 1),
    {ok, [#{actions := #{<<"a">> := <<"b">>}}]} = luna_chat:get_messages(CID, FollowerID, 4, 1),
    {ok, [#{actions := #{<<"a">> := <<"b">>}}]} = luna_chat:get_messages(CID, FollowerID, 5, 1),
    
    %% Set Action (error)
    {error, invalid_uid} = luna_chat:set_message_action(CID, ?ID, 5, #{a => b}),
    {error, invalid_cid} = luna_chat:set_message_action(?ID, StarterID, 5, #{a => b}),
    {error, invalid_seq} = luna_chat:set_message_action(CID, StarterID, 8, #{a => b}),
    {error, invalid_seq} = luna_chat:set_message_action(CID, FollowerID, -1, #{a => b}),
    {error, invalid_role} = luna_chat:set_message_action(CID, StarterID, 4, #{a => b}),
    {error, invalid_role} = luna_chat:set_message_action(CID, FollowerID, 3, #{a => b}),
    {error, invalid_params} = luna_chat:set_message_action(CID, StarterID, 5, []),
    {error, invalid_params} = luna_chat:set_message_action(CID, StarterID, a, #{a => b}),
    {error, invalid_params} = luna_chat:set_message_action(CID, a, 5, #{a => b}),
    {error, invalid_params} = luna_chat:set_message_action(a, StarterID, 5, #{a => b}),
    
    ok.

pin_message(_) ->
    StarterID = ?ID,
    FollowerID = ?ID,
    {ok, new, #{cid := CID}} = luna_chat:add(StarterID, FollowerID),

    %% Add some messages
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, S3, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, S5, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, StarterID, null, ?STR(16)),
    {ok, _, _, _, _} = luna_chat:add_message(CID, FollowerID, null, ?STR(16)),
    {ok, _, _} = luna_chat:set_delivered(CID, StarterID, 5),
    {ok, _, _} = luna_chat:set_delivered(CID, FollowerID, 4),

    %% Add pin message (normal)
    {ok, MDA1, 8, 1, FollowerID} = luna_chat:add_pin_message(CID, StarterID, #{items => [S3, S5]}),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA1)))),
    {ok, #{pinned_messages := #{<<"items">> := [S3, S5]}}} = luna_chat:get(CID, FollowerID),
    {ok, #{pinned_messages := #{<<"items">> := [S3, S5]}}} = luna_chat:get(CID, StarterID),
    
    %% Del pin message (normal) 
    {ok, MDA2, 9, 2, StarterID} = luna_chat:del_pin_message(CID, FollowerID),
    ?assert(is_tuple(ec_date:parse(binary_to_list(MDA2)))),
    {ok, MDA3, 10, 3, StarterID} = luna_chat:add_pin_message(CID, FollowerID, #{items => [2, 3]}),
    {ok, MDA4, 11, 4, FollowerID} = luna_chat:del_pin_message(CID, StarterID),
    {ok, #{pinned_messages := null}} = luna_chat:get(CID, FollowerID),
    {ok, #{pinned_messages := null}} = luna_chat:get(CID, StarterID),

    %% Add pin message (error)
    {error, invalid_uid} = luna_chat:add_pin_message(CID, ?ID, #{items => [S3, S5]}),
    {error, invalid_cid} = luna_chat:add_pin_message(?ID, StarterID, #{items => [S3, S5]}),
    {error, invalid_seq} = luna_chat:add_pin_message(CID, StarterID, #{items => [6, 7]}),
    {error, invalid_seq} = luna_chat:add_pin_message(CID, FollowerID, #{items => [6, 7]}),
    {error, invalid_params} = luna_chat:add_pin_message(CID, FollowerID, [3, 5]),
    {error, invalid_params} = luna_chat:add_pin_message(CID, a, #{items => [S3, S5]}),
    {error, invalid_params} = luna_chat:add_pin_message(a, StarterID, #{items => [S3, S5]}),

    %% Del pin message (error)
    {error, invalid_uid} = luna_chat:del_pin_message(CID, ?ID),
    {error, invalid_cid} = luna_chat:del_pin_message(?ID, StarterID),
    {error, invalid_params} = luna_chat:del_pin_message(CID, a),
    {error, invalid_params} = luna_chat:del_pin_message(a, StarterID),

    ok.
