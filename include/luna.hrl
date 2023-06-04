%% -*- mode:erlang -*-

-ifndef(HEADER_LUNA).
-define(HEADER_LUNA, true).

-define(DBE_EXCEPTION, 1).
-define(DBE_SAME_STARTER_AND_FOLLOWER, 3).
-define(DBI_CHAT_EXIST_ALREADY, 2).
-define(DBI_CHAT_NOT_EXIST_ALREADY, 4).
-define(DBI_CHAT_EXIST_BUT_BOTH_SIDES_HAD_BEEN_DELETED_IT, 5).
-define(DBI_CHAT_EXIST_BUT_STARTER_HAD_DELETED_IT, 6).
-define(DBI_CHAT_EXIST_BUT_FOLLOWER_HAD_DELETED_IT, 7).

-record( luna_chat
       , { cid :: non_neg_integer()
	 , cra :: luna_date()
	 , mda :: luna_date()
	 , starter_id :: non_neg_integer()
	 , follower_id :: non_neg_integer()
	 , last_message_sequence :: non_neg_integer()
	 , last_event_sequence :: non_neg_integer()
	 , pinned_messages :: luna_json()
	 , starter_start_sequence :: non_neg_integer()
	 , starter_delivered_sequence  :: non_neg_integer()
	 , starter_seen_sequence :: non_neg_integer()
	 , starter_is_muted :: boolean()
	 , starter_is_blocked :: boolean()
	 , starter_is_deleted :: boolean()
	 , starter_auto_delete :: null | non_neg_integer()
	 , follower_start_sequence :: non_neg_integer()
	 , follower_delivered_sequence :: non_neg_integer()
	 , follower_seen_sequence :: non_neg_integer()
	 , follower_is_muted :: boolean()
	 , follower_is_blocked :: boolean()
	 , follower_is_deleted :: boolean()
	 , follower_auto_delete :: null | non_neg_integer()
	 , kivi :: luna_json()
	 }
       ).
-type luna_chat() :: #luna_chat{}.

-type luna_json() :: map().
-type luna_date() :: non_neg_integer().

-ifdef(TEST).
-define(LOG_ERROR(Format, Args), ct:print(default, 50, Format, Args)).
-define(LOG_INFO(Format, Args), ?LOG_ERROR(Format, Args)).
-define(LOG_DEBUG(Format, Args), ?LOG_ERROR(Format, Args)).
-else.
-define(LOG_ERROR(Format, Args), lager:error(Format, Args)).
-define(LOG_INFO(Format, Args), lager:info(Format, Args)).
-define(LOG_DEBUG(Format, Args), lager:debug(Format, Args)).
-endif.

-endif.
