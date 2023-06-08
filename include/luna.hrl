%% -*- mode:erlang -*-

-ifndef(HEADER_LUNA).
-define(HEADER_LUNA, true).

-define(GREGORIAN_SECONDS_1970, 62167219200).

-define(DBE_EXCEPTION, 1).
-define(DBE_INVALID_UID, 2).
-define(DBE_INVALID_CID, 3).
-define(DBE_INVALID_SEQ, 4).
-define(DBE_ALREADY_SET, 5).
-define(DBE_CHAT_SAME_ID_AS_SIDES, 6).
-define(DBI_CHAT_ALREADY_EXIST, 7).
-define(DBI_CHAT_ALREADY_EXIST_BOTH_SIDES_HAD_BEEN_DELETED, 8).
-define(DBI_CHAT_ALREADY_EXIST_STARTER_HAD_BEEN_DELETED, 9).
-define(DBI_CHAT_ALREADY_EXIST_FOLLOWER_HAD_BEEN_DELETED, 10).
-define(DBI_CHAT_NEW, 11).

-record( luna_chat_state
       , { chat_meta :: luna_chat_meta()
	 }
       ).
-type luna_chat_state() :: #luna_chat_state{}.

-record( luna_chat_meta
       , { cid :: non_neg_integer()
	 , cra :: luna_date()
	 , mda :: null | luna_date()
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
-type luna_chat_meta() :: #luna_chat_meta{}.

-record( luna_chat_message
       , { cra :: luna_date()
	 , mda :: null | luna_date()
	 , type :: luna_chat_message_type()
	 , sequence :: non_neg_integer()
	 , reply_sequence :: null | non_neg_integer()
	 , writer :: luna_chat_message_writer()
	 , body :: binary()
	 , objects :: luna_json()
	 , actions :: luna_json()
	 , is_deleted_by_starter :: boolean()
	 , is_deleted_by_follower :: boolean()
	 , auto_delete :: null | non_neg_integer()
	 , kivi :: luna_json()
	 , version :: non_neg_integer()
	 }
       ).
-type luna_chat_message() :: #luna_chat_message{}.

-record( luna_chat_object
       , { cra :: luna_date()
	 , type :: luna_chat_object_type()
	 , sequence :: non_neg_integer()
	 , mime :: null | binary()
	 , body :: null | binary()
	 , oid :: null | binary()
	 }
       ).

-type luna_json() :: map().
-type luna_date() :: non_neg_integer().
-type luna_chat_message_type() :: 'MESSAGE' | 'CREATE' | 'DELETE' | 'MUTE' | 'UNMUTE' | 'BLOCK' |  'UNBLOCK'.
-type luna_chat_message_writer() :: 'STARTER' | 'FOLLOWER'.
-type luna_chat_object_type() :: 'FILE' | 'LINK'.

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
