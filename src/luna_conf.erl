-module(luna_conf).

-export([ get/1
	, get/2
	, set/2
	, load/1
	, get_root/0
	, get_path/0
	, fix_test_profile/0
	]).

-include("luna.hrl").

-spec get(atom()) -> {ok, any()} | {error, undefined}.
get(Key) ->
    case application:get_env(luna, Key) of
	{ok, Value} ->
	    {ok, Value};
	_ ->
	    {error, undefined}
    end.

get(Key, List) ->
    case lists:keyfind(Key, 1, List) of
        false ->
            undefined;
        {Key, Value} ->
            {ok, Value}
    end.


-spec set(atom(), any()) -> ok.
set(Key, Value) ->
    application:set_env(luna, Key, Value).

-spec load(string()) -> ok.
load(Path) ->
    {ok, [ConfigList]} = file:consult(Path),
    _ = [application:set_env(Application, Key, Val)
         || {Application, Items} <- ConfigList,
            {Key, Val} <- Items],
    ok.

-spec get_path() -> string().
get_path() ->
    code:lib_dir(luna).

-spec get_root() -> string().
get_root() ->
    get_path() ++ "/../../../../".

%% @NOTE: Rebar3 ct command doesn't have any
%% understanding about config/test.sys.config
%% so we have to load it manually
-spec fix_test_profile() -> ok.
-ifdef(TEST).
fix_test_profile() ->
    Path = get_root() ++ "/config/test.sys.config",
    ok = load(Path),
    ok.
-else.
fix_test_profile() ->
    ok.
-endif.
