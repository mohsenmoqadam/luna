-module(luna_test_utils).

-export([ get_rand_int/0
	, get_rand_string/1
	]).

-include("luna.hrl").

get_rand_int() ->
    crypto:rand_uniform(999999, 999999999).

get_rand_string(Len) ->
    DefaultAllowedChars =
        "123456789"
        "abcdefghijklmnopqrstuvwxyz",
    list_to_binary(get_rand_str(Len, DefaultAllowedChars)).
    
get_rand_str(CharSize, AllowedChars) ->
    [begin
         Picked = crypto:rand_uniform(1, length(AllowedChars)),
         lists:nth(Picked, AllowedChars)
     end || _ <- lists:seq(1, CharSize)].

