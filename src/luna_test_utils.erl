-module(luna_test_utils).

-export([ get_rand_int/0
	, get_rand_string/1
	]).

-include("luna.hrl").

get_rand_int() ->
    ran_int(999999, 999999999).

get_rand_string(Len) ->
    DefaultAllowedChars =
        "123456789"
        "abcdefghijklmnopqrstuvwxyz",
    list_to_binary(get_rand_str(Len, DefaultAllowedChars)).
    
get_rand_str(CharSize, AllowedChars) ->
    [begin
         Picked = ran_int(1, length(AllowedChars)),
         lists:nth(Picked, AllowedChars)
     end || _ <- lists:seq(1, CharSize)].

ran_int(From, To) when From < To ->
    rand:uniform(To - From + 1) - 1 + From;
ran_int(_, _) ->
    invalid_params.

    
