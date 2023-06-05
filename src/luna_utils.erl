-module(luna_utils).

-export([ json/1
	, date/1
	, bool/1
	]
       ).

-include("luna.hrl").

json(Input) when is_bitstring(Input) ->
    jiffy:decode(Input, [return_maps]);
json(Input) when is_map(Input) ->
    jiffy:encode(Input);
json(Input) when is_list(Input) ->
    jiffy:encode(Input).

date({{Y, MO, D}, {H, MI, S0}}) when is_float(S0) ->
    date({{Y, MO, D}, {H, MI, trunc(S0), ceil(S0 * 1000000) rem 1000000}});
date({{Y, MO, D}, {H, MI, S0}}) when is_integer(S0) ->
    date({{Y, MO, D}, {H, MI, S0, 0}});
date({{_, _, _}, {_, _, _, _}} = ErlangDate) ->
    list_to_binary(ec_date:format_iso8601(ErlangDate));
date(DateAsBitstring) when is_binary(DateAsBitstring) ->
    DateAsBitstring.

bool(0) -> false;
bool(1) -> true; 
bool(false) -> 0; 
bool(true) -> 1.
