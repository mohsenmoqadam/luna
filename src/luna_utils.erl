-module(luna_utils).

-export([ json/1
	, date/1
	]
       ).

json(Input) when is_bitstring(Input) ->
    jiffy:decode(Input, [return_maps]);
json(Input) when is_map(Input) ->
    jiffy:encode(Input).

date(ErlangDate) when is_tuple(ErlangDate) ->
    calendar:datetime_to_gregorian_seconds(ErlangDate) - 62167219200;
date(DateAsBitstring) when is_binary(DateAsBitstring) ->
    ErlangDate = ec_date:parse(binary_to_list(DateAsBitstring)),
    calendar:datetime_to_gregorian_seconds(ErlangDate) - 62167219200.
    
