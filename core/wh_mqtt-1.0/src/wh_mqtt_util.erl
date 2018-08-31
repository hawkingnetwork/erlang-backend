-module(wh_mqtt_util).
-export([to_bin/1]).

to_bin(Val) when is_list(Val) ->
  list_to_binary(Val) ;
to_bin(Val) when is_atom(Val)  ->
  atom_to_binary(Val, utf8);
to_bin(Val) when is_integer(Val) ->
  list_to_binary(integer_to_list(Val));
to_bin(Val) when is_float(Val) ->
  float_to_binary(Val, [{decimals, 4}, compact]);
to_bin(Val) ->
  Val.