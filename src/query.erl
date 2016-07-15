-module(query).

-export([build/2]).

build(Query, Values) when is_list(Query) ->
  build(list_to_binary(Query), Values);

build(Query, [Value|Rest]) ->
  Query2 = replace(Query, Value),
  build(Query2, Rest);

build(Query, []) ->
  binary_to_list(Query).

replace(Query, Value) when is_list(Value) ->
  binary:replace(Query, <<"?">>,iolist_to_binary([list_to_binary(Value)]));
replace(Query, Value) when is_integer(Value) ->
  binary:replace(Query, <<"?">>, list_to_binary(integer_to_list(Value)));
replace(Query, Value) when is_float(Value) ->
  binary:replace(Query, <<"?">>, list_to_binary(float_to_list(Value, [ compact])));
replace(Query, Value) ->
  binary:replace(Query, <<"?">>, term_to_binary(Value)).
