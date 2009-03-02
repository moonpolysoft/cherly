-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  {ok, C} = cherly:start(120),
  Value = <<"value">>,
  cherly:put(C, "key", Value),
  ?assertEqual(Value, cherly:get(C, "key")),
  ?assertEqual(16, cherly:size(C)).