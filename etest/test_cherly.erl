-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  {ok, C} = cherly:start(120),
  Value = <<"value">>,
  cherly:put(C, "key", Value),
  ?assertEqual(Value, cherly:get(C, "key")),
  ?assertEqual(16, cherly:size(C)),
  cherly:stop(C).
  
put_get_and_remove_test() ->
  {ok, C} = cherly:start(120),
  Value = <<"value">>,
  ?assertEqual(not_found, cherly:get(C, "key")),
  cherly:put(C, "key", Value),
  ?assertEqual(Value, cherly:get(C, "key")),
  cherly:remove(C, "key"),
  ?assertEqual(not_found, cherly:get(C, "key")),
  ?assertEqual(0, cherly:size(C)),
  cherly:stop(C).
  
put_with_lru_eject_test() ->
  {ok, C} = cherly:start(120),
  Value = <<"value">>,
  lists:foldl(fun(_, Str) ->
      Mod = succ(Str),
      cherly:put(C, Mod, Value),
      Mod
    end, "aaa", lists:seq(1, 10)),
  ?assertEqual(112, cherly:size(C)),
  ?assertEqual(7, cherly:items(C)),
  cherly:stop(C).
  

  
  
  
succ([]) ->
  [];
  
succ(Str) ->
  succ_int(lists:reverse(Str), []).
  
succ_int([Char|Str], Acc) ->
  if
    Char >= $z -> succ_int(Str, [$a|Acc]);
    true -> lists:reverse([Char+1|Acc] ++ Str)
  end.