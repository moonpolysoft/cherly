-include_lib("eunit/include/eunit.hrl").

simple_test() ->
  {ok, C} = cherly:start(120),
  Value = <<"value">>,
  cherly:put(C, "key", Value),
  ?assertEqual(Value, cherly:get(C, "key")),
  ?assertEqual(16, cherly:size(C)),
  ?debugHere,
  cherly:stop(C).
  
put_get_and_remove_test() ->
  ?debugHere,
  {ok, C} = cherly:start(120),
  Value = <<"value">>,
  ?debugHere,
  timer:sleep(10),
  ?assertEqual(not_found, cherly:get(C, "key")),
  cherly:put(C, "key", Value),
  ?assertEqual(Value, cherly:get(C, "key")),
  cherly:remove(C, "key"),
  ?assertEqual(not_found, cherly:get(C, "key")),
  ?assertEqual(0, cherly:size(0)),
  cherly:stop(C).
  
  
  
  
  
  % lists:foldl(fun(_, Str) ->
  %     
  %   end, "aaa", lists:seq())
  % cherly:put(C, "key", Value).
  
  
  
  
  
succ([]) ->
  [];
  
succ(Str) ->
  succ_int(lists:reverse(Str), []).
  
succ_int([Char|Str], Acc) ->
  if
    Char >= $z -> succ_int(Str, [$a|Acc]);
    true -> lists:reverse([Char+1|Acc] ++ Str)
  end.