-include_lib("eunit/include/eunit.hrl").

-export([succ/1, fast_acc/3, time_to_epoch_float/1]).

simple_test() ->
  {ok, C} = cherly:start(120),
  Value = <<"value">>,
  cherly:put(C, "key", Value),
  ?assertEqual({ok, Value}, cherly:get(C, "key")),
  ?assertEqual(24, cherly:size(C)),
  cherly:stop(C).
  
put_get_and_remove_test() ->
  {ok, C} = cherly:start(120),
  Value = <<"value">>,
  ?assertEqual(not_found, cherly:get(C, "key")),
  cherly:put(C, "key", Value),
  ?assertEqual({ok, Value}, cherly:get(C, "key")),
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
  ?assertEqual(120, cherly:size(C)),
  ?assertEqual(5, cherly:items(C)),
  cherly:stop(C).
  
what_goes_in_must_come_out_test() ->
  {ok, C} = cherly:start(120),
  cherly:put(C, "key", [<<"val1">>, <<"val2">>]),
  ?assertEqual({ok, [<<"val1">>, <<"val2">>]}, cherly:get(C, "key")),
  cherly:stop(C).
  
big_stuff_that_goes_in_must_come_out_test() ->
  {ok, C} = cherly:start(1048576),
  V1 = <<0:524288>>,
  V2 = <<1:524288>>,
  cherly:put(C, "key", [V1, V2]),
  Ret = cherly:get(C, "key"),
  ?assertEqual({ok, [V1,V2]}, Ret),
  cherly:stop(C).
  
remove_nonexistant_test() ->
  {ok, C} = cherly:start(120),
  cherly:remove(C, "key"),
  ?assertEqual(not_found, cherly:get(C, "key")),
  cherly:stop(C).

succ([]) ->
  [];

succ(Str) ->
  succ_int(lists:reverse(Str), []).

succ_int([Char|Str], Acc) ->
  if
    Char >= $z -> succ_int(Str, [$a|Acc]);
    true -> lists:reverse(lists:reverse([Char+1|Acc]) ++ Str)
  end.

fast_acc(_, Acc, 0) -> Acc;

fast_acc(Fun, Acc, N) ->
  fast_acc(Fun, Fun(Acc), N-1).

time_to_epoch_float({Mega,Sec,Micro}) ->
  Mega * 1000000 + Sec + Micro / 1000000.