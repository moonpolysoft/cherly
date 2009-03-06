%%%-------------------------------------------------------------------
%%% File:      cherly.erl
%%% @author    Cliff Moon <cliff@moonpolysoft.com> []
%%% @copyright 2009 Cliff Moon See LICENSE file
%%% @doc  
%%%
%%% @end  
%%%
%%% @since 2009-02-22 by Cliff Moon
%%%-------------------------------------------------------------------
-module(cherly).
-author('cliff@moonpolysoft.com').

-export([start/1, put/3, get/2, remove/2, size/1, items/1, stop/1]).

-define(INIT, $i).
-define(GET, $g).
-define(PUT, $p).
-define(REMOVE, $r).
-define(SIZE, $s).
-define(ITEMS, $t).

-ifdef(TEST).
-include("test_cherly.erl").
-endif.

%% api bitches

start(Size) ->
  case load_driver() of
    ok -> 
      P = open_port({spawn, 'cherly_drv'}, [binary]),
      port_command(P, [?INIT, term_to_binary({0, Size})]),
      {ok, {cherly, P}};
    {error, Err} ->
      Msg = erl_ddll:format_error(Err),
      {error, Msg}
  end.
  
put({cherly, P}, Key, Value) ->
  Len = length(Key),
  port_command(P, [?PUT, <<Len:32>>, Key, Value]).
  
get({cherly, P}, Key) ->
  Len = length(Key),
  port_command(P, [?GET, <<Len:32>>, Key]),
  receive
    {P, {data, Bin}} -> Bin;
    So -> So
  end.
  
remove({cherly, P}, Key) ->
  Len = length(Key),
  port_command(P, [?REMOVE, <<Len:32>>, Key]).
  
size({cherly, P}) ->
  port_command(P, [?SIZE]),
  receive
    {P, {data, Bin}} -> binary_to_term(Bin)
  end.
  
items({cherly, P}) ->
  port_command(P, [?ITEMS]),
  receive
    {P, {data, Bin}} -> binary_to_term(Bin)
  end.
  
stop({cherly, P}) ->
  unlink(P),
  port_close(P).
%%====================================================================
%% Internal functions
%%====================================================================
  
load_driver() ->
  Dir = filename:join([filename:dirname(code:which(cherly)), "..", "priv"]),
  erl_ddll:load(Dir, "cherly_drv").
  