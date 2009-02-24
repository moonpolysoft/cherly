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

-export([start/1]).

start(Size) ->
  case load_driver() of
    ok -> 
      P = open_port({spawn, 'cherly_drv'}, [binary]),
      port_command(P, [$i, term_to_binary(0), term_to_binary(Size)]),
      {ok, {cherly, P}};
    {error, Err} ->
      Msg = erl_ddll:format_error(Err),
      {error, Msg}
  end.
  
%%====================================================================
%% Internal functions
%%====================================================================
  
load_driver() ->
  Dir = filename:join([filename:dirname(code:which(cherly)), "..", "priv"]),
  erl_ddll:load(Dir, "cherly_drv").