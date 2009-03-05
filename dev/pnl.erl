%%%-------------------------------------------------------------------
%%%
%%%  sum checks
%%%  sum checks by restaurant
%%%  sum checks between 01/01/08 and 01/31/08 by restaurant
%%%
%%%-------------------------------------------------------------------

-module(pnl).

-export([all/0, test/0]).

-include_lib("eunit/include/eunit.hrl").

all() ->
  application:start(dubdub),
  testdata:load(),
  test().

test() ->
  Map = fun(Pid, X) ->
	    try
	      %% filtering part (all checks, all stores)
	      {_Key, {{time, _Time},
		      {store, _Store},
		      {data, Data}}} = X,
	      %%?debugFmt("data: ~p~n", [Data]),
	      %% send P&L line items (they're {K,V}) to reducer
	      F = fun(LineItem) ->
		      %%{Acct, Val} = LineItem,
		      %%?debugFmt("~p~n", [Val]),
		      Pid ! LineItem %%{Acct, Val}
		  end,
	      lists:foreach(F, Data)
	    catch
	      _:_ -> badmatch
	    end
	end,

  Reduce = fun(Key, Vals, A) ->
	       [{Key, sum(Vals)} | A]
	   end,

  {Time, [Results]} = timer:tc(node_manager, q, [tuple, Map, Reduce, []]),
  %%Results = node_manager:q(tuple, Map, Reduce, []),
  Msg = "Summing all checks in ~p ms ~n~p~n",
  io:format(Msg, [Time/1000, Results]).

sum([H|T]) ->
  H + sum(T);
sum([]) ->
  0.
