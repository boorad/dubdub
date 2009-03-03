%%%-------------------------------------------------------------------
%%%
%%%  sum checks
%%%  sum checks by restaurant
%%%  sum checks between 01/01/08 and 01/31/08 by restaurant
%%%
%%%-------------------------------------------------------------------

-module(pnl_dsl).
-export([broker/0]).
broker() ->
  receive
    {sum, checks} ->
      Map = fun(Pid, X) ->
		 try
		   %% filtering part (all checks, all stores)
		   {_Key, {{time, _Time},
			   {store,_Store},
			   {data, Data}}} = X,
		   %% send P&L line items (they're {K,V}) to reducer
		   F = fun(LineItem) ->
			   {_Acct, Val} = LineItem,
			     Pid ! {all_checks, Val}
		       end,
		   lists:foreach(F, Data)
		 catch
		   _:_ -> badmatch
		 end
	     end,

      Reduce = fun(Key, Vals, A) ->
		   [{Key, sum(Vals)} | A]
	       end,
      Results = node_manager:q(tuple, Map, Reduce, []),
      Msg = "Summing all checks ~p~n",
      io:format(Msg, [Results]),
      broker()
  end.


sum([H|T]) ->
  H + sum(T);
sum([]) ->
  0.
