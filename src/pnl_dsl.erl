%%%-------------------------------------------------------------------
%%%
%%%  sum checks
%%%  sum checks by restaurant
%%%  sum checks between 01/01/08 and 01/31/08 by restaurant
%%%
%%%-------------------------------------------------------------------

-module(pnl_dsl).

-export([start/0]).

start() ->
  spawn(fun() ->
	    loop()
	end).

loop() ->
  io:format("entering loop...~n"),
  receive
    {sum, checks} ->
      io:format("received {sum, checks}~n"),
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
		   io:format("in reduce...~n"),
		   [{Key, sum(Vals)} | A]
	       end,
      Results = node_manager:q(tuple, Map, Reduce, []),
      Msg = "Summing all checks ~p~n",
      io:format(Msg, [Results]),
      loop();

    _ ->
      ok
  end.


sum([H|T]) ->
  H + sum(T);
sum([]) ->
  0.
