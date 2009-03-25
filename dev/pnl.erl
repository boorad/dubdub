%%%-------------------------------------------------------------------
%%%
%%%  sum checks
%%%  sum checks by restaurant
%%%  sum checks between 01/01/08 and 01/31/08 by restaurant
%%%
%%%-------------------------------------------------------------------

-module(pnl).

-export([all/2, test/0]).

-include_lib("eunit/include/eunit.hrl").

all(Dbs, DupeLoads) ->
  utils:ensure_started(dubdub),
  node_manager:add_dbs(Dbs-1),
  load(DupeLoads),
  test(),
  dubdub:stop().

test() ->
  Map = fun(Pid, X) ->
            try
              %% filtering part (all checks, all stores)
              {_Key, {{time, _Time},
                      {store, _Store},
                      {data, Data}}} = X,
              %% send P&L line items (they're {K,V}) to reducer
              F = fun(LineItem) ->
                      Pid ! LineItem %%{Acct, Val}
                  end,
              lists:foreach(F, Data)
            catch
              _:_ -> badmatch
            end
        end,

  Reduce = fun({Key, Vals}, A) ->
               [{Key, utils:sum(Vals)} | A]
           end,

  {Time, Results} = timer:tc(node_manager, q, [tuple, Map, Reduce, []]),
  Msg = "Sum all checks: ~p~nTime (ms)     : ~p ms~n",
  io:format(Msg, [Results, Time/1000]).

%% internal functions
load(0) ->
  ok;
load(Count) ->
  testdata:load(),
  load(Count-1).
