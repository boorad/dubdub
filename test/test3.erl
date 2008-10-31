-module(test3).

-export([test/0]).

-import(db).

%% let's test dicts as our key structure
test() ->
  io:format("Test 3~n"),
  db:start_link(),

  %% create some test data
  K1 = {key,
	{time,
	 {year, 2008},
	 {month, 1},
	 {day, 1}},
	{loc,
	 {storenum, 1},
	 {storename, "Joe's Plumbing Store 1"},
	 {market,
	  {num, 1},
	  {name, "Atlanta"}}},
	{product,
	 {name, "faucet repair"}}
       },
  V1 = {val,
	{sales, 100.00},
	{hours, 2.0},
	{tax, 8.00},
	{total, 108.00}
       },
  db:insert(K1,V1),


  All1 = db:get_all(),
  io:format("All before query   : ~p~n", [All1]),

  %% test a query
  Filter = fun(_Records) ->
	       not_implemented_yet
	   end,
  Reduce = fun(_Records) ->
	       not_implemented_yet
	   end,
  Results = db:q(dict, Filter, Reduce),
  io:format("Query Results      : ~p~n", [Results]),

  %% now zap all, and print the results of get_all()
  db:truncate(),
  All2 = db:get_all(),
  io:format("All after truncate : ~p~n", [All2]),
  ok.
