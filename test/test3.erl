-module(test3).

-export([test/0]).

-import(db).

%% let's test dicts as our key structure
test() ->
  io:format("Test 3 - dict~n"),
  db:start_link(),

  %% create some test data
  Time1 = dict:from_list([{year, 2008}, {month, 1}, {day, 1}]),
  Loc1 = dict:from_list([{storenum, 1}, {storename, "Joe's Plumbing Store 1"}]),
  Product1 = dict:store(name, "faucet repair", dict:new()),
  K1 = dict:from_list([{time, Time1}, {loc, Loc1}, {product, Product1}]),
  V1 = {val,
	{sales, 100.00},
	{hours, 2.0},
	{tax, 8.00},
	{total, 108.00}
       },
  db:insert(K1,V1),

  Time2 = dict:from_list([{year, 2008}, {month, 1}, {day, 2}]),
  Loc2 = dict:from_list([{storenum, 1}, {storename, "Joe's Plumbing Store 1"}]),
  Product2 = dict:store(name, "faucet repair", dict:new()),
  K2 = dict:from_list([{time, Time2}, {loc, Loc2}, {product, Product2}]),
  V2 = {val,
	{sales, 200.00},
	{hours, 4.0},
	{tax, 16.00},
	{total, 216.00}
       },
  db:insert(K2,V2),


  All1 = db:get_all(),
  io:format("All before query   : ~p~n", [All1]),

  %% test a query
  %%  This Filter fun searches the Key, which is a dict, looking for time as a
  %%  key in the dict.  If it's present, we return the value
  Filter = fun(K, V) ->
	       case dict:find(time, K) of
		 error -> error;
		 _ -> V
	       end
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
