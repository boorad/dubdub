-module(test2).

-export([start/0, test/0, stop/0]).

start() ->
  application:start(dubdub),
  testdata:load().


%% let's test tuples as our doc structure
test() ->
  io:format("Test 2 - Tuples~n"),

  %% test a query

%% (boot@boorad)8> db:get_docs_limit(Db, 1).
%% {ok,[{null,{{time,{{year,2008},{month,1}}},
%%             {store,[{"store_num",9340},
%%                     {"city",<<"Kettering">>},
%%                     {"state",<<"OH">>},
%%                     {"zip",<<"45440">>},
%%                     {"exterior",true},
%%                     {"interior",true},
%%                     {"restroom",true},
%%                     {"breakfast",true},
%%                     {"sunday",true},
%%                     {"adi_name",<<"Dayton, OH">>},
%%                     {"adi_num",200},
%%                     {"ownership",<<"Company">>},
%%                     {"playground",<<"Yes">>},
%%                     {"seats",94},
%%                     {"parking_spaces",67}]},
%%             {data,[{"COS Burger Beef",382.26},
%%                    {"Other Hourly Pay",0.38},
%%                    {"Workers Comp - State Funds Exp",367.8},
%%                    {"Retiree Health Ins#",-152.34},
%%                    {"Rent Expense - Company",211.0},
%%                    {"Revised Hours allowed per",4.01},
%%                    {"Merch# Standard",72.42},
%%                    {"Total Property Tax",532.7},
%%                    {"Food Standard",39.59},
%%                    {"Food Cost",-21.96},
%%                    {"Storage and Handling",70.64},
%%                    {"COS Paper Other",899.57},
%%                    {"R & M Equipment - In House Maintenance",69.2},
%%                    {"Inc - Clearing",98.1},
%%                    {"Recruitment backgr",202.7},
%%                    {"Others",-2669.79},
%%                    {"COS Kids Club",231.84},
%%                    {[...],...},
%%                    {...},...]}}}]}


  FMap = fun(Pid, X) ->
	     try
	       %% filtering part (basically Jan. 2008, all stores)
	       {_Key, {{time, {{year,2008},{month,1}}},
		       {store,_Store},
		       {data, Data}}} = X,
	       %% send P&L line items (they're {K,V}) to reducer
	       F = fun(LineItem) ->
		       Pid ! LineItem
		   end,
	       lists:foreach(F, Data)
	     catch
	       _:_ -> badmatch
	     end
	 end,

  FReduce = fun(Key, Vals, A) ->
		[{Key, sum(Vals)} | A]
	   end,

  {Time, Results} = timer:tc(node_manager, q, [tuple, FMap, FReduce, []]),

  io:format("Query Results : ~p~n", [Results]),
  io:format("Time (ms)     : ~p~n", [Time/1000]),
%%   io:format("Query Results Len : ~p~n", [length(Results)]),
  ok.

stop() ->
  application:stop(dubdub).


%% TODO: prolly needs to go into utils at some point,
%%       maybe an aggregation module w/ lots of methods like this
sum([H|T]) ->
  H + sum(T);
sum([]) ->
  0.
