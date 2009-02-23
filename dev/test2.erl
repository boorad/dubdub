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

  %% hack to test on 'one node' for our tests, usually the local one.
  %% TODO: do we need a fun to get the local node for convenience?
  [Node | _T] = node_manager:get_all_nodes(),

  FMap = fun(Pid, X) ->
	     %% filtering part (basically Jan. 2008, all stores)
	     %% screw match_spec for this.  pattern matching should work ok
	     %%  although, what about a badmatch?  trap it?
	     {_Key, {{time, {{year,Year},{month,Month}}},
	      {store,_Store},
	      {data, Data}}} = X,
	     %% send P&L line items to reducer
	     case (Year =:= 2008 andalso Month =:= 11) of
	       true ->
		 F = fun(LineItem) ->
			 Pid ! LineItem
		     end,
		 lists:foreach(F, Data);
	       _ ->
		 nomatch
	     end
	 end,

  FReduce = fun(Key, Vals, A) ->
		[{Key, sum(Vals)} | A]
	   end,

  {Time, Return} = timer:tc(db_manager, q, [Node, tuple, FMap, FReduce]),
  [{ok, Results}] = Return, %% db_manager:q(Node, tuple, FMap, FReduce),

  io:format("Query Results     : ~p~n", [Results]),
  io:format("Time (us): ~p~n", [Time]),
%%   io:format("Query Results Len : ~p~n", [length(Results)]),
  ok.

stop() ->
  application:stop(dubdub).


%% TODO: prolly needs to go into utils at some point
sum([H|T]) ->
  H + sum(T);
sum([]) ->
  0.
