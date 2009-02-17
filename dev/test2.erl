-module(test2).

-export([test/0]).

%% let's test tuples as our doc structure
test() ->
  io:format("Test 2 - Tuples~n"),
  application:start(dubdub),
  testdata:load(),

  %% test a query

  %% hack to test on 'one node' for our tests, usually the local one.
  %% TODO: do we need a fun to get the local node for convenience?
  [Node | _T] = node_manager:get_all_nodes(),

  %% This sucks.  Why do I have to know the structure of terms?
  %% Asked a Q in erlang-questions on 10/31/08 to see if there are better
  %% wildcards to use.
  MatchSpec = [{
                {'_',{{time,{{year,'$1'},{month,'$2'}}},
		      '_',
		      '$3'
		     }},
		[{'and', {'=:=', 2008, '$1'}, {'=:=', 12, '$2'}}],
		['$3']
	       }],
  Reduce = fun(_Records) ->
	       not_implemented_yet
	   end,
  [{ok, Results}] = db_manager:q(Node, match_spec, MatchSpec, Reduce),
  %% io:format("Query Results     : ~p~n", [Results]),
  io:format("Query Results Len : ~p~n", [length(Results)]),


  application:stop(dubdub).
