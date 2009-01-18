-module(test2).

-export([test/0]).

-import(db).

%% let's test tuples as our doc structure
test() ->
  io:format("Test 2 - Tuples~n"),
  %% db:start_link(),

  %% test a query

  %% This sucks.  Why do I have to know the structure of terms?
  %% Asked a Q in erlang-questions on 10/31/08 to see if there are better
  %% wildcards to use.
  MatchSpec = [{{null,
		 {{time,
		   {year,'$1'},
		   {month,'$2'},
		   '_'
		  },
		  '_',
		  '$3'
		 }},
		[{'and', {'=:=', 2008, '$1'}, {'=:=', 1, '$2'}}],
		['$3']
	       }],
  Reduce = fun(_Records) ->
	       not_implemented_yet
	   end,
  Results = db:q(match_spec, MatchSpec, Reduce),
  io:format("Query Results      : ~p~n", [length(Results)]),
  ok.
