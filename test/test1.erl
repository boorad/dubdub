-module(test1).

-export([test/0]).

-import(db).

test() ->
  io:format("Test 1~n"),
  db:start_link(),
  db:insert("Key1","Val1"),
  db:insert("Key2","Val2"),
  db:insert("Key3","Val3"),

  Filter = fun(Records) ->
	       {K,_V} = Records,
	       case K of
		 "Key2" -> true;
		 _ -> false
	       end
	   end,
  Reduce = fun(_Records) ->
	       not_implemented_yet
	   end,

  %% test a query
  Results = db:q(Filter, Reduce),
  io:format("Query Results      : ~p~n", [Results]),

  %% now zap all, and print the results of get_all()
  db:truncate(),
  All = db:get_all(),
  io:format("All after truncate : ~p~n", [All]),
  ok.
