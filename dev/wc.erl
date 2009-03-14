-module(wc).

-export([all/2, load/1, test/0]).

-include_lib("eunit/include/eunit.hrl").

-define(PATH, "../dev/data/").
-define(BOOK, "moby_dick.txt").


all(Dbs, Books) ->
  utils:ensure_started(dubdub),
  node_manager:add_dbs(Dbs-1),
  load(Books),
  test(),
  dubdub:stop().


%% load a certain number of books into dubdub
load(0) ->
  ok;
load(Count) ->
  Filename = ?PATH ++ ?BOOK,
  %% io:format("loading ~p~n", [Filename]),
  case file:read_file(Filename) of
    {ok, Chapter} ->
      Lines = string:tokens(binary_to_list(Chapter), "\r\n"),
      LineFun = fun(Line) ->
                    Words = string:tokens(Line, "\n-\t().!?,;:\" "),
                    Words1 = lists:map(fun(Word) ->
                                           list_to_binary(Word)
                                       end, Words),
                    dataloader:insert({?BOOK, Count, Words1})
                end,
      lists:foreach(LineFun, Lines);
    {error, Reason} ->
      io:format("barf: ~p~n", [Reason])
  end,
  load(Count-1).


test() ->
  Map = fun(Pid, X) ->
            {_Key, {_Book, _Num, Line}} = X,
            lists:foreach(fun(Word) -> Pid ! {Word, 1} end, Line)
        end,

  Reduce = fun({Key, Vals}, Acc) ->
               [{Key, utils:sum(Vals)} | Acc]
           end,

  {Time, Results} = timer:tc(node_manager, q, [tuple, Map, Reduce, []]),
  Msg = "word count: ~p~nTime (ms)  : ~p ms~n",
%%  io:format(Msg, [Results, Time/1000]).
  io:format(Msg, [length(Results), Time/1000]).
