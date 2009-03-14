-module(wc).

-export([all/1, load/2, test/0]).

-include_lib("eunit/include/eunit.hrl").

-define(PATH, "../dev/data/").


all(Dbs) ->
  utils:ensure_started(dubdub),
  node_manager:add_dbs(Dbs-1),
  load(moby, 10),
  test().


%% load a certain number of books into dubdub
load(_Book, 0) ->
  ok;
load(Book, Count) ->
  Book1 = case is_atom(Book) of
            true ->
              atom_to_list(Book);
            _ ->
              Book
          end,
  {ok, Files} = file:list_dir(?PATH ++ Book1),
  Fun = fun(File) ->
            Filename = ?PATH ++ Book1 ++ "/" ++ File,
            %% io:format("loading ~p~n", [Filename]),
            case file:read_file(Filename) of
              {ok, Chapter} ->
                Lines = string:tokens(binary_to_list(Chapter), "\n"),
                LineFun = fun(Line) ->
                              Words = string:tokens(Line, "\n-\t().!?,;:\" "),
                              dataloader:insert({Book1, Count, Words})
                          end,
                lists:foreach(LineFun, Lines);
              {error, Reason} ->
                io:format("barf: ~p~n", [Reason])
            end
        end,
  lists:foreach(Fun, Files),
  load(Book, Count-1).


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
