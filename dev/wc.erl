-module(wc).

-export([all/0, load/2, test/0]).

-define(PATH, "../dev/data/").


all() ->
  utils:ensure_started(dubdub),
  load(moby, 20),
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

            %% this streams better?
            %% F = fun(Bin, _Args) ->
            %%         dataloader:insert({Book1, Count, Bin})
            %%     end,
            %% klib:with_file(Filename, F, [])

            %% this is easier API-wise
            case file:read_file(Filename) of
              {ok, Bin} ->
                dataloader:insert({Book1, Count, Bin});
              {error, Reason} ->
                io:format("barf: ~p~n", [Reason])
            end
        end,
  lists:foreach(Fun, Files),
  load(Book, Count-1).


test() ->
  Map = fun(Pid, X) ->
            {_key, {_Book, _Num, Chapter}} = X,
%%             Lines = string:tokens(binary_to_list(Chapter), "\n"),
%%             WordFun = fun(Line) ->
            Words = string:tokens(binary_to_list(Chapter), "\n-\t().!?,;:\" "),
%%                          Words = string:tokens(Line, "-\t "),
            WordFun = fun(Word) ->
                          Pid ! {Word, 1}
                      end,
            lists:foreach(WordFun, Words)
%%                       end,
%%             lists:foreach(LineFun, Lines)
        end,
  Reduce = fun(Key, Vals, A) ->
               [{Key, utils:sum(Vals)} | A]
           end,

  {Time, Results} = timer:tc(node_manager, q, [tuple, Map, Reduce, []]),
  Msg = "word count: ~p~nTime (ms)     : ~p ms~n",
  io:format(Msg, [Results, Time/1000]).
