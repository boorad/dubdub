-module(wc).

-export([moby/0, warpeace/0, test/0]).

-define(PATH, "../dev/data/moby/").

moby() ->
  {ok, Files} = file:list_dir(?PATH),
  Fun = fun(File) ->
            case file:read_file(?PATH ++ File) of
              {ok, Bin} ->
                dataloader:insert(Bin);
                %%io:format("~p~n~n", [Bin]);
              {error, Reason} ->
                io:format("barf: ~p~n", [Reason])
            end
        end,
  lists:foreach(Fun, Files).


warpeace() ->
  ok.


test() ->
  ok.
