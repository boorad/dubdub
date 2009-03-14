-module(phofs_test).

-include_lib("eunit/include/eunit.hrl").

%%% These tests are trying to isolate phofs internals and are not the
%%% main integration tests running the entire system (dubdub_test.erl).

mr_wc_test() ->
  Map = fun(Pid, X) ->
            Pid ! {X, 1}
        end,
  Reduce = fun({Key, Vals}, Acc) ->
               [{Key, utils:sum(Vals)} | Acc]
           end,
  L = ["the","quick","brown","fox","jumped","over","the","lazy","dog"],
  Results = phofs:mapreduce(Map, Reduce, [], L),
  ?debugFmt("mr wc results:~n~p~n", [Results]).

%% TODO: add tests around the contents like:
%% [{"the",2},
%%  {"over",1},
%%  {"dog",1},
%%  {"fox",1},
%%  {"brown",1},
%%  {"jumped",1},
%%  {"lazy",1},
%%  {"quick",1}]
