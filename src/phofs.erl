%% ---
%%  Excerpted from "Programming Erlang",
%%  published by The Pragmatic Bookshelf.
%%  Copyrights apply to this code. It may not be used to create training material,
%%  courses, books, articles, and the like. Contact us if you are in doubt.
%%  We make no guarantees that this code is fit for any purpose.
%%  Visit http://www.pragmaticprogrammer.com/titles/jaerlang for more book information.
%% ---

%% Parallel Higher Order FunctionS module

-module(phofs).
-export([mapreduce/4, pmap/2]).

-import(lists, [foreach/2]).


%% FMap(Pid, X) -> sends {Key,Val} messages to Pid
%% FReduce(Key, [Val], AccIn) -> AccOut
%% Acc0 is the initial value of the accumulator
%% L is the list of values
mapreduce(FMap, FReduce, Acc0, L) ->
  S = self(),
  Pid = spawn(fun() -> reduce(S, FMap, FReduce, Acc0, L) end),
  receive
    {Pid, Result} ->
      Result
  end.


%% the reduce function process (running Pid from mapreduce/4)
reduce(Parent, FMap, FReduce, Acc0, L) ->
  process_flag(trap_exit, true),
  ReducePid = self(),
  %% Create the Map processes
  %%   One for each element X in L
  foreach(fun(X) ->
	      spawn_link(fun() -> do_map(ReducePid, FMap, X) end)
	  end, L),
  N = length(L),
  %% make a dictionary to store the Keys
  Dict0 = dict:new(),
  %% Wait for N Map processes to terminate
  %% TODO: handle fewer than N processes terminating?  in collect_replies/2 ?
  Dict1 = collect_replies(N, Dict0),
  Acc = dict:fold(FReduce, Acc0, Dict1),
  Parent ! {self(), Acc}.


%% collect_replies(N, Dict)
%%     collect and merge {Key, Value} messages from N processes.
%%     When N processes have terminated return a dictionary
%%     of {Key, [Value]} pairs
collect_replies(0, Dict) ->
  Dict;
collect_replies(N, Dict) ->
  receive
    {Key, Val} ->
      case dict:is_key(Key, Dict) of
	true ->
	  Dict1 = dict:append(Key, Val, Dict),
	  collect_replies(N, Dict1);
	false ->
	  Dict1 = dict:store(Key,[Val], Dict),
	  collect_replies(N, Dict1)
      end;
    {'EXIT', _,  _Why} ->
      case N rem 100 of
	0 ->
	  io:format("N: ~p~n", [N]);
	_ ->
	  ok
      end,
      collect_replies(N-1, Dict)
  end.


%% Call F(Pid, X)
%%   F must send {Key, Value} messsages to Pid
%%     and then terminate
do_map(ReducePid, FMap, X) ->
  FMap(ReducePid, X).


%% Parallelizing map.  Stolen verbatim from book.
pmap(F, L) ->
  S = self(),
  Pids = lists:map(fun(I) ->
		 spawn(fun() -> do_f(S, F, I) end)
	     end, L),
  gather(Pids).

gather([H|T]) ->
  receive
    {H, Ret} -> [Ret|gather(T)]
  end;
gather([]) ->
  [].

do_f(Parent, F, I) ->
  Parent ! {self(), (catch F(I))}.
