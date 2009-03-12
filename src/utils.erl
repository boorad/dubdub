-module(utils).

-export([sum/1, ensure_started/1]).


%% sum elements in a list
%% TODO - error check, like at all.  anything.  kthxbye.
sum([H|T]) ->
  H + sum(T);
sum([]) ->
  0.


%% @spec
%% @doc make sure an OTP application is started
ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.
