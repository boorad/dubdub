-module(dubdub).

-export([start/0, stop/0, restart/0, test/0]).

start() ->
  application:start(dubdub).

stop() ->
  application:stop(dubdub).

restart() ->
  stop(),
  start().

test() ->
  test_suite:test().
