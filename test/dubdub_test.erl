-module(dubdub_test).

-include_lib("eunit/include/eunit.hrl").

dubdub_test_() ->
  [{setup,
    fun() -> application:start(dubdub) end,
    fun(_Msg) -> application:stop(dubdub) end,
    [
     ?_assertEqual(1, length(node_manager:get_all_nodes()))
    ]}].
