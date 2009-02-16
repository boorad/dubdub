-module(node_manager_test).

-include_lib("eunit/include/eunit.hrl").

%%% These tests are trying to isolate node_manager internals and are not the
%%% main integration tests running the entire system (dubdub_test.erl).

register_test_() ->
  [{setup,
    fun() -> node_manager:start_link() end,
    fun({ok, Pid}) -> exit(Pid, shutdown) end,
    [
     %% register non-pid
     ?_assertMatch({error, _}, node_manager:register_node(atom)),

     %% start/register a db_manager and do some tests on it
     fun() ->
	 {ok, Pid} = db_manager:start_link(),
	 %% pid matches get_all_nodes/0?
	 ?assertMatch([Pid], node_manager:get_all_nodes()),
	 %% get_all_nodes/0 length is 1?
	 ?assertEqual(1,length(node_manager:get_all_nodes())),
	 %% next_node
	 ?assertMatch(Pid, node_manager:next_node(roundrobin)),
 	 ?assertMatch(Pid, node_manager:next_node(roundrobin)),
	 ?assertEqual(true, is_pid(node_manager:next_node(roundrobin)))

	 %% unregister?

	 %% db_manager dying?

     end

    ]}].
