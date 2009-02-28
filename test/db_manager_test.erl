-module(db_manager_test).

-include_lib("eunit/include/eunit.hrl").

%%% These tests are trying to isolate db_manager internals and are not the
%%% main integration tests running the entire system (dubdub_test.erl).

register_test_() ->
  [{setup,
    fun() -> node_manager:start_link() end,
    fun({ok, Pid}) -> exit(Pid, shutdown) end,
    [
     %% start a db manager and do some tests
     fun() ->
	 {ok, Node} = db_manager:start_link(),
	 %% no dbs at first
	 ?assertMatch(empty_db_manager, db_manager:next_db(Node, roundrobin)),
	 %% add a db
	 {ok, DbPid} = db:start_link("1234"),
	 db_manager:register_db(Node, DbPid),
	 ?assertEqual(1, length(db_manager:get_all_dbs(Node)))
     end
    ]}].
