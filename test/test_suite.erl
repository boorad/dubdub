-module(test_suite).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [{module, dubdub_test},
   {module, db_manager_test},
   {module, node_manager_test}].
