%%%-------------------------------------------------------------------
%%% File    : admin.erl
%%% Author  : Brad Anderson <brad@sankatygroup.com>
%%% Description : Supervisor for boot nodes
%%%
%%% Created : 16 Jan 2009 by Brad Anderson <brad@sankatygroup.com>
%%%-------------------------------------------------------------------
-module(admin).

-export([add_dbs/1, add_dbs/2, nodes/0, start_link/0, start/0]).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% Function: add_dbs(int()) -> ok
%% Description: add new dbs
%%--------------------------------------------------------------------
% @doc add new dbs on the local node
% @spec add_dbs(int()) -> ok

add_dbs(Count) ->
  add_dbs(Count, 0).

% @spec add_dbs(int(), int()) -> ok
add_dbs(Count, Delay) ->
  add_dbs_loop(Count, Delay).

add_dbs_loop(0, _) ->
  ok;
add_dbs_loop(Count, Delay) ->
  NewId = randoms:getRandomId(),
  InstanceId = string:concat("db_", NewId),
  supervisor:start_child(main_sup, {list_to_atom(NewId),
				    {db, start_link, [InstanceId]},
				    permanent,
				    brutal_kill,
				    worker,
				    []}),
  timer:sleep(Delay),
  add_dbs_loop(Count - 1, Delay).

%%===============================================================================
%% admin server functions
%%===============================================================================
start_link() ->
  process_flag(trap_exit, true),
  {ok, spawn_link(?MODULE, start, [])}.

start() ->
  register(admin_server, self()),
  add_dbs(1),  %% add first db
  loop().

loop() ->
  receive
    {get_comm_layer_dump, Sender} ->
      cs_send:send(Sender, {get_comm_layer_dump_response,
			    comm_layer.comm_logger:dump()}),
      loop()
  end.

%%--------------------------------------------------------------------
%% Function: nodes() -> list()
%% Description: contact boot server and list the known ip addresses
%%--------------------------------------------------------------------
% @doc contact boot server and list the known ip addresses
% @spec nodes() -> list()
nodes() ->
  util:uniq([IP || {IP, _, _} <- lists:sort(boot_server:node_list())]).
