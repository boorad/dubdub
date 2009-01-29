%%%-------------------------------------------------------------------
%%% File    : db_sup_or.erl
%%% Author  : Brad Anderson <brad@sankatygroup.com>
%%% Description : Supervisor for db nodes
%%%
%%% Created : 27 Jan 2009 by Brad Anderson <brad@sankatygroup.com>
%%%-------------------------------------------------------------------

-module(db_sup_or).

-author('brad@sankatygroup.com').

-behaviour(supervisor).

-export([start_link/0, init/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link(?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using
%% supervisor:start_link/[2,3], this function is called by the new process
%% to find out about restart strategy, maximum restart frequency and child
%% specifications.
%%--------------------------------------------------------------------
init([]) ->
  InstanceId = string:concat("db_node_", randoms:getRandomId()),
  DB =
    {db_node,
     {db, start_link, [InstanceId]},
     permanent,
     brutal_kill,
     worker,
     []},
%%     KeyHolder =
%% 	{cs_keyholder,
%% 	 {cs_keyholder, start_link, [InstanceId]},
%% 	 permanent,
%% 	 brutal_kill,
%% 	 worker,
%% 	 []},
%%     Supervisor_AND =
%% 	{cs_supervisor_and,
%% 	 {cs_sup_and, start_link, [InstanceId]},
%% 	 permanent,
%% 	 brutal_kill,
%% 	 supervisor,
%% 	 []},
%%     RingMaintenance =
%% 	{?RM,
%% 	 {?RM, start_link, [InstanceId]},
%% 	 permanent,
%% 	 brutal_kill,
%% 	 worker,
%% 	 []},
%%     RoutingTable =
%% 	{routingtable,
%% 	 {rt_loop, start_link, [InstanceId]},
%% 	 permanent,
%% 	 brutal_kill,
%% 	 worker,
%% 	 []},
    {ok, {{one_for_one, 10, 1},
	  [
	   DB
%% 	   KeyHolder,
%% 	   RingMaintenance,
%% 	   RoutingTable,
%% 	   Supervisor_AND
	  ]}}.
