%%%-------------------------------------------------------------------
%%% File    : db_sup_and.erl
%%% Author  : Brad Anderson <brad@sankatygroup.com>
%%% Description : Supervisor for db nodes
%%%
%%% Created : 29 Jan 2009 by Brad Anderson <brad@sankatygroup.com>
%%%-------------------------------------------------------------------

-module(db_sup_and).

-behaviour(supervisor).


-export([start_link/1, init/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(InstanceId) ->
    supervisor:start_link(?MODULE, [InstanceId]).

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
init([InstanceId]) ->
  Node =
    {db_node,
     {db_node, start_link, [InstanceId]},
     permanent,
     brutal_kill,
     worker,
     []},
  DB =
    {db,
     {db, start_link, [InstanceId]},
     permanent,
     brutal_kill,
     worker,
     []},
  %% important for DB to start first, b/c Node uses DB, but not vice-versa
  {ok, {{one_for_all, 10, 1},
	[
	 DB,
	 Node
	]}}.
