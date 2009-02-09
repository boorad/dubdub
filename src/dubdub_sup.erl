%%%-------------------------------------------------------------------
%%% File    : dubdub_sup.erl
%%% Author  : Brad Anderson <brad@sankatygroup.com>
%%% Description : Supervisor for boot nodes
%%%
%%% Created : 15 Jan 2009 by Brad Anderson <brad@sankatygroup.com>
%%%-------------------------------------------------------------------
-module(dubdub_sup).

-author('brad@sankatygroup.com').

-behaviour(supervisor).

-export([start_link/1, init/1]).

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(_StartArgs) ->
  supervisor:start_link({local, main_sup}, ?MODULE, []).

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
init(_Args) ->
  crypto:start(),
%%   InstanceId = string:concat("db_node_", randoms:getRandomId()),
%%   error_logger:logfile({open, preconfig:cs_log_file()}),
%%   DBNode =
%%     {list_to_atom(InstanceId),
%%      {db_node, start_link, [InstanceId]},
%%      permanent,
%%      brutal_kill,
%%      worker,
%%      []},
  DataLoader =
    {dataloader,
     {dataloader, start_link, []},
     permanent,
     brutal_kill,
     worker,
     []},
  NodeManager =
    {node_manager,
     {node_manager, start_link, []},
     permanent,
     brutal_kill,
     worker,
     []},
  AdminServer =
    {admin_server,
     {admin, start_link, []},
     permanent,
     brutal_kill,
     worker,
     []},
  {ok, {{one_for_one, 3, 10},
	[
%% 	 DBNode,
	 DataLoader,
	 NodeManager,
	 AdminServer
	]}}.
