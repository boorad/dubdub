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
  DataLoader =
    {dataloader,
     {dataloader, start_link, []},
     permanent,
     brutal_kill,
     worker,
     []},
  DbManager =
    {db_manager,
     {db_manager, start_link, []},
     permanent,
     brutal_kill,
     worker,
     []},
  {ok, {{one_for_one, 3, 10},
	[
	 DataLoader,
	 DbManager
	]}}.
