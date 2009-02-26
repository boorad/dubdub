%%%-------------------------------------------------------------------
%%% File    : dubdub_boot.erl
%%% Author  : Brad Anderson <brad@sankatygroup.com>
%%% Description :
%%%
%%% Created : 17 Jan 2009 by Brad Anderson <brad@sankatygroup.com>
%%%-------------------------------------------------------------------
-module(dubdub_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

start() ->
  application:start(dubdub).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->

  case start_node() of  %% TODO: remove hardcode
    ok ->
      case dubdub_sup:start_link(StartArgs) of
	{ok, Pid} ->
	  node_manager:add_dbs(1),  %% start first db
	  {ok, Pid};
	Error ->
	  Error
      end;
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% Function: start_node(BootNode) -> ok | {error, Msg}
%% Description: start the node as either boot or worker, based on this node's
%%              name.  There can be only one with node name 'boot@....'
start_node() ->
  [Node | _T] = string:tokens(atom_to_list(node()), "@"),
  case Node of
    "boot" ->
      start_boot_node();
    _ ->
      io:format("~nDid not find 'boot' in node name~n"),
      % Grab BootNode name from command line
      io:format("args: ~p~n", [init:get_arguments()]),
      case init:get_argument(m) of
	{ok, [[BootNode]]} ->
	  start_worker_node(BootNode);
	_ ->
	  error
      end
  end,

  %% TODO: this is called too soon after ping (if worker), and is not seeing
  %%       node_manager :(    Do we need a timeout of some kind?
  timer:sleep(1000),
  node_manager_present().


%% Function: start_boot_node() -> {ok, Pid}
%% Description: starts the node_manager, which is unique to boot node
start_boot_node() ->
  io:format("~nboot node detected, starting node_manager...~n"),
  node_manager:start_link().


%% Function: start_worker_node(BootNode) -> pong | pang
%% Description: ping the boot node and establish this node into the cluster
start_worker_node(BootNode) ->
  io:format("worker node detected, pinging boot node...~n"),
  case net_adm:ping(BootNode) of
    pong ->
      io:format("successfully joined cluster~n");
    _ ->
      io:format("could not join cluster~n")
  end.


%% Function: node_manager_present() -> true | false
%% Description: detect whether node_manager is up and running, after having
%%              started whatever this node is (boot or worker).
node_manager_present() ->
  case lists:member(node_manager,global:registered_names()) of
    true ->
      ok;
    _ ->
      {error, node_manager_not_detected}
  end.
