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
  start_node_manager(),
  case dubdub_sup:start_link(StartArgs) of
    {ok, Pid} ->
      {ok, Pid};
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

start_node_manager() ->
  [Node | _T] = string:tokens(atom_to_list(node()), "@"),
  case Node of
    "boot" ->
      io:format("boot node detected, starting node_manager...~n"),
      node_manager:start_link();
    _ ->
      ok
  end.
