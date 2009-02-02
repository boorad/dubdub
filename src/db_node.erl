%%%-------------------------------------------------------------------
%%% File    : db_node.erl
%%% Author  : Brad Anderson <brad@sankatygroup.com>
%%% Description : db node main file
%%%
%%% Created :
%%%-------------------------------------------------------------------
-module(db_node).

-export([start_link/1, start/2]).


%logging on
-define(LOG(S, L), io:format(S, L)).
%logging off
%-define(LOG(S, L), ok).

%debugging on
-define(DEBUG(State), State).
%debugging off
%-define(DEBUG(State), ok).


%% @doc The main loop of a db node
%% @spec loop(State, Debug) -> State
loop(State, Debug) ->
  receive
    {kill} ->
      ok;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Ping Messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    {ping, Ping_PID, Cookie} ->
      cs_send:send(Ping_PID, {pong, Cookie}),
      loop(State, ?DEBUG(Debug));
    {ping, Ping_PID} ->
      cs_send:send(Ping_PID, {pong, Ping_PID}),
      loop(State, ?DEBUG(Debug));
    {ping_with_cookie, Ping_PID, Cookie} ->
      cs_send:send(Ping_PID, {pong_with_cookie, Cookie}),
      loop(State, ?DEBUG(Debug));

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% database
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    {insert, Value} ->
      db:insert(null, Value),
      loop(State, ?DEBUG(Debug));

    {get_all_data} ->
      db:get_all(),
      loop(State, ?DEBUG(Debug));



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% join messages (see cs_join.erl)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 	{join, Source_PID, Id, UniqueId} = _Message ->
%% 	    ?LOG("[ ~w | I | Node   | ~w ] join~n",
%% 		      [calendar:universal_time(), self()]),
%% 	    NewState = cs_join:join_request(State, Source_PID, Id, UniqueId),
%% 	    loop(NewState, ?DEBUG(cs_debug:debug(Debug, NewState, _Message)));


	{die} ->
	    ?LOG("die ~w~n", [self()]),
	    ok;

	{reregister} ->
%% 	    cs_reregister:reregister(),
	    loop(State, ?DEBUG(Debug));


%% TODO buggy ...
      {get_node_response, _, _} ->
	loop(State, ?DEBUG(Debug));
      X ->
	io:format("db_node: unknown message ~w~n", [X]),
	%% ok
	loop(State, ?DEBUG(Debug))
    end.


%% @doc joins this node to the cluster and calls the main loop
%% TODO: _InstanceId - remove?  keep the node id in here somewhere?
-spec(start/2 :: (any(), any()) -> cs_state:state()).
start(_InstanceId, Parent) ->
  node_manager:register_node(self()),
  Parent ! done,
  timer:sleep(crypto:rand_uniform(1, 100) * 100),
  State = null,
  io:format("[ I | Node   | ~w ] joined~n",[self()]),
  loop(State, []).   % 2nd param used to be cs_debug:new()

%% @doc spawns a db node, called by the db supervisor process
%% @spec start_link(term()) -> {ok, pid()}
start_link(InstanceId) ->
  Link = spawn_link(?MODULE, start, [InstanceId, self()]),
  receive
    done ->
      ok
  end,
  {ok, Link}.
