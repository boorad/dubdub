%%%-------------------------------------------------------------------
%%% File    : node_manager.erl
%%% Author  : Brad Anderson <brad@sankatygroup.com>
%%% Description : The Node Manager - its main purpose is to maintain
%%%               a list of all active erlang nodes in the cluster.
%%%
%%% Created : 31 Jan 2009 by Brad Anderson <brad@sankatygroup.com>
%%%
%%%-------------------------------------------------------------------
-module(node_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, register_node/1, next_node/1, get_all_nodes/0,
	 get_all_db_data/0, get_counts/0, add_dbs/1]).

-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,
        {nodes=[],
         lookaside=[]}).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).


%% takes a new db_manager pid and registers it into State
register_node(WorkerPid) ->
  case is_pid(WorkerPid) of
    true ->
      gen_server:call({global, ?SERVER}, {register_node, WorkerPid});
    _ ->
      {error, cannot_register_non_pid}
  end.

%% given a method like roundrobin or other, this returns the Pid of the
%% next registered db_manager on an Erlang node
next_node(Method) ->
  gen_server:call({global, ?SERVER}, {next_node, Method}).


%% get a list of all the nodes registered in the cluster
get_all_nodes() ->
  gen_server:call({global, ?SERVER}, {get_all_nodes}).


%% across all nodes, all db's, get a dump of the data
%% careful, this dumps it all ;)  you have been warned.
get_all_db_data() ->
  map_nodes(fun db_manager:get_all_db_data/1).


%% per node, and per db, returns count of # docs.
get_counts() ->
  map_nodes(fun(Node) ->
	       {Node, db_manager:get_counts(Node)}
	    end).


%% add Count db's to the cluster
add_dbs(Count) ->
  add_dbs_loop(Count).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
  process_flag(trap_exit, true),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

%% register a node in the cluster
handle_call({register_node, WorkerPid}, _From, State) ->
  link(WorkerPid),
  {reply, ok, State#state{nodes=[WorkerPid|State#state.nodes]}};


%% return the next node in the cluster

%% round robin implementation
handle_call({next_node, roundrobin}, _From, State) when
    length(State#state.nodes) > 0 ->
  [NextWorker|T] = State#state.nodes,
  {reply,
   NextWorker,
   State#state{nodes=T,
	       lookaside=[NextWorker|State#state.lookaside]}};

handle_call({next_node, roundrobin}, _From, State) ->
  NewState = State#state{nodes=State#state.lookaside, lookaside=[]},
  [NextWorker|T] = NewState#state.nodes,
  {reply,
   NextWorker,
   NewState#state{nodes=T,
		  lookaside=[NextWorker|NewState#state.lookaside]}};

%% get a list of all the nodes in the cluster
handle_call({get_all_nodes}, _From, State) ->
  AllNodes = lists:flatten([State#state.nodes, State#state.lookaside]),
  {reply, AllNodes, State};

%% bad message, ignored
handle_call(_Request, _From, State) ->
  {reply, ignored, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({'EXIT', From, _Reason}, State) ->
  #state{nodes=Nodes, lookaside=Lookaside} = State,
  {noreply, State#state{nodes=filter_worker(From, Nodes), lookaside=filter_worker(From, Lookaside)}};

handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


filter_worker(WorkerPid, Workers) ->
  lists:filter(fun(WP) -> (WP =:= WorkerPid) == false end, Workers).


%% convenience fun to map across all nodes
map_nodes(NodeFun) ->
  Nodes = get_all_nodes(),
  lists:map(NodeFun, Nodes).


%% loop to add db's to the cluster.
%% TODO: roundrobin is hardcoded
add_dbs_loop(0) ->
  ok;
add_dbs_loop(Count) ->
  Node = next_node(roundrobin),
  io:format("~p~n", [Node]),
  db_manager:add_dbs(Node, 1),
  add_dbs_loop(Count-1).
