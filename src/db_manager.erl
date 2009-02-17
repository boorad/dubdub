%%%-------------------------------------------------------------------
%%% File    : db_manager.erl
%%% Author  : Brad Anderson <brad@sankatygroup.com>
%%% Description : The DB Manager - its main purpose is to maintain
%%%               a list of all active db processes this local node.
%%%               One can have different load balancing strategies
%%%               based on this list, like round-robin, least loaded,
%%%               etc.  Used by data loader and query manager.
%%%
%%% Created : 31 Jan 2009 by Brad Anderson <brad@sankatygroup.com>
%%%
%%% Thanks:  hemulen via IRC 30 Jan 2009
%%%-------------------------------------------------------------------
-module(db_manager).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0, register_db/2, next_db/2, get_all_dbs/1,
	 get_all_db_data/1, get_counts/1, add_dbs/2, add_dbs/3, q/4]).

-define(SERVER, ?MODULE).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-record(state,
        {dbs=[],
         lookaside=[]}).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% register_db(Node, DBPid) -> ok | barfs
register_db(Node, DBPid) ->
  gen_server:call(Node, {register_db, DBPid}).


next_db(Node, Method) ->
  gen_server:call(Node, {next_db, Method}).


get_all_dbs(Node) ->
  gen_server:call(Node, {get_all_dbs}).


get_all_db_data(Node) ->
  map_dbs(Node, fun db:get_all/1).


get_counts(Node) ->
  map_dbs(Node, fun db:get_count/1).

%%--------------------------------------------------------------------
%% Function: add_dbs(pid(), int()) -> ok
%% Description: add new dbs to a node
%%  Node is the db_manager on the erlang node for the new db
%%--------------------------------------------------------------------
% @doc add new dbs on the local node
% @spec add_dbs(pid(), int()) -> ok
add_dbs(Node, Count) ->
  add_dbs(Node, Count, 0).

% @spec add_dbs(pid(), int(), int()) -> ok
add_dbs(Node, Count, Delay) ->
  add_dbs_loop(Node, Count, Delay).

q(Node, Type, Map, Reduce) ->
  map_dbs(Node, fun(Db) ->
		    db:q(Db, Type, Map, Reduce)
		end).

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
  node_manager:register_node(self()),
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

handle_call({register_db, WorkerPid}, _From, State) ->
  link(WorkerPid),
  {reply, ok, State#state{dbs=[WorkerPid|State#state.dbs]}};

%% round robin implementation
handle_call({next_db, roundrobin}, _From, State) when
    length(State#state.dbs) == 0, length(State#state.lookaside) == 0->
  {reply, empty_db_manager, State};

handle_call({next_db, roundrobin}, _From, State) when
    length(State#state.dbs) > 0 ->
  [NextDB|T] = State#state.dbs,
  {reply, NextDB, State#state{dbs=T, lookaside=[NextDB|State#state.lookaside]}};

handle_call({next_db, roundrobin}, _From, State) ->
  NewState = State#state{dbs=State#state.lookaside, lookaside=[]},
  [NextDB|T] = NewState#state.dbs,
  {reply, NextDB, NewState#state{dbs=T, lookaside=[NextDB|NewState#state.lookaside]}};

handle_call({get_all_dbs}, _From, State) ->
   AllNodes = lists:flatten([State#state.dbs, State#state.lookaside]),
  {reply, AllNodes, State};

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
  #state{dbs=DBs, lookaside=Lookaside} = State,
  {noreply,
   State#state{dbs=filter_db(From, DBs),
	       lookaside=filter_db(From, Lookaside)}};

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
filter_db(DBPid, DBs) ->
  lists:filter(fun(DB) -> (DB =:= DBPid) == false end, DBs).


map_dbs(Node, DbFun) ->
  DBs = get_all_dbs(Node),
  lists:map(DbFun, DBs).

%% this adds databases to the local erlang node, main_sup.
add_dbs_loop(_Node, 0, _) ->
  ok;
add_dbs_loop(Node, Count, Delay) ->
  NewId = randoms:getRandomId(),
  InstanceId = string:concat("db_", NewId),
  supervisor:start_child(main_sup, {list_to_atom(NewId),
				    {db, start_link, [Node, InstanceId]},
				    permanent,
				    brutal_kill,
				    worker,
				    []}),
  timer:sleep(Delay),
  add_dbs_loop(Node, Count - 1, Delay).
