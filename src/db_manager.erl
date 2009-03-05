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
	 get_all_db_data/1, get_counts/1, add_db/1, q/5]).

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

%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server, register it locally on this node
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%% register_db(Node, DbPid) -> ok | barfs
register_db(Node, DbPid) ->
  gen_server:call(Node, {register_db, DbPid}).


%% get the next db on this node, depending on Method passed in
%% for now, roundrobin is only one implemented
next_db(Node, Method) ->
  gen_server:call(Node, {next_db, Method}).


%% get a list of all the db pids on the provided node
get_all_dbs(Node) ->
  gen_server:call(Node, {get_all_dbs}).


%% get_all_db_data(Node) -> [Results]
%% get a dump of all the data in all the db's on the provided node
get_all_db_data(Node) ->
  map_dbs(Node, fun db:get_all/1).


%% get_counts(Node) -> {ok, Count}
get_counts(Node) ->
  map_dbs(Node, fun db:get_count/1).


%% Function: add_db(pid()) -> ok
%% Description: add a new db to a node
%%  Node is the db_manager on the erlang node for the new db
% @doc add a new db on the local node
% @spec add_db(pid()) -> ok
add_db(Node) ->
  {add_db, Result} = gen_server:call(Node, {add_db}),
  case Result of
    {ok, DbPid} ->
      register_db(Node, DbPid);
    {error, Error} ->
      io:format("~nError adding db: ~p~n", [Error]);
    _ ->
      {error, undetermined_error, db_manager, add_db}
  end.


%% Function: q(Node, Type, Map, Reduce) -> {ok, Results}
%% Description: query the databases on the provided node, returning results.
%%              The intermediate results are further m/r
q(Node, Type, Map, Reduce, Acc0) ->
  IntermediateResults =
    map_dbs(Node, fun(Db) ->
		      db:q(Db, Type, Map, Reduce, Acc0)
		  end),
  Data = [Data1 || {ok, Data1} <- IntermediateResults ],
  IncrMap = fun(Pid, X) ->
		F = fun(LineItem) ->
			Pid ! LineItem
		    end,
		lists:foreach(F, X)
	    end,
  phofs:mapreduce(IncrMap, Reduce, [], Data).


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

%% register this db pid into the db_manager
handle_call({register_db, WorkerPid}, _From, State) ->
  link(WorkerPid),
  {reply, ok, State#state{dbs=[WorkerPid|State#state.dbs]}};

%% get the next db in this node
%%  * round robin implementation *
handle_call({next_db, roundrobin}, _From, State) when
    length(State#state.dbs) == 0, length(State#state.lookaside) == 0->
  {reply, empty_db_manager, State};

handle_call({next_db, roundrobin}, _From, State) when
    length(State#state.dbs) > 0 ->
  [NextDB|T] = State#state.dbs,
  {reply, NextDB, State#state{dbs=T,
			      lookaside=[NextDB|State#state.lookaside]}};

handle_call({next_db, roundrobin}, _From, State) ->
  NewState = State#state{dbs=State#state.lookaside, lookaside=[]},
  [NextDB|T] = NewState#state.dbs,
  {reply, NextDB, NewState#state{dbs=T,
				 lookaside=[NextDB|NewState#state.lookaside]}};

%% get all of the db pids on this node
handle_call({get_all_dbs}, _From, State) ->
   AllNodes = lists:flatten([State#state.dbs, State#state.lookaside]),
  {reply, AllNodes, State};

%% add some db gen_servers on this node
handle_call({add_db}, _From, State) ->
  NewId = randoms:getRandomId(),
  InstanceId = string:concat("db_", NewId),
  Result = supervisor:start_child(main_sup, {list_to_atom(NewId),
					     {db, start_link,
					      [InstanceId]},
					     permanent,
					     brutal_kill,
					     worker,
					     []}),
  {reply, {add_db, Result}, State};

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


%% convenience fun to map across all db's on the provided node
map_dbs(Node, DbFun) ->
  DBs = get_all_dbs(Node),
  phofs:pmap(DbFun, DBs).
