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

%% API
-export([start_link/0, register_db/1, next_db/1, get_all_dbs/0,
	 get_all_db_data/0]).

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
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).


register_db(DBPid) ->
  gen_server:call({global, ?SERVER}, {register_db, DBPid}).


next_db(Method) ->
  gen_server:call({global, ?SERVER}, {next_db, Method}).


get_all_dbs() ->
  gen_server:call({global, ?SERVER}, {get_all_dbs}).

get_all_db_data() ->
  DBs = get_all_dbs(),
  Fun = fun(DB) ->
	    Data = gen_server:call(DB, {get_all}),
	    io:format("~p - ~p~n~n", [DB, Data])
	end,
  lists:map(Fun, DBs),
  ok.

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

handle_call({register_db, WorkerPid}, _From, State) ->
  link(WorkerPid),
  {reply, ok, State#state{dbs=[WorkerPid|State#state.dbs]}};

%% round robin implementation
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
  {noreply, State#state{dbs=filter_db(From, DBs), lookaside=filter_db(From, Lookaside)}};

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