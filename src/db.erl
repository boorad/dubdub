%%%-------------------------------------------------------------------
%%% File    : db.erl
%%% Author  : Brad Anderson <brad@sankatygroup.com>
%%% Description : This module serves as a database process inside
%%% of a single node
%%%
%%% Created : 26 Oct 2008 by Brad Anderson <brad@sankatygroup.com>
%%%-------------------------------------------------------------------
-module(db).

-behaviour(gen_server).

%% API
-export([start_link/0, insert/2, get_all/0, q/2, truncate/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(K,V) ->
  gen_server:call(?MODULE, {insert, K, V}).

q(Filter, Reduce) ->
    gen_server:call(?MODULE, {q, Filter, Reduce}).

get_all() ->
  gen_server:call(?MODULE, {get_all}).

truncate() ->
  gen_server:call(?MODULE, {truncate}).

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
  {ok, []}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({insert, K, V}, _From, State) ->
  NewState = [{K,V} | State],
  {reply, {ok, insert, State}, NewState};

handle_call({q, Filter, _Reduce}, _From, State) ->
  Results = lists:filter(Filter, State),
  {reply, {ok, Results}, State};

handle_call({get_all}, _From, State) ->
  {reply, {ok, State}, State};

handle_call({truncate}, _From, _State) ->
  {reply, {ok, truncate}, []};

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
