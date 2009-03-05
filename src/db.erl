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

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/1, insert/2, get_all/1, get_docs_limit/2, get_count/1, q/5,
	 truncate/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


%%====================================================================
%% API
%%====================================================================

%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link(InstanceId) ->
  gen_server:start_link(?SERVER, [InstanceId], []).


%% insert a document value into a given node
insert(Node, V) ->
  gen_server:call(Node, {insert, null, V}).  % TODO: generate key hashes

%%
%% query a db process
%%

%% %% for tuples/match_spec
%% q(Db, match_spec, MatchSpec, Reduce) ->
%%   CompiledMatchSpec = ets:match_spec_compile(MatchSpec),
%%   case ets:is_compiled_ms(CompiledMatchSpec) of
%%     true ->
%%       gen_server:call(Db, {q, match_spec, CompiledMatchSpec, Reduce});
%%     _ ->
%%       {error, bad_matchspec}
%%   end;

%% for all other data structures and query methods
q(Db, Type, Map, Reduce, Acc0) ->
  gen_server:call(Db, {q, Type, self(), Map, Reduce, Acc0}),
  q_waitloop().

q_waitloop() ->
  receive
    {query_ended, Results} ->
      Results;
    _ ->
      ignored,
      q_waitloop()
  end.

%% get all documents in the supplied DB
get_all(Db) ->
  gen_server:call(Db, {get_all}).


%% get first 'Limit' number of docs in the supplied DB
get_docs_limit(Db, Limit) ->
  gen_server:call(Db, {get_docs_limit, Limit}).


%% get a count of the docs in the provided DB
get_count(Db) ->
  gen_server:call(Db, {get_count}).


%% whack all data in the provided DB
truncate(Db) ->
  gen_server:call(Db, {truncate}).

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
init([InstanceId]) ->
  process_flag(trap_exit, true),
  io:format("starting DB ~p...~n", [InstanceId]),
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
  {reply, {ok, insert}, NewState};

handle_call({q, list, _Caller, Filter, _Reduce, _Acc0}, _From, State) ->
  Results = lists:filter(Filter, State),
  {reply, {ok, Results}, State};

handle_call({q, tuple, Parent, Map, Reduce, Acc0}, _From, State) ->
  spawn(fun() ->
	    {Time, Results}  = timer:tc(phofs, mapreduce, [Map, Reduce, Acc0, State]),
	    Parent ! {query_ended, [{results, Results}, {time, Time}, {db, self()}]}
	end),
  {reply, {ok, query_started}, State};

%% handle_call({q, match_spec, CompiledMatchSpec, _Reduce}, _From, State) ->
%%   Results = ets:match_spec_run(State, CompiledMatchSpec),
%%   {reply, {ok, Results}, State};

handle_call({q, dict, Filter, _Reduce}, _From, State) ->
  Results = dict_filter(Filter, State),
  {reply, {ok, Results}, State};

handle_call({get_all}, _From, State) ->
  {reply, {ok, State}, State};

handle_call({get_docs_limit, Limit}, _From, State) when is_integer(Limit) ->
  Return = case is_integer(Limit) of
	     true ->
	       {ok, lists:sublist(State, Limit)};
	     _ ->
	       {error, badarg, get_docs_limit, non_integer}
	   end,
  {reply, Return, State};

handle_call({get_count}, _From, State) ->
  {reply, {ok, length(State)}, State};

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

dict_filter(Filter, State) ->
  [ Filter(K, V) || {K, V} <- State ].
