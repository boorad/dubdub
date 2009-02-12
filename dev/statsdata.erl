%% Author: rjurney
%% Created: Feb 12, 2009
%% Description: loads Amazon datasets 2002 statistics data for development
-module(statsdata).

-import(lists, [foreach/2, append/2]).
-import(re, [split/3]).
-import(string, [join/2]).
-import(file, [open/2]).

-export([geography/0, read_geography_lines/2]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
%-export([gen/0, load/0]).


%% possible OUTPUT_FORMAT values:
%%  json - couchdb ready
%%  tuple -
-define(OUTPUT_FORMAT, tuple).

%%
%% API Functions
%%


%% Load geography key from file
geography() ->
  Dict = dict:new(),
  {ok, S} = file:open("./data/geograpghy_key.csv", read),
  GeographyDict = read_geography_lines(Dict, S),
  
  % Print the whole dict, for debug.
  lists:foreach(fun(X) -> {Key,Value} = X,    
    io:fwrite("~p: ~p~n", [Key, Value]) end,
    dict:to_list(GeographyDict)).
    
% Read all lines from filehandle S and append to Dict
read_geography_lines(Dict1, S) ->

  % Read the line
  Line = io:get_line(S, ''),
  
  % If we hit EOF, just return the dict.  Otherwise parse and recurse
  case Line of
    eof -> Dict1;
    _ ->
      % Split the line by ',' - note the cute hack to address commas in place name
      L  = re:split(Line, ",", []),
      [SummaryLevel, GeographyId, GeographyName | CommaSplits] = L,
      
      % Re-join GeographyName with any illigitement comma splits
      JoinedGeo = string:join([GeographyName, CommaSplits], ","),
      
      % Insert to dict, keyed by Geography ID
      Dict2 = dict:append(GeographyId, [SummaryLevel, JoinedGeo], Dict1),
    
      % Recurse, read the next line
      read_geography_lines(Dict2, S)
    end.
  
  

%% generates .dat file based on data in pgsql (on boorad's local)
%gen() ->
%  ensure_started(inets),
%
%  %% get psql ready, and loop thru resultset
%  ensure_started(psql),
%  Pid = psql:allocate(),
%  MonthStoreQuery = get_month_store_query(),
%  {_,[{_,MonthStoreRS}]} = psql:sql_query(Pid, MonthStoreQuery),
%
%  %% okay, blast it out in binary format to file
%  glogger:start_link(?DAT_FILE),
%  process_docs(Pid, MonthStoreRS, []),
%  glogger:stop().
%
%
%%% takes result of .dat file from gen/0 and loads it into a db node for
%%% performance testing so we can build 'dubdub' the proper way
%load() ->
%  db:start_link(),
%  db:truncate(),
%  Fun = fun(Term) ->
%	    db:insert(null, Term)
%	end,
%  glogger:start_link(?DAT_FILE),
%  glogger:upread(Fun)
%  .%glogger:stop().
%
%
%%%
%%% Local Functions
%%%
%
%ensure_started(App) ->
%  case application:start(App) of
%    ok ->
%      ok;
%    {error, {already_started, App}} ->
%      ok
%  end.
%
%
%%% write list of docs (erlang terms) out to .dat file
%write_docs_to_file(Docs) ->
%  [ glogger:log(Doc) || Doc <- Docs ].
%
%
%%% this fun loops through MonthStoreRS, filling up DocList to the BULK_CNT
%%% amount and once it's there, writes the doc list out to its ultimate
%%% destination.
%
%%% this clause does the last few DocList remaining, when less than BULK_CNT
%process_docs(_Pid, [], DocList) ->
%  write_docs_to_file(DocList);
%%% this clause writes to destination and continues on through MonthStoreRS
%%% with a new blank DocList
%process_docs(Pid, MonthStoreRS, DocList) when length(DocList) >= ?BULK_CNT ->
%  write_docs_to_file(DocList),
%  process_docs(Pid, MonthStoreRS, []);
%%% this clause adds a new doc to the DocList
%process_docs(Pid, MonthStoreRS, DocList) ->
%  [ {M,S} | Rest ] = MonthStoreRS,
%  Doc = get_month_store(Pid, M, S),
%  case Doc of
%    null ->
%      process_docs(Pid, Rest, DocList);
%    _ ->
%      process_docs(Pid, Rest, [Doc | DocList])
%  end.
%
%
%get_month_store(Pid, M, S) ->
%  io:format("month: ~p  store: ~p~n", [M, S]),
%
%  %% Raw P&L Data
%  SingleDocQuery = get_singledoc_query(M, S),
%  SingleDocRS = psql:sql_query(Pid, SingleDocQuery),
%  RawData = parse_singledoc(SingleDocRS),
%  %%io:format("~p~n", [RawData]),
%
%  case RawData of
%    null ->
%      null;
%    _ ->
%      %% Store Information
%      StoreInfoQuery = get_storeinfo_query(S),
%      StoreInfoRS = psql:sql_query(Pid, StoreInfoQuery),
%      StoreInfo = parse_storeinfo(?OUTPUT_FORMAT, StoreInfoRS),
%
%      %% return output of doc
%      Time = format_time(?OUTPUT_FORMAT, M),
%      format_month_store(?OUTPUT_FORMAT, Time, StoreInfo, RawData)
%  end.
%
%
%format_time(json, Month) ->
%  {obj, [{"year", 2008},
%	 {"month", Month}]};
%format_time(tuple, Month) ->
%  {{year, 2008},
%   {month, Month}}.
%
%
%format_month_store(json, Time, Store, Data) ->
%  {obj, [{"time", Time},
%	 {"store", Store},
%	 {"data", Data}]};
%format_month_store(tuple, Time, Store, Data) ->
%  {{time, Time},
%   {store, Store},
%   {data, Data}}.
%
%
%get_month_store_query() ->
%  "  SELECT gmonth-96, gstore "
%    "FROM data "
%    "WHERE gaccount < 224 "
%    "  AND gmonth-96 between 1 and 12 "
%    "GROUP BY gmonth, gstore "
%    "ORDER BY gstore, gmonth "
%    "LIMIT 100;".  % for dev
%
%
%get_singledoc_query(M, S) ->
%  "  SELECT AccountName, mamt "
%    "FROM data d "
%    " INNER JOIN stores s ON d.gstore = s.storeint "
%    " INNER JOIN accounts a ON d.gaccount = a.aaccount "
%    "WHERE gstore = " ++ io_lib:format("~p", [S]) ++ " "
%    "  AND gmonth = " ++ io_lib:format("~p", [M]) ++ "+96;".
%
%
%parse_singledoc(RS) ->
%  {_,[{_,Data}]} = RS,
%  case Data of
%    [] ->
%      null;
%    _ ->
%      Raw = [ {K, val(V)} || {K,V} <- Data ],
%      case Raw of
%	[] ->
%	  null;
%	_ ->
%	  format_singledoc(?OUTPUT_FORMAT, Raw)
%      end
%  end.
%
%
%format_singledoc(json, Raw) ->
%  {obj, Raw};
%format_singledoc(tuple, Raw) ->
%  list_to_tuple(Raw).
%
%
%get_storeinfo_query(S) ->
%  "  SELECT s.storeint as store ,site_city, site_state, site_zip, "
%    " exterior, interior, restroom, breakfast, sunday, adi_name, adi, "
%    " ownership, playground_cd, seat_cnt, park_space_cnt "
%    "FROM stores s "
%    "WHERE sstore = " ++ io_lib:format("~p", [S]) ++ ";".
%
%
%parse_storeinfo(json, RS) ->
%  {_,[{_,Data}]} = RS,
%  [StoreInfo|_] = Data,
%  {Store, SiteCity, SiteState, SiteZip, Exterior, Interior, Restroom,
%   Breakfast, Sunday, AdiName, Adi, Ownership, Playground, SeatCount,
%   ParkingSpaces} = StoreInfo,
%  {obj, [
%   {"store_num", val(Store)},
%   {"city", val(SiteCity)},
%   {"state", val(SiteState)},
%   {"zip", val(SiteZip)},
%   {"exterior", val(Exterior == "T")},
%   {"interior", val(Interior == "T")},
%   {"restroom", val(Restroom == "T")},
%   {"breakfast", val(Breakfast == "T")},
%   {"sunday", val(Sunday == "T")},
%   {"adi_name", val(AdiName)},
%   {"adi_num", val(list_to_integer(Adi))},
%   {"ownership", val(Ownership)},
%   {"playground", val(Playground)},
%   {"seats", int(SeatCount)},
%   {"parking_spaces", int(ParkingSpaces)}
%  ]};
%parse_storeinfo(tuple, RS) ->
%  {_,[{_,Data}]} = RS,
%  [StoreInfo|_] = Data,
%  {Store, SiteCity, SiteState, SiteZip, Exterior, Interior, Restroom,
%   Breakfast, Sunday, AdiName, Adi, Ownership, Playground, SeatCount,
%   ParkingSpaces} = StoreInfo,
%  {
%   {"store_num", val(Store)},
%   {"city", val(SiteCity)},
%   {"state", val(SiteState)},
%   {"zip", val(SiteZip)},
%   {"exterior", val(Exterior == "T")},
%   {"interior", val(Interior == "T")},
%   {"restroom", val(Restroom == "T")},
%   {"breakfast", val(Breakfast == "T")},
%   {"sunday", val(Sunday == "T")},
%   {"adi_name", val(AdiName)},
%   {"adi_num", val(list_to_integer(Adi))},
%   {"ownership", val(Ownership)},
%   {"playground", val(Playground)},
%   {"seats", int(SeatCount)},
%   {"parking_spaces", int(ParkingSpaces)}
%  }.
%
%
%
%val(Val) when is_list(Val) ->
%  list_to_binary(string:strip(Val));
%val(Val) ->
%  Val.
%
%int(Val) when is_list(Val) ->
%  case Val of
%    [] -> 0;
%    _ -> list_to_integer(Val)
%  end;
%int(Val) ->
%  Val.
