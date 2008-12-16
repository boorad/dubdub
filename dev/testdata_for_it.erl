%% Author: brad
%% Created: Jun 16, 2008
%% Description: creates consistent test data for development
-module(testdata_for_it).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([go/0]).


-define(OUTPUT_FORMAT, tuple).
-define(OUTPUT_FILE, atom_to_list(?OUTPUT_FORMAT) ++ ".dat").
-define(BULK_CNT, 100).

%%
%% API Functions
%%


go() ->
  ensure_started(inets),

  %% get psql ready, and loop thru resultset
  ensure_started(psql),
  Pid = psql:allocate(),
  MonthStoreQuery = get_month_store_query(),
  {_,[{_,MonthStoreRS}]} = psql:sql_query(Pid, MonthStoreQuery),

  process_docs(Pid, MonthStoreRS, []).


%%
%% Local Functions
%%

ensure_started(App) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok
  end.


write_docs_to_file(Docs) ->
  [ write_doc_to_file(?OUTPUT_FILE, [append], Doc) || Doc <- Docs ],
  ok.


write_doc_to_file(File, Mode, Doc) ->
  case file:open(File, Mode) of
    {ok, FD} ->
      io:format(FD, "~p.~n~n", [Doc]);
    {error, Reason} ->
      {error, Reason}
  end.


%% this fun loops through MonthStoreRS, filling up DocList to the BULK_CNT
%% amount and once it's there, writes the doc list out to its ultimate
%% destination.

%% this clause does the last few DocList remaining, when less than BULK_CNT
process_docs(_Pid, [], DocList) ->
  write_docs_to_file(DocList);
%% this clause writes to destination and continues on through MonthStoreRS
%% with a new blank DocList
process_docs(Pid, MonthStoreRS, DocList) when length(DocList) >= ?BULK_CNT ->
  write_docs_to_file(DocList),
  process_docs(Pid, MonthStoreRS, []);
%% this clause adds a new doc to the DocList
process_docs(Pid, MonthStoreRS, DocList) ->
  [ {M,S} | Rest ] = MonthStoreRS,
  Doc = get_month_store(Pid, M, S),
  case Doc of
    null ->
      process_docs(Pid, Rest, DocList);
    _ ->
      process_docs(Pid, Rest, [Doc | DocList])
  end.


%%format_docs(json, DocList) ->
%%  io:format("~p~n~n", [DocList]).



get_month_store(Pid, M, S) ->
  io:format("month: ~p  store: ~p~n", [M, S]),

  %% Raw P&L Data
  SingleDocQuery = get_singledoc_query(M, S),
  SingleDocRS = psql:sql_query(Pid, SingleDocQuery),
  RawData = parse_singledoc(SingleDocRS),
  %%io:format("~p~n", [RawData]),

  case RawData of
    null ->
      null;
    _ ->
      %% Store Information
      StoreInfoQuery = get_storeinfo_query(S),
      StoreInfoRS = psql:sql_query(Pid, StoreInfoQuery),
      StoreInfo = parse_storeinfo(?OUTPUT_FORMAT, StoreInfoRS),

      %% return output of doc
      Time = format_time(?OUTPUT_FORMAT, M),
      format_month_store(?OUTPUT_FORMAT, Time, StoreInfo, RawData)
  end.


format_time(json, Month) ->
  {obj, [{"year", 2008},
	 {"month", Month}]};
format_time(tuple, Month) ->
  {{year, 2008},
   {month, Month}}.


format_month_store(json, Time, Store, Data) ->
  {obj, [{"time", Time},
	 {"store", Store},
	 {"data", Data}]};
format_month_store(tuple, Time, Store, Data) ->
  {{time, Time},
   {store, Store},
   {data, Data}}.


get_month_store_query() ->
  "  SELECT gmonth-96, gstore "
    "FROM data "
    "WHERE gaccount < 224 "
    "  AND gmonth-96 between 1 and 12 "
    "GROUP BY gmonth, gstore "
    "ORDER BY gstore, gmonth "
    "LIMIT 5;".  % for dev


get_singledoc_query(M, S) ->
  "  SELECT AccountName, mamt "
    "FROM data d "
    " INNER JOIN stores s ON d.gstore = s.storeint "
    " INNER JOIN accounts a ON d.gaccount = a.aaccount "
    "WHERE gstore = " ++ io_lib:format("~p", [S]) ++ " "
    "  AND gmonth = " ++ io_lib:format("~p", [M]) ++ "+96;".


parse_singledoc(RS) ->
  {_,[{_,Data}]} = RS,
  case Data of
    [] ->
      null;
    _ ->
      Raw = [ {K, val(V)} || {K,V} <- Data ],
      case Raw of
	[] ->
	  null;
	_ ->
	  format_singledoc(?OUTPUT_FORMAT, Raw)
      end
  end.


format_singledoc(json, Raw) ->
  {obj, Raw};
format_singledoc(tuple, Raw) ->
  list_to_tuple(Raw).
  %%[ Raw1|_Rest ] = Raw,
  %%{Raw1}.


get_storeinfo_query(S) ->
  "  SELECT s.storeint as store ,site_city, site_state, site_zip, "
    " exterior, interior, restroom, breakfast, sunday, adi_name, adi, "
    " ownership, playground_cd, seat_cnt, park_space_cnt "
    "FROM stores s "
    "WHERE sstore = " ++ io_lib:format("~p", [S]) ++ ";".


parse_storeinfo(json, RS) ->
  {_,[{_,Data}]} = RS,
  [StoreInfo|_] = Data,
  {Store, SiteCity, SiteState, SiteZip, Exterior, Interior, Restroom,
   Breakfast, Sunday, AdiName, Adi, Ownership, Playground, SeatCount,
   ParkingSpaces} = StoreInfo,
  {obj, [
   {"store_num", val(Store)},
   {"city", val(SiteCity)},
   {"state", val(SiteState)},
   {"zip", val(SiteZip)},
   {"exterior", val(Exterior == "T")},
   {"interior", val(Interior == "T")},
   {"restroom", val(Restroom == "T")},
   {"breakfast", val(Breakfast == "T")},
   {"sunday", val(Sunday == "T")},
   {"adi_name", val(AdiName)},
   {"adi_num", val(list_to_integer(Adi))},
   {"ownership", val(Ownership)},
   {"playground", val(Playground)},
   {"seats", int(SeatCount)},
   {"parking_spaces", int(ParkingSpaces)}
  ]};
parse_storeinfo(tuple, RS) ->
  {_,[{_,Data}]} = RS,
  [StoreInfo|_] = Data,
  {Store, SiteCity, SiteState, SiteZip, Exterior, Interior, Restroom,
   Breakfast, Sunday, AdiName, Adi, Ownership, Playground, SeatCount,
   ParkingSpaces} = StoreInfo,
  {
   {"store_num", val(Store)},
   {"city", val(SiteCity)},
   {"state", val(SiteState)},
   {"zip", val(SiteZip)},
   {"exterior", val(Exterior == "T")},
   {"interior", val(Interior == "T")},
   {"restroom", val(Restroom == "T")},
   {"breakfast", val(Breakfast == "T")},
   {"sunday", val(Sunday == "T")},
   {"adi_name", val(AdiName)},
   {"adi_num", val(list_to_integer(Adi))},
   {"ownership", val(Ownership)},
   {"playground", val(Playground)},
   {"seats", int(SeatCount)},
   {"parking_spaces", int(ParkingSpaces)}
  }.



val(Val) when is_list(Val) ->
  list_to_binary(string:strip(Val));
val(Val) ->
  Val.

int(Val) when is_list(Val) ->
  case Val of
    [] -> 0;
    _ -> list_to_integer(Val)
  end;
int(Val) ->
  Val.
