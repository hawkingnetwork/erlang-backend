-module(customer_db).

%%% Resident API
-export([save/1
		  ,delete/1
      ,delete_by_user_name/1
			,delete_all/0
		  ,find/1
			,find_by_user_name/1
			,find_by_user_name/1
			,find_by_ids/1
			,find_by_conditions/1
			,find_by_conditions/2
      ,find_all/0
			,find_all/2
			,find_by_conditions/4
			,reindex/0 ]).

-type condition()   :: {atom(), atom()}.
-type conditions()  :: [condition()].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Resident API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates a new resident.
%% @doc Updates an resident.
-spec save(customer_doc:customer()) -> ok.
save(Resident) ->
  sumo:persist(customer_doc, Resident).

%% @doc Deletes the given resident.
-spec delete(binary()) -> boolean().
delete(Id) ->
  sumo:delete(customer_doc, Id).

-spec delete_by_user_name(binary()) -> boolean().
delete_by_user_name(UserName) ->
  sumo:delete_by(customer_doc, [{user_name_id, UserName}]).

-spec delete_all() -> boolean().
delete_all() ->
  sumo:delete_all(customer_doc).

%% @doc Finds an resident, given the Email.
-spec find(binary()) -> customer_doc:customer()|notfound.
find(Id) ->
  sumo:find(customer_doc, Id).

-spec find_by_ids(binary()) -> [customer_doc:customer()].
find_by_ids(Ids) ->
  sumo:find_by(customer_doc, [{id, 'in', Ids}]).

-spec find_by_user_name(binary()) -> [customer_doc:customer()].
find_by_user_name(UserName) ->
  sumo:find_by(customer_doc, [{user_name_id, UserName}]).

-spec find_by_user_name(binary()) -> [customer_doc:customer()].
find_by_user_name(UserName) ->
	sumo:find_by(customer_doc, [{user_name_id, UserName}]).

-spec find_all() -> [customer_doc:customer()].
find_all() ->
  sumo:find_all(customer_doc).

-spec find_all(non_neg_integer(), non_neg_integer()) -> [customer_doc:customer()].
find_all(Limit, Offset) ->
  sumo:find_all(customer_doc, [], Limit, Offset).

-spec find_by_conditions(conditions()) -> [customer_doc:customer()].
find_by_conditions(Conditions) ->
	sumo:find_by(customer_doc, Conditions).

-spec find_by_conditions(conditions(),conditions()) -> [customer_doc:customer()].
find_by_conditions(AccountQuery, Query) ->
	Conditions = build_query(AccountQuery, Query),
	sumo:find_by(customer_doc, Conditions).

-spec find_by_conditions(conditions(),conditions(),non_neg_integer(), non_neg_integer()) -> [customer_doc:customer()].
find_by_conditions(AccountQuery, Query, Limit, Offset) ->
  Conditions = build_query(AccountQuery, Query),
  SortOrders = build_sort([], Query),
  sumo:find_by(customer_doc, Conditions, SortOrders, Limit, Offset).

build_sort(Query, [{Key, Value}| Tail]) when is_list(Query) ->

  lager:debug("------ build_sort: Key ~p Value ~p ~n",[Key, Value]),
  Condition = if
        Key == <<"sort_first_name">>  -> {first_name, Value};
        Key == <<"sort_last_name">>  -> {last_name, Value};
        Key == <<"sort_created_by">> -> {created_by_id, Value};
        Key == <<"sort_created_time">> -> {created_time_dt, Value};
        Key == <<"sort_updated_by">> -> {updated_by_id, Value};
        Key == <<"sort_updated_time">> -> {updated_time_dt, Value};
        true -> ignore
      end,
        lager:debug("------ build_sort: Condition ~p ~n",[Condition]),
  NewQuery =
  case Condition of
    ignore -> Query;
    _ -> [Condition|Query]
  end,
  lager:debug("------ build_sort: NewSort ~p ~n",[NewQuery]),
  build_sort(NewQuery, Tail) ;

build_sort(Query, _Other) when is_list(Query) ->
  Query;

build_sort(_Query, _Other) ->
  [].

build_query(Query, [{Key, Value}| Tail]) when is_list(Query) ->
lager:debug("------ build_query: Key ~p Value ~p ~n",[Key, Value]),
  Condition = if
        Key == <<"filter_phone">>  -> {phone_number_id, Value};
        Key == <<"filter_user_name">>  -> {user_name_id, Value};
        Key == <<"filter_first_name">>  -> {first_name_id, Value};
        Key == <<"filter_last_name">>  -> {last_name_id, Value};
        Key == <<"filter_created_by">>  -> {created_by_id, Value};
        Key == <<"filter_created_time_gt">>  -> {created_time_dt, '>', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time_gte">>  -> {created_time_dt, '>=', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time_lt">>  -> {created_time_dt, '<', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time_lte">>  -> {created_time_dt, '=<', datetime_util:utc_format(Value)};
        Key == <<"filter_created_time">>  -> {created_time_dt, datetime_util:utc_format(Value)};
        Key == <<"filter_updated_by">>  -> {updated_by_id, Value};
        Key == <<"filter_updated_time_gt">>  -> {updated_time_dt, '>', datetime_util:utc_format(Value)};
        Key == <<"filter_updated_time_gte">>  -> {updated_time_dt, '>=', datetime_util:utc_format(Value)};
        Key == <<"filter_updated_time_lt">>  -> {updated_time_dt, '<', datetime_util:utc_format(Value)};
        Key == <<"filter_updated_time_lte">>  -> {updated_time_dt, '=<', datetime_util:utc_format(Value)};
        Key == <<"filter_updated_time">>  -> {updated_time_dt, datetime_util:utc_format(Value)};
        true -> ignore
      end,
  lager:debug("------ build_query: Condition ~p ~n",[Condition]),
  NewQuery =
  case Condition of
    ignore -> Query;
    _ -> [Condition|Query]
  end,
  lager:debug("------ build_query: NewQuery ~p ~n",[NewQuery]),
  build_query(NewQuery, Tail) ;

build_query(Query, _Other) when is_list(Query) ->
  Query ;

build_query(_Query, _Other) ->
  [].

reindex()->
  Docs = find_all(),
  reindex(Docs),
  ok.

reindex([]) ->
  finished;

reindex([H|T]) ->
  save(H),
  reindex(T).
