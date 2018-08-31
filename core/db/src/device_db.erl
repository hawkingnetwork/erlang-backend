-module(device_db).

%%% Device API
-export([new_device/1, save_device/1,
		  del_device/1, del_device_by_id/1,
      del_all_devices/0, find_device/1, find_devices_by_resident_id/1,
			find_devices_by_status_and_ownerid/1,
			find_devices_by_username/1, find_active_devices_by_username/1,
      del_device_by_username/1,find_all_devices/0, find_all_devices/2, find_by_conditions/1,
      find_by_name/3, find_by_conditions/4, reindex/0,
			find_devices_by_username_and_os_type/2]).
-type condition()   :: {atom(), atom()}.
-type conditions()  :: [condition()].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Device API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates a new device.

-spec new_device(tuple()) -> device_doc:device_info().
new_device({Id, Status, CustomerId, AppId, PushId, OsType, Env, MqttPassword, CreatedBy, CreateTime, UpdatedBy, UpdateTime}) ->

  sumo:persist(device_doc, device_doc:new({Id, Status, CustomerId, AppId, PushId, OsType, Env, MqttPassword, CreatedBy, CreateTime, UpdatedBy, UpdateTime})).

%% @doc Updates an device.
-spec save_device(device_doc:device_info()) -> ok.
save_device(Device) ->
  sumo:persist(device_doc, Device).

%% @doc Deletes the given device.
-spec del_device(device_doc:device_info()) -> boolean().
del_device(Device) ->
  sumo:delete(device_doc, device_doc:id(Device)).

-spec del_device_by_id(binary()) -> boolean().
del_device_by_id(Id) ->
  sumo:delete(device_doc, Id).

-spec del_device_by_username(binary()) -> boolean().
del_device_by_username(Username) ->
  sumo:delete_by(device_doc, [{username_id, Username}]).

-spec del_all_devices() -> boolean().
del_all_devices() ->
  sumo:delete_all(device_doc).

%% @doc Finds an device, given the Email.
-spec find_device(binary()) -> device_doc:device_info()|notfound.
find_device(Id) ->
  sumo:find(device_doc, Id).

-spec find_devices_by_username(binary()) -> [device_doc:device_info()].
find_devices_by_username(Username) ->
  sumo:find_by(device_doc, [{username_id, Username}]).

-spec find_devices_by_resident_id(binary()) -> [device_doc:device_info()].
find_devices_by_resident_id(ResidentId) ->
  sumo:find_by(device_doc, [{"owner_device_info.id", ResidentId}]).

-spec find_devices_by_status_and_ownerid(binary()) -> [device_doc:device_info()].
find_devices_by_status_and_ownerid(Id) ->
	sumo:find_by(device_doc, [{"owner_device_info.id", Id},{status_id, <<"active">>}]).

-spec find_active_devices_by_username(binary()) -> [device_doc:device_info()].
find_active_devices_by_username(Username) ->
  sumo:find_by(device_doc, [{status_id, <<"active">>},{username_id, Username}, {push_id, '/=', <<>>}]).

-spec find_devices_by_username_and_os_type(binary(), binary()) -> [device_doc:device_info()].
find_devices_by_username_and_os_type(Username, OsType) ->
  sumo:find_by(device_doc, [{username_id, Username}, {os_type_id, OsType}]).

-spec find_all_devices() -> [device_doc:device_info()].
find_all_devices() ->
  sumo:find_all(device_doc).

-spec find_all_devices(non_neg_integer(), non_neg_integer()) -> [device_doc:device_info()].
find_all_devices(Limit, Offset) ->
  sumo:find_all(device_doc, [], Limit, Offset).

-spec find_by_name(binary(), non_neg_integer(), non_neg_integer()) -> [device_doc:device_info()].
find_by_name(MqttPassword, Limit, Offset) ->
    sumo:find_by(device_doc, [{mqtt_password_id, MqttPassword}], Limit, Offset).

%-spec find_by_conditions(conditions(),conditions(), non_neg_integer(), non_neg_integer()) -> [device_doc:device_info()].
%find_by_conditions(OsType, SortCreateTime , Limit, Offset) ->
%    sumo:find_by(device_doc,[{os_type, OsType}]  ,[{created_time_dt, SortCreateTime}] , Limit, Offset).

-spec find_by_conditions(conditions()) -> [device_doc:device_info()].
find_by_conditions(AccountQuery) ->
	sumo:find_by(device_doc, AccountQuery).

-spec find_by_conditions(conditions(),conditions(),non_neg_integer(), non_neg_integer()) -> [device_doc:device_info()].
find_by_conditions(AccountQuery, Query, Limit, Offset) ->
  Conditions = build_query(AccountQuery, Query),
  SortOrders = build_sort([], Query),
  sumo:find_by(device_doc, Conditions, SortOrders, Limit, Offset).

build_sort(Query, [{Key, Value}| Tail]) when is_list(Query) ->

  lager:debug("------ build_sort: Key ~p Value ~p ~n",[Key, Value]),
  Condition = if

        Key == <<"sort_created_time">> -> {created_time_dt, Value};
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
    ignore ->
      Query;
    _ ->
      [Condition|Query]
  end,
  lager:debug("------ build_query: NewQuery ~p ~n",[NewQuery]),
  build_query(NewQuery, Tail) ;

build_query(Query, _Other) when is_list(Query) ->
  Query ;

build_query(_Query, _Other) ->
  [].

reindex()->
  Docs = find_all_devices(),
  reindex(Docs),
  ok.

reindex([]) ->
  finished;

reindex([H|T]) ->
  save_device(H),
  reindex(T).
