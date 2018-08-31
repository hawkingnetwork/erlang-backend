-module(user_db).

%%% User API
-export([ new_user/1, save_user/1,
		  del_user/1, del_user_by_id/1, del_user_by_email/1,
      del_user_by_phone_number/1, del_all_users/0,
		  find_user/1, find_users_by_email/1, find_users_by_phone_number/1,
      find_all_users/0, find_all_users/2
			,find_by_conditions/4
			,find_by_conditions/2
			,find_by_conditions/1
			,reindex/0 ]).

-type condition()   :: {atom(), atom()}.
-type conditions()  :: [condition()].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% User API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates a new user.

-spec new_user(tuple()) -> user_doc:user().
new_user({Id, Email, PhoneNumber, FirstName, LastName, Address, Password, Role, Avatar,
			TimeZone, CreatedBy, CreateTime, UpdatedBy, UpdateTime, Status, ConfirmCode, ConfirmCodeCreatedTime}) ->

  sumo:persist(user_doc, user_doc:new({Id, Email, PhoneNumber, FirstName,
  				LastName, Address, Password, Role, Avatar, TimeZone, CreatedBy, CreateTime, UpdatedBy,
  				UpdateTime, Status, ConfirmCode, ConfirmCodeCreatedTime})).

%% @doc Updates an user.
-spec save_user(user_doc:user()) -> ok.
save_user(User) ->
  sumo:persist(user_doc, User).

%% @doc Deletes the given user.
-spec del_user(user_doc:user()) -> boolean().
del_user(User) ->
  sumo:delete(user_doc, user_doc:id(User)).

-spec del_user_by_id(binary()) -> boolean().
del_user_by_id(Id) ->
  sumo:delete(user_doc, Id).

-spec del_user_by_email(binary()| [binary()]) -> boolean().
del_user_by_email(Email) when is_binary(Email) ->
  sumo:delete_by(user_doc, [{email_id, Email}]);
del_user_by_email([Email]) ->
  sumo:delete_by(user_doc, [{email_id, Email}]);
del_user_by_email(Emails) ->
   sumo:delete_by(user_doc, [{email_id,'in',Emails}]).


-spec del_user_by_phone_number(binary()) -> boolean().
del_user_by_phone_number(PhoneNumber) ->
  sumo:delete_by(user_doc, [{phone_number_id, PhoneNumber}]).

-spec del_all_users() -> boolean().
del_all_users() ->
  sumo:delete_all(user_doc).

%% @doc Finds an user, given the Email.
-spec find_user(binary()) -> user_doc:user()|notfound.
find_user(Id) ->
  sumo:find(user_doc, Id).

-spec find_users_by_email(binary()) -> [user_doc:user()].
find_users_by_email(Email) ->
  sumo:find_by(user_doc, [{email_id, Email}]).

-spec find_users_by_phone_number(binary()) -> [user_doc:user()].
find_users_by_phone_number(PhoneNumber) ->
  sumo:find_by(user_doc, [{phone_number_id, PhoneNumber}]).

-spec find_all_users() -> [user_doc:user()].
find_all_users() ->
  sumo:find_all(user_doc).

-spec find_all_users(non_neg_integer(), non_neg_integer()) -> [user_doc:user()].
find_all_users(Limit, Offset) ->
  sumo:find_all(user_doc, [], Limit, Offset).

-spec find_by_conditions(conditions(),conditions()) -> [user_doc:user()].
find_by_conditions(AccountQuery) ->
	sumo:find_by(user_doc, AccountQuery).

-spec find_by_conditions(conditions()) -> [user_doc:user()].
find_by_conditions(AccountQuery, Query) ->
  Conditions = build_query(AccountQuery, Query),
	sumo:find_by(user_doc, Conditions).

-spec find_by_conditions(conditions(),conditions(),non_neg_integer(), non_neg_integer()) -> [user_doc:user()].
find_by_conditions(AccountQuery, Query, Limit, Offset) ->
  Conditions = build_query(AccountQuery, Query),
  SortOrders = build_sort([], Query),
  sumo:find_by(user_doc, Conditions, SortOrders, Limit, Offset).

build_sort(Query, [{Key, Value}| Tail]) when is_list(Query) ->

  lager:debug("------ build_sort: Key ~p Value ~p ~n",[Key, Value]),
  Condition = if
        Key == <<"sort_email">>  -> {email_id, Value};
        Key == <<"sort_first_name">>  -> {first_name, Value};
        Key == <<"sort_last_name">>  -> {last_name, Value};
        Key == <<"sort_role">>  -> {role_id, Value};
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
        Key == <<"filter_organization">> -> {organization_id, Value};
        Key == <<"filter_email">>  -> {email_id, Value};
        Key == <<"filter_phone">>  -> {phone_number_id, Value};
        Key == <<"filter_first_name">>  -> {first_name, Value};
        Key == <<"filter_last_name">>  -> {last_name, Value};
        Key == <<"filter_role">>  -> {role_id, Value};
        Key == <<"filter_time_zone">>  -> {time_zone, Value};
        Key == <<"filter_type">>  -> {type_id, Value};
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
  Docs = find_all_users(),
  reindex(Docs),
  ok.

reindex([]) ->
  finished;

reindex([H|T]) ->
  save_user(H),
  reindex(T).
