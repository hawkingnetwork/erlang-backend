-module(user_doc).
-behaviour(sumo_doc).
-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).

-export([new/1, email/1, id/1, phone_number/1,
        first_name/1, last_name/1, password/1, role/1]).


-opaque user() ::
  #{
    id => binary(),
    organization_id => binary(),
    email => binary(),
    phone_number => binary(),
    first_name => binary(),
    last_name => binary(),
    address => binary(),
    password => binary(),
    role => binary(),
    avatar => binary(),
    timezone => binary(),
    created_by => binary(),
    created_time =>  binary(),
    updated_by => binary(),
    updated_time => binary(),
    status => binary(),
    confirm_code => binary(),
    confirm_code_created_time => binary()
  }.

-type id() :: binary().
-type email() :: binary().
-type phone_number() ::  binary().
-type first_name() :: binary().
-type last_name() :: binary().
-type password() ::  binary().
-type role() :: binary().


-export_type([user/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:doc()) -> user().
sumo_wakeup(Doc) ->
	#{
    id =>  maps:get(id, Doc, <<>>),
    organization_id =>  maps:get(organization_id, Doc, <<>>),
    email => maps:get(email_id, Doc, <<>>),
    phone_number => maps:get(phone_number_id, Doc, <<>>),
    first_name => maps:get(first_name, Doc, <<>>),
    last_name => maps:get(last_name, Doc, <<>>),
    address => maps:get(address, Doc, <<>>),
    password => maps:get(password_id, Doc, <<>>),
    role => maps:get(role_id, Doc, <<>>),
    avatar => maps:get(avatar_url_email, Doc, <<>>),
    timezone => maps:get(timezone, Doc, <<>>),
    created_by => maps:get(created_by_id, Doc, <<>>),
    created_time => maps:get(created_time_dt, Doc, <<>>),
    updated_by => maps:get(updated_by_id, Doc, <<>>),
    updated_time => maps:get(updated_time_dt, Doc, <<>>),
    status => maps:get(status_id, Doc, <<>>),
    confirm_code =>maps:get(confirm_code_id, Doc, <<>>),
    confirm_code_created_time =>maps:get(confirm_code_created_time_dt, Doc, <<>>)
    }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(user()) -> sumo:doc().
sumo_sleep(User) ->
    DefaultTime = util_db:now_to_utc_binary({0,0,0}),
    #{
    id =>  maps:get(id, User, <<>>),
    organization_id =>  maps:get(organization_id, User, <<>>),
    email_id   => maps:get(email, User, <<>>),
    phone_number_id => maps:get(phone_number, User, <<>>),
    first_name => maps:get(first_name, User, <<>>),
    last_name => maps:get(last_name, User, <<>>),
    address => maps:get(address, User, <<>>),
    password_id => maps:get(password, User, <<>>),
    role_id => maps:get(role, User, <<>>),
    avatar_url_email => maps:get(avatar, User, <<>>),
    timezone => maps:get(timezone, User, <<>>),
    created_by_id => maps:get(created_by, User, <<>>),
    created_time_dt => maps:get(created_time, User, DefaultTime),
    updated_by_id => maps:get(updated_by, User, <<>>),
    updated_time_dt => maps:get(updated_time, User, DefaultTime),
    status_id => maps:get(status, User, <<>>),
    confirm_code_id =>maps:get(confirm_code, User, <<>>),
    confirm_code_created_time_dt =>maps:get(confirm_code_created_time, User, DefaultTime)
    }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id , string, [not_null, id]),
    sumo:new_field(organization_id , string, [not_null, id]),
    sumo:new_field(email_id  , string, [ not_null]),
    sumo:new_field(phone_number_id , string, [ not_null]),
    sumo:new_field(first_name , string),
    sumo:new_field(last_name , string),
    sumo:new_field(address, string),
    sumo:new_field(password_id , string, [ not_null]),
    sumo:new_field(role_id , string, [ not_null]),
    sumo:new_field(avatar_url_email , string),
    sumo:new_field(timezone , string, [ not_null]),
    sumo:new_field(created_by_id , string, [ not_null]),
    sumo:new_field(created_time_dt , datetime, [ not_null]),
    sumo:new_field(updated_by_id , string, [ not_null]),
    sumo:new_field(updated_time_dt , datetime, [ not_null]),
    sumo:new_field(status_id , string, [ not_null]),
    sumo:new_field(confirm_code_id , string),
    sumo:new_field(confirm_code_created_time_dt , datetime)
  ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns a new user (internal).
-spec new(tuple()) -> user().
new({Id, Email, PhoneNumber, FirstName, LastName, Address, Password, Role, Avatar,
      TimeZone, CreatedBy, CreateTime, UpdatedBy, UpdateTime, Status, ConfirmCode, ConfirmCodeCreatedTime}) ->

	#{
    id  => Id,
    email   => Email,
    phone_number => PhoneNumber,
    first_name => FirstName,
    last_name => LastName,
    address => Address,
    password => Password,
    role => Role,
    avatar => Avatar,
    timezone => TimeZone,
    created_by => CreatedBy,
    created_time_dt => CreateTime,
    updated_by => UpdatedBy,
    updated_time_dt => UpdateTime,
    status => Status,
    confirm_code => ConfirmCode,
    confirm_code_created_time_dt => ConfirmCodeCreatedTime
  }.



%% @doc Returns callid of the given user.
-spec id(user()) -> id().
id(#{id:= Id}) -> Id.

-spec email(user()) -> email().
email(#{email := Email})  ->
   Email.

-spec phone_number(user()) -> phone_number().
phone_number(#{phone_number := PhoneNumber}) ->
  PhoneNumber.

-spec first_name(user()) -> first_name().
first_name(#{first_name := FirstName}) ->
  FirstName.

-spec last_name(user()) -> last_name().
last_name(#{last_name := LastName}) ->
  LastName.

-spec password(user()) -> password().
password(#{password := Password}) ->
  Password.

-spec role(user()) -> role().
role(#{role := Role})  ->
   Role.
