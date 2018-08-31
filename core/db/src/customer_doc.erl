-module(customer_doc).
-behaviour(sumo_doc).
-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).

-opaque resident()::
  #{
    id => binary(),
    avatar => binary(),
    first_name => binary(),
    last_name => binary(),
    created_by => binary(),
    created_time =>  binary(),
    updated_by => binary(),
    updated_time => binary(),
    user_name => binary(),
    password => binary(),
    status => binary(),
    confirm_code => binary(),
    confirm_code_created_time => binary()
  }.

-export_type([resident/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:doc()) -> resident().
sumo_wakeup(Doc) ->
	#{
    id =>  maps:get(id, Doc, <<>>),
    avatar => maps:get(avatar, Doc, <<>>),
    first_name => maps:get(first_name_id, Doc, <<>>),
    last_name => maps:get(last_name_id, Doc, <<>>),
    created_by => maps:get(created_by_id, Doc, <<>>),
    created_time => maps:get(created_time_dt, Doc, <<>>),
    updated_by => maps:get(updated_by_id, Doc, <<>>),
    updated_time => maps:get(updated_time_dt, Doc, <<>>),
    user_name => maps:get(user_name_id, Doc, <<>>),
    password => maps:get(password_id, Doc, <<>>),
    status => maps:get(status_id, Doc, <<>>),
    confirm_code => maps:get(confirm_code_id, Doc, <<>>),
    confirm_code_created_time => maps:get(confirm_code_created_time_dt, Doc, <<>>)
    }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(resident()) -> sumo:doc().
sumo_sleep(Resident) ->
    DefaultTime = util_db:now_to_utc_binary({0,0,0}),
    #{
    id =>  maps:get(id, Resident, <<>>),
    avatar => maps:get(avatar, Resident, <<>>),
    first_name_id => maps:get(first_name, Resident, <<>>),
    last_name_id => maps:get(last_name, Resident, <<>>),
    created_by_id => maps:get(created_by, Resident, <<>>),
    created_time_dt => maps:get(created_time, Resident, DefaultTime),
    updated_by_id => maps:get(updated_by, Resident, <<>>),
    updated_time_dt => maps:get(updated_time, Resident, DefaultTime),
    user_name_id => maps:get(user_name, Resident, <<>>),
    password_id => maps:get(password, Resident, <<>>),
    status_id => maps:get(status, Resident, <<>>),
    confirm_code_id => maps:get(confirm_code, Resident, <<>>),
    confirm_code_created_time_dt => maps:get(confirm_code_created_time, Resident, DefaultTime)
    }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id , string, [not_null, id]),
    sumo:new_field(avatar, string),
    sumo:new_field(first_name_id, string),
    sumo:new_field(last_name_id, string),
    sumo:new_field(created_by_id , string, [ not_null]),
    sumo:new_field(created_time_dt , datetime, [ not_null]),
    sumo:new_field(updated_by_id , string, [ not_null]),
    sumo:new_field(updated_time_dt , datetime, [ not_null]),
    sumo:new_field(user_name_id, string, [not_null]),
    sumo:new_field(password_id, string, [not_null]),
    sumo:new_field(status_id, string, [not_null]),
    sumo:new_field(confirm_code_id, string),
    sumo:new_field(confirm_code_created_time_dt, datetime, [not_null])
  ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
