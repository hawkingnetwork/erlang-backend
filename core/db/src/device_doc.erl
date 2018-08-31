-module(device_doc).
-behaviour(sumo_doc).
-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).
-opaque device_info() ::
  #{
    id => binary(),
    owner_device_id => binary(),
    owner_device_type => binary(),
    status => binary(),
    app_id => binary(),
    push_id => binary(),
    os_type => binary(),
    env => binary(),
    created_by => binary(),
    created_time =>  binary(),
    updated_by => binary(),
    updated_time => binary()
  }.

-export_type([device_info/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:doc()) -> device_info().
sumo_wakeup(Doc) ->
	#{
        id =>  maps:get(id, Doc, <<>>),
        owner_device_id =>  maps:get(owner_device_id, Doc, <<>>),
        owner_device_type =>  maps:get(owner_device_type, Doc, <<>>),
        status => maps:get(status_id, Doc, <<>>),
        app_id   => maps:get(app_id, Doc, <<>>),
        push_id => maps:get(push_id, Doc, <<>>),
        os_type => maps:get(os_type_id, Doc, <<>>),
        env => maps:get(env_id, Doc, <<>>),
        created_by => maps:get(created_by_id, Doc, <<>>),
        created_time => maps:get(created_time_dt, Doc, <<>>),
        updated_by => maps:get(updated_by_id, Doc, <<>>),
        updated_time => maps:get(updated_time_dt, Doc, <<>>)
    }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(device_info()) -> sumo:doc().
sumo_sleep(Device) ->
    DefaultTime = util_db:now_to_utc_binary({0,0,0}),
    #{
        id =>  maps:get(id, Device, <<>>),
        owner_device_id =>  maps:get(owner_device_id, Device, <<>>),
        owner_device_type =>  maps:get(owner_device_type, Device, <<>>),
        status_id => maps:get(status, Device, <<>>),
        app_id   => maps:get(app_id, Device, <<>>),
        push_id => maps:get(push_id, Device, <<>>),
        os_type_id => maps:get(os_type, Device, <<>>),
        env_id => maps:get(env, Device, <<>>),
        created_by_id => maps:get(created_by, Device, <<>>),
        created_time_dt => maps:get(created_time, Device, DefaultTime),
        updated_by_id => maps:get(updated_by, Device, <<>>),
        updated_time_dt => maps:get(updated_time, Device, DefaultTime)
    }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
    sumo:new_field(id , string, [not_null, id]),
    sumo:new_field(owner_device_id , string, [not_null]),
    sumo:new_field(owner_device_type , string, [not_null]),
    sumo:new_field(status_id , string, [not_null]),
    sumo:new_field(app_id  , string, [not_null]),
    sumo:new_field(push_id , string),
    sumo:new_field(os_type_id , string, [not_null]),
    sumo:new_field(env_id, string),
    sumo:new_field(created_by_id , string),
    sumo:new_field(created_time_dt , datetime),
    sumo:new_field(updated_by_id , string),
    sumo:new_field(updated_time_dt , datetime)
  ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

