-module(cb_device).

-include("../crossbar.hrl").
-include("work_order.hrl").
-include("sbo.hrl").
-include("push.hrl").


-export([init/0
				 ,allowed_methods/0
				 ,allowed_methods/1
				 ,allowed_methods/2
				 ,validate/1
				 ,validate/2
				 %,validate/3
				 ,resource_exists/0
				 ,resource_exists/1
				 ,resource_exists/2
				 ,authenticate/1
				 ,authenticate/2
				 ,authenticate/3
				 ,handle_get/1
				 ,handle_get/2
				 ,handle_post/2
				]).
-export([add_device/2]).
init() ->
	_ = crossbar_bindings:bind(<<"*.allowed_methods.devices">>, ?MODULE, 'allowed_methods'),
	_ = crossbar_bindings:bind(<<"*.resource_exists.devices">>, ?MODULE, 'resource_exists'),
	_ = crossbar_bindings:bind(<<"*.validate.devices">>, ?MODULE, 'validate'),
	_ = crossbar_bindings:bind(<<"*.authenticate.devices">>, ?MODULE, 'authenticate'),
	_ = crossbar_bindings:bind(<<"*.to_json.get.devices">>, ?MODULE, 'handle_get'),
	_ = crossbar_bindings:bind(<<"*.execute.post.devices">>, ?MODULE, 'handle_post'),
	_ = crossbar_bindings:bind(<<"*.execute.put.devices">>, ?MODULE, 'handle_put').

% This is a function that tells what methods are allowed for an end point
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), ne_binary()) -> http_methods().

%% /api/v1/devices
allowed_methods() ->
	[?HTTP_GET, ?HTTP_PUT].

%% /api/v1/devices/{device_id}
allowed_methods(_DeviceId) ->
	[?HTTP_GET, ?HTTP_POST].

%% /api/v1/devices/{device_id}/path
allowed_methods(_DeviceId, _Path) ->
	[?HTTP_POST].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), ne_binary()) -> boolean().

%% /api/v1/devices
resource_exists() -> 'true'.

%% /api/v1/devices/{device_id}
resource_exists(_DeviceId) -> 'true'.

%% /api/v1/devices/path
% resource_exists(_DeviceId, ?PATH_) -> 'true';
% resource_exists(_DeviceId, ?PATH_) -> 'true';
% resource_exists(_DeviceId, ?PATH_) -> 'true';
resource_exists(_DeviceId, _Path) -> 'false'.

%% /api/v1/devices
-spec authenticate(cb_context:context()) -> boolean().
-spec authenticate(cb_context:context(), path_token()) -> boolean().



authenticate(Context) ->
	authenticate_with_method(Context, cb_context:req_verb(Context)).
authenticate_with_method(Context, ?HTTP_PUT) -> 
	Token = cb_context:auth_token(Context),
	sbo_util:oauth2_authentic(Token, Context);

authenticate_with_method(Context, ?HTTP_GET) -> 
	Token = cb_context:auth_token(Context),
	sbo_util:oauth2_authentic(Token, Context).

%% /api/v1/devices/{device_id}
authenticate(Context, _DeviceId) ->
	Token = cb_context:auth_token(Context),
	sbo_util:oauth2_authentic(Token, Context).

%% /api/v1/devices/{device_id}/path
authenticate(_Context, _DeviceId, _Path) -> false. 

-spec validate(cb_context:context()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().

%% Validate resource : /api/v1/devices
validate(Context) ->
	validate_request(Context, cb_context:req_verb(Context)).

%% Validate resource : /api/v1/devices/{device_id}
validate(Context, Id) ->
	validate_request(Id, Context, cb_context:req_verb(Context)). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Service APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GET api/v1/devices
handle_get({Req, Context}) ->
	QueryJson = cb_context:query_string(Context),
	Role = cb_context:role(Context),
	RequesterId = cb_context:id(Context),
	Limit = sbo_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
	Offset = sbo_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
	PropQueryJson = wh_json:to_proplist(QueryJson),
	DeviceInfos =  get_devices(PropQueryJson, Limit, Offset),
	Response = get_sub_fields(DeviceInfos),  
	{Req, cb_context:setters(Context
													 ,[{fun cb_context:set_resp_data/2, Response}
														 ,{fun cb_context:set_resp_status/2, 'success'}])}.										 

handle_get({Req, Context}, Id) ->
	RequesterId = cb_context:id(Context),
	case device_db:find_device(Id) of
		DeviceDb when is_map(DeviceDb) ->
			Response = get_sub_fields([DeviceDb]), 
			{Req, cb_context:setters(Context
															 ,[{fun cb_context:set_resp_data/2, Response}
																 ,{fun cb_context:set_resp_status/2, 'success'}])};
		notfound ->
			{Req, cb_context:setters(Context
															 ,[{fun cb_context:set_resp_error_msg/2, <<"Not Found Device">>}
																 ,{fun cb_context:set_resp_status/2, 'error'}
																 ,{fun cb_context:set_resp_error_code/2, 404}])}		
	end.

handle_post(Context, DeviceId) ->
	RequesterId = cb_context:id(Context),
	ReqJson = cb_context:req_json(Context),
	case device_db:find_device(DeviceId) of
		DeviceDb when is_map(DeviceDb) ->
			ReqDeviceInfo = get_device_info(ReqJson, DeviceDb, RequesterId),
			DeviceInfo = device_db:save_device(maps:merge(DeviceDb, ReqDeviceInfo)), 
			RespData = get_sub_fields(DeviceInfo),
			cb_context:setters(Context
												 ,[{fun cb_context:set_resp_data/2, RespData}
													 ,{fun cb_context:set_resp_status/2, 'success'}
													]);										
		_ ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, <<"Device Not Found">>},
													{fun cb_context:set_resp_status/2, <<"error">>},
													{fun cb_context:set_resp_error_code/2, 404}
												 ])
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_devices(QueryJson, Limit, Offset) ->
	device_db:find_by_conditions([], [{<<"sort_created_time">>, desc}|QueryJson], Limit, Offset).

get_sub_fields(DeviceInfo) when is_map(DeviceInfo) ->
	Fields = [created_time, created_by, updated_time, updated_by],
	maps:without(Fields, DeviceInfo);

get_sub_fields(DevicesInfo) when is_list(DevicesInfo) ->
	Fun = fun(DeviceInfo) -> 
						get_sub_fields(DeviceInfo) 
				end,
	lists:map(Fun, DevicesInfo).

get_device_info(ReqJson, Device, Id) ->
	UpdatedTime = sbo_util:now_to_utc_binary(os:timestamp()),
	AppId = wh_json:get_value(<<"app_id">>, ReqJson, maps:get(app_id, Device)),
	OsType = wh_json:get_value(<<"os_type">>, ReqJson, maps:get(os_type, Device)),
	PushId = wh_json:get_value(<<"push_id">>, ReqJson, maps:get(push_id, Device)),
	#{app_id => AppId,
		push_id => PushId,
		os_type => OsType,
		created_by => Id,
		updated_by => Id,
		updated_time => UpdatedTime}. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Validate Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validate_request(Context, ?HTTP_GET) ->
	cb_context:setters(Context
										 ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(Context, ?HTTP_PUT) ->
	ReqJson = cb_context:req_json(Context),
	Context1 = cb_context:setters(Context
																,[{fun cb_context:set_resp_status/2, 'success'}]),	
	ValidateFuns = [],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context1,  ValidateFuns);

validate_request(Context, _Verb) ->
	Context.

validate_request(_Id, Context, ?HTTP_GET) ->
	cb_context:setters(Context
										 ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_Id, Context, ?HTTP_POST) ->
	cb_context:setters(Context
										 ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_Id, Context, ?HTTP_DELETE) ->
	cb_context:setters(Context
										 ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_Id, Context, _Verb) ->
	Context.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Spec Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_device(ReqInfo, OwnerDeviceInfo) ->
	RequesterId = maps:get(owner_device_id, OwnerDeviceInfo),
	UpdatedTime = sbo_util:now_to_utc_binary(os:timestamp()),
	SubAliases = proplists:substitute_aliases([{<<"device_id">>, id}
																										,{<<"app_id">>, app_id}
																										,{<<"push_id">>, push_id}
																										,{<<"os_type">>, os_type}
																										,{<<"env">>, env}]
																									 , ReqInfo),
	ReqDeviceInfo = maps:from_list(SubAliases),
	DeviceId = maps:get(id, ReqDeviceInfo, <<>>),
	AddDevice = 
	if DeviceId == <<>> ->
			 %Add new
			 Id = sbo_util:get_uuid(),
			 maps:merge(ReqDeviceInfo, #{id => Id
																		 ,env => ?DEV_ENV
																		 ,status => ?ACTIVE
																		 ,created_by => RequesterId
																		 ,updated_by => RequesterId
																		 ,created_time => UpdatedTime
																		 ,updated_time => UpdatedTime})
			 ;
		 true ->
			 %Update
			 case device_db:find_device(DeviceId) of
				 #{status := ?INACTIVE} = DeviceDb ->
					 UpdateDeviceInfo = maps:merge(DeviceDb, ReqDeviceInfo),
					 maps:merge(UpdateDeviceInfo, #{env => ?DEV_ENV
																		 ,status => ?ACTIVE
																		 ,updated_by => RequesterId
																		 ,updated_time => UpdatedTime});
				 #{status := ?ACTIVE} ->
					 {error, 400, <<"Device Is Active">>};
				 notfound ->
					 {error, 404, <<"Not Found Device">>}
			 end
	end,	
	case AddDevice of
		{error, _, _} = ErrDevice ->
			ErrDevice
			;
		_ ->
			device_db:save_device(maps:merge(AddDevice, OwnerDeviceInfo)),
			[{device_id, maps:get(id, AddDevice)}]
	end.


validate_app_id(ReqJson, Context) ->
	AppId = wh_json:get_value(<<"app_id">>, ReqJson, <<>>),
	api_util:check_val(Context, <<"app_id">>, AppId).

validate_push_id(ReqJson, Context) ->
	PushId = wh_json:get_value(<<"push_id">>, ReqJson, <<>>),
	api_util:check_val(Context, <<"push_id">>, PushId).

validate_env(ReqJson, Context) ->
	Env = wh_json:get_value(<<"env">>, ReqJson, <<>>),
	api_util:check_val(Context, <<"env">>, Env).

validate_os_type(ReqJson, Context) ->
	OsType = wh_json:get_value(<<"os_type">>, ReqJson, <<>>),
	if
		OsType == ?IOS orelse OsType == ?ANDROID ->
			Context;
		true ->
			api_util:check_val(Context, <<"os_type">>, OsType)
	end.
