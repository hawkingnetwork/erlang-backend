-module(cb_user).

-include("../crossbar.hrl").
-include("sbo.hrl").
-define(USER_CONFIRM, <<"confirm">>).
-define(USER_LOGIN, <<"login">>).
-define(RESEND, <<"resend">>).
-define(USER_FORGOT_PASSWORD, <<"forgot">>).
-define(USER_RESET_PASSWORD, <<"reset">>).

-export([init/0
				 ,allowed_methods/0
				 ,allowed_methods/1
				 ,allowed_methods/2
				 ,validate/1
				 ,validate/2
				 ,validate/3
				 ,resource_exists/0
				 ,resource_exists/1
				 ,resource_exists/2
				 ,authenticate/1
				 ,authenticate/2
				 ,authenticate/3
				 ,handle_get/1
				 ,handle_get/2
				 ,handle_put/1
				 ,handle_post/3
				 ,handle_post/2
				]).

-export([get_users_profile/1
				 ,get_user_profile/1
				 ,get_user_info/1]).

init() ->
	_ = crossbar_bindings:bind(<<"*.allowed_methods.users">>, ?MODULE, 'allowed_methods'),
	_ = crossbar_bindings:bind(<<"*.resource_exists.users">>, ?MODULE, 'resource_exists'),
	_ = crossbar_bindings:bind(<<"*.validate.users">>, ?MODULE, 'validate'),
	_ = crossbar_bindings:bind(<<"*.authenticate.users">>, ?MODULE, 'authenticate'),
	_ = crossbar_bindings:bind(<<"*.to_json.get.users">>, ?MODULE, 'handle_get'),
	_ = crossbar_bindings:bind(<<"*.execute.post.users">>, ?MODULE, 'handle_post'),
	_ = crossbar_bindings:bind(<<"*.execute.put.users">>, ?MODULE, 'handle_put').

% This is a function that tells what methods are allowed for an end point
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), ne_binary()) -> http_methods().

%% /api/v1/users
allowed_methods() ->
	[?HTTP_GET, ?HTTP_PUT].

%% /api/v1/users/{user_id}
allowed_methods(_UserId) ->
	[?HTTP_GET, ?HTTP_POST].

%% /api/v1/users/{user_id}/path
allowed_methods(_UserId, _Path) ->
	[?HTTP_POST].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), ne_binary()) -> boolean().

%% /api/v1/users
resource_exists() -> 'true'.

%% /api/v1/users/{user_id}
resource_exists(_UserId) -> 'true'.

%% /api/v1/users/path
% resource_exists(_UserId, ?PATH_) -> 'true';
% resource_exists(_UserId, ?PATH_) -> 'true';
% resource_exists(_UserId, ?PATH_) -> 'true';
resource_exists(_UserId, ?CONFIRM) ->	'true';
resource_exists(_UserId, ?PASSWORD_CHANGE) -> 'true';
resource_exists(_UserId, ?LOGOUT) -> 'true';
resource_exists(_UserId, ?RESEND) -> 'true';
resource_exists(_UserId, _Path) -> 'false'.

%% /api/v1/users
-spec authenticate(cb_context:context()) -> boolean().
-spec authenticate(cb_context:context(), path_token()) -> boolean().
-spec authenticate(cb_context:context(), path_token(), path_token()) -> boolean().


authenticate(Context) ->
	authenticate_with_method(Context, cb_context:req_verb(Context)).
authenticate_with_method(_Context, ?HTTP_PUT) -> true;

authenticate_with_method(Context, ?HTTP_GET) ->
	Token = cb_context:auth_token(Context),
	hwk_util:oauth2_authentic(Token, Context).

%% /api/v1/users/{user_id}
authenticate(_Context, ?USER_LOGIN) -> 'true';
authenticate(_Context, ?USER_FORGOT_PASSWORD) -> 'true';
authenticate(_Context, ?USER_RESET_PASSWORD) -> 'true';
authenticate(Context, _UserId) ->
	Token = cb_context:auth_token(Context),
	hwk_util:oauth2_authentic(Token, Context).

%% /api/v1/users/{user_id}/path
authenticate(_Context, _UserId, ?CONFIRM) ->
	'true';
authenticate(_Context, _UserId, ?RESEND) -> 
	'true';
authenticate(Context, _UserId, ?PASSWORD_CHANGE) ->
	Token = cb_context:auth_token(Context),
	hwk_util:oauth2_authentic(Token, Context);
authenticate(Context, _UserId, ?LOGOUT) ->
	Token = cb_context:auth_token(Context),
	hwk_util:oauth2_authentic(Token, Context);
authenticate(_Context, _UserId, _Path) -> 'false'.

-spec validate(cb_context:context()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().

%% Validate resource : /api/v1/users
validate(Context) ->
	validate_request(Context, cb_context:req_verb(Context)).

%% Validate resource : /api/v1/users/{user_id}
validate(Context, Id) ->
	validate_request(Id, Context, cb_context:req_verb(Context)).

%% Validate resource: /api/v1/users/{user_id}/path
validate(Context, Id, Path) ->
	validate_request(Id, Path, Context, cb_context:req_verb(Context)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Service APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% PUT api/v1/users
-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->
	ReqJson = cb_context:req_json(Context),
	RequesterId = hwk_util:get_uuid(),
	ConfirmCode = hwk_util:get_confirm_code(),
	ReqUserInfo = get_user_info(?CREATE_ACCOUNT, ReqJson, RequesterId),
	FirstName = wh_json:get_value(<<"first_name">>, ReqJson, <<>>),
	LastName = wh_json:get_value(<<"last_name">>, ReqJson, <<>>),
	Email = wh_json:get_value(<<"email">>, ReqJson, <<>>),
	UserDb = maps:merge(ReqUserInfo, #{id => RequesterId
																		 ,email => Email
																		 ,confirm_code => ConfirmCode
																		 ,first_name => FirstName
																		 ,last_name => LastName}),
	user_db:save_user(UserDb),
	FullName = <<FirstName/binary," ",LastName/binary>>,
	EmailInfo = #{id => RequesterId
								,confirm_code => ConfirmCode
								,full_name => FullName},
	hook_handle_pool:send_email(?EMAIL_CONFIRM_CODE, Email, EmailInfo),
	Response = [{id, RequesterId},{confirm_code, ConfirmCode}],
	cb_context:setters(Context
										 ,[{fun cb_context:set_resp_data/2, Response}
											 ,{fun cb_context:set_resp_status/2, 'success'}]).

-spec handle_post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
handle_post(Context, RequesterId, ?USER_CONFIRM) ->
	ReqJson =  cb_context:req_json(Context),
	ConfirmCode = wh_json:get_value(<<"confirm_code">>, ReqJson),
	CurrentTimeToSecond = hwk_util:timestamp_second(),
	case user_db:find_user(RequesterId) of
		#{confirm_code := ConfirmCode
			,status := ?ACCOUNT_STATUS_INACTIVE} = UserDb ->
			ConfirmCodeCreatedTime = maps:get(confirm_code_created_time, UserDb),
			ConfirmCodeCreatedTimeToSecond = hwk_util:datetime_binary_to_second(ConfirmCodeCreatedTime),
			if
				CurrentTimeToSecond - ConfirmCodeCreatedTimeToSecond > ?DATESECOND ->
					cb_context:setters(Context,
														 [{fun cb_context:set_resp_error_msg/2, <<"Code Exprired">>},
															{fun cb_context:set_resp_status/2, <<"error">>},
															{fun cb_context:set_resp_error_code/2, 400}
														 ]);
				true ->
					UpdateTime = hwk_util:now_to_utc_binary(os:timestamp()),
					UpdatedUserInfo = maps:merge(UserDb, #{status => ?ACCOUNT_STATUS_ACTIVE
																								 ,updated_time => UpdateTime
																								 ,updated_by => RequesterId}),
					user_db:save_user(UpdatedUserInfo),
					cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}])
			end;
		#{status := ?ACCOUNT_STATUS_ACTIVE} ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, <<"User Actived">>},
													{fun cb_context:set_resp_status/2, <<"error">>},
													{fun cb_context:set_resp_error_code/2, 400}
												 ]);
		#{confirm_code := ConfirmCodeDb} ->
			lager:error("ConfirmCodeDb ~p; ConfirmCode ~p ~n", [ConfirmCode, ConfirmCodeDb]),
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, <<"Invalid Code">>},
													{fun cb_context:set_resp_status/2, <<"error">>},
													{fun cb_context:set_resp_error_code/2, 400}
												 ]);
		notfound ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, <<"Not Found User">>},
													{fun cb_context:set_resp_status/2, <<"error">>},
													{fun cb_context:set_resp_error_code/2, 404}
												 ])

	end;

handle_post(Context, UserId, ?RESEND) ->
	ReqJson = cb_context:req_json(Context),
	ConfirmCode = hwk_util:get_confirm_code(),
	UpdatedTime = hwk_util:now_to_utc_binary(os:timestamp()),
	case user_db:find_user(UserId) of
		#{status := AccountStatus
			,email := Email
      ,first_name := FirstName
		 ,last_name := LastName} = UserInfo ->
			if
				AccountStatus == ?ACCOUNT_STATUS_INACTIVE ->
					UpdateInfo = #{confirm_code => ConfirmCode,
												 updated_time => UpdatedTime,
												 updated_by => UserId,
												 confirm_code_created_time => UpdatedTime},
					user_db:save_user(maps:merge(UserInfo, UpdateInfo)),
					FullName = <<FirstName/binary," ",LastName/binary>>,
					EmailInfo = #{id => UserId
												,confirm_code => ConfirmCode
												,full_name => FullName},
					hook_handle_pool:send_email(?RESEND, Email, EmailInfo),
					cb_context:setters(Context
														 ,[{fun cb_context:set_resp_data/2, [{id, UserId},
																																 {confirm_code, ConfirmCode}]}
															 ,{fun cb_context:set_resp_status/2, 'success'}]);							
				AccountStatus == ?ACCOUNT_STATUS_ACTIVE ->
					cb_context:setters(Context,
														 [{fun cb_context:set_resp_error_msg/2, <<"Account Is Active">>},
															{fun cb_context:set_resp_status/2, <<"error">>},
															{fun cb_context:set_resp_error_code/2, 400}
														 ]);
				true ->
					cb_context:setters(Context,
														 [{fun cb_context:set_resp_error_msg/2, <<"Bad Request">>},
															{fun cb_context:set_resp_status/2, <<"error">>},
															{fun cb_context:set_resp_error_code/2, 400}
														 ])			
			end;
		notfound ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, <<"Not Found User">>},
													{fun cb_context:set_resp_status/2, <<"error">>},
													{fun cb_context:set_resp_error_code/2, 404}])	
	end;

handle_post(Context, UserId, ?PASSWORD_CHANGE) ->
	% Role = cb_context:role(Context),
	RequesterId = cb_context:id(Context),
	case user_db:find_user(UserId) of
		#{id := UserIdDb, password := CurrPassHashServer} = UserDb ->
			if 	RequesterId == UserIdDb ->
						ReqJson =  cb_context:req_json(Context),
						CurrPass  = wh_json:get_value(<<"current_password">>, ReqJson),
						NewPass = wh_json:get_value(<<"new_password">>, ReqJson),
						PassHash = hwk_util:to_str(CurrPassHashServer),
						{ok, ProvidedHash} = bcrypt:hashpw(CurrPass, PassHash),
						if  ProvidedHash == PassHash ->
									UpdatedTime = hwk_util:now_to_utc_binary(os:timestamp()),
									{ok, Salt} = bcrypt:gen_salt(?WORKFACTOR),
									{ok, NewPassHash} = bcrypt:hashpw(NewPass, Salt),
									UpdatedUserDB = maps:merge(UserDb, #{password => hwk_util:to_bin(NewPassHash),
																											 updated_time =>  UpdatedTime,
																											 updated_by =>  UserIdDb}),
									user_db:save_user(UpdatedUserDB),
									access_token_db:del_access_token_by_account_id(UserId),
									RespData = [{user_id, UserId}],
									cb_context:setters(Context,[{fun cb_context:set_resp_data/2, RespData},
																							{fun cb_context:set_resp_status/2,'success'}]);
								true ->
									cb_context:setters(Context,
																		 [{fun cb_context:set_resp_error_msg/2, <<"Invalid Current Password">>},
																			{fun cb_context:set_resp_status/2, <<"error">>},
																			{fun cb_context:set_resp_error_code/2, 400}
																		 ])

						end;
					true ->
						cb_context:setters(Context,
															 [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
																{fun cb_context:set_resp_status/2, <<"error">>},
																{fun cb_context:set_resp_error_code/2, 403}
															 ])
			end;
		_ ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
													{fun cb_context:set_resp_status/2, <<"error">>},
													{fun cb_context:set_resp_error_code/2, 404}
												 ])
	end;
handle_post(Context, UserId, ?LOGOUT) ->
	UserIdDb = cb_context:id(Context),
	ReqJson =  cb_context:req_json(Context),
	DeviceId = wh_json:get_value(<<"device_id">>, ReqJson, <<>>),
	Token = cb_context:auth_token(Context),
	if  UserId == UserIdDb ->
				case user_db:find_user(UserId) of
					UserDb when is_map(UserDb) ->
						if
							DeviceId == <<>> ->
								access_token_db:del_access_token_by_token(Token),
								%refresh_token_db:del_refresh_token_by_account_id(UserId),
								cb_context:setters(Context ,[{fun cb_context:set_resp_data/2, [{user_id, UserId}]},
																						 {fun cb_context:set_resp_status/2, 'success'}]);
							true ->
								case device_db:find_device(DeviceId) of
									DeviceInfo when is_map(DeviceInfo) ->
										device_db:save_device(maps:merge(DeviceInfo, #{status => ?INACTIVE})),
										access_token_db:del_access_token_by_token(Token),
										%refresh_token_db:del_refresh_token_by_account_id(UserId),
										RespData = [{user_id, UserId}],
										cb_context:setters(Context ,[{fun cb_context:set_resp_data/2, RespData},
																								 {fun cb_context:set_resp_status/2, 'success'}]);
									notfound ->	
										cb_context:setters(Context ,[{fun cb_context:set_resp_error_msg/2, <<"Not Found Device">>},
																								 {fun cb_context:set_resp_status/2, 'error'},
																								 {fun cb_context:set_resp_error_code/2, 404}])
								end														 	
						end;						
					notfound ->
						cb_context:setters(Context,
															 [{fun cb_context:set_resp_error_msg/2, <<"Not Found User">>},
																{fun cb_context:set_resp_status/2, <<"error">>},
																{fun cb_context:set_resp_error_code/2, 404}])		
				end;
			true ->
				cb_context:setters(Context,
													 [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
														{fun cb_context:set_resp_status/2, <<"error">>},
														{fun cb_context:set_resp_error_code/2, 403}])
	end.


-spec handle_post(cb_context:context(), path_token()) -> cb_context:context().
handle_post(Context, ?USER_LOGIN) ->
	ReqJson = cb_context:req_json(Context),
	DeviceInfo = wh_json:get_value(<<"device_info">>, ReqJson, []),
	UserName = wh_json:get_value(<<"user_name">>, ReqJson, <<>>),
	ResultCheck = 
	case user_db:find_users_by_email(UserName) of
		[#{status := ?ACCOUNT_STATUS_ACTIVE, id := UserId} = UserDb] ->			
			ProcessPass = cb_auth:process_password_grant(?USER_SCOPE, Context, ReqJson),
			lager:info("ProcessPass ~p ~n", [ProcessPass]),
			case ProcessPass of
				{error, _, _} = ErrProcessPass ->
					ErrProcessPass;
				{success, ResponseProcess, AccessToken} ->
					OwnerDeviceInfo = #{owner_device_id => UserId
															,owner_device_type => ?USER},
					case add_device(DeviceInfo, OwnerDeviceInfo) of
						{error, _, _} = ErrDevice ->
							ErrDevice;
						NewDeviceInfo ->
							lager:info("NewDeviceInfo ~p ~n", [NewDeviceInfo]),
							ResponseData = lists:merge3(ResponseProcess, NewDeviceInfo, [{user_name, UserName}]),
							lager:info("ResponseData ~p ~n", [ResponseData]),
							AddRole = add_user_roles(UserId),
							lager:info("AddRole ~p ~n", [AddRole]),
							NewResponseData = lists:merge(ResponseData, AddRole),
							{NewResponseData, AccessToken}	
					end
			end;
		[#{status := ?ACCOUNT_STATUS_INACTIVE , id := UserId}] ->
			{error, 400, <<"Inactive User">>, [{id, UserId}]}
			;
		[] ->
			{error, 400, <<"Not found user">>, []}
	end,
	case ResultCheck of
		{error, ErrCode, ErrMsg, Data} ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_data/2, Data}
													,{fun cb_context:set_resp_error_msg/2, ErrMsg}
													,{fun cb_context:set_resp_status/2, error}
													,{fun cb_context:set_resp_error_code/2, ErrCode}]);

		{error, ErrCode, ErrMsg} ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, ErrMsg}
													,{fun cb_context:set_resp_status/2, error}
													,{fun cb_context:set_resp_error_code/2, ErrCode}])
			;
		{Res, AuthenToken} ->
			cb_context:setters(Context
												 ,[{fun cb_context:set_resp_status/2, 'success'}
													 ,{fun cb_context:set_resp_data/2, Res}
													 ,{fun cb_context:set_auth_token/2, AuthenToken}])
	end;

handle_post(Context, ?USER_FORGOT_PASSWORD) ->
	ReqJson = cb_context:req_json(Context),
	UserName = wh_json:get_value(<<"email">>, ReqJson, <<>>),
	CheckResult = 
	case user_db:find_users_by_email(UserName) of
		[#{status := AccountStatus}] = [UserDb] ->
			if
				AccountStatus == ?ACCOUNT_STATUS_ACTIVE ->
					RequesterId = maps:get(id, UserDb, <<>>),
					UpdatedTime = hwk_util:get_timestamp(),
					ConfirmCode = hwk_util:get_confirm_code(),
					UserInfo = maps:merge(UserDb, #{confirm_code => ConfirmCode
																					,updated_time => UpdatedTime
																					,updated_by => RequesterId
																					,confirm_code_created_time => UpdatedTime}),
					user_db:save_user(UserInfo),
					EmailInfo = #{id => RequesterId
												,confirm_code => ConfirmCode},
					hook_handle_pool:send_email(?EMAIL_FORGOT_PASSWORD, UserName, EmailInfo)
					;
				true ->
					{error,  403, <<"Account Is Inactive">>}
			end;
		[] ->
			{error, 404, <<"Not found user">>}
	end,
	case CheckResult of
		{error, ErrCode, ErrMsg} ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, ErrMsg}
													,{fun cb_context:set_resp_status/2, <<"error">>}
													,{fun cb_context:set_resp_error_code/2, ErrCode}])
			;
		_ ->
			cb_context:setters(Context
												 ,[{fun cb_context:set_resp_status/2, 'success'}])
	end;

handle_post(Context, ?USER_RESET_PASSWORD) ->
	ReqJson = cb_context:req_json(Context),
	Email = wh_json:get_value(<<"email">>, ReqJson, <<>>),
	Password = wh_json:get_value(<<"password">>, ReqJson, <<>>),
	ConfirmCode = wh_json:get_value(<<"confirm_code">>, ReqJson, <<>>),
	lager:info("Email ~p Password ~p ConfirmCode ~p ~n",[Email, Password, ConfirmCode]),
	case user_db:find_users_by_email(Email) of
		[#{id := UserId, confirm_code := ConfirmCodeServer
			 , confirm_code_created_time:= ConfirmCodeCreatedTimeServer}] = [UserInfo] ->
			UpdatedTime = hwk_util:now_to_utc_binary(os:timestamp()),
			CurrentTimeToSecond = hwk_util:timestamp_second(),		
			ConfirmCodeCreatedTimeServerToSecond = hwk_util:datetime_binary_to_second(ConfirmCodeCreatedTimeServer),
			if
				CurrentTimeToSecond - ConfirmCodeCreatedTimeServerToSecond > ?DATESECOND ->
					cb_context:setters(Context,
														 [{fun cb_context:set_resp_error_msg/2, <<"Code Exprired">>}
															,{fun cb_context:set_resp_status/2, <<"error">>}
															,{fun cb_context:set_resp_error_code/2, 400}]);
				ConfirmCode =:= ConfirmCodeServer ->
					{ok, Salt} = bcrypt:gen_salt(?WORKFACTOR),
					{ok, HashPass} = bcrypt:hashpw(Password, Salt),
					UpdatedUserInfo = maps:merge(UserInfo, #{password => hwk_util:to_bin(HashPass)
																									 , updated_time => UpdatedTime
																									 ,confirm_code => <<>>
																									 , confirm_code_created_time => UpdatedTime}),
					user_db:save_user(UpdatedUserInfo),
					access_token_db:del_access_token_by_account_id(UserId),
					cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}]);				
				true ->
					cb_context:setters(Context,
														 [{fun cb_context:set_resp_error_msg/2, <<"Invalid Code">>}
															,{fun cb_context:set_resp_status/2, <<"error">>}
															,{fun cb_context:set_resp_error_code/2, 400}])																									
			end
			;
		[] ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, <<"Not Found User">>}
													,{fun cb_context:set_resp_status/2, <<"error">>}
													,{fun cb_context:set_resp_error_code/2, 404}])	
	end
	;

handle_post(Context, UserId) ->
	ReqJson =  cb_context:req_json(Context),
	Role = cb_context:role(Context),
	RequesterId = cb_context:id(Context),
	case user_db:find_user(UserId) of
		#{status := ?ACCOUNT_STATUS_ACTIVE} = UserDb ->
			if	RequesterId == UserId ->
						ReqUserInfo = get_user_info(?UPDATE_ACCOUNT, ReqJson, RequesterId, UserDb),
						user_db:save_user(ReqUserInfo),
						RespData = get_sub_fields_user(ReqUserInfo),
						cb_context:setters(Context, [{fun cb_context:set_resp_data/2, RespData}
																				 ,{fun cb_context:set_resp_status/2, 'success'}]);
					true ->
						cb_context:setters(Context,
															 [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
																{fun cb_context:set_resp_status/2, <<"error">>},
																{fun cb_context:set_resp_error_code/2, 403}])
			end;
		_ ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, <<"User Not Found">>},
													{fun cb_context:set_resp_status/2, <<"error">>},
													{fun cb_context:set_resp_error_code/2, 404}])
	end.

%% GET api/v1/users
handle_get({Req, Context}) ->
	QueryJson = cb_context:query_string(Context),
	RequesterId = cb_context:id(Context),
	Limit = hwk_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
	Offset = hwk_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
	PropQueryJson = wh_json:to_proplist(QueryJson),
	UserInfos =  get_users(PropQueryJson, Limit, Offset),
	Response = get_sub_fields(UserInfos),
	{Req, cb_context:setters(Context
													 ,[{fun cb_context:set_resp_data/2, Response}
														 ,{fun cb_context:set_resp_status/2, 'success'}])}.
% {Req, cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
%                                    {fun cb_context:set_resp_status/2, 'error'},
%                                    {fun cb_context:set_resp_error_code/2, 403}])}

handle_get({Req, Context}, UserId) ->
	RequesterId = cb_context:id(Context),
	%lager:error("RequesterId ~p ~n", [RequesterId]),
	Role = cb_context:role(Context),
	if
		RequesterId == UserId ->
			case user_db:find_user(UserId) of
				UserDb when is_map(UserDb) ->
					Response = get_sub_fields(UserDb),
					{Req, cb_context:setters(Context
																	 ,[{fun cb_context:set_resp_data/2, Response}
																		 ,{fun cb_context:set_resp_status/2, 'success'}])};
				notfound ->
					{Req, cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, <<"Not found user">>},
																						 {fun cb_context:set_resp_status/2, 'error'},
																						 {fun cb_context:set_resp_error_code/2, 404}])}
			end;
		true ->
			{Req, cb_context:setters(Context, [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
																				 {fun cb_context:set_resp_status/2, 'error'},
																				 {fun cb_context:set_resp_error_code/2, 403}])}
	end.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_user_roles(UserId) ->
	lager:info("UserId ~p ", [UserId]),
	SysTemRoles = get_system_roles(UserId),
	%OrgRoles = get_org_roles(UserId),
	OrgRoles = get_roles(UserId),
	%GroupRoles = get_group_roles(UserId),
	[{system_roles, SysTemRoles}, {organization_roles, OrgRoles}].

get_system_roles(UserId) ->
	[].

get_roles(UserId) ->
	UserRoleDbs = user_role_db:find_by_conditions([{user_id, UserId}
																								 ,{status_id, ?USER_ROLE_STATUS_ACTIVE}]),
	get_org_roles(UserRoleDbs).

get_org_roles(UserRoles) ->
	Fun = fun(UserRole, Acc) ->
						Type = maps:get(type, UserRole, <<>>),
						lager:debug("Type ~p ~n", [Type]),
						ProcessUserRole = process_user_role_with_type(Type, UserRole),
						ProcessUserRole ++ Acc
				end,
	RawRoles = lists:foldr(Fun, [], UserRoles),
	lager:debug("RawRoles ~p ~n", [RawRoles]),
	OrgIdBranchRoles = lists:foldl(fun({K, V}, Acc) ->
																		 case lists:keytake(K, 1, Acc) of
																			 {value, {K, Vals}, NewAcc} ->
																				 [{K, [V | Vals]} | NewAcc];
																			 _ -> 
																				 [{K, [V]} | Acc]
																		 end
																 end, [], RawRoles),
	%lager:debug("OrgIdBranchRoles ~p ~n", [OrgIdBranchRoles]),
	FunAddOrg = fun(OrgIdBranchRole) ->
									lager:debug("OrgIdBranchRole ~p ~n", [OrgIdBranchRole]),
									handle_format_user_role(OrgIdBranchRole)	
							end,	
	lists:map(FunAddOrg, OrgIdBranchRoles).

handle_format_user_role({OrgId, OrgBranchPermissions}) ->
	%lager:info("OrgBranchPermissions ~p ~n", [OrgBranchPermissions]),
	Fun = 
	fun({V1, V2} = K, {Acc1, Acc2} = Acc) ->
			lager:info("K ~p; Acc ~p", [K, Acc]),
			VAcc1 = merge_org_permission([V1|Acc1]),
			VAcc2 = merge_branch_permission(VAcc1, [V2|Acc2]),
			lager:info("VAcc1 ~p; VAcc2 ~p", [VAcc1, VAcc2]),
			{VAcc1, VAcc2}
	end,
	{OrgPermissions, BranchPermissions} = lists:foldr(Fun, {[],[]}, OrgBranchPermissions),
	lager:info("OrgPermissions ~p ~n", [BranchPermissions]),
	%Set = sets:from_list(OrgBranchRoles),
	%sets:to_list(Set)
	RespOrgPermission = 
	case lists:member(?ALL_PERMISSIONS_FOR_ORG, OrgPermissions) of
		?TRUE ->
			?ALL_PERMISSIONS_FOR_ORG
			;
		_ ->
			OrgPermissions

	end,
	#{organization_id => OrgId
		,permissions => RespOrgPermission
		,branch_roles => BranchPermissions}.

merge_org_permission(Permissions) ->
	case lists:member(?ALL_PERMISSIONS_FOR_ORG, Permissions) of
		?TRUE ->
			[?ALL_PERMISSIONS_FOR_ORG]
			;
		_ ->
			lists:foldr(fun(V, Acc) -> V ++ Acc end, [], Permissions)
	end.

merge_branch_permission([?ALL_PERMISSIONS_FOR_ORG], _Permissions) ->
	[];

merge_branch_permission(_, Permissions) ->
	Permissions.

get_role_pair(UserRole) ->
	Roles = maps:get(roles, UserRole, []),
	BranchId = maps:get(branch_id, UserRole, <<>>),
	OrgId = maps:get(organization_id, UserRole, <<>>),
	PermissionRoles = get_permissions_with_roles(Roles),
	BranchRoles = #{branch_id => BranchId
									,permissions => PermissionRoles},
	[{OrgId, {[], BranchRoles}}].

process_user_role_with_type(?USER_ROLE_TYPE_ORG, UserRole) ->
	OrgId = maps:get(organization_id, UserRole, <<>>),	
	Roles = maps:get(roles, UserRole, []),
	PermissionRoles = case lists:member(?USER_ROLE_ADMIN_ORG, Roles) of
											?TRUE ->
												[?ALL_PERMISSIONS_FOR_ORG]
												;
											_ ->
												get_permissions_with_roles(Roles)
										end,
	[{OrgId, {PermissionRoles, #{}}}];

process_user_role_with_type(?USER_ROLE_TYPE_BRANCH, UserRole) ->
	lager:info("Type ~p", [?USER_ROLE_TYPE_BRANCH]),
	get_role_pair(UserRole)
	;
process_user_role_with_type(_, _UserRole) ->
	[].

get_permissions_with_roles([]) ->
	get_permissions([]);


get_permissions_with_roles(Roles) ->
	lager:debug("Roles ~p ~n", [Roles]),
	Fun = fun(Role, Acc) ->
						Permissions = get_permissions(Role),
						Permissions ++ Acc											
				end,
	PermissionRoles = lists:foldl(Fun, [], Roles),
	Set = sets:from_list(PermissionRoles),
	sets:to_list(Set).


get_permissions(Role) ->
	sbo_authorize:get_permissions(Role).


add_device([], _) ->
	[{device_id, <<>>}];

add_device(DeviceInfo, OwnerDeviceInfo) ->
	cb_device:add_device(DeviceInfo, OwnerDeviceInfo).

get_user_info(UserDb) when is_map(UserDb) ->
	Id = maps:get(id, UserDb, <<>>),
	FirstName = maps:get(first_name, UserDb, <<>>),
	LastName = maps:get(last_name, UserDb, <<>>),
	Email = maps:get(email, UserDb, <<>>),
	Avatar = maps:get(avatar, UserDb, <<>>),
	Address = maps:get(address, UserDb, <<>>),
	PhoneNumber = maps:get(phone_number, UserDb, <<>>),
	#{id => Id
		,first_name => FirstName
		,last_name => LastName
		,email => Email
		,phone_number => PhoneNumber
		,address => Address 
		,avatar => Avatar
		,type => ?USER}

	;

get_user_info([]) ->
	#{};

get_user_info([UserDb]) when is_map(UserDb) ->
	get_user_info(UserDb)
	;

get_user_info(UsersInfo) when is_list(UsersInfo) ->
	Fun = fun(UserDb, Acc) ->
						[get_user_info(UserDb)|Acc]					
				end,
	lists:foldr(Fun, [],  UsersInfo).


get_users_info(Users) ->
	lists:map(fun(User) -> get_user_info(User)
						end,Users).

get_user_profile(UserId) ->
	case user_db:find_user(UserId) of
		UserDb when is_map(UserDb) ->
			get_user_info(UserDb)
			;
		_ ->
			#{}
	end.

get_users_profile(UsersRole) ->
	UserIds = get_user_ids(UsersRole),
	Users = get_users_by_ids(UserIds),
	get_users_info(Users).

get_users_by_ids(UserId) when is_binary(UserId) ->
	get_users_by_ids([UserId])
	;

get_users_by_ids(UserIds) ->
	user_db:find_by_conditions([{id, 'in', UserIds}]).

get_user_ids(UserInfo) when is_map(UserInfo) ->
	maps:get(id, UserInfo, <<>>)
	;
get_user_ids(UsersInfo) when is_list(UsersInfo) ->
	Fun = fun(User, Acc) ->
						[get_user_ids(User)] ++ Acc
				end,
	lists:foldr(Fun, [], UsersInfo).

get_users(QueryJson, Limit, Offset) ->
	user_db:find_by_conditions([], [{<<"sort_created_time">>, desc}|QueryJson], Limit, Offset).

get_sub_fields(UserInfo) when is_map(UserInfo) ->
	Fields = [created_time, created_by
						,updated_time, updated_by
						,confirm_code, confirm_code_created_time
						,password, role],
	maps:without(Fields, UserInfo);

get_sub_fields(UserInfos) ->
	Fun = fun(UserInfo) ->
						get_sub_fields(UserInfo)						
				end,
	lists:map(Fun, UserInfos).

get_user_info(?CREATE_ACCOUNT, ReqJson, RequesterId) ->
	CreatedTime = hwk_util:now_to_utc_binary(os:timestamp()),
	Address = wh_json:get_value(<<"address">>, ReqJson, <<>>),
	Avatar = wh_json:get_value(<<"avatar">>, ReqJson, <<>>),
	PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson, <<>>),
	Password = wh_json:get_value(<<"password">>, ReqJson, <<>>),
	{ok, Salt} = bcrypt:gen_salt(?WORKFACTOR),
	{ok, HashPass} = bcrypt:hashpw(Password, Salt),
	#{address => Address,
		avatar => Avatar,
		phone_number => PhoneNumber,
		created_by => RequesterId,
		updated_by => RequesterId,
		created_time => CreatedTime,
		updated_time => CreatedTime,
		confirm_code_created_time => CreatedTime,
		role => ?USER_ROLE_USER,
		password => HashPass,
		status => ?ACCOUNT_STATUS_INACTIVE}.

get_user_info(?UPDATE_ACCOUNT, ReqJson, RequesterId, UserDb) ->
	UpdateTime =  hwk_util:now_to_utc_binary(os:timestamp()),
	PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson, maps:get(phone_number, UserDb, <<>>)),
	Address = wh_json:get_value(<<"address">>, ReqJson, maps:get(address, UserDb, <<>>)),
	Avatar = wh_json:get_value(<<"avatar">>, ReqJson, maps:get(avatar, UserDb, <<>>)),
	FirstName = wh_json:get_value(<<"first_name">>, ReqJson, maps:get(first_name, UserDb, <<>>)),
	LastName = wh_json:get_value(<<"last_name">>, ReqJson, maps:get(last_name, UserDb, <<>>)),
	maps:merge(UserDb, #{phone_number => PhoneNumber
											 ,address => Address
											 ,avatar => Avatar
											 ,updated_time => UpdateTime
											 ,updated_by => RequesterId
											 ,first_name => FirstName
											 ,last_name => LastName}).

get_sub_fields_user(User) ->
	Fields = [password
						,created_by
						,created_time
						,updated_by
						,updated_time
						,confirm_code
						,confirm_code_created_time],
	maps:without(Fields, User).
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
	validate_put_user(ReqJson, Context1);

validate_request(Context, _Verb) ->
	Context.

validate_request(_Id, Context, ?HTTP_GET) ->
	cb_context:setters(Context
										 ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(?USER_LOGIN, Context, ?HTTP_POST) ->
	Context1 = cb_context:setters(Context
																,[{fun cb_context:set_resp_status/2, 'success'}]),
	ReqJson = cb_context:req_json(Context),
	%GrantType = wh_json:get_value(<<"grant_type">>, ReqJson, <<>>),
	ValidateFuns = [fun validate_user_name/2
									,fun validate_password/2
									,fun validate_device_info/2],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context1,  ValidateFuns);

validate_request(?USER_FORGOT_PASSWORD, Context, ?HTTP_POST) ->
	Context1 = cb_context:setters(Context
																,[{fun cb_context:set_resp_status/2, 'success'}]),
	ReqJson = cb_context:req_json(Context),
	%GrantType = wh_json:get_value(<<"grant_type">>, ReqJson, <<>>),
	ValidateFuns = [fun validate_email_forgot_password/2],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context1,  ValidateFuns);

validate_request(?USER_RESET_PASSWORD, Context, ?HTTP_POST) ->
	Context1 = cb_context:setters(Context
																,[{fun cb_context:set_resp_status/2, 'success'}]),
	ReqJson = cb_context:req_json(Context),
	%GrantType = wh_json:get_value(<<"grant_type">>, ReqJson, <<>>),
	ValidateFuns = [fun validate_email_forgot_password/2,
									fun validate_password/2,
									fun validate_confirm_code/2],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context1,  ValidateFuns);

validate_request(_Id, Context, ?HTTP_POST) ->
	cb_context:setters(Context
										 ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_Id, Context, ?HTTP_DELETE) ->
	cb_context:setters(Context
										 ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_Id, Context, _Verb) ->
	Context.

validate_request(_Id, ?CONFIRM, Context, ?HTTP_POST) ->
	Context1 = cb_context:setters(Context
																,[{fun cb_context:set_resp_status/2, 'success'}]),
	ReqJson =  cb_context:req_json(Context1),
	ValidateFuns = [fun validate_confirm_code/2],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context1,  ValidateFuns);

validate_request(_Id, ?PASSWORD_CHANGE, Context, ?HTTP_POST) ->
	Context1 = cb_context:setters(Context
																,[{fun cb_context:set_resp_status/2, 'success'}]),
	ReqJson =  cb_context:req_json(Context1),
	ValidateFuns = [fun validate_curr_password/2
									,fun validate_new_password/2],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context1,  ValidateFuns);

validate_request(_Id, ?RESEND, Context, ?HTTP_POST) -> 
	cb_context:setters(Context
										 ,[{fun cb_context:set_resp_status/2, 'success'}]);				

validate_request(_Id, ?LOGOUT, Context, ?HTTP_POST) ->
	Context1 = cb_context:setters(Context
																,[{fun cb_context:set_resp_status/2, 'success'}]),
	ReqJson =  cb_context:req_json(Context1),
	ValidateFuns = [],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context1,  ValidateFuns).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Spec Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validate_put_user(ReqJson, Context) ->
	ValidateFuns = [
									fun validate_phone_number/2
									,fun validate_email/2
									,fun validate_first_name/2
									,fun validate_last_name/2
									,fun validate_password/2
								 ],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context,  ValidateFuns).


validate_user_name(ReqJson, Context) ->
	UserName = wh_json:get_value(<<"user_name">>, ReqJson, <<>>),
	case UserName of
		<<>> ->
			api_util:check_val(Context, <<"email">>, UserName);
		_->
			case re:run(hwk_util:to_str(UserName), ?EMAILREGX) of
				nomatch ->
					api_util:validate_error(Context, <<"email">>, <<"invalid">>, <<"Invalid Email">>);
				_->
					Context
			end
	end.

validate_first_name(ReqJson, Context) ->
	FirstName = wh_json:get_value(<<"first_name">>, ReqJson, <<>>),
	api_util:check_val(Context, <<"first_name">>, FirstName).

validate_last_name(ReqJson, Context) ->
	LastName = wh_json:get_value(<<"last_name">>, ReqJson, <<>>),
	api_util:check_val(Context, <<"last_name">>, LastName).

validate_phone_number(_CustomerId, Context) ->
	ReqJson = cb_context:req_json(Context),
	PhoneNumber = wh_json:get_value(<<"phone_number">>, ReqJson, <<>>),
	case PhoneNumber of
		<<>> ->
			api_util:check_val(Context, <<"phone_number">>, PhoneNumber);
		_  ->
			case re:run(hwk_util:to_str(PhoneNumber), ?PHONEREGX) of
				nomatch ->
					api_util:validate_error(Context, <<"phone_number">>, <<"invalid">>, <<"Invalid PhoneNumber">>);
				_ ->
					Context
			end
	end.


validate_email(ReqJson, Context) ->
	Email = wh_json:get_value(<<"email">>, ReqJson, <<>>),
	case Email of
		<<>> ->
			api_util:check_val(Context, <<"email">>, Email);
		_->
			case re:run(hwk_util:to_str(Email), ?EMAILREGX) of
				nomatch ->
					api_util:validate_error(Context, <<"email">>, <<"invalid">>, <<"Invalid Email">>);
				_->
					case is_user_exist(Email) of
						false -> Context;
						_ -> api_util:validate_error(Context, <<"email">>, <<"unique">>, <<"Email already in use">>)
					end
			end
	end.

validate_email_forgot_password(ReqJson, Context) ->
	Email = wh_json:get_value(<<"email">>, ReqJson, <<>>),
	case Email of
		<<>> ->
			api_util:check_val(Context, <<"email">>, Email);
		_->
			case re:run(hwk_util:to_str(Email), ?EMAILREGX) of
				nomatch ->
					api_util:validate_error(Context, <<"email">>, <<"invalid">>, <<"Invalid Email">>);
				_->
					Context
			end
	end.

validate_confirm_code(ReqJson, Context) ->
	ConfirmCode = wh_json:get_value(<<"confirm_code">>, ReqJson, <<>>),
	case ConfirmCode of
		<<>> ->
			api_util:validate_error(Context, <<"confirm_code">>, <<"required">>, <<"Field 'confirm_code' is required">>);
		_ ->
			Context
	end.

validate_password(ReqJson, Context) ->
	FieldPassword = <<"password">>,
	process_password(FieldPassword, ReqJson, Context).

validate_curr_password(ReqJson, Context) ->
	CurrPassword = wh_json:get_value(<<"current_password">>, ReqJson, <<>>),
	api_util:check_val(Context, <<"current_password">>, CurrPassword).

validate_new_password(ReqJson, Context) ->
	NewPassword = <<"new_password">>,
	process_password(NewPassword, ReqJson, Context).

validate_device_info(ReqJson, Context) ->
	DeviceInfo = wh_json:get_value(<<"device_info">>, ReqJson, []),
	lager:info("DeviceInfo ~p ~n",[DeviceInfo]),	
	if
		DeviceInfo == [] ->
			Context;
		true ->
			check_val_element_device_info(DeviceInfo, Context)
	end.

validate_push_id(DeviceInfo, Context) ->
	PushId = proplists:get_value(<<"push_id">>, DeviceInfo, <<>>),
	api_util:check_val(Context, <<"push_id">>, PushId).

validate_app_id(DeviceInfo, Context) ->
	AppId = proplists:get_value(<<"app_id">>, DeviceInfo, <<>>),
	api_util:check_val(Context, <<"app_id">>, AppId).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% extra function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec is_user_exist(binary()) ->  boolean().
is_user_exist(Email) ->
	lager:info("Email: ~p ~n", [Email]),
	case user_db:find_users_by_email(Email) of
		[UserDb] when is_map(UserDb) ->
			?TRUE ;
		[] ->
			?FALSE
	end.

-spec process_password(binary(), api_binary(), cb_context:context()) -> cb_context:context().
process_password(Field, ReqJson, Context) ->
	Password = wh_json:get_value(Field, ReqJson, <<>>),
	LenPass = length(hwk_util:to_str(Password)),
	case LenPass of
		0 ->
			api_util:validate_error(Context, Field, <<"required">>, <<"Field '",Field/binary,"' is required">>);
		Val when Val < 8 ->
			api_util:validate_error(Context, Field, <<"invalid">>, << "'", Field/binary,"'"," must have at least 8 characters">>);
		_ ->
			Context
	end.

check_val_element_device_info(DeviceInfo, Context) ->
	DeviceId = proplists:get_value(<<"device_id">>, DeviceInfo, <<>>),
	OsType = proplists:get_value(<<"os_type">>, DeviceInfo, <<>>),
	Context1 = cb_context:setters(Context
																,[{fun cb_context:set_resp_status/2, 'success'}]),
	if
		DeviceId == <<>> ->
			if
				OsType == <<>> ->
					api_util:check_val(Context, <<"os_type">>, OsType);
				OsType == ?IOS orelse OsType == ?ANDROID ->
					ValidateFuns = [
													fun validate_push_id/2
													,fun validate_app_id/2
												 ],
					lists:foldl(fun(F, C) ->
													F(DeviceInfo, C)
											end, Context1,  ValidateFuns);
				true ->
					api_util:validate_error(Context, <<"os_type">>, <<"invalid">>, <<"Field '", <<"os_type">>/binary, "' must be ios/android.">>)											
			end;
		true ->
			Context	
	end.
