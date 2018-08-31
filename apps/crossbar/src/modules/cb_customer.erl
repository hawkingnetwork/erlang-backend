-module(cb_customer).

-include("../crossbar.hrl").
-include("sbo.hrl").

-define(RESIDENT_LOGIN,<<"login">>).
-define(USERNAME_TYPE_PHONE, <<"phone">>).
-define(USERNAME_TYPE_EMAIL, <<"email">>).
-define(CONFIRM_CODE, <<"confirm_code">>).
-define(RESIDENT_RESEND_CONFIRM_CODE, <<"resend">>).
-define(RESIDENT_FORGOT_PASSWORD, <<"forgot">>).
-define(RESIDENT_RESET_PASSWORD, <<"reset">>).

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
				 ,handle_post/2
				 ,handle_post/3
				]).

-export([get_profile_resident/1
				 ,existed_residents/1]).

init() ->
	_ = crossbar_bindings:bind(<<"*.allowed_methods.customers">>, ?MODULE, 'allowed_methods'),
	_ = crossbar_bindings:bind(<<"*.resource_exists.customers">>, ?MODULE, 'resource_exists'),
	_ = crossbar_bindings:bind(<<"*.validate.customers">>, ?MODULE, 'validate'),
	_ = crossbar_bindings:bind(<<"*.authenticate.customers">>, ?MODULE, 'authenticate'),
	_ = crossbar_bindings:bind(<<"*.to_json.get.customers">>, ?MODULE, 'handle_get'),
	_ = crossbar_bindings:bind(<<"*.execute.post.customers">>, ?MODULE, 'handle_post'),
	_ = crossbar_bindings:bind(<<"*.execute.put.customers">>, ?MODULE, 'handle_put').

% This is a function that tells what methods are allowed for an end point
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), ne_binary()) -> http_methods().

%% /api/v1/residents
allowed_methods() ->
	[?HTTP_GET, ?HTTP_PUT].

%% /api/v1/residents/{resident_id}
allowed_methods(_ResidentId) ->
	[?HTTP_GET, ?HTTP_POST].

%% /api/v1/residents/{resident_id}/path
allowed_methods(_ResidentId, _Path) ->
	[?HTTP_POST].

-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), ne_binary()) -> boolean().

%% /api/v1/residents
resource_exists() -> 'true'.

%% /api/v1/residents/{resident_id}
resource_exists(_ResidentId) -> 'true'.

%% /api/v1/residents/path
% resource_exists(_ResidentId, ?PATH_) -> 'true';
% resource_exists(_ResidentId, ?PATH_) -> 'true';
resource_exists(_ResidentId, ?CONFIRM) -> 'true';
resource_exists(_ResidentId, ?LOGOUT) -> 'true';
resource_exists(_ResidentId, ?RESIDENT_RESEND_CONFIRM_CODE) -> 'true';
resource_exists(_ResidentId, _Path) -> 'false'.

%% /api/v1/residents
-spec authenticate(cb_context:context()) -> boolean().
-spec authenticate(cb_context:context(), path_token()) -> boolean().

authenticate(Context) ->
	authenticate_with_method(Context, cb_context:req_verb(Context)).
authenticate_with_method(_Context, ?HTTP_PUT) -> true;

authenticate_with_method(Context, ?HTTP_GET) ->
	Token = cb_context:auth_token(Context),
	hwk_util:oauth2_authentic(Token, Context).

%% /api/v1/residents/{resident_id}
authenticate(_Context, ?LOGIN) -> 'true';
authenticate(_Context, ?RESIDENT_FORGOT_PASSWORD) -> 'true';
authenticate(_Context, ?RESIDENT_RESET_PASSWORD) -> 'true';
authenticate(Context, _ResidentId) ->
	Token = cb_context:auth_token(Context),
	hwk_util:oauth2_authentic(Token, Context).

%% /api/v1/residents/{resident_id}/path
authenticate(_Context, _ResidentId, ?CONFIRM) -> 'true';
authenticate(_Context, _ResidentId, ?RESIDENT_RESEND_CONFIRM_CODE) -> 'true';
authenticate(Context, _ResidentId, ?PASSWORD_CHANGE) ->
	Token = cb_context:auth_token(Context),
	hwk_util:oauth2_authentic(Token, Context);
authenticate(Context, _ResidentId, ?LOGOUT) ->
	Token = cb_context:auth_token(Context),
	hwk_util:oauth2_authentic(Token, Context);
authenticate(_Context, _ResidentId, _Path) -> false.

-spec validate(cb_context:context()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token(), path_token()) -> cb_context:context().

%% Validate resource : /api/v1/residents
validate(Context) ->
	validate_request(Context, cb_context:req_verb(Context)).

%% Validate resource : /api/v1/residents/{resident_id}
validate(Context, Id) ->
	validate_request(Id, Context, cb_context:req_verb(Context)).

%% Validate resource : /api/v1/residents/{resident_id}/path
validate(Context, Id, Path) ->
	validate_request(Id, Path, Context, cb_context:req_verb(Context)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Service APIs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% PUT api/v1/residents
-spec handle_put(cb_context:context()) -> cb_context:context().
handle_put(Context) ->
	ReqJson = cb_context:req_json(Context),
	Id = hwk_util:get_uuid(),
	ConfirmCode = hwk_util:get_confirm_code(),
	UserName = wh_json:get_value(<<"user_name">>, ReqJson, <<>>),
	FirstName = wh_json:get_value(<<"first_name">>, ReqJson, <<>>),
	LastName = wh_json:get_value(<<"last_name">>, ReqJson, <<>>),
	case check_user_name_type(UserName) of
		{error, Msg} ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, Msg},
													{fun cb_context:set_resp_status/2, 'error'},
													{fun cb_context:set_resp_error_code/2, 400}]);
		UserNameType ->
			ReqResident = get_resident_info(ReqJson, Id, ConfirmCode),
			ResidentInfo = maps:merge(ReqResident, #{id => Id
																							 ,confirm_code => ConfirmCode
																							 ,user_name => UserName
																							 ,first_name => FirstName
																							 ,last_name => LastName }),
			customer_db:save(ResidentInfo),
			%if UserNameType == ?USERNAME_TYPE_PHONE ->
			%   hook_handle_pool:send_sms(<<"+84938536588">>, UserNameType, ConfirmCode);
			% UserNameType == ?USERNAME_TYPE_EMAIL ->
			   EmailInfo = #{
			    id => Id
			    ,confirm_code => ConfirmCode
			    ,first_name => FirstName
			    ,last_name => LastName
			    },
			hook_handle_pool:send_email(?EMAIL_CONFIRM_CODE, Context, EmailInfo),
			% end,
			cb_context:setters(Context
												 ,[{fun cb_context:set_resp_data/2, [{id, Id}, {confirm_code, ConfirmCode}]}
													 ,{fun cb_context:set_resp_status/2, 'success'}])

	end.

%% POST api/v1/residents/login
-spec handle_post(cb_context:context(), path_token()) -> cb_context:context().
handle_post(Context, ?RESIDENT_LOGIN) ->
	ReqJson = cb_context:req_json(Context),
	UserName = wh_json:get_value(<<"user_name">>, ReqJson, <<>>),
	DeviceInfo = wh_json:get_value(<<"device_info">>, ReqJson, []),
	ResidentDbs = customer_db:find_by_user_name(UserName),
	ResultCheck = 
	case ResidentDbs of
		[#{id := ResidentId
			 ,status := ?ACCOUNT_STATUS_ACTIVE}] ->
			ProcessPass = cb_auth:process_password_grant(?RESIDENT_SCOPE, Context, ReqJson),
			lager:info("ProcessPass ~p ~n", [ProcessPass]),
			case ProcessPass of
				{error, _, _} = ErrProcessPass ->
					ErrProcessPass;
				{success, ResponseProcess, AccessToken} ->
					OwnerDeviceInfo = #{owner_device_id => ResidentId
															,owner_device_type => ?RESIDENT},
					case check_device_info(DeviceInfo, OwnerDeviceInfo) of
						{error, _, _} = ErrDevice ->
							ErrDevice;
						NewDeviceInfo ->
							lager:info("NewDeviceInfo ~p ~n", [NewDeviceInfo]),
							NewResponseData = lists:merge3(ResponseProcess, NewDeviceInfo, [{user_name, UserName}]),
							lager:info("NewResponseData ~p ~n", [NewResponseData]),
							{NewResponseData, AccessToken}	
					end
			end;
		[#{status := ?ACCOUNT_STATUS_INACTIVE , id := ResidentId}] ->
			{error, 400, <<"Inactive Resident">>, [{id, ResidentId}]}
			;
		[] ->
			{error, 404, <<"Not found resident">>, []}
	end,
	case ResultCheck of
		{error, ErrCode, ErrMsg, Data} ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_data/2, Data}
													,{fun cb_context:set_resp_error_msg/2, ErrMsg}
													,{fun cb_context:set_resp_status/2, error}
													,{fun cb_context:set_resp_error_code/2, ErrCode}])
			;
		{error, ErrCode, ErrMsg} ->
			cb_context:setters(Context,[{fun cb_context:set_resp_error_msg/2, ErrMsg}
																	,{fun cb_context:set_resp_status/2, error}
																	,{fun cb_context:set_resp_error_code/2, ErrCode}])
			;
		{Res, AuthenToken} ->
			cb_context:setters(Context
												 ,[{fun cb_context:set_resp_status/2, 'success'}
													 ,{fun cb_context:set_resp_data/2, Res}
													 ,{fun cb_context:set_auth_token/2, AuthenToken}])
	end;

handle_post(Context, ?RESIDENT_FORGOT_PASSWORD) ->
	ReqJson = cb_context:req_json(Context),
	UserName = wh_json:get_value(<<"user_name">>, ReqJson, <<>>),
	ConfirmCode = hwk_util:get_confirm_code(),
	UpdatedTime = hwk_util:get_timestamp(),
	case customer_db:find_residents_by_user_name(UserName) of
		[#{id := ResidentId, account_status := AccountStatus} = ResidentInfo | _] ->
			if
				AccountStatus == ?ACCOUNT_STATUS_ACTIVE ->
					UserNameType = check_user_name_type(UserName),
					SendResetPass = send_reset_password(UserNameType, ResidentInfo, ConfirmCode),
					case SendResetPass of						
						{error, Err} ->
							cb_context:setters(Context,
																 [{fun cb_context:set_resp_error_msg/2, Err}
																	,{fun cb_context:set_resp_status/2, <<"error">>}
																	,{fun cb_context:set_resp_error_code/2, 400}]);
						_->
							UpdatedResidentInfo = maps:merge(ResidentInfo, #{confirm_code => ConfirmCode
																															 ,updated_time =>	UpdatedTime
																															 ,updated_by => ResidentId
																															 ,confirm_code_created_time => UpdatedTime}),
							customer_db:save(UpdatedResidentInfo),
							cb_context:setters(Context,[{fun cb_context:set_resp_status/2, 'success'}])																					
					end;
				AccountStatus == ?ACCOUNT_STATUS_INACTIVE ->
					cb_context:setters(Context,
														 [{fun cb_context:set_resp_error_msg/2, <<"user_name inactive">>}
															,{fun cb_context:set_resp_status/2, <<"error">>}
															,{fun cb_context:set_resp_error_code/2, 400}]
														);	
				true ->
					cb_context:setters(Context,
														 [{fun cb_context:set_resp_error_msg/2, <<"Bad Request">>}
															,{fun cb_context:set_resp_status/2, <<"error">>}
															,{fun cb_context:set_resp_error_code/2, 400}]
														)														
			end;
		[] ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, <<"user_name not found">>}
													,{fun cb_context:set_resp_status/2, <<"error">>}
													,{fun cb_context:set_resp_error_code/2, 404}]
												)	
	end;

handle_post(Context, ?RESIDENT_RESET_PASSWORD) ->
	ReqJson = cb_context:req_json(Context),
	Username = wh_json:get_value(<<"user_name">>, ReqJson, <<>>),
	Password = wh_json:get_value(<<"password">>, ReqJson, <<>>),
	ConfirmCode = wh_json:get_value(<<"confirm_code">>, ReqJson, <<>>),
	lager:info("Username ~p Password ~p ConfirmCode ~p ~n",[Username, Password, ConfirmCode]),
	case customer_db:find_residents_by_user_name(Username) of
		[#{id := ResidentId, confirm_code := ConfirmCodeServer
			 , confirm_code_created_time:= ConfirmCodeCreatedTimeServer}] = [ResidentInfo] ->
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
					UpdatedResidentInfo = maps:merge(ResidentInfo, #{password => hwk_util:to_bin(HashPass)
																													 , updated_time => UpdatedTime
																													 ,	confirm_code => <<>>
																													 , confirm_code_created_time => UpdatedTime}),
					customer_db:save(UpdatedResidentInfo),
					access_token_db:del_access_token_by_account_id(ResidentId),
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
												 [{fun cb_context:set_resp_error_msg/2, <<"Not Found Resident">>}
													,{fun cb_context:set_resp_status/2, <<"error">>}
													,{fun cb_context:set_resp_error_code/2, 404}])	
	end
	;

handle_post(Context, ResidentId) ->
	ReqJson = cb_context:req_json(Context),
	ResidentIdToken = cb_context:id(Context),
	Resp = 
	if ResidentId == ResidentIdToken ->
			 case customer_db:find_resident(ResidentId) of
				 ResidentDb when is_map(ResidentDb) ->
					 update_resident(ReqJson,ResidentId, ResidentDb)
					 ;
				 notfound ->
					 {error, 404, <<"Not found resident">>}
					 ;
				 ErrResidentDb ->
					 lager:error("ErrResidentDb ~p ~n", [ErrResidentDb]),
					 {error, 400, <<"Error resident database">>}
			 end
			 ;
		 true ->
			 {error, 403, <<"Forbidden">>}
	end,
	case Resp of
		{error, ErrCode, ErrMsg} ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, ErrMsg},
													{fun cb_context:set_resp_status/2, 'error'},
													{fun cb_context:set_resp_error_code/2, ErrCode}])

			;
		ResidentInfo when is_map(ResidentInfo) ->
			cb_context:setters(Context,[{fun cb_context:set_resp_data/2, ResidentInfo}
																	,{fun cb_context:set_resp_status/2, 'success'}])

	end.

-spec handle_post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
handle_post(Context, ResidentId, ?CONFIRM) ->
	ReqJson = cb_context:req_json(Context),
	ConfirmCode = wh_json:get_value(<<"confirm_code">>, ReqJson, <<>>),
	case customer_db:find_resident(ResidentId) of
		#{account_status := ?ACCOUNT_STATUS_ACTIVE} ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, <<"Account Is Active">>},
													{fun cb_context:set_resp_status/2, <<"error">>},
													{fun cb_context:set_resp_error_code/2, 400}
												 ]);
		#{account_status := ?ACCOUNT_STATUS_INACTIVE
			,confirm_code := ConfirmCode} = ResidentDb ->
			ConfirmCodeCreatedTime = maps:get(confirm_code_created_time, ResidentDb),
			CurrentTimeToSecond = hwk_util:timestamp_second(),
			ConfirmCodeCreatedTimeToSecond = hwk_util:datetime_binary_to_second(ConfirmCodeCreatedTime),
			if
				CurrentTimeToSecond - ConfirmCodeCreatedTimeToSecond > ?DATESECOND ->
					cb_context:setters(Context,
														 [{fun cb_context:set_resp_error_msg/2, <<"Code Exprired">>},
															{fun cb_context:set_resp_status/2, <<"error">>},
															{fun cb_context:set_resp_error_code/2, 400}
														 ]);
				true ->
					UpdatedTime = hwk_util:now_to_utc_binary(os:timestamp()),
					UpdatedResidentDb = maps:merge(ResidentDb, #{account_status => ?ACCOUNT_STATUS_ACTIVE,
																											 updated_time => UpdatedTime,
																											 updated_by => ResidentId}),
					customer_db:save(UpdatedResidentDb),
					cb_context:setters(Context, [{fun cb_context:set_resp_status/2, 'success'}])

			end;
		#{account_status := ?ACCOUNT_STATUS_INACTIVE
			,confirm_code := ConfirmCodeDb} ->
			lager:info("ConfirmCode ~p; ConfirmCodeDb ~p ~n", [ConfirmCode, ConfirmCodeDb]),
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, <<"Invalid Code">>},
													{fun cb_context:set_resp_status/2, <<"error">>},
													{fun cb_context:set_resp_error_code/2, 400}
												 ]);		

		notfound ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, <<"Residents Not Found">>},
													{fun cb_context:set_resp_status/2, <<"error">>},
													{fun cb_context:set_resp_error_code/2, 404}
												 ]);

		Err ->
			lager:error("Err ~p ~n", [Err]),
			throw(dberror)
	end;

handle_post(Context, ResidentId, ?RESIDENT_RESEND_CONFIRM_CODE) ->
	ReqJson = cb_context:req_json(Context),
	ConfirmCode = hwk_util:get_confirm_code(),
	UpdatedTime = hwk_util:now_to_utc_binary(os:timestamp()),
	case customer_db:find_resident(ResidentId) of
		#{account_status := AccountStatus} = ResidentInfo ->
			if
				AccountStatus == ?ACCOUNT_STATUS_INACTIVE ->
					UpdateInfo = #{confirm_code => ConfirmCode
												 ,updated_time => UpdatedTime
												 ,updated_by => ResidentId 
												 ,confirm_code_created_time => UpdatedTime},
					customer_db:save(maps:merge(ResidentInfo, UpdateInfo)),
					cb_context:setters(Context
														 ,[{fun cb_context:set_resp_data/2, [{id, ResidentId},
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
												 [{fun cb_context:set_resp_error_msg/2, <<"Account Not Found">>},
													{fun cb_context:set_resp_status/2, <<"error">>},
													{fun cb_context:set_resp_error_code/2, 404}])	
	end;

handle_post(Context, ResidentId, ?PASSWORD_CHANGE) ->
	ResidentIdToken = cb_context:id(Context),
	case customer_db:find_resident(ResidentId) of
		#{id := ResidentIdDb, password := CurrPassHashServer} = ResidentDb ->
			if 	ResidentId == ResidentIdToken ->
						ReqJson = cb_context:req_json(Context),
						CurrPass = wh_json:get_value(<<"current_password">>, ReqJson),
						NewPass = wh_json:get_value(<<"new_password">>, ReqJson),
						PassHash = hwk_util:to_str(CurrPassHashServer),
						{ok, ProvidedHash} = bcrypt:hashpw(CurrPass, PassHash),
						if  ProvidedHash == PassHash ->
									UpdatedTime = hwk_util:now_to_utc_binary(os:timestamp()),
									{ok, Salt} = bcrypt:gen_salt(?WORKFACTOR),
									{ok, NewPassHash} = bcrypt:hashpw(NewPass, Salt),
									UpdatedResidentDb = maps:merge(ResidentDb, #{password => hwk_util:to_bin(NewPassHash),
																															 updated_time =>  UpdatedTime,
																															 updated_by =>  ResidentId}),
									customer_db:save(UpdatedResidentDb),
									access_token_db:del_access_token_by_account_id(ResidentId),
									RespData = [{resident_id, ResidentId}],
									cb_context:setters(Context,[{fun cb_context:set_resp_data/2, RespData},
																							{fun cb_context:set_resp_status/2,'success'}]);
								true ->
									cb_context:setters(Context,
																		 [{fun cb_context:set_resp_error_msg/2, <<"Invalid Current Password">>},
																			{fun cb_context:set_resp_status/2, <<"error">>},
																			{fun cb_context:set_resp_error_code/2, 400}])

						end;
					true ->
						cb_context:setters(Context,
															 [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
																{fun cb_context:set_resp_status/2, <<"error">>},
																{fun cb_context:set_resp_error_code/2, 403}])
			end;
		_ ->
			cb_context:setters(Context,
												 [{fun cb_context:set_resp_error_msg/2, <<"Resident Not Found">>},
													{fun cb_context:set_resp_status/2, <<"error">>},
													{fun cb_context:set_resp_error_code/2, 404}])
	end;
handle_post(Context, ResidentId, ?LOGOUT) ->
	ResidentIdToken = cb_context:id(Context),
	lager:info("ResidentId: ~p ~n",[ResidentId]),
	ReqJson =  cb_context:req_json(Context),
	DeviceId = wh_json:get_value(<<"device_id">>, ReqJson, <<>>),
	lager:info("device_id: ~p ~n",[DeviceId]),
	Token = cb_context:auth_token(Context),
	if  ResidentId == ResidentIdToken ->
				case customer_db:find_resident(ResidentId) of
					ResidentDb when is_map(ResidentDb) ->
						case device_db:find_device(DeviceId) of
							DeviceInfo when is_map(DeviceInfo) ->
								device_db:save_device(maps:merge(DeviceInfo, #{status => ?INACTIVE})),
								access_token_db:del_access_token_by_token(Token),
								%refresh_token_db:del_refresh_token_by_account_id(ResidentId),
								RespData = [{residentId, ResidentId}],
								cb_context:setters(Context ,[{fun cb_context:set_resp_data/2, RespData},
																						 {fun cb_context:set_resp_status/2, 'success'}]);
							notfound -> 
								cb_context:setters(Context,
																	 [{fun cb_context:set_resp_error_msg/2, <<"Device Not Found">>},
																		{fun cb_context:set_resp_status/2, <<"error">>},
																		{fun cb_context:set_resp_error_code/2, 404}])		
						end;
					notfound ->
						cb_context:setters(Context,
															 [{fun cb_context:set_resp_error_msg/2, <<"Resident Not Found">>},
																{fun cb_context:set_resp_status/2, <<"error">>},
																{fun cb_context:set_resp_error_code/2, 404}])	
				end;				
			true ->
				cb_context:setters(Context,
													 [{fun cb_context:set_resp_error_msg/2, <<"Forbidden">>},
														{fun cb_context:set_resp_status/2, <<"error">>},
														{fun cb_context:set_resp_error_code/2, 403}])
	end.

%% GET api/v1/residents
handle_get({Req, Context}) ->
	QueryJson = cb_context:query_string(Context),
	Limit = hwk_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)),
	Offset = hwk_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)),
	PropQueryJson = wh_json:to_proplist(QueryJson),
	%  if
	% Role == ?USER_ROLE_USER ->
	Residents =  get_residents(PropQueryJson, Limit, Offset),
	Response = get_sub_fields(Residents),
	{Req, cb_context:setters(Context
													 ,[{fun cb_context:set_resp_data/2, Response}
														 ,{fun cb_context:set_resp_status/2, 'success'}])}.

%% GET api/v1/residents/{residents}
handle_get({Req, Context}, ResidentId) ->
	RequesterId = cb_context:id(Context),
	lager:info("RequesterId: ~p ~n",[RequesterId]),
	case customer_db:find_resident(ResidentId) of
		ResidentDb when is_map(ResidentDb) ->
			ResidentInfo = get_sub_fields(ResidentDb),
			{Req, cb_context:setters(Context
															 ,[{fun cb_context:set_resp_data/2, ResidentInfo}
																 ,{fun cb_context:set_resp_status/2, 'success'}])};
		notfound ->
			{Req, cb_context:setters(Context,
															 [{fun cb_context:set_resp_error_msg/2, <<"Not found resident">>},
																{fun cb_context:set_resp_status/2, 'error'},
																{fun cb_context:set_resp_error_code/2, 400}])}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
existed_residents(ResidentIds) ->
	ResidentsDb = customer_db:find_residents_by_ids(ResidentIds),
	if length(ResidentIds) == length(ResidentsDb) ->
			 []
			 ;
		 ?TRUE ->
			 ResidentIds
	end	.


get_resident_info(ResidentDb) when is_map(ResidentDb) ->
	Id = maps:get(id, ResidentDb, <<>>),
	FirstName = maps:get(first_name, ResidentDb, <<>>),
	LastName = maps:get(last_name, ResidentDb, <<>>),
	Email = maps:get(email, ResidentDb, <<>>),
	Avatar = maps:get(avatar, ResidentDb, <<>>),
	Address = maps:get(address, ResidentDb, <<>>),
	PhoneNumber = maps:get(phone_number, ResidentDb, <<>>),
	#{id => Id
		,type => ?RESIDENT
		,first_name => FirstName
		,last_name => LastName
		,email => Email
		,phone_number => PhoneNumber
		,address => Address 
		,avatar => Avatar}
	;

get_resident_info(notfound) ->
	#{}.

get_profile_resident(#{id := ResidentId}) ->
	ResidentInfo = customer_db:find_resident(ResidentId),
	get_resident_info(ResidentInfo);

get_profile_resident(ResidentId) ->
	ResidentInfo = customer_db:find_resident(ResidentId),
	get_resident_info(ResidentInfo).

get_residents(QueryJson, Limit, Offset) ->
	customer_db:find_by_conditions([], [{<<"sort_created_time">>, desc}|QueryJson], Limit, Offset).

get_sub_fields(Resident) when is_map(Resident) ->
	Fields = [created_time, created_by, updated_time, updated_by,
						confirm_code, confirm_code_created_time, password, status],
	maps:without(Fields, Resident)
	;

get_sub_fields(Residents) ->
	Fun = fun(Resident) ->
						get_sub_fields(Resident)
				end,
	lists:map(Fun, Residents).

get_resident_info(ReqJson, RequesterId, ConfirmCode) ->
	CreatedTime = hwk_util:now_to_utc_binary(os:timestamp()),
	Password = wh_json:get_value(<<"password">>, ReqJson),
	{ok, Salt} = bcrypt:gen_salt(?WORKFACTOR),
	{ok, HashPass} = bcrypt:hashpw(Password, Salt),
	#{
																						 created_by => RequesterId,
																						 updated_by => RequesterId,
																						 created_time => CreatedTime,
																						 updated_time => CreatedTime,
																						 password => hwk_util:to_bin(HashPass),
																						 status => ?ACCOUNT_STATUS_INACTIVE,
																						 confirm_code => ConfirmCode,
																						 confirm_code_created_time => CreatedTime}.



check_device_info(DeviceInfo, OwnerDeviceInfo) ->
	cb_device:add_device(DeviceInfo, OwnerDeviceInfo).


update_resident(ReqJson, RequesterId, ResidentDb) ->
	UpdatedTime = hwk_util:now_to_utc_binary(os:timestamp()),
	FirstName = wh_json:get_value(<<"first_name">>, ReqJson, maps:get(first_name, ResidentDb, <<>>)),
	LastName = wh_json:get_value(<<"last_name">>, ReqJson, maps:get(last_name, ResidentDb, <<>>)),
	Avatar = wh_json:get_value(<<"avatar">>, ReqJson, maps:get(avatar, ResidentDb, <<>>)),
	UpdatedResidentDb = maps:merge(ResidentDb, #{first_name => FirstName,
																							 last_name => LastName,
																							 avatar => Avatar,
																							 updated_time => UpdatedTime,
																							 updated_by => RequesterId}),
	customer_db:save(UpdatedResidentDb),
	UpdatedResidentDb.

send_reset_password(?USERNAME_TYPE_PHONE, ResidentInfo, ConfirmCode) ->
	UserName = maps:get(user_name, ResidentInfo, <<>>),
	Msg = <<"Hi ",UserName/binary,",\r\n\r\nCode to reset your password: ", ConfirmCode/binary>>,
	hook_handle_pool:send_sms(UserName, Msg),
	{send_sms, Msg};	

send_reset_password(?USERNAME_TYPE_EMAIL, ResidentInfo, ConfirmCode)	->
	UserName = maps:get(user_name, ResidentInfo, <<>>),
	ResidentId = maps:get(id, ResidentInfo, <<>>),
	EmailInfo = #{id => ResidentId, confirm_code => ConfirmCode},
	hook_handle_pool:send_email(?SEND_EMAIL_TO_RESIDENT, UserName, EmailInfo),
	{send_email, EmailInfo};	

send_reset_password(_Type, _ResidentInfo, _ConfirmCode)	->
	{error, <<"User Name Type Invalid">>}.

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
	validate_put_resident(ReqJson, Context1);
validate_request(Context, _Verb) ->
	Context.

validate_request(_Id, Context, ?HTTP_GET) ->
	cb_context:setters(Context
										 ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(?LOGIN, Context, ?HTTP_POST) ->
	Context1 = cb_context:setters(Context
																,[{fun cb_context:set_resp_status/2, 'success'}]),
	ReqJson =  cb_context:req_json(Context1),
	ValidateFuns = [
									fun validate_user_name/2
									,fun validate_password/2
									,fun validate_device_info/2	
								 ],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context1,  ValidateFuns);

validate_request(?RESIDENT_FORGOT_PASSWORD, Context, ?HTTP_POST) ->
	Context1 = cb_context:setters(Context
																,[{fun cb_context:set_resp_status/2, 'success'}]),
	ReqJson =  cb_context:req_json(Context1),
	ValidateFuns = [
									fun validate_user_name_forgot_password/2
								 ],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context1,  ValidateFuns);					

validate_request(?RESIDENT_RESET_PASSWORD, Context, ?HTTP_POST) ->
	Context1 = cb_context:setters(Context
																,[{fun cb_context:set_resp_status/2, 'success'}]),
	ReqJson =  cb_context:req_json(Context1),
	ValidateFuns = [
									fun validate_user_name_forgot_password/2,
									fun validate_password/2,
									fun validate_confirm_code/2		
								 ],
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
	ValidateFuns = [fun validate_confirm_code/2
								 ],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context1,  ValidateFuns);

validate_request(_Id, ?RESIDENT_RESEND_CONFIRM_CODE, Context, ?HTTP_POST) ->
	cb_context:setters(Context
										 ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(_Id, ?PASSWORD_CHANGE, Context, ?HTTP_POST) ->
	Context1 = cb_context:setters(Context
																,[{fun cb_context:set_resp_status/2, 'success'}]),
	ReqJson =  cb_context:req_json(Context1),
	ValidateFuns = [fun validate_curr_password/2,
									fun validate_new_password/2],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context1,  ValidateFuns);
validate_request(_Id, ?LOGOUT, Context, ?HTTP_POST) ->
	cb_context:setters(Context
										 ,[{fun cb_context:set_resp_status/2, 'success'}]).

validate_first_name(ReqJson, Context) ->
	FirstName = wh_json:get_value(<<"first_name">>, ReqJson, <<>>),
	api_util:check_val(Context, <<"first_name">>, FirstName).

validate_last_name(ReqJson, Context) ->
	LastName = wh_json:get_value(<<"last_name">>, ReqJson, <<>>),
	api_util:check_val(Context, <<"last_name">>, LastName).

validate_put_resident(ReqJson, Context) ->
	ValidateFuns = [
									fun validate_first_name/2
									,fun validate_last_name/2
									,fun validate_user_info/2
								 ],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context,  ValidateFuns).

validate_confirm_code(ReqJson, Context) ->
	ConfirmCode = wh_json:get_value(<<"confirm_code">>, ReqJson, <<>>),
	api_util:check_val(Context, <<"confirm_code">>, ConfirmCode).

validate_curr_password(ReqJson, Context) ->
	FieldPassword = <<"current_password">>,
	process_password(FieldPassword, ReqJson, Context).

validate_new_password(ReqJson, Context) ->
	FieldPassword = <<"new_password">>,
	process_password(FieldPassword, ReqJson, Context).

validate_device_info(ReqJson, Context) ->
	DeviceInfo = wh_json:get_value(<<"device_info">>, ReqJson, []),
	lager:info("DeviceInfo ~p ~n",[DeviceInfo]),	
	if
		DeviceInfo == [] ->
			api_util:check_arr_val(Context, <<"device_info">>, DeviceInfo);
		true ->
			check_val_element_device_info(DeviceInfo, Context)
	end.

validate_user_name_forgot_password(ReqJson, Context) ->
	UserName = wh_json:get_value(<<"user_name">>, ReqJson, <<>>),
	CheckUserNameType = check_user_name_type(UserName),
	lager:info("CheckUserNameType ~p ~n",[CheckUserNameType]),
	case CheckUserNameType of
		?USERNAME_TYPE_EMAIL ->	Context;
		?USERNAME_TYPE_PHONE -> Context;
		{error, ErrMsg} ->
			api_util:validate_error(Context, <<"user_name">>, <<"unique">>, ErrMsg)
	end.
% if
% 	CheckUserNameType == ?USERNAME_TYPE_EMAIL
% 	orelse CheckUserNameType == ?USERNAME_TYPE_PHONE ->
% 		Context;
% 	CheckUserNameType == {error, Err} ->
% 		api_util:validate_error(Context, <<"user_name">>, <<"unique">>, Err/binary);		
% 	true ->
% 		api_util:validate_error(Context, <<"user_name">>, <<"unique">>, <<"Bad Request">>)
% end.
% validate_push_id(DeviceInfo, Context) ->
% PushId = proplists:get_value(<<"push_id">>, DeviceInfo, <<>>),
% api_util:check_val(Context, <<"push_id">>, PushId).

validate_app_id(DeviceInfo, Context) ->
	AppId = proplists:get_value(<<"app_id">>, DeviceInfo, <<>>),
	api_util:check_val(Context, <<"app_id">>, AppId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% extra function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validate_user_info(ReqJson, Context) ->
	case check_user_info(ReqJson, Context) of
		?EXISTED ->
			api_util:validate_error(Context, <<"user_name">>, <<"unique">>, <<"user_name already in use">>);
		?NOTFOUND ->
			Context;
		Err ->
			Err
	end.

validate_password(ReqJson, Context) ->
	process_password(<<"password">>, ReqJson, Context).

validate_user_name(ReqJson, Context) ->
	Password = wh_json:get_value(<<"user_name">>, ReqJson, <<>>),
	api_util:check_val(Context, <<"user_name">>, Password).

validate_id_card(ReqJson, Context) ->
	IdCard = wh_json:get_value(<<"id_card">>, ReqJson, <<>>),
	api_util:check_val(Context, <<"id_card">>, IdCard).

check_user_info(ReqJson, Context) ->
	Username = wh_json:get_value(<<"user_name">>, ReqJson, <<>>),
	case check_query(Context, ReqJson) of
		Context ->
			ResidentDbs = customer_db:find_by_conditions([{user_name_id, Username}], []),
			lager:info("ResidentDbs ~p ~n", [ResidentDbs]),
			case ResidentDbs of
				[ResidentDb] when is_map(ResidentDb) ->
					?EXISTED;
				[] ->
					?NOTFOUND;
				Error ->
					lager:error("User Can't Sign Up.Username ~p; ErrorDb: ~p ~n", [Username, Error]),
					throw(dberror)
			end;
		Err ->
			lager:error("Err ~p ~n", [Err]),
			Err

	end.

check_query(Context, ReqJson) ->
	ValidateFuns = [
									fun validate_id_card/2
									,fun validate_user_name/2
									,fun validate_password/2
								 ],
	lists:foldl(fun(F, C) ->
									F(ReqJson, C)
							end, Context,  ValidateFuns).

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

check_user_name_type(UserName) ->
	case re:run(hwk_util:to_str(UserName), ?PHONEREGX) of
		nomatch ->
			case re:run(hwk_util:to_str(UserName), ?EMAILREGX) of
				nomatch ->
					{error, <<"Invalid UserName">>};
				{match, _} ->
					?USERNAME_TYPE_EMAIL
			end;
		{match, _} ->
			?USERNAME_TYPE_PHONE
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
													%fun validate_push_id/2
													fun validate_app_id/2
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
