%%%-------------------------------------------------------------------
%%% @copyright (C) 2015-2020, Zentech INC
%%% @doc
%%% API test demo
%%% @end
%%% @contributors
%%%   emnguyen@zentech.io
%%%-------------------------------------------------------------------

-module(cb_demo).

-include("../crossbar.hrl").

-export([init/0
	,validate/1
	,validate/2
	,resource_exists/0
	,resource_exists/1
	,authenticate/1
	,authenticate/2
	,allowed_methods/0
	,allowed_methods/1
	,handle_get/1
	,handle_get/2
	,handle_put/1
	,handle_post/2
	,handle_delete/2
	]).

init() ->
    lager:info("huet: registerd: ~n",[]),
	_ = crossbar_bindings:bind(<<"*.resource_exists.demos">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.demos">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.authenticate.demos">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.demos">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.to_json.get.demos">>, ?MODULE, 'handle_get'),
    _ = crossbar_bindings:bind(<<"*.execute.post.demos">>, ?MODULE, 'handle_post'),
	_ = crossbar_bindings:bind(<<"*.execute.put.demos">>, ?MODULE, 'handle_put'),
	_ = crossbar_bindings:bind(<<"*.execute.delete.demos">>, ?MODULE, 'handle_delete').


%% /api/v1/demos
allowed_methods() ->
	[?HTTP_PUT, ?HTTP_GET].

%% /api/v1/demos/{id}
allowed_methods(_Id) ->
	[?HTTP_POST, ?HTTP_GET, ?HTTP_DELETE].
	
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.

%% /api/v1/demos
resource_exists() -> 'true'.

%% /api/v1/demos/{id}
resource_exists(_Id) -> 'true'.

-spec authenticate(cb_context:context()) -> boolean().
-spec authenticate(cb_context:context(), path_token()) -> boolean().
authenticate(_Context) -> 
	%Token = cb_context:auth_token(Context),
	true.
	%sbo_util:oauth2_authentic(Token,Context).
	
authenticate(_Context, _Id) -> 
	%Token = cb_context:auth_token(Context),
	true.

-spec validate(cb_context:context()) ->  cb_context:context().
-spec validate(cb_context:context(), path_token()) ->  cb_context:context().

%% Validate resource : /api/v1/demos
validate(Context) ->
    validate_request(Context, cb_context:req_verb(Context)).

%% Validate resource : /api/v1/demos/{id}
validate(Context, Id) ->
    validate_request(Id, Context, cb_context:req_verb(Context)).   


handle_get({Req, Context}) ->
	%Sample get code from query_string
	QueryJson = cb_context:query_string(Context),
	lager:debug("Query Json: ~p ~n",[QueryJson]),
	Resp = #{
		limit => sbo_util:to_integer(wh_json:get_value(<<"limit">>, QueryJson, ?DEFAULT_LIMIT)), 
		offset => sbo_util:to_integer(wh_json:get_value(<<"offset">>, QueryJson, ?DEFAULT_OFFSET)), 
		mesage => <<"Demo GET list Worked!">>,
		query_string_params => wh_json:to_proplist(QueryJson)
	},
	Context1 = cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, Resp}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                        ]),
	{Req, Context1}.

handle_get({Req, Context},Id) ->
	Context1 = cb_context:setters(Context
                       ,[{fun cb_context:set_resp_data/2, <<"Demo GET by Id worked! ID:",Id/binary>>}
                         ,{fun cb_context:set_resp_status/2, 'success'}
                        ]),
	{Req, Context1}.
	
handle_put(Context) ->
	%Sample get code from request param
	ReqJson = cb_context:req_json(Context),
	Resp = #{
		sample_param => wh_json:get_value(<<"param1">>, ReqJson, <<"Default value">>), 
		mesage => <<"Demo PUT method worked!">>,
		request_params => wh_json:to_proplist(ReqJson)
	},
	cb_context:setters(Context, [{fun cb_context:set_resp_data/2, Resp}
										,{fun cb_context:set_resp_status/2, 'success'}
										]).

handle_post(Context, Id) ->
	cb_context:setters(Context, [{fun cb_context:set_resp_data/2, <<"Demo POST method worked! ID: ",Id/binary>>}
										,{fun cb_context:set_resp_status/2, 'success'}
										]).

handle_delete(Context, Id) ->
	cb_context:setters(Context, [{fun cb_context:set_resp_data/2, <<"Demo DELETE method worked! ID: ",Id/binary>>}
										,{fun cb_context:set_resp_status/2, 'success'}
										]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal Function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

					   
validate_request(Context, ?HTTP_GET) ->
	cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}]);

validate_request(Context, ?HTTP_PUT) ->
	cb_context:setters(Context
                       ,[{fun cb_context:set_resp_status/2, 'success'}]);

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
