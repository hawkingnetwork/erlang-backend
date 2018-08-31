-module(oauth2_hwk_backend).

-behavior(oauth2_backend).

-include("sbo.hrl").

%%% API
-export([
				 start/0
				 ,stop/0
				]).

%%% Behavior API
-export([authenticate_user/2]).
-export([authenticate_client/2]).
-export([get_client_identity/2]).
-export([associate_access_code/3]).
-export([associate_refresh_token/3]).
-export([associate_access_token/3]).
-export([resolve_access_code/2]).
-export([resolve_refresh_token/2]).
-export([resolve_access_token/2]).
-export([revoke_access_code/2]).
-export([revoke_access_token/2]).
-export([revoke_refresh_token/2]).
-export([get_redirection_uri/1]).
-export([verify_redirection_uri/3]).
-export([verify_client_scope/3]).
-export([verify_resowner_scope/3]).
-export([verify_scope/3]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
	ok.
stop() ->
	ok.

%%%===================================================================
%%% OAuth2 backend functions
%%%===================================================================
%% There are two kind of scope of authenticate
%% First scope : ?USER_ROLE_CUSTOMER : Customer of a restaurant
%% Second scope: ?USER_ROLE_USER : user of restor - indicate admin of a restaurant
%% Default scope : ?USER_ROLE_CUSTOMER
authenticate_user(User, Ctx) ->
	Scope = proplists:get_value(scope, Ctx, ?USER_SCOPE),
	if Scope == ?USER_SCOPE ->
			 authenticate_scope_user(User, Ctx) ;
		 true ->
			 authenticate_scope_customer(User, Ctx)
	end.

authenticate_scope_user({Username, Password}, _Ctx) ->
	case user_db:find_users_by_email(Username) of
		[#{id := UserId, password := SecretKey}] ->
			PassHash = sbo_util:to_str(SecretKey),
			{ok, ProvidedHash} = bcrypt:hashpw(Password, PassHash),
			case PassHash == ProvidedHash of
				true ->
					NewCtx = [{<<"id">>, UserId}],
					{ok, {NewCtx, Username}};
				_  ->
					{error, badpass}
			end;
		_ ->
			{ok, {{error, notfound}, Username}}
	end.

authenticate_scope_customer({UserName, Password}, _Ctx) ->
	%lager:info("UserName ~p ~n", [UserName]),
	case resident_db:find_by_conditions([{user_name_id, UserName}], []) of
		[#{id := ResidentId, password := SecretKey}] ->
			PassHash = sbo_util:to_str(SecretKey),
			{ok, ProvidedHash} = bcrypt:hashpw(Password, PassHash),
			case PassHash == ProvidedHash of
				?TRUE ->
					NewCtx = [{<<"id">>, ResidentId}],
					{ok, {NewCtx, UserName}};
				_ ->
					{error, badpass}
			end;
		_->
			{ok, {{error, notfound}, UserName}}
	end.

authenticate_client({ClientKey, _ClientSecret}, Ctx) ->
	{ok, {Ctx, ClientKey}};

authenticate_client(Client, Ctx) ->
	{ok, {Ctx, Client}}.

get_client_identity(ClientKey, _) ->
	case db:find_client(ClientKey) of
		#{client_key := ClientKey} -> {ok, {<<"client">>, ClientKey}};

		notfound -> {error, notfound}
	end.

associate_access_code(AccessCode, Context, _AppContext) ->
	associate_access_token(AccessCode, Context, _AppContext).

associate_refresh_token(RefreshToken, Context, Ctx) ->
	AccessToken = proplists:get_value(<<"access_token">>, Ctx, <<>>),
	Id = proplists:get_value(<<"id">>, Ctx, <<>>),
	MapContext= maps:from_list(Context),
	refresh_token_db:new_refresh_token(RefreshToken, AccessToken, Id, MapContext, <<>>),
	{ok, Ctx}.

associate_access_token(AccessToken, Context, Ctx) ->
	RefreshToken = proplists:get_value(<<"refresh_token">>, Ctx, <<>>),
	Id = proplists:get_value(<<"id">>, Ctx, <<>>),
	MapContext= maps:from_list(Context),
	access_token_db:new_access_token({AccessToken, RefreshToken, Id, MapContext, <<>>}),
	{ok, lists:merge(Ctx, [{<<"access_token">>, AccessToken}])}.

resolve_access_code(AccessCode, _AppContext) ->
	resolve_access_token(AccessCode, _AppContext).

resolve_refresh_token(RefreshToken, Ctx) ->
	case refresh_token_db:find_refresh_token(RefreshToken) of
		#{account_id := AccountId, context := Context} ->
			RequesterId = proplists:get_value(<<"id">>, Ctx, <<>>),
			if RequesterId == AccountId ->
					 ListContext= lists:map(fun
																		({expiry_time =K, V}) -> {to_binary(K), to_integer(V)} ;
				({K, V}) -> {to_binary(K), V}
																	end, maps:to_list(Context)),
					 {ok, {[{<<"id">>, RequesterId}], ListContext}};
				 true -> {error, invalid}
			end;
		_ ->
			{error, invalid}
	end.

resolve_access_token(AccessToken, Ctx) ->
	%% The case trickery is just here to make sure that
	%% we don't propagate errors that cannot be legally
	%% returned from this function according to the spec.
	%case db:find_access_token_by_token(AccessToken) of
	case access_token_db:find_access_token_by_token(AccessToken) of
		#{context := Context, account_id := AccountId, role := Role} ->
			ListContext=
			lists:map(fun({expiry_time =K, V}) -> {to_binary(K), to_integer(V)} ;
									 ({K, V}) -> {to_binary(K), V}
								end, maps:to_list(Context)),
			Resp = [{<<"id">>, AccountId}, {<<"role">>, Role} | ListContext],
			{ok, {Ctx, Resp}};
		_ -> {error, notfound}
	end.

revoke_access_code(AccessCode, _AppContext) ->
	revoke_access_token(AccessCode, _AppContext).

revoke_access_token(AccessToken, _) ->
	access_token_db:del_access_token_by_token(AccessToken).

revoke_refresh_token(RefreshToken, _) ->
	refresh_token_db:del_refresh_token_by_token(RefreshToken).


get_redirection_uri(ClientKey) ->
	case db:find_client(ClientKey) of
		#{redirect_uri := RedirectUri} -> {ok, RedirectUri};
		notfound -> {error, notfound}
	end.

verify_redirection_uri(ClientKey, ClientUri, Ctx) ->
	case get_redirection_uri(ClientKey) of
		{ok, RedirectUri} when ClientUri =:= RedirectUri ->
			{ok, Ctx};
		_ ->
			{error, mismatch}
	end.

verify_client_scope(_ClientId, Scope, Ctx) ->
	{ok, {Ctx, Scope}}.

verify_resowner_scope(_ResOwner, Scope, Ctx) ->
	{ok, {Ctx,Scope}}.

verify_scope(Scope, Scope, Ctx) ->
	{ok, {Ctx, Scope}};
verify_scope(_, _, _) ->
	{error, invalid_scope}.

to_binary(Val) when is_atom(Val) ->
	list_to_binary(atom_to_list(Val));

to_binary(Val) when is_list(Val) ->
	list_to_binary(Val);

to_binary(Val)  when is_integer(Val) ->
	list_to_binary(integer_to_list(Val));

to_binary(Val) ->
	Val.


to_integer(Val) when is_binary(Val) ->
	binary_to_integer(Val) ;

to_integer(Val) when is_list(Val) ->
	list_to_integer(Val) ;

to_integer(Val)  ->
	Val.
