-module(refresh_token_doc).

-behaviour(sumo_doc).

-export([sumo_schema/0, sumo_sleep/1, sumo_wakeup/1]).
-export([new/5, token/1, context/1]).

-type scope()    :: list(binary()) | binary().

-opaque context_type() ::
#{
    client => term(),
    resource_owner => term(),
    expiry_time_i => binary(),
    scope => scope()
 }.


-opaque refresh_token_info() ::
  #{
    token	=> binary(),
    access_token	=> binary(),
    account_id => binary(),
	  context => context_type(),
    role => binary()
   }.

-type context() :: context_type().



-export_type([refresh_token_info/0, context/0, context_type/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo behavior follows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Part of the sumo_doc behavior.
-spec sumo_wakeup(sumo:doc()) -> refresh_token_info().
sumo_wakeup(Doc) ->
	#{
    	token   => maps:get(token_id, Doc, <<>>),
    	access_token   => maps:get(access_token_id, Doc, <<>>),
      account_id => maps:get(account_id, Doc, <<>>),
    	context => maps:get(context, Doc, #{}),
      role => maps:get(role, Doc, <<>>)
    }.

%% @doc Part of the sumo_doc behavior.
-spec sumo_sleep(refresh_token_info()) -> sumo:doc().
sumo_sleep(RefreshToken) ->
    #{
      token_id   => maps:get(token, RefreshToken, <<>>),
      access_token_id   => maps:get(access_token, RefreshToken, <<>>),
      account_id => maps:get(account_id, RefreshToken, <<>>),
      context => maps:get(context, RefreshToken, #{}),
      role => maps:get(role, RefreshToken, <<>>)
    }.

-spec sumo_schema() -> sumo:schema().
sumo_schema() ->
  sumo:new_schema(?MODULE, [
  	sumo:new_field(token_id, string, [not_null, id]),
  	sumo:new_field(access_token_id, string, [not_null, id]),
    sumo:new_field(account_id, string, [not_null]),
    sumo:new_field(context, context_type, [ not_null]),
    sumo:new_field(role, string)
  ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Public API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Returns a new refresh token  (internal).
-spec new(binary(),binary(),binary(), context_type(), binary()) -> refresh_token_info().
new(RefreshToken, AccessToken, AccountId, Context, Role) ->
	#{
    	token	=> RefreshToken,
    	access_token	=> AccessToken,
      account_id => AccountId,
    	context => Context,
      role => Role
	}.

%% @doc Returns token  of the refresh token.
token(#{token := Token})  ->
   Token.

%% @doc Returns context  of the refresh token.
-spec context(refresh_token_info()) -> context().
context(#{context := Context}) ->
  Context.
