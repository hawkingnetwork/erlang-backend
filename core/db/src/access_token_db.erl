-module(access_token_db).

%%% Access Token API.
-export([ new_access_token/1, save_access_token/1, 
		  del_access_token/1, del_access_token_by_token/1, del_token_by_refresh_token/1,
      del_all_access_tokens/0, del_access_token_by_account_id/1,
      find_access_token_by_token/1, find_all_access_tokens/0, 
		  find_access_token_by_account_id/1, find_access_token_by_account_id/3,
      find_all_access_tokens/2,reindex/0 ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Access Token API starts here.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec new_access_token({binary(), binary(), binary(), access_token_doc:context()}) -> access_token_doc:access_token_info().
new_access_token({AccessToken, RefreshToken, AccountId, Context, Role}) ->
  sumo:persist(access_token_doc, access_token_doc:new(AccessToken, RefreshToken, AccountId, Context, Role)).

-spec save_access_token(access_token_doc:access_token_info()) -> ok.
save_access_token(AccessToken) ->
  sumo:persist(access_token_doc, AccessToken).

-spec del_access_token(access_token_doc:access_token_info()) -> boolean().
del_access_token(AccessToken) ->
  sumo:delete(access_token_doc, access_token_doc:token(AccessToken)).

-spec del_access_token_by_token(binary()) -> boolean().
del_access_token_by_token(Token) ->
  sumo:delete(access_token_doc, Token).

-spec del_access_token_by_account_id(binary()) -> boolean().
del_access_token_by_account_id(AccountId) ->
  sumo:delete_by(access_token_doc, [{account_id, AccountId}]).

-spec del_token_by_refresh_token(binary()) -> boolean().
del_token_by_refresh_token(Token) ->
  sumo:delete_by(refresh_token_doc, [{refresh_token_id, Token}]).

-spec del_all_access_tokens() -> boolean().
del_all_access_tokens() ->
  sumo:delete_all(access_token_doc).

-spec find_access_token_by_token(binary()) -> access_token_doc:access_token_info()|notfound.
find_access_token_by_token(Token) ->
  sumo:find(access_token_doc, Token).

-spec find_access_token_by_account_id(binary()) -> access_token_doc:access_token_info()|notfound.
find_access_token_by_account_id(AccountId) ->
  sumo:find_by(access_token_doc, [{account_id, AccountId}]).

-spec find_access_token_by_account_id(binary(), non_neg_integer(), non_neg_integer()) -> 
                                                          access_token_doc:access_token_info()|notfound.
find_access_token_by_account_id(AccountId, Limit, Offset) ->
  sumo:find_by(access_token_doc, [{account_id, AccountId}], Limit, Offset).

-spec find_all_access_tokens() -> [access_token_doc:access_token_info()].
find_all_access_tokens() ->
  sumo:find_all(access_token_doc).

-spec find_all_access_tokens(non_neg_integer(), non_neg_integer()) -> [access_token_doc:access_token_info()].
find_all_access_tokens(Limit, Offset) ->
  sumo:find_all(access_token_doc, [], Limit, Offset).

reindex()->
  Docs = find_all_access_tokens(),
  reindex(Docs),
  ok.

reindex([]) ->
  finished;
  
reindex([H|T]) ->
  save_access_token(H),
  reindex(T).