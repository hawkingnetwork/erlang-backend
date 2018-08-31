-module(refresh_token_db).

%%% Refresh Token API.
-export([ new_refresh_token/5, save_refresh_token/1,
		  del_refresh_token/1, del_refresh_token_by_token/1, del_refresh_token_by_refresh_token/1,
		  del_all_refresh_tokens/0, del_refresh_token_by_account_id/1,
      find_refresh_token/1, find_all_refresh_tokens/0, find_refresh_token_by_account_id/1,
		  find_all_refresh_tokens/2,reindex/0 ]).


-spec new_refresh_token(binary(), binary(), binary(), refresh_token_doc:context(), binary()) -> refresh_token_doc:refresh_token().
new_refresh_token(RefreshToken, AccessToken, AccountId, Context, Role) ->
  sumo:persist(refresh_token_doc, refresh_token_doc:new(RefreshToken, AccessToken, AccountId, Context, Role)).

-spec save_refresh_token(refresh_token_doc:refresh_token()) -> ok.
save_refresh_token(RefreshToken) ->
  sumo:persist(refresh_token_doc, RefreshToken).

-spec del_refresh_token(refresh_token_doc:refresh_token()) -> boolean().
del_refresh_token(RefreshToken) ->
  sumo:delete(refresh_token_doc, refresh_token_doc:token(RefreshToken)).

-spec del_refresh_token_by_refresh_token(binary()) -> boolean().
del_refresh_token_by_refresh_token(Token) ->
  sumo:delete(refresh_token_doc, Token).

-spec del_refresh_token_by_account_id(binary()) -> boolean().
del_refresh_token_by_account_id(AccountId) ->
	sumo:delete_by(refresh_token_doc, [{account_id,AccountId}]).

-spec del_refresh_token_by_token(binary()) -> boolean().
del_refresh_token_by_token(Token) ->
  sumo:delete_by(refresh_token_doc, [{access_token_id, Token}]).

-spec del_all_refresh_tokens() -> boolean().
del_all_refresh_tokens() ->
  sumo:delete_all(refresh_token_doc).

-spec find_refresh_token(binary()) -> refresh_token_doc:refresh_token()|notfound.
find_refresh_token(Token) ->
  sumo:find(refresh_token_doc, Token).

-spec find_all_refresh_tokens() -> [refresh_token_doc:refresh_token()].
find_all_refresh_tokens() ->
  sumo:find_all(refresh_token_doc).

-spec find_refresh_token_by_account_id(binary()) -> [refresh_token_doc:refresh_token()].
find_refresh_token_by_account_id(AccountId) ->
	sumo:find_by(refresh_token_doc, [{account_id, AccountId}]).

-spec find_all_refresh_tokens(non_neg_integer(), non_neg_integer()) -> [refresh_token_doc:refresh_token()].
find_all_refresh_tokens(Limit, Offset) ->
  sumo:find_all(refresh_token_doc, [], Limit, Offset).


reindex()->
  Docs = find_all_refresh_tokens(),
  reindex(Docs),
  ok.

reindex([]) ->
  finished;

reindex([H|T]) ->
  save_refresh_token(H),
  reindex(T).
