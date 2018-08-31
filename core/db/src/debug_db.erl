-module(debug_db).
-export([get_conn/0, reset_indexes/1, reset_index/2
		,list_indexes/1,get_index/2, list_buckets/2
		,reset_bucket/2,reset_buckets/2, get_bucket/2
        ,search/5, search/4]).

get_conn() ->
	{ok, Backends} = application:get_env(sumo_db, storage_backends),
	[{Name, _Module, _Options}] =  Backends,
	sumo_backend_riak:get_connection(Name).

reset_indexes(Conn) ->
    %% clear indexes
    {ok, Indexes} = riakc_pb_socket:list_search_indexes(Conn),
    [ riakc_pb_socket:delete_search_index(Conn, proplists:get_value(index,Index)) || Index <- Indexes ],
    wait_until( fun() ->
        {ok, []} == riakc_pb_socket:list_search_indexes(Conn)
    end, 50, 1000),
    ok.

reset_index(Conn, Index) ->
	riakc_pb_socket:delete_search_index(Conn, Index).

list_indexes(Conn) ->
	riakc_pb_socket:list_search_indexes(Conn).

get_index(Conn, Index) ->
	riakc_pb_socket:get_search_index(Conn, Index).

reset_bucket(Conn, TypeAndBucket) ->
	riakc_pb_socket:reset_bucket(Conn, TypeAndBucket).

reset_buckets(Conn, Type) ->
	Bs = riakc_pb_socket:list_buckets(Conn, Type),
	lists:foreach(fun(B) ->
		riakc_pb_socket:reset_bucket(Conn, B)
	end, Bs).

list_buckets(Conn, Type) ->
	riakc_pb_socket:list_buckets(Conn, Type).

get_bucket(Conn, TypeAndBucket) ->
	riakc_pb_socket:get_bucket(Conn, TypeAndBucket).

search(Conn, Index, Start, Limit, FieldSort) -> 
    riakc_pb_socket:search(Conn, Index, <<"*:*">>, [{start, Start}, {rows, Limit}, {sort, FieldSort}]).

search(Conn, Index, Start, Limit) -> 
    riakc_pb_socket:search(Conn, Index, <<"*:*">>, [{start, Start}, {rows, Limit}]).

wait_until(Fun) when is_function(Fun) ->
    wait_until(Fun, 20, 500).
wait_until(_, 0, _) ->
    fail;
wait_until(Fun, Retry, Delay) when Retry > 0 ->
    Pass = Fun(),
    case Pass of
        true ->
            ok;
        _ ->
            timer:sleep(Delay),
            wait_until(Fun, Retry-1, Delay)
    end.

