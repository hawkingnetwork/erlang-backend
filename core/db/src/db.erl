-module(db).
-export([start_link/0]).

%%% Exports for gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {}).

-type state() :: #state{}.

%%  Create index search and tie to bucket
start_link() ->
  lager:info("emnvn enter start_link function ",[]),
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	lager:info("emnvn enter init function ",[]),
	 Backends = application:get_env(sumo_db, storage_backends, []),
	 Schema = application:get_env(sumo_db, schema, []),
	 lager:info("Schemas: ~p ~n",[Schema]),
	case Backends of 
	[{_Name, _Module, Options}|_] ->

		Host = proplists:get_value(host, Options, "127.0.0.1"),
		Port = proplists:get_value(port, Options, 8087),
		Opts = riak_opts(Options),
		{ok, Conn} = riakc_pb_socket:start_link(Host, Port, Opts),
		lager:info("Conn: ~p ~n",[Conn]),
		SchemaName =
		case Schema of 
			[] ->
				<<"_yz_default">> ;
			Info -> 
				Name = proplists:get_value(name, Info, <<>>),
				File = proplists:get_value(file, Info, <<>>),
				lager:info("SchemaName:~p ~n; SchemaFile: ~p ~n",[Name, File]),
				{ok, SchemaData} = file:read_file(File),
				riakc_pb_socket:create_search_schema(Conn, Name, SchemaData),
				Name 
		end, 
		{ok, Stores} = application:get_env(sumo_db, stores),
		lists:foreach(fun({_StoreName, _StoreModule, StoreOpts}) ->
			BucketType = iolist_to_binary(
				proplists:get_value(bucket_type, StoreOpts, <<"sbo">>)),
			Bucket = iolist_to_binary(
				proplists:get_value(bucket, StoreOpts, <<"sumo">>)),
			Index = iolist_to_binary(
				proplists:get_value(index, StoreOpts, <<"sumo_index">>)),
			lager:info("emnvn index: ~p Bucket: ~p Bucket Type: ~p ~n",[Index, Bucket, BucketType]),

			%riakc_pb_socket:set_bucket(Conn, {BucketType, Bucket}, [{last_write_wins, true}]),
			riakc_pb_socket:create_search_index(Conn, Index, SchemaName, infinity),
			Res = riakc_pb_socket:set_search_index(Conn, {BucketType, Bucket}, Index), %% tie index search to bucket
			lager:info("emnvn create index: ~p Result: ~p ~n",[Index, Res])
		end, Stores);
		% IndexList = riakc_pb_socket:list_search_indexes(Conn),
		% lager:info("Data: ~p ~n",[IndexList]);
	_ ->
		ok
	end,
	{ok, noreply}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unused Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call(_Msg, _From, State) -> {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Msg, State) -> {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.



riak_opts(Options) ->
  User = proplists:get_value(username, Options),
  Pass = proplists:get_value(password, Options),
  Opts0 = case User /= undefined andalso Pass /= undefined of
            true -> [{credentials, User, Pass}];
            _    -> []
          end,
  Opts1 = case lists:keyfind(connect_timeout, 1, Options) of
            {_, V1} -> [{connect_timeout, V1}, {auto_reconnect, true}] ++ Opts0;
            _       -> [{auto_reconnect, true}] ++ Opts0
          end,
  Opts1.