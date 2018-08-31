%% Process manage broker connections

-module(wh_mqtt_connections).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).
-export([add_conn/1, 
		remove_conn/1, 
		get_conn/0,
		get_conns/0]).


-define(TAB, ?MODULE).

-record(mqtt_client, {conn, conn_ref}).

start_link() ->
    gen_server:start_link({'local', ?MODULE}, ?MODULE, [], []).

init([]) -> 
	ets:new(?TAB, ['named_table'
	                   ,'public'
	                   ,{'read_concurrency', 'true'}
	                  ]),
	{ok, #{}}.
	

handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast({new_connnection, Conn}, State) ->
	Ref = erlang:monitor('process', Conn),
	ets:insert(?TAB, {Conn, Ref}),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({'DOWN', Ref, 'process', Conn, _Reason}, State) ->
    lager:warning("connection ~p went down: ~p"
                  ,[Conn, _Reason]),
    erlang:demonitor(Ref, ['flush']),
    _ = ets:delete(?TAB, Conn),
    {'noreply', State, 'hibernate'};

handle_info(_Msg, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _State) ->
	ok. 

add_conn(ConnInfo) ->
	case wh_mqtt_connection_sup:add(ConnInfo) of 
	{ok, Conn} -> 
		gen_server:cast(?MODULE, {new_connnection, Conn});
	{'error', Reason} ->
        lager:warning("unable to start connection to ~p", [{'error', Reason}])
    end.

remove_conn(Conn) ->
	wh_mqtt_connections_sup:remove(Conn).

get_conn() ->
	ClientList = ets:tab2list(?TAB),
	case ClientList of 
	[] ->
		none ;
	_ ->
		{Conn, _ } = lists:nth(erlang:phash(os:timestamp(), length(ClientList)), ClientList) ,
		Conn 
	end. 

get_conns() ->
	lists:map(fun({Conn, _}) ->
		Conn 
	end, ets:tab2list(?TAB)). 
