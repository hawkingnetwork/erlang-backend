-module(wh_mqtt_connection).

-behaviour(gen_server).

-export([start_link/0]).

-export([init/1
				 ,handle_call/3
				 ,handle_cast/2
				 ,handle_info/2
				 ,terminate/2
				 ,code_change/3
				]).
-export([start_pool/2, statistic/0, pool_name/0, get_mqtt_conn/0]).
-export([subscribe/1, subscribe/2, publish/2, publish/3, unsubscribe/2]).
-export([connect/1, reconnect/1, disconnected/1]).

-record(mqtt_connection, {mqttc, available = false, 
						info,
						monitor_ref,
						pqueue = queue:new(), 
						squeue = queue:new()}).

% start_link(ConnInfo) ->
% 	gen_server:start_link(?MODULE, [ConnInfo], []).

start_pool(PoolSize, ConnInfo) ->
	ConnectionWorkerOPts = [{workers, PoolSize},
						{worker, { ?MODULE, [ConnInfo]}}],
	supervisor:start_child(wh_mqtt_connection_sup,
							{pool_sup_ind(), 
							{ wpool, start_pool, [pool_name(), ConnectionWorkerOPts]}, 
							transient, 5000, supervisor, [ wpool ]}).

start_link() ->
	init().

init() ->
	Options = application:get_env(wh_mqtt, mqtt, []),
	PoolSize = proplists:get_value(poolsize, Options, 100),
	ConnInfo = get_conn_info(Options),
	ConnectionWorkerOPts    = [ {overrun_warning, 10000}
					, {overrun_handler, {sumo_internal, report_overrun}}
					, {workers, PoolSize}
					, {pool_sup_shutdown, 'infinity'}
					, {pool_sup_intensity, 10}
					, {pool_sup_period, 10}
					, {worker, {?MODULE, [undefined, ConnInfo]}}],
	wpool:start_pool(pool_name(), ConnectionWorkerOPts).

init([undefined, ConnInfo]) ->
	process_flag(trap_exit, true),
	{registered_name, RegisteredName} = process_info(self(), registered_name),
	Name = mqtt_client_name(RegisteredName),
	ClientId = atom_to_binary(Name, utf8),
	case connect([{client_id, ClientId} | ConnInfo]) of 
	{ok, C} ->
		Ref = erlang:monitor(process, C),
		{ok, #mqtt_connection{mqttc = C, info = ConnInfo, monitor_ref = Ref}};
	_ ->
		lager:error("MqttClient: Fail To connect to Broker",[]),
		{stop, "Fail To Conn To Mqtt Broker"}
	end. 	


handle_call({subscribe, Topic}, _From, #mqtt_connection{available = false, squeue = SQueue} = State) ->
	%% mqtt connection not ready to subscribe, put subscribe to queue
	{reply, pending, State#mqtt_connection{squeue = queue:in({subscribe, Topic}, SQueue)}};

handle_call({subscribe, Topic, QOS}, _From, #mqtt_connection{available = false, squeue = SQueue} = State) ->
	%% mqtt connection not ready to subscribe, put subscribe to queue
	{reply, pending, State#mqtt_connection{squeue = queue:in({subscribe, Topic, QOS}, SQueue)}};

handle_call({subscribe, Topic}, _From, #mqtt_connection{mqttc = C, available = true} = State) ->
	emqttc:subscribe(C, Topic),
	{reply, C, State};

handle_call({subscribe, Topic, QOS}, _From, #mqtt_connection{mqttc = C, available = true} = State) ->
	emqttc:subscribe(C, Topic, QOS),
	{reply, C, State};

handle_call(get_conn, _From, #mqtt_connection{mqttc = C, available = Status} = State) ->
	{reply, {C, Status, self()}, State};

handle_call(_Msg, _From, State) ->
	{reply, ok, State}.

handle_cast({publish, Topic, Payload}, #mqtt_connection{available = false, pqueue = PQueue} = State) ->
	%% Mqtt not ready to publish, put publish to queue
	{noreply, State#mqtt_connection{pqueue = queue:in({Topic, Payload}, PQueue)}};

handle_cast({publish, Topic, Payload, QOS}, #mqtt_connection{available = false, pqueue = PQueue} = State) ->
	%% Mqtt not ready to publish, put publish to queue
	{noreply, State#mqtt_connection{pqueue = queue:in({Topic, Payload, QOS}, PQueue)}};

handle_cast({publish, Topic, Payload},  #mqtt_connection{mqttc = C, available = true} = State) ->
	emqttc:publish(C, Topic, Payload),
	{noreply, State};

handle_cast({publish, Topic, Payload, QOS}, #mqtt_connection{mqttc = C, available = true} = State) ->
	emqttc:publish(C, Topic, Payload, QOS),
	{noreply, State};

handle_cast({unsubscribe, Topic}, #mqtt_connection{mqttc = C, available = true} = State) ->
	emqttc:unsubscribe(C, Topic),
	{noreply, State};

handle_cast({unsubscribe, Topic, C}, #mqtt_connection{available = true} = State) ->
	emqttc:unsubscribe(C, Topic),
	{noreply, State};

handle_cast({unsubscribe, Topic, C}, #mqtt_connection{available = false, squeue = SQueue} = State) ->
	%% Mqtt no ready to unsubscribe
	{noreply, State#mqtt_connection{squeue = queue:in({unsubscribe, Topic, C}, SQueue)}};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({reconnect, ConnInfo}, #mqtt_connection{available = false} = State) ->
	lager:warning("MqttClient: perform reconnect: ConnInfo : ~p ~n",[ConnInfo]),
	NewState = case connect(ConnInfo) of 
	{ok, C} ->
		Ref = erlang:monitor(process, C),
		State#mqtt_connection{mqttc = C, monitor_ref = Ref};
	_ -> State
	end, 
	{noreply, NewState};


handle_info({publish, Topic, Payload}, #mqtt_connection{mqttc = _C} =State) ->
	%% TODO:  Msg get from broker
	wh_mqtt_execute:receive_mqtt_msg(Topic, Payload),
	{noreply, State};

handle_info({mqttc, C, connected}, #mqtt_connection{mqttc = C, pqueue = PQueue, squeue = SQueue} = State) ->
	%% TODO : retry pending publish and subscribe 
	% lager:info("MQTT Client connected ~n",[]),
	NewPQueue = send_pending_publish(C, PQueue),
	NewSQueue = send_pending_subscribe(C, SQueue),
	{noreply, State#mqtt_connection{available = true, pqueue = NewPQueue, squeue = NewSQueue}};

%% Mqtt Client process still live, just notif for disconnetion
handle_info({mqttc, C ,disconnected}, State) ->
	lager:warning("MQTTClient disconnected ~p ~n",[{self(), C}]),
	{noreply, State#mqtt_connection{available = false}};


handle_info({'DOWN', _, process, MqttC, Reason}, #mqtt_connection{mqttc = MqttC, monitor_ref=Ref} = State)  ->
	lager:warning("MQTTClient  ~p went down ~p",[MqttC, Reason]),
	erlang:demonitor(Ref),
	NewState = reconnect(State),
	{noreply, NewState};

handle_info(_Msg, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, #mqtt_connection{mqttc = C, monitor_ref = Ref}) when is_pid(C) ->
	% lager:warning("MqttClient: terminate with Reason: ~p",[Reason]),
	erlang:demonitor(Ref),
	disconnected(C), 
	ok;
terminate(_Reason, _State) ->
	% lager:warning("MqttClient: terminate: Reason: ~p",[Reason]), 
	ok.

connect(ConnInfo) ->
	emqttc:start_link(ConnInfo).


disconnected(MqttC) ->
	emqttc:disconnect(MqttC).

reconnect(#mqtt_connection{info = ConnInfo} = MqttConn) ->
	erlang:send_after(5000, self(), {reconnect, ConnInfo}),
	MqttConn#mqtt_connection{available = false}.

send_pending_subscribe(MqttC, SQueue) ->
	queue:filter(fun
		({subscribe, Topic}) ->
			emqttc:subscribe(MqttC, Topic),
			wpool:cast(wh_mqtt_manage:pool_name(), {process_pending, subscribe, MqttC, Topic}),
			false;
		({subscribe, Topic, QOS}) ->
			emqttc:subscribe(MqttC, Topic, QOS),
			wpool:cast(wh_mqtt_manage:pool_name(), {process_pending, subscribe, MqttC, Topic}),
			false;
		({unsubscribe, Topic, Client}) ->
			emqttc:unsubscribe(Client, Topic),
			false;
		(_Msg) -> false
	end, SQueue). 

send_pending_publish(MqttC, PQueue) ->
	queue:filter(fun
		({Topic, Payload}) ->
			% lager:info("pending publish: Topic ~p; Payload: ~p",[Topic, Payload]),
			emqttc:publish(MqttC, Topic, Payload),
			false;
		({Topic, Payload,QOS}) ->
			% lager:info("pending publish QOS: Topic: ~p; Payload: ~p",[Topic, Payload]),
			emqttc:publish(MqttC, Topic, Payload, QOS),
			false;
		(_Msg) -> false 
	end, PQueue).

subscribe(Topic) ->
	wpool:call(pool_name(), {subscribe, Topic}).

subscribe(Topic, QOS) ->
	wpool:call(pool_name(), {subscribe, Topic, QOS}).

publish(Topic, Payload) ->
	wpool:cast(pool_name(),{publish, Topic, Payload}).

publish(Topic, Payload, QOS) ->
	wpool:cast(pool_name(), {publish, Topic, Payload, QOS}).

unsubscribe(Topic, MqttClient) ->
	wpool:cast(pool_name(), {unsubscribe, Topic, MqttClient}).

get_mqtt_conn() ->
	wpool:call(pool_name(), get_conn).

pool_name() ->
	?MODULE.

pool_sup_ind() ->
	list_to_atom(atom_to_list(?MODULE) ++ "_subsup").

mqtt_client_name(Name) ->
	list_to_atom(atom_to_list(Name) ++ "-" ++ atom_to_list(node())).


get_conn_info(Options) ->
	Host = proplists:get_value(host, Options, "localhost"),
	Port  = proplists:get_value(port, Options, 1883),
	KeepAlive = proplists:get_value(keepalive, Options, 60),
	Loglevel = proplists:get_value(loglevel, Options, error),
	Duration = proplists:get_value(duration, Options, 5),
	UserName = list_to_binary(proplists:get_value(username, Options, "admin")),
	Password = list_to_binary(proplists:get_value(password, Options, "public")), 
	ConnOption = [{host, Host}, {port, Port}, {username, UserName}, {password, Password}, 
					{keepalive, KeepAlive}, 
					{logger, {lager, Loglevel}}, 
					auto_resub,
					{reconnect, {Duration, Duration, 17280}}],
	case proplists:get_value(ssl, Options, <<>>)  of
		[] -> [ssl|ConnOption];
		SslOpts when is_list(SslOpts) ->
			[{ssl, SslOpts}|ConnOption];
		_ -> ConnOption
	end.

statistic() ->
	Get = fun proplists:get_value/2,
	InitStats = wpool:stats(pool_name()),
	PoolPid = Get(supervisor, InitStats),
	Options = Get(options, InitStats),
	InitWorkers = Get(workers, InitStats),
	WorkerStatus = 
	[begin
	    WorkerStats = Get(I, InitWorkers),
	    MsgQueueLen = Get(message_queue_len, WorkerStats),
	    Memory = Get(memory, WorkerStats),
	    {status, WorkerStats, MsgQueueLen, Memory}
   	end || I <- lists:seq(1, length(InitWorkers))],
   	[PoolPid, Options, WorkerStatus].
