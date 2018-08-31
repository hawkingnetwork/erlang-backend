-module(wh_mqtt_manage).
-behaviour(gen_server).
-include("wh_mqtt.hrl").

-export([start_link/0]).
-export([init/1, handle_cast/2, handle_info/2, handle_call/3, code_change/3, terminate/2]).
-export([pool_name/0, statistic/0, start_pool/1]).
-export([subscribe/2, subscribe/3, publish/2, publish/3, unsubscribe/1]).


start_pool(WorkerSize) ->
	ManagerWorkerOPts = [{workers, WorkerSize},
						{worker, { ?MODULE, [undefined]}}],
	supervisor:start_child(wh_mqtt_manage_sup,
							{pool_sup_ind(), 
							{ wpool, start_pool, [pool_name(), ManagerWorkerOPts]}, 
							transient, 5000, supervisor, [ wpool ]}).

start_link() ->
	init().

init() ->
	ets:new(?TOPIC, ['named_table' ,'public',{'write_concurrency','true'}, {'read_concurrency', 'true'}]),

	Options = application:get_env(wh_mqtt, mqtt, []),
	WorkerSize = proplists:get_value(workersize, Options, 100),
	ManagerWorkerOPts    = [ {overrun_warning, 10000}
					, {overrun_handler, {sumo_internal, report_overrun}}
					, {workers, WorkerSize}
					, {pool_sup_shutdown, 'infinity'}
					, {pool_sup_intensity, 10}
					, {pool_sup_period, 10}
					, {worker, {?MODULE, [undefined]}}],
	wpool:start_pool(pool_name(), ManagerWorkerOPts).

init([undefined]) ->
	{ok, 'manager_worker'}.

handle_cast({process_pending, subscribe, MqttClient, Topic}, State) ->
	case ets:lookup(?TOPIC, Topic) of 
	[{_, Handler, _}] -> 
		ets:insert(?TOPIC, {Topic, Handler, MqttClient});
	_ ->
		lager:error("resubcribe on topic ~p not exist yet",[Topic]),
		ok 
	end,
	{noreply, State};

handle_cast({subscribe, Handler, Topic}, State) ->
	case ets:lookup(?TOPIC, Topic) of 
	[] ->
		MqttClient = wh_mqtt_connection:subscribe(Topic),
		ets:insert(?TOPIC, {Topic, Handler, MqttClient});
		%lager:debug("Subscribe: Topic: ~p; Resp: ~p ~n",[Topic, MqttClient]);
	_ ->
		lager:error("already subscribed in topic ~p",[Topic])
	end, 
	{noreply, State};

handle_cast({subscribe, Handler, Topic, QOS}, State) ->
	case ets:lookup(?TOPIC, Topic) of 		
	[] ->
		MqttClient = wh_mqtt_connection:subscribe(Topic, QOS),
		ets:insert(?TOPIC, {Topic, Handler, MqttClient});
		%lager:debug("Subscribe: Topic: ~p; QOS: ~p; Resp: ~p ~n",[Topic, QOS, MqttClient]);
	_ ->
		lager:error("already subscribed in topic ~p",[Topic])
	end,
	{noreply, State};

handle_cast({unsubscribe, Topic}, State) ->
	case ets:lookup(?TOPIC, Topic) of 
	[{_, _, MqttClient}] -> 
		wh_mqtt_connection:unsubscribe(Topic, MqttClient),
		ets:delete(?TOPIC, Topic);
	_  ->
		lager:error("no subscribed in topic ~p",[Topic])
	end,
	{noreply, State};

handle_cast({publish, Topic, Payload}, State) ->
	%lager:debug("Topic: ~p ; Payload: ~p ~n",[Topic, Payload]),
	wh_mqtt_connection:publish(Topic, Payload),
	{noreply, State};

handle_cast({publish, Topic, Payload, QOS}, State) ->
	%lager:debug("Topic: ~p ; Payload: ~p; QOS ~p ~n",[Topic, Payload, QOS]),
	wh_mqtt_connection:publish(Topic, Payload, QOS),
	{noreply, State};

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_call(_Msg, _From, State) ->
	{reply, State}.

handle_info(_Msg, State) ->
	{noreply, State}.

code_change(_OldVsn, State, _Extra) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

subscribe(Handler, Topic) when is_binary(Topic) ->
	wpool:cast(pool_name(), {subscribe, Handler, Topic});
subscribe(Handler, Topic) ->
	subscribe(Handler, wh_mqtt_util:to_bin(Topic)).

subscribe(Handler, Topic, QOS) when is_binary(Topic) ->
	wpool:cast(pool_name(), {subscribe, Handler, Topic, QOS});
subscribe(Handler, Topic, QOS) ->
	subscribe(Handler, wh_mqtt_util:to_bin(Topic), QOS).

publish(Topic, Payload) when is_binary(Topic) ->
	wpool:cast(pool_name(), {publish, Topic, Payload});
publish(Topic, Payload) ->
	publish(wh_mqtt_util:to_bin(Topic), Payload).

publish(Topic, Payload, QOS) when is_binary(Topic) ->
	wpool:cast(pool_name(), {publish, Topic, Payload, QOS});
publish(Topic, Payload, QOS) ->
	publish(wh_mqtt_util:to_bin(Topic), Payload, QOS).

unsubscribe(Topic) when is_binary(Topic) ->
	wpool:cast(pool_name(), {unsubscribe, Topic});

unsubscribe(Topic) ->
	unsubscribe(wh_mqtt_util:to_bin(Topic)).

pool_name()->
	?MODULE.

pool_sup_ind() ->
	list_to_atom(atom_to_list(?MODULE) ++ "_subsup").

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

