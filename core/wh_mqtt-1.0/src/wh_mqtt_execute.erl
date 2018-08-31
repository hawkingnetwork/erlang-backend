-module(wh_mqtt_execute).

-behaviour(gen_server).
-behaviour(wh_mqtt).

-include("wh_mqtt.hrl").

-export([start_link/0]).
-export([init/1, handle_cast/2, handle_info/2, handle_call/3, code_change/3, terminate/2]).
-export([start_pool/1, pool_name/0, statistic/0]).
-export([receive_mqtt_msg/2]).


start_pool(WorkerSize) -> 
	ExcuteWorkerOPts = [{workers, WorkerSize},
						{worker, { ?MODULE, [undefined]}}],
	supervisor:start_child(wh_mqtt_execute_sup,
							{pool_sup_ind(), 
							{ wpool, start_pool, [pool_name(), ExcuteWorkerOPts]}, 
								transient, 5000, supervisor, [ wpool ]}).


start_link() ->
	init().

init() ->
	Options = application:get_env(wh_mqtt, mqtt, []),
	WorkerSize = proplists:get_value(workersize, Options, 100),
	ExcuteWorkerOPts = [ {overrun_warning, 10000}
					, {overrun_handler, {sumo_internal, report_overrun}}
					, {workers, WorkerSize}
					, {pool_sup_shutdown, 'infinity'}
					, {pool_sup_intensity, 10}
					, {pool_sup_period, 10}
					, {worker, {?MODULE, [undefined]}}
					],
	wpool:start_pool(pool_name(), ExcuteWorkerOPts).

init([undefined]) ->
	{ok, 'execute_worker'}.

handle_cast({receive_mqtt_msg, Topic, Payload}, State) ->
	case ets:lookup(?TOPIC, Topic) of 
	[{Topic, Handler, _}] ->
		Handler:receive_mqtt_msg(Topic, Payload);
	_ ->
		ok 
	end, 
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

receive_mqtt_msg(Topic, Payload) ->
	wpool:cast(pool_name(), {receive_mqtt_msg, Topic, Payload}, next_worker).

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

