-module(wh_mqtt).
-export([start_link/0]).
-export([subscribe/2, subscribe/3, publish/2, publish/3, unsubscribe/1]).

-callback receive_mqtt_msg(binary(), term()) -> ok.

start_link() ->
	ConnInfo = application:get_env(wh_mqtt, mqtt, []),
	PoolSize = proplists:get_value(poolsize, ConnInfo, 100),
	WorkerSize = proplists:get_value(workersize, ConnInfo, 100),
	Host = proplists:get_value(host, ConnInfo, "localhost"),
	Port  = proplists:get_value(port, ConnInfo, 1883),
	KeepAlive = proplists:get_value(keepalive, ConnInfo, 60),
	Loglevel = proplists:get_value(loglevel, ConnInfo, error),
	Duration = proplists:get_value(duration, ConnInfo, 5),
	UserName = list_to_binary(proplists:get_value(username, ConnInfo, "admin")),
	Password = list_to_binary(proplists:get_value(password, ConnInfo, "public")), 
	wh_mqtt_manage:start_pool(WorkerSize),
	wh_mqtt_execute:start_pool(WorkerSize),
	ConnOption = [{host, Host}, {port, Port}, {username, UserName}, {password, Password}, 
					{keepalive, KeepAlive}, 
					{logger, {lager, Loglevel}}, 
					auto_resub,
					{reconnect, {Duration, Duration, 17280}}],
	SslConnOption = case proplists:get_value(ssl, ConnInfo, <<>>)  of
						[] -> [ssl|ConnOption];
						SslOpts when is_list(SslOpts) ->
							[{ssl, SslOpts}|ConnOption];
						_ -> ConnOption
					end,
	wh_mqtt_connection:start_pool(PoolSize, SslConnOption). %% reconnect in 24h


subscribe(Module, Topic) ->
	wh_mqtt_manage:subscribe(Module, Topic).

subscribe(Module, Topic, QOS) ->
	wh_mqtt_manage:subscribe(Module, Topic, QOS).

unsubscribe(Topic) ->
	wh_mqtt_manage:unsubscribe(Topic).



publish(Topic, Payload) ->
	wh_mqtt_manage:publish(Topic, Payload).

publish(Topic, Payload, QOS) ->
	wh_mqtt_manage:publish(Topic, Payload, QOS).
