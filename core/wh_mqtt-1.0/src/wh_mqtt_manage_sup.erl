-module(wh_mqtt_manage_sup).

-behaviour(supervisor).

-include("wh_mqtt.hrl").

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, [?SUPER(wh_mqtt_manage)]}}.

