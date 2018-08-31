-module(wh_mqtt_sup).

-include("wh_mqtt.hrl").


-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	RestartStrategy = 'one_for_one',
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, [
    					?SUPER('wh_mqtt_connection_sup'),
    					?SUPER('wh_mqtt_manage_sup'),
    					?SUPER('wh_mqtt_execute_sup')
    					]}}.

