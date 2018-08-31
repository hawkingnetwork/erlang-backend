-module(wh_mqtt_connection_sup).

-behaviour(supervisor).

-include("wh_mqtt.hrl").

-export([start_link/0]).
% -export([add/1]).
% -export([remove/1]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------

start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

% add(ConnInfo) ->
% 	supervisor:start_child(?MODULE, [ConnInfo]).

% remove(Conn) when is_pid(Conn) ->
%     supervisor:terminate_child(?MODULE, Conn).

% init([]) ->
%     RestartStrategy = 'simple_one_for_one',
%     MaxRestarts = 5,
%     MaxSecondsBetweenRestarts = 10,
%     SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
%     {'ok', {SupFlags, [?WORKER('wh_mqtt_connection')]}}.

init([]) ->
	RestartStrategy = 'one_for_one',
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, [?SUPER(wh_mqtt_connection)]}}.

