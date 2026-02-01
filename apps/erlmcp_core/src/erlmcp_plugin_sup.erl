%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin Supervisor - Main supervisor for plugin system
%%%
%%% Strategy: one_for_one
%%% - Each component fails independently
%%% - Registry failure doesn't affect manager
%%% - Manager failure doesn't affect workers
%%%
%%% Children:
%%% 1. erlmcp_plugin_registry - Plugin metadata and routing
%%% 2. erlmcp_plugin_manager - Plugin lifecycle management
%%% 3. erlmcp_plugin_worker_sup - Dynamic plugin workers
%%%
%%% Restart strategy: permanent
%%% - All children are critical and should always be running
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags =
        #{strategy => one_for_one,         % Independent failures
          intensity => 5,                  % Max 5 restarts
          period => 60},                     % In 60 seconds

    ChildSpecs =
        [%% ================================================================
         %% REGISTRY: Plugin metadata and routing (gproc-based)
         %% ================================================================
         #{id => erlmcp_plugin_registry,
           start => {erlmcp_plugin_registry, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_plugin_registry]},
         %% ================================================================
         %% MANAGER: Plugin lifecycle orchestration
         %% ================================================================
         #{id => erlmcp_plugin_manager,
           start => {erlmcp_plugin_manager, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_plugin_manager]},
         %% ================================================================
         %% WORKER SUPERVISOR: Dynamic plugin instances
         %% ================================================================
         #{id => erlmcp_plugin_worker_sup,
           start => {erlmcp_plugin_worker_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,        % Supervisor - wait for children
           type => supervisor,
           modules => [erlmcp_plugin_worker_sup]}],

    {ok, {SupFlags, ChildSpecs}}.
