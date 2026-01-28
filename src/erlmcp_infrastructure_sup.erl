-module(erlmcp_infrastructure_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% v1.3.0: Infrastructure Subsystem Supervisor
%%
%% Supervises session management, task queues, and resource subscriptions.
%% This is TIER 2 - depends on registry being up.
%%
%% Strategy: one_for_one - each infrastructure component can fail independently
%% Impact: Failures here do not affect active connections or servers
%% ====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Each infrastructure component fails independently
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% Hot reload system - zero-downtime upgrades
        #{
            id => erlmcp_hot_reload,
            start => {erlmcp_hot_reload, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_hot_reload]
        },

        %% Graceful drain coordinator - connection draining for upgrades
        #{
            id => erlmcp_graceful_drain,
            start => {erlmcp_graceful_drain, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_graceful_drain]
        },

        %% Session manager - HTTP session management and tracking
        #{
            id => erlmcp_session_manager,
            start => {erlmcp_session_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_session_manager]
        },

        %% Task manager - MCP tasks API / async job queue
        #{
            id => erlmcp_task_manager,
            start => {erlmcp_task_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_task_manager]
        },

        %% Resource subscriptions manager
        #{
            id => erlmcp_resource_subscriptions,
            start => {erlmcp_resource_subscriptions, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_resource_subscriptions]
        },

        %% SSE Event Store - maintains recent events for stream resumability
        #{
            id => erlmcp_sse_event_store,
            start => {erlmcp_sse_event_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_sse_event_store]
        },

        %% Icon Cache - caches icon metadata with TTL enforcement
        #{
            id => erlmcp_icon_cache,
            start => {erlmcp_icon_cache, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_icon_cache]
        },

        %% Session Replicator - distributed session state management
        #{
            id => erlmcp_session_replicator,
            start => {erlmcp_session_replicator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_session_replicator]
        },

        %% Session Failover Manager
        #{
            id => erlmcp_session_failover,
            start => {erlmcp_session_failover, start_link, [node()]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_session_failover]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
