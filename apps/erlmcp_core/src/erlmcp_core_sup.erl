-module(erlmcp_core_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% v1.4.0: Core Infrastructure Supervisor (TIER 1)
%%
%% Consolidates registry and infrastructure into single core supervisor.
%% This is the foundation - no external dependencies.
%%
%% Strategy: one_for_one - each component fails independently
%% Impact: Individual service failures don't cascade
%% ====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{strategy => one_for_one,  % Each domain fails independently
          intensity => 5,
          period => 60},

    %% Domain-specific supervisors for better isolation and modularity
    %% Replaces monolithic core_sup with 4 specialized supervisors

    %% Base domain supervisors (always present)
    BaseChildSpecs =
        [%% ================================================================
         %% REGISTRY SUPERVISOR: Registry domain (servers, transports)
         %% ================================================================
         #{id => erlmcp_registry_sup,
           start => {erlmcp_registry_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [erlmcp_registry_sup]},
         %% ================================================================
         %% SESSION SUPERVISOR: Session domain (backend, manager, failover)
         %% ================================================================
         #{id => erlmcp_session_sup,
           start => {erlmcp_session_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [erlmcp_session_sup]},
         %% ================================================================
         %% RESILIENCE SUPERVISOR: Resilience domain (cache, circuit_breaker)
         %% ================================================================
         #{id => erlmcp_resilience_sup,
           start => {erlmcp_resilience_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [erlmcp_resilience_sup]}],

    %% Additional supervisors for optional features
    AdditionalChildSpecs =
        %% ================================================================
        %% CLIENT SUPERVISOR: Dynamic client process management (TIER 2)
        %% Manages client connections using simple_one_for_one strategy
        %% ================================================================
        [#{id => erlmcp_client_sup,
           start => {erlmcp_client_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,  % Supervisor - wait for all children
           type => supervisor,
           modules => [erlmcp_client_sup]}],
        %% ================================================================
        %% PLUGIN SUPERVISOR: Plugin system management
        %% Manages plugin discovery, loading, registry, and execution
        %% Each plugin runs in isolated process (let-it-crash)
        %% ================================================================
        [#{id => erlmcp_plugin_sup,
           start => {erlmcp_plugin_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,  % Supervisor - wait for all children
           type => supervisor,
           modules => [erlmcp_plugin_sup]}],

    %% Include base domain supervisors
    %% Conditionally include additional supervisors based on configuration
    ChildSpecs =
        %% Always include domain supervisors (critical infrastructure)
        %% Domain failures are isolated and don't cascade
        BaseChildSpecs ++
        %% Include additional optional supervisors
        case application:get_env(erlmcp_core, enable_plugins, true) of
            true ->
                AdditionalChildSpecs;
            false ->
                %% Skip plugin supervisor when disabled to prevent zombie process
                []
        end ++
        %% Conditionally include cluster supervisor
        case application:get_env(erlmcp_core, cluster_enabled, false) of
            true ->
                [%% ================================================================
                 %% CLUSTER: Distributed registry and cluster management
                 %% ================================================================
                 #{id => erlmcp_cluster_sup,
                   start => {erlmcp_cluster_sup, start_link, []},
                   restart => permanent,
                   shutdown => 5000,
                   type => supervisor,
                   modules => [erlmcp_cluster_sup]}];
            false ->
                %% Skip cluster supervisor when disabled to prevent zombie process
                []
        end,

    {ok, {SupFlags, ChildSpecs}}.
