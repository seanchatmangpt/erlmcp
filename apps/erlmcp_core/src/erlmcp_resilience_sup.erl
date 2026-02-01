-module(erlmcp_resilience_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% Resilience Supervisor
%%
%% Supervises resilience and resource protection components:
%% - Circuit breakers (DoS protection via failure thresholds)
%% - Rate limiters (DoS protection via request throttling)
%% - Connection limiters (prevent FD exhaustion)
%% - Connection monitors (detect FD leaks)
%% - Memory monitors (binary GC, heap protection)
%% - CPU quotas (prevent CPU-intensive DoS)
%%
%% Strategy: one_for_one - independent protection mechanisms
%% ====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% ================================================================
        %% CIRCUIT BREAKER: DoS protection via failure threshold detection
        %% Critical: Maintains DoS protection state, must survive restarts
        %% ================================================================
        #{
            id => erlmcp_circuit_breaker,
            start => {erlmcp_circuit_breaker, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_circuit_breaker]
        },

        %% ================================================================
        %% RATE LIMITER: DoS protection via rate limiting and throttling
        %% Critical: Maintains rate limit state and DDoS blocking
        %% ================================================================
        #{
            id => erlmcp_rate_limiter,
            start => {erlmcp_rate_limiter, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_rate_limiter]
        },

        %% ================================================================
        %% CONNECTION LIMITING: Prevent FD exhaustion at 10K connections
        %% ================================================================
        #{
            id => erlmcp_connection_limiter,
            start => {erlmcp_connection_limiter, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_connection_limiter]
        },

        %% ================================================================
        %% CONNECTION MONITORING: Detect and prevent FD leaks
        %% ================================================================
        #{
            id => erlmcp_connection_monitor,
            start => {erlmcp_connection_monitor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_connection_monitor]
        },

        %% ================================================================
        %% MEMORY MONITORING: Binary garbage collection to prevent heap exhaustion
        %% ================================================================
        #{
            id => erlmcp_memory_monitor,
            start => {erlmcp_memory_monitor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_memory_monitor]
        },

        %% ================================================================
        %% CPU QUOTA MANAGEMENT: Prevent CPU-intensive DoS attacks (TASK #107)
        %% ================================================================
        #{
            id => erlmcp_cpu_quota,
            start => {erlmcp_cpu_quota, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cpu_quota]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
