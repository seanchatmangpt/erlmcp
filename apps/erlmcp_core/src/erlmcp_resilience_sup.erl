-module(erlmcp_resilience_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% Resilience Infrastructure Supervisor
%%
%% Manages resilience and protection mechanisms
%% These components provide critical protection and must be isolated
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{strategy => one_for_one,  % Each resilience component fails independently
          intensity => 5,
          period => 60},

    ChildSpecs =
        %% ================================================================
        %% CONNECTION LIMITING: Prevent FD exhaustion at 10K connections
        %% ================================================================
        [#{id => erlmcp_connection_limiter,
           start => {erlmcp_connection_limiter, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_connection_limiter]},
         %% ================================================================
        %% CONNECTION MONITORING: Detect and prevent FD leaks
        %% ================================================================
        #{id => erlmcp_connection_monitor,
           start => {erlmcp_connection_monitor, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_connection_monitor]},
         %% ================================================================
        %% MEMORY MONITORING: Binary garbage collection to prevent heap exhaustion
        %% ================================================================
        #{id => erlmcp_memory_monitor,
           start => {erlmcp_memory_monitor, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_memory_monitor]},
         %% ================================================================
        %% CPU QUOTA MANAGEMENT: Prevent CPU-intensive DoS attacks
        %% ================================================================
        #{id => erlmcp_cpu_quota,
           start => {erlmcp_cpu_quota, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_cpu_quota]},
         %% ================================================================
        %% CANCELLATION: Request cancellation for long-running operations
        %% ================================================================
        #{id => erlmcp_cancellation,
           start => {erlmcp_cancellation, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_cancellation]},
         %% ================================================================
        %% CIRCUIT BREAKER: DoS protection via failure threshold detection
        %% Critical: Maintains DoS protection state, must survive restarts
        %% ================================================================
        #{id => erlmcp_circuit_breaker,
           start => {erlmcp_circuit_breaker, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_circuit_breaker]},
         %% ================================================================
        %% RATE LIMITER: DoS protection via rate limiting and throttling
        %% Critical: Maintains rate limit state and DDoS blocking
        %% ================================================================
        #{id => erlmcp_rate_limiter,
           start => {erlmcp_rate_limiter, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_rate_limiter]}],

    {ok, {SupFlags, ChildSpecs}}.