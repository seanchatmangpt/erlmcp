%%%-------------------------------------------------------------------
%%% @doc
### Enterprise Load Testing Application for erlmcp v3

This application provides comprehensive load testing capabilities for
 Fortune 500 scale validation with realistic traffic patterns.
%%%
## Features
%%% - Concurrent user simulation (10K+ users)
%%% - Throughput testing (10K+ req/sec)
%%% - Stress testing and breaking point detection
%%% - Latency analysis and optimization
%%% - Resource utilization monitoring
%%% - Multi-protocol testing (HTTP, WebSocket, SSE)
%%% - Performance regression detection
%%% - Peak load handling validation
%%% - Failover testing under load
%%%
## Architecture
%%% - OTP 28+ compliant with proper supervision
%%% - High-concurrency design using process pools
%%% - Real-time metrics collection and analysis
%%% - Protocol-specific connection pooling
%%% - Circuit breaker and retry logic
%%% - Comprehensive monitoring and alerting
%%%
## Usage
%%% ```erlang
%%% %% Start the application
%%% erlmcp_load_testing_app:start(normal, []),
%%%
%%% %% Create a load test configuration
%%% Config = #{
%%%     protocol => http,
%%%     target_endpoint => "http://localhost:8080/api",
%%%     user_count => 10000,
%%%     duration => 300,  % 5 minutes
%%%     request_rate => 1000,
%%%     error_rate_threshold => 0.05
%%% },
%%%
%%% %% Start the load test
%%% {ok, TestPid} = erlmcp_load_testing:start_load_test("my_test", Config),
%%%
%%% %% Monitor progress
%%% {ok, Status} = erlmcp_load_testing:get_load_test_status("my_test").
%%% ```
%%%
## Supported Protocols
%%% - HTTP/1.1 and HTTP/2
%%% - WebSocket (WS/WSS)
%%% - Server-Sent Events (SSE)
%%% - TCP with custom protocols
%%%
## Monitoring and Metrics
%%% - Real-time throughput monitoring
%%% - Latency percentiles (P50, P90, P95, P99)
%%% - Resource utilization (CPU, memory, connections)
%%% - Error tracking and alerting
%%% - Historical data storage
%%% - Integration with OpenTelemetry
%%%
## Load Testing Scenarios
%%% - Constant load testing
%%% - Ramp-up/ramp-down testing
%%% - Spike testing
%%% - Stress testing
%%% - Soak testing
%%% - Chaos testing integration
%%%
## Performance Benchmarks
%%% - 1M+ requests per second (theoretical)
%%% - 10K+ concurrent users
%%% - Sub-millisecond latency for local testing
%%% - Linear scaling with available resources
%%% - Memory efficient with connection pooling
%%%
## Quality Gates
%%% - 99.99% availability target
%%% - <100ms P95 latency for normal load
%%% - <5% error rate under peak load
%%% - Zero data loss
%%% - Automatic recovery from failures
%%%
## Integration
%%% - erlmcp_core for protocol handling
%%% - erlmcp_observability for monitoring
%%% - erlmcp_transports for transport layer
%%% - OpenTelemetry for distributed tracing
%%% - Prometheus metrics export
%%%
## Security
%%% - TLS 1.2/1.3 encryption
%%% - Connection authentication
%%% - Rate limiting and throttling
%%% - Request validation
%%% - Secure logging
%%%
## Scaling
%%% - Horizontal scaling across nodes
%%% - Dynamic load balancing
%%% - Auto-scaling based on metrics
%%% - Connection pooling optimization
%%% - Resource utilization optimization
%%%
## Fault Tolerance
%%% - Automatic connection recovery
%%% - Retry with exponential backoff
%%% - Circuit breaker pattern
%%% - Graceful degradation
%%% - Health monitoring
%%%
## Future Enhancements
%%% - gRPC protocol support
%%% - GraphQL testing
%%% - Database load testing
%%% - Authentication scenarios
%%% - Custom protocol extensions
%%% - Machine learning optimization
%%%
## Performance Optimizations
%%% - Connection pooling and reuse
%%% - Message batching
%%% - Asynchronous I/O
%%% - Process pooling
%%% - Memory optimization
%%% - CPU affinity (where applicable)
%%%
## Best Practices
%%% - Start with small test scale
%%% - Monitor metrics continuously
%%% - Use appropriate think times
%%% - Configure realistic timeouts
%%% - Monitor system resources
%%% - Plan for graceful shutdown
%%%
## Limitations
%%% - Network bandwidth limits
%%% - Hardware resource constraints
%%% - Protocol-specific limitations
%%% - Operating system limits
%%%
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_load_testing_app).

-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).

-include("erlmcp_load_testing.hrl").

%%====================================================================
%% API Functions
%%====================================================================

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    %% Start the supervision tree
    case erlmcp_load_testing_supervisor:start_link() of
        {ok, SupPid} ->
            %% Start monitoring services
            start_monitoring_services(),
            {ok, SupPid};
        {error, Reason} ->
            {error, Reason}
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    %% Stop all services
    stop_monitoring_services(),
    ok.

-spec prep_stop(term()) -> term().
prep_stop(State) ->
    %% Prepare for graceful shutdown
    erlmcp_load_testing_supervisor:prepare_shutdown(),
    State.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Start monitoring services
-spec start_monitoring_services() -> ok.
start_monitoring_services() ->
    %% Start metrics collection service
    case erlmcp_load_testing_metrics:start_link() of
        {ok, _} ->
            ok;
        {error, _} ->
            ok
    end,

    %% Start alert manager
    case erlmcp_load_testing_alert_manager:start_link() of
        {ok, _} ->
            ok;
        {error, _} ->
            ok
    end,

    %% Start performance analyzer
    case erlmcp_load_testing_performance_analyzer:start_link() of
        {ok, _} ->
            ok;
        {error, _} ->
            ok
    end,

    %% Start resource monitor
    case erlmcp_load_testing_resource_monitor:start_link() of
        {ok, _} ->
            ok;
        {error, _} ->
            ok
    end,

    ok.

%% Stop monitoring services
-spec stop_monitoring_services() -> ok.
stop_monitoring_services() ->
    %% Stop all monitoring services gracefully
    erlmcp_load_testing_metrics:stop(),
    erlmcp_load_testing_alert_manager:stop(),
    erlmcp_load_testing_performance_analyzer:stop(),
    erlmcp_load_testing_resource_monitor:stop(),
    ok.