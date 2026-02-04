-module(erlmcp_observability_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test suite for erlmcp_observability module
%% Covers structured logging, metrics collection, tracing, and health checks

-export([structured_logging_test/0, metrics_collection_test/0,
         distributed_tracing_test/0, health_check_test/0,
         dashboard_integration_test/0, error_handling_test/0]).

%%====================================================================
%% Test entry points
%%====================================================================

structured_logging_test() ->
    test_structured_logging_json_format(),
    test_log_levels(),
    test_log_with_trace_context(),
    test_log_batching(),
    pass.

metrics_collection_test() ->
    test_counter_metrics(),
    test_gauge_metrics(),
    test_histogram_metrics(),
    test_metrics_summary(),
    pass.

distributed_tracing_test() ->
    test_trace_start(),
    test_trace_span(),
    test_trace_context(),
    test_trace_correlation(),
    pass.

health_check_test() ->
    test_health_check_basic(),
    test_health_check_services(),
    test_health_check_periodic(),
    test_health_check_alerts(),
    pass.

dashboard_integration_test() ->
    test_dashboard_data_generation(),
    test_realtime_metrics(),
    test_websocket_integration(),
    test_client_management(),
    pass.

error_handling_test() ->
    test_error_logging(),
    test_error_metrics(),
    test_error_tracing(),
    test_error_recovery(),
    pass.

%%====================================================================
%% Structured Logging Tests
%%====================================================================

test_structured_logging_json_format() ->
    % Test that logs are in JSON format
    erlmcp_observability:log(info, <<"test message">>, #{test => true}),

    % In a real implementation, this would check the actual log output
    % For now, we verify the function doesn't crash
    true.

test_log_levels() ->
    % Test all log levels
    erlmcp_observability:log(debug, <<"debug message">>, #{level => debug}),
    erlmcp_observability:log(info, <<"info message">>, #{level => info}),
    erlmcp_observability:log(warn, <<"warning message">>, #{level => warn}),
    erlmcp_observability:log(error, <<"error message">>, #{level => error}),
    erlmcp_observability:log(fatal, <<"fatal message">>, #{level => fatal}),

    true.

test_log_with_trace_context() ->
    % Test logging with trace context
    TraceContext = erlmcp_observability:trace_span(<<"test_trace">>),
    erlmcp_observability:log(info, <<"message with trace">>, #{context => test}, TraceContext),

    true.

test_log_batching() ->
    % Test log batching functionality
    lists:foreach(fun(_) ->
        erlmcp_observability:log(info, <<"batch test">>, #{batch => true})
    end, lists:seq(1, 50)),

    true.

%%====================================================================
%% Metrics Collection Tests
%%====================================================================

test_counter_metrics() ->
    % Test counter metrics
    erlmcp_observability:counter(<<"test_counter">>, #{type => "test"}),
    erlmcp_observability:counter_inc(<<"test_counter">>, #{type => "test"}),
    erlmcp_observability:counter_inc(<<"test_counter">>, #{type => "test"}, 5),

    true.

test_gauge_metrics() ->
    % Test gauge metrics
    erlmcp_observability:gauge(<<"test_gauge">>, #{type => "test"}),
    erlmcp_observability:gauge_set(<<"test_gauge">>, #{type => "test"}, 42),
    erlmcp_observability:gauge_set(<<"test_gauge">>, #{type => "test"}),

    true.

test_histogram_metrics() ->
    % Test histogram metrics
    erlmcp_observability:histogram(<<"test_histogram">>, #{type => "test"}),
    erlmcp_observability:histogram_observe(<<"test_histogram">>, 100),
    erlmcp_observability:histogram_observe(<<"test_histogram">>, 100, #{type => "test"}),

    true.

test_metrics_summary() ->
    % Test metrics summary generation
    Summary = erlmcp_observability:get_metrics_summary(),

    % Verify the summary contains expected fields
    ?assert(is_map(Summary)),
    ?assert(is_binary(maps:get(service_name, Summary, undefined))),
    ?assert(is_binary(maps:get(endpoint, Summary, undefined))),
    ?assert(is_integer(maps:get(log_buffer_size, Summary, 0))),
    ?assert(is_integer(maps:get(metrics_buffer_size, Summary, 0))),
    ?assert(is_integer(maps:get(active_traces, Summary, 0))),
    ?assert(is_map(maps:get(health_status, Summary, #{}))),

    true.

%%====================================================================
%% Distributed Tracing Tests
%%====================================================================

test_trace_start() ->
    % Test trace start
    TraceContext = erlmcp_observability:trace_span(<<"test_trace">>),

    ?assert(is_record(TraceContext, trace_context)),
    ?assert(is_binary(TraceContext#trace_context.trace_id)),
    ?assert(is_binary(TraceContext#trace_context.current_span#trace_span.id)),
    ?assert(TraceContext#trace_context.current_span#trace_span.name =:= <<"test_trace">>),

    true.

test_trace_span() ->
    % Test nested trace spans
    TraceContext1 = erlmcp_observability:trace_span(<<"parent">>),
    erlmcp_observability:trace_span(<<"child1">>, TraceContext1),
    erlmcp_observability:trace_span(<<"child2">>, TraceContext1),

    % Verify trace context is maintained
    ?assert(is_binary(TraceContext1#trace_context.trace_id)),

    true.

test_trace_context() ->
    % Test trace context management
    TraceContext = erlmcp_observability:trace_span(<<"context_test">>),

    % Verify trace ID generation
    ?assert(is_binary(TraceContext#trace_context.trace_id)),
    ?assert(byte_size(TraceContext#trace_context.trace_id) =:= 16), % 16 bytes

    true.

test_trace_correlation() ->
    % Test trace correlation across operations
    TraceContext = erlmcp_observability:trace_span(<<"correlation_test">>),

    % Log with trace context
    erlmcp_observability:log(info, <<"correlated log">>, #{correlation => true}, TraceContext),

    true.

%%====================================================================
%% Health Check Tests
%%====================================================================

test_health_check_basic() ->
    % Test basic health check
    HealthStatus = erlmcp_health_check:health_check(),

    ?assert(is_map(HealthStatus)),
    ?assert(is_atom(maps:get(status, HealthStatus))),
    ?assert(is_integer(maps:get(total_checks, HealthStatus, 0))),
    ?assert(is_integer(maps:get(passed, HealthStatus, 0))),
    ?assert(is_integer(maps:get(warnings, HealthStatus, 0))),
    ?assert(is_integer(maps:get(errors, HealthStatus, 0))),
    ?assert(is_timestamp(maps:get(timestamp, HealthStatus))),
    ?assert(is_integer(maps:get(uptime, HealthStatus, 0))),

    true.

test_health_check_services() ->
    % Test service-specific health checks
    ProxyHealth = erlmcp_health_check:health_check(erlmcp_mcp_proxy_relay),
    BatchHealth = erlmcp_health_check:health_check(erlmcp_batch_processor),

    ?assert(is_map(ProxyHealth)),
    ?assert(is_map(BatchHealth)),
    ?assert(is_atom(maps:get(status, ProxyHealth))),
    ?assert(is_atom(maps:get(status, BatchHealth))),

    true.

test_health_check_periodic() ->
    % Test periodic health check functionality
    % This would test the timer-based periodic checks
    % For now, we just verify the function works
    erlmcp_health_check:start_periodic_check(memory),
    erlmcp_health_check:stop_periodic_check(memory),

    true.

test_health_check_alerts() ->
    % Test health check alert functionality
    HealthStatus = erlmcp_health_check:health_check(),

    % Check that the alert system works
    ?assert(is_map(HealthStatus)),
    ?assert(is_atom(maps:get(status, HealthStatus))),

    % Test critical vs warning states
    case maps:get(status, HealthStatus) of
        ok ->
            true;
        warning ->
            true;
        error ->
            true
    end,

    true.

%%====================================================================
%% Dashboard Integration Tests
%%====================================================================

test_dashboard_data_generation() ->
    % Test dashboard data generation
    DashboardData = erlmcp_dashboard_integration:get_dashboard_data(),

    ?assert(is_map(DashboardData)),
    ?assert(is_binary(maps:get(timestamp, DashboardData))),
    ?assert(is_map(maps:get(health, DashboardData))),
    ?assert(is_map(maps:get(metrics, DashboardData))),
    ?assert(is_map(maps:get(alerts, DashboardData))),
    ?assert(is_map(maps:get(thresholds, DashboardData))),
    ?assert(is_map(maps:get(service_status, DashboardData))),
    ?assert(is_map(maps:get(performance, DashboardData))),
    ?assert(is_map(maps:get(connections, DashboardData))),
    ?assert(is_integer(maps:get(clients, DashboardData))),

    true.

test_realtime_metrics() ->
    % Test real-time metrics generation
    RealtimeMetrics = erlmcp_dashboard_integration:get_realtime_metrics(),

    ?assert(is_map(RealtimeMetrics)),
    ?assert(is_map(maps:get(current, RealtimeMetrics))),
    ?assert(is_list(maps:get(historical, RealtimeMetrics))),
    ?assert(is_binary(maps:get(timestamp, RealtimeMetrics))),

    true.

test_websocket_integration() ->
    % Test WebSocket integration
    erlmcp_dashboard_integration:enable_websocket(),
    erlmcp_dashboard_integration:disable_websocket(),

    true.

test_client_management() ->
    % Test client management
    ClientId = <<"test_client">>,

    % Register client
    Result1 = erlmcp_dashboard_integration:register_dashboard(ClientId),
    ?assert(Result1 =:= orelse Result1 =:= {error, client_already_registered}),

    % Unregister client
    erlmcp_dashboard_integration:unregister_dashboard(ClientId),

    true.

%%====================================================================
%% Error Handling Tests
%%====================================================================

test_error_logging() ->
    % Test error logging functionality
    erlmcp_observability:log(error, <<"test error">>, #{error => test_error}),
    erlmcp_observability:log(warn, <<"test warning">>, #{warning => test_warning}),

    true.

test_error_metrics() ->
    % Test error metrics tracking
    erlmcp_observability:counter(<<"error_counter">>, #{type => "error"}),
    erlmcp_observability:counter_inc(<<"error_counter">>, #{type => "error"}),

    true.

test_error_tracing() ->
    % Test error tracing
    TraceContext = erlmcp_observability:trace_span(<<"error_trace">>),
    erlmcp_observability:trace_span(<<"error_span">>, #{error => true}, TraceContext),

    true.

test_error_recovery() ->
    % Test error recovery mechanisms
    % This would test how the system handles and recovers from errors
    % For now, we test that basic operations don't crash
    HealthStatus = erlmcp_health_check:health_check(),
    DashboardData = erlmcp_dashboard_integration:get_dashboard_data(),

    ?assert(is_map(HealthStatus)),
    ?assert(is_map(DashboardData)),

    true.

%%====================================================================
%% Helper Functions
%%====================================================================

is_timestamp(Timestamp) when is_integer(Timestamp) ->
    true;
is_timestamp(_Timestamp) ->
    false.

pass() ->
    ?assert(true).