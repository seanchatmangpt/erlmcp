%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_prometheus_exporter_tests - Prometheus Exporter Tests
%%%
%%% Chicago School TDD:
%%% - NO MOCKS: Test real Prometheus export
%%% - Test ALL observable behavior
%%% - Test Prometheus text format compliance
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_prometheus_exporter_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

prometheus_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_export_format/0,
         fun test_export_with_system_metrics/0,
         fun test_prometheus_text_format_compliance/0,
         fun test_metric_types/0,
         fun test_counter_metrics/0,
         fun test_gauge_metrics/0,
         fun test_flag_metrics/0
     ]}.

setup() ->
    erlmcp_counters:init(),
    erlmcp_flags:init(),
    erlmcp_counters:reset(),
    ok.

cleanup(_) ->
    erlmcp_counters:reset(),
    ok.

%%====================================================================
%% Export Format Tests
%%====================================================================

test_export_format() ->
    ?_test(begin
        % Export metrics
        Metrics = erlmcp_prometheus_exporter:export(),
        MetricsText = iolist_to_binary(Metrics),

        % Verify output is non-empty
        ?assert(byte_size(MetricsText) > 0),

        % Verify basic Prometheus format
        ?assertMatch({match, _}, re:run(MetricsText, <<"# HELP">>)),
        ?assertMatch({match, _}, re:run(MetricsText, <<"# TYPE">>))
    end).

test_export_with_system_metrics() ->
    ?_test(begin
        % Export metrics with system metrics
        Metrics = erlmcp_prometheus_exporter:export_with_system_metrics(),
        MetricsText = iolist_to_binary(Metrics),

        % Verify counter metrics are present
        ?assertMatch({match, _}, re:run(MetricsText, <<"erlmcp_requests_total">>)),

        % Verify system metrics are present
        ?assertMatch({match, _}, re:run(MetricsText, <<"erlmcp_memory_total_bytes">>)),
        ?assertMatch({match, _}, re:run(MetricsText, <<"erlmcp_process_count">>)),
        ?assertMatch({match, _}, re:run(MetricsText, <<"erlmcp_schedulers">>)),

        % Verify flag metrics are present
        ?assertMatch({match, _}, re:run(MetricsText, <<"erlmcp_accepting_connections">>)),
        ?assertMatch({match, _}, re:run(MetricsText, <<"erlmcp_healthy">>))
    end).

%%====================================================================
%% Prometheus Format Compliance Tests
%%====================================================================

test_prometheus_text_format_compliance() ->
    ?_test(begin
        % Set up some metrics
        erlmcp_counters:inc_requests(),
        erlmcp_counters:inc_success(),

        % Export metrics
        Metrics = erlmcp_prometheus_exporter:export(),
        MetricsText = iolist_to_binary(Metrics),

        % Verify format compliance
        % 1. HELP lines start with "# HELP"
        % 2. TYPE lines start with "# TYPE"
        % 3. Metric lines have format: metric_name value

        % Split into lines
        Lines = binary:split(MetricsText, <<"\n">>, [global]),

        % Verify each line matches expected format
        lists:foreach(fun(Line) ->
            case Line of
                <<>> -> ok;  % Empty line
                <<"# HELP ", _/binary>> -> ok;  % HELP line
                <<"# TYPE ", _/binary>> -> ok;  % TYPE line
                _ ->
                    % Metric line: should have "metric_name value"
                    ?assertMatch({match, _}, re:run(Line, <<"^[a-z_]+ [0-9]+$">>))
            end
        end, Lines)
    end).

test_metric_types() ->
    ?_test(begin
        % Export metrics
        Metrics = erlmcp_prometheus_exporter:export_with_system_metrics(),
        MetricsText = iolist_to_binary(Metrics),

        % Verify counter types
        ?assertMatch({match, _}, re:run(MetricsText, <<"# TYPE erlmcp_requests_total counter">>)),
        ?assertMatch({match, _}, re:run(MetricsText, <<"# TYPE erlmcp_requests_success_total counter">>)),

        % Verify gauge types
        ?assertMatch({match, _}, re:run(MetricsText, <<"# TYPE erlmcp_connections_active gauge">>)),
        ?assertMatch({match, _}, re:run(MetricsText, <<"# TYPE erlmcp_memory_total_bytes gauge">>))
    end).

%%====================================================================
%% Counter Metrics Tests
%%====================================================================

test_counter_metrics() ->
    ?_test(begin
        % Increment counters
        erlmcp_counters:inc_requests(),
        erlmcp_counters:inc_requests(),
        erlmcp_counters:inc_success(),

        % Export metrics
        Metrics = erlmcp_prometheus_exporter:export(),
        MetricsText = iolist_to_binary(Metrics),

        % Verify counter values
        ?assertMatch({match, _}, re:run(MetricsText, <<"erlmcp_requests_total 2">>)),
        ?assertMatch({match, _}, re:run(MetricsText, <<"erlmcp_requests_success_total 1">>))
    end).

%%====================================================================
%% Gauge Metrics Tests
%%====================================================================

test_gauge_metrics() ->
    ?_test(begin
        % Set up gauge metrics
        erlmcp_counters:inc_connections(),
        erlmcp_counters:inc_connections(),

        % Export metrics
        Metrics = erlmcp_prometheus_exporter:export(),
        MetricsText = iolist_to_binary(Metrics),

        % Verify gauge values
        ?assertMatch({match, _}, re:run(MetricsText, <<"erlmcp_connections_active 2">>))
    end).

%%====================================================================
%% Flag Metrics Tests
%%====================================================================

test_flag_metrics() ->
    ?_test(begin
        % Set flags
        erlmcp_flags:stop_accepting(),
        erlmcp_flags:mark_unhealthy(),

        % Export metrics
        Metrics = erlmcp_prometheus_exporter:export_with_system_metrics(),
        MetricsText = iolist_to_binary(Metrics),

        % Verify flag values (0 = false, 1 = true)
        ?assertMatch({match, _}, re:run(MetricsText, <<"erlmcp_accepting_connections 0">>)),
        ?assertMatch({match, _}, re:run(MetricsText, <<"erlmcp_healthy 0">>)),
        ?assertMatch({match, _}, re:run(MetricsText, <<"erlmcp_maintenance_mode 0">>)),
        ?assertMatch({match, _}, re:run(MetricsText, <<"erlmcp_shutting_down 0">>))
    end).

%%====================================================================
%% Integration Tests
%%====================================================================

% Additional tests could include:
% - HTTP endpoint testing (requires Cowboy integration)
% - Scraping by actual Prometheus client
% - Metric cardinality validation
% - Label validation
