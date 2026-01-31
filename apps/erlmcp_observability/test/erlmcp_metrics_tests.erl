%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_metrics module following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls only
%%% - Use REAL erlmcp_metrics gen_server (no mocks, no dummy processes)
%%% - NO internal state inspection (test API boundaries only)
%%% - NO record duplication (respect encapsulation)
%%% - Split into focused modules (this file: recording & query tests)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_metrics_tests).

-include_lib("eunit/include/eunit.hrl").

%% Note: NO record duplication - the #metric record is defined in erlmcp_metrics module
%% We test the observable behavior through API calls only, not internal state

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Setup/teardown for metrics gen_server
metrics_test_() ->
    {setup,
     fun setup_metrics/0,
     fun cleanup_metrics/1,
     [
         {"Record transport operation", fun test_record_transport_operation/0},
         {"Record server operation", fun test_record_server_operation/0},
         {"Record registry operation", fun test_record_registry_operation/0},
         {"Get all metrics", fun test_get_metrics/0},
         {"Get metrics by name", fun test_get_metrics_by_name/0},
         {"Reset metrics", fun test_reset_metrics/0},
         {"With metrics helper", fun test_with_metrics/0}
     ]}.

setup_metrics() ->
    % Start metrics gen_server
    {ok, Pid} = erlmcp_metrics:start_link(),
    Pid.

cleanup_metrics(Pid) ->
    % Stop metrics gen_server
    catch gen_server:stop(Pid),
    % Clean up any registered name
    catch unregister(erlmcp_metrics).

%%====================================================================
%% Recording Tests
%%====================================================================

%% Test recording transport operations with correct API signature
%% API: record_transport_operation(TransportId, TransportType, Operation, Duration)
test_record_transport_operation() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    Result = erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, 15),
    ?assertEqual(ok, Result),
    gen_server:stop(Pid).

%% Test recording server operations with correct API signature
%% API: record_server_operation(ServerId, Operation, Duration, ExtraLabels)
test_record_server_operation() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    Labels = #{<<"result">> => ok},
    Result = erlmcp_metrics:record_server_operation(<<"server_1">>, initialize, 50, Labels),
    ?assertEqual(ok, Result),
    gen_server:stop(Pid).

%% Test recording registry operations with correct API signature
%% API: record_registry_operation(Operation, Duration, ExtraLabels)
test_record_registry_operation() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    Labels = #{},
    Result = erlmcp_metrics:record_registry_operation(register, 10, Labels),
    ?assertEqual(ok, Result),
    gen_server:stop(Pid).

%%====================================================================
%% Query Tests
%%====================================================================

%% Test get_metrics returns a list of metric records
test_get_metrics() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, 10),

    % Get all metrics - returns a list
    Result = erlmcp_metrics:get_metrics(),
    ?assert(is_list(Result)),
    ?assertEqual(1, length(Result)),

    % Verify first metric has expected name (observable behavior)
    [FirstMetric | _] = Result,
    ?assert(is_map(FirstMetric)),
    ?assertEqual(<<"transport_operation_duration_ms">>, maps:get(name, FirstMetric)),

    gen_server:stop(Pid).

%% Test get_metrics/1 filters by metric name
test_get_metrics_by_name() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, 15),
    erlmcp_metrics:record_server_operation(<<"server_1">>, call, 20, #{}),

    % Query by specific metric name
    TransportMetrics = erlmcp_metrics:get_metrics(<<"transport_operation_duration_ms">>),
    ?assertEqual(1, length(TransportMetrics)),

    ServerMetrics = erlmcp_metrics:get_metrics(<<"server_operation_duration_ms">>),
    ?assertEqual(1, length(ServerMetrics)),

    % Non-existent metric returns empty list
    EmptyMetrics = erlmcp_metrics:get_metrics(<<"nonexistent">>),
    ?assertEqual(0, length(EmptyMetrics)),

    gen_server:stop(Pid).

%%====================================================================
%% Lifecycle Tests
%%====================================================================

%% Test reset_metrics clears all recorded metrics
test_reset_metrics() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, 15),

    % Verify metric was recorded
    Metrics1 = erlmcp_metrics:get_metrics(),
    ?assertEqual(1, length(Metrics1)),

    % Reset metrics
    ok = erlmcp_metrics:reset_metrics(),

    % Verify metrics are cleared
    Metrics2 = erlmcp_metrics:get_metrics(),
    ?assertEqual(0, length(Metrics2)),

    gen_server:stop(Pid).

%% Test full lifecycle with multiple metric types
metrics_lifecycle_test_() ->
    {setup,
     fun setup_metrics/0,
     fun cleanup_metrics/1,
     fun(_Pid) ->
         [
          ?_test(begin
              % Record multiple metric types
              ok = erlmcp_metrics:record_transport_operation(<<"tcp_conn_1">>, tcp, send, 15),
              ok = erlmcp_metrics:record_server_operation(<<"server_1">>, call, 20, #{<<"result">> => ok}),
              ok = erlmcp_metrics:record_registry_operation(lookup, 5, #{}),

              % Query all metrics
              Metrics = erlmcp_metrics:get_metrics(),
              ?assert(is_list(Metrics)),
              ?assertEqual(3, length(Metrics)),

              % Reset
              ok = erlmcp_metrics:reset_metrics(),

              % Verify cleared
              EmptyMetrics = erlmcp_metrics:get_metrics(),
              ?assertEqual(0, length(EmptyMetrics))
          end)
         ]
     end}.

%%====================================================================
%% Helper Function Tests
%%====================================================================

%% Test with_metrics helper function
test_with_metrics() ->
    {ok, Pid} = erlmcp_metrics:start_link(),
    Labels = #{<<"test">> => true},

    % Execute function with metrics recording
    Result = erlmcp_metrics:with_metrics(<<"test_operation">>, Labels, fun() ->
        timer:sleep(10),
        success
    end),

    % Verify function result is returned
    ?assertEqual(success, Result),

    % Verify metric was recorded
    Metrics = erlmcp_metrics:get_metrics(<<"test_operation">>),
    ?assertEqual(1, length(Metrics)),

    % Verify duration was recorded (observable behavior via value field)
    [FirstMetric | _] = Metrics,
    Duration = maps:get(value, FirstMetric),
    ?assert(Duration >= 10),

    gen_server:stop(Pid).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%% Test recording with empty labels
record_empty_labels_test_() ->
    {setup,
     fun setup_metrics/0,
     fun cleanup_metrics/1,
     fun(_Pid) ->
         [
          ?_test(begin
              % Record with empty labels
              ok = erlmcp_metrics:record_transport_operation(<<"tcp">>, tcp, send, 10),
              Metrics = erlmcp_metrics:get_metrics(),
              ?assertEqual(1, length(Metrics))
          end)
         ]
     end}.

%% Test multiple recordings of same metric
multiple_recordings_test_() ->
    {setup,
     fun setup_metrics/0,
     fun cleanup_metrics/1,
     fun(_Pid) ->
         [
          ?_test(begin
              % Record same metric multiple times
              ok = erlmcp_metrics:record_transport_operation(<<"tcp_1">>, tcp, send, 10),
              ok = erlmcp_metrics:record_transport_operation(<<"tcp_2">>, tcp, recv, 15),
              ok = erlmcp_metrics:record_transport_operation(<<"tcp_3">>, tcp, send, 20),

              % All should be recorded
              Metrics = erlmcp_metrics:get_metrics(),
              ?assertEqual(3, length(Metrics))
          end)
         ]
     end}.

%% Test with_metrics with exception
with_metrics_exception_test_() ->
    {setup,
     fun setup_metrics/0,
     fun cleanup_metrics/1,
     fun(_Pid) ->
         [
          ?_test(begin
              % Function that throws an error
              ErrorFun = fun() -> throw(test_error) end,

              % Error should be re-raised
              ?assertThrow(test_error, erlmcp_metrics:with_metrics(<<"error_op">>, #{}, ErrorFun)),

              % Metric should still be recorded despite error
              Metrics = erlmcp_metrics:get_metrics(<<"error_op">>),
              ?assertEqual(1, length(Metrics))
          end)
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

%% Test complete metrics workflow
metrics_workflow_test_() ->
    {setup,
     fun setup_metrics/0,
     fun cleanup_metrics/1,
     fun(_Pid) ->
         [
          ?_test(begin
              % Simulate a complete request workflow with metrics

              % Record transport operation
              ok = erlmcp_metrics:record_transport_operation(<<"conn_1">>, tcp, send, 5),

              % Record server operation
              ok = erlmcp_metrics:record_server_operation(
                  <<"server_1">>,
                  initialize,
                  10,
                  #{<<"result">> => ok}
              ),

              % Use with_metrics helper
              Result = erlmcp_metrics:with_metrics(
                  <<"process_request">>,
                  #{<<"request_type">> => <<"tool_call">>},
                  fun() ->
                          timer:sleep(5),
                          {ok, processed}
                  end
              ),
              ?assertEqual({ok, processed}, Result),

              % Record registry operation
              ok = erlmcp_metrics:record_registry_operation(lookup, 3, #{}),

              % Verify all metrics were recorded
              AllMetrics = erlmcp_metrics:get_metrics(),
              ?assertEqual(4, length(AllMetrics)),

              % Verify specific metric types
              TransportMetrics = erlmcp_metrics:get_metrics(<<"transport_operation_duration_ms">>),
              ?assertEqual(1, length(TransportMetrics)),

              ServerMetrics = erlmcp_metrics:get_metrics(<<"server_operation_duration_ms">>),
              ?assertEqual(1, length(ServerMetrics)),

              RequestMetrics = erlmcp_metrics:get_metrics(<<"process_request">>),
              ?assertEqual(1, length(RequestMetrics)),

              RegistryMetrics = erlmcp_metrics:get_metrics(<<"registry_operation_duration_ms">>),
              ?assertEqual(1, length(RegistryMetrics))
          end)
         ]
     end}.
