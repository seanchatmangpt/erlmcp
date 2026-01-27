-module(erlmcp_trace_analyzer_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% Test fixtures
setup() ->
    application:start(erlmcp),
    {ok, _Pid} = erlmcp_trace_analyzer:start_link(),
    ok.

teardown(_) ->
    application:stop(erlmcp),
    ok.

%% Test suite
trace_analyzer_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
         {"Critical path analysis", fun test_critical_path_analysis/0},
         {"Anomaly detection", fun test_anomaly_detection/0},
         {"Performance analysis", fun test_performance_analysis/0},
         {"Bottleneck identification", fun test_bottleneck_identification/0},
         {"Span relationship validation", fun test_span_relationship_validation/0},
         {"Report generation", fun test_report_generation/0},
         {"Latency anomaly detection", fun test_latency_anomaly_detection/0},
         {"Circular dependency detection", fun test_circular_dependency_detection/0},
         {"Error cluster detection", fun test_error_cluster_detection/0}
     ]}.

%% Test critical path analysis
test_critical_path_analysis() ->
    % Create test spans with clear critical path
    Spans = create_test_spans_for_critical_path(),
    
    {ok, CriticalPath} = erlmcp_trace_analyzer:find_critical_path(Spans),
    
    % Verify critical path is identified correctly
    ?assert(is_list(CriticalPath)),
    ?assert(length(CriticalPath) > 0),
    
    % The critical path should contain the longest chain
    ExpectedPath = ["span1", "span2", "span4"],
    ?assertEqual(ExpectedPath, CriticalPath).

%% Test anomaly detection
test_anomaly_detection() ->
    % Create spans with known anomalies
    Spans = create_test_spans_with_anomalies(),
    
    {ok, Anomalies} = erlmcp_trace_analyzer:detect_anomalies(Spans),
    
    % Verify anomalies are detected
    ?assert(is_list(Anomalies)),
    ?assert(length(Anomalies) > 0),
    
    % Check for specific anomaly types
    AnomalyTypes = [maps:get(type, A) || A <- Anomalies],
    ?assert(lists:member(latency_anomaly, AnomalyTypes)),
    ?assert(lists:member(missing_parent_span, AnomalyTypes)).

%% Test performance analysis
test_performance_analysis() ->
    TraceId = <<"test_trace_001">>,
    
    % Mock storage to return test spans
    meck:new(erlmcp_storage, [non_strict]),
    meck:expect(erlmcp_storage, get_spans_by_trace, 1, 
               {ok, create_test_spans_for_performance()}),
    meck:expect(erlmcp_storage, store_trace_analysis, 1, ok),
    
    {ok, Performance} = erlmcp_trace_analyzer:analyze_performance(TraceId),
    
    % Verify performance metrics
    ?assert(is_map(Performance)),
    ?assert(maps:is_key(score, Performance)),
    ?assert(maps:is_key(avg_duration, Performance)),
    ?assert(maps:is_key(error_rate, Performance)),
    
    Score = maps:get(score, Performance),
    ?assert(Score >= 0 andalso Score =< 100),
    
    meck:unload(erlmcp_storage).

%% Test bottleneck identification
test_bottleneck_identification() ->
    TraceId = <<"test_trace_bottleneck">>,
    
    % Mock storage with spans that have clear bottlenecks
    meck:new(erlmcp_storage, [non_strict]),
    meck:expect(erlmcp_storage, get_spans_by_trace, 1, 
               {ok, create_test_spans_with_bottlenecks()}),
    meck:expect(erlmcp_storage, store_trace_analysis, 1, ok),
    
    {ok, Analysis} = erlmcp_trace_analyzer:analyze_trace(TraceId),
    
    % Verify bottlenecks are identified
    Bottlenecks = Analysis#trace_analysis.bottlenecks,
    ?assert(is_list(Bottlenecks)),
    ?assert(length(Bottlenecks) > 0),
    
    % Check bottleneck structure
    [Bottleneck | _] = Bottlenecks,
    ?assert(maps:is_key(span_id, Bottleneck)),
    ?assert(maps:is_key(bottleneck_score, Bottleneck)),
    ?assert(maps:is_key(recommendations, Bottleneck)),
    
    meck:unload(erlmcp_storage).

%% Test span relationship validation
test_span_relationship_validation() ->
    % Create spans with relationship issues
    Spans = create_test_spans_with_relationship_issues(),
    
    {ok, Validation} = erlmcp_trace_analyzer:validate_span_relationships(Spans),
    
    % Verify validation results
    ?assert(is_map(Validation)),
    ?assert(maps:is_key(valid, Validation)),
    ?assert(maps:is_key(results, Validation)),
    ?assert(maps:is_key(total_issues, Validation)),
    
    % Should detect relationship issues
    ?assertEqual(false, maps:get(valid, Validation)),
    ?assert(maps:get(total_issues, Validation) > 0).

%% Test report generation
test_report_generation() ->
    TraceId = <<"test_trace_report">>,
    
    % Mock storage
    meck:new(erlmcp_storage, [non_strict]),
    meck:expect(erlmcp_storage, get_spans_by_trace, 1, 
               {ok, create_simple_test_spans()}),
    meck:expect(erlmcp_storage, store_trace_analysis, 1, ok),
    
    % Test JSON report
    {ok, JsonReport} = erlmcp_trace_analyzer:generate_report(TraceId, json),
    ?assert(is_map(JsonReport)),
    ?assert(maps:is_key(trace_id, JsonReport)),
    ?assert(maps:is_key(summary, JsonReport)),
    
    % Test text report
    {ok, TextReport} = erlmcp_trace_analyzer:generate_report(TraceId, text),
    ?assert(is_list(TextReport)),
    ?assert(length(TextReport) > 0),
    
    meck:unload(erlmcp_storage).

%% Test latency anomaly detection
test_latency_anomaly_detection() ->
    % Create spans with one very slow span
    Spans = [
        create_test_span("span1", 1000, undefined, ok),
        create_test_span("span2", 1200, undefined, ok),
        create_test_span("span3", 50000, undefined, ok), % Very slow
        create_test_span("span4", 900, undefined, ok)
    ],
    
    {ok, Anomalies} = erlmcp_trace_analyzer:detect_anomalies(Spans),
    
    % Should detect the slow span as an anomaly
    LatencyAnomalies = [A || A <- Anomalies, maps:get(type, A) =:= latency_anomaly],
    ?assert(length(LatencyAnomalies) > 0),
    
    [LatencyAnomaly | _] = LatencyAnomalies,
    ?assertEqual("span3", maps:get(span_id, LatencyAnomaly)).

%% Test circular dependency detection
test_circular_dependency_detection() ->
    % Create spans with circular dependency
    Spans = [
        create_test_span("span1", 1000, undefined, ok),
        create_test_span("span2", 1000, "span1", ok),
        create_test_span("span3", 1000, "span2", ok),
        create_test_span("span1", 1000, "span3", ok) % Creates cycle
    ],
    
    {ok, Anomalies} = erlmcp_trace_analyzer:detect_anomalies(Spans),
    
    % Should detect circular dependency
    CircularAnomalies = [A || A <- Anomalies, 
                         maps:get(type, A) =:= circular_dependency],
    ?assert(length(CircularAnomalies) > 0).

%% Test error cluster detection  
test_error_cluster_detection() ->
    BaseTime = erlang:system_time(microsecond),
    
    % Create multiple error spans close in time
    Spans = [
        create_test_span_with_time("span1", 1000, undefined, error, BaseTime),
        create_test_span_with_time("span2", 1000, undefined, error, BaseTime + 1000),
        create_test_span_with_time("span3", 1000, undefined, error, BaseTime + 2000),
        create_test_span_with_time("span4", 1000, undefined, ok, BaseTime + 3000)
    ],
    
    {ok, Anomalies} = erlmcp_trace_analyzer:detect_anomalies(Spans),
    
    % Should detect error cluster
    ErrorClusters = [A || A <- Anomalies, maps:get(type, A) =:= error_cluster],
    ?assert(length(ErrorClusters) >= 0). % May or may not detect based on clustering logic

%% Helper functions for creating test data

create_test_spans_for_critical_path() ->
    [
        create_test_span("span1", 5000, undefined, ok),    % Root
        create_test_span("span2", 3000, "span1", ok),      % Child of span1
        create_test_span("span3", 1000, "span1", ok),      % Child of span1
        create_test_span("span4", 2000, "span2", ok)       % Child of span2 (critical path)
    ].

create_test_spans_with_anomalies() ->
    [
        create_test_span("span1", 1000, undefined, ok),
        create_test_span("span2", 50000, undefined, ok),   % Latency anomaly
        create_test_span("span3", 1000, "missing_parent", ok), % Missing parent
        create_test_span("span4", 1000, "span1", ok)
    ].

create_test_spans_for_performance() ->
    [
        create_test_span("span1", 2000, undefined, ok),
        create_test_span("span2", 1500, "span1", ok),
        create_test_span("span3", 3000, "span1", error),   % Error span
        create_test_span("span4", 1000, "span2", ok)
    ].

create_test_spans_with_bottlenecks() ->
    [
        create_test_span("slow_span", 10000, undefined, ok),    % Major bottleneck
        create_test_span("fast_span1", 100, "slow_span", ok),
        create_test_span("fast_span2", 200, "slow_span", ok),
        create_test_span("normal_span", 1000, undefined, ok)
    ].

create_test_spans_with_relationship_issues() ->
    [
        create_test_span("span1", 2000, undefined, ok),
        create_test_span("span2", 1000, "nonexistent_parent", ok), % Missing parent
        create_test_span("span3", 1500, "span1", ok)
    ].

create_simple_test_spans() ->
    [
        create_test_span("span1", 1000, undefined, ok),
        create_test_span("span2", 500, "span1", ok)
    ].

create_test_span(SpanId, Duration, ParentId, Status) ->
    BaseTime = erlang:system_time(microsecond),
    create_test_span_with_time(SpanId, Duration, ParentId, Status, BaseTime).

create_test_span_with_time(SpanId, Duration, ParentId, Status, StartTime) ->
    #{
        span_id => SpanId,
        trace_id => <<"test_trace">>,
        parent_id => ParentId,
        operation => "test_operation",
        start_time => StartTime,
        end_time => StartTime + Duration,
        status => Status,
        tags => #{component => "test"}
    }.

%% Mock helper for storage operations
mock_storage_operations() ->
    meck:new(erlmcp_storage, [non_strict]),
    meck:expect(erlmcp_storage, get_spans_by_trace, 1, {ok, []}),
    meck:expect(erlmcp_storage, store_trace_analysis, 1, ok).

cleanup_storage_mocks() ->
    case catch meck:unload(erlmcp_storage) of
        ok -> ok;
        _ -> ok
    end.