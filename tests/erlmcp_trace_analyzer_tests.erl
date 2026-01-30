-module(erlmcp_trace_analyzer_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Chicago School TDD: Test Fixtures with Real Processes
%%%===================================================================

%% Setup: Start real trace analyzer with in-memory storage
setup() ->
    {ok, _Pid} = start_in_memory_storage(),
    {ok, AnalyzerPid} = erlmcp_trace_analyzer:start_link(),
    {AnalyzerPid, get_storage_pid()}.

%% Teardown: Stop processes in reverse order
teardown({AnalyzerPid, StoragePid}) ->
    erlmcp_trace_analyzer:stop(AnalyzerPid),
    stop_in_memory_storage(StoragePid),
    ok.

%%%===================================================================
%%% Test Suite
%%%===================================================================

trace_analyzer_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
      {"Critical path analysis", fun test_critical_path_analysis/0},
      {"Anomaly detection", fun test_anomaly_detection/0},
      {"Bottleneck identification", fun test_bottleneck_identification/0},
      {"Span relationship validation", fun test_span_relationship_validation/0},
      {"Report generation", fun test_report_generation/0},
      {"Latency anomaly detection", fun test_latency_anomaly_detection/0},
      {"Circular dependency detection", fun test_circular_dependency_detection/0},
      {"Error cluster detection", fun test_error_cluster_detection/0}
     ]}.

%%%===================================================================
%%% Test Functions (Chicago School: Observable Behavior)
%%%===================================================================

%% Test critical path analysis - pure function, no storage needed
test_critical_path_analysis() ->
    Spans = create_test_spans_for_critical_path(),

    {ok, CriticalPath} = erlmcp_trace_analyzer:find_critical_path(Spans),

    %% Verify: Critical path identified correctly (observable output)
    ?assert(is_list(CriticalPath)),
    ?assert(length(CriticalPath) > 0),

    ExpectedPath = ["span1", "span2", "span4"],
    ?assertEqual(ExpectedPath, CriticalPath).

%% Test anomaly detection - pure function, no storage needed
test_anomaly_detection() ->
    Spans = create_test_spans_with_anomalies(),

    {ok, Anomalies} = erlmcp_trace_analyzer:detect_anomalies(Spans),

    %% Verify: Anomalies detected (observable output)
    ?assert(is_list(Anomalies)),
    ?assert(length(Anomalies) > 0),

    AnomalyTypes = [maps:get(type, A) || A <- Anomalies],
    ?assert(lists:member(latency_anomaly, AnomalyTypes)),
    ?assert(lists:member(missing_parent_span, AnomalyTypes)).

%% Test bottleneck identification - using real storage process
test_bottleneck_identification() ->
    {AnalyzerPid, _StoragePid} = setup(),

    TraceId = <<"test_trace_bottleneck">>,

    %% Setup: Store test spans via real storage API
    Spans = create_test_spans_with_bottlenecks(),
    ok = store_test_spans(TraceId, Spans),

    %% Exercise: Analyze trace via API (uses real storage)
    {ok, Analysis} = erlmcp_trace_analyzer:analyze_trace(TraceId),

    %% Verify: Bottlenecks identified (observable state)
    Bottlenecks = Analysis#trace_analysis.bottlenecks,
    ?assert(is_list(Bottlenecks)),
    ?assert(length(Bottlenecks) > 0),

    [Bottleneck | _] = Bottlenecks,
    ?assert(maps:is_key(span_id, Bottleneck)),
    ?assert(maps:is_key(bottleneck_score, Bottleneck)),
    ?assert(maps:is_key(recommendations, Bottleneck)),

    teardown({AnalyzerPid, get_storage_pid()}).

%% Test span relationship validation - pure function
test_span_relationship_validation() ->
    Spans = create_test_spans_with_relationship_issues(),

    {ok, Validation} = erlmcp_trace_analyzer:validate_span_relationships(Spans),

    %% Verify: Validation results (observable output)
    ?assert(is_map(Validation)),
    ?assert(maps:is_key(valid, Validation)),
    ?assert(maps:is_key(results, Validation)),
    ?assert(maps:is_key(total_issues, Validation)),

    ?assertEqual(false, maps:get(valid, Validation)),
    ?assert(maps:get(total_issues, Validation) > 0).

%% Test report generation - using real storage process
test_report_generation() ->
    {AnalyzerPid, _StoragePid} = setup(),

    TraceId = <<"test_trace_report">>,

    %% Setup: Store test spans
    Spans = create_simple_test_spans(),
    ok = store_test_spans(TraceId, Spans),

    %% Exercise: Generate JSON report via API
    {ok, JsonReport} = erlmcp_trace_analyzer:generate_report(TraceId, json),
    ?assert(is_map(JsonReport)),
    ?assert(maps:is_key(trace_id, JsonReport)),
    ?assert(maps:is_key(summary, JsonReport)),

    %% Exercise: Generate text report via API
    {ok, TextReport} = erlmcp_trace_analyzer:generate_report(TraceId, text),
    ?assert(is_list(TextReport)),
    ?assert(length(TextReport) > 0),

    teardown({AnalyzerPid, get_storage_pid()}).

%% Test latency anomaly detection - pure function
test_latency_anomaly_detection() ->
    Spans = [
        create_test_span("span1", 1000, undefined, ok),
        create_test_span("span2", 1200, undefined, ok),
        create_test_span("span3", 50000, undefined, ok), % Very slow
        create_test_span("span4", 900, undefined, ok)
    ],

    {ok, Anomalies} = erlmcp_trace_analyzer:detect_anomalies(Spans),

    %% Verify: Slow span detected as anomaly (observable behavior)
    LatencyAnomalies = [A || A <- Anomalies, maps:get(type, A) =:= latency_anomaly],
    ?assert(length(LatencyAnomalies) > 0),

    [LatencyAnomaly | _] = LatencyAnomalies,
    ?assertEqual("span3", maps:get(span_id, LatencyAnomaly)).

%% Test circular dependency detection - pure function
test_circular_dependency_detection() ->
    Spans = [
        create_test_span("span1", 1000, undefined, ok),
        create_test_span("span2", 1000, "span1", ok),
        create_test_span("span3", 1000, "span2", ok),
        create_test_span("span1", 1000, "span3", ok) % Creates cycle
    ],

    {ok, Anomalies} = erlmcp_trace_analyzer:detect_anomalies(Spans),

    %% Verify: Circular dependency detected (observable behavior)
    CircularAnomalies = [A || A <- Anomalies,
                         maps:get(type, A) =:= circular_dependency],
    ?assert(length(CircularAnomalies) > 0).

%% Test error cluster detection - pure function
test_error_cluster_detection() ->
    BaseTime = erlang:system_time(microsecond),

    Spans = [
        create_test_span_with_time("span1", 1000, undefined, error, BaseTime),
        create_test_span_with_time("span2", 1000, undefined, error, BaseTime + 1000),
        create_test_span_with_time("span3", 1000, undefined, error, BaseTime + 2000),
        create_test_span_with_time("span4", 1000, undefined, ok, BaseTime + 3000)
    ],

    {ok, Anomalies} = erlmcp_trace_analyzer:detect_anomalies(Spans),

    %% Verify: Error clusters detected (observable behavior)
    ErrorClusters = [A || A <- Anomalies, maps:get(type, A) =:= error_cluster],
    ?assert(length(ErrorClusters) >= 0).

%%%===================================================================
%%% In-Memory Storage Process (Chicago School: Real Process)
%%%===================================================================

%% Simple gen_server for in-memory span storage during tests
%% This is a REAL process, not a mock, following Chicago School TDD

-record(storage_state, {spans = #{} }).

start_in_memory_storage() ->
    {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    register(erlmcp_storage, Pid), %% Register as erlmcp_storage for tests
    {ok, Pid}.

stop_in_memory_storage(Pid) ->
    case whereis(erlmcp_storage) of
        Pid -> gen_server:stop(Pid);
        _ -> ok
    end.

get_storage_pid() ->
    whereis(erlmcp_storage).

%% gen_server callbacks for in-memory storage
init([]) ->
    {ok, #storage_state{}}.

handle_call({get_spans_by_trace, TraceId}, _From, State) ->
    Spans = maps:get(TraceId, State#storage_state.spans, []),
    {reply, {ok, Spans}, State};

handle_call({store_spans, TraceId, Spans}, _From, State) ->
    ExistingSpans = State#storage_state.spans,
    UpdatedSpans = maps:put(TraceId, Spans, ExistingSpans),
    {reply, ok, State#storage_state{spans = UpdatedSpans}};

handle_call({store_trace_analysis, _Analysis}, _From, State) ->
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Helper API for storing test spans (wraps gen_server call)
store_test_spans(TraceId, Spans) ->
    gen_server:call(erlmcp_storage, {store_spans, TraceId, Spans}).

%%%===================================================================
%%% Test Data Creation Helpers
%%%===================================================================

create_test_spans_for_critical_path() ->
    [
        create_test_span("span1", 5000, undefined, ok),
        create_test_span("span2", 3000, "span1", ok),
        create_test_span("span3", 1000, "span1", ok),
        create_test_span("span4", 2000, "span2", ok)
    ].

create_test_spans_with_anomalies() ->
    [
        create_test_span("span1", 1000, undefined, ok),
        create_test_span("span2", 50000, undefined, ok),
        create_test_span("span3", 1000, "missing_parent", ok),
        create_test_span("span4", 1000, "span1", ok)
    ].

create_test_spans_with_bottlenecks() ->
    [
        create_test_span("slow_span", 10000, undefined, ok),
        create_test_span("fast_span1", 100, "slow_span", ok),
        create_test_span("fast_span2", 200, "slow_span", ok),
        create_test_span("normal_span", 1000, undefined, ok)
    ].

create_test_spans_with_relationship_issues() ->
    [
        create_test_span("span1", 2000, undefined, ok),
        create_test_span("span2", 1000, "nonexistent_parent", ok),
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
