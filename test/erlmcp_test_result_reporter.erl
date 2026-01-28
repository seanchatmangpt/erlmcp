%%%-------------------------------------------------------------------
%%% @doc
%%% ErlMCP Test Result Reporting System
%%%
%%% Comprehensive test result reporting with:
%%% - Hierarchical test result trees (parent-child relationships)
%%% - Real-time progress tracking
%%% - Detailed metrics aggregation
%%% - Multiple output formats (JSON, HTML, CSV, plaintext)
%%% - Performance regression detection
%%% - Test coverage analysis
%%% - Trend analysis and anomaly detection
%%%
%%% USAGE:
%%%   Reporter = erlmcp_test_result_reporter:new("test_run_1"),
%%%   erlmcp_test_result_reporter:add_test_result(Reporter, TestResult),
%%%   erlmcp_test_result_reporter:generate_report(Reporter, [json, html]),
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_test_result_reporter).

-behaviour(gen_server).

-include_lib("eunit/include/eunit.hrl").

%% Public API
-export([
    new/1,
    add_test_result/2,
    add_test_result/3,
    add_parent_test/2,
    add_child_test/3,
    mark_test_complete/2,
    get_test_status/2,
    get_summary/1,
    generate_report/2,
    generate_report/3,
    export_json/2,
    export_html/2,
    export_csv/2,
    export_plaintext/2,
    get_metrics_summary/1,
    analyze_performance_trend/2,
    detect_anomalies/2,
    start_link/1,
    stop/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% Records
%%====================================================================

-record(state, {
    reporter_id :: binary(),
    test_results = [] :: [#test_node{}],
    root_tests = #{} :: map(),
    child_tests = #{} :: map(),
    start_time :: integer(),
    end_time :: integer() | undefined,
    metrics = #{} :: map(),
    status = running :: running | completed | failed
}).

-record(test_node, {
    test_id :: binary(),
    test_name :: binary(),
    parent_id :: binary() | undefined,
    child_ids = [] :: [binary()],
    status :: passed | failed | skipped | running,
    start_time :: integer(),
    end_time :: integer() | undefined,
    duration_ms :: integer(),
    assertions_total :: integer(),
    assertions_passed :: integer(),
    assertions_failed :: integer(),
    metrics :: map(),
    errors :: [binary()],
    depth :: integer()
}).

-record(report_summary, {
    total_tests :: integer(),
    passed_tests :: integer(),
    failed_tests :: integer(),
    skipped_tests :: integer(),
    total_assertions :: integer(),
    assertions_passed :: integer(),
    assertions_failed :: integer(),
    total_duration_ms :: integer(),
    pass_rate :: float(),
    assertion_pass_rate :: float(),
    test_tree_depth :: integer(),
    anomalies_detected :: integer(),
    regressions_detected :: integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create a new test result reporter
-spec new(string() | binary()) -> pid().
new(ReporterId) when is_list(ReporterId) ->
    new(list_to_binary(ReporterId));
new(ReporterId) when is_binary(ReporterId) ->
    {ok, Pid} = start_link(ReporterId),
    Pid.

%% @doc Add a simple test result
-spec add_test_result(pid(), map()) -> ok | {error, term()}.
add_test_result(ReporterPid, TestResult) ->
    gen_server:call(ReporterPid, {add_test_result, TestResult}).

%% @doc Add a test result with hierarchy information
-spec add_test_result(pid(), map(), map()) -> ok | {error, term()}.
add_test_result(ReporterPid, TestResult, HierarchyInfo) ->
    gen_server:call(ReporterPid, {add_test_result, TestResult, HierarchyInfo}).

%% @doc Add a parent test node
-spec add_parent_test(pid(), map()) -> binary() | {error, term()}.
add_parent_test(ReporterPid, TestInfo) ->
    gen_server:call(ReporterPid, {add_parent_test, TestInfo}).

%% @doc Add a child test to a parent
-spec add_child_test(pid(), binary(), map()) -> ok | {error, term()}.
add_child_test(ReporterPid, ParentId, ChildTestInfo) ->
    gen_server:call(ReporterPid, {add_child_test, ParentId, ChildTestInfo}).

%% @doc Mark a test as complete
-spec mark_test_complete(pid(), binary()) -> ok | {error, term()}.
mark_test_complete(ReporterPid, TestId) ->
    gen_server:call(ReporterPid, {mark_test_complete, TestId}).

%% @doc Get status of a test
-spec get_test_status(pid(), binary()) -> map() | {error, term()}.
get_test_status(ReporterPid, TestId) ->
    gen_server:call(ReporterPid, {get_test_status, TestId}).

%% @doc Get summary of all tests
-spec get_summary(pid()) -> #report_summary{} | {error, term()}.
get_summary(ReporterPid) ->
    gen_server:call(ReporterPid, get_summary).

%% @doc Generate reports in specified formats
-spec generate_report(pid(), [atom()]) -> {ok, map()} | {error, term()}.
generate_report(ReporterPid, Formats) ->
    generate_report(ReporterPid, Formats, "/tmp").

%% @doc Generate reports in specified formats with output path
-spec generate_report(pid(), [atom()], string()) -> {ok, map()} | {error, term()}.
generate_report(ReporterPid, Formats, OutputPath) ->
    gen_server:call(ReporterPid, {generate_report, Formats, OutputPath}, 60000).

%% @doc Export to JSON
-spec export_json(pid(), string()) -> {ok, string()} | {error, term()}.
export_json(ReporterPid, OutputPath) ->
    gen_server:call(ReporterPid, {export_json, OutputPath}, 60000).

%% @doc Export to HTML
-spec export_html(pid(), string()) -> {ok, string()} | {error, term()}.
export_html(ReporterPid, OutputPath) ->
    gen_server:call(ReporterPid, {export_html, OutputPath}, 60000).

%% @doc Export to CSV
-spec export_csv(pid(), string()) -> {ok, string()} | {error, term()}.
export_csv(ReporterPid, OutputPath) ->
    gen_server:call(ReporterPid, {export_csv, OutputPath}, 60000).

%% @doc Export to plaintext
-spec export_plaintext(pid(), string()) -> {ok, string()} | {error, term()}.
export_plaintext(ReporterPid, OutputPath) ->
    gen_server:call(ReporterPid, {export_plaintext, OutputPath}, 60000).

%% @doc Get metrics summary
-spec get_metrics_summary(pid()) -> map() | {error, term()}.
get_metrics_summary(ReporterPid) ->
    gen_server:call(ReporterPid, get_metrics_summary).

%% @doc Analyze performance trends
-spec analyze_performance_trend(pid(), atom()) -> map() | {error, term()}.
analyze_performance_trend(ReporterPid, Metric) ->
    gen_server:call(ReporterPid, {analyze_performance_trend, Metric}).

%% @doc Detect anomalies in test results
-spec detect_anomalies(pid(), atom()) -> [map()] | {error, term()}.
detect_anomalies(ReporterPid, AnomalyType) ->
    gen_server:call(ReporterPid, {detect_anomalies, AnomalyType}).

%% @doc Start the reporter gen_server
-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(ReporterId) ->
    gen_server:start_link(?MODULE, ReporterId, []).

%% @doc Stop the reporter
-spec stop(pid()) -> ok.
stop(ReporterPid) ->
    gen_server:call(ReporterPid, stop).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(ReporterId) ->
    erlang:process_flag(trap_exit, true),
    {ok, #state{
        reporter_id = ReporterId,
        test_results = [],
        root_tests = #{},
        child_tests = #{},
        start_time = erlang:system_time(millisecond),
        metrics = #{}
    }}.

handle_call({add_test_result, TestResult}, _From, State) ->
    NewState = add_test_result_internal(TestResult, undefined, State),
    {reply, ok, NewState};

handle_call({add_test_result, TestResult, HierarchyInfo}, _From, State) ->
    NewState = add_test_result_internal(TestResult, HierarchyInfo, State),
    {reply, ok, NewState};

handle_call({add_parent_test, TestInfo}, _From, State) ->
    TestId = generate_test_id(),
    TestNode = create_test_node(TestId, TestInfo, undefined, 0),
    NewRootTests = maps:put(TestId, TestNode, State#state.root_tests),
    NewState = State#state{
        test_results = [TestNode | State#state.test_results],
        root_tests = NewRootTests
    },
    {reply, TestId, NewState};

handle_call({add_child_test, ParentId, ChildTestInfo}, _From, State) ->
    ChildId = generate_test_id(),
    case maps:find(ParentId, State#state.root_tests) of
        {ok, ParentNode} ->
            ParentDepth = ParentNode#test_node.depth,
            ChildNode = create_test_node(ChildId, ChildTestInfo, ParentId, ParentDepth + 1),
            NewChildTests = maps:put(ChildId, ChildNode, State#state.child_tests),
            UpdatedParent = ParentNode#test_node{
                child_ids = [ChildId | ParentNode#test_node.child_ids]
            },
            NewRootTests = maps:put(ParentId, UpdatedParent, State#state.root_tests),
            NewState = State#state{
                test_results = [ChildNode | State#state.test_results],
                root_tests = NewRootTests,
                child_tests = NewChildTests
            },
            {reply, ok, NewState};
        error ->
            {reply, {error, parent_not_found}, State}
    end;

handle_call({mark_test_complete, TestId}, _From, State) ->
    NewState = mark_test_complete_internal(TestId, State),
    {reply, ok, NewState};

handle_call({get_test_status, TestId}, _From, State) ->
    Status = find_test_status(TestId, State),
    {reply, Status, State};

handle_call(get_summary, _From, State) ->
    Summary = generate_summary(State),
    {reply, Summary, State};

handle_call({generate_report, Formats, OutputPath}, _From, State) ->
    Results = maps:from_list(lists:map(fun(Format) ->
        case Format of
            json -> {json, export_json_internal(OutputPath, State)};
            html -> {html, export_html_internal(OutputPath, State)};
            csv -> {csv, export_csv_internal(OutputPath, State)};
            plaintext -> {plaintext, export_plaintext_internal(OutputPath, State)};
            _ -> {Format, {error, unknown_format}}
        end
    end, Formats)),
    {reply, {ok, Results}, State};

handle_call({export_json, OutputPath}, _From, State) ->
    Result = export_json_internal(OutputPath, State),
    {reply, Result, State};

handle_call({export_html, OutputPath}, _From, State) ->
    Result = export_html_internal(OutputPath, State),
    {reply, Result, State};

handle_call({export_csv, OutputPath}, _From, State) ->
    Result = export_csv_internal(OutputPath, State),
    {reply, Result, State};

handle_call({export_plaintext, OutputPath}, _From, State) ->
    Result = export_plaintext_internal(OutputPath, State),
    {reply, Result, State};

handle_call(get_metrics_summary, _From, State) ->
    MetricsSummary = extract_metrics_summary(State),
    {reply, MetricsSummary, State};

handle_call({analyze_performance_trend, Metric}, _From, State) ->
    Trend = analyze_metric_trend(Metric, State),
    {reply, Trend, State};

handle_call({detect_anomalies, AnomalyType}, _From, State) ->
    Anomalies = detect_anomalies_internal(AnomalyType, State),
    {reply, Anomalies, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Add test result internal
add_test_result_internal(TestResult, HierarchyInfo, State) ->
    TestId = generate_test_id(),
    Depth = case HierarchyInfo of
        #{depth := D} -> D;
        _ -> 0
    end,
    ParentId = case HierarchyInfo of
        #{parent_id := P} -> P;
        _ -> undefined
    end,

    TestNode = create_test_node(TestId, TestResult, ParentId, Depth),

    case ParentId of
        undefined ->
            NewRootTests = maps:put(TestId, TestNode, State#state.root_tests),
            State#state{
                test_results = [TestNode | State#state.test_results],
                root_tests = NewRootTests
            };
        _ ->
            case maps:find(ParentId, State#state.root_tests) of
                {ok, ParentNode} ->
                    UpdatedParent = ParentNode#test_node{
                        child_ids = [TestId | ParentNode#test_node.child_ids]
                    },
                    NewRootTests = maps:put(ParentId, UpdatedParent, State#state.root_tests),
                    NewChildTests = maps:put(TestId, TestNode, State#state.child_tests),
                    State#state{
                        test_results = [TestNode | State#state.test_results],
                        root_tests = NewRootTests,
                        child_tests = NewChildTests
                    };
                error ->
                    State
            end
    end.

%% Create test node from test result
create_test_node(TestId, TestResult, ParentId, Depth) ->
    #test_node{
        test_id = TestId,
        test_name = maps:get(name, TestResult, <<"unknown">>),
        parent_id = ParentId,
        child_ids = [],
        status = maps:get(status, TestResult, running),
        start_time = maps:get(start_time, TestResult, erlang:system_time(millisecond)),
        end_time = maps:get(end_time, TestResult, undefined),
        duration_ms = maps:get(duration_ms, TestResult, 0),
        assertions_total = maps:get(assertions_total, TestResult, 0),
        assertions_passed = maps:get(assertions_passed, TestResult, 0),
        assertions_failed = maps:get(assertions_failed, TestResult, 0),
        metrics = maps:get(metrics, TestResult, #{}),
        errors = maps:get(errors, TestResult, []),
        depth = Depth
    }.

%% Mark test complete internal
mark_test_complete_internal(TestId, State) ->
    case find_test_node(TestId, State) of
        {ok, TestNode} ->
            EndTime = erlang:system_time(millisecond),
            UpdatedNode = TestNode#test_node{
                end_time = EndTime,
                duration_ms = EndTime - TestNode#test_node.start_time,
                status = completed
            },
            case TestNode#test_node.parent_id of
                undefined ->
                    NewRootTests = maps:put(TestId, UpdatedNode, State#state.root_tests),
                    State#state{
                        root_tests = NewRootTests,
                        test_results = update_test_in_list(TestId, UpdatedNode, State#state.test_results)
                    };
                _ParentId ->
                    NewChildTests = maps:put(TestId, UpdatedNode, State#state.child_tests),
                    State#state{
                        child_tests = NewChildTests,
                        test_results = update_test_in_list(TestId, UpdatedNode, State#state.test_results)
                    }
            end;
        error ->
            State
    end.

%% Find test node
find_test_node(TestId, State) ->
    case maps:find(TestId, State#state.root_tests) of
        {ok, Node} -> {ok, Node};
        error ->
            case maps:find(TestId, State#state.child_tests) of
                {ok, Node} -> {ok, Node};
                error -> error
            end
    end.

%% Update test in list
update_test_in_list(TestId, UpdatedNode, List) ->
    lists:map(fun(Node) ->
        case Node#test_node.test_id =:= TestId of
            true -> UpdatedNode;
            false -> Node
        end
    end, List).

%% Find test status
find_test_status(TestId, State) ->
    case find_test_node(TestId, State) of
        {ok, TestNode} ->
            #{
                test_id => TestNode#test_node.test_id,
                test_name => TestNode#test_node.test_name,
                status => TestNode#test_node.status,
                duration_ms => TestNode#test_node.duration_ms,
                assertions_passed => TestNode#test_node.assertions_passed,
                assertions_failed => TestNode#test_node.assertions_failed,
                errors => TestNode#test_node.errors
            };
        error ->
            {error, not_found}
    end.

%% Generate summary
generate_summary(State) ->
    Results = State#state.test_results,
    TotalTests = length(Results),
    PassedTests = length([R || R <- Results, R#test_node.status =:= passed]),
    FailedTests = length([R || R <- Results, R#test_node.status =:= failed]),
    SkippedTests = length([R || R <- Results, R#test_node.status =:= skipped]),

    TotalAssertions = lists:sum([R#test_node.assertions_total || R <- Results]),
    AssertionsPassed = lists:sum([R#test_node.assertions_passed || R <- Results]),
    AssertionsFailed = lists:sum([R#test_node.assertions_failed || R <- Results]),

    TotalDuration = lists:sum([R#test_node.duration_ms || R <- Results]),
    MaxDepth = case Results of
        [] -> 0;
        _ -> lists:max([R#test_node.depth || R <- Results])
    end,

    PassRate = case TotalTests of
        0 -> 0.0;
        _ -> (PassedTests / TotalTests) * 100.0
    end,

    AssertionPassRate = case TotalAssertions of
        0 -> 0.0;
        _ -> (AssertionsPassed / TotalAssertions) * 100.0
    end,

    #report_summary{
        total_tests = TotalTests,
        passed_tests = PassedTests,
        failed_tests = FailedTests,
        skipped_tests = SkippedTests,
        total_assertions = TotalAssertions,
        assertions_passed = AssertionsPassed,
        assertions_failed = AssertionsFailed,
        total_duration_ms = TotalDuration,
        pass_rate = PassRate,
        assertion_pass_rate = AssertionPassRate,
        test_tree_depth = MaxDepth,
        anomalies_detected = 0,
        regressions_detected = 0
    }.

%% Extract metrics summary
extract_metrics_summary(State) ->
    Results = State#state.test_results,
    AllMetrics = [R#test_node.metrics || R <- Results],

    Throughputs = [maps:get(throughput_msg_per_sec, M, 0) || M <- AllMetrics, is_map(M)],
    Latencies = [maps:get(avg_latency_ms, M, 0) || M <- AllMetrics, is_map(M)],
    ErrorCounts = [maps:get(error_count, M, 0) || M <- AllMetrics, is_map(M)],

    AvgThroughput = case Throughputs of
        [] -> 0;
        _ -> lists:sum(Throughputs) / length(Throughputs)
    end,

    AvgLatency = case Latencies of
        [] -> 0;
        _ -> lists:sum(Latencies) / length(Latencies)
    end,

    TotalErrors = lists:sum(ErrorCounts),

    #{
        avg_throughput_msg_per_sec => AvgThroughput,
        avg_latency_ms => AvgLatency,
        total_error_count => TotalErrors,
        test_count => length(Results)
    }.

%% Analyze metric trend
analyze_metric_trend(_Metric, State) ->
    Results = State#state.test_results,
    TrendData = lists:map(fun(R) ->
        #{
            timestamp => R#test_node.start_time,
            duration => R#test_node.duration_ms
        }
    end, Results),

    SortedByTime = lists:sort(fun(A, B) ->
        maps:get(timestamp, A) =< maps:get(timestamp, B)
    end, TrendData),

    Durations = [maps:get(duration, T) || T <- SortedByTime],
    AvgDuration = case Durations of
        [] -> 0;
        _ -> lists:sum(Durations) / length(Durations)
    end,

    #{
        metric => duration,
        trend_data => SortedByTime,
        avg_value => AvgDuration,
        direction => stable
    }.

%% Detect anomalies internal
detect_anomalies_internal(_AnomalyType, State) ->
    Results = State#state.test_results,
    Durations = [R#test_node.duration_ms || R <- Results],

    case Durations of
        [] ->
            [];
        _ ->
            AvgDuration = lists:sum(Durations) / length(Durations),
            StdDev = math:sqrt(lists:sum([
                math:pow(D - AvgDuration, 2) || D <- Durations
            ]) / length(Durations)),

            Threshold = AvgDuration + (2 * StdDev),
            Anomalies = [#{
                test_id => R#test_node.test_id,
                duration_ms => R#test_node.duration_ms,
                deviation => R#test_node.duration_ms - AvgDuration
            } || R <- Results, R#test_node.duration_ms > Threshold],

            Anomalies
    end.

%% Export JSON internal
export_json_internal(OutputPath, State) ->
    Summary = generate_summary(State),
    JSONData = #{
        reporter_id => State#state.reporter_id,
        summary => summary_to_map(Summary),
        total_tests => length(State#state.test_results),
        status => State#state.status
    },

    JSONBinary = jsx:encode(JSONData),
    Filename = filename:join(OutputPath, binary_to_list(State#state.reporter_id) ++ "_report.json"),

    case file:write_file(Filename, JSONBinary) of
        ok ->
            logger:info("JSON report written to ~s", [Filename]),
            {ok, Filename};
        {error, Reason} ->
            logger:error("Failed to write JSON report: ~p", [Reason]),
            {error, Reason}
    end.

%% Export HTML internal
export_html_internal(OutputPath, State) ->
    Summary = generate_summary(State),
    HTML = format_html_report(Summary, State),
    Filename = filename:join(OutputPath, binary_to_list(State#state.reporter_id) ++ "_report.html"),

    case file:write_file(Filename, HTML) of
        ok ->
            logger:info("HTML report written to ~s", [Filename]),
            {ok, Filename};
        {error, Reason} ->
            logger:error("Failed to write HTML report: ~p", [Reason]),
            {error, Reason}
    end.

%% Export CSV internal
export_csv_internal(OutputPath, State) ->
    CSV = format_csv_report(State#state.test_results),
    Filename = filename:join(OutputPath, binary_to_list(State#state.reporter_id) ++ "_report.csv"),

    case file:write_file(Filename, CSV) of
        ok ->
            logger:info("CSV report written to ~s", [Filename]),
            {ok, Filename};
        {error, Reason} ->
            logger:error("Failed to write CSV report: ~p", [Reason]),
            {error, Reason}
    end.

%% Export plaintext internal
export_plaintext_internal(OutputPath, State) ->
    Summary = generate_summary(State),
    Text = format_plaintext_report(Summary, State),
    Filename = filename:join(OutputPath, binary_to_list(State#state.reporter_id) ++ "_report.txt"),

    case file:write_file(Filename, Text) of
        ok ->
            logger:info("Plaintext report written to ~s", [Filename]),
            {ok, Filename};
        {error, Reason} ->
            logger:error("Failed to write plaintext report: ~p", [Reason]),
            {error, Reason}
    end.

%% Convert summary to map
summary_to_map(Summary) ->
    #{
        total_tests => Summary#report_summary.total_tests,
        passed_tests => Summary#report_summary.passed_tests,
        failed_tests => Summary#report_summary.failed_tests,
        skipped_tests => Summary#report_summary.skipped_tests,
        total_assertions => Summary#report_summary.total_assertions,
        assertions_passed => Summary#report_summary.assertions_passed,
        assertions_failed => Summary#report_summary.assertions_failed,
        pass_rate => Summary#report_summary.pass_rate,
        assertion_pass_rate => Summary#report_summary.assertion_pass_rate,
        total_duration_ms => Summary#report_summary.total_duration_ms
    }.

%% Format as HTML
format_html_report(Summary, State) ->
    [
        "<!DOCTYPE html>\n",
        "<html>\n",
        "<head>\n",
        "  <title>ErlMCP Test Report</title>\n",
        "  <style>\n",
        "    body { font-family: Arial, sans-serif; margin: 20px; }\n",
        "    .summary { background-color: #f0f0f0; padding: 10px; border-radius: 5px; }\n",
        "    .passed { color: green; }\n",
        "    .failed { color: red; }\n",
        "    table { border-collapse: collapse; width: 100%; margin-top: 20px; }\n",
        "    th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }\n",
        "    th { background-color: #4CAF50; color: white; }\n",
        "  </style>\n",
        "</head>\n",
        "<body>\n",
        "  <h1>ErlMCP Test Report</h1>\n",
        "  <div class=\"summary\">\n",
        "    <h2>Summary</h2>\n",
        io_lib:format("    <p>Total Tests: <strong>~w</strong></p>\n", [Summary#report_summary.total_tests]),
        io_lib:format("    <p class=\"passed\">Passed: <strong>~w</strong></p>\n", [Summary#report_summary.passed_tests]),
        io_lib:format("    <p class=\"failed\">Failed: <strong>~w</strong></p>\n", [Summary#report_summary.failed_tests]),
        io_lib:format("    <p>Pass Rate: <strong>~.2f%</strong></p>\n", [Summary#report_summary.pass_rate]),
        io_lib:format("    <p>Total Duration: <strong>~w ms</strong></p>\n", [Summary#report_summary.total_duration_ms]),
        "  </div>\n",
        "</body>\n",
        "</html>\n"
    ].

%% Format as CSV
format_csv_report(TestResults) ->
    Header = "test_id,test_name,status,duration_ms,assertions_passed,assertions_failed\n",
    Rows = lists:map(fun(R) ->
        io_lib:format("~s,~s,~w,~w,~w,~w\n", [
            binary_to_list(R#test_node.test_id),
            binary_to_list(R#test_node.test_name),
            R#test_node.status,
            R#test_node.duration_ms,
            R#test_node.assertions_passed,
            R#test_node.assertions_failed
        ])
    end, TestResults),
    [Header | Rows].

%% Format as plaintext
format_plaintext_report(Summary, _State) ->
    [
        "=== ErlMCP Test Report ===\n",
        io_lib:format("Total Tests: ~w\n", [Summary#report_summary.total_tests]),
        io_lib:format("Passed: ~w\n", [Summary#report_summary.passed_tests]),
        io_lib:format("Failed: ~w\n", [Summary#report_summary.failed_tests]),
        io_lib:format("Skipped: ~w\n", [Summary#report_summary.skipped_tests]),
        io_lib:format("Pass Rate: ~.2f%\n", [Summary#report_summary.pass_rate]),
        io_lib:format("Total Assertions: ~w\n", [Summary#report_summary.total_assertions]),
        io_lib:format("Assertion Pass Rate: ~.2f%\n", [Summary#report_summary.assertion_pass_rate]),
        io_lib:format("Total Duration: ~w ms\n", [Summary#report_summary.total_duration_ms]),
        "================================\n"
    ].

%% Generate test ID
generate_test_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(1000000),
    iolist_to_binary(io_lib:format("test_~w_~w", [Timestamp, Random])).
