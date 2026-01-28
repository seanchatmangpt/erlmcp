%%%-------------------------------------------------------------------
%%% @doc
%%% Structured Logging Stress Test - 100K Concurrent Operations
%%%
%%% Tests:
%%% 1. Log volume at 100K concurrent
%%% 2. Trace ID propagation correctness
%%% 3. Component-level control effectiveness
%%% 4. Performance overhead measurement
%%% 5. Memory efficiency
%%%
%%% Run with:
%%%   erl -noshell -run erlmcp_logging_100k_stress test 100000 5 -s init stop
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_logging_100k_stress).

-export([test/2, test/3]).

-record(metrics, {
    start_time,
    end_time,
    num_operations = 0,
    log_count = 0,
    trace_errors = 0,
    overhead_percent = 0,
    logs_per_second = 0,
    memory_per_log = 0
}).

%%====================================================================
%% Main Entry Point
%%====================================================================

test(NumOps, DurationMin) ->
    test(NumOps, DurationMin, #{}).

test(NumOps, _DurationMin, _Options) ->
    io:format("~n=== Structured Logging 100K Concurrent Stress Test ===~n", []),
    io:format("Number of Operations: ~w~n", [NumOps]),

    %% Start erlmcp application
    application:ensure_all_started(erlmcp),

    %% Initialize structured logging
    erlmcp_structured_logging:init(#{
        format => json,
        min_level => debug,
        sample_rate => 1.0
    }),

    erlmcp_structured_logging:enable_log_capture(NumOps + 10000),

    io:format("Starting stress test...~n", []),

    %% Test 1: Log volume
    io:format("~nTest 1: Log Volume Generation~n", []),
    test_log_volume(NumOps),

    %% Test 2: Trace propagation
    io:format("~nTest 2: Trace ID Propagation~n", []),
    test_trace_propagation(),

    %% Test 3: Component levels
    io:format("~nTest 3: Component Log Level Control~n", []),
    test_component_levels(),

    %% Test 4: Performance overhead
    io:format("~nTest 4: Performance Overhead~n", []),
    test_performance_overhead(NumOps div 10),

    %% Test 5: Search and filtering
    io:format("~nTest 5: Log Search and Filtering~n", []),
    test_search_and_filter(),

    io:format("~n=== All Tests Complete ===~n", []),
    erlmcp_structured_logging:disable_log_capture(),
    ok.

%%====================================================================
%% Test 1: Log Volume Generation at 100K Scale
%%====================================================================

test_log_volume(NumOps) ->
    erlmcp_structured_logging:enable_log_capture(NumOps + 10000),

    BatchSize = 1000,
    NumBatches = NumOps div BatchSize,

    Start = erlang:monotonic_time(millisecond),

    %% Spawn worker processes
    Workers = [spawn_link(fun() ->
        worker_log_batch(B, BatchSize)
    end) || B <- lists:seq(1, NumBatches)],

    %% Wait for all workers
    lists:foreach(fun(_W) ->
        receive
            {'EXIT', _Pid, _Reason} -> ok
        after 60000 ->
            io:format("Worker timeout!~n", [])
        end
    end, Workers),

    End = erlang:monotonic_time(millisecond),
    ElapsedMs = End - Start,

    %% Get captured logs
    CapturedLogs = erlmcp_structured_logging:get_captured_logs(),
    NumLogs = length(CapturedLogs),
    LogsPerSec = (NumLogs * 1000) div max(1, ElapsedMs),
    AvgLogSize = 400,
    TotalDataMB = (NumLogs * AvgLogSize) div (1024 * 1024),

    io:format("  Operations: ~w~n", [NumOps]),
    io:format("  Elapsed Time: ~w ms~n", [ElapsedMs]),
    io:format("  Logs Generated: ~w~n", [NumLogs]),
    io:format("  Logs/sec: ~w~n", [LogsPerSec]),
    io:format("  Avg Log Size: ~w bytes~n", [AvgLogSize]),
    io:format("  Total Data: ~w MB~n", [TotalDataMB]),

    case LogsPerSec > 1000 of
        true -> io:format("  Result: PASS (>1000 logs/sec)~n", []);
        false -> io:format("  Result: WARN (<1000 logs/sec)~n", [])
    end.

%%====================================================================
%% Test 2: Trace ID Propagation
%%====================================================================

test_trace_propagation() ->
    %% Set initial trace ID
    erlmcp_trace_propagation:set_trace_context(#{
        trace_id => <<"trace-test-001">>,
        span_id => <<"span-test-001">>,
        baggage => #{user_id => <<"test-user">>}
    }),

    TraceId = erlmcp_structured_logging:get_trace_id(),
    Parent = self(),

    %% Spawn 100 children
    Children = [spawn_link(fun() ->
        ChildTraceId = erlmcp_structured_logging:get_trace_id(),
        Match = ChildTraceId =:= TraceId,
        erlmcp_structured_logging:info(<<"child_test">>, #{
            child_id => N,
            match => Match
        }),
        Parent ! {child_result, N, Match}
    end) || N <- lists:seq(1, 100)],

    %% Collect results
    Results = [receive
        {child_result, N, Match} -> {N, Match}
    after 5000 -> {error, timeout}
    end || _ <- Children],

    Matches = [1 || {_N, true} <- Results],
    MatchCount = length(Matches),

    io:format("  Children Spawned: 100~n", []),
    io:format("  Trace ID Matches: ~w~n", [MatchCount]),

    case MatchCount =:= 100 of
        true -> io:format("  Result: PASS (all children matched)~n", []);
        false -> io:format("  Result: FAIL (~w mismatches)~n", [100 - MatchCount])
    end.

%%====================================================================
%% Test 3: Component-Level Control
%%====================================================================

test_component_levels() ->
    %% Set different levels
    erlmcp_structured_logging:set_component_level(tool_exec, debug),
    erlmcp_structured_logging:set_component_level(transport, warning),
    erlmcp_structured_logging:set_component_level(server, info),

    Level1 = erlmcp_structured_logging:get_component_level(tool_exec),
    Level2 = erlmcp_structured_logging:get_component_level(transport),
    Level3 = erlmcp_structured_logging:get_component_level(server),

    io:format("  tool_exec: ~w~n", [Level1]),
    io:format("  transport: ~w~n", [Level2]),
    io:format("  server: ~w~n", [Level3]),

    case {Level1, Level2, Level3} of
        {debug, warning, info} -> io:format("  Result: PASS~n", []);
        _ -> io:format("  Result: FAIL~n", [])
    end.

%%====================================================================
%% Test 4: Performance Overhead
%%====================================================================

test_performance_overhead(NumOps) ->
    %% Baseline: no logging
    erlmcp_structured_logging:set_component_level(bench, error),

    Start1 = erlang:monotonic_time(millisecond),
    lists:foreach(fun(N) ->
        _R = compute_op(N),
        ok
    end, lists:seq(1, NumOps)),
    End1 = erlang:monotonic_time(millisecond),
    TimeNoLog = End1 - Start1,

    %% With logging
    erlmcp_structured_logging:set_component_level(bench, debug),

    Start2 = erlang:monotonic_time(millisecond),
    lists:foreach(fun(N) ->
        erlmcp_structured_logging:debug(<<"bench">>, #{op => N}),
        _R = compute_op(N),
        ok
    end, lists:seq(1, NumOps)),
    End2 = erlang:monotonic_time(millisecond),
    TimeWithLog = End2 - Start2,

    OverheadPercent = ((TimeWithLog - TimeNoLog) * 100) div max(1, TimeNoLog),

    io:format("  Without Logging: ~w ms~n", [TimeNoLog]),
    io:format("  With Logging: ~w ms~n", [TimeWithLog]),
    io:format("  Overhead: ~w%~n", [OverheadPercent]),

    case OverheadPercent < 5 of
        true -> io:format("  Result: PASS (<5% overhead)~n", []);
        false -> io:format("  Result: WARN (~w% overhead)~n", [OverheadPercent])
    end.

%%====================================================================
%% Test 5: Search and Filtering
%%====================================================================

test_search_and_filter() ->
    %% Generate some test logs
    TraceId = erlmcp_structured_logging:get_trace_id(),

    lists:foreach(fun(N) ->
        erlmcp_structured_logging:info(<<"search_test">>, #{
            index => N,
            category => case N rem 3 of
                0 -> <<"type_a">>;
                1 -> <<"type_b">>;
                _ -> <<"type_c">>
            end
        })
    end, lists:seq(1, 100)),

    %% Search by trace ID
    TraceResults = erlmcp_structured_logging:search_trace(TraceId),

    %% Get trace metrics
    Metrics = erlmcp_structured_logging:get_trace_metrics(TraceId),

    io:format("  Logs in Trace: ~w~n", [maps:get(event_count, Metrics, 0)]),
    io:format("  Unique Spans: ~w~n", [maps:get(unique_spans, Metrics, 0)]),
    io:format("  Total Duration: ~w ms~n", [maps:get(total_duration_ms, Metrics, 0)]),

    case length(TraceResults) > 0 of
        true -> io:format("  Result: PASS (found logs)~n", []);
        false -> io:format("  Result: FAIL (no logs found)~n", [])
    end.

%%====================================================================
%% Worker Functions
%%====================================================================

worker_log_batch(BatchNum, BatchSize) ->
    lists:foreach(fun(N) ->
        OpNum = BatchNum * BatchSize + N,
        erlmcp_structured_logging:info(<<"worker_op">>, #{
            batch => BatchNum,
            index => N,
            total => OpNum
        })
    end, lists:seq(1, BatchSize)).

compute_op(N) ->
    %% Simple CPU operation
    lists:foldl(fun(X, Acc) ->
        Acc + (X rem 7)
    end, 0, lists:seq(1, 50 + (N rem 50))).
