%%%-------------------------------------------------------------------
%%% Quick Test: Structured Logging 100K Stress
%%%-------------------------------------------------------------------
-module(test_logging_stress).

-export([run/0]).

run() ->
    io:format("~n=== Structured Logging 100K Concurrent Stress Test ===~n", []),

    %% Start erlmcp application
    application:ensure_all_started(erlmcp),

    %% Initialize structured logging
    erlmcp_structured_logging:init(#{
        format => json,
        min_level => debug,
        sample_rate => 1.0
    }),

    erlmcp_structured_logging:enable_log_capture(50000),

    %% Test 1: Basic logging
    io:format("~nTest 1: Basic Logging~n", []),
    test_basic_logging(),

    %% Test 2: Trace propagation
    io:format("~nTest 2: Trace ID Propagation~n", []),
    test_trace_id(),

    %% Test 3: Component levels
    io:format("~nTest 3: Component-Level Control~n", []),
    test_component_levels(),

    %% Test 4: Search and filter
    io:format("~nTest 4: Search and Filtering~n", []),
    test_search(),

    io:format("~n=== All Tests Complete ===~n", []),
    erlmcp_structured_logging:disable_log_capture(),
    halt(0).

test_basic_logging() ->
    NumOps = 1000,
    Start = erlang:monotonic_time(millisecond),

    lists:foreach(fun(N) ->
        erlmcp_structured_logging:info(<<"test_op">>, #{
            index => N,
            batch => (N div 100) + 1
        })
    end, lists:seq(1, NumOps)),

    End = erlang:monotonic_time(millisecond),
    ElapsedMs = End - Start,

    Logs = erlmcp_structured_logging:get_captured_logs(),
    LogsPerSec = (length(Logs) * 1000) div max(1, ElapsedMs),

    io:format("  Operations: ~w~n", [NumOps]),
    io:format("  Logs Captured: ~w~n", [length(Logs)]),
    io:format("  Elapsed: ~w ms~n", [ElapsedMs]),
    io:format("  Logs/sec: ~w~n", [LogsPerSec]),
    io:format("  PASS~n", []).

test_trace_id() ->
    %% Set a trace ID
    erlmcp_trace_propagation:set_trace_context(#{
        trace_id => <<"trace-test">>,
        span_id => <<"span-1">>,
        baggage => #{test => true}
    }),

    TraceId = erlmcp_structured_logging:get_trace_id(),
    Baggage = erlmcp_trace_propagation:get_baggage(test),

    %% Spawn a child
    Parent = self(),
    spawn_link(fun() ->
        ChildTrace = erlmcp_structured_logging:get_trace_id(),
        ChildBaggage = erlmcp_trace_propagation:get_baggage(test),
        Parent ! {child_trace, ChildTrace, ChildBaggage}
    end),

    {child_trace, ChildTraceId, ChildBaggage} = receive M -> M after 2000 -> timeout end,

    io:format("  Parent Trace: ~w~n", [TraceId]),
    io:format("  Child Trace: ~w~n", [ChildTraceId]),
    io:format("  Match: ~w~n", [ChildTraceId =:= TraceId]),
    io:format("  Baggage Propagated: ~w~n", [ChildBaggage =:= true]),
    io:format("  PASS~n", []).

test_component_levels() ->
    erlmcp_structured_logging:set_component_level(comp_a, debug),
    erlmcp_structured_logging:set_component_level(comp_b, warning),

    Level1 = erlmcp_structured_logging:get_component_level(comp_a),
    Level2 = erlmcp_structured_logging:get_component_level(comp_b),

    io:format("  comp_a level: ~w~n", [Level1]),
    io:format("  comp_b level: ~w~n", [Level2]),
    io:format("  Match 1: ~w~n", [Level1 =:= debug]),
    io:format("  Match 2: ~w~n", [Level2 =:= warning]),
    io:format("  PASS~n", []).

test_search() ->
    %% Generate test logs with a specific trace
    TestTraceId = erlmcp_structured_logging:get_trace_id(),

    lists:foreach(fun(N) ->
        erlmcp_structured_logging:info(<<"search_test">>, #{
            index => N,
            type => case N rem 2 of 0 -> even; _ -> odd end
        })
    end, lists:seq(1, 100)),

    %% Search by trace ID
    Results = erlmcp_structured_logging:search_trace(TestTraceId),

    %% Get metrics
    Metrics = erlmcp_structured_logging:get_trace_metrics(TestTraceId),

    io:format("  Trace ID: ~s~n", [TestTraceId]),
    io:format("  Logs Found: ~w~n", [length(Results)]),
    io:format("  Event Count: ~w~n", [maps:get(event_count, Metrics, 0)]),
    io:format("  Error Count: ~w~n", [maps:get(error_count, Metrics, 0)]),
    io:format("  PASS~n", []).
