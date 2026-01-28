#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% Profiling System Validation Script - Tests profilers at 100K scale
%%%
%%% Usage: ./scripts/profiling_validation.escript
%%%-------------------------------------------------------------------

main([]) ->
    io:format("=== Profiling System Validation ===~n~n"),

    %% Start profilers
    io:format("Starting profilers...~n"),
    erlmcp_cpu_profiler:start_profiling(),
    erlmcp_latency_profiler:start_profiling(),
    erlmcp_memory_profiler:start_profiling(),
    erlmcp_bottleneck_detector:start_detection(),

    %% Run test workload
    io:format("Running 10K operation test...~n"),
    run_test_workload(10000),

    %% Get reports
    io:format("~nGenerating reports...~n"),
    CpuReport = erlmcp_cpu_profiler:get_profiling_report(),
    LatencyReport = erlmcp_latency_profiler:get_profiling_report(),
    BottleneckReport = erlmcp_bottleneck_detector:get_bottleneck_report(),

    %% Display results
    io:format("~n=== CPU Profiler Report ===~n"),
    print_map(CpuReport),

    io:format("~n=== Latency Profiler Report ===~n"),
    print_map(LatencyReport),

    io:format("~n=== Bottleneck Detector Report ===~n"),
    print_map(BottleneckReport),

    %% Check overhead
    CpuOverhead = erlmcp_cpu_profiler:get_cpu_overhead(),
    io:format("~n=== Profiler Overhead ===~n"),
    io:format("CPU Overhead: ~.2f%~n", [CpuOverhead]),

    %% Validation
    io:format("~n=== Validation Results ===~n"),
    case CpuOverhead < 15.0 of
        true -> io:format("[PASS] CPU overhead < 15%~n");
        false -> io:format("[FAIL] CPU overhead >= 15%~n")
    end,

    case maps:get(total_alerts, BottleneckReport, 0) >= 0 of
        true -> io:format("[PASS] Bottleneck detector working~n");
        false -> io:format("[FAIL] Bottleneck detector failed~n")
    end,

    case maps:get(total_samples, LatencyReport, 0) >= 10000 of
        true -> io:format("[PASS] Latency samples collected~n");
        false -> io:format("[FAIL] Latency samples not collected~n")
    end,

    io:format("~n=== Validation Complete ===~n"),
    erlang:halt(0);

main(_) ->
    usage().

usage() ->
    io:format("Usage: profiling_validation.escript~n"),
    erlang:halt(1).

run_test_workload(NumOps) ->
    lists:foreach(
        fun(I) ->
            %% CPU profiling
            erlmcp_cpu_profiler:measure_function_call(
                {test, workload, I rem 10},
                100 + (I rem 500)
            ),

            %% Latency profiling
            Latency = case I rem 100 of
                0 -> 100000;   %% 100ms - slow
                _ -> 5000      %% 5ms - normal
            end,
            erlmcp_latency_profiler:measure_operation(test_op, Latency),

            %% Memory profiling
            case I rem 1000 of
                0 -> erlmcp_memory_profiler:measure_memory_snapshot();
                _ -> ok
            end
        end,
        lists:seq(1, NumOps)
    ).

print_map(Map) when is_map(Map) ->
    maps:foreach(
        fun(Key, Value) ->
            case is_map(Value) of
                true ->
                    io:format("~p:~n", [Key]),
                    print_map_indented(Value, "  ");
                false ->
                    case is_list(Value) of
                        true ->
                            io:format("~p: [~B items]~n", [Key, length(Value)]);
                        false ->
                            io:format("~p: ~p~n", [Key, Value])
                    end
            end
        end,
        Map
    );
print_map(_) ->
    ok.

print_map_indented(Map, Indent) when is_map(Map) ->
    maps:foreach(
        fun(Key, Value) ->
            case is_map(Value) of
                true ->
                    io:format("~s~p:~n", [Indent, Key]),
                    print_map_indented(Value, Indent ++ "  ");
                false ->
                    case is_list(Value) of
                        true ->
                            io:format("~s~p: [~B items]~n", [Indent, Key, length(Value)]);
                        false ->
                            io:format("~s~p: ~p~n", [Indent, Key, Value])
                    end
            end
        end,
        Map
    );
print_map_indented(_, _) ->
    ok.
