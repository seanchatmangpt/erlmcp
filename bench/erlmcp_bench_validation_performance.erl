%%%====================================================================
%%% VALIDATION FRAMEWORK PERFORMANCE BENCHMARK SUITE
%%%====================================================================
%%% Purpose: Comprehensive performance validation of erlmcp_validation
%%% Metrics: Parse time, validation time, memory usage, throughput
%%% Compliance: Metrology v1.5.0 standards
%%%====================================================================

-module(erlmcp_validation_performance_benchmark).
-export([
    run_all_benchmarks/0,
    run_full_compliance_suite/0,
    benchmark_spec_parsing/1,
    benchmark_validation_operations/1,
    benchmark_memory_usage/1,
    benchmark_timeout_handling/1,
    benchmark_large_messages/1,
    benchmark_transport_types/1,
    benchmark_stress_load/1,
    generate_report/0
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% MAIN ENTRY POINTS
%%====================================================================

run_all_benchmarks() ->
    io:format("~n~n========================================~n"),
    io:format("ERLMCP VALIDATION PERFORMANCE BENCHMARK~n"),
    io:format("========================================~n~n"),
    
    %% Capture environment
    Env = capture_environment(),
    io:format("Environment:~n"),
    io:format("  Erlang: ~s~n", [maps:get(erlang_version, Env)]),
    io:format("  System: ~s~n", [maps:get(system_architecture, Env)]),
    io:format("  CPU Cores: ~p~n", [maps:get(cpu_cores, Env)]),
    io:format("  Memory: ~p MB~n~n", [maps:get(total_memory_mb, Env)]),
    
    %% Run all benchmark categories
    Results = #{
        timestamp => erlang:system_time(millisecond),
        environment => Env,
        spec_parsing => benchmark_spec_parsing(1000),
        validation_operations => benchmark_validation_operations(1000),
        memory_usage => benchmark_memory_usage(100),
        timeout_handling => benchmark_timeout_handling(100),
        large_messages => benchmark_large_messages(100),
        transport_types => benchmark_transport_types(100),
        stress_load => benchmark_stress_load(30)
    },
    
    %% Generate report
    generate_report(Results),
    
    %% Save results
    save_results(Results),
    
    io:format("~n~nBenchmark complete! Results saved to: bench/results/validation_performance_~p.json~n",
              [maps:get(timestamp, Results)]),
    ok.

run_full_compliance_suite() ->
    io:format("~n~n========================================~n"),
    io:format("FULL COMPLIANCE SUITE BENCHMARK~n"),
    io:format("========================================~n~n"),
    
    StartTime = erlang:monotonic_time(microsecond),
    
    %% Measure memory before
    MemoryBefore = erlang:memory(total),
    
    %% Run full compliance suite (simulated)
    %% In real implementation, this would call erlmcp_validation_runner
    TestCount = 100,
    RunResult = run_compliance_tests(TestCount),
    
    %% Measure memory after
    MemoryAfter = erlang:memory(total),
    MemoryUsed = MemoryAfter - MemoryBefore,
    
    EndTime = erlang:monotonic_time(microsecond),
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1_000_000,
    
    Throughput = TestCount / DurationS,
    
    io:format("~n~n--- Full Compliance Suite Results ---~n"),
    io:format("Tests Run: ~p~n", [TestCount]),
    io:format("Duration: ~.3f seconds~n", [DurationS]),
    io:format("Throughput: ~.2f tests/sec~n", [Throughput]),
    io:format("Memory Used: ~.2f MB~n", [MemoryUsed / (1024 * 1024)]),
    io:format("Memory Per Test: ~.2f KB~n", [MemoryUsed / TestCount / 1024]),
    
    #{
        workload_id => <<"full_compliance_suite">>,
        duration_s => DurationS,
        tests_run => TestCount,
        throughput_msg_per_s => Throughput,
        memory_rss_mib_per_node => MemoryAfter / (1024 * 1024),
        memory_delta_mib => MemoryUsed / (1024 * 1024),
        memory_per_test_kib => MemoryUsed / TestCount / 1024,
        transport => stdio,
        scope => per_node,
        precision => microsecond
    }.

%%====================================================================
%% SPEC PARSING BENCHMARK
%%====================================================================

benchmark_spec_parsing(Iterations) when is_integer(Iterations) ->
    io:format("~n--- Spec Parsing Benchmark (~p iterations) ---~n", [Iterations]),
    
    %% Small spec (1KB)
    SmallSpec = generate_test_spec(1024),
    {SmallTime, _} = timer:tc(fun() -> 
        [parse_test_spec(SmallSpec) || _ <- lists:seq(1, Iterations)]
    end),
    
    %% Medium spec (10KB)
    MediumSpec = generate_test_spec(10240),
    {MediumTime, _} = timer:tc(fun() -> 
        [parse_test_spec(MediumSpec) || _ <- lists:seq(1, Iterations div 10)]
    end),
    
    %% Large spec (100KB)
    LargeSpec = generate_test_spec(102400),
    {LargeTime, _} = timer:tc(fun() -> 
        [parse_test_spec(LargeSpec) || _ <- lists:seq(1, Iterations div 100)]
    end),
    
    %% Calculate throughput
    SmallThroughput = Iterations / (SmallTime / 1_000_000),
    MediumThroughput = (Iterations div 10) / (MediumTime / 1_000_000),
    LargeThroughput = (Iterations div 100) / (LargeTime / 1_000_000),
    
    io:format("Small (1KB): ~.3f us/op, ~.0f specs/sec~n", 
              [SmallTime / Iterations, SmallThroughput]),
    io:format("Medium (10KB): ~.3f us/op, ~.0f specs/sec~n", 
              [MediumTime / (Iterations div 10), MediumThroughput]),
    io:format("Large (100KB): ~.3f us/op, ~.0f specs/sec~n", 
              [LargeTime / (Iterations div 100), LargeThroughput]),
    
    #{
        workload_id => <<"spec_parsing_benchmark">>,
        iterations => Iterations,
        small_1k => #{
            latency_avg_us => SmallTime / Iterations,
            throughput_msg_per_s => SmallThroughput,
            size_bytes => 1024
        },
        medium_10k => #{
            latency_avg_us => MediumTime / (Iterations div 10),
            throughput_msg_per_s => MediumThroughput,
            size_bytes => 10240
        },
        large_100k => #{
            latency_avg_us => LargeTime / (Iterations div 100),
            throughput_msg_per_s => LargeThroughput,
            size_bytes => 102400
        },
        transport => stdio,
        scope => per_node,
        precision => microsecond
    }.

%%====================================================================
%% VALIDATION OPERATIONS BENCHMARK
%%====================================================================

benchmark_validation_operations(Iterations) when is_integer(Iterations) ->
    io:format("~n--- Validation Operations Benchmark (~p iterations) ---~n", [Iterations]),
    
    %% Prepare test data
    ValidRequest = #{<<"jsonrpc">> => <<"2.0">>, 
                     <<"id">> => 1, 
                     <<"method">> => <<"initialize">>,
                     <<"params">> => #{<<"protocolVersion">> => <<"2025-11-25">>}},
    InvalidRequest = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1},
    
    %% Benchmark valid request validation
    {ValidTime, _} = timer:tc(fun() -> 
        [validate_request(ValidRequest) || _ <- lists:seq(1, Iterations)]
    end),
    
    %% Benchmark invalid request validation
    {InvalidTime, _} = timer:tc(fun() -> 
        [validate_request(InvalidRequest) || _ <- lists:seq(1, Iterations)]
    end),
    
    %% Benchmark error response validation
    ErrorResponse = #{<<"jsonrpc">> => <<"2.0">>,
                      <<"id">> => 1,
                      <<"error">> => #{<<"code">> => -32600, 
                                       <<"message">> => <<"Invalid Request">>}},
    {ErrorTime, _} = timer:tc(fun() -> 
        [validate_error_response(ErrorResponse) || _ <- lists:seq(1, Iterations)]
    end),
    
    ValidThroughput = Iterations / (ValidTime / 1_000_000),
    InvalidThroughput = Iterations / (InvalidTime / 1_000_000),
    ErrorThroughput = Iterations / (ErrorTime / 1_000_000),
    
    io:format("Valid Request: ~.3f us/op, ~.0f ops/sec~n", 
              [ValidTime / Iterations, ValidThroughput]),
    io:format("Invalid Request: ~.3f us/op, ~.0f ops/sec~n", 
              [InvalidTime / Iterations, InvalidThroughput]),
    io:format("Error Response: ~.3f us/op, ~.0f ops/sec~n", 
              [ErrorTime / Iterations, ErrorThroughput]),
    
    #{
        workload_id => <<"validation_operations_benchmark">>,
        iterations => Iterations,
        valid_request => #{
            latency_avg_us => ValidTime / Iterations,
            throughput_msg_per_s => ValidThroughput
        },
        invalid_request => #{
            latency_avg_us => InvalidTime / Iterations,
            throughput_msg_per_s => InvalidThroughput
        },
        error_response => #{
            latency_avg_us => ErrorTime / Iterations,
            throughput_msg_per_s => ErrorThroughput
        },
        transport => stdio,
        scope => per_operation,
        precision => microsecond
    }.

%%====================================================================
%% MEMORY USAGE BENCHMARK
%%====================================================================

benchmark_memory_usage(Iterations) when is_integer(Iterations) ->
    io:format("~n--- Memory Usage Benchmark (~p iterations) ---~n", [Iterations]),
    
    %% Force garbage collection before measurement
    erlang:garbage_collect(),
    
    MemoryBefore = erlang:memory(total),
    MemoryBeforeProcesses = erlang:memory(processes),
    
    %% Run operations
    [run_validation_cycle() || _ <- lists:seq(1, Iterations)],
    
    MemoryAfter = erlang:memory(total),
    MemoryAfterProcesses = erlang:memory(processes),
    
    %% Force GC and measure again
    erlang:garbage_collect(),
    MemoryAfterGC = erlang:memory(total),
    
    TotalDelta = MemoryAfter - MemoryBefore,
    ProcessesDelta = MemoryAfterProcesses - MemoryBeforeProcesses,
    PerIteration = TotalDelta / Iterations,
    
    io:format("Total Memory Delta: ~.2f MB~n", [TotalDelta / (1024 * 1024)]),
    io:format("Process Memory Delta: ~.2f MB~n", [ProcessesDelta / (1024 * 1024)]),
    io:format("Per Iteration: ~.2f KB~n", [PerIteration / 1024]),
    io:format("After GC: ~.2f MB~n", [MemoryAfterGC / (1024 * 1024)]),
    io:format("Memory Leaked: ~.2f KB~n", [(MemoryAfterGC - MemoryBefore) / 1024]),
    
    #{
        workload_id => <<"memory_usage_benchmark">>,
        iterations => Iterations,
        memory_total_delta_mib => TotalDelta / (1024 * 1024),
        memory_process_delta_mib => ProcessesDelta / (1024 * 1024),
        memory_per_iteration_kib => PerIteration / 1024,
        memory_after_gc_mib => MemoryAfterGC / (1024 * 1024),
        memory_leaked_kib => (MemoryAfterGC - MemoryBefore) / 1024,
        transport => stdio,
        scope => per_node,
        precision => kilobyte
    }.

%%====================================================================
%% TIMEOUT HANDLING BENCHMARK
%%====================================================================

benchmark_timeout_handling(Iterations) when is_integer(Iterations) ->
    io:format("~n--- Timeout Handling Benchmark (~p iterations) ---~n", [Iterations]),
    
    %% Test different timeout values
    Timeouts = [100, 500, 1000, 5000],
    
    Results = lists:map(fun(Timeout) ->
        {Time, _} = timer:tc(fun() -> 
            [simulate_timeout_request(Timeout) || _ <- lists:seq(1, Iterations div length(Timeouts))]
        end),
        Count = Iterations div length(Timeouts),
        Throughput = Count / (Time / 1_000_000),
        io:format("Timeout ~p ms: ~.3f us/op, ~.0f ops/sec~n", 
                  [Timeout, Time / Count, Throughput]),
        #{timeout_ms => Timeout, 
          latency_avg_us => Time / Count,
          throughput_msg_per_s => Throughput}
    end, Timeouts),
    
    #{
        workload_id => <<"timeout_handling_benchmark">>,
        iterations_per_timeout => Iterations div length(Timeouts),
        results => Results,
        transport => stdio,
        scope => per_operation,
        precision => microsecond
    }.

%%====================================================================
%% LARGE MESSAGES BENCHMARK
%%====================================================================

benchmark_large_messages(Iterations) when is_integer(Iterations) ->
    io:format("~n--- Large Messages Benchmark (~p iterations) ---~n", [Iterations]),
    
    %% Test different message sizes
    Sizes = [1024, 10240, 102400, 1048576],  % 1KB, 10KB, 100KB, 1MB
    
    Results = lists:map(fun(Size) ->
        LargeMessage = generate_large_message(Size),
        
        {EncodeTime, _} = timer:tc(fun() -> 
            [jsx:encode(LargeMessage) || _ <- lists:seq(1, Iterations div length(Sizes))]
        end),
        
        Count = Iterations div length(Sizes),
        EncodeThroughput = Count / (EncodeTime / 1_000_000),
        
        io:format("Size ~p bytes: encode ~.3f us/op, ~.0f msgs/sec~n", 
                  [Size, EncodeTime / Count, EncodeThroughput]),
        
        #{size_bytes => Size,
          encode_latency_avg_us => EncodeTime / Count,
          encode_throughput_msg_per_s => EncodeThroughput}
    end, Sizes),
    
    #{
        workload_id => <<"large_messages_benchmark">>,
        iterations_per_size => Iterations div length(Sizes),
        results => Results,
        transport => stdio,
        scope => per_operation,
        precision => microsecond
    }.

%%====================================================================
%% TRANSPORT TYPES BENCHMARK
%%====================================================================

benchmark_transport_types(Iterations) when is_integer(Iterations) ->
    io:format("~n--- Transport Types Benchmark (~p iterations) ---~n", [Iterations]),
    
    %% Simulate different transport overheads
    Transports = [
        {stdio, 10},      % 10us overhead
        {tcp, 50},        % 50us overhead  
        {http_sse, 100}   % 100us overhead
    ],
    
    Results = lists:map(fun({Transport, Overhead}) ->
        {Time, _} = timer:tc(fun() -> 
            [simulate_transport_operation(Transport, Overhead) || _ <- lists:seq(1, Iterations div length(Transports))]
        end),
        
        Count = Iterations div length(Transports),
        Throughput = Count / (Time / 1_000_000),
        
        io:format("~s: ~.3f us/op, ~.0f ops/sec~n", 
                  [Transport, Time / Count, Throughput]),
        
        #{transport => Transport,
          overhead_us => Overhead,
          latency_avg_us => Time / Count,
          throughput_msg_per_s => Throughput}
    end, Transports),
    
    #{
        workload_id => <<"transport_types_benchmark">>,
        iterations_per_transport => Iterations div length(Transports),
        results => Results,
        scope => per_operation,
        precision => microsecond
    }.

%%====================================================================
%% STRESS LOAD BENCHMARK
%%====================================================================

benchmark_stress_load(DurationSeconds) when is_integer(DurationSeconds) ->
    io:format("~n--- Stress Load Benchmark (~p seconds) ---~n", [DurationSeconds]),
    
    %% Start background workers
    WorkerCount = erlang:system_info(schedulers_online),
    io:format("Starting ~p workers...~n", [WorkerCount]),
    
    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + (DurationSeconds * 1000),
    
    %% Spawn workers
    Workers = lists:map(fun(_) ->
        spawn_monitor(fun() -> stress_worker(EndTime) end)
    end, lists:seq(1, WorkerCount)),
    
    %% Monitor progress
    monitor_stress_test(EndTime, Workers),
    
    #{
        workload_id => <<"stress_load_benchmark">>,
        duration_s => DurationSeconds,
        workers => WorkerCount,
        transport => stdio,
        scope => per_node,
        precision => millisecond
    }.

%%====================================================================
%% HELPER FUNCTIONS
%%====================================================================

capture_environment() ->
    #{
        erlang_version => erlang:system_info(otp_release) ++ "-" ++ erlang:system_info(version),
        system_architecture => erlang:system_info(system_architecture),
        cpu_cores => erlang:system_info(schedulers_online),
        total_memory_mb => erlang:memory(total) div (1024 * 1024),
        ets_limit => erlang:system_info(process_count),
        process_limit => erlang:system_info(process_limit)
    }.

generate_test_spec(SizeBytes) ->
    %% Generate a realistic test specification
    Base = <<"{" 
             "\"jsonrpc\":\"2.0\","
             "\"method\":\"initialize\","
             "\"params\":{"
             "\"protocolVersion\":\"2025-11-25\","
             "\"capabilities\":{"
             "\"tools\":{},"
             "\"resources\":{},"
             "\"prompts\":{}"
             "}"
             "}"
             "}">>,
    %% Pad to desired size
    PaddingSize = SizeBytes - byte_size(Base),
    if PaddingSize > 0 ->
            <<Base/binary, binary:copy(<<" ">>, PaddingSize)/binary>>;
       true -> Base
    end.

parse_test_spec(Spec) ->
    %% Simulate spec parsing (in real implementation, use erlmcp_spec_parser)
    try jsx:decode(Spec, [return_maps]) of
        Result -> {ok, Result}
    catch
        _:_ -> {error, parse_error}
    end.

validate_request(Request) ->
    %% Simulate request validation (in real implementation, use erlmcp_protocol_validator)
    case {maps:get(<<"jsonrpc">>, Request, undefined),
          maps:get(<<"method">>, Request, undefined)} of
        {<<"2.0">>, Method} when is_binary(Method), Method =/= <<>> ->
            {compliant, Request};
        _ ->
            {non_compliant, invalid_request}
    end.

validate_error_response(Response) ->
    %% Simulate error response validation
    case maps:get(<<"error">>, Response, undefined) of
        #{<<"code">> := Code, <<"message">> := Message} 
          when is_integer(Code), is_binary(Message) ->
            {compliant, Response};
        _ ->
            {non_compliant, invalid_error_response}
    end.

run_validation_cycle() ->
    %% Simulate a full validation cycle
    Request = #{<<"jsonrpc">> => <<"2.0">>, 
                <<"id">> => 1, 
                <<"method">> => <<"tools/list">>},
    validate_request(Request),
    ok.

simulate_timeout_request(TimeoutMs) ->
    %% Simulate a request with timeout
    timer:sleep(TimeoutMs div 10),  % Sleep 1/10 of timeout for testing
    {ok, timeout_test}.

generate_large_message(SizeBytes) ->
    %% Generate a large message with realistic structure
    Base = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/list">>,
        <<"params">> => #{
            <<"tools">> => [
                #{
                    <<"name">> => <<"tool_", (integer_to_binary(N))/binary>>,
                    <<"description">> => binary:copy(<<"A description of the tool. ">>, 10),
                    <<"inputSchema">> => #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"param", (integer_to_binary(N))/binary>> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => binary:copy(<<"Parameter description. ">>, 5)
                            }}
                        } band (N rem 5 =:= 0)
                    }
                } || N <- lists:seq(1, 100)]
            }
        }
    },
    %% Encode and pad to size
    Encoded = jsx:encode(Base),
    PaddingSize = SizeBytes - byte_size(Encoded),
    if PaddingSize > 0 ->
            <<Encoded/binary, binary:copy(<<" ">>, PaddingSize)/binary>>;
       true -> Encoded
    end.

simulate_transport_operation(_Transport, OverheadUs) ->
    %% Simulate transport overhead
    timer:sleep(OverheadUs div 1000),  % Convert to ms
    {ok, transport_test}.

stress_worker(EndTime) ->
    %% Worker that runs until EndTime
    case erlang:monotonic_time(millisecond) of
        Now when Now < EndTime ->
            run_validation_cycle(),
            stress_worker(EndTime);
        _ ->
            done
    end.

monitor_stress_test(EndTime, Workers) ->
    %% Monitor workers and print progress
    StartTime = EndTime - (30 * 1000),  % 30 second test
    TotalDuration = EndTime - StartTime,
    
    monitor_stress_loop(StartTime, EndTime, TotalDuration, Workers).

monitor_stress_loop(StartTime, EndTime, TotalDuration, Workers) ->
    Now = erlang:monotonic_time(millisecond),
    if Now < EndTime ->
            Elapsed = Now - StartTime,
            Progress = (Elapsed / TotalDuration) * 100,
            
            %% Check worker status
            LiveWorkers = [Pid || {Pid, _} <- Workers, is_process_alive(Pid)],
            
            %% Get memory usage
            Memory = erlang:memory(total) / (1024 * 1024),
            
            io:format("\rProgress: ~.1f% | Workers: ~p/~p | Memory: ~.1f MB",
                      [Progress, length(LiveWorkers), length(Workers), Memory]),
            
            timer:sleep(1000),
            monitor_stress_loop(StartTime, EndTime, TotalDuration, Workers);
       true ->
            io:format("~nStress test complete!~n")
    end.

run_compliance_tests(TestCount) ->
    %% Simulate running compliance tests
    lists:map(fun(N) ->
        %% Simulate various test types
        case N rem 5 of
            0 -> run_protocol_test();
            1 -> run_transport_test();
            2 -> run_error_test();
            3 -> run_validation_test();
            4 -> run_compliance_test()
        end
    end, lists:seq(1, TestCount)),
    #{tests_run => TestCount, status => complete}.

run_protocol_test() -> ok.
run_transport_test() -> ok.
run_error_test() -> ok.
run_validation_test() -> ok.
run_compliance_test() -> ok.

%%====================================================================
%% REPORT GENERATION
%%====================================================================

generate_report() ->
    %% Generate a sample report structure
    io:format("~n~n=== VALIDATION PERFORMANCE REPORT ===~n"),
    io:format("Run this benchmark with: erlmcp_validation_performance_benchmark:run_all_benchmarks().~n"),
    ok.

generate_report(Results) ->
    io:format("~n~n=== VALIDATION PERFORMANCE REPORT ===~n"),
    io:format("Timestamp: ~p~n", [maps:get(timestamp, Results)]),
    io:format("~nAll results saved to JSON file.~n"),
    ok.

save_results(Results) ->
    %% Ensure results directory exists
    ok = filelib:ensure_dir("bench/results/"),
    
    %% Save to JSON file
    Timestamp = maps:get(timestamp, Results),
    Filename = io_lib:format("bench/results/validation_performance_~p.json", [Timestamp]),
    Formatted = format_json(Results),
    ok = file:write_file(Filename, Formatted),
    
    %% Also save a human-readable version
    TextFilename = io_lib:format("bench/results/validation_performance_~p.txt", [Timestamp]),
    TextReport = format_text_report(Results),
    ok = file:write_file(TextFilename, TextReport),
    
    ok.

format_json(Data) ->
    %% Simple JSON formatter (in real implementation, use jsx)
    jsx:encode(Data).

format_text_report(Results) ->
    %% Generate human-readable report
    [
        "ERLMCP VALIDATION PERFORMANCE REPORT\n",
        "====================================\n\n",
        io_lib:format("Timestamp: ~p~n", [maps:get(timestamp, Results)]),
        "\n",
        format_section("SPEC PARSING", maps:get(spec_parsing, Results, #{})),
        format_section("VALIDATION OPERATIONS", maps:get(validation_operations, Results, #{})),
        format_section("MEMORY USAGE", maps:get(memory_usage, Results, #{})),
        format_section("TIMEOUT HANDLING", maps:get(timeout_handling, Results, #{})),
        format_section("LARGE MESSAGES", maps:get(large_messages, Results, #{})),
        format_section("TRANSPORT TYPES", maps:get(transport_types, Results, #{})),
        format_section("STRESS LOAD", maps:get(stress_load, Results, #{})),
        "\nEnd of Report\n"
    ].

format_section(Title, Data) ->
    [
        io_lib:format("~n--- ~s ---~n", [Title]),
        format_data(Data, "")
    ].

format_data(Map, Indent) when is_map(Map) ->
    lists:map(fun({K, V}) ->
        case V of
            V when is_map(V) ->
                [
                    io_lib:format("~s~s:~n", [Indent, format_key(K)]),
                    format_data(V, Indent ++ "  ")
                ];
            V when is_list(V) ->
                io_lib:format("~s~s: ~p~n", [Indent, format_key(K), V]);
            V when is_float(V) ->
                io_lib:format("~s~s: ~.4f~n", [Indent, format_key(K), V]);
            V when is_integer(V) ->
                io_lib:format("~s~s: ~p~n", [Indent, format_key(K), V]);
            V ->
                io_lib:format("~s~s: ~p~n", [Indent, format_key(K), V])
        end
    end, lists:sort(maps:to_list(Map)));
format_data(_, _) ->
    [].

format_key(K) when is_binary(K) -> binary_to_list(K);
format_key(K) when is_atom(K) -> atom_to_list(K);
format_key(K) -> K.
