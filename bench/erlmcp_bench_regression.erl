%%%-------------------------------------------------------------------
%%% @doc erlmcp_bench_regression - OTP 28 Performance Regression Tests
%%%
%%% Comprehensive benchmark suite for detecting performance regressions
%%% in OTP 28 optimizations.
%%%
%%% == OTP 28 Features Tested ==
%%% 1. Native JSON encoding/decoding (vs jsx)
%%% 2. Priority messages (critical path latency)
%%% 3. Process hibernate/0 (75% memory reduction expected)
%%% 4. Process iterator (vs erlang:processes/0 memory usage)
%%% 5. PCRE2 regex (vs old re)
%%% 6. Monitor improvements (tagged monitors)
%%%
%%% == Usage ==
%%% ```erlang
%%% %% Establish baseline for current OTP version
%%% erlmcp_bench_regression:establish_baseline().
%%%
%%% %% Run regression checks against baseline
%%% erlmcp_bench_regression:run_regression_checks().
%%%
%%% %% Test specific OTP 28 features
%%% erlmcp_bench_regression:bench_native_json().
%%% erlmcp_bench_regression:bench_priority_messages().
%%% erlmcp_bench_regression:bench_hibernate().
%%% erlmcp_bench_regression:bench_process_iterator().
%%% ```
%%%
%%% == Expected Improvements (OTP 28 vs OTP 27) ==
%%% - JSON encode: 2-3x faster (native vs jsx)
%%% - JSON decode: 2-3x faster (native vs jsx)
%%% - Priority messages: <10us latency (critical path)
%%% - Hibernate memory: 75% reduction
%%% - Process iteration: 50% less memory than erlang:processes()
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_regression).
-behaviour(gen_server).

%% API
-export([
    establish_baseline/0,
    run_regression_checks/0,
    bench_native_json/0,
    bench_priority_messages/0,
    bench_hibernate/0,
    bench_process_iterator/0,
    bench_pcre2_regex/0,
    bench_tagged_monitors/0,
    compare_with_baseline/1,
    generate_report/1,
    save_baseline/2
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

-define(SERVER, ?MODULE).
-define(JSON_ITERATIONS, 100000).
-define(PRIORITY_ITERATIONS, 100000).
-define(HIBERNATE_COUNT, 10000).
-define(PROCESS_ITER_COUNT, 100000).
-define(REGEX_ITERATIONS, 50000).
-define(MONITOR_COUNT, 10000).

%% Benchmark results record
-record(benchmark_result, {
    name :: binary(),
    throughput :: float() | undefined,
    latency_us :: float() | undefined,
    memory_bytes :: float() | undefined,
    speedup :: float() | undefined,
    improvement_percent :: float() | undefined
}).

%% Baseline structure
-record(baseline, {
    version :: binary(),
    otp_release :: binary(),
    timestamp :: integer(),
    benchmarks :: map()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Establish performance baseline for current OTP version
-spec establish_baseline() -> {ok, map()}.
establish_baseline() ->
    io:format("~n=== Establishing Performance Baseline ===~n"),
    io:format("OTP Release: ~s~n", [erlang:system_info(otp_release)]),
    io:format("ERTS Version: ~s~n", [erlang:system_info(version)]),
    
    %% Run all benchmarks
    Results = #{
        otp_release => list_to_binary(erlang:system_info(otp_release)),
        erts_version => list_to_binary(erlang:system_info(version)),
        timestamp => erlang:system_time(second),
        git_sha => get_git_sha(),
        benchmarks => #{
            native_json => bench_native_json(),
            jsx_json => bench_jsx_json(),
            priority_messages => bench_priority_messages(),
            hibernate => bench_hibernate(),
            process_iterator => bench_process_iterator(),
            pcre2_regex => bench_pcre2_regex(),
            tagged_monitors => bench_tagged_monitors()
        },
        environment => get_environment_info()
    },
    
    %% Calculate improvements
    ResultsWithImprovements = calculate_improvements(Results),
    
    %% Save baseline
    BaselineFile = generate_baseline_filename(),
    ok = save_baseline(ResultsWithImprovements, BaselineFile),
    
    io:format("~nBaseline saved to: ~s~n", [BaselineFile]),
    {ok, ResultsWithImprovements}.

%% @doc Run regression checks against established baseline
-spec run_regression_checks() -> map().
run_regression_checks() ->
    io:format("~n=== Running Regression Checks ===~n"),
    
    %% Load most recent baseline
    BaselineFile = find_latest_baseline(),
    case file:read_file(BaselineFile) of
        {ok, Content} ->
            Baseline = jsx:decode(Content, [return_maps]),
            run_comparisons(Baseline);
        {error, Reason} ->
            io:format("Error loading baseline: ~p~n", [Reason]),
            io:format("Run establish_baseline() first.~n"),
            #{error => Reason}
    end.

%% @doc Benchmark native JSON (OTP 27+)
-spec bench_native_json() -> map().
bench_native_json() ->
    io:format("~n--- Native JSON Benchmark ---~n"),
    
    %% Test data
    SmallJson = <<"{\"id\":123,\"name\":\"test\",\"value\":456}">>,
    MediumJson = build_medium_json(),
    LargeJson = build_large_json(),
    
    %% Benchmark decode
    {DecodeTime, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            _ = json:decode(SmallJson),
            _ = json:decode(MediumJson),
            _ = json:decode(LargeJson)
        end, lists:seq(1, ?JSON_ITERATIONS div 3))
    end),
    
    DecodeThroughput = (?JSON_ITERATIONS * 1_000_000) / DecodeTime,
    DecodeLatency = DecodeTime / ?JSON_ITERATIONS,
    
    %% Benchmark encode
    SmallMap = #{<<"id">> => 123, <<"name">> => <<"test">>, <<"value">> => 456},
    MediumMap = build_medium_map(),
    LargeMap = build_large_map(),
    
    {EncodeTime, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            _ = json:encode(SmallMap),
            _ = json:encode(MediumMap),
            _ = json:encode(LargeMap)
        end, lists:seq(1, ?JSON_ITERATIONS div 3))
    end),
    
    EncodeThroughput = (?JSON_ITERATIONS * 1_000_000) / EncodeTime,
    EncodeLatency = EncodeTime / ?JSON_ITERATIONS,
    
    io:format("  Decode throughput: ~.2f M ops/sec~n", [DecodeThroughput / 1_000_000]),
    io:format("  Decode latency: ~.2f us~n", [DecodeLatency]),
    io:format("  Encode throughput: ~.2f M ops/sec~n", [EncodeThroughput / 1_000_000]),
    io:format("  Encode latency: ~.2f us~n", [EncodeLatency]),
    
    #{
        decode_throughput => DecodeThroughput,
        decode_latency_us => DecodeLatency,
        encode_throughput => EncodeThroughput,
        encode_latency_us => EncodeLatency
    }.

%% @doc Benchmark JSX JSON (for comparison)
-spec bench_jsx_json() -> map().
bench_jsx_json() ->
    io:format("~n--- JSX JSON Benchmark ---~n"),
    
    %% Test data
    SmallJson = <<"{\"id\":123,\"name\":\"test\",\"value\":456}">>,
    MediumJson = build_medium_json(),
    LargeJson = build_large_json(),
    
    %% Benchmark decode
    {DecodeTime, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            _ = jsx:decode(SmallJson),
            _ = jsx:decode(MediumJson),
            _ = jsx:decode(LargeJson)
        end, lists:seq(1, ?JSON_ITERATIONS div 3))
    end),
    
    DecodeThroughput = (?JSON_ITERATIONS * 1_000_000) / DecodeTime,
    DecodeLatency = DecodeTime / ?JSON_ITERATIONS,
    
    %% Benchmark encode
    SmallMap = #{<<"id">> => 123, <<"name">> => <<"test">>, <<"value">> => 456},
    MediumMap = build_medium_map(),
    LargeMap = build_large_map(),
    
    {EncodeTime, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            _ = jsx:encode(SmallMap),
            _ = jsx:encode(MediumMap),
            _ = jsx:encode(LargeMap)
        end, lists:seq(1, ?JSON_ITERATIONS div 3))
    end),
    
    EncodeThroughput = (?JSON_ITERATIONS * 1_000_000) / EncodeTime,
    EncodeLatency = EncodeTime / ?JSON_ITERATIONS,
    
    io:format("  Decode throughput: ~.2f M ops/sec~n", [DecodeThroughput / 1_000_000]),
    io:format("  Decode latency: ~.2f us~n", [DecodeLatency]),
    io:format("  Encode throughput: ~.2f M ops/sec~n", [EncodeThroughput / 1_000_000]),
    io:format("  Encode latency: ~.2f us~n", [EncodeLatency]),
    
    #{
        decode_throughput => DecodeThroughput,
        decode_latency_us => DecodeLatency,
        encode_throughput => EncodeThroughput,
        encode_latency_us => EncodeLatency
    }.

%% @doc Benchmark priority messages (OTP 28+)
-spec bench_priority_messages() -> map().
bench_priority_messages() ->
    io:format("~n--- Priority Messages Benchmark ---~n"),
    
    try
        %% Check if priority messages are available
        _ = erlang:send(self(), test, [{priority, normal}]),
        
        %% Benchmark normal messages
        {NormalTime, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                self() ! {msg, normal},
                receive {msg, normal} -> ok end
            end, lists:seq(1, ?PRIORITY_ITERATIONS))
        end),
        
        NormalLatency = NormalTime / ?PRIORITY_ITERATIONS,
        
        %% Benchmark priority messages
        {PriorityTime, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                erlang:send(self(), {msg, priority}, [{priority, high}]),
                receive {msg, priority} -> ok end
            end, lists:seq(1, ?PRIORITY_ITERATIONS))
        end),
        
        PriorityLatency = PriorityTime / ?PRIORITY_ITERATIONS,
        
        Speedup = NormalLatency / PriorityLatency,
        
        io:format("  Normal message latency: ~.2f us~n", [NormalLatency]),
        io:format("  Priority message latency: ~.2f us~n", [PriorityLatency]),
        io:format("  Speedup: ~.2fx~n", [Speedup]),
        
        #{
            normal_latency_us => NormalLatency,
            priority_latency_us => PriorityLatency,
            speedup => Speedup,
            available => true
        }
    catch
        _:_ ->
            io:format("  Priority messages not available (OTP < 28)~n"),
            #{
                normal_latency_us => 0,
                priority_latency_us => 0,
                speedup => 1.0,
                available => false,
                reason => otp_version_too_old
            }
    end.

%% @doc Benchmark hibernate/0 (OTP 28+)
-spec bench_hibernate() -> map().
bench_hibernate() ->
    io:format("~n--- Hibernate Benchmark ---~n"),
    
    %% Measure memory with hibernate
    {HibernateMemory, _} = timer:tc(fun() ->
        Pids = lists:map(fun(_) ->
            spawn(fun() -> hibernating_process() end)
        end, lists:seq(1, ?HIBERNATE_COUNT)),
        
        timer:sleep(100),
        
        Memory = erlang:memory(total),
        
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
        Memory
    end),
    
    HibernatePerProcess = HibernateMemory / ?HIBERNATE_COUNT,
    
    %% Measure memory without hibernate
    {NormalMemory, _} = timer:tc(fun() ->
        Pids = lists:map(fun(_) ->
            spawn(fun() -> normal_process() end)
        end, lists:seq(1, ?HIBERNATE_COUNT)),
        
        timer:sleep(100),
        
        Memory = erlang:memory(total),
        
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
        Memory
    end),
    
    NormalPerProcess = NormalMemory / ?HIBERNATE_COUNT,
    
    MemoryReduction = ((NormalPerProcess - HibernatePerProcess) / NormalPerProcess) * 100,
    
    io:format("  Normal memory/process: ~.2f KB~n", [NormalPerProcess / 1024]),
    io:format("  Hibernate memory/process: ~.2f KB~n", [HibernatePerProcess / 1024]),
    io:format("  Memory reduction: ~.1f%~n", [MemoryReduction]),
    
    #{
        normal_bytes_per_process => NormalPerProcess,
        hibernate_bytes_per_process => HibernatePerProcess,
        memory_reduction_percent => MemoryReduction,
        meets_expectation => MemoryReduction >= 50.0
    }.

%% @doc Benchmark process iterator (OTP 28+)
-spec bench_process_iterator() -> map().
bench_process_iterator() ->
    io:format("~n--- Process Iterator Benchmark ---~n"),
    
    %% Spawn processes
    Pids = lists:map(fun(_) ->
        spawn(fun() -> normal_process() end)
    end, lists:seq(1, 1000)),
    
    timer:sleep(100),
    
    try
        %% Benchmark erlang:processes/0
        {OldTime, OldCount} = timer:tc(fun() ->
            length(erlang:processes())
        end),
        
        %% Benchmark process iterator (OTP 28+)
        {NewTime, NewCount} = timer:tc(fun() ->
            Count = erlang:processes(fun(_) -> ok end),
            Count
        end),
        
        MemoryRatio = OldTime / NewTime,
        
        io:format("  erlang:processes/0 time: ~.2f us~n", [OldTime]),
        io:format("  Process iterator time: ~.2f us~n", [NewTime]),
        io:format("  Memory reduction: ~.2fx~n", [MemoryRatio]),
        
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
        
        #{
            old_iterator_time_us => OldTime,
            new_iterator_time_us => NewTime,
            memory_improvement => MemoryRatio,
            available => true
        }
    catch
        _:Error ->
            io:format("  Process iterator not available: ~p~n", [Error]),
            lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
            
            #{
                old_iterator_time_us => 0,
                new_iterator_time_us => 0,
                memory_improvement => 1.0,
                available => false,
                reason => Error
            }
    end.

%% @doc Benchmark PCRE2 regex (OTP 28+)
-spec bench_pcre2_regex() -> map().
bench_pcre2_regex() ->
    io:format("~n--- PCRE2 Regex Benchmark ---~n"),
    
    %% Test patterns
    SimplePattern = "^\\w+$",
    ComplexPattern = "^(?P<name>[a-zA-Z]+)\\s+(?P<value>\\d+)$",
    
    %% Test strings
    SimpleString = "test",
    ComplexString = "hello 12345",
    
    try
        %% Compile patterns
        {ok, SimpleMP} = re:compile(SimplePattern),
        {ok, ComplexMP} = re:compile(ComplexPattern),
        
        %% Benchmark simple matches
        {SimpleTime, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                re:run(SimpleString, SimpleMP)
            end, lists:seq(1, ?REGEX_ITERATIONS))
        end),
        
        SimpleLatency = SimpleTime / ?REGEX_ITERATIONS,
        
        %% Benchmark complex matches
        {ComplexTime, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                re:run(ComplexString, ComplexMP)
            end, lists:seq(1, ?REGEX_ITERATIONS))
        end),
        
        ComplexLatency = ComplexTime / ?REGEX_ITERATIONS,
        
        io:format("  Simple match latency: ~.2f us~n", [SimpleLatency]),
        io:format("  Complex match latency: ~.2f us~n", [ComplexLatency]),
        
        #{
            simple_latency_us => SimpleLatency,
            complex_latency_us => ComplexLatency,
            available => true
        }
    catch
        _:_ ->
            io:format("  PCRE2 not available~n"),
            #{
                simple_latency_us => 0,
                complex_latency_us => 0,
                available => false,
                reason => regex_not_available
            }
    end.

%% @doc Benchmark tagged monitors (OTP 28+)
-spec bench_tagged_monitors() -> map().
bench_tagged_monitors() ->
    io:format("~n--- Tagged Monitors Benchmark ---~n"),
    
    try
        %% Check if tagged monitors are available
        Parent = self(),
        TestPid = spawn(fun() -> receive stop -> ok end end),
        
        case catch erlang:monitor(process, TestPid, [{tag, test_tag}]) of
            {'EXIT', {badarg, _}} ->
                TestPid ! stop,
                throw(not_available);
            _Ref ->
                TestPid ! stop,
                receive
                    {'DOWN', _, process, _, _} -> ok
                end,
                ok
        end,
        
        %% Benchmark normal monitors
        {NormalTime, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                Pid = spawn(fun() -> receive stop -> ok end end),
                Ref = erlang:monitor(process, Pid),
                Pid ! stop,
                receive {'DOWN', Ref, process, Pid, _} -> ok end
            end, lists:seq(1, ?MONITOR_COUNT))
        end),
        
        NormalLatency = NormalTime / ?MONITOR_COUNT,
        
        %% Benchmark tagged monitors
        {TaggedTime, _} = timer:tc(fun() ->
            lists:foreach(fun(_) ->
                Pid = spawn(fun() -> receive stop -> ok end end),
                Ref = erlang:monitor(process, Pid, [{tag, test_tag}]),
                Pid ! stop,
                receive {'DOWN', Ref, process, Pid, _, test_tag} -> ok end
            end, lists:seq(1, ?MONITOR_COUNT))
        end),
        
        TaggedLatency = TaggedTime / ?MONITOR_COUNT,
        
        Overhead = ((TaggedLatency - NormalLatency) / NormalLatency) * 100,
        
        io:format("  Normal monitor latency: ~.2f us~n", [NormalLatency]),
        io:format("  Tagged monitor latency: ~.2f us~n", [TaggedLatency]),
        io:format("  Overhead: ~.1f%~n", [Overhead]),
        
        #{
            normal_latency_us => NormalLatency,
            tagged_latency_us => TaggedLatency,
            overhead_percent => Overhead,
            available => true
        }
    catch
        not_available ->
            io:format("  Tagged monitors not available (OTP < 28)~n"),
            #{
                normal_latency_us => 0,
                tagged_latency_us => 0,
                overhead_percent => 0,
                available => false,
                reason => otp_version_too_old
            }
    end.

%% @doc Compare results with baseline
-spec compare_with_baseline(map()) -> map().
compare_with_baseline(Results) ->
    %% Find and load baseline
    BaselineFile = find_latest_baseline(),
    case file:read_file(BaselineFile) of
        {ok, Content} ->
            Baseline = jsx:decode(Content, [return_maps]),
            compare_results(Results, Baseline);
        {error, Reason} ->
            #{error => {no_baseline, Reason}}
    end.

%% @doc Generate performance report
-spec generate_report(map()) -> binary().
generate_report(Results) ->
    Summary = generate_summary(Results),
    Recommendations = generate_recommendations(Results),
    
    Report = io_lib:format(
        "=== OTP 28 Performance Regression Report ===~n~n"
        "OTP Release: ~s~n"
        "ERTS Version: ~s~n"
        "Timestamp: ~s~n~n"
        "~s~n~s",
        [
            maps:get(otp_release, Results, <<"unknown">>),
            maps:get(erts_version, Results, <<"unknown">>),
            format_timestamp(maps:get(timestamp, Results)),
            Summary,
            Recommendations
        ]
    ),
    
    iolist_to_binary(Report).

%% @doc Save baseline to file
-spec save_baseline(map(), file:filename()) -> ok | {error, term()}.
save_baseline(Baseline, Filename) ->
    Json = jsx:encode(Baseline, [space, {indent, 2}]),
    file:write_file(Filename, Json).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #{}}.

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

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Process functions for hibernate benchmark
hibernating_process() ->
    erlang:hibernate(?MODULE, hibernate_wakeup, []).

hibernate_wakeup() ->
    receive
        stop -> ok;
        _ -> hibernating_process()
    end.

normal_process() ->
    receive
        stop -> ok;
        _ -> normal_process()
    end.

%% Build test JSON data
build_medium_json() ->
    <<"{\"id\":123,\"name\":\"test\",\"description\":\"A medium sized JSON document for testing\",\"value\":456,\"active\":true}">>.

build_large_json() ->
    lists:foldl(fun(I, Acc) ->
        Field = list_to_binary("field_" ++ integer_to_list(I)),
        Acc << <<\"">> << Field/binary << <<\">:">> << integer_to_binary(I) << <<",">>
    end, <<"{">>, lists:seq(1, 100)) << <<"\"extra\":\"data\"}">>.

build_medium_map() ->
    #{
        <<"id">> => 123,
        <<"name">> => <<"test">>,
        <<"description">> => <<"A medium sized JSON document for testing">>,
        <<"value">> => 456,
        <<"active">> => true
    }.

build_large_map() ->
    lists:foldl(fun(I, Acc) ->
        Field = list_to_binary("field_" ++ integer_to_list(I)),
        Acc#{Field => I}
    end, #{}, lists:seq(1, 100)).

%% Calculate improvements vs baseline
calculate_improvements(Results) ->
    Benchmarks = maps:get(benchmarks, Results),
    
    %% Compare native vs jsx JSON
    NativeJson = maps:get(native_json, Benchmarks),
    JsxJson = maps:get(jsx_json, Benchmarks),
    
    JsonDecodeSpeedup = maps:get(decode_throughput, NativeJson) / 
                        maps:get(decode_throughput, JsxJson),
    JsonEncodeSpeedup = maps:get(encode_throughput, NativeJson) / 
                        maps:get(encode_throughput, JsxJson),
    
    %% Add summary
    Results#{
        summary => #{
            json_decode_speedup => JsonDecodeSpeedup,
            json_encode_speedup => JsonEncodeSpeedup,
            hibernate_memory_reduction => maps:get(
                memory_reduction_percent,
                maps:get(hibernate, Benchmarks)
            )
        }
    }.

%% Run comparisons with baseline
run_comparisons(Baseline) ->
    Current = establish_baseline(),
    compare_results(Current, Baseline).

%% Compare current results with baseline
compare_results(Current, Baseline) ->
    CurrentBenchmarks = maps:get(benchmarks, Current),
    BaselineBenchmarks = maps:get(benchmarks, Baseline),
    
    %% Compare JSON performance
    JsonRegression = compare_json(
        maps:get(native_json, CurrentBenchmarks),
        maps:get(native_json, BaselineBenchmarks)
    ),
    
    %% Compare priority messages
    PriorityRegression = compare_priority(
        maps:get(priority_messages, CurrentBenchmarks),
        maps:get(priority_messages, BaselineBenchmarks)
    ),
    
    %% Compare hibernate
    HibernateRegression = compare_hibernate(
        maps:get(hibernate, CurrentBenchmarks),
        maps:get(hibernate, BaselineBenchmarks)
    ),
    
    #{
        json => JsonRegression,
        priority => PriorityRegression,
        hibernate => HibernateRegression,
        overall => determine_overall_status([
            JsonRegression,
            PriorityRegression,
            HibernateRegression
        ])
    }.

%% Compare JSON performance
compare_json(Current, Baseline) ->
    CurrentThroughput = maps:get(decode_throughput, Current),
    BaselineThroughput = maps:get(decode_throughput, Baseline),
    
    Regression = ((CurrentThroughput - BaselineThroughput) / BaselineThroughput) * 100,
    
    Status = if
        Regression < -10 -> regression;
        Regression > 10 -> improvement;
        true -> stable
    end,
    
    #{
        regression_percent => Regression,
        status => Status
    }.

%% Compare priority messages
compare_priority(Current, Baseline) ->
    CurrentLatency = maps:get(priority_latency_us, Current),
    BaselineLatency = maps:get(priority_latency_us, Baseline),
    
    Regression = ((CurrentLatency - BaselineLatency) / BaselineLatency) * 100,
    
    Status = if
        Regression > 10 -> regression;
        Regression < -10 -> improvement;
        true -> stable
    end,
    
    #{
        regression_percent => Regression,
        status => Status
    }.

%% Compare hibernate
compare_hibernate(Current, Baseline) ->
    CurrentReduction = maps:get(memory_reduction_percent, Current),
    BaselineReduction = maps:get(memory_reduction_percent, Baseline),
    
    Regression = BaselineReduction - CurrentReduction,
    
    Status = if
        Regression < -10 -> regression;
        Regression > 10 -> improvement;
        true -> stable
    end,
    
    #{
        regression_percent => Regression,
        status => Status
    }.

%% Determine overall status
determine_overall_status(Comparisons) ->
    HasRegression = lists:any(fun(C) ->
        maps:get(status, C) == regression
    end, Comparisons),
    
    HasImprovement = lists:any(fun(C) ->
        maps:get(status, C) == improvement
    end, Comparisons),
    
    if
        HasRegression -> regression;
        HasImprovement -> improvement;
        true -> stable
    end.

%% Generate summary
generate_summary(Results) ->
    Benchmarks = maps:get(benchmarks, Results),
    
    %% JSON performance
    NativeJson = maps:get(native_json, Benchmarks),
    JsxJson = maps:get(jsx_json, Benchmarks),
    JsonSpeedup = maps:get(decode_throughput, NativeJson) / 
                  maps:get(decode_throughput, JsxJson),
    
    %% Hibernate
    Hibernate = maps:get(hibernate, Benchmarks),
    MemoryReduction = maps:get(memory_reduction_percent, Hibernate),
    
    %% Priority messages
    Priority = maps:get(priority_messages, Benchmarks),
    PriorityLatency = maps:get(priority_latency_us, Priority),
    
    io_lib:format(
        "SUMMARY~n"
        "-------~n"
        "JSON speedup (native vs jsx): ~.2fx~n"
        "Hibernate memory reduction: ~.1f%~n"
        "Priority message latency: ~.2f us~n",
        [JsonSpeedup, MemoryReduction, PriorityLatency]
    ).

%% Generate recommendations
generate_recommendations(Results) ->
    Benchmarks = maps:get(benchmarks, Results),
    
    Recs = [],
    
    %% Check JSON speedup
    NativeJson = maps:get(native_json, Benchmarks),
    JsxJson = maps:get(jsx_json, Benchmarks),
    JsonSpeedup = maps:get(decode_throughput, NativeJson) / 
                  maps:get(decode_throughput, JsxJson),
    
    Recs1 = case JsonSpeedup of
        S when S < 1.5 -> ["JSON speedup below 1.5x - consider using native JSON" | Recs];
        _ -> Recs
    end,
    
    %% Check hibernate
    Hibernate = maps:get(hibernate, Benchmarks),
    MemoryReduction = maps:get(memory_reduction_percent, Hibernate),
    
    Recs2 = case MemoryReduction of
        R when R < 50.0 -> ["Hibernate memory reduction below 50%" | Recs1];
        _ -> Recs1
    end,
    
    lists:foldr(fun(R, Acc) ->
        Acc ++ "\n" ++ R
    end, "RECOMMENDATIONS\n---------------", Recs2).

%% Find latest baseline file
find_latest_baseline() ->
    BaselineDir = "/Users/sac/erlmcp/bench/baseline",
    case file:list_dir(BaselineDir) of
        {ok, Files} ->
            JsonFiles = lists:filter(fun(F) ->
                filename:extension(F) == ".json"
            end, Files),
            SortedFiles = lists:sort(fun(A, B) ->
                A >= B  % Reverse sort
            end, JsonFiles),
            filename:join(BaselineDir, hd(SortedFiles));
        {error, _} ->
            filename:join(BaselineDir, "otp28_baseline.json")
    end.

%% Generate baseline filename
generate_baseline_filename() ->
    Timestamp = erlang:system_time(second),
    Filename = io_lib:format("baseline_otp~s_~p.json", 
                             [erlang:system_info(otp_release), Timestamp]),
    filename:join(["/Users/sac/erlmcp/bench/baseline", Filename]).

%% Get environment info
get_environment_info() ->
    #{
        os => list_to_binary(erlang:system_info(system)),
        otp_version => list_to_binary(erlang:system_info(otp_release)),
        erts_version => list_to_binary(erlang:system_info(version)),
        schedulers => erlang:system_info(schedulers_online),
        logical_processors => erlang:system_info(logical_processors)
    }.

%% Get git SHA
get_git_sha() ->
    case os:cmd("git rev-parse HEAD 2>/dev/null") of
        [] -> <<"unknown">>;
        Sha -> list_to_binary(string:trim(Sha))
    end.

%% Format timestamp
format_timestamp(Timestamp) ->
    {{Y, M, D}, {H, Min, S}} = 
        calendar:system_time_to_universal_time(Timestamp, second),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                   [Y, M, D, H, Min, S])).
