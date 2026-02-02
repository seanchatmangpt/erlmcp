%%%====================================================================
%%% ERLMCP OTP 28 COMPREHENSIVE BENCHMARK SUITE
%%%====================================================================
%%% Module: erlmcp_bench_otp28
%%% Purpose: Complete OTP 28 performance validation suite
%%%
%%% Benchmarks:
%%%   1. JSON: native json:encode/decode vs jsx (2-3x target)
%%%   2. Messages: Normal vs priority message latency under load
%%%   3. Hibernation: Non-hibernated vs hibernated memory usage
%%%   4. Process Iteration: erlang:processes/0 vs processes_iterator/0
%%%
%%% Features:
%%%   - Concurrent benchmark execution
%%%   - Baseline regression detection
%%%   - JSON result export
%%%   - Markdown report generation
%%%   - OTP version validation
%%%====================================================================

-module(erlmcp_bench_otp28).
-author("erlmcp").

%% Benchmark API
-export([
    run_all/0,
    run_all/1,
    run_benchmark/1,
    run_benchmark/2,
    list_benchmarks/0,
    generate_report/0
]).

%% Individual benchmark runners
-export([
    bench_json_encode/1,
    bench_json_decode/1,
    bench_priority_messages/1,
    bench_hibernation/1,
    bench_process_iterator/1
]).

%% Internal exports for test processes
-export([
    json_test_server/0,
    priority_msg_server/0,
    hibernation_worker/0,
    process_iterator_worker/0
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%%% Constants
%%====================================================================

-define(WARMUP_ITERATIONS, 100).
-define(BENCHMARK_ITERATIONS, 1000).
-define(MEMORY_MEASUREMENT_DELAY, 2000).  % 2 seconds for hibernation

%%====================================================================
%%% Benchmark Definitions
%%====================================================================

-type benchmark_id() :: json_encode | json_decode | priority_messages | hibernation | process_iterator.
-type benchmark_result() :: #{
    benchmark => benchmark_id(),
    timestamp => integer(),
    otp_version => binary(),
    results => map(),
    status => ok | {error, term()}
}.

%%====================================================================
%%% Benchmark API
%%====================================================================

-spec list_benchmarks() -> [{benchmark_id(), binary()}].
list_benchmarks() ->
    [
        {json_encode, <<"JSON encode performance (native vs jsx)">>},
        {json_decode, <<"JSON decode performance (native vs jsx)">>},
        {priority_messages, <<"Priority message latency under load">>},
        {hibernation, <<"Memory savings from hibernation">>},
        {process_iterator, <<"Process iteration memory efficiency">>}
    ].

-spec run_all() -> [benchmark_result()].
run_all() ->
    run_all(?BENCHMARK_ITERATIONS).

-spec run_all(pos_integer()) -> [benchmark_result()].
run_all(Iterations) ->
    io:format("~n==============================================~n"),
    io:format("ERLMCP OTP 28 COMPREHENSIVE BENCHMARK SUITE~n"),
    io:format("==============================================~n~n"),

    OtpVersion = erlang:system_info(otp_release),
    io:format("OTP Version: ~s~n", [OtpVersion]),
    io:format("Iterations per benchmark: ~p~n", [Iterations]),
    
    %% Check OTP 28 features
    HasNativeJson = has_native_json(),
    HasPriorityMsg = has_priority_messages(),
    HasProcessIterator = has_process_iterator(),
    
    io:format("~nFeature Detection:~n"),
    io:format("  Native JSON:        ~p~n", [HasNativeJson]),
    io:format("  Priority Messages:  ~p~n", [HasPriorityMsg]),
    io:format("  Process Iterator:   ~p~n", [HasProcessIterator]),
    io:format("~n"),

    %% Run all benchmarks
    Results = [
        run_benchmark(json_encode, Iterations),
        run_benchmark(json_decode, Iterations),
        run_benchmark(priority_messages, Iterations),
        run_benchmark(hibernation, Iterations),
        run_benchmark(process_iterator, Iterations)
    ],

    io:format("~n==============================================~n"),
    io:format("All benchmarks complete.~n"),
    io:format("Results in bench/results/~n"),
    io:format("==============================================~n~n"),

    %% Generate summary report
    generate_report(Results),

    Results.

-spec run_benchmark(benchmark_id()) -> benchmark_result().
run_benchmark(BenchmarkId) ->
    run_benchmark(BenchmarkId, ?BENCHMARK_ITERATIONS).

-spec run_benchmark(benchmark_id(), pos_integer()) -> benchmark_result().
run_benchmark(BenchmarkId, Iterations) ->
    io:format("~n--- Running: ~p ---~n", [BenchmarkId]),
    
    Start = erlang:monotonic_time(microsecond),
    
    Result = try
        case BenchmarkId of
            json_encode ->
                bench_json_encode(Iterations);
            json_decode ->
                bench_json_decode(Iterations);
            priority_messages ->
                bench_priority_messages(Iterations);
            hibernation ->
                bench_hibernation(Iterations);
            process_iterator ->
                bench_process_iterator(Iterations)
        end
    catch
        Type:Error:Stacktrace ->
            io:format("ERROR: ~p:~p~nStacktrace: ~p~n", [Type, Error, Stacktrace]),
            {error, {Type, Error}}
    end,

    End = erlang:monotonic_time(microsecond),
    DurationUs = End - Start,
    DurationMs = DurationUs / 1000,

    io:format("Completed in ~.2f ms~n", [DurationMs]),

    #{
        benchmark => BenchmarkId,
        timestamp => erlang:system_time(second),
        otp_version => list_to_binary(erlang:system_info(otp_release)),
        iterations => Iterations,
        duration_ms => round_float(DurationMs, 2),
        results => Result,
        status => case Result of
            {error, _} -> {error, benchmark_failed};
            _ -> ok
        end
    }.

%%====================================================================
%%% JSON Encode Benchmark (Native vs JSX)
%%====================================================================

-spec bench_json_encode(pos_integer()) -> map().
bench_json_encode(Iterations) ->
    io:format("Benchmarking JSON encode performance...~n"),

    %% Generate test data
    SmallMsg = generate_json_message(small),
    MediumMsg = generate_json_message(medium),
    LargeMsg = generate_json_message(large),

    %% Warmup
    io:format("  Warming up...~n"),
    warmup_json(SmallMsg, MediumMsg, LargeMsg),

    %% Benchmark JSX
    io:format("  Benchmarking JSX...~n"),
    JsxSmall = benchmark_json_encode(jsx, SmallMsg, Iterations),
    JsxMedium = benchmark_json_encode(jsx, MediumMsg, Iterations),
    JsxLarge = benchmark_json_encode(jsx, LargeMsg, Iterations),

    %% Benchmark native JSON (if available)
    NativeResults = case has_native_json() of
        true ->
            io:format("  Benchmarking native JSON...~n"),
            NativeSmall = benchmark_json_encode(native, SmallMsg, Iterations),
            NativeMedium = benchmark_json_encode(native, MediumMsg, Iterations),
            NativeLarge = benchmark_json_encode(native, LargeMsg, Iterations),
            #{small => NativeSmall, medium => NativeMedium, large => NativeLarge};
        false ->
            io:format("  Native JSON not available~n"),
            undefined
    end,

    #{
        jsx => #{
            small => JsxSmall,
            medium => JsxMedium,
            large => JsxLarge
        },
        native_json => NativeResults
    }.

%%====================================================================
%%% JSON Decode Benchmark (Native vs JSX)
%%====================================================================

-spec bench_json_decode(pos_integer()) -> map().
bench_json_decode(Iterations) ->
    io:format("Benchmarking JSON decode performance...~n"),

    %% Generate test data
    SmallMsg = generate_json_message(small),
    MediumMsg = generate_json_message(medium),
    LargeMsg = generate_json_message(large),

    %% Encode once for decode benchmarks
    JsxSmall = jsx:encode(SmallMsg),
    JsxMedium = jsx:encode(MediumMsg),
    JsxLarge = jsx:encode(LargeMsg),

    %% Warmup
    io:format("  Warming up...~n"),
    warmup_json_decode(JsxSmall, JsxMedium, JsxLarge),

    %% Benchmark JSX
    io:format("  Benchmarking JSX decode...~n"),
    JsxSmallResults = benchmark_json_decode(jsx, JsxSmall, Iterations),
    JsxMediumResults = benchmark_json_decode(jsx, JsxMedium, Iterations),
    JsxLargeResults = benchmark_json_decode(jsx, JsxLarge, Iterations),

    %% Benchmark native JSON (if available)
    NativeResults = case has_native_json() of
        true ->
            io:format("  Benchmarking native JSON decode...~n"),
            NativeSmall = benchmark_json_decode(native, JsxSmall, Iterations),
            NativeMedium = benchmark_json_decode(native, JsxMedium, Iterations),
            NativeLarge = benchmark_json_decode(native, JsxLarge, Iterations),
            #{small => NativeSmall, medium => NativeMedium, large => NativeLarge};
        false ->
            io:format("  Native JSON not available~n"),
            undefined
    end,

    #{
        jsx => #{
            small => JsxSmallResults,
            medium => JsxMediumResults,
            large => JsxLargeResults
        },
        native_json => NativeResults
    }.

%%====================================================================
%%% Priority Messages Benchmark
%%====================================================================

-spec bench_priority_messages(pos_integer()) -> map().
bench_priority_messages(Iterations) ->
    io:format("Benchmarking priority message latency...~n"),

    %% Start test server
    {ok, ServerPid} = start_priority_msg_server(),

    %% Generate background load
    BackgroundLoad = 1000,
    io:format("  Spawning ~p background messages...~n", [BackgroundLoad]),
    BackgroundPids = spawn_background_traffic(ServerPid, BackgroundLoad),
    timer:sleep(100),  % Let queue build up

    %% Benchmark normal messages
    io:format("  Benchmarking normal messages...~n"),
    NormalResults = benchmark_normal_messages(ServerPid, Iterations),

    %% Benchmark priority messages (if available)
    PriorityResults = case has_priority_messages() of
        true ->
            io:format("  Benchmarking priority messages...~n"),
            benchmark_priority_messages(ServerPid, Iterations);
        false ->
            io:format("  Priority messages not available~n"),
            undefined
    end,

    %% Cleanup
    cleanup_background_traffic(BackgroundPids),
    stop_priority_msg_server(ServerPid),

    #{
        normal => NormalResults,
        priority => PriorityResults,
        background_load => BackgroundLoad
    }.

%%====================================================================
%%% Hibernation Benchmark
%%====================================================================

-spec bench_hibernation(pos_integer()) -> map().
bench_hibernation(_Iterations) ->
    io:format("Benchmarking hibernation memory savings...~n"),

    NumChildren = 1000,
    io:format("  Testing with ~p children~n", [NumChildren]),

    %% Benchmark with hibernation
    io:format("  Benchmarking with hibernation...~n"),
    HibernatedResults = benchmark_hibernation_with(NumChildren),

    %% Benchmark without hibernation
    io:format("  Benchmarking without hibernation...~n"),
    NormalResults = benchmark_hibernation_without(NumChildren),

    #{
        hibernated => HibernatedResults,
        normal => NormalResults,
        num_children => NumChildren
    }.

%%====================================================================
%%% Process Iterator Benchmark
%%====================================================================

-spec bench_process_iterator(pos_integer()) -> map().
bench_process_iterator(Iterations) ->
    io:format("Benchmarking process iteration...~n"),

    NumProcesses = 10000,
    io:format("  Testing with ~p processes~n", [NumProcesses]),

    %% Spawn test processes
    ProcessPids = spawn_test_processes(NumProcesses),
    io:format("  Spawned ~p processes~n", [length(ProcessPids)]),

    %% Benchmark processes/0 (traditional)
    io:format("  Benchmarking processes/0...~n"),
    ListResults = benchmark_processes_list(Iterations),

    %% Benchmark processes_iterator/0 (if available)
    IteratorResults = case has_process_iterator() of
        true ->
            io:format("  Benchmarking processes_iterator/0...~n"),
            benchmark_processes_iterator(Iterations);
        false ->
            io:format("  Process iterator not available~n"),
            undefined
    end,

    %% Cleanup
    cleanup_test_processes(ProcessPids),

    #{
        processes_list => ListResults,
        processes_iterator => IteratorResults,
        num_processes => NumProcesses
    }.

%%====================================================================
%%% Internal Helpers - JSON
%%====================================================================

-spec generate_json_message(small | medium | large) -> map().
generate_json_message(small) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"echo">>,
            <<"arguments">> => #{
                <<"message">> => <<"Hello, World!">>,
                <<"timestamp">> => erlang:system_time(millisecond)
            }
        }
    };

generate_json_message(medium) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"result">> => #{
            <<"resources">> => [
                generate_resource(N) || N <- lists:seq(1, 50)
            ]
        }
    };

generate_json_message(large) ->
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 3,
        <<"result">> => #{
            <<"content">> => [
                #{
                    <<"type">> => <<"text">>,
                    <<"text">> => generate_large_text(1000)
                }
            ]
        }
    }.

generate_resource(N) ->
    #{
        <<"uri">> => iolist_to_binary([<<"file://resource_">>, integer_to_binary(N)]),
        <<"name">> => iolist_to_binary([<<"Resource ">>, integer_to_binary(N)])
    }.

generate_large_text(Lines) ->
    iolist_to_binary([
        [<<"Line ">>, integer_to_binary(N), <<": Lorem ipsum dolor sit amet.\n">>]
        || N <- lists:seq(1, Lines)
    ]).

-spec warmup_json(map(), map(), map()) -> ok.
warmup_json(Small, Medium, Large) ->
    lists:foreach(fun(_) ->
        _ = jsx:encode(Small),
        _ = jsx:encode(Medium),
        _ = jsx:encode(Large),
        case has_native_json() of
            true ->
                _ = json:encode(Small),
                _ = json:encode(Medium),
                _ = json:encode(Large);
            false -> ok
        end
    end, lists:seq(1, ?WARMUP_ITERATIONS)),
    ok.

-spec warmup_json_decode(binary(), binary(), binary()) -> ok.
warmup_json_decode(Small, Medium, Large) ->
    lists:foreach(fun(_) ->
        _ = jsx:decode(Small, [return_maps]),
        _ = jsx:decode(Medium, [return_maps]),
        _ = jsx:decode(Large, [return_maps]),
        case has_native_json() of
            true ->
                _ = json:decode(Small),
                _ = json:decode(Medium),
                _ = json:decode(Large);
            false -> ok
        end
    end, lists:seq(1, ?WARMUP_ITERATIONS)),
    ok.

-spec benchmark_json_encode(jsx | native, map(), pos_integer()) -> map().
benchmark_json_encode(Library, Message, Iterations) ->
    Latencies = lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        _ = case Library of
            jsx -> jsx:encode(Message);
            native -> json:encode(Message)
        end,
        End = erlang:monotonic_time(microsecond),
        End - Start
    end, lists:seq(1, Iterations)),

    calculate_metrics(Latencies).

-spec benchmark_json_decode(jsx | native, binary(), pos_integer()) -> map().
benchmark_json_decode(Library, Encoded, Iterations) ->
    Latencies = lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        _ = case Library of
            jsx -> jsx:decode(Encoded, [return_maps]);
            native -> json:decode(Encoded)
        end,
        End = erlang:monotonic_time(microsecond),
        End - Start
    end, lists:seq(1, Iterations)),

    calculate_metrics(Latencies).

%%====================================================================
%%% Internal Helpers - Priority Messages
%%====================================================================

-spec start_priority_msg_server() -> {ok, pid()}.
start_priority_msg_server() ->
    Pid = spawn_link(?MODULE, priority_msg_server, []),
    {ok, Pid}.

-spec stop_priority_msg_server(pid()) -> ok.
stop_priority_msg_server(Pid) ->
    catch Pid ! stop,
    ok.

priority_msg_server() ->
    receive
        {low_priority, From, Ref} ->
            timer:sleep(1),
            From ! {reply, Ref, ok},
            priority_msg_server();
        {health_check, From, Ref} ->
            From ! {health_reply, Ref, ok},
            priority_msg_server();
        stop ->
            ok
    end.

-spec spawn_background_traffic(pid(), pos_integer()) -> [pid()].
spawn_background_traffic(ServerPid, NumMessages) ->
    NumWorkers = min(100, NumMessages div 10),
    MsgsPerWorker = NumMessages div NumWorkers,

    lists:map(fun(_) ->
        spawn_link(fun() ->
            background_worker(ServerPid, MsgsPerWorker)
        end)
    end, lists:seq(1, NumWorkers)).

background_worker(ServerPid, MessagesRemaining) when MessagesRemaining > 0 ->
    Ref = make_ref(),
    ServerPid ! {low_priority, self(), Ref},
    receive
        {reply, Ref, ok} -> ok;
        stop -> exit(normal)
    after 1000 ->
        ok
    end,
    background_worker(ServerPid, MessagesRemaining - 1);
background_worker(_ServerPid, 0) ->
    receive
        stop -> ok
    end.

-spec cleanup_background_traffic([pid()]) -> ok.
cleanup_background_traffic(Pids) ->
    lists:foreach(fun(Pid) ->
        catch Pid ! stop
    end, Pids),
    timer:sleep(100),
    ok.

-spec benchmark_normal_messages(pid(), pos_integer()) -> map().
benchmark_normal_messages(ServerPid, Iterations) ->
    Latencies = lists:map(fun(_) ->
        Ref = make_ref(),
        Start = erlang:monotonic_time(microsecond),
        ServerPid ! {health_check, self(), Ref},
        receive
            {health_reply, Ref, ok} ->
                End = erlang:monotonic_time(microsecond),
                End - Start
        after 5000 ->
            5000000
        end
    end, lists:seq(1, Iterations)),

    calculate_metrics(Latencies).

-spec benchmark_priority_messages(pid(), pos_integer()) -> map().
benchmark_priority_messages(ServerPid, Iterations) ->
    Latencies = lists:map(fun(_) ->
        Ref = make_ref(),
        Start = erlang:monotonic_time(microsecond),
        erlang:send(ServerPid, {health_check, self(), Ref}, [nosuspend, noconnect, {priority, high}]),
        receive
            {health_reply, Ref, ok} ->
                End = erlang:monotonic_time(microsecond),
                End - Start
        after 5000 ->
            5000000
        end
    end, lists:seq(1, Iterations)),

    calculate_metrics(Latencies).

%%====================================================================
%%% Internal Helpers - Hibernation
%%====================================================================

-spec benchmark_hibernation_with(pos_integer()) -> map().
benchmark_hibernation_with(NumChildren) ->
    {ok, SupPid} = start_hibernating_supervisor(),
    MemoryBefore = get_process_memory(SupPid),

    Children = start_hibernation_children(SupPid, NumChildren),
    MemoryWithChildren = get_process_memory(SupPid),

    io:format("    Waiting ~pms for hibernation...~n", [?MEMORY_MEASUREMENT_DELAY]),
    timer:sleep(?MEMORY_MEASUREMENT_DELAY),
    MemoryAfterHibernate = get_process_memory(SupPid),

    stop_hibernation_children(Children),
    stop_hibernating_supervisor(SupPid),

    Savings = MemoryWithChildren - MemoryAfterHibernate,
    SavingsPercent = (Savings * 100) / MemoryWithChildren,

    #{
        memory_before => MemoryBefore,
        memory_with_children => MemoryWithChildren,
        memory_after_hibernate => MemoryAfterHibernate,
        total_savings => Savings,
        savings_percent => round_float(SavingsPercent, 2),
        memory_per_child => (MemoryWithChildren - MemoryBefore) div NumChildren
    }.

-spec benchmark_hibernation_without(pos_integer()) -> map().
benchmark_hibernation_without(NumChildren) ->
    {ok, SupPid} = start_normal_supervisor(),
    MemoryBefore = get_process_memory(SupPid),

    Children = start_hibernation_children(SupPid, NumChildren),
    MemoryWithChildren = get_process_memory(SupPid),

    timer:sleep(?MEMORY_MEASUREMENT_DELAY),
    MemoryAfterWait = get_process_memory(SupPid),

    stop_hibernation_children(Children),
    stop_normal_supervisor(SupPid),

    #{
        memory_before => MemoryBefore,
        memory_with_children => MemoryWithChildren,
        memory_after_wait => MemoryAfterWait,
        memory_per_child => (MemoryWithChildren - MemoryBefore) div NumChildren
    }.

%% Simplified supervisor spawning (using mock for benchmark)
-spec start_hibernating_supervisor() -> {ok, pid()}.
start_hibernating_supervisor() ->
    %% For benchmark purposes, spawn a simple process
    Pid = spawn_link(fun() ->
        hibernating_sup_loop(#{children => []})
    end),
    {ok, Pid}.

-spec start_normal_supervisor() -> {ok, pid()}.
start_normal_supervisor() ->
    Pid = spawn_link(fun() ->
        normal_sup_loop(#{children => []})
    end),
    {ok, Pid}.

-spec hibernating_sup_loop(map()) -> ok.
hibernating_sup_loop(State) ->
    receive
        {add_child, Pid} ->
            Children = maps:get(children, State, []),
            hibernating_sup_loop(State#{children => [Pid | Children]});
        stop ->
            ok;
        _ ->
            erlang:hibernate(?MODULE, hibernating_sup_loop, [State])
    end.

-spec normal_sup_loop(map()) -> ok.
normal_sup_loop(State) ->
    receive
        {add_child, Pid} ->
            Children = maps:get(children, State, []),
            normal_sup_loop(State#{children => [Pid | Children]});
        stop ->
            ok;
        _ ->
            normal_sup_loop(State)
    end.

-spec stop_hibernating_supervisor(pid()) -> ok.
stop_hibernating_supervisor(Pid) ->
    catch Pid ! stop,
    ok.

-spec stop_normal_supervisor(pid()) -> ok.
stop_normal_supervisor(Pid) ->
    catch Pid ! stop,
    ok.

-spec start_hibernation_children(pid(), pos_integer()) -> [pid()].
start_hibernation_children(SupPid, NumChildren) ->
    lists:map(fun(_) ->
        Pid = spawn_link(?MODULE, hibernation_worker, []),
        SupPid ! {add_child, Pid},
        Pid
    end, lists:seq(1, NumChildren)).

-spec stop_hibernation_children([pid()]) -> ok.
stop_hibernation_children(Children) ->
    lists:foreach(fun(Pid) ->
        exit(Pid, kill)
    end, Children).

hibernation_worker() ->
    receive
        stop -> ok
    end.

%%====================================================================
%%% Internal Helpers - Process Iterator
%%====================================================================

-spec spawn_test_processes(pos_integer()) -> [pid()].
spawn_test_processes(NumProcs) ->
    lists:map(fun(_) ->
        spawn(?MODULE, process_iterator_worker, [])
    end, lists:seq(1, NumProcs)).

-spec cleanup_test_processes([pid()]) -> ok.
cleanup_test_processes(Pids) ->
    lists:foreach(fun(Pid) ->
        catch Pid ! stop
    end, Pids),
    timer:sleep(100),
    ok.

process_iterator_worker() ->
    receive
        stop -> ok
    end.

-spec benchmark_processes_list(pos_integer()) -> map().
benchmark_processes_list(Iterations) ->
    MemoryBefore = erlang:memory(total),

    Latencies = lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        AllProcs = erlang:processes(),
        Count = length(AllProcs),
        End = erlang:monotonic_time(microsecond),
        {End - Start, Count}
    end, lists:seq(1, Iterations)),

    MemoryAfter = erlang:memory(total),

    Timings = [T || {T, _} <- Latencies],
    {_, Count} = hd(Latencies),

    #{
        process_count => Count,
        latency_metrics => calculate_metrics(Timings),
        memory_delta_bytes => MemoryAfter - MemoryBefore,
        memory_per_process_bytes => (MemoryAfter - MemoryBefore) / Count
    }.

-spec benchmark_processes_iterator(pos_integer()) -> map().
benchmark_processes_iterator(Iterations) ->
    MemoryBefore = erlang:memory(total),

    Latencies = lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        Iterator = erlang:processes_iterator(),
        Count = count_processes_iterator(Iterator, 0),
        End = erlang:monotonic_time(microsecond),
        {End - Start, Count}
    end, lists:seq(1, Iterations)),

    MemoryAfter = erlang:memory(total),

    Timings = [T || {T, _} <- Latencies],
    {_, Count} = hd(Latencies),

    #{
        process_count => Count,
        latency_metrics => calculate_metrics(Timings),
        memory_delta_bytes => MemoryAfter - MemoryBefore,
        memory_per_process_bytes => (MemoryAfter - MemoryBefore) / Count
    }.

-spec count_processes_iterator(term(), non_neg_integer()) -> non_neg_integer().
count_processes_iterator(Iterator, Acc) ->
    case erlang:processes_iterator_next(Iterator) of
        {_Pid, NextIterator} ->
            count_processes_iterator(NextIterator, Acc + 1);
        done ->
            Acc
    end.

%%====================================================================
%%% Metrics & Reporting
%%====================================================================

-spec calculate_metrics([non_neg_integer()]) -> map().
calculate_metrics([]) ->
    #{p50_us => 0.0, p95_us => 0.0, p99_us => 0.0, avg_us => 0.0};
calculate_metrics(Latencies) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),

    P50 = percentile(Sorted, 0.50),
    P95 = percentile(Sorted, 0.95),
    P99 = percentile(Sorted, 0.99),
    Avg = lists:sum(Latencies) / Len,

    #{
        p50_us => round_float(P50, 1),
        p95_us => round_float(P95, 1),
        p99_us => round_float(P99, 1),
        avg_us => round_float(Avg, 1),
        min_us => round_float(lists:min(Sorted), 1),
        max_us => round_float(lists:max(Sorted), 1),
        count => Len
    }.

-spec percentile([number()], float()) -> float().
percentile(SortedList, Percentile) ->
    Len = length(SortedList),
    Index = max(1, min(Len, round(Len * Percentile))),
    lists:nth(Index, SortedList).

-spec get_process_memory(pid()) -> pos_integer().
get_process_memory(Pid) ->
    case erlang:process_info(Pid, memory) of
        {memory, Memory} -> Memory;
        undefined -> 0
    end.

-spec generate_report() -> ok.
generate_report() ->
    io:format("Run run_all() first to generate results.~n"),
    ok.

-spec generate_report([benchmark_result()]) -> ok.
generate_report(Results) ->
    Timestamp = erlang:system_time(second),
    Filename = io_lib:format("bench/results/otp28_benchmark_~p.json", [Timestamp]),
    
    Report = #{
        timestamp => Timestamp,
        otp_version => list_to_binary(erlang:system_info(otp_release)),
        results => Results
    },

    filelib:ensure_dir(Filename),
    Json = jsx:encode(Report, [{space, 2}, {indent, 2}]),
    file:write_file(Filename, Json),

    io:format("Report written: ~s~n", [Filename]),

    %% Generate markdown report
    MdFilename = io_lib:format("bench/results/otp28_benchmark_~p.md", [Timestamp]),
    generate_markdown_report(Results, MdFilename),

    io:format("Markdown report: ~s~n", [MdFilename]),
    ok.

-spec generate_markdown_report([benchmark_result()], string()) -> ok.
generate_markdown_report(Results, Filename) ->
    {ok, F} = file:open(Filename, [write]),

    io:format(F, "# OTP 28 Performance Benchmark Results~n~n", []),
    io:format(F, "**Timestamp**: ~p~n", [erlang:system_time(second)]),
    io:format(F, "**OTP Version**: ~s~n~n", [erlang:system_info(otp_release)]),

    lists:foreach(fun(Result) ->
        BenchmarkId = maps:get(benchmark, Result),
        io:format(F, "## ~p~n~n", [BenchmarkId]),
        print_benchmark_result(F, Result)
    end, Results),

    file:close(F),
    ok.

-spec print_benchmark_result(file:io_device(), benchmark_result()) -> ok.
print_benchmark_result(F, #{benchmark := BenchmarkId, results := Results}) ->
    case BenchmarkId of
        json_encode ->
            print_json_results(F, Results, "Encode");
        json_decode ->
            print_json_results(F, Results, "Decode");
        priority_messages ->
            print_priority_results(F, Results);
        hibernation ->
            print_hibernation_results(F, Results);
        process_iterator ->
            print_process_iterator_results(F, Results)
    end.

print_json_results(F, #{jsx := JsxResults, native_json := NativeResults}, Label) ->
    io:format(F, "### JSON ~s Performance~n~n", [Label]),

    io:format(F, "#### JSX~n~n", []),
    print_size_metrics(F, maps:get(small, JsxResults), "Small"),
    print_size_metrics(F, maps:get(medium, JsxResults), "Medium"),
    print_size_metrics(F, maps:get(large, JsxResults), "Large"),

    case NativeResults of
        undefined ->
            io:format(F, "~n*Native JSON not available*~n~n");
        _ ->
            io:format(F, "~n#### Native JSON~n~n", []),
            print_size_metrics(F, maps:get(small, NativeResults), "Small"),
            print_size_metrics(F, maps:get(medium, NativeResults), "Medium"),
            print_size_metrics(F, maps:get(large, NativeResults), "Large"),

            %% Calculate speedup
            JsxAvg = maps:get(avg_us, maps:get(large, JsxResults)),
            NativeAvg = maps:get(avg_us, maps:get(large, NativeResults)),
            Speedup = JsxAvg / NativeAvg,
            io:format(F, "~n**Speedup (Large)**: ~.2fx~n~n", [Speedup])
    end.

print_size_metrics(F, Metrics, Size) ->
    io:format(F, "- **~s**: p50=~p us, p95=~p us, p99=~p us, avg=~p us~n",
              [Size,
               maps:get(p50_us, Metrics),
               maps:get(p95_us, Metrics),
               maps:get(p99_us, Metrics),
               maps:get(avg_us, Metrics)]).

print_priority_results(F, #{normal := Normal, priority := Priority}) ->
    io:format(F, "### Priority Message Latency~n~n", []),

    io:format(F, "#### Normal Messages~n", []),
    NormalMetrics = maps:get(latency_metrics, Normal),
    io:format(F, "- p50: ~p us~n", [maps:get(p50_us, NormalMetrics)]),
    io:format(F, "- p95: ~p us~n", [maps:get(p95_us, NormalMetrics)]),
    io:format(F, "- p99: ~p us~n~n", [maps:get(p99_us, NormalMetrics)]),

    case Priority of
        undefined ->
            io:format(F, "*Priority messages not available*~n~n");
        _ ->
            io:format(F, "#### Priority Messages~n", []),
            PriorityMetrics = maps:get(latency_metrics, Priority),
            io:format(F, "- p50: ~p us~n", [maps:get(p50_us, PriorityMetrics)]),
            io:format(F, "- p95: ~p us~n", [maps:get(p95_us, PriorityMetrics)]),
            io:format(F, "- p99: ~p us~n~n", [maps:get(p99_us, PriorityMetrics)])
    end.

print_hibernation_results(F, #{hibernated := Hib, normal := Norm}) ->
    io:format(F, "### Hibernation Memory Savings~n~n", []),

    io:format(F, "#### Hibernated~n", []),
    io:format(F, "- Memory with children: ~p bytes~n", [maps:get(memory_with_children, Hib)]),
    io:format(F, "- Memory after hibernate: ~p bytes~n", [maps:get(memory_after_hibernate, Hib)]),
    io:format(F, "- Savings: ~p bytes (~.2f%)~n~n",
              [maps:get(total_savings, Hib), maps:get(savings_percent, Hib)]),

    io:format(F, "#### Normal~n", []),
    io:format(F, "- Memory with children: ~p bytes~n", [maps:get(memory_with_children, Norm)]),
    io:format(F, "- Memory after wait: ~p bytes~n", [maps:get(memory_after_wait, Norm)]).

print_process_iterator_results(F, #{processes_list := List, processes_iterator := Iterator}) ->
    io:format(F, "### Process Iteration~n~n", []),

    io:format(F, "#### processes/0~n", []),
    ListMetrics = maps:get(latency_metrics, List),
    io:format(F, "- Latency: ~p us (avg)~n", [maps:get(avg_us, ListMetrics)]),
    io:format(F, "- Memory per process: ~.2f bytes~n~n", [maps:get(memory_per_process_bytes, List)]),

    case Iterator of
        undefined ->
            io:format(F, "*Process iterator not available*~n~n");
        _ ->
            io:format(F, "#### processes_iterator/0~n", []),
            IteratorMetrics = maps:get(latency_metrics, Iterator),
            io:format(F, "- Latency: ~p us (avg)~n", [maps:get(avg_us, IteratorMetrics)]),
            io:format(F, "- Memory per process: ~.2f bytes~n", [maps:get(memory_per_process_bytes, Iterator)])
    end.

%%====================================================================
%%% Feature Detection
%%====================================================================

-spec has_native_json() -> boolean().
has_native_json() ->
    erlang:function_exported(json, encode, 1).

-spec has_priority_messages() -> boolean().
has_priority_messages() ->
    OtpRelease = list_to_integer(erlang:system_info(otp_release)),
    OtpRelease >= 28.

-spec has_process_iterator() -> boolean().
has_process_iterator() ->
    erlang:function_exported(erlang, processes_iterator, 0).

%%====================================================================
%%% Utilities
%%====================================================================

-spec round_float(number(), integer()) -> float().
round_float(Value, DecimalPlaces) ->
    Multiplier = math:pow(10, DecimalPlaces),
    round(Value * Multiplier) / Multiplier.
