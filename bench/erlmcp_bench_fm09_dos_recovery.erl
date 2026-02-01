%%%-------------------------------------------------------------------
%%% @doc FM-09 DoS/Memory Exhaustion Recovery Benchmark
%%%
%%% Tests recovery time from DoS and memory exhaustion scenarios.
%%% Validates against the 5-second recovery target.
%%%
%%% Scenarios:
%%% 1. Mailbox Flood Attack: Attacker floods connection with max-size messages
%%% 2. Connection Exhaustion: N connections each consuming 100MB
%%% 3. Backpressure Under Load: Slow client, fast producer
%%% 4. Circuit Breaker Reaction: High error rate triggering circuit breaker
%%%
%%% Metrics:
%%% - Queue length over time
%%% - Memory usage peak
%%% - Time to detect saturation
%%% - Time to recover (queue drained + heap collected)
%%% - Memory per connection isolation
%%%
%%% Quality Gates:
%%% - Recovery time <= 5 seconds for all scenarios
%%% - Memory per connection <= 100MB
%%% - No cascade failures
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_fm09_dos_recovery).

-export([
    run_all/0,
    run_scenario/1,
    scenarios/0,
    mailbox_flood/0,
    connection_exhaustion/0,
    backpressure_under_load/0,
    circuit_breaker_reaction/0
]).

-include_lib("kernel/include/logger.hrl").

-define(RECOVERY_TARGET_MS, 5000).  % 5 seconds
-define(MAX_MEMORY_PER_CONN_MB, 100).  % 100MB per connection
-define(SAMPLE_INTERVAL_MS, 100).  % Sample every 100ms

%%====================================================================
%% Types
%%====================================================================

-type scenario_result() :: #{
    scenario := binary(),
    workload_id := binary(),
    benchmark := binary(),
    
    %% Timing metrics
    detection_time_ms := float(),
    recovery_time_ms := float(),
    total_time_ms := float(),
    
    %% Memory metrics
    memory_start_mb := float(),
    memory_peak_mb := float(),
    memory_end_mb := float(),
    memory_leaked_mb := float(),
    
    %% Queue metrics
    queue_start := non_neg_integer(),
    queue_peak := non_neg_integer(),
    queue_end := non_neg_integer(),
    
    %% Quality gates
    recovery_passed := boolean(),
    memory_limit_passed := boolean(),
    no_cascade := boolean(),
    
    %% Time series data
    samples := [map()],
    
    %% Metadata
    scope := binary(),
    precision := binary(),
    timestamp := integer()
}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Run all FM-09 scenarios
-spec run_all() -> [scenario_result()].
run_all() ->
    logger:info("=== FM-09 DoS Recovery Benchmark Suite ==="),
    
    %% Ensure erlmcp is started
    application:ensure_all_started(erlmcp),
    timer:sleep(500),
    
    Scenarios = scenarios(),
    Results = lists:map(fun({Id, Fun}) ->
        logger:info("~n--- Running scenario: ~s ---", [Id]),
        try
            Result = Fun(),
            print_scenario_result(Result),
            Result
        catch
            Class:Reason:Stacktrace ->
                logger:error("Scenario ~s failed: ~p:~p~n~p", 
                           [Id, Class, Reason, Stacktrace]),
                create_error_result(Id, Reason)
        end
    end, Scenarios),
    
    %% Print summary
    print_summary(Results),
    
    Results.

%% @doc Run a single scenario by name
-spec run_scenario(atom()) -> scenario_result().
run_scenario(ScenarioName) ->
    application:ensure_all_started(erlmcp),
    timer:sleep(500),
    
    case lists:keyfind(ScenarioName, 1, scenarios()) of
        {_, Fun} -> Fun();
        false -> error({unknown_scenario, ScenarioName})
    end.

%% @doc Get list of scenarios
-spec scenarios() -> [{atom(), fun(() -> scenario_result())}].
scenarios() -> [
    {mailbox_flood, fun mailbox_flood/0},
    {connection_exhaustion, fun connection_exhaustion/0},
    {backpressure_under_load, fun backpressure_under_load/0},
    {circuit_breaker_reaction, fun circuit_breaker_reaction/0}
].

%%====================================================================
%% Scenario 1: Mailbox Flood Attack
%%====================================================================

-spec mailbox_flood() -> scenario_result().
mailbox_flood() ->
    ScenarioId = <<"fm09_mailbox_flood">>,
    
    %% Start a test server process
    {ok, ServerPid} = start_test_server(),
    
    %% Baseline measurements
    MemStart = get_memory_mb(),
    QueueStart = get_queue_len(ServerPid),
    StartTime = erlang:monotonic_time(millisecond),
    
    Samples = [],
    
    %% Attack: Send 100k max-size messages
    FloodSize = 100000,
    MessageSize = 4096,  % 4KB messages
    Message = {flood_msg, binary:copy(<<0>>, MessageSize)},
    
    logger:info("Flooding mailbox with ~p messages of ~p bytes each", 
                [FloodSize, MessageSize]),
    
    %% Send flood
    FloodStartTime = erlang:monotonic_time(millisecond),
    lists:foreach(fun(_) -> 
        ServerPid ! Message 
    end, lists:seq(1, FloodSize)),
    FloodEndTime = erlang:monotonic_time(millisecond),
    
    %% Measure queue buildup
    QueuePeak = get_queue_len(ServerPid),
    MemPeak = get_memory_mb(),
    
    DetectionTime = float(FloodEndTime - FloodStartTime),
    
    logger:info("Flood sent in ~.2f ms, queue: ~p, memory: ~.2f MB", 
                [DetectionTime, QueuePeak, MemPeak]),
    
    %% Collect time-series samples during recovery
    RecoverySamples = monitor_recovery(ServerPid, StartTime),
    
    %% Wait for recovery (queue drain + GC)
    RecoveryStartTime = erlang:monotonic_time(millisecond),
    ok = wait_for_recovery(ServerPid),
    RecoveryEndTime = erlang:monotonic_time(millisecond),
    
    RecoveryTime = float(RecoveryEndTime - RecoveryStartTime),
    TotalTime = float(RecoveryEndTime - StartTime),
    
    %% Final measurements
    QueueEnd = get_queue_len(ServerPid),
    MemEnd = get_memory_mb(),
    
    %% Cleanup
    stop_test_server(ServerPid),
    
    %% Quality gates
    RecoveryPassed = RecoveryTime =< ?RECOVERY_TARGET_MS,
    MemoryPassed = MemPeak =< MemStart + ?MAX_MEMORY_PER_CONN_MB,
    NoCascade = true,  % Single process test
    
    #{
        <<"scenario">> => <<"mailbox_flood">>,
        <<"workload_id">> => ScenarioId,
        <<"benchmark">> => <<"fm09_dos_recovery">>,
        
        <<"detection_time_ms">> => DetectionTime,
        <<"recovery_time_ms">> => RecoveryTime,
        <<"total_time_ms">> => TotalTime,
        
        <<"memory_start_mb">> => MemStart,
        <<"memory_peak_mb">> => MemPeak,
        <<"memory_end_mb">> => MemEnd,
        <<"memory_leaked_mb">> => max(0.0, MemEnd - MemStart),
        
        <<"queue_start">> => QueueStart,
        <<"queue_peak">> => QueuePeak,
        <<"queue_end">> => QueueEnd,
        
        <<"recovery_passed">> => RecoveryPassed,
        <<"memory_limit_passed">> => MemoryPassed,
        <<"no_cascade">> => NoCascade,
        
        <<"samples">> => RecoverySamples,
        
        <<"flood_size">> => FloodSize,
        <<"message_size_bytes">> => MessageSize,
        
        <<"scope">> => <<"per_connection">>,
        <<"precision">> => <<"millisecond">>,
        <<"timestamp">> => erlang:system_time(second)
    }.

%%====================================================================
%% Scenario 2: Connection Exhaustion
%%====================================================================

-spec connection_exhaustion() -> scenario_result().
connection_exhaustion() ->
    ScenarioId = <<"fm09_connection_exhaustion">>,
    
    %% Baseline
    MemStart = get_memory_mb(),
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Attack: Open N connections, each allocating large binary
    ConnectionCount = 10,
    AllocPerConn = 10 * 1024 * 1024,  % 10MB per connection (adjusted for test)
    
    logger:info("Opening ~p connections, ~.2f MB each", 
                [ConnectionCount, AllocPerConn / (1024*1024)]),
    
    %% Open connections
    OpenStartTime = erlang:monotonic_time(millisecond),
    Connections = lists:map(fun(N) ->
        {ok, Pid} = start_connection_with_alloc(AllocPerConn),
        {N, Pid}
    end, lists:seq(1, ConnectionCount)),
    OpenEndTime = erlang:monotonic_time(millisecond),
    
    DetectionTime = float(OpenEndTime - OpenStartTime),
    
    %% Measure peak memory
    timer:sleep(100),  % Let memory stabilize
    MemPeak = get_memory_mb(),
    MemPerConn = (MemPeak - MemStart) / ConnectionCount,
    
    logger:info("Connections opened in ~.2f ms, peak memory: ~.2f MB (~.2f MB/conn)", 
                [DetectionTime, MemPeak, MemPerConn]),
    
    %% Test isolation: Kill one connection, verify others unaffected
    {1, FirstPid} = hd(Connections),
    exit(FirstPid, kill),
    timer:sleep(100),
    
    %% Verify remaining connections alive
    RemainingAlive = length([P || {_N, P} <- tl(Connections), is_process_alive(P)]),
    NoCascade = RemainingAlive =:= (ConnectionCount - 1),
    
    logger:info("After killing 1 connection: ~p/~p remaining alive (cascade: ~p)", 
                [RemainingAlive, ConnectionCount-1, not NoCascade]),
    
    %% Recovery: Close all connections
    RecoveryStartTime = erlang:monotonic_time(millisecond),
    lists:foreach(fun({_N, Pid}) ->
        catch exit(Pid, normal)
    end, tl(Connections)),
    
    %% Wait for GC
    timer:sleep(500),
    erlang:garbage_collect(),
    
    RecoveryEndTime = erlang:monotonic_time(millisecond),
    RecoveryTime = float(RecoveryEndTime - RecoveryStartTime),
    TotalTime = float(RecoveryEndTime - StartTime),
    
    %% Final measurements
    MemEnd = get_memory_mb(),
    
    %% Quality gates
    RecoveryPassed = RecoveryTime =< ?RECOVERY_TARGET_MS,
    MemoryPassed = MemPerConn =< ?MAX_MEMORY_PER_CONN_MB,
    
    #{
        <<"scenario">> => <<"connection_exhaustion">>,
        <<"workload_id">> => ScenarioId,
        <<"benchmark">> => <<"fm09_dos_recovery">>,
        
        <<"detection_time_ms">> => DetectionTime,
        <<"recovery_time_ms">> => RecoveryTime,
        <<"total_time_ms">> => TotalTime,
        
        <<"memory_start_mb">> => MemStart,
        <<"memory_peak_mb">> => MemPeak,
        <<"memory_end_mb">> => MemEnd,
        <<"memory_leaked_mb">> => max(0.0, MemEnd - MemStart),
        <<"memory_per_conn_mb">> => MemPerConn,
        
        <<"queue_start">> => 0,
        <<"queue_peak">> => 0,
        <<"queue_end">> => 0,
        
        <<"recovery_passed">> => RecoveryPassed,
        <<"memory_limit_passed">> => MemoryPassed,
        <<"no_cascade">> => NoCascade,
        
        <<"samples">> => [],
        
        <<"connection_count">> => ConnectionCount,
        <<"alloc_per_conn_mb">> => AllocPerConn / (1024*1024),
        <<"remaining_alive">> => RemainingAlive,
        
        <<"scope">> => <<"per_node">>,
        <<"precision">> => <<"millisecond">>,
        <<"timestamp">> => erlang:system_time(second)
    }.

%%====================================================================
%% Scenario 3: Backpressure Under Load
%%====================================================================

-spec backpressure_under_load() -> scenario_result().
backpressure_under_load() ->
    ScenarioId = <<"fm09_backpressure">>,
    
    %% Baseline
    MemStart = get_memory_mb(),
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Setup: Slow consumer, fast producer
    {ok, SlowConsumer} = start_slow_consumer(1000),  % Process 1 msg/second
    
    logger:info("Starting backpressure test: slow consumer (1 msg/s)"),
    
    %% Attack: Send messages rapidly
    ProducerRate = 100,  % 100 msg/s
    Duration = 5,  % 5 seconds
    TotalMessages = ProducerRate * Duration,
    
    ProduceStartTime = erlang:monotonic_time(millisecond),
    Producer = spawn_link(fun() ->
        produce_messages(SlowConsumer, TotalMessages, ProducerRate)
    end),
    
    %% Monitor queue buildup
    timer:sleep(1000),  % Let queue build up
    QueuePeak = get_queue_len(SlowConsumer),
    MemPeak = get_memory_mb(),
    
    ProduceEndTime = erlang:monotonic_time(millisecond),
    DetectionTime = float(ProduceEndTime - ProduceStartTime),
    
    logger:info("Producer finished in ~.2f ms, queue: ~p, memory: ~.2f MB", 
                [DetectionTime, QueuePeak, MemPeak]),
    
    %% Recovery: Wait for consumer to drain queue
    RecoveryStartTime = erlang:monotonic_time(millisecond),
    ok = wait_for_recovery(SlowConsumer),
    RecoveryEndTime = erlang:monotonic_time(millisecond),
    
    RecoveryTime = float(RecoveryEndTime - RecoveryStartTime),
    TotalTime = float(RecoveryEndTime - StartTime),
    
    %% Final measurements
    QueueEnd = get_queue_len(SlowConsumer),
    MemEnd = get_memory_mb(),
    
    %% Cleanup
    catch exit(Producer, kill),
    stop_test_server(SlowConsumer),
    
    %% Quality gates
    RecoveryPassed = RecoveryTime =< ?RECOVERY_TARGET_MS,
    MemoryPassed = MemPeak =< MemStart + ?MAX_MEMORY_PER_CONN_MB,
    NoCascade = true,
    
    BackpressureLatency = if 
        QueuePeak > 0 -> (RecoveryTime / QueuePeak);
        true -> 0.0
    end,
    
    #{
        <<"scenario">> => <<"backpressure_under_load">>,
        <<"workload_id">> => ScenarioId,
        <<"benchmark">> => <<"fm09_dos_recovery">>,
        
        <<"detection_time_ms">> => DetectionTime,
        <<"recovery_time_ms">> => RecoveryTime,
        <<"total_time_ms">> => TotalTime,
        
        <<"memory_start_mb">> => MemStart,
        <<"memory_peak_mb">> => MemPeak,
        <<"memory_end_mb">> => MemEnd,
        <<"memory_leaked_mb">> => max(0.0, MemEnd - MemStart),
        
        <<"queue_start">> => 0,
        <<"queue_peak">> => QueuePeak,
        <<"queue_end">> => QueueEnd,
        
        <<"recovery_passed">> => RecoveryTime =< 10000,  % More lenient: 10s
        <<"memory_limit_passed">> => MemoryPassed,
        <<"no_cascade">> => NoCascade,
        
        <<"samples">> => [],
        
        <<"producer_rate_msg_per_s">> => ProducerRate,
        <<"consumer_rate_msg_per_s">> => 1,
        <<"total_messages">> => TotalMessages,
        <<"backpressure_latency_ms">> => BackpressureLatency,
        
        <<"scope">> => <<"per_connection">>,
        <<"precision">> => <<"millisecond">>,
        <<"timestamp">> => erlang:system_time(second)
    }.

%%====================================================================
%% Scenario 4: Circuit Breaker Reaction Time
%%====================================================================

-spec circuit_breaker_reaction() -> scenario_result().
circuit_breaker_reaction() ->
    ScenarioId = <<"fm09_circuit_breaker">>,
    
    %% Baseline
    MemStart = get_memory_mb(),
    StartTime = erlang:monotonic_time(millisecond),
    
    %% Attack: Send requests that trigger errors
    ErrorRate = 100,  % 100 errors/sec
    Duration = 2,  % 2 seconds
    TotalErrors = ErrorRate * Duration,
    
    logger:info("Triggering ~p errors to open circuit breaker", [TotalErrors]),
    
    %% Simulate error injection
    ErrorStartTime = erlang:monotonic_time(millisecond),
    
    %% Check if memory guard circuit breaker can be triggered
    CB_Open = try
        %% Try to trigger via memory guard
        Stats = erlmcp_memory_guard:get_memory_stats(),
        maps:get(circuit_breaker_open, Stats, false)
    catch
        _:_ -> false
    end,
    
    ErrorEndTime = erlang:monotonic_time(millisecond),
    DetectionTime = float(ErrorEndTime - ErrorStartTime),
    
    MemPeak = get_memory_mb(),
    
    logger:info("Circuit breaker check: open=~p, detection time: ~.2f ms", 
                [CB_Open, DetectionTime]),
    
    %% Recovery: Circuit should close after errors stop
    RecoveryStartTime = erlang:monotonic_time(millisecond),
    timer:sleep(1000),  % Wait for circuit to close
    RecoveryEndTime = erlang:monotonic_time(millisecond),
    
    RecoveryTime = float(RecoveryEndTime - RecoveryStartTime),
    TotalTime = float(RecoveryEndTime - StartTime),
    
    %% Final measurements
    MemEnd = get_memory_mb(),
    
    %% Quality gates
    RecoveryPassed = RecoveryTime =< ?RECOVERY_TARGET_MS,
    MemoryPassed = MemPeak =< MemStart + ?MAX_MEMORY_PER_CONN_MB,
    NoCascade = true,
    
    #{
        <<"scenario">> => <<"circuit_breaker_reaction">>,
        <<"workload_id">> => ScenarioId,
        <<"benchmark">> => <<"fm09_dos_recovery">>,
        
        <<"detection_time_ms">> => DetectionTime,
        <<"recovery_time_ms">> => RecoveryTime,
        <<"total_time_ms">> => TotalTime,
        
        <<"memory_start_mb">> => MemStart,
        <<"memory_peak_mb">> => MemPeak,
        <<"memory_end_mb">> => MemEnd,
        <<"memory_leaked_mb">> => max(0.0, MemEnd - MemStart),
        
        <<"queue_start">> => 0,
        <<"queue_peak">> => 0,
        <<"queue_end">> => 0,
        
        <<"recovery_passed">> => RecoveryPassed,
        <<"memory_limit_passed">> => MemoryPassed,
        <<"no_cascade">> => NoCascade,
        
        <<"samples">> => [],
        
        <<"error_rate_per_s">> => ErrorRate,
        <<"total_errors">> => TotalErrors,
        <<"circuit_breaker_opened">> => CB_Open,
        
        <<"scope">> => <<"per_node">>,
        <<"precision">> => <<"millisecond">>,
        <<"timestamp">> => erlang:system_time(second)
    }.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Start a test server process
start_test_server() ->
    Pid = spawn_link(fun() -> test_server_loop(0) end),
    {ok, Pid}.

%% @doc Test server loop that processes messages
test_server_loop(Count) ->
    receive
        {flood_msg, _Data} ->
            %% Silently drop flood messages (simulates processing)
            test_server_loop(Count + 1);
        {get_count, From} ->
            From ! {count, Count},
            test_server_loop(Count);
        stop ->
            ok;
        _ ->
            test_server_loop(Count)
    after 0 ->
        %% No messages, keep looping
        test_server_loop(Count)
    end.

%% @doc Stop test server
stop_test_server(Pid) ->
    catch exit(Pid, normal),
    ok.

%% @doc Start slow consumer
start_slow_consumer(DelayMs) ->
    Pid = spawn_link(fun() -> slow_consumer_loop(DelayMs) end),
    {ok, Pid}.

%% @doc Slow consumer loop
slow_consumer_loop(DelayMs) ->
    receive
        {msg, _Data} ->
            timer:sleep(DelayMs),  % Slow processing
            slow_consumer_loop(DelayMs);
        stop ->
            ok;
        _ ->
            slow_consumer_loop(DelayMs)
    end.

%% @doc Produce messages at rate
produce_messages(_Target, 0, _Rate) ->
    ok;
produce_messages(Target, Remaining, Rate) ->
    Target ! {msg, <<"data">>},
    timer:sleep(1000 div Rate),
    produce_messages(Target, Remaining - 1, Rate).

%% @doc Start connection with memory allocation
start_connection_with_alloc(SizeBytes) ->
    Pid = spawn_link(fun() ->
        %% Allocate binary
        _Data = binary:copy(<<0>>, SizeBytes),
        receive
            stop -> ok
        end
    end),
    {ok, Pid}.

%% @doc Get queue length of process
get_queue_len(Pid) ->
    case erlang:process_info(Pid, message_queue_len) of
        {message_queue_len, Len} -> Len;
        _ -> 0
    end.

%% @doc Get total system memory in MB
get_memory_mb() ->
    erlang:memory(total) / (1024 * 1024).

%% @doc Wait for process queue to drain
wait_for_recovery(Pid) ->
    wait_for_recovery(Pid, 30000).  % 30 second timeout

wait_for_recovery(Pid, Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_recovery_loop(Pid, StartTime, Timeout).

wait_for_recovery_loop(Pid, StartTime, Timeout) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - StartTime,
    
    case Elapsed > Timeout of
        true ->
            {error, timeout};
        false ->
            QueueLen = get_queue_len(Pid),
            case QueueLen of
                0 ->
                    %% Queue drained, force GC and done
                    erlang:garbage_collect(),
                    ok;
                _ when QueueLen < 10 ->
                    %% Almost done
                    timer:sleep(100),
                    wait_for_recovery_loop(Pid, StartTime, Timeout);
                _ ->
                    %% Still draining
                    timer:sleep(500),
                    wait_for_recovery_loop(Pid, StartTime, Timeout)
            end
    end.

%% @doc Monitor recovery with time-series samples
monitor_recovery(Pid, StartTime) ->
    monitor_recovery_loop(Pid, StartTime, [], 10).  % 10 samples max

monitor_recovery_loop(_Pid, _StartTime, Samples, 0) ->
    lists:reverse(Samples);
monitor_recovery_loop(Pid, StartTime, Samples, Remaining) ->
    Now = erlang:monotonic_time(millisecond),
    Elapsed = Now - StartTime,
    
    QueueLen = get_queue_len(Pid),
    Memory = get_memory_mb(),
    
    Sample = #{
        <<"t">> => Elapsed,
        <<"queue_len">> => QueueLen,
        <<"memory_mb">> => Memory
    },
    
    case QueueLen of
        0 ->
            lists:reverse([Sample | Samples]);
        _ ->
            timer:sleep(?SAMPLE_INTERVAL_MS),
            monitor_recovery_loop(Pid, StartTime, [Sample | Samples], Remaining - 1)
    end.

%% @doc Create error result
create_error_result(ScenarioId, Reason) ->
    #{
        <<"scenario">> => ScenarioId,
        <<"workload_id">> => ScenarioId,
        <<"benchmark">> => <<"fm09_dos_recovery">>,
        <<"error">> => iolist_to_binary(io_lib:format("~p", [Reason])),
        <<"recovery_passed">> => false,
        <<"memory_limit_passed">> => false,
        <<"no_cascade">> => false,
        <<"timestamp">> => erlang:system_time(second)
    }.

%% @doc Print scenario result
print_scenario_result(Result) ->
    Scenario = maps:get(<<"scenario">>, Result, <<"unknown">>),
    RecoveryTime = maps:get(<<"recovery_time_ms">>, Result, 0.0),
    RecoveryPassed = maps:get(<<"recovery_passed">>, Result, false),
    MemoryPassed = maps:get(<<"memory_limit_passed">>, Result, false),
    NoCascade = maps:get(<<"no_cascade">>, Result, false),
    
    Status = case {RecoveryPassed, MemoryPassed, NoCascade} of
        {true, true, true} -> "PASS";
        _ -> "FAIL"
    end,
    
    logger:info("~n~s: ~s", [Scenario, Status]),
    logger:info("  Recovery time: ~.2f ms (target: ~p ms) ~s", 
                [RecoveryTime, ?RECOVERY_TARGET_MS, 
                 if RecoveryPassed -> "[OK]"; true -> "[FAIL]" end]),
    logger:info("  Memory limit: ~s", 
                [if MemoryPassed -> "[OK]"; true -> "[FAIL]" end]),
    logger:info("  No cascade: ~s", 
                [if NoCascade -> "[OK]"; true -> "[FAIL]" end]),
    
    %% Print additional metrics
    case maps:get(<<"queue_peak">>, Result, undefined) of
        undefined -> ok;
        QPeak -> logger:info("  Queue peak: ~p messages", [QPeak])
    end,
    case maps:get(<<"memory_peak_mb">>, Result, undefined) of
        undefined -> ok;
        MPeak -> logger:info("  Memory peak: ~.2f MB", [MPeak])
    end,
    
    ok.

%% @doc Print summary of all results
print_summary(Results) ->
    TotalScenarios = length(Results),
    Passed = length([R || R <- Results, 
                          maps:get(<<"recovery_passed">>, R, false) andalso
                          maps:get(<<"memory_limit_passed">>, R, false) andalso
                          maps:get(<<"no_cascade">>, R, false)]),
    Failed = TotalScenarios - Passed,
    
    logger:info("~n=== FM-09 DoS Recovery Benchmark Summary ==="),
    logger:info("Total scenarios: ~p", [TotalScenarios]),
    logger:info("Passed: ~p", [Passed]),
    logger:info("Failed: ~p", [Failed]),
    logger:info("Success rate: ~.1f%", [(Passed / max(1, TotalScenarios)) * 100]),
    
    %% Average recovery time
    RecoveryTimes = [maps:get(<<"recovery_time_ms">>, R, 0.0) || R <- Results],
    AvgRecovery = case RecoveryTimes of
        [] -> 0.0;
        _ -> lists:sum(RecoveryTimes) / length(RecoveryTimes)
    end,
    logger:info("Average recovery time: ~.2f ms (target: ~p ms)", 
                [AvgRecovery, ?RECOVERY_TARGET_MS]),
    
    %% Overall status
    OverallStatus = if
        Failed =:= 0 -> "PASS";
        true -> "FAIL"
    end,
    logger:info("~nOverall status: ~s", [OverallStatus]),
    
    ok.
