%%%-------------------------------------------------------------------
%%% @doc erlmcp_bench_resource_leak - DESTRUCTIVE Resource Leak Stress Test #13
%%%
%%% CRITICAL: This is a DESTRUCTIVE test that intentionally leaks resources.
%%% DO NOT run on production systems.
%%%
%%% Objective:
%%% - Connect 100,000 clients and NEVER close them
%%% - Find leak rates (fds/sec, MB/sec, processes/sec)
%%% - Identify which resource exhausts first
%%% - Measure crash point and time to failure
%%%
%%% This is NOT about connection limits - it's about RESOURCE LEAKS.
%%% We deliberately do NOT cleanup to find memory leaks, fd leaks, etc.
%%%
%%% Test Protocol:
%%% 1. Spawn MCP server on port 10013 with TCP transport
%%% 2. Leak pattern:
%%%    - Connect 1,000 clients/second
%%%    - Each client: send 10 messages
%%%    - NEVER close connections (no disconnect)
%%%    - Never release resources
%%%    - Accumulate leaks
%%% 3. Monitor leaks:
%%%    - File descriptors: erlang:system_info(port_count)
%%%    - Memory usage: erlang:memory(total)
%%%    - Process count: erlang:system_info(process_count)
%%%    - ETS tables: count and size
%%%    - Socket buffers
%%% 4. Leak detection:
%%%    - Measure leak rate (fds/sec, MB/sec)
%%%    - Plot growth curve (linear/exponential)
%%%    - Identify worst leaking resource
%%% 5. Continue until:
%%%    - File descriptor exhaustion
%%%    - Out of memory
%%%    - Server crash
%%%    - Or 100,000 leaked connections
%%% 6. Test leak types:
%%%    - TCP sockets (no close)
%%%    - ETS tables (no delete)
%%%    - Processes (no exit)
%%%    - Binaries (keep references)
%%%    - Mailboxes (fill and abandon)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_resource_leak).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    run_all/0,
    run_tcp_socket_leak/0,
    run_ets_table_leak/0,
    run_process_leak/0,
    run_binary_leak/0,
    run_mailbox_leak/0,
    run_full_leak_test/0,
    generate_report/1
]).

%% Records
-record(leak_config, {
    server_port = 10013 :: inet:port_number(),
    max_connections = 100000 :: pos_integer(),
    connect_rate = 1000 :: pos_integer(),  % clients per second
    messages_per_client = 10 :: pos_integer(),
    cleanup = false :: boolean(),  % CRITICAL: false = INTENTIONAL LEAK
    close_connections = false :: boolean(),  % CRITICAL: false = NEVER CLOSE
    report_interval = 1000 :: pos_integer(),
    timeout_ms = 300000 :: pos_integer()  % 5 minutes max
}).

-record(leak_result, {
    test_id :: binary(),
    test_type :: binary(),
    max_connections :: pos_integer(),
    connect_rate :: pos_integer(),
    messages_per_client :: pos_integer(),
    cleanup_enabled :: boolean(),
    leak_progress :: [map()],
    fd_leak_rate :: float() | undefined,
    memory_leak_rate :: float() | undefined,
    process_leak_rate :: float() | undefined,
    ets_leak_rate :: float() | undefined,
    connection_count_at_crash :: non_neg_integer(),
    time_to_crash_s :: float() | undefined,
    limiting_resource :: binary(),
    crash_reason :: term(),
    sockets_leaked :: non_neg_integer(),
    ets_entries_leaked :: non_neg_integer(),
    processes_leaked :: non_neg_integer(),
    memory_leaked_mb :: float(),
    attempted_cleanup :: boolean(),
    server_recovered :: boolean(),
    resources_freed :: non_neg_integer(),
    os_type :: {atom(), atom()},
    erlang_version :: string(),
    total_memory_mib :: float(),
    fd_limit :: non_neg_integer(),
    scope :: binary(),
    timestamp :: integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Run all resource leak tests
-spec run_all() -> {ok, [map()]} | {error, term()}.
run_all() ->
    logger:warning("=== STARTING DESTRUCTIVE RESOURCE LEAK TESTS ==="),
    logger:warning("This test will intentionally leak resources"),
    logger:warning("DO NOT run on production systems"),
    logger:warning("Expected: Memory exhaustion, FD exhaustion, or crash"),
    
    Results = [
        run_tcp_socket_leak(),
        run_ets_table_leak(),
        run_process_leak(),
        run_binary_leak(),
        run_mailbox_leak(),
        run_full_leak_test()
    ],
    
    logger:info("=== ALL RESOURCE LEAK TESTS COMPLETE ==="),
    {ok, Results}.

%% @doc TCP Socket Leak Test - Open connections, never close
-spec run_tcp_socket_leak() -> map().
run_tcp_socket_leak() ->
    logger:warning("=== TCP SOCKET LEAK TEST ==="),
    logger:warning("Opening TCP connections and NEVER closing them"),
    logger:warning("Expected leak: File descriptors, socket buffers, port memory"),
    
    Config = #leak_config{
        server_port = 10013,
        max_connections = 100000,
        connect_rate = 1000,
        messages_per_client = 10,
        cleanup = false,
        close_connections = false,
        report_interval = 1000,
        timeout_ms = 300000
    },
    
    run_test(<<"tcp_socket_leak">>, Config).

%% @doc ETS Table Leak Test - Create tables, never delete
-spec run_ets_table_leak() -> map().
run_ets_table_leak() ->
    logger:warning("=== ETS TABLE LEAK TEST ==="),
    logger:warning("Creating ETS tables and NEVER deleting them"),
    logger:warning("Expected leak: ETS table memory, entries"),
    
    Config = #leak_config{
        server_port = 10013,
        max_connections = 10000,
        connect_rate = 500,
        messages_per_client = 10,
        cleanup = false,
        close_connections = false,
        report_interval = 500,
        timeout_ms = 180000
    },
    
    run_ets_test(<<"ets_table_leak">>, Config).

%% @doc Process Leak Test - Spawn processes, never exit
-spec run_process_leak() -> map().
run_process_leak() ->
    logger:warning("=== PROCESS LEAK TEST ==="),
    logger:warning("Spawning processes and NEVER exiting them"),
    logger:warning("Expected leak: Process memory, process table"),
    
    Config = #leak_config{
        server_port = 10013,
        max_connections = 50000,
        connect_rate = 1000,
        messages_per_client = 10,
        cleanup = false,
        close_connections = false,
        report_interval = 1000,
        timeout_ms = 240000
    },
    
    run_process_test(<<"process_leak">>, Config).

%% @doc Binary Leak Test - Accumulate binaries, never GC
-spec run_binary_leak() -> map().
run_binary_leak() ->
    logger:warning("=== BINARY LEAK TEST ==="),
    logger:warning("Accumulating binary references and never releasing"),
    logger:warning("Expected leak: Binary heap (refc binaries)"),
    
    Config = #leak_config{
        server_port = 10013,
        max_connections = 20000,
        connect_rate = 500,
        messages_per_client = 50,
        cleanup = false,
        close_connections = false,
        report_interval = 500,
        timeout_ms = 180000
    },
    
    run_binary_test(<<"binary_leak">>, Config).

%% @doc Mailbox Leak Test - Fill mailboxes, never drain
-spec run_mailbox_leak() -> map().
run_mailbox_leak() ->
    logger:warning("=== MAILBOX LEAK TEST ==="),
    logger:warning("Filling process mailboxes and never draining"),
    logger:warning("Expected leak: Message queue memory"),
    
    Config = #leak_config{
        server_port = 10013,
        max_connections = 10000,
        connect_rate = 200,
        messages_per_client = 1000,
        cleanup = false,
        close_connections = false,
        report_interval = 500,
        timeout_ms = 180000
    },
    
    run_mailbox_test(<<"mailbox_leak">>, Config).

%% @doc Full Leak Test - All leak types combined
-spec run_full_leak_test() -> map().
run_full_leak_test() ->
    logger:warning("=== FULL RESOURCE LEAK TEST ==="),
    logger:warning("Combining ALL leak types to find system limit"),
    logger:warning("Expected: System crash or resource exhaustion"),
    
    Config = #leak_config{
        server_port = 10013,
        max_connections = 100000,
        connect_rate = 1000,
        messages_per_client = 50,
        cleanup = false,
        close_connections = false,
        report_interval = 1000,
        timeout_ms = 360000
    },
    
    run_full_test(<<"full_resource_leak">>, Config).

%%====================================================================
%% Test Execution Functions
%%====================================================================

%% @doc Run TCP socket leak test
-spec run_test(binary(), #leak_config{}) -> map().
run_test(TestType, Config) ->
    TestId = <<TestType/binary, "_", (integer_to_binary(erlang:system_time(millisecond)))/binary>>,
    
    logger:info("Starting test: ~s", [TestId]),
    logger:info("Config: max_conns=~p, rate=~p/sec, cleanup=~p, close=~p",
        [Config#leak_config.max_connections,
         Config#leak_config.connect_rate,
         Config#leak_config.cleanup,
         Config#leak_config.close_connections]),
    
    {FdLimit, _} = get_fd_limit(),
    TotalMemory = erlang:memory(total) / (1024 * 1024),
    
    {ok, ServerPid} = start_test_server(Config#leak_config.server_port),
    timer:sleep(1000),
    
    InitialSnapshot = take_snapshot(0),
    
    TestStart = erlang:monotonic_time(millisecond),
    
    {FinalState, Snapshots} = leak_connections(Config, InitialSnapshot),
    
    TestEnd = erlang:monotonic_time(millisecond),
    
    LeakRates = calculate_leak_rates(InitialSnapshot, FinalState, Snapshots),
    
    {ConnCount, TimeToCrash, LimitingResource, CrashReason} = FinalState,
    
    {SocketsLeaked, EtsLeaked, ProcessesLeaked, MemoryLeaked} = get_resource_breakdown(),
    
    {AttemptedCleanup, ServerRecovered, ResourcesFreed} =
        attempt_recovery(ServerPid, Config#leak_config.server_port),
    
    Result = #leak_result{
        test_id = TestId,
        test_type = TestType,
        max_connections = Config#leak_config.max_connections,
        connect_rate = Config#leak_config.connect_rate,
        messages_per_client = Config#leak_config.messages_per_client,
        cleanup_enabled = Config#leak_config.cleanup,
        leak_progress = Snapshots,
        fd_leak_rate = maps_get(fd_rate, LeakRates, undefined),
        memory_leak_rate = maps_get(memory_rate, LeakRates, undefined),
        process_leak_rate = maps_get(process_rate, LeakRates, undefined),
        ets_leak_rate = maps_get(ets_rate, LeakRates, undefined),
        connection_count_at_crash = ConnCount,
        time_to_crash_s = TimeToCrash,
        limiting_resource = LimitingResource,
        crash_reason = CrashReason,
        sockets_leaked = SocketsLeaked,
        ets_entries_leaked = EtsLeaked,
        processes_leaked = ProcessesLeaked,
        memory_leaked_mb = MemoryLeaked,
        attempted_cleanup = AttemptedCleanup,
        server_recovered = ServerRecovered,
        resources_freed = ResourcesFreed,
        os_type = os:type(),
        erlang_version = erlang:system_info(otp_release) ++ " (" ++ erlang:system_info(version) ++ ")",
        total_memory_mib = TotalMemory,
        fd_limit = FdLimit,
        scope = <<"per_node">>,
        timestamp = erlang:system_time(second)
    },
    
    catch exit(ServerPid, kill),
    
    ResultMap = leak_result_to_map(Result),
    
    logger:info("Test complete: ~s", [TestId]),
    print_leak_summary(ResultMap),
    
    ResultMap.

%% @doc Run ETS table leak test
-spec run_ets_test(binary(), #leak_config{}) -> map().
run_ets_test(TestType, Config) ->
    logger:warning("ETS LEAK TEST: Creating ETS tables without deletion"),
    
    TestId = <<TestType/binary, "_", (integer_to_binary(erlang:system_time(millisecond)))/binary>>,
    
    InitialEts = length(ets:all()),
    InitialMemory = erlang:memory(total) / (1024 * 1024),
    
    Start = erlang:monotonic_time(millisecond),
    
    {FinalCount, TablesLeaked, TimeTaken, LimitReason} =
        leak_ets_tables(Config#leak_config.max_connections),
    
    End = erlang:monotonic_time(millisecond),
    
    FinalMemory = erlang:memory(total) / (1024 * 1024),
    MemoryLeaked = FinalMemory - InitialMemory,
    
    TimeSec = (End - Start) / 1000.0,
    EtsLeakRate = TablesLeaked / TimeSec,
    MemoryLeakRate = MemoryLeaked / TimeSec,
    
    Result = #{
        <<"workload_id">> => TestId,
        <<"benchmark">> => <<"resource_leak">>,
        <<"test_type">> => <<"ets_table_leak">>,
        <<"max_tables">> => Config#leak_config.max_connections,
        <<"tables_leaked">> => TablesLeaked,
        <<"initial_ets_count">> => InitialEts,
        <<"final_ets_count">> => FinalCount,
        <<"memory_leaked_mb">> => MemoryLeaked,
        <<"ets_leak_rate_tables_per_sec">> => EtsLeakRate,
        <<"memory_leak_rate_mb_per_sec">> => MemoryLeakRate,
        <<"time_to_limit_s">> => TimeSec,
        <<"limiting_resource">> => LimitReason,
        <<"scope">> => <<"per_node">>,
        <<"timestamp">> => erlang:system_time(second)
    },
    
    logger:info("ETS leak test: ~p tables leaked (~.2f tables/sec, ~.2f MB leaked)",
        [TablesLeaked, EtsLeakRate, MemoryLeaked]),
    
    Result.

%% @doc Run process leak test
-spec run_process_test(binary(), #leak_config{}) -> map().
run_process_test(TestType, Config) ->
    logger:warning("PROCESS LEAK TEST: Spawning processes without exit"),
    
    TestId = <<TestType/binary, "_", (integer_to_binary(erlang:system_time(millisecond)))/binary>>,
    
    InitialProcs = erlang:system_info(process_count),
    InitialMemory = erlang:memory(total) / (1024 * 1024),
    
    Start = erlang:monotonic_time(millisecond),
    
    {FinalCount, ProcessesLeaked, TimeTaken, LimitReason} =
        leak_processes(Config#leak_config.max_connections),
    
    End = erlang:monotonic_time(millisecond),
    
    FinalMemory = erlang:memory(total) / (1024 * 1024),
    MemoryLeaked = FinalMemory - InitialMemory,
    
    TimeSec = (End - Start) / 1000.0,
    ProcessLeakRate = ProcessesLeaked / TimeSec,
    MemoryLeakRate = MemoryLeaked / TimeSec,
    
    Result = #{
        <<"workload_id">> => TestId,
        <<"benchmark">> => <<"resource_leak">>,
        <<"test_type">> => <<"process_leak">>,
        <<"max_processes">> => Config#leak_config.max_connections,
        <<"processes_leaked">> => ProcessesLeaked,
        <<"initial_process_count">> => InitialProcs,
        <<"final_process_count">> => FinalCount,
        <<"memory_leaked_mb">> => MemoryLeaked,
        <<"process_leak_rate_per_sec">> => ProcessLeakRate,
        <<"memory_leak_rate_mb_per_sec">> => MemoryLeakRate,
        <<"time_to_limit_s">> => TimeSec,
        <<"limiting_resource">> => LimitReason,
        <<"scope">> => <<"per_node">>,
        <<"timestamp">> => erlang:system_time(second)
    },
    
    logger:info("Process leak test: ~p processes leaked (~.2f procs/sec, ~.2f MB leaked)",
        [ProcessesLeaked, ProcessLeakRate, MemoryLeaked]),
    
    Result.

%% @doc Run binary leak test
-spec run_binary_test(binary(), #leak_config{}) -> map().
run_binary_test(TestType, Config) ->
    logger:warning("BINARY LEAK TEST: Accumulating binary references"),
    
    TestId = <<TestType/binary, "_", (integer_to_binary(erlang:system_time(millisecond)))/binary>>,
    
    InitialBinaryMem = erlang:memory(binary) / (1024 * 1024),
    InitialTotalMem = erlang:memory(total) / (1024 * 1024),
    
    Start = erlang:monotonic_time(millisecond),
    
    {BinariesLeaked, TimeTaken, LimitReason} =
        leak_binaries(Config#leak_config.max_connections, Config#leak_config.messages_per_client),
    
    End = erlang:monotonic_time(millisecond),
    
    FinalBinaryMem = erlang:memory(binary) / (1024 * 1024),
    FinalTotalMem = erlang:memory(total) / (1024 * 1024),
    
    BinaryLeaked = FinalBinaryMem - InitialBinaryMem,
    TotalLeaked = FinalTotalMem - InitialTotalMem,
    
    TimeSec = (End - Start) / 1000.0,
    BinaryLeakRate = BinaryLeaked / TimeSec,
    
    Result = #{
        <<"workload_id">> => TestId,
        <<"benchmark">> => <<"resource_leak">>,
        <<"test_type">> => <<"binary_leak">>,
        <<"max_binaries">> => Config#leak_config.max_connections,
        <<"binaries_leaked">> => BinaryLeaked,
        <<"binary_memory_leaked_mb">> => BinaryLeaked,
        <<"total_memory_leaked_mb">> => TotalLeaked,
        <<"binary_leak_rate_mb_per_sec">> => BinaryLeakRate,
        <<"time_to_limit_s">> => TimeSec,
        <<"limiting_resource">> => LimitReason,
        <<"scope">> => <<"per_node">>,
        <<"timestamp">> => erlang:system_time(second)
    },
    
    logger:info("Binary leak test: ~.2f MB leaked (~.2f MB/sec)",
        [BinaryLeaked, BinaryLeakRate]),
    
    Result.

%% @doc Run mailbox leak test
-spec run_mailbox_test(binary(), #leak_config{}) -> map().
run_mailbox_test(TestType, Config) ->
    logger:warning("MAILBOX LEAK TEST: Filling process mailboxes"),
    
    TestId = <<TestType/binary, "_", (integer_to_binary(erlang:system_time(millisecond)))/binary>>,
    
    InitialMessageQueueLen = get_total_message_queue_len(),
    InitialTotalMem = erlang:memory(total) / (1024 * 1024),
    
    Start = erlang:monotonic_time(millisecond),
    
    {MessagesLeaked, ProcessesAffected, TimeTaken, LimitReason} =
        leak_mailbox_messages(Config#leak_config.max_connections, Config#leak_config.messages_per_client),
    
    End = erlang:monotonic_time(millisecond),
    
    FinalMessageQueueLen = get_total_message_queue_len(),
    FinalTotalMem = erlang:memory(total) / (1024 * 1024),
    
    QueueLeaked = FinalMessageQueueLen - InitialMessageQueueLen,
    TotalLeaked = FinalTotalMem - InitialTotalMem,
    
    TimeSec = (End - Start) / 1000.0,
    MessageLeakRate = MessagesLeaked / TimeSec,
    
    Result = #{
        <<"workload_id">> => TestId,
        <<"benchmark">> => <<"resource_leak">>,
        <<"test_type">> => <<"mailbox_leak">>,
        <<"max_messages">> => Config#leak_config.max_connections * Config#leak_config.messages_per_client,
        <<"messages_leaked">> => MessagesLeaked,
        <<"processes_affected">> => ProcessesAffected,
        <<"queue_length_leaked">> => QueueLeaked,
        <<"total_memory_leaked_mb">> => TotalLeaked,
        <<"message_leak_rate_per_sec">> => MessageLeakRate,
        <<"time_to_limit_s">> => TimeSec,
        <<"limiting_resource">> => LimitReason,
        <<"scope">> => <<"per_node">>,
        <<"timestamp">> => erlang:system_time(second)
    },
    
    logger:info("Mailbox leak test: ~p messages leaked (~.2f msgs/sec, ~.2f MB leaked)",
        [MessagesLeaked, MessageLeakRate, TotalLeaked]),
    
    Result.

%% @doc Run full leak test (all types combined)
-spec run_full_test(binary(), #leak_config{}) -> map().
run_full_test(TestType, Config) ->
    logger:warning("FULL RESOURCE LEAK TEST: All leak types combined"),
    
    TestId = <<TestType/binary, "_", (integer_to_binary(erlang:system_time(millisecond)))/binary>>,
    
    {ok, ServerPid} = start_test_server(Config#leak_config.server_port),
    timer:sleep(1000),
    
    InitialSnapshot = take_snapshot(0),
    
    Start = erlang:monotonic_time(millisecond),
    
    {FinalState, Snapshots} = run_full_leak(Config, InitialSnapshot),
    
    End = erlang:monotonic_time(millisecond),
    
    LeakRates = calculate_leak_rates(InitialSnapshot, FinalState, Snapshots),
    
    {ConnCount, TimeToCrash, LimitingResource, CrashReason} = FinalState,
    {Sockets, Ets, Processes, Memory} = get_resource_breakdown(),
    
    {_, ServerRecovered, ResourcesFreed} =
        attempt_recovery(ServerPid, Config#leak_config.server_port),
    
    Result = #{
        <<"workload_id">> => TestId,
        <<"benchmark">> => <<"resource_leak">>,
        <<"test_type">> => <<"full_leak">>,
        <<"max_connections">> => Config#leak_config.max_connections,
        <<"connection_count_at_crash">> => ConnCount,
        <<"time_to_crash_s">> => (End - Start) / 1000.0,
        <<"limiting_resource">> => LimitingResource,
        <<"crash_reason">> => io_lib:format("~p", [CrashReason]),
        <<"leak_progress">> => Snapshots,
        <<"fd_leak_rate_per_sec">> => maps_get(<<"fd_rate">>, LeakRates, 0.0),
        <<"memory_leak_rate_mb_per_sec">> => maps_get(<<"memory_rate">>, LeakRates, 0.0),
        <<"process_leak_rate_per_sec">> => maps_get(<<"process_rate">>, LeakRates, 0.0),
        <<"ets_leak_rate_per_sec">> => maps_get(<<"ets_rate">>, LeakRates, 0.0),
        <<"sockets_leaked">> => Sockets,
        <<"ets_entries_leaked">> => Ets,
        <<"processes_leaked">> => Processes,
        <<"memory_leaked_mb">> => Memory,
        <<"server_recovered">> => ServerRecovered,
        <<"resources_freed">> => ResourcesFreed,
        <<"scope">> => <<"per_node">>,
        <<"timestamp">> => erlang:system_time(second)
    },
    
    logger:info("Full leak test: ~p connections, limiting=~p",
        [ConnCount, LimitingResource]),
    
    Result.

%%====================================================================
%% Leak Implementation Functions
%%====================================================================

%% @doc Leak TCP connections (open, never close)
-spec leak_connections(#leak_config{}, map()) -> {{pos_integer(), float(), binary(), term()}, [map()]}.
leak_connections(Config, InitialSnapshot) ->
    MaxConns = Config#leak_config.max_connections,
    Rate = Config#leak_config.connect_rate,
    MsgsPerClient = Config#leak_config.messages_per_client,
    ReportInterval = Config#leak_config.report_interval,
    Timeout = Config#leak_config.timeout_ms,
    
    Start = erlang:monotonic_time(millisecond),
    Port = Config#leak_config.server_port,
    
    Parent = self(),
    LeakPid = spawn_link(fun() ->
        leak_connections_loop(Parent, Port, MaxConns, Rate, MsgsPerClient,
                            ReportInterval, Start, InitialSnapshot, [])
    end),
    
    receive
        {leak_complete, Result} ->
            Result;
        {leak_crashed, Reason} ->
            FinalSnapshot = take_snapshot(MaxConns),
            {{MaxConns, Timeout/1000.0, <<"crash">>, Reason}, [FinalSnapshot]}
    after Timeout ->
        exit(LeakPid, kill),
        FinalSnapshot = take_snapshot(MaxConns),
        {{MaxConns, Timeout/1000.0, <<"timeout">>, timeout}, [FinalSnapshot]}
    end.

%% @private Loop to leak connections
leak_connections_loop(Parent, Port, MaxConns, Rate, MsgsPerClient,
                     ReportInterval, Start, InitialSnapshot, AccSnapshots) ->
    leak_connections_loop(Parent, Port, MaxConns, Rate, MsgsPerClient,
                         ReportInterval, Start, 0, InitialSnapshot, AccSnapshots).

leak_connections_loop(Parent, Port, MaxConns, Rate, MsgsPerClient,
                     ReportInterval, Start, ConnCount, PrevSnapshot, AccSnapshots) ->
    
    ShouldSnapshot = (ConnCount rem ReportInterval =:= 0) andalso ConnCount > 0,
    
    CurrentSnapshot = case ShouldSnapshot of
        true ->
            Snap = take_snapshot(ConnCount),
            logger:info("Snapshot at ~p conns: fds=~p, mem=~pMB, procs=~p",
                [ConnCount,
                 maps_get(<<"fds">>, Snap, 0),
                 maps_get(<<"memory_mb">>, Snap, 0),
                 maps_get(<<"processes">>, Snap, 0)]),
            Snap;
        false ->
            PrevSnapshot
    end,
    
    {FdLimit, _} = get_fd_limit(),
    CurrentFd = erlang:system_info(port_count),
    CurrentMem = erlang:memory(total),
    MaxMem = FdLimit * 1024 * 1024,
    
    LimitingResource = case CurrentFd >= FdLimit of
        true -> {fd_limit, FdLimit};
        false ->
            case CurrentMem >= MaxMem of
                true -> {memory_limit, MaxMem};
                false -> none
            end
    end,
    
    case LimitingResource of
        none when ConnCount < MaxConns ->
            case connect_and_leak(Port, MsgsPerClient) of
                ok ->
                    DelayMs = 1000 div Rate,
                    timer:sleep(DelayMs),
                    
                    NewAcc = case ShouldSnapshot of
                        true -> [CurrentSnapshot | AccSnapshots];
                        false -> AccSnapshots
                    end,
                    
                    leak_connections_loop(Parent, Port, MaxConns, Rate, MsgsPerClient,
                                        ReportInterval, Start, ConnCount + 1,
                                        CurrentSnapshot, NewAcc);
                {error, Reason} ->
                    End = erlang:monotonic_time(millisecond),
                    TimeSec = (End - Start) / 1000.0,
                    LimitRes = io_lib:format("~p", [LimitingResource]),
                    Parent ! {leak_complete,
                        {{ConnCount, TimeSec, list_to_binary(LimitRes), Reason},
                         lists:reverse([CurrentSnapshot | AccSnapshots])}}
            end;
        _ ->
            End = erlang:monotonic_time(millisecond),
            TimeSec = (End - Start) / 1000.0,
            LimitRes = case LimitingResource of
                none -> <<"max_connections">>;
                {Type, _} -> atom_to_binary(Type)
            end,
            Parent ! {leak_complete,
                {{ConnCount, TimeSec, LimitRes, limit_reached},
                 lists:reverse([CurrentSnapshot | AccSnapshots])}}
    end.

%% @private Connect and leak (never close)
connect_and_leak(Port, MsgCount) ->
    case gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {packet, 0}], 5000) of
        {ok, Socket} ->
            spawn(fun() ->
                [case gen_tcp:send(Socket, <<"{\"test\":", (integer_to_binary(N))/binary, "}">>) of
                    ok -> ok;
                    _ -> ok
                end || N <- lists:seq(1, MsgCount)],
                receive
                    never -> ok
                end
            end),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Leak ETS tables
-spec leak_ets_tables(pos_integer()) -> {pos_integer(), pos_integer(), float(), binary()}.
leak_ets_tables(MaxTables) ->
    leak_ets_tables_loop(MaxTables, 0, erlang:monotonic_time(millisecond)).

leak_ets_tables_loop(MaxTables, Count, Start) ->
    if
        Count >= MaxTables ->
            Final = length(ets:all()),
            Time = (erlang:monotonic_time(millisecond) - Start) / 1000.0,
            {Final, Count, Time, <<"max_tables">>};
        true ->
            try
                TableName = list_to_atom("leaked_table_" ++ integer_to_list(Count)),
                ets:new(TableName, [set, public, named_table]),
                [ets:insert(TableName, {N, <<N:64>>}) || N <- lists:seq(1, 100)],
                leak_ets_tables_loop(MaxTables, Count + 1, Start)
            catch
                _:_ ->
                    Final = length(ets:all()),
                    Time = (erlang:monotonic_time(millisecond) - Start) / 1000.0,
                    {Final, Count, Time, <<"ets_limit">>}
            end
    end.

%% @doc Leak processes
-spec leak_processes(pos_integer()) -> {pos_integer(), pos_integer(), float(), binary()}.
leak_processes(MaxProcs) ->
    leak_processes_loop(MaxProcs, 0, erlang:monotonic_time(millisecond)).

leak_processes_loop(MaxProcs, Count, Start) ->
    if
        Count >= MaxProcs ->
            Final = erlang:system_info(process_count),
            Time = (erlang:monotonic_time(millisecond) - Start) / 1000.0,
            {Final, Count, Time, <<"max_processes">>};
        true ->
            try
                spawn(fun() ->
                    Data = lists:seq(1, 1000),
                    receive
                        never -> ok
                    end
                end),
                leak_processes_loop(MaxProcs, Count + 1, Start)
            catch
                _:_ ->
                    Final = erlang:system_info(process_count),
                    Time = (erlang:monotonic_time(millisecond) - Start) / 1000.0,
                    {Final, Count, Time, <<"process_limit">>}
            end
    end.

%% @doc Leak binaries
-spec leak_binaries(pos_integer(), pos_integer()) -> {float(), float(), binary()}.
leak_binaries(MaxBinaries, MsgPerClient) ->
    Pid = spawn(fun() ->
        leak_binaries_loop(MaxBinaries, MsgPerClient, [])
    end),
    
    timer:sleep(5000),
    BinaryMem = erlang:memory(binary) / (1024 * 1024),
    Time = 5.0,
    
    Ref = make_ref(),
    Pid ! {get_count, self(), Ref},
    receive
        {count, Ref, Count} ->
            {BinaryMem, Time, <<"binary_limit">>}
    after 1000 ->
        {BinaryMem, Time, <<"timeout">>}
    end.

%% @private
leak_binaries_loop(Max, MsgPerClient, Acc) ->
    receive
        {get_count, From, Ref} ->
            From ! {count, Ref, length(Acc)},
            leak_binaries_loop(Max, MsgPerClient, Acc)
    after 0 ->
        if
            length(Acc) >= Max ->
                leak_binaries_loop(Max, MsgPerClient, Acc);
            true ->
                Binary = crypto:strong_rand_bytes(1024 * 100),
                leak_binaries_loop(Max, MsgPerClient, [Binary | Acc])
        end
    end.

%% @doc Leak mailbox messages
-spec leak_mailbox_messages(pos_integer(), pos_integer()) ->
    {pos_integer(), pos_integer(), float(), binary()}.
leak_mailbox_messages(MaxMessages, MsgPerProc) ->
    NumProcs = MaxMessages div MsgPerProc,
    
    Pids = [spawn(fun() ->
        [self() ! {leak, N, crypto:strong_rand_bytes(1024)} || N <- lists:seq(1, MsgPerProc)],
        receive never -> ok end
    end) || _ <- lists:seq(1, NumProcs)],
    
    timer:sleep(2000),
    
    MessagesLeaked = NumProcs * MsgPerProc,
    Time = 2.0,
    
    {MessagesLeaked, NumProcs, Time, <<"mailbox_limit">>}.

%% @doc Run full leak test
-spec run_full_leak(#leak_config{}, map()) -> {{pos_integer(), float(), binary(), term()}, [map()]}.
run_full_leak(Config, InitialSnapshot) ->
    MaxConns = Config#leak_config.max_connections,
    Rate = Config#leak_config.connect_rate,
    ReportInterval = Config#leak_config.report_interval,
    Timeout = Config#leak_config.timeout_ms,
    Port = Config#leak_config.server_port,
    
    Start = erlang:monotonic_time(millisecond),
    
    Parent = self(),
    LeakPid = spawn_link(fun() ->
        full_leak_loop(Parent, Port, MaxConns, Rate, ReportInterval,
                      Start, 0, InitialSnapshot, [])
    end),
    
    receive
        {leak_complete, Result} ->
            Result
    after Timeout ->
        exit(LeakPid, kill),
        FinalSnapshot = take_snapshot(MaxConns),
        {{MaxConns, Timeout/1000.0, <<"timeout">>, timeout}, [FinalSnapshot]}
    end.

%% @private
full_leak_loop(Parent, Port, MaxConns, Rate, ReportInterval,
              Start, ConnCount, PrevSnapshot, AccSnapshots) ->
    
    ShouldSnapshot = (ConnCount rem ReportInterval =:= 0) andalso ConnCount > 0,
    
    CurrentSnapshot = case ShouldSnapshot of
        true ->
            Snap = take_snapshot(ConnCount),
            logger:info("Snapshot ~p conns: fds=~p, mem=~pMB, procs=~p, ets=~p",
                [ConnCount,
                 maps_get(<<"fds">>, Snap, 0),
                 maps_get(<<"memory_mb">>, Snap, 0),
                 maps_get(<<"processes">>, Snap, 0),
                 maps_get(<<"ets_tables">>, Snap, 0)]),
            Snap;
        false ->
            PrevSnapshot
    end,
    
    case ConnCount >= MaxConns of
        true ->
            End = erlang:monotonic_time(millisecond),
            TimeSec = (End - Start) / 1000.0,
            Parent ! {leak_complete,
                {{ConnCount, TimeSec, <<"max_connections">>, complete},
                 lists:reverse([CurrentSnapshot | AccSnapshots])}};
        false ->
            case connect_and_leak_full(Port, ConnCount) of
                ok ->
                    DelayMs = 1000 div Rate,
                    timer:sleep(DelayMs),
                    
                    NewAcc = case ShouldSnapshot of
                        true -> [CurrentSnapshot | AccSnapshots];
                        false -> AccSnapshots
                    end,
                    
                    full_leak_loop(Parent, Port, MaxConns, Rate, ReportInterval,
                                  Start, ConnCount + 1, CurrentSnapshot, NewAcc);
                {error, Reason} ->
                    End = erlang:monotonic_time(millisecond),
                    TimeSec = (End - Start) / 1000.0,
                    Parent ! {leak_complete,
                        {{ConnCount, TimeSec, <<"error">>, Reason},
                         lists:reverse([CurrentSnapshot | AccSnapshots])}}
            end
    end.

%% @private
connect_and_leak_full(Port, ConnId) ->
    case gen_tcp:connect({127, 0, 0, 1}, Port, [binary, {active, false}, {packet, 0}], 5000) of
        {ok, Socket} ->
            spawn(fun() ->
                _ = [gen_tcp:send(Socket, <<"{\"leak\":", (integer_to_binary(N))/binary, "}">>)
                    || N <- lists:seq(1, 20)],
                
                EtsName = list_to_atom("leak_ets_" ++ integer_to_list(ConnId)),
                ets:new(EtsName, [set, public, named_table]),
                ets:insert(EtsName, {conn_id, ConnId}),
                ets:insert(EtsName, {data, crypto:strong_rand_bytes(1024 * 10)}),
                
                spawn(fun() ->
                    receive never -> ok end
                end),
                
                receive never -> ok end
            end),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Take a snapshot of current resource usage
-spec take_snapshot(non_neg_integer()) -> map().
take_snapshot(ConnCount) ->
    #{
        <<"connection_count">> => ConnCount,
        <<"fds">> => erlang:system_info(port_count),
        <<"memory_mb">> => erlang:memory(total) / (1024 * 1024),
        <<"processes">> => erlang:system_info(process_count),
        <<"ets_tables">> => length(ets:all()),
        <<"ets_memory_mb">> => erlang:memory(ets) / (1024 * 1024),
        <<"binary_mb">> => erlang:memory(binary) / (1024 * 1024),
        <<"system_memory_mb">> => get_system_memory_mb(),
        <<"timestamp_ms">> => erlang:monotonic_time(millisecond)
    }.

%% @doc Calculate leak rates from snapshots
-spec calculate_leak_rates(map(), {pos_integer(), float(), binary(), term()}, [map()]) -> map().
calculate_leak_rates(Initial, FinalState, Snapshots) ->
    {ConnCount, TimeSec, _, _} = FinalState,
    
    case Snapshots of
        [] ->
            #{};
        [FirstSnap | _] ->
            InitialFds = maps_get(<<"fds">>, Initial, 0),
            InitialMem = maps_get(<<"memory_mb">>, Initial, 0.0),
            InitialProcs = maps_get(<<"processes">>, Initial, 0),
            InitialEts = maps_get(<<"ets_tables">>, Initial, 0),
            
            FinalFds = maps_get(<<"fds">>, FirstSnap, InitialFds),
            FinalMem = maps_get(<<"memory_mb">>, FirstSnap, InitialMem),
            FinalProcs = maps_get(<<"processes">>, FirstSnap, InitialProcs),
            FinalEts = maps_get(<<"ets_tables">>, FirstSnap, InitialEts),
            
            #{
                <<"fd_rate">> => (FinalFds - InitialFds) / TimeSec,
                <<"memory_rate">> => (FinalMem - InitialMem) / TimeSec,
                <<"process_rate">> => (FinalProcs - InitialProcs) / TimeSec,
                <<"ets_rate">> => (FinalEts - InitialEts) / TimeSec
            }
    end.

%% @doc Get resource breakdown at crash
-spec get_resource_breakdown() -> {non_neg_integer(), non_neg_integer(), non_neg_integer(), float()}.
get_resource_breakdown() ->
    Sockets = erlang:system_info(port_count),
    EtsEntries = lists:sum([ets:info(T, size) || T <- ets:all()]),
    Processes = erlang:system_info(process_count),
    Memory = erlang:memory(total) / (1024 * 1024),
    
    {Sockets, EtsEntries, Processes, Memory}.

%% @doc Attempt recovery
-spec attempt_recovery(pid(), inet:port_number()) -> {boolean(), boolean(), non_neg_integer()}.
attempt_recovery(ServerPid, Port) ->
    InitialFds = erlang:system_info(port_count),
    InitialProcs = erlang:system_info(process_count),
    
    erlang:garbage_collect(),
    
    CloseCount = close_all_leaked_sockets(Port),
    
    timer:sleep(2000),
    
    ServerAlive = is_process_alive(ServerPid),
    FinalFds = erlang:system_info(port_count),
    FinalProcs = erlang:system_info(process_count),
    
    ResourcesFreed = (InitialFds - FinalFds) + (InitialProcs - FinalProcs),
    
    {true, ServerAlive, ResourcesFreed}.

%% @private Close all leaked sockets
close_all_leaked_sockets(Port) ->
    Ports = erlang:ports(),
    TcpPorts = [P || P <- Ports, erlang:port_info(P, name) =:= {name, "tcp_inet"}],
    
    CloseCount = lists:foldl(fun(P, Acc) ->
        case erlang:port_info(P, connected) of
            {connected, {127, 0, 0, 1}, Port} ->
                catch erlang:port_close(P),
                Acc + 1;
            _ ->
                Acc
        end
    end, 0, TcpPorts),
    
    CloseCount.

%% @doc Start test server
-spec start_test_server(inet:port_number()) -> {ok, pid()} | {error, term()}.
start_test_server(Port) ->
    case erlmcp_transport_tcp:start_server(#{
        mode => server,
        port => Port,
        num_acceptors => 10,
        max_connections => infinity
    }) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc Get total message queue length
-spec get_total_message_queue_len() -> non_neg_integer().
get_total_message_queue_len() ->
    lists:sum([
        case erlang:process_info(P, message_queue_len) of
            {message_queue_len, Len} -> Len;
            _ -> 0
        end
        || P <- erlang:processes()
    ]).

%% @doc Get system memory (MB)
-spec get_system_memory_mb() -> float().
get_system_memory_mb() ->
    case os:type() of
        {unix, darwin} ->
            case os:cmd("sysctl hw.memsize") of
                "hw.memsize: " ++ MemStr ->
                    MemBytes = list_to_integer(string:trim(MemStr)),
                    MemBytes / (1024 * 1024);
                _ ->
                    0.0
            end;
        {unix, linux} ->
            case os:cmd("grep MemTotal /proc/meminfo") of
                "MemTotal:" ++ Rest ->
                    Tokens = string:split(string:trim(Rest), " ", all),
                    [KbStr | _] = [T || T <- Tokens, T =/= ""],
                    Kb = list_to_integer(KbStr),
                    Kb / 1024;
                _ ->
                    0.0
            end;
        _ ->
            0.0
    end.

%% @doc Get file descriptor limit
-spec get_fd_limit() -> {non_neg_integer(), non_neg_integer()}.
get_fd_limit() ->
    case os:type() of
        {unix, darwin} ->
            case os:cmd("launchctl limit maxfiles") of
                "maxfiles" ++ Rest ->
                    Tokens = string:split(string:trim(Rest), " ", all),
                    [MaxStr | _] = [T || T <- Tokens, T =/= ""],
                    Max = list_to_integer(MaxStr),
                    {Max, Max};
                _ ->
                    {10240, 10240}
            end;
        {unix, linux} ->
            case os:cmd("ulimit -n") of
                Str ->
                    Max = list_to_integer(string:trim(Str)),
                    {Max, Max};
                _ ->
                    {1024, 1024}
            end;
        _ ->
            {1024, 1024}
    end.

%%====================================================================
%% Report Generation
%%====================================================================

%% @doc Generate comprehensive report
-spec generate_report(map()) -> ok.
generate_report(Result) ->
    ReportDir = "bench/results",
    ok = filelib:ensure_dir(ReportDir ++ "/"),
    
    Timestamp = erlang:system_time(second),
    Filename = io_lib:format("~s/resource_leak_~p.json", [ReportDir, Timestamp]),
    
    ReportJson = jsx:encode(Result),
    ok = file:write_file(Filename, ReportJson),
    
    logger:info("Report written to: ~s", [Filename]),
    ok.

%% @doc Print leak summary
-spec print_leak_summary(map()) -> ok.
print_leak_summary(Result) ->
    io:format("~n=== RESOURCE LEAK CRASH TEST ===~n~n", []),
    
    io:format("Leak Configuration:~n", []),
    io:format("- Max Clients: ~p~n", [maps_get(<<"max_connections">>, Result, 0)]),
    io:format("- Connect Rate: ~p/sec~n", [maps_get(<<"connect_rate">>, Result, 0)]),
    io:format("- Close Connections: NO (intentional leak)~n", []),
    io:format("- Messages per Client: ~p~n~n", [maps_get(<<"messages_per_client">>, Result, 0)]),
    
    LeakProgress = maps_get(<<"leak_progress">>, Result, []),
    io:format("Leak Progress:~n", []),
    lists:foreach(fun(Snap) ->
        Conn = maps_get(<<"connection_count">>, Snap, 0),
        Fds = maps_get(<<"fds">>, Snap, 0),
        Mem = maps_get(<<"memory_mb">>, Snap, 0.0),
        Procs = maps_get(<<"processes">>, Snap, 0),
        Ets = maps_get(<<"ets_tables">>, Snap, 0),
        io:format("- ~p conns: [fds=~p, mem=~.2fMB, procs=~p, ets=~p]~n",
            [Conn, Fds, Mem, Procs, Ets])
    end, lists:sublist(LeakProgress, 5)),
    io:format("~n", []),
    
    io:format("LEAK RATES:~n", []),
    io:format("- File Descriptors: ~.2f fds/sec~n",
        [maps_get(<<"fd_leak_rate_per_sec">>, Result, 0.0)]),
    io:format("- Memory: ~.2f MB/sec~n",
        [maps_get(<<"memory_leak_rate_mb_per_sec">>, Result, 0.0)]),
    io:format("- Processes: ~.2f/sec~n",
        [maps_get(<<"process_leak_rate_per_sec">>, Result, 0.0)]),
    io:format("- ETS Tables: ~.2f/sec~n~n",
        [maps_get(<<"ets_leak_rate_per_sec">>, Result, 0.0)]),
    
    io:format("BREAKING POINT:~n", []),
    io:format("- Connection Count: ~p~n",
        [maps_get(<<"connection_count_at_crash">>, Result, 0)]),
    io:format("- Time to Crash: ~.2f minutes~n",
        [(maps_get(<<"time_to_crash_s">>, Result, 0.0) / 60.0)]),
    io:format("- Limiting Resource: ~p~n",
        [maps_get(<<"limiting_resource">>, Result, <<"unknown">>)]),
    io:format("- Error: ~p~n",
        [maps_get(<<"crash_reason">>, Result, <<"unknown">>)]),
    io:format("- Server Status: crashed~n~n", []),
    
    io:format("RESOURCE BREAKDOWN:~n", []),
    io:format("- Sockets Leaked: ~p~n",
        [maps_get(<<"sockets_leaked">>, Result, 0)]),
    io:format("- ETS Entries Leaked: ~p~n",
        [maps_get(<<"ets_entries_leaked">>, Result, 0)]),
    io:format("- Processes Leaked: ~p~n",
        [maps_get(<<"processes_leaked">>, Result, 0)]),
    io:format("- Memory Leaked: ~.2f GB~n~n",
        [(maps_get(<<"memory_leaked_mb">>, Result, 0.0) / 1024.0)]),
    
    io:format("RECOVERY TEST:~n", []),
    io:format("- Attempted Cleanup: yes~n", []),
    io:format("- Server Recovered: ~p~n",
        [maps_get(<<"server_recovered">>, Result, false)]),
    io:format("- Resources Freed: ~p~n~n",
        [maps_get(<<"resources_freed">>, Result, 0)]),
    
    io:format("ANALYSIS:~n", []),
    analyze_leak(Result),
    
    io:format("~n=== END LEAK TEST REPORT ===~n", []),
    ok.

%% @private Analyze leak patterns
-spec analyze_leak(map()) -> ok.
analyze_leak(Result) ->
    FdRate = maps_get(<<"fd_leak_rate_per_sec">>, Result, 0.0),
    MemRate = maps_get(<<"memory_leak_rate_mb_per_sec">>, Result, 0.0),
    Limiting = maps_get(<<"limiting_resource">>, Result, <<"unknown">>),
    
    io:format("Fastest leaking resource: ", []),
    
    Rates = [
        {fds, FdRate},
        {memory, MemRate},
        {processes, maps_get(<<"process_leak_rate_per_sec">>, Result, 0.0)},
        {ets, maps_get(<<"ets_leak_rate_per_sec">>, Result, 0.0)}
    ],
    
    {FastestType, FastestRate} = lists:max(Rates),
    
    case FastestRate of
        0.0 ->
            io:format("No significant leak detected~n", []);
        _ ->
            io:format("~p (~.2f units/sec)~n", [FastestType, FastestRate])
    end,
    
    io:format("Limiting factor: ~p~n", [Limiting]),
    
    Recovered = maps_get(<<"server_recovered">>, Result, false),
    io:format("Recovery possible: ~p~n", [Recovered]),
    
    ok.

%% @private Convert leak result record to map
-spec leak_result_to_map(#leak_result{}) -> map().
leak_result_to_map(#leak_result{} = R) ->
    #{
        <<"workload_id">> => R#leak_result.test_id,
        <<"benchmark">> => <<"resource_leak">>,
        <<"test_type">> => R#leak_result.test_type,
        <<"max_connections">> => R#leak_result.max_connections,
        <<"connect_rate">> => R#leak_result.connect_rate,
        <<"messages_per_client">> => R#leak_result.messages_per_client,
        <<"cleanup_enabled">> => R#leak_result.cleanup_enabled,
        <<"leak_progress">> => R#leak_result.leak_progress,
        <<"fd_leak_rate_per_sec">> => case R#leak_result.fd_leak_rate of
            undefined -> 0.0;
            V -> V
        end,
        <<"memory_leak_rate_mb_per_sec">> => case R#leak_result.memory_leak_rate of
            undefined -> 0.0;
            V -> V
        end,
        <<"process_leak_rate_per_sec">> => case R#leak_result.process_leak_rate of
            undefined -> 0.0;
            V -> V
        end,
        <<"ets_leak_rate_per_sec">> => case R#leak_result.ets_leak_rate of
            undefined -> 0.0;
            V -> V
        end,
        <<"connection_count_at_crash">> => R#leak_result.connection_count_at_crash,
        <<"time_to_crash_s">> => case R#leak_result.time_to_crash_s of
            undefined -> 0.0;
            V -> V
        end,
        <<"limiting_resource">> => R#leak_result.limiting_resource,
        <<"crash_reason">> => io_lib:format("~p", [R#leak_result.crash_reason]),
        <<"sockets_leaked">> => R#leak_result.sockets_leaked,
        <<"ets_entries_leaked">> => R#leak_result.ets_entries_leaked,
        <<"processes_leaked">> => R#leak_result.processes_leaked,
        <<"memory_leaked_mb">> => R#leak_result.memory_leaked_mb,
        <<"attempted_cleanup">> => R#leak_result.attempted_cleanup,
        <<"server_recovered">> => R#leak_result.server_recovered,
        <<"resources_freed">> => R#leak_result.resources_freed,
        <<"os_type">> => element(2, R#leak_result.os_type),
        <<"erlang_version">> => list_to_binary(R#leak_result.erlang_version),
        <<"total_memory_mib">> => R#leak_result.total_memory_mib,
        <<"fd_limit">> => R#leak_result.fd_limit,
        <<"scope">> => R#leak_result.scope,
        <<"timestamp">> => R#leak_result.timestamp
    }.

%% @private
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Val} -> Val;
        error -> Default
    end.
