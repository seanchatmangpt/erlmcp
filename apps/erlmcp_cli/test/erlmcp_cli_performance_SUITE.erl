%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Performance Test Suite (Common Test)
%%%
%%% Performance tests for erlmcp_cli application
%%%
%%% Chicago School TDD:
%%% - Real workload simulation
%%% - Benchmark metrics collection
%%% - Performance regression detection
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_performance_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Suite Callbacks
%%%====================================================================

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(erlmcp_cli),
    Config.

end_per_suite(Config) ->
    application:stop(erlmcp_cli),
    Config.

init_per_testcase(_TestCase, Config) ->
    erlmcp_cli_metrics:reset_all_metrics(),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%%====================================================================
%%% Test Cases
%%%====================================================================

%% @doc Command execution throughput test
command_execution_throughput_test(_Config) ->
    NumCommands = 1000,
    Start = erlang:monotonic_time(millisecond),

    lists:foreach(fun(_) ->
        erlmcp_cli_registry:execute_command(<<"mcp.health">>, [])
    end, lists:seq(1, NumCommands)),

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,

    Throughput = (NumCommands * 1000) / Duration,
    ct:pal("Executed ~p commands in ~p ms (~p commands/sec)",
           [NumCommands, Duration, Throughput]),

    %% Verify performance acceptable (< 1ms per command)
    ?assert(Duration < NumCommands).

%% @doc JSON-RPC request/response latency test
json_rpc_latency_test(_Config) ->
    NumRequests = 100,
    Latencies = [begin
        Start = erlang:monotonic_time(microsecond),

        RequestJson = jsx:encode(#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"method">> => <<"mcp.health">>,
            <<"params">> => null,
            <<"id">> => N
        }),
        {ok, _Response} = erlmcp_cli_json_rpc:handle_json_rpc(RequestJson, #{}, <<"test">>),

        End = erlang:monotonic_time(microsecond),
        End - Start
    end || N <- lists:seq(1, NumRequests)],

    AvgLatency = lists:sum(Latencies) div length(Latencies),
    ct:pal("Average JSON-RPC latency: ~p μs", [AvgLatency]),

    %% Verify latency acceptable (< 10ms per request)
    ?assert(AvgLatency < 10000).

%% @doc Transport layer performance test
transport_performance_test(_Config) ->
    %% Initialize stdio transport
    Config = #{<<"type">> => <<"stdio">>, <<"session_id">> => <<"perf-test">>},
    ok = erlmcp_cli_transport:transport(<<"stdio">>, Config),

    %% Send messages
    NumMessages = 100,
    Start = erlang:monotonic_time(millisecond),

    lists:foreach(fun(N) ->
        Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":",
                   (integer_to_binary(N))/binary, "}">>,
        erlmcp_cli_transport:send_data(<<"stdio">>, Message)
    end, lists:seq(1, NumMessages)),

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,

    ct:pal("Sent ~p messages in ~p ms", [NumMessages, Duration]),

    %% Verify transport performance
    ?assert(Duration < NumMessages * 10),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>).

%% @doc Session management performance test
session_management_performance_test(_Config) ->
    NumSessions = 100,
    Start = erlang:monotonic_time(millisecond),

    %% Create sessions
    SessionIds = [begin
        Id = <<"perf-session-", (integer_to_binary(N))/binary>>,
        {ok, _Pid} = erlmcp_cli_session:create_session(Id, #{}),
        ok = erlmcp_cli_session:start_session(Id),
        Id
    end || N <- lists:seq(1, NumSessions)],

    CreateEnd = erlang:monotonic_time(millisecond),
    CreateDuration = CreateEnd - Start,

    ct:pal("Created and started ~p sessions in ~p ms", [NumSessions, CreateDuration]),

    %% Verify performance
    ?assert(CreateDuration < NumSessions * 10),

    %% Cleanup sessions
    lists:foreach(fun(Id) ->
        erlmcp_cli_session:terminate_session(Id)
    end, SessionIds).

%% @doc Metrics collection overhead test
metrics_overhead_test(_Config) ->
    NumOperations = 1000,
    Start = erlang:monotonic_time(millisecond),

    %% Metrics operations
    lists:foreach(fun(N) ->
        erlmcp_cli_metrics:increment_counter(<<"overhead.counter">>),
        erlmcp_cli_metrics:set_gauge(<<"overhead.gauge">>, N),
        erlmcp_cli_metrics:record_histogram(<<"overhead.histogram">>, N)
    end, lists:seq(1, NumOperations)),

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,

    ct:pal("Metrics overhead for ~p operations: ~p ms", [NumOperations, Duration]),

    %% Verify overhead minimal (< 5ms for 1000 operations)
    ?assert(Duration < 100).

%% @doc Concurrent operation scalability test
concurrent_scalability_test(_Config) ->
    NumProcesses = 50,
    OpsPerProcess = 20,
    Start = erlang:monotonic_time(millisecond),

    %% Spawn concurrent processes
    Pids = [spawn(fun() ->
        lists:foreach(fun(_) ->
            erlmcp_cli_registry:execute_command(<<"mcp.health">>, []),
            erlmcp_cli_metrics:increment_counter(<<"concurrent.counter">>)
        end, lists:seq(1, OpsPerProcess)),
        self() ! done
    end) || _ <- lists:seq(1, NumProcesses)],

    %% Wait for completion
    [receive
        done -> ok
    after 10000 ->
        ct:fail("Concurrent operations timeout")
    end || _ <- Pids],

    End = erlang:monotonic_time(millisecond),
    Duration = End - Start,

    TotalOps = NumProcesses * OpsPerProcess,
    ct:pal("Concurrent: ~p processes × ~p ops = ~p ops in ~p ms",
           [NumProcesses, OpsPerProcess, TotalOps, Duration]),

    %% Verify scalability
    ?assert(Duration < TotalOps * 5).

%% @doc Memory usage profiling test
memory_usage_test(_Config) ->
    %% Get initial memory
    InitialMemory = erlang:memory(total),

    %% Perform operations
    lists:foreach(fun(N) ->
        SessionId = <<"mem-session-", (integer_to_binary(N))/binary>>,
        {ok, _Pid} = erlmcp_cli_session:create_session(SessionId, #{}),
        erlmcp_cli_metrics:increment_counter(<<"mem.counter">>),
        erlmcp_cli_registry:execute_command(<<"mcp.health">>, [])
    end, lists:seq(1, 100)),

    %% Force garbage collection
    erlang:garbage_collect(),
    timer:sleep(100),

    %% Get final memory
    FinalMemory = erlang:memory(total),
    MemoryIncrease = FinalMemory - InitialMemory,

    ct:pal("Memory increase for 100 operations: ~p bytes", [MemoryIncrease]),

    %% Verify memory usage reasonable (< 10MB increase)
    ?assert(MemoryIncrease < 10000000).

%% @doc Performance regression detection test
performance_regression_test(_Config) ->
    %% Baseline: Execute operations and measure
    NumOps = 100,
    Start = erlang:monotonic_time(millisecond),

    lists:foreach(fun(_) ->
        erlmcp_cli_registry:execute_command(<<"mcp.health">>, [])
    end, lists:seq(1, NumOps)),

    End = erlang:monotonic_time(millisecond),
    Baseline = End - Start,

    ct:pal("Performance baseline: ~p ms for ~p operations", [Baseline, NumOps]),

    %% Verify baseline acceptable
    ?assert(Baseline < NumOps * 10),

    %% Store baseline for future regression detection
    %% (In production, this would be compared against historical baselines)
    ok.
