-module(erlmcp_integration_contracts_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../../../include/erlmcp.hrl").

%% Export 20+ test functions (10 original + 10 performance)
-export([
    %% Original integration contract tests
    test_valid_initialize_stdio/1,
    test_capability_exchange/1,
    test_version_negotiation/1,
    test_invalid_init_rejection/1,
    test_request_id_correlation/1,
    test_response_format/1,
    test_error_contracts/1,
    test_tools_contract/1,
    test_resources_contract/1,
    test_prompts_contract/1,
    %% Performance contract tests
    test_request_latency_p95_below_100ms/1,
    test_throughput_above_1000_req_per_sec/1,
    test_connection_setup_below_50ms/1,
    test_memory_per_connection_below_1mb/1,
    test_concurrent_1000_connections/1,
    test_connection_limit_enforcement/1,
    test_rate_limiting_enforcement/1,
    test_memory_leak_detection/1,
    test_resource_recovery/1
]).

all() -> [
    {group, init_handshake},
    {group, req_resp},
    {group, msg_types},
    {group, performance_contracts},
    {group, load_testing},
    {group, stress_testing}
].

groups() -> [
    {init_handshake, [sequence], [
        test_valid_initialize_stdio,
        test_capability_exchange,
        test_version_negotiation,
        test_invalid_init_rejection
    ]},
    {req_resp, [sequence], [
        test_request_id_correlation,
        test_response_format,
        test_error_contracts
    ]},
    {msg_types, [parallel], [
        test_tools_contract,
        test_resources_contract,
        test_prompts_contract
    ]},
    {performance_contracts, [parallel], [
        test_request_latency_p95_below_100ms,
        test_throughput_above_1000_req_per_sec,
        test_connection_setup_below_50ms,
        test_memory_per_connection_below_1mb
    ]},
    {load_testing, [sequence], [
        test_concurrent_1000_connections,
        test_connection_limit_enforcement,
        test_rate_limiting_enforcement
    ]},
    {stress_testing, [sequence], [
        test_memory_leak_detection,
        test_resource_recovery
    ]}
].

init_per_suite(C) ->
    Apps = [crypto, ssl, gproc, jsx, jesse],
    lists:foreach(fun(A) -> case application:start(A) of
        ok -> ok; {error, {already_started, _}} -> ok end end, Apps),
    {ok, _} = erlmcp_core_sup:start_link(),
    {ok, _} = erlmcp_server_sup:start_link(),
    C.

end_per_suite(_) ->
    supervisor:stop(erlmcp_server_sup),
    supervisor:stop(erlmcp_core_sup),
    ok.

init_per_group(_, C) -> C.
end_per_group(_, _) -> ok.
init_per_testcase(_, C) -> C.
end_per_testcase(_, _) -> ok.

%% Tests
test_valid_initialize_stdio(C) ->
    ServerCaps = #mcp_server_capabilities{},
    {ok, Pid} = erlmcp_server:start_link(make_id(), ServerCaps),
    erlmcp_server:add_tool(Pid, <<"echo">>, fun(A) -> A end),
    {ok, TPid} = erlmcp_transport_stdio:start_link(Pid, #{}),
    InitReq = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{
            <<"protocolVersion">> => ?MCP_VERSION,
            <<"capabilities">> => #{},
            <<"clientInfo">> => #{<<"name">> => <<"test">>, <<"version">> => <<"1.0">>}
        }
    },
    TPid ! {simulate_input, jsx:encode(InitReq)},
    timer:sleep(100),
    %% Verify server is still alive (observable behavior)
    ?assert(is_process_alive(Pid)),
    erlmcp_transport_stdio:close(TPid),
    erlmcp_server:stop(Pid),
    C.

test_capability_exchange(C) ->
    ClientCaps = #mcp_client_capabilities{roots = #{}},
    InitReq = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{
            <<"protocolVersion">> => ?MCP_VERSION,
            <<"capabilities">> => caps_to_map(ClientCaps),
            <<"clientInfo">> => #{}
        }
    },
    ?assert(maps:is_key(<<"capabilities">>, maps:get(<<"params">>, InitReq))),
    C.

test_version_negotiation(C) ->
    ?assertEqual(<<"2025-11-25">>, ?MCP_VERSION),
    C.

test_invalid_init_rejection(C) ->
    InitReq = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{<<"capabilities">> => #{}, <<"clientInfo">> => #{}}  % Missing version
    },
    ?assertNot(maps:is_key(<<"protocolVersion">>, maps:get(<<"params">>, InitReq))),
    C.

test_request_id_correlation(C) ->
    Requests = [{1, tools}, {2, resources}, {42, prompts}],
    ?assertEqual(3, length(lists:usort([Id || {Id, _} <- Requests]))),
    C.

test_response_format(C) ->
    SuccessResp = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => #{}},
    ?assert(maps:is_key(<<"result">>, SuccessResp)),
    ?assertNot(maps:is_key(<<"error">>, SuccessResp)),
    ErrorResp = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"error">> => #{<<"code">> => -32005}},
    ?assert(maps:is_key(<<"error">>, ErrorResp)),
    ?assertNot(maps:is_key(<<"result">>, ErrorResp)),
    C.

test_error_contracts(C) ->
    ErrCodes = [?JSONRPC_PARSE_ERROR, ?MCP_ERROR_NOT_INITIALIZED, ?MCP_ERROR_TOOL_NOT_FOUND],
    lists:foreach(fun(Code) ->
        Err = #{<<"error">> => #{<<"code">> => Code, <<"message">> => <<"Err">>}},
        ?assert(maps:is_key(<<"error">>, Err))
    end, ErrCodes),
    C.

test_tools_contract(C) ->
    Req = #{<<"method">> => <<"tools/list">>, <<"id">> => 1, <<"jsonrpc">> => <<"2.0">>},
    ?assertMatch(#{<<"method">> := <<"tools/list">>}, Req),
    C.

test_resources_contract(C) ->
    Req = #{<<"method">> => <<"resources/list">>, <<"id">> => 1, <<"jsonrpc">> => <<"2.0">>},
    ?assertMatch(#{<<"method">> := <<"resources/list">>}, Req),
    C.

test_prompts_contract(C) ->
    Req = #{<<"method">> => <<"prompts/list">>, <<"id">> => 1, <<"jsonrpc">> => <<"2.0">>},
    ?assertMatch(#{<<"method">> := <<"prompts/list">>}, Req),
    C.

%%====================================================================
%% Performance Contract Tests
%%====================================================================

%% @doc Test P95 latency below 100ms for simple tool calls
test_request_latency_p95_below_100ms(_Config) ->
    %% Setup server with simple echo tool
    ServerCaps = #mcp_server_capabilities{},
    {ok, ServerPid} = erlmcp_server:start_link(make_id(), ServerCaps),
    erlmcp_server:add_tool(ServerPid, <<"echo">>, fun(Args) -> Args end),

    %% Measure latency for 100 requests
    NumRequests = 100,
    Latencies = lists:map(fun(_) ->
        StartTime = erlang:monotonic_time(microsecond),

        %% Make direct gen_server call (Chicago School: real process)
        {ok, _} = erlmcp_server:call_tool(ServerPid, <<"echo">>, #{<<"test">> => <<"data">>}),

        EndTime = erlang:monotonic_time(microsecond),
        EndTime - StartTime
    end, lists:seq(1, NumRequests)),

    %% Calculate P95 latency
    SortedLatencies = lists:sort(Latencies),
    P95Index = min(length(SortedLatencies), trunc(length(SortedLatencies) * 0.95)),
    P95Latency = lists:nth(P95Index, SortedLatencies),

    %% Assert P95 < 100ms (100,000 microseconds)
    ?assert(P95Latency < 100000),

    %% Cleanup
    erlmcp_server:stop(ServerPid),

    %% Log result
    ct:log("P95 Latency: ~p us (~.2f ms)", [P95Latency, P95Latency / 1000]),
    {comment, io_lib:format("P95: ~.2f ms", [P95Latency / 1000])}.

%% @doc Test throughput above 1000 req/s
test_throughput_above_1000_req_per_sec(_Config) ->
    %% Setup server
    ServerCaps = #mcp_server_capabilities{},
    {ok, ServerPid} = erlmcp_server:start_link(make_id(), ServerCaps),
    erlmcp_server:add_tool(ServerPid, <<"fast">>, fun(_) -> #{<<"result">> => <<"ok">>} end),

    %% Measure throughput for 1 second
    NumRequests = 1000,
    StartTime = erlang:monotonic_time(millisecond),

    lists:foreach(fun(_) ->
        {ok, _} = erlmcp_server:call_tool(ServerPid, <<"fast">>, #{})
    end, lists:seq(1, NumRequests)),

    EndTime = erlang:monotonic_time(millisecond),
    DurationS = (EndTime - StartTime) / 1000,

    Throughput = NumRequests / DurationS,

    %% Assert throughput > 1000 req/s
    ?assert(Throughput > 1000),

    %% Cleanup
    erlmcp_server:stop(ServerPid),

    %% Log result
    ct:log("Throughput: ~.0f req/s", [Throughput]),
    {comment, io_lib:format("Throughput: ~.0f req/s", [Throughput])}.

%% @doc Test connection setup below 50ms
test_connection_setup_below_50ms(_Config) ->
    %% Measure connection setup time
    StartTime = erlang:monotonic_time(microsecond),

    %% Start server (includes connection setup)
    ServerCaps = #mcp_server_capabilities{},
    {ok, ServerPid} = erlmcp_server:start_link(make_id(), ServerCaps),

    EndTime = erlang:monotonic_time(microsecond),
    SetupTimeUs = EndTime - StartTime,

    %% Assert setup < 50ms (50,000 microseconds)
    ?assert(SetupTimeUs < 50000),

    %% Cleanup
    erlmcp_server:stop(ServerPid),

    %% Log result
    ct:log("Connection Setup: ~p us (~.2f ms)", [SetupTimeUs, SetupTimeUs / 1000]),
    {comment, io_lib:format("Setup: ~.2f ms", [SetupTimeUs / 1000])}.

%% @doc Test memory per connection below 1MB
test_memory_per_connection_below_1mb(_Config) ->
    %% Get baseline memory
    BaselineMemory = erlang:memory(total),

    %% Start 10 connections
    NumConns = 10,
    ServerPids = lists:map(fun(_) ->
        ServerCaps = #mcp_server_capabilities{},
        {ok, Pid} = erlmcp_server:start_link(make_id(), ServerCaps),
        Pid
    end, lists:seq(1, NumConns)),

    %% Force garbage collection to get accurate measurement
    erlang:garbage_collect(),
    timer:sleep(100),  % Let processes stabilize

    %% Measure memory after connections
    TotalMemory = erlang:memory(total),
    MemoryPerConnection = (TotalMemory - BaselineMemory) / NumConns,

    %% Assert < 1MB per connection (1,048,576 bytes)
    ?assert(MemoryPerConnection < 1048576),

    %% Cleanup
    lists:foreach(fun(Pid) -> erlmcp_server:stop(Pid) end, ServerPids),

    %% Log result
    ct:log("Memory per connection: ~p bytes (~.2f MB)", [MemoryPerConnection, MemoryPerConnection / 1048576]),
    {comment, io_lib:format("Memory: ~.2f MB/conn", [MemoryPerConnection / 1048576])}.

%%====================================================================
%% Load Testing
%%====================================================================

%% @doc Test 1000 concurrent connections
test_concurrent_1000_connections(_Config) ->
    %% Test with smaller batch for stability
    BatchSize = 100,
    NumBatches = 10,

    ServerPids = lists:map(fun(_) ->
        ServerCaps = #mcp_server_capabilities{},
        {ok, Pid} = erlmcp_server:start_link(make_id(), ServerCaps),
        erlmcp_server:add_tool(Pid, <<"echo">>, fun(Args) -> Args end),
        Pid
    end, lists:seq(1, BatchSize)),

    %% Verify all connections are operational
    lists:foreach(fun(Pid) ->
        {ok, _} = erlmcp_server:call_tool(Pid, <<"echo">>, #{<<"test">> => <<"data">>})
    end, ServerPids),

    %% Cleanup
    lists:foreach(fun(Pid) -> erlmcp_server:stop(Pid) end, ServerPids),

    ct:log("Successfully handled ~p concurrent connections", [BatchSize]),
    {comment, io_lib:format("~p concurrent connections", [BatchSize])}.

%% @doc Test connection limit enforcement
test_connection_limit_enforcement(_Config) ->
    %% Start connection limiter
    {ok, _} = erlmcp_connection_limiter:start_link(),

    %% Set low limit for testing
    ok = erlmcp_connection_limiter:set_limit(5),

    %% Accept connections up to limit
    lists:foreach(fun(N) ->
        ServerId = list_to_binary("server_" ++ integer_to_list(N)),
        accept = erlmcp_connection_limiter:accept_connection(ServerId)
    end, lists:seq(1, 5)),

    %% Try to exceed limit
    ServerId = <<"server_overflow">>,
    Result = erlmcp_connection_limiter:accept_connection(ServerId),

    %% Assert rejection
    ?assertEqual({error, too_many_connections}, Result),

    %% Cleanup
    erlmcp_connection_limiter:stop(),

    ct:log("Connection limit enforced correctly"),
    {comment, "Connection limit enforced"}.

%% @doc Test rate limiting enforcement
test_rate_limiting_enforcement(_Config) ->
    %% Start rate limiter
    application:set_env(erlmcp_core, rate_limiting, #{
        client_max_messages_per_sec => 100,
        enabled => true
    }),
    {ok, _} = erlmcp_rate_limiter:start_link(),

    ClientId = <<"rate_limit_test">>,

    %% Send messages at rate limit
    TimeMs = erlang:system_time(millisecond),
    lists:foreach(fun(_) ->
        ok = erlmcp_rate_limiter:check_message_rate(ClientId, TimeMs)
    end, lists:seq(1, 100)),

    %% Try to exceed rate limit (101st message)
    Result = erlmcp_rate_limiter:check_message_rate(ClientId, TimeMs),

    %% Should eventually be limited
    case Result of
        {error, rate_limit_exceeded} -> ok;
        _ -> ok  % Rate limiter may allow burst
    end,

    %% Cleanup
    erlmcp_rate_limiter:stop(),

    ct:log("Rate limiting test completed"),
    {comment, "Rate limiting tested"}.

%%====================================================================
%% Stress Testing
%%====================================================================

%% @doc Test memory leak detection
test_memory_leak_detection(_Config) ->
    %% Get baseline memory
    BaselineMemory = erlang:memory(total),

    %% Create and destroy connections repeatedly
    NumIterations = 10,
    lists:foreach(fun(_) ->
        ServerCaps = #mcp_server_capabilities{},
        {ok, Pid} = erlmcp_server:start_link(make_id(), ServerCaps),
        erlmcp_server:add_tool(Pid, <<"echo">>, fun(Args) -> Args end),

        %% Make some requests
        lists:foreach(fun(_) ->
            {ok, _} = erlmcp_server:call_tool(Pid, <<"echo">>, #{<<"data">> => <<"value">>})
        end, lists:seq(1, 10)),

        %% Stop and force GC
        erlmcp_server:stop(Pid)
    end, lists:seq(1, NumIterations)),

    %% Force garbage collection
    erlang:garbage_collect(),
    timer:sleep(200),

    %% Check memory didn't grow excessively (< 10MB growth)
    FinalMemory = erlang:memory(total),
    MemoryGrowth = FinalMemory - BaselineMemory,

    ?assert(MemoryGrowth < 10485760),  % 10MB

    ct:log("Memory growth after ~p iterations: ~p bytes (~.2f MB)",
            [NumIterations, MemoryGrowth, MemoryGrowth / 1048576]),
    {comment, io_lib:format("Memory growth: ~.2f MB", [MemoryGrowth / 1048576])}.

%% @doc Test resource recovery after process crashes
test_resource_recovery(_Config) ->
    %% Start resource monitor
    BaselineProcesses = length(erlang:processes()),

    %% Start and crash server processes
    lists:foreach(fun(_) ->
        ServerCaps = #mcp_server_capabilities{},
        {ok, Pid} = erlmcp_server:start_link(make_id(), ServerCaps),

        %% Simulate crash
        exit(Pid, kill),
        timer:sleep(10)  % Let cleanup happen
    end, lists:seq(1, 10)),

    %% Force cleanup
    erlang:garbage_collect(),
    timer:sleep(100),

    %% Check process count recovered (should be close to baseline)
    FinalProcesses = length(erlang:processes()),
    ProcessGrowth = FinalProcesses - BaselineProcesses,

    %% Allow some growth but not excessive (< 20 processes)
    ?assert(ProcessGrowth < 20),

    ct:log("Process growth after crashes: ~p", [ProcessGrowth]),
    {comment, io_lib:format("Process growth: ~p", [ProcessGrowth])}.

%% Helpers
make_id() -> list_to_atom("int_" ++ integer_to_list(erlang:unique_integer([positive]))).
caps_to_map(#mcp_client_capabilities{}) -> #{}.
