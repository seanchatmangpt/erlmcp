%%%====================================================================
%%% test_network_real_bench.erl - Quick integration test
%%%====================================================================
%%% Simple script to verify the consolidated benchmark works
%%%====================================================================

-module(test_network_real_bench).

-export([run/0, test_tcp/0, test_http/0, test_workload_list/0]).

run() ->
    io:format("~n========================================~n"),
    io:format("NETWORK BENCHMARK INTEGRATION TEST~n"),
    io:format("========================================~n~n"),

    %% Start applications
    application:ensure_all_started(erlmcp),
    application:ensure_all_started(ranch),
    application:ensure_all_started(gun),
    application:ensure_all_started(cowboy),

    %% Test workload listing
    test_workload_list(),

    %% Test minimal TCP workload
    test_tcp(),

    %% Test minimal HTTP workload
    test_http(),

    io:format("~n========================================~n"),
    io:format("ALL TESTS PASSED~n"),
    io:format("========================================~n~n"),

    halt(0).

test_workload_list() ->
    io:format("Test: List workloads...~n"),
    Workloads = erlmcp_bench_network_real:list_workloads(),
    io:format("  Found ~B workloads~n", [length(Workloads)]),

    TcpCount = length([Name || {Name, _, tcp} <- Workloads]),
    HttpCount = length([Name || {Name, _, http} <- Workloads]),

    io:format("  TCP workloads: ~B~n", [TcpCount]),
    io:format("  HTTP workloads: ~B~n", [HttpCount]),

    case length(Workloads) >= 7 of
        true ->
            io:format("  PASS~n~n");
        false ->
            io:format("  FAIL: Expected at least 7 workloads~n~n"),
            halt(1)
    end.

test_tcp() ->
    io:format("Test: TCP minimal workload...~n"),

    Result = erlmcp_bench_network_real:run_workload(tcp_burst_100_1kib, #{
        connections => 5,
        duration_s => 3,
        payload_size_bytes => 256
    }),

    case Result of
        {ok, Metrics} ->
            %% Extract key metrics (adjust field positions based on record)
            Throughput = element(9, Metrics),
            MessagesSent = element(8, Metrics),

            io:format("  Messages sent: ~B~n", [MessagesSent]),
            io:format("  Throughput: ~.2f msg/s~n", [Throughput]),

            case MessagesSent > 0 andalso Throughput > 0.0 of
                true ->
                    io:format("  PASS~n~n");
                false ->
                    io:format("  FAIL: No messages sent or zero throughput~n~n"),
                    halt(1)
            end;

        {error, Reason} ->
            io:format("  FAIL: ~p~n~n", [Reason]),
            halt(1)
    end.

test_http() ->
    io:format("Test: HTTP minimal workload...~n"),

    Result = erlmcp_bench_network_real:run_workload(http_burst_100_1kib, #{
        connections => 3,
        duration_s => 3,
        payload_size_bytes => 256
    }),

    case Result of
        {ok, Metrics} ->
            %% Extract key metrics
            Throughput = element(9, Metrics),
            MessagesSent = element(8, Metrics),
            Protocol = element(22, Metrics),

            io:format("  Messages sent: ~B~n", [MessagesSent]),
            io:format("  Throughput: ~.2f msg/s~n", [Throughput]),
            io:format("  Protocol: ~s~n", [Protocol]),

            case MessagesSent > 0 andalso Throughput > 0.0 of
                true ->
                    io:format("  PASS~n~n");
                false ->
                    io:format("  FAIL: No messages sent or zero throughput~n~n"),
                    halt(1)
            end;

        {error, Reason} ->
            io:format("  FAIL: ~p~n~n", [Reason]),
            halt(1)
    end.
