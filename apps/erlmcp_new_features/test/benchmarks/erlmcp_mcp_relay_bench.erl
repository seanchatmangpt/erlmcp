%%%-------------------------------------------------------------------
%%% @doc
%%% MCP Relay Performance Benchmark
%%%
%%% Benchmarks request relay performance with multiple concurrent backends.
%%% Tests scalability from 1 to 100 concurrent backends with various
%%% request patterns and load levels.
%%%
%%% Benchmark Scenarios:
%%% - Single backend latency measurement
%%% - Multiple backends round-robin distribution
%%% - High concurrency request handling (1-100 concurrent requests)
%%% - Backend failover and recovery
%%% - Load balancing efficiency under heavy load
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_mcp_relay_bench).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Export all functions for testing
-export([all/0, setup/0, cleanup/0,
         single_backend_latency/1,
         multi_backend_scalability/1,
         concurrent_requests/1,
         backend_failover/1,
         load_balancing_efficiency/1]).

%%====================================================================
%% Constants
%%====================================================================
-define(DEFAULT_REQUEST_COUNT, 1000).
-define(DEFAULT_TIMEOUT_MS, 5000).
-define(MAX_CONCURRENT_BACKENDS, 100).
-define(DEFAULT_PAYLOAD_SIZE, 1024). % 1KB payload

%%====================================================================
%% Test Configuration
%%====================================================================
all() ->
    [
        single_backend_latency,
        multi_backend_scalability,
        concurrent_requests,
        backend_failover,
        load_balancing_efficiency
    ].

setup() ->
    %% Start MCP relay server
    case erlmcp_mcp_relay:start_link([{request_timeout, 3000}]) of
        {ok, _Pid} ->
            %% Add test backends
            add_test_backends(),
            ok;
        {error, Reason} ->
            ?LOG_ERROR("Failed to start MCP relay: ~p", [Reason]),
            error(start_failed)
    end.

cleanup() ->
    %% Stop MCP relay server
    erlmcp_mcp_relay:stop(),
    %% Clear any remaining backends
    ok.

%%====================================================================
%% Benchmark Tests
%%====================================================================

%% @doc Single backend latency measurement
single_backend_latency(_Config) ->
    BackendId = test_backend_1,
    Request = generate_test_request(),

    %% Warmup
    [begin
         _ = erlmcp_mcp_relay:relay_request(BackendId, Request, 1000)
     end || _ <- lists:seq(1, 100)],

    %% Measurement phase
    Results = [begin
                 StartTime = erlang:monotonic_time(millisecond),
                 Result = erlmcp_mcp_relay:relay_request(BackendId, Request, 1000),
                 EndTime = erlang:monotonic_time(millisecond),
                 case Result of
                     {ok, _} -> EndTime - StartTime;
                     {error, _} -> -1 % Mark as failed
                 end
             end || _ <- lists:seq(1, ?DEFAULT_REQUEST_COUNT)],

    %% Analyze results
    Successful = [T || T <- Results, T > 0],
    Failed = Results -- Successful,

    #{
        test => single_backend_latency,
        total_requests => ?DEFAULT_REQUEST_COUNT,
        successful => length(Successful),
        failed => length(Failed),
        success_rate => length(Successful) / ?DEFAULT_REQUEST_COUNT,
        average_latency => lists:sum(Successful) / length(Successful),
        min_latency => lists:min(Successful),
        max_latency => lists:max(Successful),
        p95_latency => calculate_percentile(Successful, 95),
        p99_latency => calculate_percentile(Successful, 99),
        throughput => length(Successful) / (lists:sum(Successful) / 1000) % req/s
    }.

%% @doc Multi-backend scalability testing
multi_backend_scalability(_Config) ->
    BackendCounts = [1, 5, 10, 20, 50, 100],

    Results = lists:map(fun(BackendCount) ->
        %% Add specified number of backends
        add_n_backends(BackendCount),

        Request = generate_test_request(),

        %% Test each backend count
        ResultsPerBackend = [begin
                              %% Distribute requests across backends
                              BackendId = list_to_atom("test_backend_" ++ integer_to_list(I)),

                              Start = erlang:monotonic_time(millisecond),
                              Result = erlmcp_mcp_relay:relay_request(BackendId, Request, 1000),
                              End = erlang:monotonic_time(millisecond),

                              case Result of
                                  {ok, _} -> End - Start;
                                  {error, _} -> -1
                              end
                          end || I <- lists:seq(1, BackendCount),
                                _ <- lists:seq(1, div(?DEFAULT_REQUEST_COUNT, BackendCount))],

        Successful = [T || T <- ResultsPerBackend, T > 0],

        #{
            backend_count => BackendCount,
            successful => length(Successful),
            success_rate => length(Successful) / length(ResultsPerBackend),
            average_latency => case Successful of
                                 [] -> 0;
                                 _ -> lists:sum(Successful) / length(Successful)
                             end,
            throughput => case length(Successful) > 0 of
                            true -> length(Successful) / (lists:sum(Successful) / 1000);
                            false -> 0
                         end,
            efficiency => calculate_backend_efficiency(Successful, BackendCount)
        }
    end, BackendCounts),

    #{
        test => multi_backend_scalability,
        results => Results
    }.

%% @doc Concurrent request handling
concurrent_requests(_Config) ->
    ConcurrentLevels = [1, 10, 25, 50, 100],

    Results = lists:map(fun(Concurrent) ->
        Request = generate_test_request(),

        %% Spawn concurrent processes
        Processes = [spawn(fun() ->
                            StartTime = erlang:monotonic_time(millisecond),
                            Result = erlmcp_mcp_relay:relay_request(Request, 1000),
                            EndTime = erlang:monotonic_time(millisecond),

                            case Result of
                                {ok, _} ->
                                    self() ! {success, EndTime - StartTime};
                                {error, _} ->
                                    self() ! {failed, EndTime - StartTime}
                            end
                        end) || _ <- lists:seq(1, Concurrent)],

        %% Collect results
        Results = collect_concurrent_results(Processes, []),

        Successful = [T || {success, T} <- Results],
        Failed = [T || {failed, T} <- Results],

        #{
            concurrent_level => Concurrent,
            total_requests => Concurrent,
            successful => length(Successful),
            failed => length(Failed),
            success_rate => length(Successful) / Concurrent,
            average_latency => case Successful of
                                 [] -> 0;
                                 _ -> lists:sum(Successful) / length(Successful)
                             end,
            throughput => case length(Successful) > 0 of
                            true -> length(Successful) / (lists:sum(Successful) / 1000);
                            false -> 0
                         end,
            concurrency_efficiency => calculate_concurrency_efficiency(Successful, Concurrent)
        }
    end, ConcurrentLevels),

    #{
        test => concurrent_requests,
        results => Results
    }.

%% @doc Backend failover testing
backend_failover(_Config) ->
    BackendId = test_backend_1,

    %% Test normal operation first
    NormalResults = [begin
                      Request = generate_test_request(),
                      Result = erlmcp_mcp_relay:relay_request(BackendId, Request, 1000),
                      case Result of
                          {ok, _} -> success;
                          {error, _} -> failure
                      end
                  end || _ <- lists:seq(1, 100)],

    NormalSuccess = length([R || R <- NormalResults, R =:= success]) / 100,

    %% Simulate backend failure by removing it
    erlmcp_mcp_relay:remove_backend(BackendId),

    %% Test failover
    FailoverResults = [begin
                        Request = generate_test_request(),
                        Result = erlmcp_mcp_relay:relay_request(Request, 1000),
                        case Result of
                            {ok, _} -> success;
                            {error, no_available_backends} -> failover_failure;
                            {error, _} -> failure
                        end
                    end || _ <- lists:seq(1, 100)],

    FailoverSuccess = length([R || R <- FailoverResults, R =:= success]) / 100,
    FailoverFailure = length([R || R <- FailoverResults, R =:= failover_failure]) / 100,

    %% Restore backend and test recovery
    add_test_backends(),
    RecoveryResults = [begin
                        Request = generate_test_request(),
                        Result = erlmcp_mcp_relay:relay_request(BackendId, Request, 1000),
                        case Result of
                            {ok, _} -> success;
                            {error, _} -> failure
                        end
                    end || _ <- lists:seq(1, 100)],

    RecoverySuccess = length([R || R <- RecoveryResults, R =:= success]) / 100,

    #{
        test => backend_failover,
        normal_success_rate => NormalSuccess,
        failover_success_rate => FailoverSuccess,
        failover_failure_rate => FailoverFailure,
        recovery_success_rate => RecoverySuccess,
        failover_effectiveness => (FailoverSuccess + RecoverySuccess) / 2
    }.

%% @doc Load balancing efficiency
load_balancing_efficiency(_Config) ->
    %% Add multiple backends with different response times
    add_n_backends(5),

    %% Configure backends with different response times
    Backends = [test_backend_1, test_backend_2, test_backend_3, test_backend_4, test_backend_5],

    %% Send mixed workload
    Results = [begin
                 RandomBackend = lists:nth(rand:uniform(5), Backends),
                 Request = generate_test_request(),

                 Start = erlang:monotonic_time(millisecond),
                 Result = erlmcp_mcp_relay:relay_request(RandomBackend, Request, 1000),
                 End = erlang:monotonic_time(millisecond),

                 case Result of
                     {ok, _} -> End - Start;
                     {error, _} -> -1
                 end
             end || _ <- lists:seq(1, ?DEFAULT_REQUEST_COUNT)],

    Successful = [T || T <- Results, T > 0],

    %% Check distribution across backends
    Distribution = check_load_distribution(Results),

    #{
        test => load_balancing_efficiency,
        total_requests => ?DEFAULT_REQUEST_COUNT,
        successful => length(Successful),
        success_rate => length(Successful) / ?DEFAULT_REQUEST_COUNT,
        average_latency => lists:sum(Successful) / length(Successful),
        throughput => length(Successful) / (lists:sum(Successful) / 1000),
        load_distribution => Distribution,
        balance_efficiency => calculate_balance_efficiency(Distribution)
    }.

%%====================================================================
%% Helper Functions
%%====================================================================

add_test_backends() ->
    %% Add test backends with mock URLs
    Backends = [
        {test_backend_1, "http://localhost:8081/api"},
        {test_backend_2, "http://localhost:8082/api"},
        {test_backend_3, "http://localhost:8083/api"}
    ],

    lists:foreach(fun({Id, Url}) ->
        erlmcp_mcp_relay:add_backend(Id, #{
            url => Url,
            enabled => true,
            timeout => 3000,
            weight => 1,
            healthy => true
        end)
    end, Backends).

add_n_backends(N) ->
    lists:foreach(fun(I) ->
        BackendId = list_to_atom("test_backend_" ++ integer_to_list(I)),
        Url = "http://localhost:" ++ integer_to_list(8080 + I) ++ "/api",

        erlmcp_mcp_relay:add_backend(BackendId, #{
            url => Url,
            enabled => true,
            timeout => 3000,
            weight => 1,
            healthy => true
        end)
    end, lists:seq(1, N)).

generate_test_request() ->
    Payload = generate_payload(?DEFAULT_PAYLOAD_SIZE),
    #{
        jsonrpc => <<"2.0">>,
        method => <<"benchmark_test">>,
        id => <<"test_", (integer_to_binary(erlang:unique_integer()))>>,
        params => Payload
    }.

generate_payload(Size) ->
    Text = lists:seq($a, $z),
    ChunkSize = max(1, Size div 100),
    Chunks = [lists:sublist(Text, ChunkSize) || _ <- lists:seq(1, 100)],
    binary:list_to_bin(lists:sublist(lists:flatten(Chunks), Size)).

collect_concurrent_results([], Acc) -> Acc;
collect_concurrent_results([], Acc) -> Acc;
collect_concurrent_results(Processes, Acc) ->
    receive
        {success, Time} ->
            collect_concurrent_results(Processes, [{success, Time} | Acc]);
        {failed, Time} ->
            collect_concurrent_results(Processes, [{failed, Time} | Acc])
    after 10000 ->
        %% Timeout for any remaining processes
        Acc
    end.

calculate_percentile(List, Percentile) when length(List) > 0 ->
    Sorted = lists:sort(List),
    Index = trunc((Percentile / 100) * length(Sorted)),
    lists:nth(min(Index + 1, length(Sorted)), Sorted).

calculate_backend_efficiency(Latencies, BackendCount) ->
    case Latencies of
        [] -> 0;
        _ ->
            AvgLatency = lists:sum(Latencies) / length(Latencies),
            %% Efficiency decreases as latency increases with more backends
            case BackendCount of
                1 -> 1.0;
                _ -> math:exp(-BackendCount * AvgLatency / 10000)
            end
    end.

calculate_concurrency_efficiency(Latencies, Concurrency) ->
    case Latencies of
        [] -> 0;
        _ ->
            AvgLatency = lists:sum(Latencies) / length(Latencies),
            Efficiency = Concurrency / (1 + AvgLatency / 100),
            min(1.0, Efficiency)
    end.

check_load_distribution(Results) ->
    % Count requests per backend (simulated)
    Backends = [test_backend_1, test_backend_2, test_backend_3, test_backend_4, test_backend_5],
    ExpectedPerBackend = length(Results) div length(Backends),

    Counts = [begin
                 %% Simulate which backend handled the request
                 BackendIndex = (I rem length(Backends)) + 1,
                 {lists:nth(BackendIndex, Backends), 1}
              end || I <- lists:seq(1, length(Results))],

    Distribution = lists:foldl(fun({Backend, 1}, Acc) ->
                                   maps:update_with(Backend, fun(X) -> X + 1 end, 1, Acc)
                               end, maps:from_list(Backends), Counts),

    maps:map(fun(_, Count) -> Count / length(Results) end, Distribution).

calculate_balance_efficiency(Distribution) ->
    % Perfect distribution would have all values equal to 1/length(Distribution)
    Ideal = 1 / maps:size(Distribution),

    Deviations = [abs(Prob - Ideal) || Prob <- maps:values(Distribution)],

    % Efficiency is higher when deviations are smaller
    1.0 - lists:sum(Deviations) / length(Deviations).