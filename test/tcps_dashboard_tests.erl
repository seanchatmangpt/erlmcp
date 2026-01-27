%%%-------------------------------------------------------------------
%%% @doc TCPS Dashboard Test Suite
%%%
%%% Comprehensive tests for dashboard backend functionality including:
%%% - Metrics aggregation
%%% - SSE streaming
%%% - WebSocket communication
%%% - Cache performance
%%% - Load testing
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_dashboard_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

dashboard_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Metrics Aggregator Tests", fun test_metrics_aggregator/0},
         {"Metrics Cache Tests", fun test_metrics_cache/0},
         {"SSE Manager Tests", fun test_sse_manager/0},
         {"Dashboard API Tests", fun test_dashboard_api/0},
         {"Real-time Broadcasting Tests", fun test_real_time_broadcasting/0},
         {"Load Test - 100 Concurrent SSE Clients", {timeout, 60, fun test_load_sse_clients/0}}
     ]}.

setup() ->
    %% Start required services
    application:ensure_all_started(ranch),
    application:ensure_all_started(cowboy),

    %% Initialize cache
    tcps_metrics_cache:init_cache(),

    %% Start metrics aggregator
    {ok, AggPid} = tcps_metrics_aggregator:start_link(),

    %% Start SSE manager
    {ok, SsePid} = tcps_sse_manager:start_link(),

    %% Start dashboard
    Config = #{port => 8888, refresh_interval => 1000},
    {ok, DashPid} = tcps_dashboard:start_link(Config),

    #{
        aggregator => AggPid,
        sse_manager => SsePid,
        dashboard => DashPid
    }.

cleanup(#{aggregator := AggPid, sse_manager := SsePid, dashboard := DashPid}) ->
    catch tcps_dashboard:stop_dashboard(),
    catch exit(DashPid, shutdown),
    catch exit(SsePid, shutdown),
    catch exit(AggPid, shutdown),

    %% Clean up cache
    tcps_metrics_cache:destroy_cache(),

    ok.

%%%===================================================================
%%% Metrics Aggregator Tests
%%%===================================================================

test_metrics_aggregator() ->
    %% Test overview metrics
    Overview = tcps_metrics_aggregator:get_overview_metrics(),
    ?assertMatch(#{total_work_orders := _}, Overview),
    ?assertMatch(#{active_work_orders := _}, Overview),
    ?assertMatch(#{open_andons := _}, Overview),

    %% Test quality metrics
    Quality = tcps_metrics_aggregator:get_quality_metrics(),
    ?assertMatch(#{test_pass_rate := _}, Quality),
    ?assertMatch(#{code_coverage := _}, Quality),

    %% Test Kanban metrics
    Kanban = tcps_metrics_aggregator:get_kanban_metrics(),
    ?assertMatch(#{timestamp := _}, Kanban),

    %% Test Andon metrics
    Andon = tcps_metrics_aggregator:get_andon_metrics(),
    ?assertMatch(#{total_open := _}, Andon),
    ?assertMatch(#{by_severity := _}, Andon),

    %% Test Kaizen metrics
    Kaizen = tcps_metrics_aggregator:get_kaizen_metrics(),
    ?assertMatch(#{week_over_week := _}, Kaizen),
    ?assertMatch(#{top_waste_points := _}, Kaizen),

    %% Test flow metrics
    Flow = tcps_metrics_aggregator:get_flow_metrics(),
    ?assertMatch(#{throughput_rate := _}, Flow),
    ?assertMatch(#{cycle_time_distribution := _}, Flow),

    %% Test get all metrics
    AllMetrics = tcps_metrics_aggregator:get_all_metrics(),
    ?assertMatch(#{overview := _, quality := _, kanban := _}, AllMetrics),

    ok.

%%%===================================================================
%%% Metrics Cache Tests
%%%===================================================================

test_metrics_cache() ->
    %% Test cache initialization
    ok = tcps_metrics_cache:init_cache(),

    %% Test cache write
    TestData = #{value => 42, timestamp => erlang:timestamp()},
    ok = tcps_metrics_cache:update_cache(test_key, TestData),

    %% Test cache read (should succeed immediately)
    {ok, CachedData} = tcps_metrics_cache:get_cached(test_key),
    ?assertEqual(TestData, CachedData),

    %% Test cache miss
    {error, not_found} = tcps_metrics_cache:get_cached(nonexistent_key),

    %% Test cache invalidation
    ok = tcps_metrics_cache:invalidate(test_key),
    {error, not_found} = tcps_metrics_cache:get_cached(test_key),

    %% Test cache stats
    Stats = tcps_metrics_cache:get_stats(),
    ?assertMatch(#{total_entries := _, ttl_seconds := _}, Stats),

    ok.

%%%===================================================================
%%% SSE Manager Tests
%%%===================================================================

test_sse_manager() ->
    %% Test client registration
    ClientPid = spawn(fun() ->
        receive
            {sse_update, _Update} -> ok;
            stop -> ok
        after 10000 -> timeout
        end
    end),

    ok = tcps_sse_manager:register_client(ClientPid),

    %% Test client count
    Count1 = tcps_sse_manager:get_client_count(),
    ?assert(Count1 >= 1),

    %% Test broadcast to all clients
    TestUpdate = #{
        type => test_broadcast,
        data => #{message => <<"Hello SSE">>},
        timestamp => erlang:timestamp()
    },
    ok = tcps_sse_manager:broadcast_update(TestUpdate),

    %% Wait for message
    timer:sleep(100),

    %% Test broadcast to specific client
    SpecificUpdate = #{
        type => test_specific,
        data => #{message => <<"Direct message">>}
    },
    ok = tcps_sse_manager:broadcast_to_client(ClientPid, SpecificUpdate),

    %% Test connection stats
    Stats = tcps_sse_manager:get_connection_stats(),
    ?assertMatch(#{total_clients := _, broadcast_count := _}, Stats),

    %% Test client unregistration
    ok = tcps_sse_manager:unregister_client(ClientPid),
    ClientPid ! stop,

    timer:sleep(100),
    Count2 = tcps_sse_manager:get_client_count(),
    ?assertEqual(Count1 - 1, Count2),

    ok.

%%%===================================================================
%%% Dashboard API Tests
%%%===================================================================

test_dashboard_api() ->
    %% Test get metrics summary
    Summary = tcps_dashboard:get_metrics_summary(),
    ?assert(is_map(Summary)),

    %% Test export to JSON
    JsonExport = tcps_dashboard:export_dashboard_data(json),
    ?assert(is_binary(JsonExport)),

    %% Test export to CSV
    CsvExport = tcps_dashboard:export_dashboard_data(csv),
    ?assert(is_binary(CsvExport)),

    %% Test weekly report generation
    Report = tcps_dashboard:generate_weekly_report(),
    ?assert(is_binary(Report)),
    ?assert(byte_size(Report) > 0),

    ok.

%%%===================================================================
%%% Real-time Broadcasting Tests
%%%===================================================================

test_real_time_broadcasting() ->
    %% Create test client
    TestClient = spawn(fun client_loop/0),
    ok = tcps_sse_manager:register_client(TestClient),

    %% Force metrics refresh (should trigger broadcast)
    ok = tcps_metrics_aggregator:force_refresh(),

    %% Wait for broadcast
    timer:sleep(200),

    %% Verify client received update
    TestClient ! {self(), get_updates},
    receive
        {updates, Updates} ->
            ?assert(length(Updates) > 0)
    after 1000 ->
        ?assert(false)  %% Timeout
    end,

    %% Cleanup
    ok = tcps_sse_manager:unregister_client(TestClient),
    TestClient ! stop,

    ok.

client_loop() ->
    client_loop([]).

client_loop(Updates) ->
    receive
        {sse_update, Update} ->
            client_loop([Update | Updates]);
        {From, get_updates} ->
            From ! {updates, lists:reverse(Updates)},
            client_loop(Updates);
        stop ->
            ok
    end.

%%%===================================================================
%%% Load Tests
%%%===================================================================

test_load_sse_clients() ->
    io:format("Starting load test with 100 concurrent SSE clients...~n"),

    %% Spawn 100 concurrent SSE clients
    Clients = lists:map(fun(N) ->
        ClientPid = spawn(fun() ->
            receive
                stop -> ok
            after 10000 -> timeout
            end
        end),
        ok = tcps_sse_manager:register_client(ClientPid),
        {N, ClientPid}
    end, lists:seq(1, 100)),

    %% Verify all clients registered
    ClientCount = tcps_sse_manager:get_client_count(),
    ?assert(ClientCount >= 100),
    io:format("~p clients registered~n", [ClientCount]),

    %% Broadcast 10 updates
    StartTime = erlang:monotonic_time(millisecond),
    lists:foreach(fun(I) ->
        Update = #{
            type => load_test,
            iteration => I,
            data => #{value => rand:uniform(1000)}
        },
        ok = tcps_sse_manager:broadcast_update(Update)
    end, lists:seq(1, 10)),

    EndTime = erlang:monotonic_time(millisecond),
    BroadcastTime = EndTime - StartTime,

    io:format("Broadcast 10 updates to 100 clients in ~pms~n", [BroadcastTime]),

    %% Verify broadcast performance (<500ms for 10 broadcasts to 100 clients)
    ?assert(BroadcastTime < 500),

    %% Get connection stats
    Stats = tcps_sse_manager:get_connection_stats(),
    io:format("Connection stats: ~p~n", [Stats]),

    %% Cleanup all clients
    lists:foreach(fun({_N, ClientPid}) ->
        ok = tcps_sse_manager:unregister_client(ClientPid),
        ClientPid ! stop
    end, Clients),

    timer:sleep(200),
    FinalCount = tcps_sse_manager:get_client_count(),
    io:format("Final client count after cleanup: ~p~n", [FinalCount]),

    ok.

%%%===================================================================
%%% Performance Benchmarks
%%%===================================================================

benchmark_metrics_collection_test() ->
    %% Benchmark metrics collection time
    Iterations = 100,

    StartTime = erlang:monotonic_time(microsecond),
    lists:foreach(fun(_) ->
        _AllMetrics = tcps_metrics_aggregator:get_all_metrics()
    end, lists:seq(1, Iterations)),
    EndTime = erlang:monotonic_time(microsecond),

    TotalTime = EndTime - StartTime,
    AvgTime = TotalTime / Iterations,

    io:format("Metrics collection benchmark:~n"),
    io:format("  Total time: ~pμs (~.2fms)~n", [TotalTime, TotalTime / 1000]),
    io:format("  Average per collection: ~.2fμs (~.2fms)~n", [AvgTime, AvgTime / 1000]),
    io:format("  Collections per second: ~.2f~n", [1000000 / AvgTime]),

    %% Verify performance (<10ms average)
    ?assert(AvgTime < 10000),

    ok.

benchmark_cache_operations_test() ->
    %% Benchmark cache read/write performance
    Iterations = 1000,

    %% Write benchmark
    WriteStartTime = erlang:monotonic_time(microsecond),
    lists:foreach(fun(I) ->
        Key = list_to_atom("benchmark_key_" ++ integer_to_list(I)),
        Value = #{iteration => I, data => rand:uniform(1000)},
        tcps_metrics_cache:update_cache(Key, Value)
    end, lists:seq(1, Iterations)),
    WriteEndTime = erlang:monotonic_time(microsecond),

    WriteTotalTime = WriteEndTime - WriteStartTime,
    WriteAvgTime = WriteTotalTime / Iterations,

    %% Read benchmark
    ReadStartTime = erlang:monotonic_time(microsecond),
    lists:foreach(fun(I) ->
        Key = list_to_atom("benchmark_key_" ++ integer_to_list(I)),
        {ok, _Value} = tcps_metrics_cache:get_cached(Key)
    end, lists:seq(1, Iterations)),
    ReadEndTime = erlang:monotonic_time(microsecond),

    ReadTotalTime = ReadEndTime - ReadStartTime,
    ReadAvgTime = ReadTotalTime / Iterations,

    io:format("Cache operations benchmark:~n"),
    io:format("  Write operations (~p):~n", [Iterations]),
    io:format("    Total time: ~pμs (~.2fms)~n", [WriteTotalTime, WriteTotalTime / 1000]),
    io:format("    Average per write: ~.2fμs~n", [WriteAvgTime]),
    io:format("  Read operations (~p):~n", [Iterations]),
    io:format("    Total time: ~pμs (~.2fms)~n", [ReadTotalTime, ReadTotalTime / 1000]),
    io:format("    Average per read: ~.2fμs~n", [ReadAvgTime]),

    %% Verify performance (<100μs average for both read and write)
    ?assert(WriteAvgTime < 100),
    ?assert(ReadAvgTime < 100),

    ok.
