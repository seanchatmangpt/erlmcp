%%%-------------------------------------------------------------------
%%% @doc
%%% lock_free_metrics_demo - Lock-Free Metrics Demo
%%%
%%% Demonstrates usage of erlmcp_counters, erlmcp_flags, and
%%% erlmcp_prometheus_exporter for high-performance metrics.
%%%
%%% Run:
%%% ```
%%% erl -pa _build/default/lib/*/ebin
%%% 1> c(lock_free_metrics_demo).
%%% 2> lock_free_metrics_demo:run().
%%% 3> lock_free_metrics_demo:run_concurrent_benchmark().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(lock_free_metrics_demo).

-export([
    run/0,
    run_concurrent_benchmark/0,
    run_prometheus_export/0,
    simulate_mcp_server/0
]).

%%====================================================================
%% Demo Functions
%%====================================================================

%% @doc Run basic metrics demo
run() ->
    io:format("~n=== Lock-Free Metrics Demo ===~n~n"),

    % Initialize
    io:format("1. Initializing counters and flags...~n"),
    erlmcp_counters:init(),
    erlmcp_flags:init(),
    io:format("   ✓ Initialized~n~n"),

    % Demo counters
    io:format("2. Incrementing counters...~n"),
    erlmcp_counters:inc_requests(),
    erlmcp_counters:inc_requests(),
    erlmcp_counters:inc_requests(),
    erlmcp_counters:inc_success(),
    erlmcp_counters:inc_success(),
    erlmcp_counters:inc_error(),
    io:format("   ✓ Incremented 6 counters~n~n"),

    % Demo connections
    io:format("3. Tracking connections...~n"),
    erlmcp_counters:inc_connections(),
    erlmcp_counters:inc_connections(),
    erlmcp_counters:inc_connections(),
    erlmcp_counters:dec_connections(),
    io:format("   ✓ 3 connections established, 1 closed~n~n"),

    % Demo tools and resources
    io:format("4. Tracking MCP operations...~n"),
    erlmcp_counters:inc_tools_executed(),
    erlmcp_counters:inc_resources_read(),
    erlmcp_counters:inc_prompts_used(),
    erlmcp_counters:add_bytes_sent(1024),
    erlmcp_counters:add_bytes_received(2048),
    io:format("   ✓ Tracked tools, resources, and bytes~n~n"),

    % Get metrics
    io:format("5. Reading metrics...~n"),
    Metrics = erlmcp_counters:get_all(),
    io:format("   Metrics: ~p~n~n", [Metrics]),

    % Demo flags
    io:format("6. Testing system flags...~n"),
    io:format("   Is accepting: ~p~n", [erlmcp_flags:is_accepting()]),
    io:format("   Is healthy: ~p~n", [erlmcp_flags:is_healthy()]),

    erlmcp_flags:enter_maintenance_mode(),
    io:format("   Entered maintenance mode~n"),
    io:format("   Is accepting: ~p~n", [erlmcp_flags:is_accepting()]),

    erlmcp_flags:exit_maintenance_mode(),
    io:format("   Exited maintenance mode~n"),
    io:format("   Is accepting: ~p~n~n", [erlmcp_flags:is_accepting()]),

    % Demo Prometheus export
    io:format("7. Exporting to Prometheus format...~n"),
    PrometheusText = iolist_to_binary(erlmcp_counters:get_prometheus()),
    io:format("~s~n", [PrometheusText]),

    io:format("=== Demo Complete ===~n~n"),
    ok.

%% @doc Run concurrent benchmark
run_concurrent_benchmark() ->
    io:format("~n=== Concurrent Benchmark ===~n~n"),

    % Initialize
    erlmcp_counters:init(),
    erlmcp_counters:reset(),

    NumProcesses = 100,
    OpsPerProcess = 1000,
    TotalOps = NumProcesses * OpsPerProcess,

    io:format("Spawning ~p processes, ~p ops each (~p total)...~n",
              [NumProcesses, OpsPerProcess, TotalOps]),

    StartTime = erlang:system_time(microsecond),

    % Spawn workers
    Pids = [
        spawn_link(fun() ->
            [erlmcp_counters:inc_requests() || _ <- lists:seq(1, OpsPerProcess)]
        end)
        || _ <- lists:seq(1, NumProcesses)
    ],

    % Wait for completion
    [begin
        Ref = monitor(process, Pid),
        receive
            {'DOWN', Ref, process, Pid, _} -> ok
        after 10000 ->
            error(timeout)
        end
    end || Pid <- Pids],

    EndTime = erlang:system_time(microsecond),
    Duration = EndTime - StartTime,

    % Verify results
    Metrics = erlmcp_counters:get_all(),
    ActualCount = maps:get(requests_total, Metrics),

    io:format("~n"),
    io:format("Results:~n"),
    io:format("  Total operations: ~p~n", [TotalOps]),
    io:format("  Actual count: ~p~n", [ActualCount]),
    io:format("  Lost updates: ~p~n", [TotalOps - ActualCount]),
    io:format("  Duration: ~p us~n", [Duration]),
    io:format("  Throughput: ~p ops/sec~n", [(TotalOps * 1000000) div Duration]),
    io:format("  Latency: ~p ns/op~n", [(Duration * 1000) div TotalOps]),
    io:format("~n"),

    case ActualCount of
        TotalOps ->
            io:format("✓ SUCCESS: No lost updates!~n~n");
        _ ->
            io:format("✗ FAILURE: Lost ~p updates!~n~n", [TotalOps - ActualCount])
    end,

    ok.

%% @doc Run Prometheus export demo
run_prometheus_export() ->
    io:format("~n=== Prometheus Export Demo ===~n~n"),

    % Initialize
    erlmcp_counters:init(),
    erlmcp_flags:init(),

    % Simulate some activity
    io:format("Simulating MCP activity...~n"),
    simulate_mcp_activity(100),
    io:format("✓ Simulated 100 operations~n~n"),

    % Export metrics
    io:format("Exporting to Prometheus format:~n"),
    io:format("~s~n", ["-"*60]),
    PrometheusText = iolist_to_binary(erlmcp_prometheus_exporter:export_with_system_metrics()),
    io:format("~s~n", [PrometheusText]),
    io:format("~s~n~n", ["-"*60]),

    % Parse and display
    Lines = binary:split(PrometheusText, <<"\n">>, [global]),
    MetricLines = [L || L <- Lines, byte_size(L) > 0, binary:at(L, 0) =/= $#],

    io:format("Metric Summary:~n"),
    lists:foreach(fun(Line) ->
        io:format("  ~s~n", [Line])
    end, MetricLines),

    io:format("~n"),
    ok.

%% @doc Simulate MCP server with metrics
simulate_mcp_server() ->
    io:format("~n=== Simulating MCP Server ===~n~n"),

    % Initialize
    erlmcp_counters:init(),
    erlmcp_flags:init(),

    % Start accepting
    io:format("Starting MCP server...~n"),
    io:format("  Accepting connections: ~p~n", [erlmcp_flags:is_accepting()]),
    io:format("  Healthy: ~p~n~n", [erlmcp_flags:is_healthy()]),

    % Simulate requests
    io:format("Handling requests...~n"),
    lists:foreach(fun(N) ->
        % Simulate request
        erlmcp_counters:inc_requests(),

        % Simulate success/error (90% success rate)
        case rand:uniform(10) of
            10 -> erlmcp_counters:inc_error();
            _ -> erlmcp_counters:inc_success()
        end,

        % Simulate operation type
        case rand:uniform(3) of
            1 -> erlmcp_counters:inc_tools_executed();
            2 -> erlmcp_counters:inc_resources_read();
            3 -> erlmcp_counters:inc_prompts_used()
        end,

        % Simulate bytes
        erlmcp_counters:add_bytes_sent(rand:uniform(1024)),
        erlmcp_counters:add_bytes_received(rand:uniform(2048)),

        % Print progress every 100 requests
        case N rem 100 of
            0 -> io:format("  Processed ~p requests~n", [N]);
            _ -> ok
        end
    end, lists:seq(1, 1000)),

    io:format("~n"),

    % Display metrics
    Metrics = erlmcp_counters:get_all(),
    io:format("Final Metrics:~n"),
    io:format("  Total requests: ~p~n", [maps:get(requests_total, Metrics)]),
    io:format("  Successful: ~p~n", [maps:get(requests_success, Metrics)]),
    io:format("  Errors: ~p~n", [maps:get(requests_error, Metrics)]),
    io:format("  Success rate: ~.2f%~n",
              [maps:get(requests_success, Metrics) * 100.0 / maps:get(requests_total, Metrics)]),
    io:format("  Tools executed: ~p~n", [maps:get(tools_executed, Metrics)]),
    io:format("  Resources read: ~p~n", [maps:get(resources_read, Metrics)]),
    io:format("  Prompts used: ~p~n", [maps:get(prompts_used, Metrics)]),
    io:format("  Bytes sent: ~p~n", [maps:get(bytes_sent, Metrics)]),
    io:format("  Bytes received: ~p~n", [maps:get(bytes_received, Metrics)]),
    io:format("~n"),

    % Enter maintenance mode
    io:format("Entering maintenance mode...~n"),
    erlmcp_flags:enter_maintenance_mode(),
    io:format("  Accepting connections: ~p~n", [erlmcp_flags:is_accepting()]),
    io:format("  Maintenance mode: ~p~n", [erlmcp_flags:is_maintenance_mode()]),
    io:format("~n"),

    % Exit maintenance mode
    io:format("Exiting maintenance mode...~n"),
    erlmcp_flags:exit_maintenance_mode(),
    io:format("  Accepting connections: ~p~n", [erlmcp_flags:is_accepting()]),
    io:format("  Maintenance mode: ~p~n", [erlmcp_flags:is_maintenance_mode()]),
    io:format("~n"),

    io:format("=== Simulation Complete ===~n~n"),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

simulate_mcp_activity(N) ->
    lists:foreach(fun(_) ->
        erlmcp_counters:inc_requests(),
        case rand:uniform(10) of
            10 -> erlmcp_counters:inc_error();
            _ -> erlmcp_counters:inc_success()
        end,
        erlmcp_counters:inc_tools_executed(),
        erlmcp_counters:add_bytes_sent(rand:uniform(1024))
    end, lists:seq(1, N)).
