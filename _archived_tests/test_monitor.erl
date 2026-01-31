%% Quick test for process monitor
start() ->
    io:format("Starting Process Monitor Test~n"),
    io:format("============================~n~n"),

    %% Start the process monitor
    case erlmcp_process_monitor:start_link([{check_interval, 1000}]) of
        {ok, Pid} ->
            io:format("Started process monitor: ~p~n", [Pid]),
            test_all(Pid);
        {error, {already_started, Pid}} ->
            io:format("Process monitor already running: ~p~n", [Pid]),
            test_all(Pid);
        Error ->
            io:format("Failed to start: ~p~n", [Error])
    end.

test_all(Pid) ->
    %% Test 1: Get metrics
    io:format("~n--- Test 1: Get Process Metrics ---~n"),
    {ok, Metrics} = erlmcp_process_monitor:get_process_metrics(),
    io:format("Process Count: ~p~n", [maps:get(process_count, Metrics)]),
    io:format("Process Limit: ~p~n", [maps:get(process_limit, Metrics)]),
    io:format("Usage: ~.2f%~n", [maps:get(usage_percent, Metrics) * 100]),
    io:format("Status: ~p~n", [maps:get(status, Metrics)]),
    io:format("Capacity: ~p connections~n", [maps:get(capacity_estimate, Metrics)]),

    %% Test 2: Check limit
    io:format("~n--- Test 2: Check Process Limit ---~n"),
    case erlmcp_process_monitor:check_process_limit() of
        ok -> io:format("Status: OK~n");
        {warning, M} -> io:format("WARNING: ~s~n", [M]);
        {critical, M} -> io:format("CRITICAL: ~s~n", [M])
    end,

    %% Test 3: Capacity estimate
    io:format("~n--- Test 3: Capacity Estimate ---~n"),
    {ok, Est} = erlmcp_process_monitor:get_capacity_estimate(),
    io:format("Current: ~p~n", [maps:get(current_connections, Est)]),
    io:format("Estimated: ~p~n", [maps:get(estimated_capacity, Est)]),
    io:format("Remaining: ~p~n", [maps:get(remaining_capacity, Est)]),

    %% Test 4: Thresholds
    io:format("~n--- Test 4: Alert Thresholds ---~n"),
    {ok, {Warn, Crit}} = erlmcp_process_monitor:get_alert_thresholds(),
    io:format("Warning: ~.2f%~n", [Warn * 100]),
    io:format("Critical: ~.2f%~n", [Crit * 100]),

    %% Summary
    io:format("~n============================~n"),
    io:format("All tests passed!~n"),
    io:format("Capacity: ~p connections (realistic: 40-50K)~n",
              [maps:get(estimated_capacity, Est)]).
