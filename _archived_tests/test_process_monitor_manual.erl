#!/usr/bin/env escript
%% Manual test script for process monitor

main(_) ->
    io:format("Starting Process Monitor Manual Test~n"),
    io:format("======================================~n~n"),

    %% Start the process monitor
    {ok, Pid} = erlmcp_process_monitor:start_link([
        {check_interval, 1000},
        {warning_threshold, 0.70},
        {critical_threshold, 0.90}
    ]),
    io:format("✓ Process monitor started: ~p~n", [Pid]),

    %% Test 1: Get process metrics
    io:format("~nTest 1: Get Process Metrics~n"),
    {ok, Metrics} = erlmcp_process_monitor:get_process_metrics(),
    io:format("  Process Count: ~p~n", [maps:get(process_count, Metrics)]),
    io:format("  Process Limit: ~p~n", [maps:get(process_limit, Metrics)]),
    io:format("  Usage Percent: ~.2f%~n", [maps:get(usage_percent, Metrics) * 100]),
    io:format("  Status: ~p~n", [maps:get(status, Metrics)]),
    io:format("  Capacity Estimate: ~p~n", [maps:get(capacity_estimate, Metrics)]),

    %% Test 2: Get capacity estimate
    io:format("~nTest 2: Get Capacity Estimate~n"),
    {ok, Estimate} = erlmcp_process_monitor:get_capacity_estimate(),
    io:format("  Current Connections: ~p~n", [maps:get(current_connections, Estimate)]),
    io:format("  Estimated Capacity: ~p~n", [maps:get(estimated_capacity, Estimate)]),
    io:format("  Remaining Capacity: ~p~n", [maps:get(remaining_capacity, Estimate)]),
    io:format("  Utilization: ~.2f%~n", [maps:get(utilization_percent, Estimate)]),
    io:format("  Memory Total: ~.2f MB~n", [maps:get(memory_total_bytes, Estimate) / 1024 / 1024]),
    io:format("  Memory Used: ~.2f MB~n", [maps:get(memory_used_bytes, Estimate) / 1024 / 1024]),

    %% Test 3: Check process limit
    io:format("~nTest 3: Check Process Limit~n"),
    case erlmcp_process_monitor:check_process_limit() of
        ok ->
            io:format("  ✓ Process usage OK~n");
        {warning, Msg} ->
            io:format("  ⚠ WARNING: ~s~n", [Msg]);
        {critical, Msg} ->
            io:format("  ✗ CRITICAL: ~s~n", [Msg])
    end,

    %% Test 4: Get alert thresholds
    io:format("~nTest 4: Alert Thresholds~n"),
    {ok, {Warning, Critical}} = erlmcp_process_monitor:get_alert_thresholds(),
    io:format("  Warning Threshold: ~.2f%~n", [Warning * 100]),
    io:format("  Critical Threshold: ~.2f%~n", [Critical * 100]),

    %% Test 5: Set custom thresholds
    io:format("~nTest 5: Set Custom Thresholds~n"),
    ok = erlmcp_process_monitor:set_alert_thresholds(0.60, 0.85),
    {ok, {Warning2, Critical2}} = erlmcp_process_monitor:get_alert_thresholds(),
    io:format("  New Warning Threshold: ~.2f%~n", [Warning2 * 100]),
    io:format("  New Critical Threshold: ~.2f%~n", [Critical2 * 100]),

    %% Test 6: Enable auto-scaling
    io:format("~nTest 6: Auto-Scaling~n"),
    erlmcp_process_monitor:enable_auto_scaling(),
    io:format("  ✓ Auto-scaling enabled~n"),
    {ok, Estimate2} = erlmcp_process_monitor:get_capacity_estimate(),
    Recommendations = maps:get(recommendations, Estimate2),
    io:format("  Recommendations: ~p~n", [length(Recommendations)]),
    lists:foreach(fun(Rec) ->
        io:format("    - ~s~n", [Rec])
    end, Recommendations),

    %% Test 7: Wait for periodic check
    io:format("~nTest 7: Periodic Monitoring (waiting 2 seconds)...~n"),
    timer:sleep(2000),
    {ok, Metrics2} = erlmcp_process_monitor:get_process_metrics(),
    io:format("  ✓ Periodic check complete~n"),
    io:format("  New timestamp: ~p~n", [maps:get(timestamp, Metrics2)]),

    %% Cleanup
    gen_server:stop(Pid),
    io:format("~n✓ Process monitor stopped~n"),
    io:format("~n======================================~n"),
    io:format("All tests passed!~n"),
    io:format("~nSUMMARY:~n"),
    io:format("  - Process monitor lifecycle: ✓~n"),
    io:format("  - Metrics collection: ✓~n"),
    io:format("  - Capacity estimation: ✓~n"),
    io:format("  - Process limit checks: ✓~n"),
    io:format("  - Alert threshold management: ✓~n"),
    io:format("  - Auto-scaling recommendations: ✓~n"),
    io:format("  - Periodic monitoring: ✓~n"),
    io:format("~nCapacity Note:~n"),
    io:format("  Realistic single-node capacity: 40-50K connections~n"),
    io:format("  100K+ connections require clustering + connection pooling~n"),
    halt(0).
