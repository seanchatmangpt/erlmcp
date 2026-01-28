#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin -pa _build/default/lib/*/include

main([]) ->
    io:format("Testing erlmcp_pool_manager...~n"),

    %% Start dependencies
    application:ensure_all_started(erlmcp_transports),

    %% Test 1: Pool creation
    io:format("Test 1: Creating pool...~n"),
    PoolOpts = #{
        min_size => 5,
        max_size => 20,
        strategy => round_robin,
        worker_module => erlmcp_pool_manager_tests,
        worker_opts => #{}
    },

    {ok, Pool} = erlmcp_pool_manager:start_link(PoolOpts),
    io:format("  ✓ Pool created: ~p~n", [Pool]),

    %% Test 2: Get status
    io:format("Test 2: Checking status...~n"),
    Status = erlmcp_pool_manager:get_status(Pool),
    io:format("  ✓ Current size: ~p~n", [maps:get(current_size, Status)]),
    io:format("  ✓ Idle count: ~p~n", [maps:get(idle_count, Status)]),
    io:format("  ✓ Active count: ~p~n", [maps:get(active_count, Status)]),

    %% Test 3: Checkout/checkin
    io:format("Test 3: Checkout/checkin...~n"),
    {ok, Conn} = erlmcp_pool_manager:checkout(Pool),
    io:format("  ✓ Checked out connection: ~p~n", [Conn]),

    ok = erlmcp_pool_manager:checkin(Pool, Conn),
    io:format("  ✓ Checked in connection~n"),

    %% Test 4: Metrics
    io:format("Test 4: Metrics...~n"),
    Metrics = erlmcp_pool_manager:get_metrics(Pool),
    io:format("  ✓ Total checkouts: ~p~n", [maps:get(total_checkouts, Metrics)]),
    io:format("  ✓ Pool utilization: ~.2f%~n", [maps:get(pool_utilization_percent, Metrics)]),
    io:format("  ✓ Avg checkout time: ~.2f μs~n", [maps:get(avg_checkout_time_us, Metrics)]),

    %% Cleanup
    erlmcp_pool_manager:stop(Pool),

    io:format("~nAll tests passed!~n"),
    halt(0).
