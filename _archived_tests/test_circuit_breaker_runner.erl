#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

%% Quick test runner for circuit breaker
main([]) ->
    main(["run"]);
main(["run"]) ->
    %% Start application
    ok = application:ensure_all_started(erlmcp),
    timer:sleep(100),

    %% Start circuit breaker
    {ok, _Pid} = erlmcp_circuit_breaker:start_link(),
    timer:sleep(100),

    %% Run basic tests
    run_tests();
main(_) ->
    io:format("Usage: test_circuit_breaker_runner.erl [run]~n", []),
    halt(1).

run_tests() ->
    io:format("~n=== Circuit Breaker v1.3.0 Tests ===~n~n", []),

    % Test 1: Initial state
    io:format("[TEST 1] Initial state is closed...~n", []),
    {ok, Status1} = erlmcp_circuit_breaker:get_status(),
    case Status1 =:= closed of
        true -> io:format("  PASS: Initial state is closed~n~n", []);
        false -> io:format("  FAIL: Expected closed, got ~p~n~n", [Status1])
    end,

    % Test 2: Move to open after failures
    io:format("[TEST 2] Circuit opens after 5 failures...~n", []),
    [erlmcp_circuit_breaker:record_error(Id) || Id <- lists:seq(1, 5)],
    timer:sleep(200),
    erlmcp_circuit_breaker:record_request(test_id, 10),
    timer:sleep(200),
    {ok, Status2} = erlmcp_circuit_breaker:get_status(),
    case Status2 =:= open of
        true -> io:format("  PASS: Circuit opened~n~n", []);
        false -> io:format("  FAIL: Expected open, got ~p~n~n", [Status2])
    end,

    % Test 3: can_execute returns deny when open
    io:format("[TEST 3] can_execute denies execution when open...~n", []),
    {CanExec, Action} = erlmcp_circuit_breaker:can_execute(),
    case {CanExec, Action} =:= {false, deny} of
        true -> io:format("  PASS: can_execute returned {false, deny}~n~n", []);
        false -> io:format("  FAIL: Expected {false, deny}, got ~p~n~n", [{CanExec, Action}])
    end,

    % Test 4: Retry attempts blocked
    io:format("[TEST 4] Retry attempts are blocked when circuit open...~n", []),
    [erlmcp_circuit_breaker:record_retry_attempt() || _ <- lists:seq(1, 20)],
    timer:sleep(100),
    {ok, Metrics1} = erlmcp_circuit_breaker:get_metrics(),
    BlockedRetries = maps:get(retry_attempts_blocked, Metrics1, 0),
    case BlockedRetries >= 20 of
        true -> io:format("  PASS: ~p retries blocked~n~n", [BlockedRetries]);
        false -> io:format("  FAIL: Expected >= 20 blocked, got ~p~n~n", [BlockedRetries])
    end,

    % Test 5: Reset functionality
    io:format("[TEST 5] Reset returns circuit to closed...~n", []),
    erlmcp_circuit_breaker:reset(),
    timer:sleep(100),
    {ok, Status5} = erlmcp_circuit_breaker:get_status(),
    case Status5 =:= closed of
        true -> io:format("  PASS: Circuit reset to closed~n~n", []);
        false -> io:format("  FAIL: Expected closed after reset, got ~p~n~n", [Status5])
    end,

    % Test 6: Metrics collection
    io:format("[TEST 6] Metrics are collected...~n", []),
    {ok, Metrics6} = erlmcp_circuit_breaker:get_metrics(),
    HasStateTransitions = maps:is_key(state_transitions, Metrics6),
    HasLastStateChange = maps:is_key(last_state_change, Metrics6),
    case {HasStateTransitions, HasLastStateChange} of
        {true, true} -> io:format("  PASS: Metrics include state tracking~n~n", []);
        _ -> io:format("  FAIL: Missing state tracking metrics~n~n", [])
    end,

    io:format("=== All tests completed ===~n~n", []),
    erlmcp_circuit_breaker:stop(),
    halt(0).
