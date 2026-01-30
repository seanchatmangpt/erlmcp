#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/gproc/ebin -pa _build/default/lib/jsx/ebin -pa _build/default/lib/jesse/ebin -pa _build/test/lib/erlmcp_core/ebin -pa _build/test/lib/gproc/ebin

main(_) ->
    application:ensure_all_started(gproc),

    io:format('~n=== Running Connection Limiter Tests ===~n~n'),

    % Test 1: test_per_server_counters
    io:format('Test 1: test_per_server_counters... '),
    case test_per_server_counters() of
        ok -> io:format('PASSED~n');
        Error1 -> io:format('FAILED: ~p~n', [Error1])
    end,

    % Test 2: test_connection_tracking
    io:format('Test 2: test_connection_tracking... '),
    case test_connection_tracking() of
        ok -> io:format('PASSED~n');
        Error2 -> io:format('FAILED: ~p~n', [Error2])
    end,

    % Test 3: test_counter_increment
    io:format('Test 3: test_counter_increment... '),
    case test_counter_increment() of
        ok -> io:format('PASSED~n');
        Error3 -> io:format('FAILED: ~p~n', [Error3])
    end,

    % Test 4: test_counter_decrement
    io:format('Test 4: test_counter_decrement... '),
    case test_counter_decrement() of
        ok -> io:format('PASSED~n');
        Error4 -> io:format('FAILED: ~p~n', [Error4])
    end,

    % Test 5: test_multiple_servers
    io:format('Test 5: test_multiple_servers... '),
    case test_multiple_servers() of
        ok -> io:format('PASSED~n');
        Error5 -> io:format('FAILED: ~p~n', [Error5])
    end,

    % Final cleanup
    erlmcp_connection_limiter:stop(),

    io:format('~n=== All Tests Complete ===~n'),
    halt().

setup() ->
    case application:start(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end,
    application:set_env(erlmcp_core, connection_limiting, #{
        max_connections => 10000,
        alert_threshold => 0.7,
        enabled => true
    }),
    % Check if already running
    case whereis(erlmcp_connection_limiter) of
        undefined ->
            {ok, _Pid} = erlmcp_connection_limiter:start_link();
        _Pid ->
            ok
    end,
    timer:sleep(10),
    ok.

test_per_server_counters() ->
    setup(),
    ServerId1 = test_server_gproc_1,
    ServerId2 = test_server_gproc_2,

    accept = erlmcp_connection_limiter:accept_connection(ServerId1),
    timer:sleep(10),
    accept = erlmcp_connection_limiter:accept_connection(ServerId1),
    timer:sleep(10),
    accept = erlmcp_connection_limiter:accept_connection(ServerId2),
    timer:sleep(10),

    Count1 = erlmcp_connection_limiter:get_connection_count(ServerId1),
    Count2 = erlmcp_connection_limiter:get_connection_count(ServerId2),

    Result = case {Count1, Count2} of
        {2, 1} -> ok;
        _ -> {error, {expected, {2, 1}}, got, {Count1, Count2}}
    end,

    erlmcp_connection_limiter:release_connection(ServerId1),
    erlmcp_connection_limiter:release_connection(ServerId1),
    erlmcp_connection_limiter:release_connection(ServerId2),

    % Don't stop the gen_server - let it stay running for next test
    Result.

test_connection_tracking() ->
    setup(),
    ServerId1 = test_server_5a,
    ServerId2 = test_server_5b,

    accept = erlmcp_connection_limiter:accept_connection(ServerId1),
    timer:sleep(10),
    accept = erlmcp_connection_limiter:accept_connection(ServerId2),
    timer:sleep(10),

    Count1 = erlmcp_connection_limiter:get_connection_count(ServerId1),
    Count2 = erlmcp_connection_limiter:get_connection_count(ServerId2),
    GlobalCount = erlmcp_connection_limiter:get_connection_count(),

    Result = case {Count1, Count2, GlobalCount >= 2} of
        {1, 1, true} -> ok;
        _ -> {error, expected, {1, 1, true}, got, {Count1, Count2, GlobalCount}}
    end,

    erlmcp_connection_limiter:release_connection(ServerId1),
    erlmcp_connection_limiter:release_connection(ServerId2),

    Result.

test_counter_increment() ->
    setup(),
    ServerId = test_server_gproc_inc,
    CountBefore = erlmcp_connection_limiter:get_connection_count(),

    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    timer:sleep(10),
    CountAfter = erlmcp_connection_limiter:get_connection_count(),

    Expected = CountBefore + 1,
    Result = case CountAfter of
        Expected -> ok;
        _ -> {error, {expected, Expected}, got, CountAfter}
    end,

    erlmcp_connection_limiter:release_connection(ServerId),

    Result.

test_counter_decrement() ->
    setup(),
    ServerId = test_server_gproc_dec,

    accept = erlmcp_connection_limiter:accept_connection(ServerId),
    timer:sleep(10),
    CountDuring = erlmcp_connection_limiter:get_connection_count(),

    ok = erlmcp_connection_limiter:release_connection(ServerId),
    timer:sleep(10),
    CountAfter = erlmcp_connection_limiter:get_connection_count(),

    Expected = CountDuring - 1,
    Result = case CountAfter of
        Expected -> ok;
        _ -> {error, expected, Expected, got, CountAfter}
    end,

    Result.

test_multiple_servers() ->
    setup(),
    ServerIds = [test_multi_server_1, test_multi_server_2, test_multi_server_3],
    AcceptCounts = [10, 20, 30],

    lists:foreach(fun({ServerId, Count}) ->
        [begin
            erlmcp_connection_limiter:accept_connection(ServerId),
            timer:sleep(10)
        end || _ <- lists:seq(1, Count)]
    end, lists:zip(ServerIds, AcceptCounts)),

    timer:sleep(50),

    Checks = lists:map(fun({ServerId, ExpectedCount}) ->
        ActualCount = erlmcp_connection_limiter:get_connection_count(ServerId),
        ActualCount =:= ExpectedCount
    end, lists:zip(ServerIds, AcceptCounts)),

    GlobalCount = erlmcp_connection_limiter:get_connection_count(),
    GlobalCheck = GlobalCount =:= lists:sum(AcceptCounts),

    lists:foreach(fun({ServerId, Count}) ->
        [erlmcp_connection_limiter:release_connection(ServerId) || _ <- lists:seq(1, Count)]
    end, lists:zip(ServerIds, AcceptCounts)),

    case lists:all(fun(X) -> X end, Checks ++ [GlobalCheck]) of
        true -> ok;
        false -> {error, checks_failed, Checks, GlobalCount}
    end.
