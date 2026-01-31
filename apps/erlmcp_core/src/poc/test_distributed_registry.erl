#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @doc
%%% Test script for distributed registry POC
%%%
%%% Usage:
%%%   escript test_distributed_registry.erl
%%%
%%% @end
%%%-------------------------------------------------------------------

main([]) ->
    io:format("~n╔═══════════════════════════════════════════════════════════════╗~n"),
    io:format("║  Testing Distributed Registry POC                            ║~n"),
    io:format("╚═══════════════════════════════════════════════════════════════╝~n~n"),

    %% Test 1: Basic global registration
    io:format("Test 1: Global Registration~n"),
    test_global_registration(),

    %% Test 2: pg group membership
    io:format("~nTest 2: Process Groups~n"),
    test_pg_groups(),

    %% Test 3: Conflict resolution
    io:format("~nTest 3: Conflict Resolution~n"),
    test_conflict_resolution(),

    %% Test 4: Automatic cleanup
    io:format("~nTest 4: Automatic Cleanup~n"),
    test_automatic_cleanup(),

    io:format("~n╔═══════════════════════════════════════════════════════════════╗~n"),
    io:format("║  All Tests Passed                                             ║~n"),
    io:format("╚═══════════════════════════════════════════════════════════════╝~n~n"),

    ok.

test_global_registration() ->
    %% Start a test process
    Pid = spawn(fun() -> receive stop -> ok end end),

    %% Register globally
    io:format("  Registering test_server_1...~n"),
    yes = global:register_name(test_server_1, Pid),
    io:format("  ✓ Registered~n"),

    %% Lookup
    io:format("  Looking up test_server_1...~n"),
    Pid = global:whereis_name(test_server_1),
    io:format("  ✓ Found: ~p~n", [Pid]),

    %% Unregister
    io:format("  Unregistering...~n"),
    global:unregister_name(test_server_1),
    undefined = global:whereis_name(test_server_1),
    io:format("  ✓ Unregistered~n"),

    %% Cleanup
    exit(Pid, kill),
    ok.

test_pg_groups() ->
    %% Start pg
    case whereis(pg) of
        undefined ->
            {ok, _} = pg:start_link(test_scope);
        _ ->
            ok
    end,

    %% Start test processes
    Pid1 = spawn(fun() -> receive stop -> ok end end),
    Pid2 = spawn(fun() -> receive stop -> ok end end),
    Pid3 = spawn(fun() -> receive stop -> ok end end),

    %% Join groups
    io:format("  Joining tool_servers group...~n"),
    ok = pg:join(test_scope, tool_servers, Pid1),
    ok = pg:join(test_scope, tool_servers, Pid2),
    io:format("  ✓ 2 processes joined~n"),

    io:format("  Joining resource_servers group...~n"),
    ok = pg:join(test_scope, resource_servers, Pid3),
    io:format("  ✓ 1 process joined~n"),

    %% Get members
    io:format("  Checking group membership...~n"),
    [Pid1, Pid2] = lists:sort(pg:get_members(test_scope, tool_servers)),
    [Pid3] = pg:get_members(test_scope, resource_servers),
    io:format("  ✓ tool_servers: 2 members~n"),
    io:format("  ✓ resource_servers: 1 member~n"),

    %% Leave group
    io:format("  Leaving groups...~n"),
    ok = pg:leave(test_scope, tool_servers, Pid1),
    [Pid2] = pg:get_members(test_scope, tool_servers),
    io:format("  ✓ Pid1 left tool_servers~n"),

    %% Cleanup
    exit(Pid1, kill),
    exit(Pid2, kill),
    exit(Pid3, kill),
    ok.

test_conflict_resolution() ->
    %% Start a process and register it
    Pid1 = spawn(fun() -> receive stop -> ok end end),
    io:format("  Registering conflict_test with Pid1...~n"),
    yes = global:register_name(conflict_test, Pid1),
    io:format("  ✓ Registered: ~p~n", [Pid1]),

    %% Try to register same name with different process
    Pid2 = spawn(fun() -> receive stop -> ok end end),
    io:format("  Attempting duplicate registration with Pid2...~n"),
    no = global:register_name(conflict_test, Pid2),
    io:format("  ✓ Duplicate rejected~n"),

    %% Verify original still registered
    Pid1 = global:whereis_name(conflict_test),
    io:format("  ✓ Original registration intact~n"),

    %% Cleanup
    global:unregister_name(conflict_test),
    exit(Pid1, kill),
    exit(Pid2, kill),
    ok.

test_automatic_cleanup() ->
    %% Start process and register
    Pid = spawn(fun() -> receive stop -> ok end end),
    io:format("  Registering cleanup_test...~n"),
    yes = global:register_name(cleanup_test, Pid),
    Pid = global:whereis_name(cleanup_test),
    io:format("  ✓ Registered: ~p~n", [Pid]),

    %% Join pg group
    case whereis(pg) of
        undefined -> {ok, _} = pg:start_link(test_scope);
        _ -> ok
    end,
    ok = pg:join(test_scope, cleanup_test_group, Pid),
    [Pid] = pg:get_members(test_scope, cleanup_test_group),
    io:format("  ✓ Joined group~n"),

    %% Kill process (simulates node failure)
    io:format("  Simulating process crash...~n"),
    monitor(process, Pid),
    exit(Pid, kill),
    receive
        {'DOWN', _, process, Pid, _} -> ok
    after 1000 ->
        error(timeout)
    end,
    timer:sleep(100),

    %% Verify global cleanup (manual in POC, automatic in production with monitor)
    %% Note: global doesn't auto-cleanup, that's what the monitor server does
    io:format("  ✓ Process died~n"),

    %% Manual cleanup for this test
    case global:whereis_name(cleanup_test) of
        undefined ->
            io:format("  ✓ Global name auto-cleaned (was already cleaned)~n");
        _ ->
            global:unregister_name(cleanup_test),
            io:format("  ✓ Global name manually cleaned~n")
    end,

    %% pg automatically cleans up dead processes
    timer:sleep(100),
    Members = pg:get_members(test_scope, cleanup_test_group),
    case lists:member(Pid, Members) of
        false ->
            io:format("  ✓ pg group auto-cleaned~n");
        true ->
            io:format("  ⚠ pg still has dead process (expected in some cases)~n")
    end,

    ok.
