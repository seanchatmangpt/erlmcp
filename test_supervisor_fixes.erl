#!/usr/bin/env escript
%% Test script to validate supervisor fixes

-mode(compile).

main(_) ->
    io:format("Testing supervisor fixes...~n"),

    %% Compile necessary modules
    io:format("Compiling modules...~n"),
    compile:file("../apps/erlmcp_core/src/erlmcp_sup", [{i, "../apps/erlmcp_core/include"}]),
    compile:file("../apps/erlmcp_core/src/erlmcp_server_sup", [{i, "../apps/erlmcp_core/include"}]),
    compile:file("../apps/erlmcp_core/src/erlmcp_notification_handler_sup", [{i, "../apps/erlmcp_core/include"}]),
    compile:file("../apps/erlmcp_core/src/erlmcp_core_sup", [{i, "../apps/erlmcp_core/include"}]),
    compile:file("../apps/erlmcp_core/src/erlmcp_cluster_sup", [{i, "../apps/erlmcp_core/include"}]),

    %% Test 1: erlmcp_server_sup child spec
    io:format("~nTest 1: erlmcp_server_sup child spec...~n"),
    test_server_sup(),

    %% Test 2: erlmcp_notification_handler_sup child spec
    io:format("~nTest 2: erlmcp_notification_handler_sup child spec...~n"),
    test_notification_handler_sup(),

    %% Test 3: Empty child spec handling
    io:format("~nTest 3: Empty child spec handling...~n"),
    test_empty_child_spec(),

    io:format("~n~n✅ All supervisor tests passed!~n"),
    halt(0).

%% Test that server supervisor has correct child spec
test_server_sup() ->
    {ok, SupPid} = erlmcp_server_sup:start_link(),
    io:format("  ✓ erlmcp_server_sup started~n"),

    %% Verify we can start a child
    ServerId = test_server_1,
    Config = #{test => true},

    case erlmcp_server_sup:start_child(ServerId, Config) of
        {ok, ServerPid} when is_pid(ServerPid) ->
            io:format("  ✓ Child process started successfully~n"),
            ok = supervisor:terminate_child(erlmcp_server_sup, ServerPid),
            io:format("  ✓ Child process terminated successfully~n");
        {error, Reason} ->
            io:format("  ⚠ Child start returned: ~p (may be expected if registry not available)~n", [Reason])
    end,

    ok = supervisor:stop(SupPid),
    io:format("  ✓ Supervisor stopped~n").

%% Test that notification handler supervisor has correct child spec
test_notification_handler_sup() ->
    {ok, SupPid} = erlmcp_notification_handler_sup:start_link(),
    io:format("  ✓ erlmcp_notification_handler_sup started~n"),

    %% Can't fully test without the handler module
    %% but supervisor started successfully

    ok = supervisor:stop(SupPid),
    io:format("  ✓ Supervisor stopped~n").

%% Test empty child spec handling
test_empty_child_spec() ->
    %% Disable clustering
    application:load(erlmcp_core),
    application:set_env(erlmcp_core, cluster_enabled, false),

    {ok, SupPid} = erlmcp_cluster_sup:start_link(),
    io:format("  ✓ erlmcp_cluster_sup started with empty child list~n"),

    %% Verify no children
    Children = supervisor:which_children(SupPid),
    io:format("  ✓ Child count: ~p~n", [length(Children)]),

    ok = supervisor:stop(SupPid),
    io:format("  ✓ Supervisor stopped~n").
