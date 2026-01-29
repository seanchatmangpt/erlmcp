#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @doc Manual test script for progress token functionality
%%% Demonstrates progress tracking without full EUnit framework.
%%% @end
%%%-------------------------------------------------------------------
main(_) ->
    io:format("Starting Progress Token Manual Test~n"),
    io:format("===================================~n~n"),

    % Compile modules if needed
    code:add_patha("apps/erlmcp_core/src"),
    code:add_patha("apps/erlmcp_core/test"),
    code:add_patha("_build/default/lib/erlmcp_core/ebin"),

    % Start the progress server
    {ok, Pid} = erlmcp_progress:start_link(),
    io:format("✓ Progress server started: ~p~n~n", [Pid]),

    % Test 1: Create progress token
    io:format("Test 1: Create progress token~n"),
    ClientPid = self(),
    Token = erlmcp_progress:create(ClientPid, <<"Starting operation">>),
    io:format("  ✓ Token created: ~p~n", [Token]),
    receive
        {mcp_notification, Notification1} ->
            io:format("  ✓ Initial notification received~n"),
            io:format("    Progress: ~p~n", [maps:get(<<"progress">>, maps:get(<<"params">>, Notification1))])
    after 1000 ->
        io:format("  ✗ No notification received~n")
    end,
    io:format("~n"),

    % Test 2: Update progress with increment
    io:format("Test 2: Update progress with increment~n"),
    flush_notifications(),
    erlmcp_progress:update(Token, #{increment => 25}),
    io:format("  ✓ Incremented by 25~n"),
    receive
        {mcp_notification, Notification2} ->
            Params2 = maps:get(<<"params">>, Notification2),
            io:format("  ✓ Notification received~n"),
            io:format("    Current progress: ~p~n", [maps:get(<<"progress">>, Params2)])
    after 1000 ->
        io:format("  ✗ No notification received~n")
    end,
    io:format("~n"),

    % Test 3: Update with total (percentage calculation)
    io:format("Test 3: Update with total for percentage~n"),
    flush_notifications(),
    erlmcp_progress:update(Token, #{current => 50, total => 100}),
    io:format("  ✓ Set current=50, total=100~n"),
    receive
        {mcp_notification, Notification3} ->
            Params3 = maps:get(<<"params">>, Notification3),
            io:format("  ✓ Notification received~n"),
            io:format("    Progress: ~p% (~p/~p)~n", [
                maps:get(<<"progress">>, Params3),
                maps:get(<<"progress">>, Params3),
                maps:get(<<"total">>, Params3)
            ])
    after 1000 ->
        io:format("  ✗ No notification received~n")
    end,
    io:format("~n"),

    % Test 4: Update message
    io:format("Test 4: Update progress message~n"),
    flush_notifications(),
    erlmcp_progress:update(Token, #{message => <<"Processing data">>}),
    io:format("  ✓ Message updated~n"),
    receive
        {mcp_notification, Notification4} ->
            Params4 = maps:get(<<"params">>, Notification4),
            io:format("  ✓ Notification received~n"),
            io:format("    Message: ~p~n", [maps:get(<<"message">>, Params4)])
    after 1000 ->
        io:format("  ✗ No notification received~n")
    end,
    io:format("~n"),

    % Test 5: Get progress state
    io:format("Test 5: Get progress state~n"),
    {ok, ProgressMap} = erlmcp_progress:get_progress(Token),
    io:format("  ✓ Progress state retrieved~n"),
    io:format("    Token: ~p~n", [maps:get(token, ProgressMap)]),
    io:format("    Current: ~p~n", [maps:get(current, ProgressMap)]),
    io:format("    Total: ~p~n", [maps:get(total, ProgressMap)]),
    io:format("    Progress: ~p%~n", [maps:get(progress, ProgressMap)]),
    io:format("    Elapsed: ~p ms~n", [maps:get(elapsed_ms, ProgressMap)]),
    io:format("~n"),

    % Test 6: Complete progress
    io:format("Test 6: Complete progress~n"),
    flush_notifications(),
    erlmcp_progress:complete(Token),
    io:format("  ✓ Progress completed~n"),
    receive
        {mcp_notification, Notification5} ->
            Params5 = maps:get(<<"params">>, Notification5),
            io:format("  ✓ Completion notification received~n"),
            io:format("    Progress: ~p%~n", [maps:get(<<"progress">>, Params5)]),
            io:format("    Message: ~p~n", [maps:get(<<"message">>, Params5)])
    after 1000 ->
        io:format("  ✗ No completion notification received~n")
    end,
    io:format("~n"),

    % Test 7: Verify token removed after completion
    io:format("Test 7: Verify token cleanup~n"),
    case erlmcp_progress:get_progress(Token) of
        {error, not_found} ->
            io:format("  ✓ Token properly removed after completion~n");
        {ok, _} ->
            io:format("  ✗ Token still exists after completion~n")
    end,
    io:format("~n"),

    % Test 8: Multiple concurrent progress streams
    io:format("Test 8: Multiple concurrent progress streams~n"),
    flush_notifications(),
    TokenA = erlmcp_progress:create(ClientPid, <<"Stream A">>),
    TokenB = erlmcp_progress:create(ClientPid, <<"Stream B">>),
    io:format("  ✓ Created two concurrent tokens~n"),
    erlmcp_progress:update(TokenA, #{current => 75, total => 100}),
    erlmcp_progress:update(TokenB, #{current => 25, total => 50}),
    {ok, ProgressA} = erlmcp_progress:get_progress(TokenA),
    {ok, ProgressB} = erlmcp_progress:get_progress(TokenB),
    io:format("  ✓ Stream A: ~p% (~p/~p)~n", [
        maps:get(progress, ProgressA),
        maps:get(current, ProgressA),
        maps:get(total, ProgressA)
    ]),
    io:format("  ✓ Stream B: ~p% (~p/~p)~n", [
        maps:get(progress, ProgressB),
        maps:get(current, ProgressB),
        maps:get(total, ProgressB)
    ]),
    io:format("~n"),

    % Test 9: Cancel progress
    io:format("Test 9: Cancel progress~n"),
    TokenC = erlmcp_progress:create(ClientPid, <<"To be cancelled">>),
    flush_notifications(),
    erlmcp_progress:cancel(TokenC),
    io:format("  ✓ Progress cancelled~n"),
    case erlmcp_progress:get_progress(TokenC) of
        {error, not_found} ->
            io:format("  ✓ Token properly removed after cancellation~n");
        {ok, _} ->
            io:format("  ✗ Token still exists after cancellation~n")
    end,
    io:format("~n"),

    % Test 10: Encode notification
    io:format("Test 10: Encode progress notification~n"),
    TokenD = make_ref(),
    Encoded = erlmcp_progress:encode_progress_notification(TokenD, 85.5, 100),
    io:format("  ✓ Notification encoded~n"),
    io:format("    JSON-RPC: ~p~n", [maps:get(<<"jsonrpc">>, Encoded)]),
    io:format("    Method: ~p~n", [maps:get(<<"method">>, Encoded)]),
    Params10 = maps:get(<<"params">>, Encoded),
    io:format("    Progress: ~p~n", [maps:get(<<"progress">>, Params10)]),
    io:format("    Total: ~p~n", [maps:get(<<"total">>, Params10)]),
    io:format("~n"),

    % Cleanup
    gen_server:stop(erlmcp_progress),
    io:format("✓ Progress server stopped~n~n"),

    io:format("===================================~n"),
    io:format("All tests completed!~n").

%% @doc Flush all pending notifications from mailbox
flush_notifications() ->
    receive
        {mcp_notification, _} -> flush_notifications()
    after 0 ->
        ok
    end.
