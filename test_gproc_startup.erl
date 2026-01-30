#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa apps/erlmcp_core/_build/default/lib/*/ebin -pa apps/erlmcp_transports/_build/default/lib/*/ebin -pa apps/erlmcp_observability/_build/default/lib/*/ebin

main(_) ->
    io:format("Testing gproc startup...~n"),

    %% Start crypto
    application:start(crypto),
    io:format("✓ Started crypto~n"),

    %% Start ssl
    application:start(ssl),
    io:format("✓ Started ssl~n"),

    %% Start gproc explicitly
    case application:start(gproc) of
        ok ->
            io:format("✓ Started gproc~n");
        {error, {already_started, gproc}} ->
            io:format("✓ gproc already started~n");
        {error, Reason} ->
            io:format("✗ Failed to start gproc: ~p~n", [Reason]),
            halt(1)
    end,

    %% Start jsx
    application:start(jsx),
    io:format("✓ Started jsx~n"),

    %% Start jesse
    application:start(jesse),
    io:format("✓ Started jesse~n"),

    %% Start erlmcp_core
    case application:start(erlmcp_core) of
        ok ->
            io:format("✓ Started erlmcp_core~n");
        {error, {already_started, erlmcp_core}} ->
            io:format("✓ erlmcp_core already started~n");
        {error, Reason2} ->
            io:format("✗ Failed to start erlmcp_core: ~p~n", [Reason2]),
            halt(1)
    end,

    %% Verify core processes are running
    case whereis(erlmcp_sup) of
        undefined ->
            io:format("✗ erlmcp_sup not running~n"),
            halt(1);
        _ ->
            io:format("✓ erlmcp_sup is running~n")
    end,

    case whereis(erlmcp_registry) of
        undefined ->
            io:format("✗ erlmcp_registry not running~n"),
            halt(1);
        _ ->
            io:format("✓ erlmcp_registry is running~n")
    end,

    %% Test gproc is working
    try
        gproc:reg({n, l, test_gproc_value}),
        io:format("✓ gproc registration works~n"),
        gproc:unreg({n, l, test_gproc_value}),
        io:format("✓ gproc unregistration works~n")
    catch
        _:Error ->
            io:format("✗ gproc operations failed: ~p~n", [Error]),
            halt(1)
    end,

    io:format("~n✓ All gproc startup tests PASSED!~n"),
    halt(0).
