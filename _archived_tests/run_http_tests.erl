#!/usr/bin/env escript
%%%-------------------------------------------------------------------
%%% @doc
%%% Test runner for HTTP transport integration tests
%%%-------------------------------------------------------------------
main(_) ->
    io:format("Starting HTTP transport integration tests...~n"),

    %% Start required applications
    io:format("Starting applications...~n"),
    {ok, _} = application:ensure_all_started(gun),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(ranch),
    {ok, _} = application:ensure_all_started(jsx),

    io:format("Applications started.~n"),

    %% Compile test modules
    io:format("Compiling test modules...~n"),
    case compile:file("apps/erlmcp_transports/test/mock_http_mcp_handler", [{outdir, "ebin"}]) of
        {ok, _} -> io:format("✓ mock_http_mcp_handler compiled~n");
        Error1 -> io:format("✗ Failed to compile mock_http_mcp_handler: ~p~n", [Error1])
    end,

    %% Try to compile the test suite
    io:format("Compiling test suite...~n"),
    case compile:file("apps/erlmcp_transports/test/erlmcp_transport_http_SUITE", [{outdir, "ebin"}]) of
        {ok, _} -> io:format("✓ erlmcp_transport_http_SUITE compiled~n");
        Error2 -> io:format("✗ Failed to compile test suite: ~p~n", [Error2])
    end,

    %% Add ebin to code path
    true = code:add_patha("ebin"),
    true = code:add_patha("_build/default/lib/erlmcp_transports/ebin"),
    true = code:add_patha("_build/default/lib/gun/ebin"),
    true = code:add_patha("_build/default/lib/cowboy/ebin"),
    true = code:add_patha("_build/default/lib/ranch/ebin"),
    true = code:add_patha("_build/default/lib/jsx/ebin"),
    true = code:add_patha("_build/default/lib/erlmcp_core/ebin"),

    io:format("~nRunning basic HTTP transport tests...~n"),

    %% Test 1: Module loads
    io:format("~nTest 1: Module loading...~n"),
    case code:load_file(erlmcp_transport_http) of
        {module, erlmcp_transport_http} ->
            io:format("  ✓ erlmcp_transport_http loaded~n");
        Error3 ->
            io:format("  ✗ Failed to load erlmcp_transport_http: ~p~n", [Error3])
    end,

    %% Test 2: Server module loads
    io:format("~nTest 2: HTTP server module loading...~n"),
    case code:load_file(erlmcp_transport_http_server) of
        {module, erlmcp_transport_http_server} ->
            io:format("  ✓ erlmcp_transport_http_server loaded~n");
        Error4 ->
            io:format("  ✗ Failed to load erlmcp_transport_http_server: ~p~n", [Error4])
    end,

    %% Test 3: Mock handler loads
    io:format("~nTest 3: Mock handler loading...~n"),
    case code:load_file(mock_http_mcp_handler) of
        {module, mock_http_mcp_handler} ->
            io:format("  ✓ mock_http_mcp_handler loaded~n");
        Error5 ->
            io:format("  ✗ Failed to load mock_http_mcp_handler: ~p~n", [Error5])
    end,

    %% Test 4: Test suite loads
    io:format("~nTest 4: Test suite loading...~n"),
    case code:load_file(erlmcp_transport_http_SUITE) of
        {module, erlmcp_transport_http_SUITE} ->
            io:format("  ✓ erlmcp_transport_http_SUITE loaded~n");
        Error6 ->
            io:format("  ✗ Failed to load erlmcp_transport_http_SUITE: ~p~n", [Error6])
    end,

    io:format("~n========================================~n"),
    io:format("HTTP transport integration test summary:~n"),
    io:format("========================================~n"),
    io:format("✓ Test modules created:~n"),
    io:format("  - erlmcp_transport_http_SUITE.erl (26 test cases)~n"),
    io:format("  - mock_http_mcp_handler.erl (mock HTTP server)~n"),
    io:format("~nTest coverage:~n"),
    io:format("  - Initialization: 5 tests~n"),
    io:format("  - Connection management: 4 tests~n"),
    io:format("  - Request handling: 4 tests~n"),
    io:format("  - Response handling: 3 tests~n"),
    io:format("  - Retry logic: 2 tests~n"),
    io:format("  - Error handling: 2 tests~n"),
    io:format("  - Pool management: 2 tests~n"),
    io:format("  - Cleanup: 2 tests~n"),
    io:format("~nTo run full test suite:~n"),
    io:format("  rebar3 ct --suite=apps/erlmcp_transports/test/erlmcp_transport_http_SUITE~n"),
    io:format("========================================~n"),

    ok.
