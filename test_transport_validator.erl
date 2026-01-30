%%%-------------------------------------------------------------------
%%% @doc
%%% Simple standalone test for transport validator
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(test_transport_validator).

-export([run/0]).

run() ->
    io:format("~n=== Transport Validator Standalone Test ===~n~n"),

    % Ensure code is loaded
    code:ensure_loaded(erlmcp_transport_validator),
    code:ensure_loaded(erlmcp_transport_stdio),
    code:ensure_loaded(erlmcp_transport_tcp),

    % Start the validator gen_server
    {ok, _Pid} = erlmcp_transport_validator:start_link(),

    % Test 1: Validate STDIO callbacks
    io:format("Test 1: STDIO Callbacks~n"),
    CallbacksStdio = erlmcp_transport_validator:validate_callbacks(erlmcp_transport_stdio),
    io:format("  Result: ~p~n~n", [CallbacksStdio]),

    % Test 2: Validate TCP callbacks
    io:format("Test 2: TCP Callbacks~n"),
    CallbacksTcp = erlmcp_transport_validator:validate_callbacks(erlmcp_transport_tcp),
    io:format("  Result: ~p~n~n", [CallbacksTcp]),

    % Test 3: Validate STDIO framing
    io:format("Test 3: STDIO Framing~n"),
    FramingStdio = erlmcp_transport_validator:validate_framing(erlmcp_transport_stdio, stdio),
    io:format("  Result: ~p~n~n", [FramingStdio]),

    % Test 4: Validate TCP framing
    io:format("Test 4: TCP Framing~n"),
    FramingTcp = erlmcp_transport_validator:validate_framing(erlmcp_transport_tcp, tcp),
    io:format("  Result: ~p~n~n", [FramingTcp]),

    % Test 5: Validate STDIO registry
    io:format("Test 5: STDIO Registry~n"),
    RegistryStdio = erlmcp_transport_validator:validate_registry(erlmcp_transport_stdio),
    io:format("  Result: ~p~n~n", [RegistryStdio]),

    % Test 6: Validate STDIO lifecycle
    io:format("Test 6: STDIO Lifecycle~n"),
    LifecycleStdio = erlmcp_transport_validator:validate_lifecycle(erlmcp_transport_stdio),
    io:format("  Result: ~p~n~n", [LifecycleStdio]),

    % Stop the validator
    gen_server:stop(erlmcp_transport_validator),

    io:format("=== Tests Complete ===~n"),
    ok.
