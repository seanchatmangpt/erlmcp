#!/usr/bin/env escript
%% Simple test runner for TCP transport tests

main(_) ->
    io:format("Testing TCP Transport...~n"),
    io:format("Checking if modules can be loaded...~n"),

    %% Try to load the modules
    case code:load_file(erlmcp_transport_tcp) of
        {module, _} -> io:format("✓ erlmcp_transport_tcp loaded~n");
        {error, R1} -> io:format("✗ erlmcp_transport_tcp failed: ~p~n", [R1])
    end,

    case code:load_file(erlmcp_connection_limiter) of
        {module, _} -> io:format("✓ erlmcp_connection_limiter loaded~n");
        {error, R2} -> io:format("✗ erlmcp_connection_limiter failed: ~p~n", [R2])
    end,

    case code:load_file(erlmcp_connection_monitor) of
        {module, _} -> io:format("✓ erlmcp_connection_monitor loaded~n");
        {error, R3} -> io:format("✗ erlmcp_connection_monitor failed: ~p~n", [R3])
    end,

    case code:load_file(erlmcp_memory_guard) of
        {module, _} -> io:format("✓ erlmcp_memory_guard loaded~n");
        {error, R4} -> io:format("✗ erlmcp_memory_guard failed: ~p~n", [R4])
    end,

    case code:load_file(erlmcp_json_rpc) of
        {module, _} -> io:format("✓ erlmcp_json_rpc loaded~n");
        {error, R5} -> io:format("✗ erlmcp_json_rpc failed: ~p~n", [R5])
    end,

    io:format("~nAll core modules checked.~n"),
    init:stop().
