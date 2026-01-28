#!/bin/bash

echo "=== Testing Transport Registration ==="
echo ""

# Start Erlang shell with registration test
erl -pa _build/default/lib/*/ebin -noshell -eval '
    application:ensure_all_started(gproc),
    {ok, _} = erlmcp_registry:start_link(),

    io:format("~n1. Testing stdio without transport_id...~n"),
    {ok, P1} = erlmcp_transport_stdio:start_link(self()),
    case erlmcp_registry:find_transport(test1) of
        {error, not_found} -> io:format("   ✓ Not registered (expected)~n");
        _ -> io:format("   ✗ FAILED~n"), halt(1)
    end,
    erlmcp_transport_stdio:close(P1),

    io:format("~n2. Testing stdio WITH transport_id...~n"),
    {ok, P2} = erlmcp_transport_stdio:start_link(self(), #{transport_id => test2}),
    timer:sleep(100),
    case erlmcp_registry:find_transport(test2) of
        {ok, {P2, _}} -> io:format("   ✓ Registered (PID: ~p)~n", [P2]);
        {error, not_found} -> io:format("   ✗ FAILED - not registered~n"), halt(1)
    end,

    io:format("~n3. Testing unregistration...~n"),
    erlmcp_transport_stdio:close(P2),
    timer:sleep(100),
    case erlmcp_registry:find_transport(test2) of
        {error, not_found} -> io:format("   ✓ Unregistered~n");
        _ -> io:format("   ✗ FAILED - still registered~n"), halt(1)
    end,

    io:format("~n=== All tests passed! ===~n"),
    halt(0)
' -s init stop
