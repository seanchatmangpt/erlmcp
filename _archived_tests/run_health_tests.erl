#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/test/lib/erlmcp_observability/ebin -pa _build/test/lib/erlmcp_observability/test -pa _build/test/lib/erlmcp_core/ebin -pa _build/test/lib/erlmcp_core/test -pa _build/test/lib/*/ebin

main([]) ->
    io:format("Running erlmcp_health_monitor_tests...~n"),
    case eunit:test(erlmcp_health_monitor_tests, [verbose]) of
        ok -> io:format("All tests passed!~n");
        error -> io:format("Some tests failed!~n")
    end,
    init:stop().
