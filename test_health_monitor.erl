#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/test/lib/*/ebin -pa _build/test/lib/*/test

main([]) ->
    eunit:test(erlmcp_health_monitor_tests, [verbose]),
    init:stop().
