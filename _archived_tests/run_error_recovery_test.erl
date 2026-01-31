#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin -pa _build/test/lib/*/ebin

main(_) ->
    io:format("Starting erlmcp_error_recovery_SUITE...~n"),
    application:ensure_all_started(erlmcp),
    io:format("Applications started.~n"),
    {ok, _} = ct:run_test([{suite, erlmcp_error_recovery_SUITE}]),
    io:format("Test complete.~n"),
    init:stop().
