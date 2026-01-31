#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin -pa _build/test/lib/*/ebin -pa _build/default/lib/cowboy/ebin -pa _build/default/lib/gun/ebin -pa _build/default/lib/jsx/ebin -pa _build/default/lib/ranch/ebin -pa _build/default/lib/cowlib/ebin -pa apps/erlmcp_observability/ebin

main(_) ->
    io:format("Starting dashboard tests...~n"),

    % Start required applications
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(gun),
    {ok, _} = application:ensure_all_started(jsx),

    % Start erlmcp_observability
    {ok, _} = application:ensure_all_started(erlmcp_observability),

    io:format("Applications started~n"),

    % Compile and load the test module
    {ok, _} = compile:file("apps/erlmcp_observability/test/erlmcp_dashboard_tests.erl",
        [{i, "include"}, {i, "apps/erlmcp_observability/include"}]),

    io:format("Test module compiled~n"),

    % Run the tests
    eunit:test(erlmcp_dashboard_tests, [verbose]),

    io:format("Tests complete~n"),
    init:stop().
