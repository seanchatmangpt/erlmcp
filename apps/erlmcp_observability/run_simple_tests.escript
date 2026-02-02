#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ebin

main(_) ->
    eunit:test(erlmcp_observability_supervisor_simple_tests, [verbose]),
    init:stop().
