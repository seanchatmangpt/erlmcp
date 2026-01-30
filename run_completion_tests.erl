#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/erlmcp_core/ebin -pa _build/test/lib/erlmcp_core/ebin

main(_) ->
    code:add_patha("_build/default/lib/erlmcp_core/ebin"),
    code:add_patha("_build/test/lib/erlmcp_core/ebin"),
    code:add_patha("_build/default/lib/gun/ebin"),
    code:add_patha("_build/default/lib/cowboy/ebin"),
    code:add_patha("_build/default/lib/ranch/ebin"),
    code:add_patha("_build/default/lib/jsx/ebin"),
    code:add_patha("_build/default/lib/jesse/ebin"),
    code:add_patha("_build/default/lib/gproc/ebin"),
    code:add_patha("_build/default/lib/poolboy/ebin"),

    eunit:test(erlmcp_completion_tests, [verbose]),
    init:stop().
