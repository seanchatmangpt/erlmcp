#!/usr/bin/env escript

main(_) ->
    code:add_patha("_build/test/lib/erlmcp_core/ebin"),
    code:add_patha("_build/test/lib/jsx/ebin"),
    code:add_patha("_build/test/lib/gproc/ebin"),
    code:add_patha("_build/test/lib/jesse/ebin"),
    code:add_patha("_build/test/lib/erlmcp_validation/test"),

    io:format("Starting applications...~n"),
    {ok, _} = application:ensure_all_started(gproc),
    {ok, _} = application:ensure_all_started(jsx),
    {ok, _} = application:ensure_all_started(jesse),

    io:format("Starting erlmcp_core...~n"),
    case application:ensure_all_started(erlmcp_core) of
        {ok, _} ->
            io:format("erlmcp_core started successfully~n"),
            io:format("Registry: ~p~n", [whereis(erlmcp_registry)]),
            io:format("Sup: ~p~n", [whereis(erlmcp_sup)]),
            io:format("Core sup: ~p~n", [whereis(erlmcp_core_sup)]);
        {error, Reason} ->
            io:format("Failed to start erlmcp_core: ~p~n", [Reason])
    end,

    init:stop().
