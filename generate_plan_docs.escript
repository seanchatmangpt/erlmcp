#!/usr/bin/env escript
%% Generate plan documentation files from plan specifications

main(_) ->
    code:add_path("_build/default/lib/jsx/ebin"),
    code:add_path("_build/default/ebin"),

    case erlmcp_plan_docs_generator:generate_all_docs() of
        {ok, Files} ->
            io:format("Generated documentation files:~n", []),
            [io:format("  - ~s~n", [F]) || F <- Files],
            io:format("~nAll documentation generated successfully!~n"),
            halt(0);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason]),
            halt(1)
    end.
