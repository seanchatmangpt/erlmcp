#!/usr/bin/env escript

main(_) ->
    code:add_pathsz(filelib:wildcard("_build/default/lib/*/ebin")),
    code:add_path("_build/default/lib/erlmcp_core/ebin"),
    
    %% Compile test module
    compile:file("test/erlmcp_registry_dist_tests.erl", [
        {outdir, "_build/test/lib/erlmcp_core/eunit"},
        {i, "include"},
        debug_info,
        return_errors
    ]),
    
    %% Run tests
    eunit:test(erlmcp_registry_dist_tests, [verbose]).
