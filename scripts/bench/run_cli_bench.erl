#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

%%%-------------------------------------------------------------------
%%% @doc Standalone CLI Benchmark Runner
%%% @end
%%%-------------------------------------------------------------------

main([]) ->
    io:format("~n========================================~n"),
    io:format("ERLMCP CLI PERFORMANCE BENCHMARK~n"),
    io:format("========================================~n~n"),
    
    %% Ensure modules are loaded
    case code:ensure_loaded(erlmcp_cli_startup_bench) of
        {module, _} ->
            io:format("Running startup benchmark (10 iterations)...~n~n"),
            erlmcp_cli_startup_bench:run(#{iterations => 10}),
            io:format("~n✓ Startup benchmark complete~n~n");
        {error, Reason} ->
            io:format("Error: Could not load erlmcp_cli_startup_bench: ~p~n", [Reason]),
            io:format("Please ensure project is compiled: make compile~n"),
            halt(1)
    end,
    
    case code:ensure_loaded(erlmcp_cli_command_bench) of
        {module, _} ->
            io:format("Running command benchmark (5 iterations)...~n~n"),
            erlmcp_cli_command_bench:run(#{iterations => 5}),
            io:format("~n✓ Command benchmark complete~n~n");
        {error, Reason} ->
            io:format("Error: Could not load erlmcp_cli_command_bench: ~p~n", [Reason]),
            halt(1)
    end,
    
    io:format("========================================~n"),
    io:format("All benchmarks complete!~n"),
    io:format("Results in: bench/results/~n"),
    io:format("========================================~n~n"),
    
    halt(0);

main(_Args) ->
    io:format("Usage: ~s~n", [escript:script_name()]),
    halt(1).
