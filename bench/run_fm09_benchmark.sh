#!/bin/bash
# Run FM-09 DoS Recovery Benchmark

cd /home/user/erlmcp

# Compile and run benchmark
erl -pa _build/default/lib/*/ebin -pa bench -noshell -eval '
    %% Compile benchmark module
    io:format("Compiling erlmcp_bench_fm09_dos_recovery...~n"),
    case compile:file("bench/erlmcp_bench_fm09_dos_recovery.erl", [
        {outdir, "bench"},
        {i, "apps/erlmcp_core/include"},
        return_errors
    ]) of
        {ok, _} ->
            io:format("Compilation successful~n~n"),
            
            %% Add bench to code path
            code:add_patha("bench"),
            
            %% Run benchmark
            io:format("Running FM-09 DoS Recovery Benchmark...~n~n"),
            Results = erlmcp_bench_fm09_dos_recovery:run_all(),
            
            %% Write results to JSON
            io:format("~n~nWriting results to bench/FM09_DOS_RECOVERY_RESULTS.json...~n"),
            JSON = jsx:encode(Results, [space, indent]),
            file:write_file("bench/FM09_DOS_RECOVERY_RESULTS.json", JSON),
            
            io:format("Done!~n"),
            halt(0);
        {error, Errors, Warnings} ->
            io:format("Compilation failed:~n"),
            io:format("Errors: ~p~n", [Errors]),
            io:format("Warnings: ~p~n", [Warnings]),
            halt(1)
    end.
' -s init stop
