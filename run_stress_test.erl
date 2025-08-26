#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/erlmcp/ebin -pa test

%% Simple script to run the stress tests
main([]) ->
    main(["all"]);
main(["all"]) ->
    io:format("Running all ERLMCP stress tests...~n~n"),
    
    %% Add paths
    code:add_path("_build/default/lib/erlmcp/ebin"),
    code:add_path("test"),
    code:add_path("include"),
    
    %% Compile and load the test module
    case compile:file("test/erlmcp_simple_stress.erl", [
        {outdir, "test"},
        {i, "include"},
        return_errors,
        return_warnings
    ]) of
        {ok, _, _} ->
            io:format("Stress test module compiled successfully~n~n"),
            
            %% Run the tests
            try
                erlmcp_simple_stress:run_all()
            catch
                Class:Reason:Stacktrace ->
                    io:format("Error running tests: ~p:~p~n", [Class, Reason]),
                    io:format("Stacktrace: ~p~n", [Stacktrace]),
                    halt(1)
            end;
        {error, Errors, Warnings} ->
            io:format("Compilation failed:~n"),
            [io:format("Error: ~p~n", [E]) || E <- Errors],
            [io:format("Warning: ~p~n", [W]) || W <- Warnings],
            halt(1)
    end;

main(["flood"]) ->
    run_single_test(flood_test);
main(["connection"]) ->
    run_single_test(connection_test);
main(["memory"]) ->
    run_single_test(memory_test);

main([Other]) ->
    io:format("Usage: ~n"),
    io:format("  ./run_stress_test.erl [all|flood|connection|memory]~n"),
    io:format("  Unknown option: ~p~n", [Other]),
    halt(1).

run_single_test(TestFun) ->
    %% Add paths
    code:add_path("_build/default/lib/erlmcp/ebin"),
    code:add_path("test"),
    code:add_path("include"),
    
    %% Compile and load the test module
    case compile:file("test/erlmcp_simple_stress.erl", [
        {outdir, "test"},
        {i, "include"},
        return_errors,
        return_warnings
    ]) of
        {ok, _, _} ->
            io:format("Running ~p test...~n~n", [TestFun]),
            try
                Result = erlmcp_simple_stress:TestFun(),
                io:format("~nTest completed: ~p~n", [Result])
            catch
                Class:Reason:Stacktrace ->
                    io:format("Error running test: ~p:~p~n", [Class, Reason]),
                    io:format("Stacktrace: ~p~n", [Stacktrace]),
                    halt(1)
            end;
        {error, Errors, Warnings} ->
            io:format("Compilation failed:~n"),
            [io:format("Error: ~p~n", [E]) || E <- Errors],
            [io:format("Warning: ~p~n", [W]) || W <- Warnings],
            halt(1)
    end.