#!/usr/bin/env escript
%%% Binary Heap Exhaustion Test Runner
%%% DESTRUCTIVE TEST - Will crash the VM

main([]) ->
    main(["standard"]);
main([Arg]) ->
    io:format("~n========================================================================~n"),
    io:format("DESTRUCTIVE STRESS TEST #6: Binary Heap Exhaustion~n"),
    io:format("========================================================================~n~n"),
    io:format("WARNING: This test will allocate binary heap until VM crash.~n"),
    io:format("Do NOT run in production environments!~n~n"),

    %% Add ebin paths
    EbinPath = filename:absname("_build/default/lib/erlmcp/ebin"),
    true = code:add_patha(EbinPath),

    %% Run test based on argument
    TestType = case Arg of
        "quick" -> quick_test;
        "crash" -> crash_test;
        _ -> standard
    end,

    io:format("Starting ~p test...~n~n", [TestType]),

    %% Run the test
    try
        Result = case TestType of
            quick_test -> erlmcp_bench_binary_exhaustion:quick_test();
            crash_test -> erlmcp_bench_binary_exhaustion:crash_test();
            standard -> erlmcp_bench_binary_exhaustion:run()
        end,

        io:format("~nTest completed successfully.~n"),
        erlmcp_bench_binary_exhaustion:print_report(Result),

        %% Save report
        Timestamp = integer_to_list(erlang:system_time(second)),
        ReportFile = "bench/results/binary_exhaustion_report_" ++ Timestamp ++ ".json",
        ok = filelib:ensure_dir(ReportFile),
        {ok, F} = file:open(ReportFile, [write]),
        io:format(F, "~p~n", [Result]),
        file:close(F),

        io:format("~nReport saved to: ~s~n", [ReportFile]),
        halt(0)
    catch
        Class:Reason:Stacktrace ->
            io:format("~n~n*** TEST CRASHED (as expected) ***~n"),
            io:format("Class: ~p~n", [Class]),
            io:format("Reason: ~p~n", [Reason]),
            io:format("Stacktrace: ~p~n", [Stacktrace]),
            io:format("~nThis crash is EXPECTED behavior for the exhaustion test.~n"),
            io:format("The binary heap was successfully exhausted.~n"),
            halt(1)
    end.
