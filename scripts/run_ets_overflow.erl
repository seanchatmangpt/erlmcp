#!/usr/bin/env escript
%%% ETS Table Overflow Destructive Test
-mode(compile).

main(_) ->
    io:format("====================================================================~n"),
    io:format("ETS TABLE OVERFLOW DESTRUCTIVE STRESS TEST~n"),
    io:format("====================================================================~n~n"),
    
    % Compile module
    io:format("Compiling ETS overflow benchmark...~n"),
    case compile:file("bench/erlmcp_bench_ets_overflow.erl", 
                     [{i, "include"}, {outdir, "ebin"}]) of
        {ok, _} -> ok;
        {error, Errors, Warnings} ->
            io:format("Compilation errors: ~p~n", [Errors]),
            io:format("Warnings: ~p~n", [Warnings]),
            halt(1)
    end,
    
    % Add ebin to path
    true = code:add_patha("ebin"),
    
    % Run tests for each table type
    Results = lists:map(fun(TableType) ->
        io:format("~n====================================================================~n"),
        io:format("Testing: ~p~n", [TableType]),
        io:format("====================================================================~n~n"),
        
        Result = erlmcp_bench_ets_overflow:run(TableType, #{
            max_records => 10_000_000,  % Limit for faster testing
            checkpoint_interval => 50_000
        }),
        
        {TableType, Result}
    end, [set, ordered_set, bag]),
    
    % Print summary
    io:format("~n====================================================================~n"),
    io:format("SUMMARY OF ALL TABLE TYPES~n"),
    io:format("====================================================================~n~n"),
    
    lists:foreach(fun({TableType, Result}) ->
        BP = maps:get(<<"breaking_point">>, Result),
        Perf = maps:get(<<"performance_degradation">>, Result),
        
        io:format("~p:~n", [TableType]),
        io:format("  Records: ~p~n", [maps:get(<<"record_count">>, BP)]),
        io:format("  Memory: ~.2f GB~n", [maps:get(<<"memory_gb">>, BP)]),
        io:format("  Slowdown: ~.2fx~n", [maps:get(<<"slowdown_factor">>, Perf)]),
        io:format("~n")
    end, Results),
    
    % Save results
    Timestamp = erlang:system_time(millisecond),
    JsonFile = io_lib:format("bench/results/ets_overflow/results_~p.json", [Timestamp]),
    file:write_file(JsonFile, jsx:encode(Results, [space, indent])),
    io:format("Results saved to: ~s~n", [JsonFile]),
    
    ok.
