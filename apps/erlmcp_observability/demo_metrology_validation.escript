#!/usr/bin/env escript
%% Demo: Metrology Validator Integration

main(_) ->
    io:format("~n=== Metrology Validator Demo ===~n~n"),
    
    %% Case 1: Valid benchmark output (core_ops_100k)
    io:format("Case 1: Valid benchmark output~n"),
    Valid = #{
        workload_id => <<"core_ops_100k">>,
        transport => <<"stdio">>,
        duration_s => 10,
        scope => <<"per_node_total">>,
        precision => <<"microsecond">>,
        throughput_msg_per_s => 2690000,
        latency_p50_us => 1,
        latency_p95_us => 5,
        latency_p99_us => 10,
        memory_heap_mib_per_conn => 0.5,
        memory_rss_mib_per_node => 256
    },
    
    case erlmcp_metrology_validator:validate_benchmark_output(Valid) of
        {ok, Details} ->
            io:format("  Status: VALID~n"),
            io:format("  Details: ~p~n~n", [Details]);
        {error, Errors} ->
            io:format("  Status: INVALID~n"),
            io:format("  Errors: ~p~n~n", [Errors])
    end,
    
    %% Case 2: Ambiguous units (req/s, latency_ms, bare memory)
    io:format("Case 2: Ambiguous units (legacy format)~n"),
    Invalid = #{
        workload_id => <<"legacy_benchmark">>,
        transport => <<"tcp">>,
        duration_s => 60,
        scope => <<"per_connection_total">>,
        <<\"req/s\">> => 100000,
        <<\"latency_ms\">> => 15,
        <<\"memory\">> => 512
    },
    
    case erlmcp_metrology_validator:validate_benchmark_output(Invalid) of
        {ok, _} ->
            io:format("  Status: VALID (unexpected!)~n~n");
        {error, Errors} ->
            io:format("  Status: INVALID~n"),
            io:format("  Violations detected: ~p~n", [length(Errors)]),
            lists:foreach(fun(E) -> io:format("    - ~s~n", [E]) end, Errors),
            io:format("~n")
    end,
    
    %% Case 3: Missing required fields
    io:format("Case 3: Missing required fields~n"),
    MissingFields = #{
        throughput_msg_per_s => 500000
    },
    
    Report = erlmcp_metrology_validator:generate_violations_report(MissingFields),
    io:format("  Valid: ~p~n", [maps:get(valid, Report)]),
    io:format("  Total violations: ~p~n", [maps:get(total_violations, Report)]),
    Violations = maps:get(violations, Report),
    lists:foreach(fun(V) ->
        Type = maps:get(type, V),
        Reason = maps:get(reason, V),
        io:format("    [~s] ~s~n", [Type, Reason])
    end, Violations),
    io:format("~n"),
    
    %% Case 4: Invalid scope
    io:format("Case 4: Invalid scope~n"),
    BadScope = maps:put(scope, <<"invalid_scope">>, Valid),
    
    case erlmcp_metrology_validator:validate_scope(BadScope) of
        {ok, _} ->
            io:format("  Status: VALID (unexpected!)~n~n");
        {error, [Error]} ->
            io:format("  Status: INVALID~n"),
            io:format("  Error: ~s~n~n", [Error])
    end,
    
    %% Show validation rules
    io:format("Validation Rules Summary:~n"),
    Rules = erlmcp_metrology_validator:get_validation_rules(),
    Throughput = maps:get(throughput, Rules),
    io:format("  Throughput canonical: ~s~n", [maps:get(canonical, Throughput)]),
    Latency = maps:get(latency, Rules),
    io:format("  Latency canonical: ~s~n", [maps:get(canonical, Latency)]),
    io:format("  Latency percentiles: ~p~n", [maps:get(percentiles, Latency)]),
    Memory = maps:get(memory, Rules),
    io:format("  Memory scopes: ~p~n", [length(maps:get(scopes, Memory))]),
    io:format("~n"),
    
    io:format("=== Demo Complete ===~n").
