#!/usr/bin/env escript

%% Simple demonstration of erlmcp_simple_metrics
%% Usage: escript examples/simple_metrics_demo.erl
main(_) ->
    % Add the ebin directory to the code path
    code:add_path("ebin"),
    
    io:format("=== erlmcp_simple_metrics Demo ===~n~n"),
    
    % Start the metrics system
    erlmcp_simple_metrics:start(),
    
    % Simulate some API requests
    io:format("Simulating API requests...~n"),
    simulate_requests(10),
    
    % Show current stats
    Stats = erlmcp_simple_metrics:get_stats(),
    print_stats(Stats),
    
    % Cleanup
    erlmcp_simple_metrics:stop(),
    io:format("Demo completed.~n").

%% Simulate some requests with random latencies and outcomes
simulate_requests(0) -> ok;
simulate_requests(N) ->
    % Random latency between 10-100ms
    Latency = 10 + rand:uniform(90),
    
    % 80% success rate
    case rand:uniform(10) of
        X when X =< 8 ->
            erlmcp_simple_metrics:request(),
            erlmcp_simple_metrics:success(),
            erlmcp_simple_metrics:record_latency(api_request, Latency);
        _ ->
            erlmcp_simple_metrics:request(),
            erlmcp_simple_metrics:error(),
            erlmcp_simple_metrics:record_latency(api_request, Latency * 2)  % errors take longer
    end,
    
    % Also simulate some JSON operations
    JsonLatency = 1 + rand:uniform(5),
    erlmcp_simple_metrics:record_latency(json_parse, JsonLatency),
    
    simulate_requests(N - 1).

%% Pretty print stats
print_stats(Stats) ->
    io:format("~n=== Current Metrics ===~n"),
    
    % Counters
    Counters = maps:get(counters, Stats),
    io:format("Counters:~n"),
    lists:foreach(fun({Name, Value}) ->
        io:format("  ~-20s: ~p~n", [atom_to_list(Name), Value])
    end, lists:sort(maps:to_list(Counters))),
    
    % Latencies
    Latencies = maps:get(latencies, Stats),
    io:format("~nLatency Stats:~n"),
    lists:foreach(fun({Operation, OpStats}) ->
        Count = maps:get(count, OpStats),
        Avg = maps:get(avg_ms, OpStats),
        Min = maps:get(min_ms, OpStats),
        Max = maps:get(max_ms, OpStats),
        io:format("  ~-20s: ~3w calls, avg ~5.1fms (min ~5.1f, max ~5.1f)~n", 
                  [atom_to_list(Operation), Count, float(Avg), float(Min), float(Max)])
    end, lists:sort(maps:to_list(Latencies))),
    
    % System
    System = maps:get(system, Stats),
    Uptime = maps:get(uptime_ms, System),
    Memory = maps:get(memory_total, System),
    Processes = maps:get(process_count, System),
    io:format("~nSystem Info:~n"),
    io:format("  Uptime: ~wms~n", [Uptime]),
    io:format("  Memory: ~w bytes (~.1f MB)~n", [Memory, Memory/1024/1024]),
    io:format("  Processes: ~w~n", [Processes]),
    
    io:format("~n").