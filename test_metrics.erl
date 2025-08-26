-module(test_metrics).
-export([test/0]).

test() ->
    erlmcp_simple_metrics:start(),
    
    % Simulate some operations
    erlmcp_simple_metrics:request(),
    erlmcp_simple_metrics:success(),
    erlmcp_simple_metrics:record_latency(api_request, 45),
    
    erlmcp_simple_metrics:request(),
    erlmcp_simple_metrics:error(),
    erlmcp_simple_metrics:record_latency(api_request, 120),
    
    erlmcp_simple_metrics:request(),
    erlmcp_simple_metrics:success(),
    erlmcp_simple_metrics:record_latency(api_request, 23),
    
    % Get stats
    Stats = erlmcp_simple_metrics:get_stats(),
    io:format("Metrics System Working\!~n"),
    io:format("Stats: ~p~n", [Stats]),
    
    erlmcp_simple_metrics:stop(),
    init:stop().
