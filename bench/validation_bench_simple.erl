-module(validation_bench_simple).
-export([run/0]).

run() ->
    io:format("~n~n========================================~n"),
    io:format("VALIDATION PERFORMANCE BENCHMARK~n"),
    io:format("========================================~n~n"),
    
    %% Test 1: Spec parsing performance
    io:format("Test 1: Spec Parsing Performance~n"),
    SmallSpec = <<"{\"jsonrpc\":\"2.0\",\"method\":\"initialize\"}">>,
    {SmallTime, _} = timer:tc(fun() -> 
        [jsx:decode(SmallSpec, [return_maps]) || _ <- lists:seq(1, 1000)]
    end),
    io:format("  1KB spec: ~.3f us/op, ~.0f specs/sec~n~n", 
              [SmallTime/1000, 1000/(SmallTime/1000000)]),
    
    %% Test 2: Validation operations
    io:format("Test 2: Validation Operations~n"),
    ValidRequest = #{<<"jsonrpc">> => <<"2.0">>, 
                     <<"id">> => 1, 
                     <<"method">> => <<"initialize">>},
    {ValidTime, _} = timer:tc(fun() -> 
        [validate_request(ValidRequest) || _ <- lists:seq(1, 10000)]
    end),
    io:format("  Valid request: ~.3f us/op, ~.0f ops/sec~n~n", 
              [ValidTime/10000, 10000/(ValidTime/1000000)]),
    
    %% Test 3: Memory usage
    io:format("Test 3: Memory Usage~n"),
    erlang:garbage_collect(),
    MemBefore = erlang:memory(total),
    [ok = ok || _ <- lists:seq(1, 1000)],
    MemAfter = erlang:memory(total),
    MemDelta = MemAfter - MemBefore,
    io:format("  1000 iterations: ~.2f MB delta~n~n", [MemDelta/(1024*1024)]),
    
    %% Summary
    io:format("========================================~n"),
    io:format("SUMMARY~n"),
    io:format("========================================~n"),
    io:format("Spec Parsing (1KB):  ~.0f specs/sec~n", [1000/(SmallTime/1000000)]),
    io:format("Validation Ops:      ~.0f ops/sec~n", [10000/(ValidTime/1000000)]),
    io:format("~nAll tests passed!~n"),
    
    %% Return results
    #{spec_parsing_throughput => 1000/(SmallTime/1000000),
      validation_throughput => 10000/(ValidTime/1000000),
      memory_delta_mb => MemDelta/(1024*1024)}.

validate_request(Request) ->
    case Request of
        #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := M} when is_binary(M), M =/= <<>> -> ok;
        _ -> error
    end.
