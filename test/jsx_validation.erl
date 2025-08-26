-module(jsx_validation).
-export([run/0]).

run() ->
    %% Start jsx application
    application:start(jsx),
    
    io:format("=== JSX Production Validation Test ===~n"),
    
    %% Test 1: Basic functionality
    TestData1 = #{<<"key">> => <<"value">>, <<"number">> => 42, <<"flag">> => true},
    Encoded1 = jsx:encode(TestData1),
    Decoded1 = jsx:decode(Encoded1),
    Test1Result = is_map(Decoded1) andalso maps:size(Decoded1) == 3,
    io:format("Test 1 - Basic functionality: ~p~n", [Test1Result]),
    
    %% Test 2: MCP message structure
    McpMsg = #{
        <<"jsonrpc">> => <<"2.0">>, 
        <<"id">> => 1, 
        <<"method">> => <<"tools/list">>,
        <<"params">> => #{}
    },
    EncodedMcp = jsx:encode(McpMsg),
    DecodedMcp = jsx:decode(EncodedMcp),
    Test2Result = maps:get(<<"jsonrpc">>, DecodedMcp) == <<"2.0">>,
    io:format("Test 2 - MCP message: ~p~n", [Test2Result]),
    
    %% Test 3: Performance with larger data
    StartTime = erlang:monotonic_time(microsecond),
    LargeData = #{<<"items">> => [I || I <- lists:seq(1, 100)]},
    jsx:encode(LargeData),
    jsx:decode(jsx:encode(LargeData)),
    EndTime = erlang:monotonic_time(microsecond),
    Duration = EndTime - StartTime,
    Test3Result = Duration < 100000,  % Less than 100ms
    io:format("Test 3 - Performance (100 items): ~p microseconds (~p)~n", [Duration, Test3Result]),
    
    %% Test 4: Error handling
    Test4Result = try
        jsx:decode(<<"{invalid json}">>),
        false
    catch
        _:_ -> 
            true
    end,
    io:format("Test 4 - Error handling: ~p~n", [Test4Result]),
    
    %% Test 5: Nested structures
    NestedData = #{
        <<"outer">> => #{
            <<"inner">> => [1, 2, 3],
            <<"deep">> => #{
                <<"value">> => <<"nested">>
            }
        }
    },
    NestedEncoded = jsx:encode(NestedData),
    NestedDecoded = jsx:decode(NestedEncoded),
    Test5Result = NestedData == NestedDecoded,
    io:format("Test 5 - Nested structures: ~p~n", [Test5Result]),
    
    %% Test 6: Concurrent operations
    Self = self(),
    Workers = [spawn(fun() ->
        WorkerResult = try
            Data = #{<<"worker">> => I, <<"data">> => [1,2,3]},
            jsx:encode(Data),
            jsx:decode(jsx:encode(Data)),
            true
        catch
            _:_ -> false
        end,
        Self ! {worker, I, WorkerResult}
    end) || I <- lists:seq(1, 10)],
    
    ConcurrentResults = [receive 
        {worker, _, Result} -> Result 
    after 5000 -> 
        false 
    end || _ <- Workers],
    
    Test6Result = lists:all(fun(R) -> R == true end, ConcurrentResults),
    io:format("Test 6 - Concurrent operations: ~p~n", [Test6Result]),
    
    %% Summary
    AllTests = [Test1Result, Test2Result, Test3Result, Test4Result, Test5Result, Test6Result],
    SuccessCount = length([T || T <- AllTests, T == true]),
    TotalTests = length(AllTests),
    
    io:format("~n=== JSX Validation Summary ===~n"),
    io:format("Passed: ~p/~p tests~n", [SuccessCount, TotalTests]),
    
    case SuccessCount == TotalTests of
        true ->
            io:format("JSX INTEGRATION: PRODUCTION READY ✓~n");
        false ->
            io:format("JSX INTEGRATION: ISSUES FOUND ✗~n")
    end,
    
    io:format("=== JSX Production Validation: COMPLETED ===~n"),
    ok.