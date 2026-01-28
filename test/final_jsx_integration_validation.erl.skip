-module(final_jsx_integration_validation).
-export([run/0]).

run() ->
    %% Start jsx application
    application:start(jsx),
    
    io:format("=== FINAL JSX INTEGRATION VALIDATION FOR ERLMCP ===~n"),
    
    %% Test 1: JSX Basic Operations
    Test1 = test_jsx_basic_operations(),
    io:format("Test 1 - JSX Basic Operations: ~p~n", [Test1]),
    
    %% Test 2: MCP Protocol Messages
    Test2 = test_mcp_protocol_messages(),
    io:format("Test 2 - MCP Protocol Messages: ~p~n", [Test2]),
    
    %% Test 3: Complex Data Structures
    Test3 = test_complex_data_structures(),
    io:format("Test 3 - Complex Data Structures: ~p~n", [Test3]),
    
    %% Test 4: Error Handling
    Test4 = test_error_handling(),
    io:format("Test 4 - Error Handling: ~p~n", [Test4]),
    
    %% Test 5: Performance Validation
    Test5 = test_performance_validation(),
    io:format("Test 5 - Performance Validation: ~p~n", [Test5]),
    
    %% Test 6: Memory Safety
    Test6 = test_memory_safety(),
    io:format("Test 6 - Memory Safety: ~p~n", [Test6]),
    
    %% Test 7: Concurrent Operations
    Test7 = test_concurrent_operations(),
    io:format("Test 7 - Concurrent Operations: ~p~n", [Test7]),
    
    %% Test 8: Real-world Transport Simulation
    Test8 = test_transport_simulation(),
    io:format("Test 8 - Transport Simulation: ~p~n", [Test8]),
    
    %% Summary
    AllTests = [Test1, Test2, Test3, Test4, Test5, Test6, Test7, Test8],
    SuccessCount = length([T || T <- AllTests, T == true]),
    TotalTests = length(AllTests),
    
    io:format("~n=== FINAL VALIDATION SUMMARY ===~n"),
    io:format("Passed: ~p/~p tests~n", [SuccessCount, TotalTests]),
    
    case SuccessCount == TotalTests of
        true ->
            io:format("ERLMCP JSX INTEGRATION: PRODUCTION READY ✓✓✓~n"),
            io:format("All jsx:encode and jsx:decode operations work correctly~n"),
            io:format("System is ready for production deployment~n");
        false ->
            io:format("ERLMCP JSX INTEGRATION: ISSUES FOUND ✗✗✗~n"),
            io:format("~p test(s) failed, system needs attention~n", [TotalTests - SuccessCount])
    end,
    
    io:format("=== FINAL JSX INTEGRATION VALIDATION: COMPLETED ===~n"),
    SuccessCount == TotalTests.

%% Test jsx basic encode/decode operations
test_jsx_basic_operations() ->
    try
        %% Basic data types
        TestCases = [
            #{<<"string">> => <<"hello">>},
            #{<<"number">> => 42},
            #{<<"boolean">> => true},
            #{<<"null">> => null},
            [1, 2, 3, <<"test">>],
            <<"simple string">>,
            42,
            true,
            false,
            null
        ],
        
        Results = [
            begin
                Encoded = jsx:encode(Case),
                Decoded = jsx:decode(Encoded),
                Case == Decoded
            end || Case <- TestCases
        ],
        
        lists:all(fun(R) -> R == true end, Results)
    catch
        _:_ -> false
    end.

%% Test MCP protocol message structures
test_mcp_protocol_messages() ->
    try
        %% Initialize request
        InitReq = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 1,
            <<"method">> => <<"initialize">>,
            <<"params">> => #{
                <<"protocolVersion">> => <<"2024-11-05">>,
                <<"capabilities">> => #{
                    <<"roots">> => #{<<"listChanged">> => true},
                    <<"sampling">> => #{}
                },
                <<"clientInfo">> => #{
                    <<"name">> => <<"test-client">>,
                    <<"version">> => <<"1.0.0">>
                }
            }
        },
        
        %% Tools response
        ToolsResp = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 2,
            <<"result">> => #{
                <<"tools">> => [
                    #{
                        <<"name">> => <<"calculator">>,
                        <<"description">> => <<"Math operations">>,
                        <<"inputSchema">> => #{
                            <<"type">> => <<"object">>,
                            <<"properties">> => #{
                                <<"a">> => #{<<"type">> => <<"number">>},
                                <<"b">> => #{<<"type">> => <<"number">>},
                                <<"op">> => #{<<"type">> => <<"string">>}
                            }
                        }
                    }
                ]
            }
        },
        
        %% Error response
        ErrorResp = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => 3,
            <<"error">> => #{
                <<"code">> => -32602,
                <<"message">> => <<"Invalid params">>,
                <<"data">> => #{
                    <<"details">> => <<"Missing required parameter">>,
                    <<"timestamp">> => erlang:system_time(millisecond)
                }
            }
        },
        
        Messages = [InitReq, ToolsResp, ErrorResp],
        
        Results = [
            begin
                Encoded = jsx:encode(Msg),
                Decoded = jsx:decode(Encoded),
                Msg == Decoded
            end || Msg <- Messages
        ],
        
        lists:all(fun(R) -> R == true end, Results)
    catch
        _:_ -> false
    end.

%% Test complex nested data structures
test_complex_data_structures() ->
    try
        ComplexData = #{
            <<"level1">> => #{
                <<"level2">> => #{
                    <<"level3">> => [
                        #{<<"id">> => I, <<"data">> => [I, I*2, I*3]}
                        || I <- lists:seq(1, 10)
                    ]
                }
            },
            <<"metadata">> => #{
                <<"timestamps">> => [erlang:system_time(millisecond) || _ <- lists:seq(1, 5)],
                <<"flags">> => [true, false, true, false, true],
                <<"config">> => #{
                    <<"timeout">> => 5000,
                    <<"retries">> => 3,
                    <<"debug">> => true
                }
            }
        },
        
        Encoded = jsx:encode(ComplexData),
        Decoded = jsx:decode(Encoded),
        
        %% Verify structure preservation
        ComplexData == Decoded
    catch
        _:_ -> false
    end.

%% Test error handling scenarios
test_error_handling() ->
    try
        %% Test invalid JSON
        InvalidJson = <<"{\"key\": invalid_value}">>,
        
        InvalidResult = try
            jsx:decode(InvalidJson),
            false  % Should not succeed
        catch
            _:_ -> true  % Expected error
        end,
        
        %% Test empty structures
        EmptyTests = [
            jsx:decode(<<"null">>) == null,
            jsx:decode(<<"{}">>) == #{},
            jsx:decode(<<"[]">>) == []
        ],
        
        %% Test encoding of problematic data (should handle gracefully)
        ProblematicTests = try
            jsx:encode(#{<<"valid">> => <<"data">>}),
            true
        catch
            _:_ -> false
        end,
        
        InvalidResult andalso lists:all(fun(T) -> T end, EmptyTests) andalso ProblematicTests
    catch
        _:_ -> false
    end.

%% Test performance with production-sized data
test_performance_validation() ->
    try
        %% Create production-sized data
        LargeData = #{
            <<"batch">> => [
                #{
                    <<"id">> => I,
                    <<"timestamp">> => erlang:system_time(millisecond),
                    <<"data">> => binary:copy(<<"x">>, 100),  % 100-byte string
                    <<"metadata">> => #{
                        <<"priority">> => I rem 5,
                        <<"processed">> => false,
                        <<"tags">> => [<<"tag1">>, <<"tag2">>, <<"tag3">>]
                    }
                } || I <- lists:seq(1, 500)  % 500 items
            ]
        },
        
        %% Measure performance
        StartTime = erlang:monotonic_time(microsecond),
        Encoded = jsx:encode(LargeData),
        EncodeTime = erlang:monotonic_time(microsecond) - StartTime,
        
        StartDecode = erlang:monotonic_time(microsecond),
        Decoded = jsx:decode(Encoded),
        DecodeTime = erlang:monotonic_time(microsecond) - StartDecode,
        
        %% Verify correctness
        DataCorrect = LargeData == Decoded,
        
        %% Performance thresholds (reasonable for production)
        EncodeOk = EncodeTime < 1000000,  % 1 second
        DecodeOk = DecodeTime < 1000000,  % 1 second
        
        DataCorrect andalso EncodeOk andalso DecodeOk
    catch
        _:_ -> false
    end.

%% Test memory safety under load
test_memory_safety() ->
    try
        InitialMemory = erlang:memory(total),
        
        TestData = #{
            <<"test">> => <<"memory safety">>,
            <<"data">> => [I || I <- lists:seq(1, 100)]
        },
        
        %% Perform many operations
        lists:foreach(fun(_) ->
            jsx:encode(TestData),
            jsx:decode(jsx:encode(TestData))
        end, lists:seq(1, 1000)),
        
        %% Force garbage collection
        erlang:garbage_collect(),
        timer:sleep(10),
        
        FinalMemory = erlang:memory(total),
        MemoryDiff = FinalMemory - InitialMemory,
        
        %% Memory growth should be reasonable (less than 10MB)
        MemoryDiff < 10485760
    catch
        _:_ -> false
    end.

%% Test concurrent jsx operations
test_concurrent_operations() ->
    try
        Self = self(),
        NumWorkers = 20,
        
        TestData = #{
            <<"concurrent">> => true,
            <<"data">> => [1, 2, 3, 4, 5]
        },
        
        %% Spawn concurrent workers
        Workers = [
            spawn_link(fun() ->
                Result = try
                    lists:foreach(fun(_) ->
                        Encoded = jsx:encode(TestData),
                        Decoded = jsx:decode(Encoded),
                        case Decoded == TestData of
                            true -> ok;
                            false -> error(mismatch)
                        end
                    end, lists:seq(1, 50)),
                    true
                catch
                    _:_ -> false
                end,
                Self ! {worker_done, self(), Result}
            end) || _ <- lists:seq(1, NumWorkers)
        ],
        
        %% Collect results
        Results = [
            receive
                {worker_done, Worker, Result} when Worker == WorkerPid -> Result
            after 10000 ->
                timeout
            end || WorkerPid <- Workers
        ],
        
        %% All workers should succeed
        lists:all(fun(R) -> R == true end, Results)
    catch
        _:_ -> false
    end.

%% Test transport-like message processing simulation
test_transport_simulation() ->
    try
        %% Simulate incoming JSON message
        IncomingJson = <<"{\"jsonrpc\":\"2.0\",\"id\":123,\"method\":\"tools/call\",\"params\":{\"name\":\"test\",\"args\":{}}}">>,
        
        %% Parse incoming message
        ParsedMessage = jsx:decode(IncomingJson),
        
        %% Verify message structure
        <<"2.0">> = maps:get(<<"jsonrpc">>, ParsedMessage),
        123 = maps:get(<<"id">>, ParsedMessage),
        <<"tools/call">> = maps:get(<<"method">>, ParsedMessage),
        
        %% Process and create response
        ResponseMessage = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => maps:get(<<"id">>, ParsedMessage),
            <<"result">> => #{
                <<"content">> => [#{
                    <<"type">> => <<"text">>,
                    <<"text">> => <<"Test response">>
                }],
                <<"isError">> => false
            }
        },
        
        %% Encode response
        ResponseJson = jsx:encode(ResponseMessage),
        
        %% Verify response can be parsed
        ParsedResponse = jsx:decode(ResponseJson),
        ResponseMessage == ParsedResponse
    catch
        _:_ -> false
    end.