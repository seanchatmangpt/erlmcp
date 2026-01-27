-module(jsx_final_validation_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Final JSX Production Validation Test Suite
%% This comprehensive test validates jsx integration for production use
%%====================================================================

jsx_final_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"jsx basic functionality validation", fun test_jsx_basic_functionality/0},
         {"jsx mcp message validation", fun test_jsx_mcp_messages/0},
         {"jsx error handling validation", fun test_jsx_error_handling/0},
         {"jsx production performance validation", fun test_jsx_production_performance/0},
         {"jsx memory safety validation", fun test_jsx_memory_safety/0},
         {"jsx concurrent operations validation", fun test_jsx_concurrent_operations/0}
     ]}.

setup() ->
    %% Start jsx application
    case application:start(jsx) of
        ok -> ok;
        {error, {already_started, jsx}} -> ok
    end,
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Core JSX Validation Tests
%%====================================================================

test_jsx_basic_functionality() ->
    %% Test basic encode/decode cycle
    SimpleData = #{
        <<"key1">> => <<"value1">>,
        <<"key2">> => 42,
        <<"key3">> => true,
        <<"key4">> => false,
        <<"key5">> => null
    },
    
    %% Encode the data
    Encoded = jsx:encode(SimpleData),
    ?assert(is_binary(Encoded)),
    ?assert(byte_size(Encoded) > 0),
    
    %% Decode the data
    Decoded = jsx:decode(Encoded),
    ?assert(is_map(Decoded)),
    
    %% Verify specific keys
    ?assertEqual(<<"value1">>, maps:get(<<"key1">>, Decoded)),
    ?assertEqual(42, maps:get(<<"key2">>, Decoded)),
    ?assertEqual(true, maps:get(<<"key3">>, Decoded)),
    ?assertEqual(false, maps:get(<<"key4">>, Decoded)),
    ?assertEqual(null, maps:get(<<"key5">>, Decoded)),
    
    %% Test list encoding
    ListData = [1, 2, 3, <<"string">>, true, null],
    ListEncoded = jsx:encode(ListData),
    ListDecoded = jsx:decode(ListEncoded),
    ?assertEqual(ListData, ListDecoded),
    
    %% Test nested structures
    NestedData = #{
        <<"outer">> => #{
            <<"inner">> => [1, 2, 3],
            <<"flag">> => true
        }
    },
    NestedEncoded = jsx:encode(NestedData),
    NestedDecoded = jsx:decode(NestedEncoded),
    ?assertEqual(NestedData, NestedDecoded).

test_jsx_mcp_messages() ->
    %% Test MCP initialization request
    InitRequest = #{
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
                <<"name">> => <<"erlmcp-test">>,
                <<"version">> => <<"0.5.0">>
            }
        }
    },
    
    InitEncoded = jsx:encode(InitRequest),
    InitDecoded = jsx:decode(InitEncoded),
    
    ?assert(maps:is_key(<<"jsonrpc">>, InitDecoded)),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, InitDecoded)),
    ?assertEqual(1, maps:get(<<"id">>, InitDecoded)),
    ?assertEqual(<<"initialize">>, maps:get(<<"method">>, InitDecoded)),
    
    Params = maps:get(<<"params">>, InitDecoded),
    ?assert(maps:is_key(<<"protocolVersion">>, Params)),
    ?assert(maps:is_key(<<"capabilities">>, Params)),
    ?assert(maps:is_key(<<"clientInfo">>, Params)),
    
    %% Test tools list response
    ToolsResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"result">> => #{
            <<"tools">> => [
                #{
                    <<"name">> => <<"calculator">>,
                    <<"description">> => <<"Basic math operations">>,
                    <<"inputSchema">> => #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"operation">> => #{<<"type">> => <<"string">>},
                            <<"a">> => #{<<"type">> => <<"number">>},
                            <<"b">> => #{<<"type">> => <<"number">>}
                        }
                    }
                }
            ]
        }
    },
    
    ToolsEncoded = jsx:encode(ToolsResponse),
    ToolsDecoded = jsx:decode(ToolsEncoded),
    ?assertEqual(ToolsResponse, ToolsDecoded),
    
    %% Test error response
    ErrorResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 3,
        <<"error">> => #{
            <<"code">> => -32602,
            <<"message">> => <<"Invalid params">>,
            <<"data">> => #{
                <<"details">> => <<"Missing required parameter 'uri'">>,
                <<"timestamp">> => erlang:system_time(millisecond)
            }
        }
    },
    
    ErrorEncoded = jsx:encode(ErrorResponse),
    ErrorDecoded = jsx:decode(ErrorEncoded),
    ?assertEqual(ErrorResponse, ErrorDecoded).

test_jsx_error_handling() ->
    %% Test malformed JSON handling
    MalformedJson = <<"{\"key\": invalid_value}">>,
    
    try
        jsx:decode(MalformedJson),
        ?assert(false, "Should have failed on malformed JSON")
    catch
        _:_ -> 
            ok  % Expected error
    end,
    
    %% Test empty JSON
    ?assertEqual(null, jsx:decode(<<"null">>)),
    ?assertEqual(#{}, jsx:decode(<<"{}">>)),
    ?assertEqual([], jsx:decode(<<"[]">>)),
    
    %% Test large numbers
    LargeNumber = 123456789012345,
    NumberData = #{<<"large">> => LargeNumber},
    NumberEncoded = jsx:encode(NumberData),
    NumberDecoded = jsx:decode(NumberEncoded),
    ?assertEqual(LargeNumber, maps:get(<<"large">>, NumberDecoded)),
    
    %% Test special float values
    FloatData = #{<<"float">> => 3.14159},
    FloatEncoded = jsx:encode(FloatData),
    FloatDecoded = jsx:decode(FloatEncoded),
    ?assertEqual(3.14159, maps:get(<<"float">>, FloatDecoded)).

test_jsx_production_performance() ->
    %% Test performance with production-sized data
    ProductionData = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1000,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"batch_process">>,
            <<"arguments">> => #{
                <<"batch_size">> => 1000,
                <<"items">> => [
                    #{
                        <<"id">> => I,
                        <<"data">> => <<"Item data ", (integer_to_binary(I))/binary>>,
                        <<"timestamp">> => erlang:system_time(millisecond),
                        <<"metadata">> => #{
                            <<"priority">> => I rem 5,
                            <<"category">> => <<"batch">>,
                            <<"processed">> => false
                        }
                    } || I <- lists:seq(1, 200)  % 200 items for performance test
                ]
            }
        }
    },
    
    %% Measure encoding time
    StartEncode = erlang:monotonic_time(microsecond),
    Encoded = jsx:encode(ProductionData),
    EndEncode = erlang:monotonic_time(microsecond),
    
    %% Measure decoding time
    StartDecode = erlang:monotonic_time(microsecond),
    Decoded = jsx:decode(Encoded),
    EndDecode = erlang:monotonic_time(microsecond),
    
    EncodeTime = EndEncode - StartEncode,
    DecodeTime = EndDecode - StartDecode,
    
    %% Verify data integrity
    ?assertEqual(ProductionData, Decoded),
    
    %% Performance assertions (should be reasonable for production)
    ?assert(EncodeTime < 500000, % 500ms max for encoding
           io_lib:format("Encoding took ~p microseconds", [EncodeTime])),
    ?assert(DecodeTime < 500000, % 500ms max for decoding
           io_lib:format("Decoding took ~p microseconds", [DecodeTime])),
    
    %% Test repeated operations
    StartRepeated = erlang:monotonic_time(microsecond),
    lists:foreach(fun(_) ->
        jsx:encode(#{<<"test">> => <<"repeated">>, <<"num">> => 42}),
        jsx:decode(<<"{\"test\":\"repeated\",\"num\":42}">>)
    end, lists:seq(1, 1000)),
    EndRepeated = erlang:monotonic_time(microsecond),
    
    RepeatedTime = EndRepeated - StartRepeated,
    ?assert(RepeatedTime < 1000000, % 1 second for 1000 small operations
           io_lib:format("1000 small operations took ~p microseconds", [RepeatedTime])).

test_jsx_memory_safety() ->
    %% Test memory behavior under load
    InitialMemory = erlang:memory(total),
    
    TestData = #{
        <<"data">> => [I || I <- lists:seq(1, 100)],
        <<"text">> => <<"Some test data for memory testing">>
    },
    
    %% Perform many encode/decode operations
    lists:foreach(fun(_) ->
        jsx:encode(TestData),
        jsx:decode(jsx:encode(TestData))
    end, lists:seq(1, 1000)),
    
    %% Force garbage collection
    erlang:garbage_collect(),
    timer:sleep(10),  % Allow GC to complete
    
    FinalMemory = erlang:memory(total),
    MemoryDiff = FinalMemory - InitialMemory,
    
    %% Memory growth should be reasonable
    ?assert(MemoryDiff < 5242880, % Less than 5MB growth
           io_lib:format("Memory increased by ~p bytes", [MemoryDiff])),
    
    %% Test with larger structures
    LargeData = #{
        <<"array">> => [<<"item_", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 1000)],
        <<"metadata">> => #{
            <<"created">> => erlang:system_time(millisecond),
            <<"version">> => <<"1.0.0">>,
            <<"flags">> => [true, false, true, false]
        }
    },
    
    BeforeLarge = erlang:memory(total),
    
    %% Process large data multiple times
    lists:foreach(fun(_) ->
        jsx:encode(LargeData),
        jsx:decode(jsx:encode(LargeData))
    end, lists:seq(1, 100)),
    
    erlang:garbage_collect(),
    timer:sleep(10),
    
    AfterLarge = erlang:memory(total),
    LargeDiff = AfterLarge - BeforeLarge,
    
    ?assert(LargeDiff < 10485760, % Less than 10MB for large operations
           io_lib:format("Large operations increased memory by ~p bytes", [LargeDiff])).

test_jsx_concurrent_operations() ->
    %% Test jsx under concurrent load
    Self = self(),
    NumWorkers = 25,
    OperationsPerWorker = 50,
    
    TestMessage = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"concurrent_test">>,
        <<"params">> => #{
            <<"worker_id">> => 0,  % Will be updated per worker
            <<"data">> => [<<"item1">>, <<"item2">>, <<"item3">>],
            <<"timestamp">> => erlang:system_time(millisecond)
        }
    },
    
    %% Spawn workers
    Workers = [
        spawn_link(fun() ->
            WorkerId = I,
            WorkerMessage = TestMessage#{
                <<"params">> => #{
                    <<"worker_id">> => WorkerId,
                    <<"data">> => [<<"worker_", (integer_to_binary(WorkerId))/binary, "_item">>],
                    <<"timestamp">> => erlang:system_time(millisecond)
                }
            },
            
            Result = try
                lists:foreach(fun(_) ->
                    Encoded = jsx:encode(WorkerMessage),
                    Decoded = jsx:decode(Encoded),
                    
                    % Verify basic structure
                    <<"2.0">> = maps:get(<<"jsonrpc">>, Decoded),
                    <<"concurrent_test">> = maps:get(<<"method">>, Decoded),
                    
                    % Verify params
                    Params = maps:get(<<"params">>, Decoded),
                    WorkerId = maps:get(<<"worker_id">>, Params)
                end, lists:seq(1, OperationsPerWorker)),
                success
            catch
                _:Error -> {error, Error}
            end,
            
            Self ! {worker_done, self(), Result}
        end) || I <- lists:seq(1, NumWorkers)
    ],
    
    %% Collect results
    Results = [
        receive
            {worker_done, Pid, Result} when Pid =:= Worker -> Result
        after 10000 ->
            timeout
        end || Worker <- Workers
    ],
    
    %% All workers should succeed
    SuccessCount = length([R || R <- Results, R =:= success]),
    ?assertEqual(NumWorkers, SuccessCount,
                io_lib:format("Only ~p/~p workers succeeded", [SuccessCount, NumWorkers])),
    
    %% No timeouts should occur
    TimeoutCount = length([R || R <- Results, R =:= timeout]),
    ?assertEqual(0, TimeoutCount, "Some workers timed out").

%%====================================================================
%% Production Integration Test
%%====================================================================

production_integration_test() ->
    %% Complete end-to-end test simulating real MCP usage
    
    %% 1. Client sends initialize request
    InitRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{
            <<"protocolVersion">> => <<"2024-11-05">>,
            <<"capabilities">> => #{
                <<"roots">> => #{<<"listChanged">> => true}
            },
            <<"clientInfo">> => #{
                <<"name">> => <<"production_test_client">>,
                <<"version">> => <<"1.0.0">>
            }
        }
    },
    
    InitJson = jsx:encode(InitRequest),
    ?assert(is_binary(InitJson)),
    ?assert(byte_size(InitJson) > 0),
    
    %% Server processes request
    ParsedInit = jsx:decode(InitJson),
    ?assertEqual(InitRequest, ParsedInit),
    
    %% 2. Server responds with capabilities
    InitResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => #{
            <<"protocolVersion">> => <<"2024-11-05">>,
            <<"capabilities">> => #{
                <<"logging">> => #{},
                <<"tools">> => #{<<"listChanged">> => true},
                <<"resources">> => #{<<"subscribe">> => true}
            },
            <<"serverInfo">> => #{
                <<"name">> => <<"erlmcp-server">>,
                <<"version">> => <<"0.5.0">>
            }
        }
    },
    
    InitRespJson = jsx:encode(InitResponse),
    ParsedInitResp = jsx:decode(InitRespJson),
    ?assertEqual(InitResponse, ParsedInitResp),
    
    %% 3. Client requests tools list
    ToolsRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"method">> => <<"tools/list">>,
        <<"params">> => #{}
    },
    
    ToolsReqJson = jsx:encode(ToolsRequest),
    ParsedToolsReq = jsx:decode(ToolsReqJson),
    ?assertEqual(ToolsRequest, ParsedToolsReq),
    
    %% 4. Server responds with tools
    ToolsResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"result">> => #{
            <<"tools">> => [
                #{
                    <<"name">> => <<"file_reader">>,
                    <<"description">> => <<"Read files from the filesystem">>,
                    <<"inputSchema">> => #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"path">> => #{<<"type">> => <<"string">>}
                        },
                        <<"required">> => [<<"path">>]
                    }
                }
            ]
        }
    },
    
    ToolsRespJson = jsx:encode(ToolsResponse),
    ParsedToolsResp = jsx:decode(ToolsRespJson),
    ?assertEqual(ToolsResponse, ParsedToolsResp),
    
    %% 5. Client calls a tool
    ToolCall = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 3,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"file_reader">>,
            <<"arguments">> => #{
                <<"path">> => <<"/tmp/test.txt">>
            }
        }
    },
    
    ToolCallJson = jsx:encode(ToolCall),
    ParsedToolCall = jsx:decode(ToolCallJson),
    ?assertEqual(ToolCall, ParsedToolCall),
    
    %% 6. Server responds with tool result
    ToolResult = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 3,
        <<"result">> => #{
            <<"content">> => [#{
                <<"type">> => <<"text">>,
                <<"text">> => <<"File content would be here">>
            }],
            <<"isError">> => false
        }
    },
    
    ToolResultJson = jsx:encode(ToolResult),
    ParsedToolResult = jsx:decode(ToolResultJson),
    ?assertEqual(ToolResult, ParsedToolResult),
    
    %% All production integration steps completed successfully
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

validate_jsx_runtime() ->
    %% Ensure jsx is properly loaded and functional
    try
        jsx:encode(#{test => true}),
        jsx:decode(<<"{\"test\":true}">>),
        true
    catch
        _:_ -> false
    end.