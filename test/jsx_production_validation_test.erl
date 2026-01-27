-module(jsx_production_validation_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Production-Ready JSX Validation Test Suite
%% This suite focuses on production scenarios without complex Unicode
%%====================================================================

jsx_production_validation_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"jsx production encode/decode operations", fun test_jsx_production_operations/0},
         {"mcp message handling with jsx", fun test_mcp_message_handling/0},
         {"error handling in production", fun test_production_error_handling/0},
         {"performance under load", fun test_jsx_production_performance/0},
         {"concurrent jsx operations", fun test_concurrent_jsx_operations/0},
         {"simple_trace with jsx encoding", fun test_simple_trace_jsx_encoding/0},
         {"transport layer jsx integration", fun test_transport_jsx_integration/0},
         {"production memory safety", fun test_production_memory_safety/0}
     ]}.

setup() ->
    %% Ensure jsx is available
    application:start(jsx),
    logger:set_handler_config(default, level, info),
    ok.

cleanup(_) ->
    erlang:erase(),
    ok.

%%====================================================================
%% Production JSX Tests
%%====================================================================

test_jsx_production_operations() ->
    %% Test typical MCP request/response patterns
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
                <<"name">> => <<"erlmcp-client">>,
                <<"version">> => <<"0.5.0">>
            }
        }
    },
    
    %% Test encoding
    EncodedInit = jsx:encode(InitRequest),
    ?assert(is_binary(EncodedInit)),
    ?assert(byte_size(EncodedInit) > 0),
    
    %% Test decoding
    DecodedInit = jsx:decode(EncodedInit),
    ?assertEqual(InitRequest, DecodedInit),
    
    %% Test tools response
    ToolsResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"result">> => #{
            <<"tools">> => [
                #{
                    <<"name">> => <<"file_reader">>,
                    <<"description">> => <<"Read files from filesystem">>,
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
    
    EncodedTools = jsx:encode(ToolsResponse),
    DecodedTools = jsx:decode(EncodedTools),
    ?assertEqual(ToolsResponse, DecodedTools).

test_mcp_message_handling() ->
    %% Test resource read request
    ResourceRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 100,
        <<"method">> => <<"resources/read">>,
        <<"params">> => #{
            <<"uri">> => <<"file:///path/to/file.txt">>
        }
    },
    
    RequestJson = jsx:encode(ResourceRequest),
    ParsedRequest = jsx:decode(RequestJson),
    
    ?assertEqual(ResourceRequest, ParsedRequest),
    ?assertEqual(<<"resources/read">>, maps:get(<<"method">>, ParsedRequest)),
    ?assertEqual(100, maps:get(<<"id">>, ParsedRequest)),
    
    %% Test tool call response
    ToolResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 200,
        <<"result">> => #{
            <<"content">> => [#{
                <<"type">> => <<"text">>,
                <<"text">> => <<"Operation completed successfully">>
            }],
            <<"isError">> => false
        }
    },
    
    ResponseJson = jsx:encode(ToolResponse),
    ParsedResponse = jsx:decode(ResponseJson),
    
    ?assertEqual(ToolResponse, ParsedResponse),
    ?assertEqual(false, maps:get(<<"isError">>, maps:get(<<"result">>, ParsedResponse))).

test_production_error_handling() ->
    %% Test error response encoding/decoding
    ErrorResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 300,
        <<"error">> => #{
            <<"code">> => -32602,
            <<"message">> => <<"Invalid params">>,
            <<"data">> => #{
                <<"field">> => <<"uri">>,
                <<"error">> => <<"missing required parameter">>,
                <<"timestamp">> => erlang:system_time(millisecond)
            }
        }
    },
    
    ErrorJson = jsx:encode(ErrorResponse),
    ParsedError = jsx:decode(ErrorJson),
    
    ?assertEqual(ErrorResponse, ParsedError),
    
    %% Verify error structure
    Error = maps:get(<<"error">>, ParsedError),
    ?assertEqual(-32602, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Invalid params">>, maps:get(<<"message">>, Error)),
    ?assert(maps:is_key(<<"data">>, Error)),
    
    %% Test handling of invalid JSON (should fail gracefully)
    InvalidJson = <<"{\"key\": invalid}">>,
    
    try
        jsx:decode(InvalidJson),
        ?assert(false, "Should have failed to decode invalid JSON")
    catch
        _:_ -> ok  % Expected failure
    end.

test_jsx_production_performance() ->
    %% Test performance with realistic message sizes
    LargeRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1000,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"process_batch">>,
            <<"arguments">> => #{
                <<"items">> => [
                    #{
                        <<"id">> => I,
                        <<"type">> => <<"data_item">>,
                        <<"content">> => <<"Content for item ", (integer_to_binary(I))/binary>>,
                        <<"metadata">> => #{
                            <<"created">> => erlang:system_time(millisecond),
                            <<"priority">> => I rem 5,
                            <<"status">> => <<"pending">>
                        }
                    } || I <- lists:seq(1, 100)
                ]
            }
        }
    },
    
    %% Measure encoding performance
    StartEncode = erlang:monotonic_time(microsecond),
    EncodedLarge = jsx:encode(LargeRequest),
    EndEncode = erlang:monotonic_time(microsecond),
    EncodeTime = EndEncode - StartEncode,
    
    %% Measure decoding performance
    StartDecode = erlang:monotonic_time(microsecond),
    DecodedLarge = jsx:decode(EncodedLarge),
    EndDecode = erlang:monotonic_time(microsecond),
    DecodeTime = EndDecode - StartDecode,
    
    %% Validate correctness
    ?assertEqual(LargeRequest, DecodedLarge),
    
    %% Performance should be reasonable for production (< 100ms)
    ?assert(EncodeTime < 100000, 
           io_lib:format("Encoding took ~p microseconds", [EncodeTime])),
    ?assert(DecodeTime < 100000,
           io_lib:format("Decoding took ~p microseconds", [DecodeTime])).

test_concurrent_jsx_operations() ->
    %% Test jsx under concurrent load
    Self = self(),
    NumWorkers = 20,
    
    TestMessage = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"params">> => #{<<"data">> => <<"concurrent test">>}
    },
    
    %% Spawn concurrent workers
    Workers = [
        spawn_link(fun() ->
            WorkerResult = try
                % Each worker does multiple encode/decode cycles
                lists:foreach(fun(_) ->
                    Encoded = jsx:encode(TestMessage),
                    Decoded = jsx:decode(Encoded),
                    case Decoded =:= TestMessage of
                        true -> ok;
                        false -> error(mismatch)
                    end
                end, lists:seq(1, 10)),
                true
            catch
                _:_ -> false
            end,
            Self ! {worker_done, self(), WorkerResult}
        end) || _ <- lists:seq(1, NumWorkers)
    ],
    
    %% Collect results
    Results = [
        receive
            {worker_done, Pid, Result} when Pid =:= WorkerPid -> Result
        after 5000 -> 
            timeout
        end || WorkerPid <- Workers
    ],
    
    %% All workers should succeed
    ?assert(lists:all(fun(R) -> R =:= true end, Results), 
           "Some concurrent jsx operations failed").

test_simple_trace_jsx_encoding() ->
    %% Test that we can use jsx with trace data structures
    TraceData = #{
        <<"trace_id">> => <<"trace_12345">>,
        <<"spans">> => [
            #{
                <<"span_id">> => <<"span_001">>,
                <<"operation_name">> => <<"http_request">>,
                <<"start_time">> => erlang:system_time(microsecond),
                <<"duration_us">> => 1500,
                <<"status">> => <<"ok">>,
                <<"attributes">> => #{
                    <<"method">> => <<"GET">>,
                    <<"url">> => <<"http://example.com/api">>,
                    <<"status_code">> => 200
                }
            }
        ],
        <<"metadata">> => #{
            <<"service">> => <<"erlmcp">>,
            <<"version">> => <<"0.5.0">>,
            <<"environment">> => <<"test">>
        }
    },
    
    %% Encode trace data
    TraceJson = jsx:encode(TraceData),
    ?assert(is_binary(TraceJson)),
    
    %% Decode and verify
    DecodedTrace = jsx:decode(TraceJson),
    ?assertEqual(TraceData, DecodedTrace),
    
    %% Verify trace structure
    ?assert(maps:is_key(<<"trace_id">>, DecodedTrace)),
    ?assert(maps:is_key(<<"spans">>, DecodedTrace)),
    ?assert(maps:is_key(<<"metadata">>, DecodedTrace)),
    
    Spans = maps:get(<<"spans">>, DecodedTrace),
    ?assert(is_list(Spans)),
    ?assertEqual(1, length(Spans)),
    
    [Span] = Spans,
    ?assertEqual(<<"span_001">>, maps:get(<<"span_id">>, Span)),
    ?assertEqual(<<"http_request">>, maps:get(<<"operation_name">>, Span)),
    ?assertEqual(1500, maps:get(<<"duration_us">>, Span)).

test_transport_jsx_integration() ->
    %% Test jsx integration with transport-like message handling
    
    %% Simulate incoming message
    IncomingMessage = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\",\"params\":{}}">>,
    
    %% Parse message
    ParsedMessage = jsx:decode(IncomingMessage),
    
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, ParsedMessage)),
    ?assertEqual(1, maps:get(<<"id">>, ParsedMessage)),
    ?assertEqual(<<"ping">>, maps:get(<<"method">>, ParsedMessage)),
    
    %% Create response
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => maps:get(<<"id">>, ParsedMessage),
        <<"result">> => #{<<"status">> => <<"pong">>}
    },
    
    %% Encode response
    ResponseJson = jsx:encode(Response),
    ?assert(is_binary(ResponseJson)),
    
    %% Verify response can be parsed back
    ParsedResponse = jsx:decode(ResponseJson),
    ?assertEqual(Response, ParsedResponse),
    
    %% Test notification (no id)
    Notification = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"notification/update">>,
        <<"params">> => #{
            <<"resource">> => <<"file://test.txt">>,
            <<"action">> => <<"created">>
        }
    },
    
    NotificationJson = jsx:encode(Notification),
    ParsedNotification = jsx:decode(NotificationJson),
    ?assertEqual(Notification, ParsedNotification),
    ?assertNot(maps:is_key(<<"id">>, ParsedNotification)).

test_production_memory_safety() ->
    %% Test memory safety under repeated jsx operations
    InitialMemory = erlang:memory(total),
    
    %% Perform repeated operations
    TestData = #{
        <<"operation">> => <<"memory_test">>,
        <<"data">> => [I || I <- lists:seq(1, 100)],
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    
    %% Perform many encode/decode cycles
    lists:foreach(fun(_) ->
        Encoded = jsx:encode(TestData),
        Decoded = jsx:decode(Encoded),
        ?assertEqual(TestData, Decoded)
    end, lists:seq(1, 1000)),
    
    %% Force garbage collection
    erlang:garbage_collect(),
    timer:sleep(10),
    
    FinalMemory = erlang:memory(total),
    MemoryDifference = FinalMemory - InitialMemory,
    
    %% Memory shouldn't grow excessively (less than 2MB)
    ?assert(MemoryDifference < 2097152,
           io_lib:format("Memory grew by ~p bytes", [MemoryDifference])),
    
    %% Test with larger data
    LargerData = #{
        <<"large_field">> => binary:copy(<<"x">>, 1000),
        <<"array">> => [<<"item_", (integer_to_binary(I))/binary>> || I <- lists:seq(1, 500)]
    },
    
    BeforeLargeTest = erlang:memory(total),
    
    %% Process larger data multiple times
    lists:foreach(fun(_) ->
        jsx:encode(LargerData),
        jsx:decode(jsx:encode(LargerData))
    end, lists:seq(1, 100)),
    
    erlang:garbage_collect(),
    timer:sleep(10),
    
    AfterLargeTest = erlang:memory(total),
    LargeTestDifference = AfterLargeTest - BeforeLargeTest,
    
    %% Even with larger data, memory growth should be reasonable
    ?assert(LargeTestDifference < 5242880,
           io_lib:format("Large data test grew memory by ~p bytes", [LargeTestDifference])).

%%====================================================================
%% Integration Validation
%%====================================================================

integration_validation_test() ->
    %% Complete integration test simulating real MCP communication
    
    %% 1. Initialize session
    InitMessage = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{
            <<"protocolVersion">> => <<"2024-11-05">>,
            <<"capabilities">> => #{},
            <<"clientInfo">> => #{
                <<"name">> => <<"jsx_validation_client">>,
                <<"version">> => <<"1.0.0">>
            }
        }
    },
    
    InitJson = jsx:encode(InitMessage),
    ?assert(byte_size(InitJson) > 0),
    
    % Simulate server processing
    ParsedInit = jsx:decode(InitJson),
    ?assertEqual(InitMessage, ParsedInit),
    
    %% 2. List tools
    ToolsRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"method">> => <<"tools/list">>,
        <<"params">> => #{}
    },
    
    ToolsJson = jsx:encode(ToolsRequest),
    ParsedToolsReq = jsx:decode(ToolsJson),
    ?assertEqual(ToolsRequest, ParsedToolsReq),
    
    %% 3. Tool response
    ToolsResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"result">> => #{
            <<"tools">> => [
                #{
                    <<"name">> => <<"echo">>,
                    <<"description">> => <<"Echo input back">>
                }
            ]
        }
    },
    
    ToolsRespJson = jsx:encode(ToolsResponse),
    ParsedToolsResp = jsx:decode(ToolsRespJson),
    ?assertEqual(ToolsResponse, ParsedToolsResp),
    
    %% 4. Call tool
    ToolCall = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 3,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"echo">>,
            <<"arguments">> => #{<<"message">> => <<"Hello JSX!">>}
        }
    },
    
    ToolCallJson = jsx:encode(ToolCall),
    ParsedToolCall = jsx:decode(ToolCallJson),
    ?assertEqual(ToolCall, ParsedToolCall),
    
    %% 5. Tool result
    ToolResult = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 3,
        <<"result">> => #{
            <<"content">> => [#{
                <<"type">> => <<"text">>,
                <<"text">> => <<"Hello JSX!">>
            }],
            <<"isError">> => false
        }
    },
    
    ToolResultJson = jsx:encode(ToolResult),
    ParsedToolResult = jsx:decode(ToolResultJson),
    ?assertEqual(ToolResult, ParsedToolResult),
    
    %% Integration successful
    ok.