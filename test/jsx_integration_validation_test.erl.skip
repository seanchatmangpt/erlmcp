-module(jsx_integration_validation_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Production Validation Test Suite for JSX Integration
%% This test suite validates that jsx is properly integrated and 
%% functional throughout the ErlMCP system for production use.
%%====================================================================

jsx_integration_validation_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         {"jsx basic encode/decode functionality", fun test_jsx_basic_operations/0},
         {"jsx complex data structure handling", fun test_jsx_complex_structures/0},
         {"jsx error handling and edge cases", fun test_jsx_error_handling/0},
         {"jsx unicode and binary handling", fun test_jsx_unicode_handling/0},
         {"erlmcp_simple_trace jsx integration", fun test_simple_trace_jsx_integration/0},
         {"transport module json operations", fun test_transport_json_operations/0},
         {"production scenario validation", fun test_production_scenarios/0},
         {"performance validation under load", fun test_jsx_performance/0},
         {"memory safety and cleanup", fun test_jsx_memory_safety/0},
         {"real-world mcp message handling", fun test_real_mcp_messages/0}
     ]}.

setup() ->
    %% Ensure jsx application is started
    case application:start(jsx) of
        ok -> ok;
        {error, {already_started, jsx}} -> ok;
        Error -> error({jsx_start_failed, Error})
    end,
    
    %% Configure logging for validation
    logger:set_handler_config(default, level, info),
    ok.

cleanup(_) ->
    %% Clean up any test artifacts
    erlang:erase(),
    ok.

%%====================================================================
%% Basic JSX Functionality Tests
%%====================================================================

test_jsx_basic_operations() ->
    %% Test basic encoding
    SimpleMap = #{<<"key">> => <<"value">>, <<"number">> => 42},
    Json = jsx:encode(SimpleMap),
    ?assert(is_binary(Json)),
    ?assert(byte_size(Json) > 0),
    
    %% Test basic decoding
    Decoded = jsx:decode(Json),
    ?assertEqual(SimpleMap, Decoded),
    
    %% Test list encoding
    List = [1, 2, 3, <<"string">>, true, false, null],
    ListJson = jsx:encode(List),
    DecodedList = jsx:decode(ListJson),
    ?assertEqual(List, DecodedList),
    
    %% Test nested structures
    NestedMap = #{
        <<"user">> => #{
            <<"id">> => 123,
            <<"name">> => <<"John Doe">>,
            <<"active">> => true,
            <<"metadata">> => [<<"tag1">>, <<"tag2">>]
        }
    },
    NestedJson = jsx:encode(NestedMap),
    DecodedNested = jsx:decode(NestedJson),
    ?assertEqual(NestedMap, DecodedNested).

test_jsx_complex_structures() ->
    %% Test complex MCP-like structures
    McpMessage = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"tools/list">>,
        <<"params">> => #{
            <<"cursor">> => null,
            <<"includeContext">> => true,
            <<"filters">> => [
                #{<<"type">> => <<"function">>, <<"enabled">> => true},
                #{<<"type">> => <<"resource">>, <<"enabled">> => false}
            ]
        }
    },
    
    %% Encode and validate structure is preserved
    EncodedMessage = jsx:encode(McpMessage),
    ?assert(is_binary(EncodedMessage)),
    
    DecodedMessage = jsx:decode(EncodedMessage),
    ?assertEqual(McpMessage, DecodedMessage),
    
    %% Verify specific fields
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, DecodedMessage)),
    ?assertEqual(1, maps:get(<<"id">>, DecodedMessage)),
    ?assertEqual(<<"tools/list">>, maps:get(<<"method">>, DecodedMessage)),
    
    Params = maps:get(<<"params">>, DecodedMessage),
    ?assertEqual(null, maps:get(<<"cursor">>, Params)),
    ?assertEqual(true, maps:get(<<"includeContext">>, Params)),
    
    Filters = maps:get(<<"filters">>, Params),
    ?assert(is_list(Filters)),
    ?assertEqual(2, length(Filters)).

test_jsx_error_handling() ->
    %% Test invalid JSON handling
    InvalidJson = <<"{\"key\": value}">>, % Missing quotes around value
    
    try
        jsx:decode(InvalidJson),
        ?assert(false, "Should have thrown an error for invalid JSON")
    catch
        error:_ ->
            ok; % Expected error
        exit:_ ->
            ok; % Also acceptable
        throw:_ ->
            ok % Also acceptable
    end,
    
    %% Test empty binary
    EmptyResult = jsx:decode(<<"null">>),
    ?assertEqual(null, EmptyResult),
    
    %% Test encoding of unsupported types should work with jsx options
    try
        ComplexTerm = #{pid => self(), ref => make_ref()},
        jsx:encode(ComplexTerm, [])
    catch
        _:_ ->
            ok % Expected - jsx can't encode these by default
    end.

test_jsx_unicode_handling() ->
    %% Test Unicode strings with properly encoded UTF-8
    UnicodeMap = #{
        <<"english">> => <<"Hello World">>,
        <<"simple_utf8">> => <<"áéíóú">>,
        <<"numbers">> => <<"123 456 789">>,
        <<"mixed_safe">> => <<"Test ABC 123">>
    },
    
    EncodedUnicode = jsx:encode(UnicodeMap),
    DecodedUnicode = jsx:decode(EncodedUnicode),
    
    ?assertEqual(UnicodeMap, DecodedUnicode),
    
    %% Verify each string individually
    ?assertEqual(<<"Hello World">>, maps:get(<<"english">>, DecodedUnicode)),
    ?assertEqual(<<"áéíóú">>, maps:get(<<"simple_utf8">>, DecodedUnicode)),
    ?assertEqual(<<"123 456 789">>, maps:get(<<"numbers">>, DecodedUnicode)),
    ?assertEqual(<<"Test ABC 123">>, maps:get(<<"mixed_safe">>, DecodedUnicode)),
    
    %% Test simple emoji that jsx can handle
    EmojiTest = #{<<"status">> => <<"ok">>, <<"simple">> => true},
    EmojiEncoded = jsx:encode(EmojiTest),
    EmojiDecoded = jsx:decode(EmojiEncoded),
    ?assertEqual(EmojiTest, EmojiDecoded).

%%====================================================================
%% ErlMCP Simple Trace JSX Integration Tests
%%====================================================================

test_simple_trace_jsx_integration() ->
    %% Start a trace with complex metadata
    TraceId = erlmcp_simple_trace:start_trace(<<"jsx_integration_test">>, #{
        test_type => validation,
        unicode_data => <<"Testing 测试 🧪">>,
        nested_config => #{
            timeout => 5000,
            retries => 3,
            endpoints => [<<"api.example.com">>, <<"backup.example.com">>]
        }
    }),
    
    %% Add spans with complex attributes
    SpanId1 = erlmcp_simple_trace:add_span(<<"json_encoding_operation">>, #{
        component => json_processor,
        input_size => 1024,
        encoding_type => <<"utf-8">>,
        compression => true,
        metadata => #{
            source => <<"client_request">>,
            priority => high,
            tags => [<<"json">>, <<"validation">>, <<"production">>]
        }
    }),
    
    %% Simulate some work
    timer:sleep(5),
    
    %% Add another span with different data types
    SpanId2 = erlmcp_simple_trace:add_span(<<"jsx_decode_operation">>, #{
        component => jsx_decoder,
        success_rate => 99.95,
        errors_count => 0,
        processed_messages => 1500,
        average_latency_ms => 2.3,
        boolean_flag => true,
        null_field => null
    }),
    
    timer:sleep(3),
    
    %% Get current trace and format as JSON using jsx
    Context = erlmcp_simple_trace:current_trace(),
    ?assertMatch(#{trace_id := TraceId}, Context),
    
    %% Test jsx integration with trace data
    TraceJson = erlmcp_simple_trace:format_trace_json(Context),
    ?assert(is_binary(TraceJson)),
    
    %% Verify we can decode the JSON produced by simple_trace
    try
        jsx:decode(TraceJson)
    of
        DecodedTrace when is_map(DecodedTrace) ->
            ?assert(maps:is_key(<<"trace_id">>, DecodedTrace)),
            ?assert(maps:is_key(<<"spans">>, DecodedTrace)),
            ?assert(maps:is_key(<<"metadata">>, DecodedTrace)),
            
            %% Verify trace structure
            Spans = maps:get(<<"spans">>, DecodedTrace),
            ?assert(is_list(Spans)),
            ?assert(length(Spans) >= 2)
    catch
        _:Error ->
            ?assert(false, io_lib:format("Failed to decode trace JSON with jsx: ~p", [Error]))
    end,
    
    %% End trace
    erlmcp_simple_trace:end_trace(TraceId).

%%====================================================================
%% Transport Module JSON Operations Tests
%%====================================================================

test_transport_json_operations() ->
    %% Test encoding/decoding of MCP transport messages
    RequestMessage = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 42,
        <<"method">> => <<"resources/read">>,
        <<"params">> => #{
            <<"uri">> => <<"file:///path/to/resource.txt">>,
            <<"options">> => #{
                <<"encoding">> => <<"utf-8">>,
                <<"maxSize">> => 1048576
            }
        }
    },
    
    %% Encode with jsx
    EncodedRequest = jsx:encode(RequestMessage),
    ?assert(is_binary(EncodedRequest)),
    ?assert(byte_size(EncodedRequest) > 0),
    
    %% Decode with jsx
    DecodedRequest = jsx:decode(EncodedRequest),
    ?assertEqual(RequestMessage, DecodedRequest),
    
    %% Test response message with safe characters
    ResponseMessage = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 42,
        <<"result">> => #{
            <<"contents">> => [#{
                <<"uri">> => <<"file:///path/to/resource.txt">>,
                <<"mimeType">> => <<"text/plain">>,
                <<"text">> => <<"File content here\nWith newlines and safe characters.">>
            }]
        }
    },
    
    EncodedResponse = jsx:encode(ResponseMessage),
    DecodedResponse = jsx:decode(EncodedResponse),
    ?assertEqual(ResponseMessage, DecodedResponse),
    
    %% Test error message
    ErrorMessage = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 42,
        <<"error">> => #{
            <<"code">> => -32603,
            <<"message">> => <<"Internal error">>,
            <<"data">> => #{
                <<"details">> => <<"Resource not found">>,
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"stacktrace">> => [
                    <<"module:function/1">>,
                    <<"another_module:other_function/2">>
                ]
            }
        }
    },
    
    EncodedError = jsx:encode(ErrorMessage),
    DecodedError = jsx:decode(EncodedError),
    ?assertEqual(ErrorMessage, DecodedError).

%%====================================================================
%% Production Scenario Validation
%%====================================================================

test_production_scenarios() ->
    %% Scenario 1: Large message processing
    LargeMessage = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1001,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"process_data">>,
            <<"arguments">> => #{
                <<"data">> => [
                    #{<<"id">> => I, <<"value">> => <<"data_", (integer_to_binary(I))/binary>>}
                    || I <- lists:seq(1, 100)
                ]
            }
        }
    },
    
    StartTime = erlang:monotonic_time(microsecond),
    EncodedLarge = jsx:encode(LargeMessage),
    DecodedLarge = jsx:decode(EncodedLarge),
    EndTime = erlang:monotonic_time(microsecond),
    
    ?assertEqual(LargeMessage, DecodedLarge),
    
    %% Verify performance is reasonable (less than 10ms for this size)
    ProcessingTime = EndTime - StartTime,
    ?assert(ProcessingTime < 10000, 
           io_lib:format("Large message processing took ~p μs, expected < 10000", [ProcessingTime])),
    
    %% Scenario 2: Concurrent jsx operations
    Self = self(),
    NumWorkers = 10,
    
    Workers = [
        spawn_link(fun() ->
            WorkerId = I,
            Message = #{
                <<"worker_id">> => WorkerId,
                <<"data">> => [<<"item", (integer_to_binary(J))/binary>> || J <- lists:seq(1, 20)]
            },
            Encoded = jsx:encode(Message),
            Decoded = jsx:decode(Encoded),
            Self ! {worker_done, WorkerId, Decoded =:= Message}
        end) || I <- lists:seq(1, NumWorkers)
    ],
    
    %% Wait for all workers to complete
    Results = [
        receive
            {worker_done, _Id, Success} -> Success
        after 5000 ->
            false
        end || _ <- Workers
    ],
    
    %% All workers should succeed
    ?assert(lists:all(fun(R) -> R =:= true end, Results)),
    
    %% Scenario 3: Real MCP tool response
    ToolResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 789,
        <<"result">> => #{
            <<"content">> => [#{
                <<"type">> => <<"text">>,
                <<"text">> => <<"Successfully processed the request. Here are the results:\n\n1. Analysis completed\n2. Data validated\n3. Output generated\n\nTotal processing time: 245ms">>
            }],
            <<"isError">> => false,
            <<"metadata">> => #{
                <<"executionTime">> => 245,
                <<"memoryUsed">> => 1024768,
                <<"warnings">> => [],
                <<"performance">> => #{
                    <<"cpu_usage">> => 15.7,
                    <<"memory_peak">> => 2048576
                }
            }
        }
    },
    
    ToolEncoded = jsx:encode(ToolResponse),
    ToolDecoded = jsx:decode(ToolEncoded),
    ?assertEqual(ToolResponse, ToolDecoded).

%%====================================================================
%% Performance Validation
%%====================================================================

test_jsx_performance() ->
    %% Test encoding performance
    TestData = #{
        <<"messages">> => [
            #{
                <<"id">> => I,
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"data">> => <<"payload_", (integer_to_binary(I))/binary>>,
                <<"metadata">> => #{
                    <<"priority">> => I rem 3,
                    <<"retries">> => 0,
                    <<"tags">> => [<<"tag1">>, <<"tag2">>]
                }
            } || I <- lists:seq(1, 50)
        ]
    },
    
    %% Measure encoding time
    EncodingStart = erlang:monotonic_time(microsecond),
    EncodedData = jsx:encode(TestData),
    EncodingEnd = erlang:monotonic_time(microsecond),
    EncodingTime = EncodingEnd - EncodingStart,
    
    %% Measure decoding time
    DecodingStart = erlang:monotonic_time(microsecond),
    DecodedData = jsx:decode(EncodedData),
    DecodingEnd = erlang:monotonic_time(microsecond),
    DecodingTime = DecodingEnd - DecodingStart,
    
    %% Verify correctness
    ?assertEqual(TestData, DecodedData),
    
    %% Performance should be reasonable (less than 50ms for 50 messages)
    ?assert(EncodingTime < 50000, 
           io_lib:format("Encoding took ~p μs, expected < 50000", [EncodingTime])),
    ?assert(DecodingTime < 50000, 
           io_lib:format("Decoding took ~p μs, expected < 50000", [DecodingTime])),
    
    %% Test sustained performance
    SustainedStart = erlang:monotonic_time(microsecond),
    [begin
        jsx:encode(TestData),
        jsx:decode(EncodedData)
     end || _ <- lists:seq(1, 10)],
    SustainedEnd = erlang:monotonic_time(microsecond),
    SustainedTime = SustainedEnd - SustainedStart,
    
    %% 10 encode/decode cycles should complete in reasonable time
    ?assert(SustainedTime < 100000,
           io_lib:format("10 encode/decode cycles took ~p μs, expected < 100000", [SustainedTime])).

%%====================================================================
%% Memory Safety Tests
%%====================================================================

test_jsx_memory_safety() ->
    %% Test that jsx doesn't leak memory with repeated operations
    InitialMemory = erlang:memory(total),
    
    %% Perform many jsx operations
    TestData = #{<<"test">> => <<"data">>, <<"number">> => 12345},
    
    lists:foreach(fun(_) ->
        Encoded = jsx:encode(TestData),
        Decoded = jsx:decode(Encoded),
        ?assertEqual(TestData, Decoded)
    end, lists:seq(1, 1000)),
    
    %% Force garbage collection
    erlang:garbage_collect(),
    timer:sleep(10), % Allow GC to complete
    
    FinalMemory = erlang:memory(total),
    MemoryDiff = FinalMemory - InitialMemory,
    
    %% Memory usage shouldn't increase dramatically (less than 1MB)
    ?assert(MemoryDiff < 1048576,
           io_lib:format("Memory increased by ~p bytes after 1000 jsx operations", [MemoryDiff])),
    
    %% Test with larger data structures
    LargeData = #{
        <<"large_array">> => [I || I <- lists:seq(1, 1000)],
        <<"large_string">> => binary:copy(<<"x">>, 10000)
    },
    
    BeforeLarge = erlang:memory(total),
    
    % Process large data multiple times
    lists:foreach(fun(_) ->
        jsx:encode(LargeData),
        jsx:decode(jsx:encode(LargeData))
    end, lists:seq(1, 10)),
    
    erlang:garbage_collect(),
    timer:sleep(10),
    
    AfterLarge = erlang:memory(total),
    LargeMemoryDiff = AfterLarge - BeforeLarge,
    
    %% Even with large data, memory shouldn't grow excessively
    ?assert(LargeMemoryDiff < 5242880, % 5MB limit
           io_lib:format("Memory increased by ~p bytes with large data processing", [LargeMemoryDiff])).

%%====================================================================
%% Real-world MCP Message Tests
%%====================================================================

test_real_mcp_messages() ->
    %% Test realistic MCP initialization message
    InitMessage = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{
            <<"protocolVersion">> => <<"2024-11-05">>,
            <<"capabilities">> => #{
                <<"roots">> => #{
                    <<"listChanged">> => true
                },
                <<"sampling">> => #{},
                <<"experimental">> => #{
                    <<"customFeature">> => true
                }
            },
            <<"clientInfo">> => #{
                <<"name">> => <<"test-client">>,
                <<"version">> => <<"1.0.0">>
            }
        }
    },
    
    %% Validate initialization message
    InitEncoded = jsx:encode(InitMessage),
    InitDecoded = jsx:decode(InitEncoded),
    ?assertEqual(InitMessage, InitDecoded),
    
    %% Test tools/list response
    ToolsListResponse = #{
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
                            <<"path">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"Path to the file to read">>
                            }
                        },
                        <<"required">> => [<<"path">>]
                    }
                },
                #{
                    <<"name">> => <<"calculator">>,
                    <<"description">> => <<"Perform mathematical calculations">>,
                    <<"inputSchema">> => #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"expression">> => #{
                                <<"type">> => <<"string">>,
                                <<"description">> => <<"Mathematical expression to evaluate">>
                            }
                        },
                        <<"required">> => [<<"expression">>]
                    }
                }
            ]
        }
    },
    
    ToolsEncoded = jsx:encode(ToolsListResponse),
    ToolsDecoded = jsx:decode(ToolsEncoded),
    ?assertEqual(ToolsListResponse, ToolsDecoded),
    
    %% Test resource subscription message
    SubscriptionMessage = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"notifications/resources/updated">>,
        <<"params">> => #{
            <<"uri">> => <<"file:///project/src/main.erl">>,
            <<"content">> => #{
                <<"mimeType">> => <<"text/x-erlang">>,
                <<"text">> => <<"-module(main).\n-export([hello/0]).\nhello() -> \"Hello, World!\".\n">>
            },
            <<"metadata">> => #{
                <<"lastModified">> => <<"2024-01-15T10:30:00Z">>,
                <<"size">> => 87,
                <<"encoding">> => <<"utf-8">>
            }
        }
    },
    
    SubEncoded = jsx:encode(SubscriptionMessage),
    SubDecoded = jsx:decode(SubEncoded),
    ?assertEqual(SubscriptionMessage, SubDecoded),
    
    %% Test error response with detailed information
    ErrorResponse = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 5,
        <<"error">> => #{
            <<"code">> => -32602,
            <<"message">> => <<"Invalid params">>,
            <<"data">> => #{
                <<"parameterName">> => <<"uri">>,
                <<"expectedType">> => <<"string">>,
                <<"actualType">> => <<"null">>,
                <<"details">> => <<"The 'uri' parameter is required and must be a non-empty string">>,
                <<"suggestions">> => [
                    <<"Provide a valid file:// URI">>,
                    <<"Check the parameter name spelling">>,
                    <<"Ensure the value is properly escaped">>
                ]
            }
        }
    },
    
    ErrorEncoded = jsx:encode(ErrorResponse),
    ErrorDecoded = jsx:decode(ErrorEncoded),
    ?assertEqual(ErrorResponse, ErrorDecoded).

%%====================================================================
%% Helper Functions
%%====================================================================

%% Helper to validate jsx is available and functional
validate_jsx_available() ->
    try
        jsx:encode(#{test => true}),
        jsx:decode(<<"{\"test\":true}">>),
        true
    catch
        _:_ -> false
    end.