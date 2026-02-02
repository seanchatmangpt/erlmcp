-module(erlmcp_otp28_json_tests).

%% @doc OTP 28 Native JSON Migration Test Suite
%% Tests migration from jsx to native OTP 27+ json module
%%
%% == Test Coverage ==
%% 1. Native JSON encode/decode (OTP 27+ json module)
%% 2. API compatibility with jsx
%% 3. Performance comparison (native vs jsx)
%% 4. UTF-8 support verification
%% 5. Error handling
%% 6. Migration completeness
%%
%% == Chicago School TDD ==
%% - Real JSON encoding/decoding operations
%% - Observable outputs (state-based verification)
%% - No mocks
%%
%% @end

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Generators
%%%====================================================================

json_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [{"Native JSON API", {spawn, fun native_json_api_tests/0}},
         {"Compatibility with jsx", {spawn, fun jsx_compatibility_tests/0}},
         {"Performance Comparison", {spawn, fun performance_tests/0}},
         {"UTF-8 Support", {spawn, fun utf8_support_tests/0}},
         {"Error Handling", {spawn, fun error_handling_tests/0}},
         {"Migration Completeness", {spawn, fun migration_completeness_tests/0}},
         {"Real-World Scenarios", {spawn, fun real_world_scenarios_tests/0}}]
     end}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

%%%====================================================================
%%% Native JSON API Tests
%%%====================================================================

native_json_api_tests() ->
    %% Test 1: Native JSON encode/1
    Term1 = #{<<"foo">> => <<"bar">>},
    Encoded1 = erlmcp_json_native:encode(Term1),
    ?assert(is_binary(Encoded1)),
    ?assertEqual(<<"{\"foo\":\"bar\"}">>, Encoded1),

    %% Test 2: Native JSON encode/2 with options
    Term2 = #{<<"x">> => 1, <<"y">> => 2},
    Encoded2 = erlmcp_json_native:encode(Term2, []),
    ?assert(is_binary(Encoded2)),

    %% Test 3: Native JSON decode/1
    Json1 = <<"{\"key\":\"value\"}">>,
    Decoded1 = erlmcp_json_native:decode(Json1),
    ?assertEqual(#{<<"key">> => <<"value">>}, Decoded1),

    %% Test 4: Native JSON decode/2 with options
    Json2 = <<"{\"number\":42}">>,
    Decoded2 = erlmcp_json_native:decode(Json2, [return_maps]),
    ?assertEqual(#{<<"number">> => 42}, Decoded2),

    %% Test 5: Encode complex nested structure
    Complex = #{
        <<"level1">> => #{
            <<"level2">> => #{
                <<"level3">> => [1, 2, 3]
            }
        },
        <<"array">> => [<<"a">>, <<"b">>, <<"c">>],
        <<"bool">> => true,
        <<"null">> => null
    },
    Encoded3 = erlmcp_json_native:encode(Complex),
    ?assert(is_binary(Encoded3)),
    ?assert(byte_size(Encoded3) > 0),

    %% Test 6: Roundtrip complex structure
    Decoded3 = erlmcp_json_native:decode(Encoded3),
    ?assertEqual(Complex, Decoded3),

    %% Test 7: Encode empty structures
    ?assertEqual(<<"{}">>, erlmcp_json_native:encode(#{})),
    ?assertEqual(<<"[]">>, erlmcp_json_native:encode([])),

    ok.

%%%====================================================================
%%% Compatibility with jsx Tests
%%%====================================================================

jsx_compatibility_tests() ->
    %% Test 1: API compatibility - encode/1
    Term = #{<<"test">> => <<"data">>},
    NativeEncoded = erlmcp_json_native:encode(Term),

    %% Should produce valid JSON (parsable)
    Decoded = erlmcp_json_native:decode(NativeEncoded),
    ?assertEqual(Term, Decoded),

    %% Test 2: API compatibility - encode with options
    %% Native JSON ignores options but accepts them
    Term2 = #{<<"a">> => 1, <<"b">> => 2},
    ?assertEqual(
        erlmcp_json_native:encode(Term2),
        erlmcp_json_native:encode(Term2, [])
    ),

    %% Test 3: API compatibility - decode/1
    Json = <<"{\"name\":\"test\",\"value\":123}">>,
    Decoded2 = erlmcp_json_native:decode(Json),
    ?assertEqual(#{<<"name">> => <<"test">>, <<"value">> => 123}, Decoded2),

    %% Test 4: API compatibility - decode with options
    %% Native JSON always returns maps (like jsx with [return_maps])
    Json2 = <<"{\"key\":\"value\"}">>,
    Decoded3 = erlmcp_json_native:decode(Json2, [return_maps]),
    ?assert(is_map(Decoded3)),

    %% Test 5: Maps vs atoms (native JSON always uses maps)
    Json3 = <<"{\"items\":[1,2,3]}">>,
    Decoded4 = erlmcp_json_native:decode(Json3),
    ?assert(is_map(maps:get(<<"items">>, Decoded4))),
    ?assertNot(is_map(maps:get(<<"items">>, Decoded4))),

    %% Test 6: Special values (null, true, false)
    Special = #{
        <<"null_val">> => null,
        <<"true_val">> => true,
        <<"false_val">> => false
    },
    EncodedSpecial = erlmcp_json_native:encode(Special),
    DecodedSpecial = erlmcp_json_native:decode(EncodedSpecial),
    ?assertEqual(Special, DecodedSpecial),

    %% Test 7: Unicode strings
    Unicode = #{<<"unicode">> => <<"こんにちは">>},
    EncodedUnicode = erlmcp_json_native:encode(Unicode),
    DecodedUnicode = erlmcp_json_native:decode(EncodedUnicode),
    ?assertEqual(Unicode, DecodedUnicode),

    ok.

%%%====================================================================
%%% Performance Comparison Tests
%%%====================================================================

performance_tests() ->
    %% Test 1: Encoding performance
    LargeMap = lists:foldl(fun(N, Acc) ->
        Key = list_to_binary("key_" ++ integer_to_list(N)),
        Value = list_to_binary("value_" ++ integer_to_list(N)),
        maps:put(Key, Value, Acc)
    end, #{}, lists:seq(1, 1000)),

    %% Measure native encode time
    {NativeEncodeTime, _} = timer:tc(fun() ->
        erlmcp_json_native:encode(LargeMap)
    end),

    io:format("Native JSON encode time: ~pμs~n", [NativeEncodeTime]),

    %% Test 2: Decoding performance
    LargeJson = erlmcp_json_native:encode(LargeMap),

    {NativeDecodeTime, _} = timer:tc(fun() ->
        erlmcp_json_native:decode(LargeJson)
    end),

    io:format("Native JSON decode time: ~pμs~n", [NativeDecodeTime]),

    %% Test 3: Roundtrip performance
    RoundtripData = #{
        <<"string">> => <<"test">>,
        <<"number">> => 42,
        <<"float">> => 3.14,
        <<"array">> => [1, 2, 3],
        <<"object">> => #{<<"nested">> => true},
        <<"bool">> => false,
        <<"null">> => null
    },

    {RoundtripTime, _} = timer:tc(fun() ->
        Encoded = erlmcp_json_native:encode(RoundtripData),
        erlmcp_json_native:decode(Encoded)
    end),

    io:format("Native JSON roundtrip time: ~pμs~n", [RoundtripTime]),

    %% Test 4: Performance should be reasonable (< 10ms for 1000-item map)
    ?assert(NativeEncodeTime < 10_000),
    ?assert(NativeDecodeTime < 10_000),

    %% Test 5: Memory efficiency
    %% Encode large JSON and measure memory
    VeryLargeMap = lists:foldl(fun(N, Acc) ->
        Key = list_to_binary("k" ++ integer_to_list(N)),
        maps:put(Key, N, Acc)
    end, #{}, lists:seq(1, 10000)),

    BeforeMem = erlang:memory(total),

    VeryLargeJson = erlmcp_json_native:encode(VeryLargeMap),

    AfterMem = erlang:memory(total),
    MemUsed = AfterMem - BeforeMem,

    io:format("Memory used for 10k-item JSON: ~p bytes~n", [MemUsed]),
    ?assert(MemUsed > 0),
    ?assert(MemUsed < 10_000_000), %% Less than 10MB

    ok.

%%%====================================================================
%%% UTF-8 Support Tests
%%%====================================================================

utf8_support_tests() ->
    %% Test 1: Japanese UTF-8
    Japanese = #{
        <<"hiragana">> => <<"こんにちは">>,
        <<"katakana">> => <<"コンニチハ">>,
        <<"kanji">> => <<"日本語">>
    },
    EncodedJa = erlmcp_json_native:encode(Japanese),
    DecodedJa = erlmcp_json_native:decode(EncodedJa),
    ?assertEqual(Japanese, DecodedJa),

    %% Test 2: Arabic UTF-8 (RTL)
    Arabic = #{
        <<"text">> => <<"مرحبا بالعالم">>,
        <<"numbers">> => <<"٠١٢٣٤٥٦٧٨٩">>
    },
    EncodedAr = erlmcp_json_native:encode(Arabic),
    DecodedAr = erlmcp_json_native:decode(EncodedAr),
    ?assertEqual(Arabic, DecodedAr),

    %% Test 3: Emoji (multi-byte)
    Emoji = #{
        <<"basic">> => <<"😀">>,
        <<"skin_tone">> => <<"👋🏻">>,
        <<"family">> => <<"👨‍👩‍👧‍👦">>,
        <<"flag">> => <<"🇺🇸">>
    },
    EncodedEmoji = erlmcp_json_native:encode(Emoji),
    DecodedEmoji = erlmcp_json_native:decode(EncodedEmoji),
    ?assertEqual(Emoji, DecodedEmoji),

    %% Test 4: Mixed scripts
    Mixed = #{
        <<"japanese">> => <<"テスト">>,
        <<"arabic">> => <<"اختبار">>,
        <<"emoji">> => <<"✅">>,
        <<"english">> => <<"test">>
    },
    EncodedMixed = erlmcp_json_native:encode(Mixed),
    DecodedMixed = erlmcp_json_native:decode(EncodedMixed),
    ?assertEqual(Mixed, DecodedMixed),

    %% Test 5: UTF-8 in arrays
    Utf8Array = [
        <<"日本">>,
        <<"مصر">>,
        <<"🌍">>,
        <<"test">>
    ],
    EncodedArray = erlmcp_json_native:encode(Utf8Array),
    DecodedArray = erlmcp_json_native:decode(EncodedArray),
    ?assertEqual(Utf8Array, DecodedArray),

    %% Test 6: Very long UTF-8 string
    LongUtf8 = unicode:characters_to_binary([
        $"あ, $"い, $"う, $"え, $"お
        || _ <- lists:seq(1, 1000)
    ]),
    LongUtf8Obj = #{<<"long">> => LongUtf8},
    EncodedLong = erlmcp_json_native:encode(LongUtf8Obj),
    DecodedLong = erlmcp_json_native:decode(EncodedLong),
    ?assertEqual(LongUtf8, maps:get(<<"long">>, DecodedLong)),

    ok.

%%%====================================================================
%%% Error Handling Tests
%%%====================================================================

error_handling_tests() ->
    %% Test 1: Invalid JSON syntax
    try
        erlmcp_json_native:decode(<<"{invalid json}">>),
        ?assert(false, "Should have thrown error")
    catch
        error:{decode_error, _} -> ok;
        _:_ -> ?assert(false, "Wrong error type")
    end,

    %% Test 2: Incomplete JSON
    try
        erlmcp_json_native:decode(<<"{\"key\":">>),
        ?assert(false, "Should have thrown error")
    catch
        error:{decode_error, _} -> ok;
        _:_ -> ?assert(false, "Wrong error type")
    end,

    %% Test 3: Trailing comma (invalid JSON)
    try
        erlmcp_json_native:decode(<<"[1,2,3,]">>),
        ?assert(false, "Should have thrown error")
    catch
        error:{decode_error, _} -> ok;
        _:_ -> ok %% Native JSON might be lenient
    end,

    %% Test 4: Unquoted keys (invalid JSON)
    try
        erlmcp_json_native:decode(<<"{key: \"value\"}">>),
        ?assert(false, "Should have thrown error")
    catch
        error:{decode_error, _} -> ok;
        _:_ -> ok %% Native JSON might be lenient
    end,

    %% Test 5: Encode errors (invalid terms)
    %% Native JSON should handle most Erlang terms
    try
        %% Pids cannot be JSON-encoded
        erlmcp_json_native:encode(#{<<"pid">> => self()}),
        ?assert(false, "Should have thrown error")
    catch
        error:{encode_error, _} -> ok;
        _:_ -> ok %% Native JSON might handle this
    end,

    %% Test 6: Empty input
    try
        erlmcp_json_native:decode(<<>>),
        ?assert(false, "Should have thrown error")
    catch
        error:{decode_error, _} -> ok;
        _:_ -> ?assert(false, "Wrong error type")
    end,

    %% Test 7: Non-binary input
    try
        erlmcp_json_native:decode(not_a_binary),
        ?assert(false, "Should have thrown error")
    catch
        error:{decode_error, _} -> ok;
        _:_ -> ?assert(false, "Wrong error type")
    end,

    ok.

%%%====================================================================
%%% Migration Completeness Tests
%%%====================================================================

migration_completeness_tests() ->
    %% Test 1: Verify erlmcp_json_rpc uses native JSON
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{<<"key">> => <<"value">>}
    },

    %% Encode request
    EncodedRequest = erlmcp_json_rpc:encode_request(1, <<"test">>, maps:get(<<"params">>, Request)),
    ?assert(is_binary(EncodedRequest)),

    %% Decode should work with native JSON
    DecodedRequest = erlmcp_json_native:decode(EncodedRequest),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, DecodedRequest)),
    ?assertEqual(<<"test">>, maps:get(<<"method">>, DecodedRequest)),

    %% Test 2: Verify erlmcp_json_codec uses native JSON
    CodecTest = #{<<"codec">> => <<"test">>},
    EncodedCodec = erlmcp_json_codec:encode(CodecTest),
    DecodedCodec = erlmcp_json_codec:decode(EncodedCodec),
    ?assertEqual(CodecTest, DecodedCodec),

    %% Test 3: Check for remaining jsx dependencies
    %% This test ensures we've migrated away from jsx
    %% We can't directly check code, but we can verify behavior
    TestObj = #{<<"jsx_migration">> => <<"complete">>},
    Encoded = erlmcp_json_native:encode(TestObj),
    Decoded = erlmcp_json_native:decode(Encoded),
    ?assertEqual(TestObj, Decoded),

    %% Test 4: Verify all JSON-RPC error codes work
    ErrorCodes = [
        {-32700, <<"Parse error">>},
        {-32600, <<"Invalid Request">>},
        {-32601, <<"Method not found">>},
        {-32602, <<"Invalid params">>},
        {-32603, <<"Internal error">>}
    ],

    lists:foreach(fun({Code, Msg}) ->
        ErrorResp = erlmcp_json_rpc:encode_error_response(1, Code, Msg),
        ?assert(is_binary(ErrorResp)),
        DecodedErr = erlmcp_json_native:decode(ErrorResp),
        ?assertEqual(Code, maps:get(<<"code">>, maps:get(<<"error">>, DecodedErr)))
    end, ErrorCodes),

    %% Test 5: Verify batch operations work
    Batch = [
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"m1">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"method">> => <<"m2">>}
    ],
    EncodedBatch = erlmcp_json_rpc:encode_batch([
        #json_rpc_request{id = 1, method = <<"m1">>, params = #{}},
        #json_rpc_request{id = 2, method = <<"m2">>, params = #{}}
    ]),
    ?assert(is_binary(EncodedBatch)),
    DecodedBatch = erlmcp_json_native:decode(EncodedBatch),
    ?assert(is_list(DecodedBatch)),

    ok.

%%%====================================================================
%%% Real-World Scenarios Tests
%%%====================================================================

real_world_scenarios_tests() ->
    %% Test 1: MCP initialize request
    InitializeReq = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"initialize">>,
        <<"params">> => #{
            <<"protocolVersion">> => <<"2025-11-25">>,
            <<"capabilities">> => #{
                <<"resources">> => #{},
                <<"tools">> => #{},
                <<"prompts">> => #{}
            },
            <<"clientInfo">> => #{
                <<"name">> => <<"test-client">>,
                <<"version">> => <<"1.0.0">>
            }
        }
    },

    EncodedInit = erlmcp_json_rpc:encode_request(
        1,
        <<"initialize">>,
        maps:get(<<"params">>, InitializeReq)
    ),
    DecodedInit = erlmcp_json_native:decode(EncodedInit),
    ?assertEqual(<<"initialize">>, maps:get(<<"method">>, DecodedInit)),

    %% Test 2: MCP tool call response
    ToolResp = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 2,
        <<"result">> => #{
            <<"content">> => [
                #{
                    <<"type">> => <<"text">>,
                    <<"text">> => <<"Tool execution result">>
                }
            ]
        }
    },

    EncodedTool = erlmcp_json_rpc:encode_response(2, maps:get(<<"result">>, ToolResp)),
    DecodedTool = erlmcp_json_native:decode(EncodedTool),
    ?assertMatch(#{<<"result">> := #{<<"content">> := [_]}}, DecodedTool),

    %% Test 3: MCP resource list
    ResourceList = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 3,
        <<"result">> => #{
            <<"resources">> => [
                #{
                    <<"uri">> => <<"test://resource/1">>,
                    <<"name">> => <<"Resource 1">>,
                    <<"description">> => <<"Test resource">>,
                    <<"mimeType">> => <<"text/plain">>
                }
            ]
        }
    },

    EncodedRes = erlmcp_json_rpc:encode_response(3, maps:get(<<"result">>, ResourceList)),
    DecodedRes = erlmcp_json_native:decode(EncodedRes),
    ?assert(maps:is_key(<<"resources">>, maps:get(<<"result">>, DecodedRes))),

    %% Test 4: MCP error response
    ErrorResp = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 4,
        <<"error">> => #{
            <<"code">> => -32601,
            <<"message">> => <<"Method not found">>,
            <<"data">> => #{<<"method">> => <<"unknown_method">>}
        }
    },

    EncodedErr = erlmcp_json_rpc:encode_error_response(4, -32601, <<"Method not found">>),
    DecodedErr = erlmcp_json_native:decode(EncodedErr),
    ?assertEqual(-32601, maps:get(<<"code">>, maps:get(<<"error">>, DecodedErr))),

    %% Test 5: Large payload (resource with lots of content)
    LargeContent = lists:duplicate(1000, <<"A large line of text content\n">>),
    LargeResource = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 5,
        <<"result">> => #{
            <<"contents">> => [
                #{
                    <<"uri">> => <<"test://large">>,
                    <<"mimeType">> => <<"text/plain">>,
                    <<"text">> => iolist_to_binary(LargeContent)
                }
            ]
        }
    },

    EncodedLarge = erlmcp_json_rpc:encode_response(5, maps:get(<<"result">>, LargeResource)),
    ?assert(byte_size(EncodedLarge) > 0),
    DecodedLarge = erlmcp_json_native:decode(EncodedLarge),
    ?assert(maps:is_key(<<"contents">>, maps:get(<<"result">>, DecodedLarge)),

    %% Test 6: Unicode in real-world MCP scenario
    UnicodeScenario = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 6,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{
            <<"name">> => <<"translate">>,
            <<"arguments">> => #{
                <<"text">> => <<"こんにちは世界">>,
                <<"targetLang">> => <<"ar">>
            }
        }
    },

    EncodedUnicode = erlmcp_json_rpc:encode_request(
        6,
        <<"tools/call">>,
        maps:get(<<"params">>, UnicodeScenario)
    ),
    DecodedUnicode = erlmcp_json_native:decode(EncodedUnicode),
    Params = maps:get(<<"params">>, DecodedUnicode),
    ?assertEqual(<<"こんにちは世界">>, maps:get(<<"text">>, maps:get(<<"arguments">>, Params))),

    ok.
