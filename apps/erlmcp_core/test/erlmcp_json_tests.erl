-module(erlmcp_json_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite for JSON Encoding/Decoding Compatibility
%% Chicago School TDD: Test ONLY observable behavior through public API
%%
%% Purpose: Verify jsx and native OTP json module produce equivalent output
%% OTP Support: jsx (OTP 25-28), native json (OTP 27-28)
%%====================================================================

%%====================================================================
%% Test: Detect which JSON implementations are available
%% Why: OTP 25-26 only have jsx, OTP 27+ have both
%%====================================================================

has_native_json() ->
    erlang:function_exported(json, encode, 1).

json_decode(Binary) ->
    case has_native_json() of
        true -> json:decode(Binary);
        false -> jsx:decode(Binary, [return_maps])
    end.

json_encode(Term) ->
    case has_native_json() of
        true -> json:encode(Term);
        false -> jsx:encode(Term)
    end.

%%====================================================================
%% Basic Types Tests
%% Test: Encode/decode integers, floats, strings, atoms, booleans, null
%% Why: These are fundamental JSON types that must round-trip correctly
%%====================================================================

json_encode_basic_types_test() ->
    %% Test: Integer encoding
    IntJson = jsx:encode(42),
    ?assertEqual(<<"42">>, IntJson),
    ?assertEqual(42, jsx:decode(IntJson)),

    %% Test: Float encoding
    FloatJson = jsx:encode(3.14159),
    ?assert(is_binary(FloatJson)),
    ?assertEqual(3.14159, jsx:decode(FloatJson)),

    %% Test: String encoding
    StringJson = jsx:encode(<<"hello">>),
    ?assertEqual(<<"\"hello\"">>, StringJson),
    ?assertEqual(<<"hello">>, jsx:decode(StringJson)),

    %% Test: Boolean true
    TrueJson = jsx:encode(true),
    ?assertEqual(<<"true">>, TrueJson),
    ?assertEqual(true, jsx:decode(TrueJson)),

    %% Test: Boolean false
    FalseJson = jsx:encode(false),
    ?assertEqual(<<"false">>, FalseJson),
    ?assertEqual(false, jsx:decode(FalseJson)),

    %% Test: Null encoding
    NullJson = jsx:encode(null),
    ?assertEqual(<<"null">>, NullJson),
    ?assertEqual(null, jsx:decode(NullJson)).

%%====================================================================
%% Lists (JSON Arrays) Tests
%% Test: Arrays of various types
%% Why: JSON arrays must preserve order and type of all elements
%%====================================================================

json_encode_lists_test() ->
    %% Test: Empty array
    EmptyJson = jsx:encode([]),
    ?assertEqual(<<"[]">>, EmptyJson),
    ?assertEqual([], jsx:decode(EmptyJson)),

    %% Test: Array of integers
    IntArrayJson = jsx:encode([1, 2, 3, 4, 5]),
    ?assert(is_binary(IntArrayJson)),
    ?assertEqual([1, 2, 3, 4, 5], jsx:decode(IntArrayJson)),

    %% Test: Array of mixed types
    MixedJson = jsx:encode([1, <<"test">>, true, null, 3.14]),
    ?assert(is_binary(MixedJson)),
    Decoded = jsx:decode(MixedJson),
    ?assertEqual(5, length(Decoded)),

    %% Test: Nested arrays
    NestedJson = jsx:encode([[1, 2], [3, 4], [5, 6]]),
    ?assert(is_binary(NestedJson)),
    ?assertEqual([[1, 2], [3, 4], [5, 6]], jsx:decode(NestedJson)).

%%====================================================================
%% Maps (JSON Objects) Tests
%% Test: JSON objects with binary/atom keys
%% Why: Maps are core JSON structure, key handling varies by implementation
%%====================================================================

json_encode_maps_test() ->
    %% Test: Empty map
    EmptyJson = jsx:encode(#{}),
    ?assertEqual(<<"{}">> , EmptyJson),
    ?assertEqual(#{}, jsx:decode(EmptyJson, [return_maps])),

    %% Test: Map with binary keys
    BinaryKeyMap = #{<<"name">> => <<"John">>, <<"age">> => 30},
    BinaryKeyJson = jsx:encode(BinaryKeyMap),
    ?assert(is_binary(BinaryKeyJson)),
    DecodedBinary = jsx:decode(BinaryKeyJson, [return_maps]),
    ?assertEqual(<<"John">>, maps:get(<<"name">>, DecodedBinary)),
    ?assertEqual(30, maps:get(<<"age">>, DecodedBinary)),

    %% Test: Map with various value types
    MixedMap = #{
        <<"string">> => <<"test">>,
        <<"number">> => 42,
        <<"float">> => 3.14,
        <<"bool">> => true,
        <<"null">> => null
    },
    MixedJson = jsx:encode(MixedMap),
    ?assert(is_binary(MixedJson)),
    DecodedMixed = jsx:decode(MixedJson, [return_maps]),
    ?assertEqual(5, maps:size(DecodedMixed)).

%%====================================================================
%% Nested Structures Tests
%% Test: Deeply nested JSON objects and arrays
%% Why: Real-world JSON often has deep nesting, must handle correctly
%%====================================================================

json_encode_nested_structures_test() ->
    %% Test: Nested objects
    Nested = #{
        <<"level1">> => #{
            <<"level2">> => #{
                <<"level3">> => #{
                    <<"value">> => <<"deep">>
                }
            }
        }
    },
    NestedJson = jsx:encode(Nested),
    ?assert(is_binary(NestedJson)),
    DecodedNested = jsx:decode(NestedJson, [return_maps]),
    Level1 = maps:get(<<"level1">>, DecodedNested),
    Level2 = maps:get(<<"level2">>, Level1),
    Level3 = maps:get(<<"level3">>, Level2),
    ?assertEqual(<<"deep">>, maps:get(<<"value">>, Level3)),

    %% Test: Arrays of objects
    ArrayOfObjects = [
        #{<<"id">> => 1, <<"name">> => <<"first">>},
        #{<<"id">> => 2, <<"name">> => <<"second">>},
        #{<<"id">> => 3, <<"name">> => <<"third">>}
    ],
    ArrayJson = jsx:encode(ArrayOfObjects),
    ?assert(is_binary(ArrayJson)),
    DecodedArray = jsx:decode(ArrayJson, [return_maps]),
    ?assertEqual(3, length(DecodedArray)),

    %% Test: Object with arrays
    ObjectWithArrays = #{
        <<"numbers">> => [1, 2, 3],
        <<"strings">> => [<<"a">>, <<"b">>, <<"c">>],
        <<"mixed">> => [1, <<"two">>, true]
    },
    ObjectJson = jsx:encode(ObjectWithArrays),
    ?assert(is_binary(ObjectJson)),
    DecodedObject = jsx:decode(ObjectJson, [return_maps]),
    ?assertEqual(3, maps:size(DecodedObject)).

%%====================================================================
%% Unicode Tests
%% Test: UTF-8 characters, special escapes
%% Why: Unicode handling is critical for international applications
%%====================================================================

json_encode_unicode_test() ->
    %% Test: Basic Unicode characters
    UnicodeText = <<"Hello 世界 🌍"/utf8>>,
    UnicodeJson = jsx:encode(UnicodeText),
    ?assert(is_binary(UnicodeJson)),
    DecodedUnicode = jsx:decode(UnicodeJson),
    ?assertEqual(UnicodeText, DecodedUnicode),

    %% Test: Various Unicode ranges
    UnicodeMap = #{
        <<"chinese">> => <<"你好"/utf8>>,
        <<"russian">> => <<"Привет"/utf8>>,
        <<"arabic">> => <<"مرحبا"/utf8>>,
        <<"emoji">> => <<"🎉🎊✨"/utf8>>
    },
    UnicodeMapJson = jsx:encode(UnicodeMap),
    ?assert(is_binary(UnicodeMapJson)),
    DecodedUnicodeMap = jsx:decode(UnicodeMapJson, [return_maps]),
    ?assertEqual(4, maps:size(DecodedUnicodeMap)),

    %% Test: Escaped characters
    EscapedText = <<"Line1\nLine2\tTabbed\"Quoted\"">>,
    EscapedJson = jsx:encode(EscapedText),
    ?assert(is_binary(EscapedJson)),
    DecodedEscaped = jsx:decode(EscapedJson),
    ?assertEqual(EscapedText, DecodedEscaped).

%%====================================================================
%% Decode Basic Types Tests
%% Test: Decoding all basic JSON types
%% Why: Decoding must correctly parse JSON primitives
%%====================================================================

json_decode_basic_types_test() ->
    %% Test: Decode integer
    ?assertEqual(42, jsx:decode(<<"42">>)),

    %% Test: Decode float
    ?assertEqual(3.14, jsx:decode(<<"3.14">>)),

    %% Test: Decode string
    ?assertEqual(<<"hello">>, jsx:decode(<<"\"hello\"">>)),

    %% Test: Decode boolean
    ?assertEqual(true, jsx:decode(<<"true">>)),
    ?assertEqual(false, jsx:decode(<<"false">>)),

    %% Test: Decode null
    ?assertEqual(null, jsx:decode(<<"null">>)),

    %% Test: Decode array
    ?assertEqual([1, 2, 3], jsx:decode(<<"[1,2,3]">>)),

    %% Test: Decode object
    Decoded = jsx:decode(<<"{\"key\":\"value\"}">>, [return_maps]),
    ?assertEqual(#{<<"key">> => <<"value">>}, Decoded).

%%====================================================================
%% Decode Maps Option Tests
%% Test: Verify [return_maps] option works correctly
%% Why: Default jsx returns proplists, but we need maps for erlmcp
%%====================================================================

json_decode_maps_test() ->
    Json = <<"{\"name\":\"John\",\"age\":30}">>,

    %% Test: Decode with return_maps
    DecodedMap = jsx:decode(Json, [return_maps]),
    ?assert(is_map(DecodedMap)),
    ?assertEqual(<<"John">>, maps:get(<<"name">>, DecodedMap)),
    ?assertEqual(30, maps:get(<<"age">>, DecodedMap)),

    %% Test: Nested objects with return_maps
    NestedJson = <<"{\"user\":{\"name\":\"Jane\",\"age\":25}}">>,
    DecodedNested = jsx:decode(NestedJson, [return_maps]),
    User = maps:get(<<"user">>, DecodedNested),
    ?assert(is_map(User)),
    ?assertEqual(<<"Jane">>, maps:get(<<"name">>, User)),

    %% Test: Array of objects with return_maps
    ArrayJson = <<"[{\"id\":1},{\"id\":2},{\"id\":3}]">>,
    DecodedArray = jsx:decode(ArrayJson, [return_maps]),
    ?assert(is_list(DecodedArray)),
    ?assertEqual(3, length(DecodedArray)),
    [First | _] = DecodedArray,
    ?assert(is_map(First)).

%%====================================================================
%% Round-trip Tests
%% Test: Encode then decode returns original data
%% Why: Data must survive encode/decode cycle without loss
%%====================================================================

json_roundtrip_test() ->
    %% Test: Simple types round-trip
    ?assertEqual(42, jsx:decode(jsx:encode(42))),
    ?assertEqual(3.14, jsx:decode(jsx:encode(3.14))),
    ?assertEqual(<<"test">>, jsx:decode(jsx:encode(<<"test">>))),
    ?assertEqual(true, jsx:decode(jsx:encode(true))),
    ?assertEqual(null, jsx:decode(jsx:encode(null))),

    %% Test: Array round-trip
    Array = [1, 2, 3, <<"test">>, true, null],
    ?assertEqual(Array, jsx:decode(jsx:encode(Array))),

    %% Test: Map round-trip
    Map = #{
        <<"string">> => <<"value">>,
        <<"number">> => 42,
        <<"bool">> => true,
        <<"null">> => null,
        <<"array">> => [1, 2, 3]
    },
    RoundTrip = jsx:decode(jsx:encode(Map), [return_maps]),
    ?assertEqual(Map, RoundTrip),

    %% Test: Complex nested structure round-trip
    Complex = #{
        <<"users">> => [
            #{<<"id">> => 1, <<"name">> => <<"Alice">>, <<"active">> => true},
            #{<<"id">> => 2, <<"name">> => <<"Bob">>, <<"active">> => false}
        ],
        <<"metadata">> => #{
            <<"total">> => 2,
            <<"page">> => 1
        }
    },
    ComplexRoundTrip = jsx:decode(jsx:encode(Complex), [return_maps]),
    ?assertEqual(Complex, ComplexRoundTrip).

%%====================================================================
%% Error Handling Tests
%% Test: Test error cases with real invalid JSON
%% Why: Must handle malformed JSON gracefully with clear errors
%%====================================================================

json_error_handling_test() ->
    %% Test: Invalid JSON syntax
    ?assertError(badarg, jsx:decode(<<"not valid json">>)),
    ?assertError(badarg, jsx:decode(<<"{invalid}">>)),
    ?assertError(badarg, jsx:decode(<<"{\"key\": invalid}">>)),

    %% Test: Incomplete JSON
    ?assertError(badarg, jsx:decode(<<"{\"key\":">>)),
    ?assertError(badarg, jsx:decode(<<"[1,2,">>)),

    %% Test: Invalid UTF-8 (create invalid UTF-8 sequence)
    InvalidUtf8 = <<"{\"key\":\"", 16#FF, 16#FE, "\"}" >>,
    ?assertError(badarg, jsx:decode(InvalidUtf8)),

    %% Test: Empty string
    ?assertError(badarg, jsx:decode(<<"">>)),

    %% Test: Only whitespace
    ?assertError(badarg, jsx:decode(<<"   ">>)).

%%====================================================================
%% Large Payload Tests
%% Test: 10KB+ JSON documents
%% Why: Performance and correctness with real-world large payloads
%%====================================================================

json_large_payload_test() ->
    %% Test: Large array (10000 integers = ~50KB)
    LargeArray = lists:seq(1, 10000),
    LargeArrayJson = jsx:encode(LargeArray),
    ?assert(byte_size(LargeArrayJson) > 10000),
    ?assertEqual(LargeArray, jsx:decode(LargeArrayJson)),

    %% Test: Large object (1000 key-value pairs)
    LargeMap = maps:from_list([
        {list_to_binary("key" ++ integer_to_list(N)), N}
        || N <- lists:seq(1, 1000)
    ]),
    LargeMapJson = jsx:encode(LargeMap),
    ?assert(byte_size(LargeMapJson) > 10000),
    DecodedLargeMap = jsx:decode(LargeMapJson, [return_maps]),
    ?assertEqual(1000, maps:size(DecodedLargeMap)),

    %% Test: Deep nesting (100 levels)
    DeepNested = lists:foldl(
        fun(_, Acc) -> #{<<"level">> => Acc} end,
        #{<<"value">> => <<"deep">>},
        lists:seq(1, 100)
    ),
    DeepJson = jsx:encode(DeepNested),
    ?assert(byte_size(DeepJson) > 1000),
    DecodedDeep = jsx:decode(DeepJson, [return_maps]),
    ?assert(is_map(DecodedDeep)),

    %% Test: Large strings
    LargeString = binary:copy(<<"X">>, 50000),
    LargeStringJson = jsx:encode(LargeString),
    ?assert(byte_size(LargeStringJson) > 50000),
    ?assertEqual(LargeString, jsx:decode(LargeStringJson)).

%%====================================================================
%% Native JSON Compatibility Tests (OTP 27+)
%% Test: Compare jsx vs native json module if both available
%% Why: Ensure semantic equivalence between implementations
%%====================================================================

json_compatibility_jsx_vs_native_test_() ->
    case has_native_json() of
        true -> jsx_vs_native_tests();
        false -> {setup, fun() -> ok end, fun(_) -> ok end, fun(_) -> [
            ?_test(io:format("~nNative json module not available (OTP < 27), skipping compatibility tests~n"))
        ] end}
    end.

jsx_vs_native_tests() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
             ?_test(test_jsx_native_basic_types()),
             ?_test(test_jsx_native_arrays()),
             ?_test(test_jsx_native_objects()),
             ?_test(test_jsx_native_unicode()),
             ?_test(test_jsx_native_roundtrip())
         ]
     end}.

test_jsx_native_basic_types() ->
    %% Test: Both produce same output for basic types
    ?assertEqual(jsx:encode(42), json:encode(42)),
    ?assertEqual(jsx:encode(3.14), json:encode(3.14)),
    ?assertEqual(jsx:encode(true), json:encode(true)),
    ?assertEqual(jsx:encode(false), json:encode(false)),
    ?assertEqual(jsx:encode(null), json:encode(null)),

    %% Test: Both decode identically
    ?assertEqual(jsx:decode(<<"42">>), json:decode(<<"42">>)),
    ?assertEqual(jsx:decode(<<"true">>), json:decode(<<"true">>)),
    ?assertEqual(jsx:decode(<<"null">>), json:decode(<<"null">>)).

test_jsx_native_arrays() ->
    Array = [1, 2, 3, <<"test">>, true, null],
    JsxJson = jsx:encode(Array),
    NativeJson = json:encode(Array),

    %% Test: Both produce valid JSON (may differ in whitespace)
    ?assertEqual(Array, jsx:decode(JsxJson)),
    ?assertEqual(Array, json:decode(NativeJson)),

    %% Test: Cross-decode works
    ?assertEqual(Array, jsx:decode(NativeJson)),
    ?assertEqual(Array, json:decode(JsxJson)).

test_jsx_native_objects() ->
    Map = #{
        <<"name">> => <<"John">>,
        <<"age">> => 30,
        <<"active">> => true
    },
    JsxJson = jsx:encode(Map),
    NativeJson = json:encode(Map),

    %% Test: Both produce valid JSON
    JsxDecoded = jsx:decode(JsxJson, [return_maps]),
    NativeDecoded = json:decode(NativeJson),

    ?assertEqual(Map, JsxDecoded),
    ?assertEqual(Map, NativeDecoded),

    %% Test: Cross-decode works
    ?assertEqual(Map, jsx:decode(NativeJson, [return_maps])),
    ?assertEqual(Map, json:decode(JsxJson)).

test_jsx_native_unicode() ->
    Unicode = <<"Hello 世界 🌍"/utf8>>,
    JsxJson = jsx:encode(Unicode),
    NativeJson = json:encode(Unicode),

    %% Test: Both handle Unicode correctly
    ?assertEqual(Unicode, jsx:decode(JsxJson)),
    ?assertEqual(Unicode, json:decode(NativeJson)),

    %% Test: Cross-decode works
    ?assertEqual(Unicode, jsx:decode(NativeJson)),
    ?assertEqual(Unicode, json:decode(JsxJson)).

test_jsx_native_roundtrip() ->
    Complex = #{
        <<"users">> => [
            #{<<"id">> => 1, <<"name">> => <<"Alice">>},
            #{<<"id">> => 2, <<"name">> => <<"Bob">>}
        ],
        <<"count">> => 2,
        <<"active">> => true
    },

    %% Test: jsx round-trip
    JsxRoundTrip = jsx:decode(jsx:encode(Complex), [return_maps]),
    ?assertEqual(Complex, JsxRoundTrip),

    %% Test: native round-trip
    NativeRoundTrip = json:decode(json:encode(Complex)),
    ?assertEqual(Complex, NativeRoundTrip),

    %% Test: Cross round-trip (jsx encode, native decode)
    CrossRoundTrip1 = json:decode(jsx:encode(Complex)),
    ?assertEqual(Complex, CrossRoundTrip1),

    %% Test: Cross round-trip (native encode, jsx decode)
    CrossRoundTrip2 = jsx:decode(json:encode(Complex), [return_maps]),
    ?assertEqual(Complex, CrossRoundTrip2).

%%====================================================================
%% Empty Collections Tests
%% Test: Empty arrays and objects
%% Why: Edge case that must be handled correctly
%%====================================================================

json_empty_collections_test() ->
    %% Test: Empty array
    EmptyArray = [],
    EmptyArrayJson = jsx:encode(EmptyArray),
    ?assertEqual(<<"[]">>, EmptyArrayJson),
    ?assertEqual(EmptyArray, jsx:decode(EmptyArrayJson)),

    %% Test: Empty map
    EmptyMap = #{},
    EmptyMapJson = jsx:encode(EmptyMap),
    ?assertEqual(<<"{}">> , EmptyMapJson),
    ?assertEqual(EmptyMap, jsx:decode(EmptyMapJson, [return_maps])),

    %% Test: Map with empty array value
    MapWithEmptyArray = #{<<"items">> => []},
    MapWithEmptyArrayJson = jsx:encode(MapWithEmptyArray),
    DecodedMapWithEmptyArray = jsx:decode(MapWithEmptyArrayJson, [return_maps]),
    ?assertEqual([], maps:get(<<"items">>, DecodedMapWithEmptyArray)),

    %% Test: Array with empty map
    ArrayWithEmptyMap = [#{}],
    ArrayWithEmptyMapJson = jsx:encode(ArrayWithEmptyMap),
    DecodedArrayWithEmptyMap = jsx:decode(ArrayWithEmptyMapJson, [return_maps]),
    ?assertEqual([#{}], DecodedArrayWithEmptyMap).

%%====================================================================
%% Special Float Values Tests
%% Test: Edge case floats (very large, very small, negative)
%% Why: Float encoding can have precision issues
%%====================================================================

json_special_floats_test() ->
    %% Test: Very small float
    SmallFloat = 0.000001,
    SmallFloatJson = jsx:encode(SmallFloat),
    DecodedSmall = jsx:decode(SmallFloatJson),
    ?assert(abs(SmallFloat - DecodedSmall) < 0.0000001),

    %% Test: Very large float
    LargeFloat = 999999999.999999,
    LargeFloatJson = jsx:encode(LargeFloat),
    DecodedLarge = jsx:decode(LargeFloatJson),
    ?assert(abs(LargeFloat - DecodedLarge) < 1.0),

    %% Test: Negative float
    NegativeFloat = -3.14159,
    NegativeFloatJson = jsx:encode(NegativeFloat),
    ?assertEqual(NegativeFloat, jsx:decode(NegativeFloatJson)),

    %% Test: Zero
    ?assertEqual(0.0, jsx:decode(jsx:encode(0.0))),
    ?assertEqual(0, jsx:decode(jsx:encode(0))).

%%====================================================================
%% Mixed Key Types Tests
%% Test: Maps with binary keys (JSON standard)
%% Why: JSON only supports string keys, must verify correct handling
%%====================================================================

json_binary_keys_test() ->
    %% Test: All binary keys (standard JSON)
    BinaryKeyMap = #{
        <<"key1">> => <<"value1">>,
        <<"key2">> => 42,
        <<"key3">> => true
    },
    BinaryJson = jsx:encode(BinaryKeyMap),
    DecodedBinary = jsx:decode(BinaryJson, [return_maps]),
    ?assertEqual(BinaryKeyMap, DecodedBinary),

    %% Test: Nested maps with binary keys
    NestedBinaryMap = #{
        <<"outer">> => #{
            <<"inner">> => #{
                <<"deep">> => <<"value">>
            }
        }
    },
    NestedJson = jsx:encode(NestedBinaryMap),
    DecodedNested = jsx:decode(NestedJson, [return_maps]),
    ?assertEqual(NestedBinaryMap, DecodedNested),

    %% Test: Keys with special characters
    SpecialKeyMap = #{
        <<"key-with-dash">> => 1,
        <<"key.with.dot">> => 2,
        <<"key_with_underscore">> => 3,
        <<"key with space">> => 4
    },
    SpecialJson = jsx:encode(SpecialKeyMap),
    DecodedSpecial = jsx:decode(SpecialJson, [return_maps]),
    ?assertEqual(SpecialKeyMap, DecodedSpecial).

%%====================================================================
%% Whitespace Handling Tests
%% Test: JSON with various whitespace patterns
%% Why: Decoders must handle whitespace correctly per JSON spec
%%====================================================================

json_whitespace_test() ->
    %% Test: Compact JSON (no whitespace)
    CompactJson = <<"{\"key\":\"value\"}">>,
    ?assertEqual(#{<<"key">> => <<"value">>}, jsx:decode(CompactJson, [return_maps])),

    %% Test: JSON with spaces
    SpacedJson = <<"{ \"key\" : \"value\" }">>,
    ?assertEqual(#{<<"key">> => <<"value">>}, jsx:decode(SpacedJson, [return_maps])),

    %% Test: JSON with newlines and tabs
    FormattedJson = <<"{\n\t\"key\": \"value\"\n}">>,
    ?assertEqual(#{<<"key">> => <<"value">>}, jsx:decode(FormattedJson, [return_maps])),

    %% Test: Array with whitespace
    ArrayJson = <<"[ 1 , 2 , 3 ]">>,
    ?assertEqual([1, 2, 3], jsx:decode(ArrayJson)).

%%====================================================================
%% Number Precision Tests
%% Test: Large integers, precise decimals
%% Why: JSON numbers can lose precision, must verify limits
%%====================================================================

json_number_precision_test() ->
    %% Test: Large integer (within JavaScript safe range)
    LargeInt = 9007199254740991, % 2^53 - 1 (max safe JavaScript integer)
    LargeIntJson = jsx:encode(LargeInt),
    ?assertEqual(LargeInt, jsx:decode(LargeIntJson)),

    %% Test: Negative large integer
    NegativeLargeInt = -9007199254740991,
    NegativeLargeIntJson = jsx:encode(NegativeLargeInt),
    ?assertEqual(NegativeLargeInt, jsx:decode(NegativeLargeIntJson)),

    %% Test: Decimal precision
    Decimal = 1.23456789,
    DecimalJson = jsx:encode(Decimal),
    DecodedDecimal = jsx:decode(DecimalJson),
    ?assert(abs(Decimal - DecodedDecimal) < 0.00000001).

%%====================================================================
%% Escaped Characters Tests
%% Test: JSON strings with escape sequences
%% Why: Escape sequences must be handled correctly per JSON spec
%%====================================================================

json_escaped_characters_test() ->
    %% Test: Backslash
    ?assertEqual(<<"\\">>, jsx:decode(jsx:encode(<<"\\"/utf8>>))),

    %% Test: Quote
    ?assertEqual(<<"\"">>, jsx:decode(jsx:encode(<<"\""/utf8>>))),

    %% Test: Newline
    ?assertEqual(<<"\n">>, jsx:decode(jsx:encode(<<"\n">>))),

    %% Test: Tab
    ?assertEqual(<<"\t">>, jsx:decode(jsx:encode(<<"\t">>))),

    %% Test: Carriage return
    ?assertEqual(<<"\r">>, jsx:decode(jsx:encode(<<"\r">>))),

    %% Test: Combined escape sequences
    ComplexString = <<"Line1\nLine2\tTabbed\"Quoted\"\\Backslash">>,
    ?assertEqual(ComplexString, jsx:decode(jsx:encode(ComplexString))).
