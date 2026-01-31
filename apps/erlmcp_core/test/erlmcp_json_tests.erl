-module(erlmcp_json_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite for erlmcp_json - Native JSON Wrapper (OTP 28.3.1+)
%% Chicago School TDD: Test ONLY observable behavior through public API
%%
%% Purpose: Verify erlmcp_json wrapper around native json module
%% OTP Support: OTP 28.3.1+ only (no backward compatibility)
%%====================================================================

%%====================================================================
%% Basic Types Tests
%% Test: Encode/decode integers, floats, strings, atoms, booleans, null
%% Why: These are fundamental JSON types that must round-trip correctly
%%====================================================================

json_encode_basic_types_test() ->
    %% Test: Integer encoding
    IntJson = erlmcp_json:encode(42),
    ?assertEqual(<<"42">>, IntJson),
    ?assertEqual(42, erlmcp_json:decode(IntJson)),

    %% Test: Float encoding
    FloatJson = erlmcp_json:encode(3.14159),
    ?assert(is_binary(FloatJson)),
    ?assertEqual(3.14159, erlmcp_json:decode(FloatJson)),

    %% Test: String encoding
    StringJson = erlmcp_json:encode(<<"hello">>),
    ?assertEqual(<<"\"hello\"">>, StringJson),
    ?assertEqual(<<"hello">>, erlmcp_json:decode(StringJson)),

    %% Test: Boolean true
    TrueJson = erlmcp_json:encode(true),
    ?assertEqual(<<"true">>, TrueJson),
    ?assertEqual(true, erlmcp_json:decode(TrueJson)),

    %% Test: Boolean false
    FalseJson = erlmcp_json:encode(false),
    ?assertEqual(<<"false">>, FalseJson),
    ?assertEqual(false, erlmcp_json:decode(FalseJson)),

    %% Test: Null encoding
    NullJson = erlmcp_json:encode(null),
    ?assertEqual(<<"null">>, NullJson),
    ?assertEqual(null, erlmcp_json:decode(NullJson)).

%%====================================================================
%% Lists (JSON Arrays) Tests
%% Test: Arrays of various types
%% Why: JSON arrays must preserve order and type of all elements
%%====================================================================

json_encode_lists_test() ->
    %% Test: Empty array
    EmptyJson = erlmcp_json:encode([]),
    ?assertEqual(<<"[]">>, EmptyJson),
    ?assertEqual([], erlmcp_json:decode(EmptyJson)),

    %% Test: Array of integers
    IntArrayJson = erlmcp_json:encode([1, 2, 3, 4, 5]),
    ?assert(is_binary(IntArrayJson)),
    ?assertEqual([1, 2, 3, 4, 5], erlmcp_json:decode(IntArrayJson)),

    %% Test: Array of mixed types
    MixedJson = erlmcp_json:encode([1, <<"test">>, true, null, 3.14]),
    ?assert(is_binary(MixedJson)),
    Decoded = erlmcp_json:decode(MixedJson),
    ?assertEqual(5, length(Decoded)),

    %% Test: Nested arrays
    NestedJson = erlmcp_json:encode([[1, 2], [3, 4], [5, 6]]),
    ?assert(is_binary(NestedJson)),
    ?assertEqual([[1, 2], [3, 4], [5, 6]], erlmcp_json:decode(NestedJson)).

%%====================================================================
%% Maps (JSON Objects) Tests
%% Test: JSON objects with binary/atom keys
%% Why: Maps are core JSON structure, key handling varies by implementation
%%====================================================================

json_encode_maps_test() ->
    %% Test: Empty map
    EmptyJson = erlmcp_json:encode(#{}),
    ?assertEqual(<<"{}">> , EmptyJson),
    ?assertEqual(#{}, erlmcp_json:decode(EmptyJson)),

    %% Test: Map with binary keys
    BinaryKeyMap = #{<<"name">> => <<"John">>, <<"age">> => 30},
    BinaryKeyJson = erlmcp_json:encode(BinaryKeyMap),
    ?assert(is_binary(BinaryKeyJson)),
    DecodedBinary = erlmcp_json:decode(BinaryKeyJson),
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
    MixedJson = erlmcp_json:encode(MixedMap),
    ?assert(is_binary(MixedJson)),
    DecodedMixed = erlmcp_json:decode(MixedJson),
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
    NestedJson = erlmcp_json:encode(Nested),
    ?assert(is_binary(NestedJson)),
    DecodedNested = erlmcp_json:decode(NestedJson),
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
    ArrayJson = erlmcp_json:encode(ArrayOfObjects),
    ?assert(is_binary(ArrayJson)),
    DecodedArray = erlmcp_json:decode(ArrayJson),
    ?assertEqual(3, length(DecodedArray)),

    %% Test: Object with arrays
    ObjectWithArrays = #{
        <<"numbers">> => [1, 2, 3],
        <<"strings">> => [<<"a">>, <<"b">>, <<"c">>],
        <<"mixed">> => [1, <<"two">>, true]
    },
    ObjectJson = erlmcp_json:encode(ObjectWithArrays),
    ?assert(is_binary(ObjectJson)),
    DecodedObject = erlmcp_json:decode(ObjectJson),
    ?assertEqual(3, maps:size(DecodedObject)).

%%====================================================================
%% Unicode Tests
%% Test: UTF-8 characters, special escapes
%% Why: Unicode handling is critical for international applications
%%====================================================================

json_encode_unicode_test() ->
    %% Test: Basic Unicode characters
    UnicodeText = <<"Hello 世界 🌍"/utf8>>,
    UnicodeJson = erlmcp_json:encode(UnicodeText),
    ?assert(is_binary(UnicodeJson)),
    DecodedUnicode = erlmcp_json:decode(UnicodeJson),
    ?assertEqual(UnicodeText, DecodedUnicode),

    %% Test: Various Unicode ranges
    UnicodeMap = #{
        <<"chinese">> => <<"你好"/utf8>>,
        <<"russian">> => <<"Привет"/utf8>>,
        <<"arabic">> => <<"مرحبا"/utf8>>,
        <<"emoji">> => <<"🎉🎊✨"/utf8>>
    },
    UnicodeMapJson = erlmcp_json:encode(UnicodeMap),
    ?assert(is_binary(UnicodeMapJson)),
    DecodedUnicodeMap = erlmcp_json:decode(UnicodeMapJson),
    ?assertEqual(4, maps:size(DecodedUnicodeMap)),

    %% Test: Escaped characters
    EscapedText = <<"Line1\nLine2\tTabbed\"Quoted\"">>,
    EscapedJson = erlmcp_json:encode(EscapedText),
    ?assert(is_binary(EscapedJson)),
    DecodedEscaped = erlmcp_json:decode(EscapedJson),
    ?assertEqual(EscapedText, DecodedEscaped).

%%====================================================================
%% Decode Basic Types Tests
%% Test: Decoding all basic JSON types
%% Why: Decoding must correctly parse JSON primitives
%%====================================================================

json_decode_basic_types_test() ->
    %% Test: Decode integer
    ?assertEqual(42, erlmcp_json:decode(<<"42">>)),

    %% Test: Decode float
    ?assertEqual(3.14, erlmcp_json:decode(<<"3.14">>)),

    %% Test: Decode string
    ?assertEqual(<<"hello">>, erlmcp_json:decode(<<"\"hello\"">>)),

    %% Test: Decode boolean
    ?assertEqual(true, erlmcp_json:decode(<<"true">>)),
    ?assertEqual(false, erlmcp_json:decode(<<"false">>)),

    %% Test: Decode null
    ?assertEqual(null, erlmcp_json:decode(<<"null">>)),

    %% Test: Decode array
    ?assertEqual([1, 2, 3], erlmcp_json:decode(<<"[1,2,3]">>)),

    %% Test: Decode object
    Decoded = erlmcp_json:decode(<<"{\"key\":\"value\"}">>),
    ?assertEqual(#{<<"key">> => <<"value">>}, Decoded).

%%====================================================================
%% Decode Maps Option Tests
%% Test: Verify [return_maps] option works correctly
%% Why: Default jsx returns proplists, but we need maps for erlmcp
%%====================================================================

json_decode_maps_test() ->
    Json = <<"{\"name\":\"John\",\"age\":30}">>,

    %% Test: Decode to map (OTP 28 native json returns maps by default)
    DecodedMap = erlmcp_json:decode(Json),
    ?assert(is_map(DecodedMap)),
    ?assertEqual(<<"John">>, maps:get(<<"name">>, DecodedMap)),
    ?assertEqual(30, maps:get(<<"age">>, DecodedMap)),

    %% Test: Nested objects
    NestedJson = <<"{\"user\":{\"name\":\"Jane\",\"age\":25}}">>,
    DecodedNested = erlmcp_json:decode(NestedJson),
    User = maps:get(<<"user">>, DecodedNested),
    ?assert(is_map(User)),
    ?assertEqual(<<"Jane">>, maps:get(<<"name">>, User)),

    %% Test: Array of objects
    ArrayJson = <<"[{\"id\":1},{\"id\":2},{\"id\":3}]">>,
    DecodedArray = erlmcp_json:decode(ArrayJson),
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
    ?assertEqual(42, erlmcp_json:decode(erlmcp_json:encode(42))),
    ?assertEqual(3.14, erlmcp_json:decode(erlmcp_json:encode(3.14))),
    ?assertEqual(<<"test">>, erlmcp_json:decode(erlmcp_json:encode(<<"test">>))),
    ?assertEqual(true, erlmcp_json:decode(erlmcp_json:encode(true))),
    ?assertEqual(null, erlmcp_json:decode(erlmcp_json:encode(null))),

    %% Test: Array round-trip
    Array = [1, 2, 3, <<"test">>, true, null],
    ?assertEqual(Array, erlmcp_json:decode(erlmcp_json:encode(Array))),

    %% Test: Map round-trip
    Map = #{
        <<"string">> => <<"value">>,
        <<"number">> => 42,
        <<"bool">> => true,
        <<"null">> => null,
        <<"array">> => [1, 2, 3]
    },
    RoundTrip = erlmcp_json:decode(erlmcp_json:encode(Map)),
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
    ComplexRoundTrip = erlmcp_json:decode(erlmcp_json:encode(Complex)),
    ?assertEqual(Complex, ComplexRoundTrip).

%%====================================================================
%% Error Handling Tests
%% Test: Test error cases with real invalid JSON
%% Why: Must handle malformed JSON gracefully with clear errors
%%====================================================================

json_error_handling_test() ->
    %% Test: Invalid JSON syntax
    ?assertError(badarg, erlmcp_json:decode(<<"not valid json">>)),
    ?assertError(badarg, erlmcp_json:decode(<<"{invalid}">>)),
    ?assertError(badarg, erlmcp_json:decode(<<"{\"key\": invalid}">>)),

    %% Test: Incomplete JSON
    ?assertError(badarg, erlmcp_json:decode(<<"{\"key\":">>)),
    ?assertError(badarg, erlmcp_json:decode(<<"[1,2,">>)),

    %% Test: Invalid UTF-8 (create invalid UTF-8 sequence)
    InvalidUtf8 = <<"{\"key\":\"", 16#FF, 16#FE, "\"}" >>,
    ?assertError(badarg, erlmcp_json:decode(InvalidUtf8)),

    %% Test: Empty string
    ?assertError(badarg, erlmcp_json:decode(<<"">>)),

    %% Test: Only whitespace
    ?assertError(badarg, erlmcp_json:decode(<<"   ">>)).

%%====================================================================
%% Large Payload Tests
%% Test: 10KB+ JSON documents
%% Why: Performance and correctness with real-world large payloads
%%====================================================================

json_large_payload_test() ->
    %% Test: Large array (10000 integers = ~50KB)
    LargeArray = lists:seq(1, 10000),
    LargeArrayJson = erlmcp_json:encode(LargeArray),
    ?assert(byte_size(LargeArrayJson) > 10000),
    ?assertEqual(LargeArray, erlmcp_json:decode(LargeArrayJson)),

    %% Test: Large object (1000 key-value pairs)
    LargeMap = maps:from_list([
        {list_to_binary("key" ++ integer_to_list(N)), N}
        || N <- lists:seq(1, 1000)
    ]),
    LargeMapJson = erlmcp_json:encode(LargeMap),
    ?assert(byte_size(LargeMapJson) > 10000),
    DecodedLargeMap = erlmcp_json:decode(LargeMapJson),
    ?assertEqual(1000, maps:size(DecodedLargeMap)),

    %% Test: Deep nesting (100 levels)
    DeepNested = lists:foldl(
        fun(_, Acc) -> #{<<"level">> => Acc} end,
        #{<<"value">> => <<"deep">>},
        lists:seq(1, 100)
    ),
    DeepJson = erlmcp_json:encode(DeepNested),
    ?assert(byte_size(DeepJson) > 1000),
    DecodedDeep = erlmcp_json:decode(DeepJson),
    ?assert(is_map(DecodedDeep)),

    %% Test: Large strings
    LargeString = binary:copy(<<"X">>, 50000),
    LargeStringJson = erlmcp_json:encode(LargeString),
    ?assert(byte_size(LargeStringJson) > 50000),
    ?assertEqual(LargeString, erlmcp_json:decode(LargeStringJson)).


%%====================================================================
%% Empty Collections Tests
%% Test: Empty arrays and objects
%% Why: Edge case that must be handled correctly
%%====================================================================

json_empty_collections_test() ->
    %% Test: Empty array
    EmptyArray = [],
    EmptyArrayJson = erlmcp_json:encode(EmptyArray),
    ?assertEqual(<<"[]">>, EmptyArrayJson),
    ?assertEqual(EmptyArray, erlmcp_json:decode(EmptyArrayJson)),

    %% Test: Empty map
    EmptyMap = #{},
    EmptyMapJson = erlmcp_json:encode(EmptyMap),
    ?assertEqual(<<"{}">> , EmptyMapJson),
    ?assertEqual(EmptyMap, erlmcp_json:decode(EmptyMapJson)),

    %% Test: Map with empty array value
    MapWithEmptyArray = #{<<"items">> => []},
    MapWithEmptyArrayJson = erlmcp_json:encode(MapWithEmptyArray),
    DecodedMapWithEmptyArray = erlmcp_json:decode(MapWithEmptyArrayJson),
    ?assertEqual([], maps:get(<<"items">>, DecodedMapWithEmptyArray)),

    %% Test: Array with empty map
    ArrayWithEmptyMap = [#{}],
    ArrayWithEmptyMapJson = erlmcp_json:encode(ArrayWithEmptyMap),
    DecodedArrayWithEmptyMap = erlmcp_json:decode(ArrayWithEmptyMapJson),
    ?assertEqual([#{}], DecodedArrayWithEmptyMap).

%%====================================================================
%% Special Float Values Tests
%% Test: Edge case floats (very large, very small, negative)
%% Why: Float encoding can have precision issues
%%====================================================================

json_special_floats_test() ->
    %% Test: Very small float
    SmallFloat = 0.000001,
    SmallFloatJson = erlmcp_json:encode(SmallFloat),
    DecodedSmall = erlmcp_json:decode(SmallFloatJson),
    ?assert(abs(SmallFloat - DecodedSmall) < 0.0000001),

    %% Test: Very large float
    LargeFloat = 999999999.999999,
    LargeFloatJson = erlmcp_json:encode(LargeFloat),
    DecodedLarge = erlmcp_json:decode(LargeFloatJson),
    ?assert(abs(LargeFloat - DecodedLarge) < 1.0),

    %% Test: Negative float
    NegativeFloat = -3.14159,
    NegativeFloatJson = erlmcp_json:encode(NegativeFloat),
    ?assertEqual(NegativeFloat, erlmcp_json:decode(NegativeFloatJson)),

    %% Test: Zero
    ?assertEqual(0.0, erlmcp_json:decode(erlmcp_json:encode(0.0))),
    ?assertEqual(0, erlmcp_json:decode(erlmcp_json:encode(0))).

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
    BinaryJson = erlmcp_json:encode(BinaryKeyMap),
    DecodedBinary = erlmcp_json:decode(BinaryJson),
    ?assertEqual(BinaryKeyMap, DecodedBinary),

    %% Test: Nested maps with binary keys
    NestedBinaryMap = #{
        <<"outer">> => #{
            <<"inner">> => #{
                <<"deep">> => <<"value">>
            }
        }
    },
    NestedJson = erlmcp_json:encode(NestedBinaryMap),
    DecodedNested = erlmcp_json:decode(NestedJson),
    ?assertEqual(NestedBinaryMap, DecodedNested),

    %% Test: Keys with special characters
    SpecialKeyMap = #{
        <<"key-with-dash">> => 1,
        <<"key.with.dot">> => 2,
        <<"key_with_underscore">> => 3,
        <<"key with space">> => 4
    },
    SpecialJson = erlmcp_json:encode(SpecialKeyMap),
    DecodedSpecial = erlmcp_json:decode(SpecialJson),
    ?assertEqual(SpecialKeyMap, DecodedSpecial).

%%====================================================================
%% Whitespace Handling Tests
%% Test: JSON with various whitespace patterns
%% Why: Decoders must handle whitespace correctly per JSON spec
%%====================================================================

json_whitespace_test() ->
    %% Test: Compact JSON (no whitespace)
    CompactJson = <<"{\"key\":\"value\"}">>,
    ?assertEqual(#{<<"key">> => <<"value">>}, erlmcp_json:decode(CompactJson)),

    %% Test: JSON with spaces
    SpacedJson = <<"{ \"key\" : \"value\" }">>,
    ?assertEqual(#{<<"key">> => <<"value">>}, erlmcp_json:decode(SpacedJson)),

    %% Test: JSON with newlines and tabs
    FormattedJson = <<"{\n\t\"key\": \"value\"\n}">>,
    ?assertEqual(#{<<"key">> => <<"value">>}, erlmcp_json:decode(FormattedJson)),

    %% Test: Array with whitespace
    ArrayJson = <<"[ 1 , 2 , 3 ]">>,
    ?assertEqual([1, 2, 3], erlmcp_json:decode(ArrayJson)).

%%====================================================================
%% Number Precision Tests
%% Test: Large integers, precise decimals
%% Why: JSON numbers can lose precision, must verify limits
%%====================================================================

json_number_precision_test() ->
    %% Test: Large integer (within JavaScript safe range)
    LargeInt = 9007199254740991, % 2^53 - 1 (max safe JavaScript integer)
    LargeIntJson = erlmcp_json:encode(LargeInt),
    ?assertEqual(LargeInt, erlmcp_json:decode(LargeIntJson)),

    %% Test: Negative large integer
    NegativeLargeInt = -9007199254740991,
    NegativeLargeIntJson = erlmcp_json:encode(NegativeLargeInt),
    ?assertEqual(NegativeLargeInt, erlmcp_json:decode(NegativeLargeIntJson)),

    %% Test: Decimal precision
    Decimal = 1.23456789,
    DecimalJson = erlmcp_json:encode(Decimal),
    DecodedDecimal = erlmcp_json:decode(DecimalJson),
    ?assert(abs(Decimal - DecodedDecimal) < 0.00000001).

%%====================================================================
%% Escaped Characters Tests
%% Test: JSON strings with escape sequences
%% Why: Escape sequences must be handled correctly per JSON spec
%%====================================================================

json_escaped_characters_test() ->
    %% Test: Backslash
    ?assertEqual(<<"\\">>, erlmcp_json:decode(erlmcp_json:encode(<<"\\"/utf8>>))),

    %% Test: Quote
    ?assertEqual(<<"\"">>, erlmcp_json:decode(erlmcp_json:encode(<<"\""/utf8>>))),

    %% Test: Newline
    ?assertEqual(<<"\n">>, erlmcp_json:decode(erlmcp_json:encode(<<"\n">>))),

    %% Test: Tab
    ?assertEqual(<<"\t">>, erlmcp_json:decode(erlmcp_json:encode(<<"\t">>))),

    %% Test: Carriage return
    ?assertEqual(<<"\r">>, erlmcp_json:decode(erlmcp_json:encode(<<"\r">>))),

    %% Test: Combined escape sequences
    ComplexString = <<"Line1\nLine2\tTabbed\"Quoted\"\\Backslash">>,
    ?assertEqual(ComplexString, erlmcp_json:decode(erlmcp_json:encode(ComplexString))).
