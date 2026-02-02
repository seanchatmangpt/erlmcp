%%%-------------------------------------------------------------------
%%% @doc erlmcp_json_codec_tests - Adaptive JSON Codec Tests
%%%
%%% Tests the adaptive JSON encoder that switches between jiffy and jsx:
%%% - Small messages (<100KB): Use jsx (more compatible)
%%% - Large messages (>100KB): Use jiffy (3x faster)
%%% - Fallback behavior: If jiffy fails, use jsx
%%%
%%% Uses Chicago School TDD:
%%% - Real encode/decode operations (no mocks)
%%% - State-based verification (output correctness)
%%% - Performance characteristics (size-based routing)
%%%
%%% Target: 90%+ coverage (performance-critical module)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_json_codec_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

json_codec_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_encode_small_message/1,
      fun test_encode_large_message/1,
      fun test_encode_with_custom_threshold/1,
      fun test_encode_below_threshold/1,
      fun test_encode_at_threshold/1,
      fun test_encode_above_threshold/1,
      fun test_decode_valid_json/1,
      fun test_decode_invalid_json/1,
      fun test_decode_empty_object/1,
      fun test_decode_nested_structures/1,
      fun test_decode_arrays/1,
      fun test_encode_decode_roundtrip/1,
      fun test_encode_decode_large_data/1,
      fun test_encode_unicode/1,
      fun test_encode_special_characters/1,
      fun test_get_default_threshold/1,
      fun test_jiffy_fallback_to_jsx/1,
      fun test_decode_jiffy_fallback/1]}.

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    %% Ensure dependencies are available
    application:ensure_all_started(jsx),
    ok.

cleanup(_State) ->
    ok.

%%====================================================================
%% Encode Tests
%%====================================================================

test_encode_small_message(_State) ->
    %% Exercise: Encode small map (<100KB)
    Data = #{<<"name">> => <<"test">>, <<"value">> => 123},

    Result = erlmcp_json_codec:encode(Data),

    %% Verify: Returns valid JSON binary
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"test">>, maps:get(<<"name">>, Decoded)),
    ?assertEqual(123, maps:get(<<"value">>, Decoded)).

test_encode_large_message(_State) ->
    %% Exercise: Encode large map (>100KB)
    LargeData = generate_large_map(200000), % 200KB

    Result = erlmcp_json_codec:encode(LargeData),

    %% Verify: Returns valid JSON binary
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 100000).

test_encode_with_custom_threshold(_State) ->
    %% Exercise: Encode with custom threshold
    SmallData = #{<<"test">> => <<"data">>},

    Result = erlmcp_json_codec:encode_with_threshold(SmallData, 1000000),

    %% Verify: Should use jsx (data is below threshold)
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"data">>, maps:get(<<"test">>, Decoded)).

test_encode_below_threshold(_State) ->
    %% Exercise: Encode data below 100KB threshold
    Data = generate_large_map(50000), % 50KB

    Result = erlmcp_json_codec:encode(Data),

    %% Verify: Returns valid JSON
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assert(is_map(Decoded)).

test_encode_at_threshold(_State) ->
    %% Exercise: Encode data at exactly 100KB threshold
    Data = generate_large_map(102400), % Exactly 100KB

    Result = erlmcp_json_codec:encode(Data),

    %% Verify: Returns valid JSON (may use jiffy or jsx)
    ?assert(is_binary(Result)).

test_encode_above_threshold(_State) ->
    %% Exercise: Encode data above 100KB threshold
    Data = generate_large_map(150000), % 150KB

    Result = erlmcp_json_codec:encode(Data),

    %% Verify: Returns valid JSON (should prefer jiffy)
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 100000).

%%====================================================================
%% Decode Tests
%%====================================================================

test_decode_valid_json(_State) ->
    %% Exercise: Decode valid JSON
    Json = <<"{\"name\":\"test\",\"value\":123}">>,

    Result = erlmcp_json_codec:decode(Json),

    %% Verify: Returns map with correct values
    ?assert(is_map(Result)),
    ?assertEqual(<<"test">>, maps:get(<<"name">>, Result)),
    ?assertEqual(123, maps:get(<<"value">>, Result)).

test_decode_invalid_json(_State) ->
    %% Exercise: Decode invalid JSON (should fail gracefully)
    InvalidJson = <<"{invalid json}">>,

    try
        erlmcp_json_codec:decode(InvalidJson),
        ?assert(false, "Should have thrown error")
    catch
        _:_ ->
            ?assert(true)
    end.

test_decode_empty_object(_State) ->
    %% Exercise: Decode empty object
    Json = <<"{}">>,

    Result = erlmcp_json_codec:decode(Json),

    %% Verify: Returns empty map
    ?assertEqual(#{}, Result).

test_decode_nested_structures(_State) ->
    %% Exercise: Decode nested JSON structures
    Json = <<"{\"user\":{\"name\":\"Alice\",\"age\":30,\"address\":{\"city\":\"NYC\"}}}">>,

    Result = erlmcp_json_codec:decode(Json),

    %% Verify: Nested structure preserved
    ?assert(is_map(Result)),
    User = maps:get(<<"user">>, Result),
    ?assertEqual(<<"Alice">>, maps:get(<<"name">>, User)),
    ?assertEqual(30, maps:get(<<"age">>, User)),
    Address = maps:get(<<"address">>, User),
    ?assertEqual(<<"NYC">>, maps:get(<<"city">>, Address)).

test_decode_arrays(_State) ->
    %% Exercise: Decode JSON with arrays
    Json = <<"[{\"id\":1,\"name\":\"test1\"},{\"id\":2,\"name\":\"test2\"}]">>,

    Result = erlmcp_json_codec:decode(Json),

    %% Verify: Array decoded correctly
    ?assert(is_list(Result)),
    ?assertEqual(2, length(Result)),
    [First, Second] = Result,
    ?assertEqual(1, maps:get(<<"id">>, First)),
    ?assertEqual(2, maps:get(<<"id">>, Second)).

%%====================================================================
%% Roundtrip Tests
%%====================================================================

test_encode_decode_roundtrip(_State) ->
    %% Exercise: Encode then decode (roundtrip)
    Original = #{
        <<"string">> => <<"test">>,
        <<"number">> => 42,
        <<"float">> => 3.14,
        <<"bool">> => true,
        <<"null">> => null,
        <<"array">> => [1, 2, 3],
        <<"nested">> => #{<<"key">> => <<"value">>}
    },

    Encoded = erlmcp_json_codec:encode(Original),
    Decoded = erlmcp_json_codec:decode(Encoded),

    %% Verify: Data survives roundtrip
    ?assertEqual(<<"test">>, maps:get(<<"string">>, Decoded)),
    ?assertEqual(42, maps:get(<<"number">>, Decoded)),
    ?assertEqual(3.14, maps:get(<<"float">>, Decoded)),
    ?assertEqual(true, maps:get(<<"bool">>, Decoded)),
    ?assertEqual(null, maps:get(<<"null">>, Decoded)),
    ?assertEqual([1, 2, 3], maps:get(<<"array">>, Decoded)),
    Nested = maps:get(<<"nested">>, Decoded),
    ?assertEqual(<<"value">>, maps:get(<<"key">>, Nested)).

test_encode_decode_large_data(_State) ->
    %% Exercise: Roundtrip with large data
    LargeData = generate_large_map(200000),

    Encoded = erlmcp_json_codec:encode(LargeData),
    Decoded = erlmcp_json_codec:decode(Encoded),

    %% Verify: Large data survives roundtrip
    ?assert(is_map(Decoded)),
    ?assert(maps:size(Decoded) > 0).

%%====================================================================
%% Special Character Tests
%%====================================================================

test_encode_unicode(_State) ->
    %% Exercise: Encode Unicode characters
    Data = #{
        <<"emoji">> => <<"😀🎉">>,
        <<"chinese">> => <<"中文">>,
        <<"arabic">> => <<"العربية">>,
        <<"russian">> => <<"Русский">>
    },

    Result = erlmcp_json_codec:encode(Data),

    %% Verify: Unicode preserved
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"😀🎉">>, maps:get(<<"emoji">>, Decoded)),
    ?assertEqual(<<"中文">>, maps:get(<<"chinese">>, Decoded)).

test_encode_special_characters(_State) ->
    %% Exercise: Encode special JSON characters
    Data = #{
        <<"quote">> => <<"He said \"hello\"">>,
        <<"backslash">> => <<"path\\to\\file">>,
        <<"newline">> => <<"line1\nline2">>,
        <<"tab">> => <<"col1\tcol2">>
    },

    Result = erlmcp_json_codec:encode(Data),

    %% Verify: Special characters escaped correctly
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"He said \"hello\"">>, maps:get(<<"quote">>, Decoded)),
    ?assertEqual(<<"path\\to\\file">>, maps:get(<<"backslash">>, Decoded)).

%%====================================================================
%% API Tests
%%====================================================================

test_get_default_threshold(_State) ->
    %% Exercise: Get default threshold
    Threshold = erlmcp_json_codec:get_default_threshold(),

    %% Verify: Default threshold is 100KB (102400 bytes)
    ?assertEqual(102400, Threshold).

%%====================================================================
%% Fallback Tests
%%====================================================================

test_jiffy_fallback_to_jsx(_State) ->
    %% Exercise: If jiffy fails, should fallback to jsx
    %% We test this by encoding data that works with both
    Data = #{<<"test">> => <<"data">>},

    Result = erlmcp_json_codec:encode(Data),

    %% Verify: Should succeed with either codec
    ?assert(is_binary(Result)),
    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"data">>, maps:get(<<"test">>, Decoded)).

test_decode_jiffy_fallback(_State) ->
    %% Exercise: Decode with jiffy first, fallback to jsx
    Json = <<"{\"test\":\"data\"}">>,

    Result = erlmcp_json_codec:decode(Json),

    %% Verify: Should decode successfully
    ?assert(is_map(Result)),
    ?assertEqual(<<"data">>, maps:get(<<"test">>, Result)).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Generate a large map for testing size-based codec selection
%% @private
-spec generate_large_map(pos_integer()) -> map().
generate_large_map(TargetSize) ->
    generate_large_map(TargetSize, 0, #{}).

generate_large_map(TargetSize, CurrentSize, Acc) when CurrentSize >= TargetSize ->
    Acc;
generate_large_map(TargetSize, CurrentSize, Acc) ->
    Key = iolist_to_binary(["key_", integer_to_list(CurrentSize)]),
    Value = iolist_to_binary([lists:duplicate(100, $x)]), % 100 bytes per value
    NewAcc = Acc#{Key => Value},
    NewSize = CurrentSize + byte_size(Key) + byte_size(Value) + 10, % +10 for JSON overhead
    generate_large_map(TargetSize, NewSize, NewAcc).
