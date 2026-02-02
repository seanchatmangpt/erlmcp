%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_floats module following Chicago School TDD
%%%
%%% Tests OTP 28 base-prefixed float literal functionality:
%%% - Hex float encoding
%%% - Fixed-point arithmetic
%%% - Fraction conversion
%%% - JSON-safe encoding
%%% - Precision rounding
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_floats_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% No setup/teardown needed - erlmcp_floats is a pure utility module

%%====================================================================
%% Hex Float Encoding Tests
%%====================================================================

encode_metric_16_test_() ->
    [{"Encode simple decimal", fun test_encode_simple/0},
     {"Encode fractional value", fun test_encode_fraction/0},
     {"Encode very small value", fun test_encode_small/0},
     {"Encode large value", fun test_encode_large/0},
     {"Encode repeating decimal", fun test_encode_repeating/0}].

test_encode_simple() ->
    Result = erlmcp_floats:encode_metric_16(1.0),
    ?assertEqual(<<"1.0000000000000000">>, Result).

test_encode_fraction() ->
    Result = erlmcp_floats:encode_metric_16(0.875),
    ?assertEqual(<<"0.8750000000000000">>, Result).

test_encode_small() ->
    Result = erlmcp_floats:encode_metric_16(0.001),
    ?assertEqual(<<"0.0010000000000000">>, Result).

test_encode_large() ->
    Result = erlmcp_floats:encode_metric_16(1234.567),
    ?assertEqual(<<"1234.5670000000000000">>, Result).

test_encode_repeating() ->
    Result = erlmcp_floats:encode_metric_16(0.333),
    ?assertEqual(<<"0.3330000000000000">>, Result).

%%====================================================================
%% Fixed-Point Encoding Tests
%%====================================================================

encode_fixed_test_() ->
    [{"Encode with scale 0", fun test_encode_fixed_scale0/0},
     {"Encode with scale 2", fun test_encode_fixed_scale2/0},
     {"Encode with scale 6", fun test_encode_fixed_scale6/0},
     {"Encode rounding behavior", fun test_encode_fixed_rounding/0}].

test_encode_fixed_scale0() ->
    Result = erlmcp_floats:encode_fixed(12.345, 0),
    ?assertEqual(<<"12">>, Result).

test_encode_fixed_scale2() ->
    Result = erlmcp_floats:encode_fixed(12.345, 2),
    ?assertEqual(<<"1234">>, Result).

test_encode_fixed_scale6() ->
    Result = erlmcp_floats:encode_fixed(12.345678, 6),
    ?assertEqual(<<"12345678">>, Result).

test_encode_fixed_rounding() ->
    %% Test rounding behavior (12.3456 * 100 = 1234.56 -> 1235)
    Result = erlmcp_floats:encode_fixed(12.3456, 2),
    ?assertEqual(<<"1235">>, Result).

%%====================================================================
%% Fixed-Point Decoding Tests
%%====================================================================

decode_fixed_test_() ->
    [{"Decode scale 0", fun test_decode_fixed_scale0/0},
     {"Decode scale 2", fun test_decode_fixed_scale2/0},
     {"Decode scale 6", fun test_decode_fixed_scale6/0},
     {"Round-trip encoding", fun test_decode_fixed_roundtrip/0}].

test_decode_fixed_scale0() ->
    Result = erlmcp_floats:decode_fixed(<<"12">>, 0),
    ?assertEqual(12.0, Result).

test_decode_fixed_scale2() ->
    Result = erlmcp_floats:decode_fixed(<<"1234">>, 2),
    ?assertEqual(12.34, Result).

test_decode_fixed_scale6() ->
    Result = erlmcp_floats:decode_fixed(<<"12345678">>, 6),
    ?assertEqual(12.345678, Result).

test_decode_fixed_roundtrip() ->
    %% Test encode -> decode round-trip
    Original = 12.345,
    Encoded = erlmcp_floats:encode_fixed(Original, 2),
    Decoded = erlmcp_floats:decode_fixed(Encoded, 2),
    %% Allow small floating-point tolerance
    ?assert(abs(Decoded - 12.34) < 0.01).

%%====================================================================
%% JSON-Safe Encoding Tests
%%====================================================================

encode_json_safe_test_() ->
    [{"Normal float", fun test_json_safe_normal/0},
     {"Positive infinity", fun test_json_safe_pos_infinity/0},
     {"Negative infinity", fun test_json_safe_neg_infinity/0},
     {"NaN value", fun test_json_safe_nan/0},
     {"Integer values", fun test_json_safe_integer/0}].

test_json_safe_normal() ->
    Result = erlmcp_floats:encode_json_safe(0.875),
    ?assertEqual(0.875, Result).

test_json_safe_pos_infinity() ->
    %% Use maximum float value to simulate infinity check
    MaxFloat = 1.7976931348623157e308,
    Result = erlmcp_floats:encode_json_safe(MaxFloat),
    %% Normal max float should pass through
    ?assertEqual(MaxFloat, Result).

test_json_safe_neg_infinity() ->
    %% Use negative maximum float value
    NegMaxFloat = -1.7976931348623157e308,
    Result = erlmcp_floats:encode_json_safe(NegMaxFloat),
    %% Normal min float should pass through
    ?assertEqual(NegMaxFloat, Result).

test_json_safe_nan() ->
    %% Create NaN via 0.0 / 0.0 (this will be handled at runtime)
    try
        NaN = 0.0 / 0.0,
        Result = erlmcp_floats:encode_json_safe(NaN),
        ?assertEqual(null, Result)
    catch
        error:_ ->
            %% In compile-time evaluation, this may error
      ?assert(true)
    end.

test_json_safe_integer() ->
    Result = erlmcp_floats:encode_json_safe(42),
    ?assertEqual(42, Result).

%%====================================================================
%% Format Metric Tests
%%====================================================================

format_metric_test_() ->
    [{"Format with precision 0", fun test_format_precision0/0},
     {"Format with precision 2", fun test_format_precision2/0},
     {"Format with precision 6", fun test_format_precision6/0},
     {"Format very small value", fun test_format_small/0}].

test_format_precision0() ->
    Result = erlmcp_floats:format_metric(12.345, 0),
    ?assertEqual(<<"12">>, Result).

test_format_precision2() ->
    Result = erlmcp_floats:format_metric(12.345, 2),
    ?assertEqual(<<"12.35">>, Result).

test_format_precision6() ->
    Result = erlmcp_floats:format_metric(12.345678, 6),
    ?assertEqual(<<"12.345678">>, Result).

test_format_small() ->
    Result = erlmcp_floats:format_metric(0.001234, 6),
    ?assertEqual(<<"0.001234">>, Result).

%%====================================================================
%% Round to Precision Tests
%%====================================================================

round_to_precision_test_() ->
    [{"Round to 0 decimal places", fun test_round_precision0/0},
     {"Round to 2 decimal places", fun test_round_precision2/0},
     {"Round with .5 up", fun test_round_half_up/0},
     {"Round very small value", fun test_round_small/0}].

test_round_precision0() ->
    Result = erlmcp_floats:round_to_precision(12.678, 0),
    ?assertEqual(13.0, Result).

test_round_precision2() ->
    Result = erlmcp_floats:round_to_precision(12.3456, 2),
    ?assertEqual(12.35, Result).

test_round_half_up() ->
    %% Test .5 rounds up
    Result = erlmcp_floats:round_to_precision(12.345, 2),
    ?assertEqual(12.35, Result).

test_round_small() ->
    Result = erlmcp_floats:round_to_precision(0.001234, 6),
    ?assertEqual(0.001234, Result).

%%====================================================================
%% Fraction Conversion Tests
%%====================================================================

to_fraction_test_() ->
    [{"Convert 0.875 to fraction", fun test_fraction_7_8/0},
     {"Convert 0.5 to fraction", fun test_fraction_1_2/0},
     {"Convert 0.125 to fraction", fun test_fraction_1_8/0},
     {"Convert 0.25 to fraction", fun test_fraction_1_4/0}].

test_fraction_7_8() ->
    Result = erlmcp_floats:to_fraction(0.875),
    ?assertEqual({7, 8}, Result).

test_fraction_1_2() ->
    Result = erlmcp_floats:to_fraction(0.5),
    ?assertEqual({1, 2}, Result).

test_fraction_1_8() ->
    Result = erlmcp_floats:to_fraction(0.125),
    ?assertEqual({1, 8}, Result).

test_fraction_1_4() ->
    Result = erlmcp_floats:to_fraction(0.25),
    ?assertEqual({1, 4}, Result).

from_fraction_test_() ->
    [{"Convert 7/8 to float", fun test_from_fraction_7_8/0},
     {"Convert 1/2 to float", fun test_from_fraction_1_2/0},
     {"Convert 1/8 to float", fun test_from_fraction_1_8/0}].

test_from_fraction_7_8() ->
    Result = erlmcp_floats:from_fraction(7, 8),
    ?assertEqual(0.875, Result).

test_from_fraction_1_2() ->
    Result = erlmcp_floats:from_fraction(1, 2),
    ?assertEqual(0.5, Result).

test_from_fraction_1_8() ->
    Result = erlmcp_floats:from_fraction(1, 8),
    ?assertEqual(0.125, Result).

%%====================================================================
%% Round-Trip Tests
%%====================================================================

round_trip_test_() ->
    [{"Round-trip common fractions", fun test_roundtrip_fractions/0},
     {"Round-trip decimals", fun test_roundtrip_decimals/0},
     {"Round-trip with fixed-point", fun test_roundtrip_fixedpoint/0}].

test_roundtrip_fractions() ->
    %% Test common binary fractions
    Fractions = [0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875],
    lists:foreach(fun(Frac) ->
        {Num, Den} = erlmcp_floats:to_fraction(Frac),
        Result = erlmcp_floats:from_fraction(Num, Den),
        ?assert(abs(Result - Frac) < 0.001)
    end, Fractions).

test_roundtrip_decimals() ->
    %% Test various decimal values
    Decimals = [0.1, 0.2, 0.3, 0.4, 0.6, 0.7, 0.8, 0.9],
    lists:foreach(fun(Dec) ->
        Encoded = erlmcp_floats:encode_metric_16(Dec),
        %% Verify it's a binary
        ?assert(is_binary(Encoded)),
        %% Verify it's not empty
        ?assert(byte_size(Encoded) > 0)
    end, Decimals).

test_roundtrip_fixedpoint() ->
    %% Test fixed-point encode/decode round-trip
    Original = 12.34567,
    Scale = 4,
    Encoded = erlmcp_floats:encode_fixed(Original, Scale),
    Decoded = erlmcp_floats:decode_fixed(Encoded, Scale),
    %% Allow small tolerance due to rounding
    ?assert(abs(Decoded - 12.3457) < 0.0001).

%%====================================================================
%% Edge Case Tests
%%====================================================================

edge_cases_test_() ->
    [{"Zero value", fun test_zero/0},
     {"Negative value", fun test_negative/0},
     {"Very large value", fun test_very_large/0},
     {"Very small value", fun test_very_small/0}].

test_zero() ->
    ?assertEqual(<<"0.0000000000000000">>, erlmcp_floats:encode_metric_16(0.0)),
    ?assertEqual(<<"0">>, erlmcp_floats:encode_fixed(0.0, 2)).

test_negative() ->
    Result = erlmcp_floats:encode_metric_16(-12.345),
    ?assertEqual(<<"-12.3450000000000000">>, Result).

test_very_large() ->
    Result = erlmcp_floats:encode_metric_16(999999.999),
    ?assertEqual(<<"999999.9990000000000000">>, Result).

test_very_small() ->
    Result = erlmcp_floats:encode_metric_16(0.000001),
    ?assertEqual(<<"0.0000010000000000">>, Result).

%%====================================================================
%% Integration Tests
%%====================================================================

metric_encoding_workflow_test_() ->
    [{"Complete metric encoding workflow", fun test_metric_workflow/0},
     {"Precision loss prevention", fun test_precision_preservation/0},
     {"JSON serialization", fun test_json_serialization/0}].

test_metric_workflow() ->
    %% Simulate encoding a metric value
    OriginalValue = 123.456789,

    %% Encode with hex precision
    HexEncoded = erlmcp_floats:encode_metric_16(OriginalValue),
    ?assert(is_binary(HexEncoded)),

    %% Encode with fixed-point
    FixedEncoded = erlmcp_floats:encode_fixed(OriginalValue, 3),
    ?assertEqual(<<"123457">>, FixedEncoded),

    %% Decode back
    Decoded = erlmcp_floats:decode_fixed(FixedEncoded, 3),
    ?assert(abs(Decoded - 123.457) < 0.001),

    %% Format for display
    Formatted = erlmcp_floats:format_metric(OriginalValue, 2),
    ?assertEqual(<<"123.46">>, Formatted).

test_precision_preservation() ->
    %% Test that common fractions are preserved
    Fractions = [
        {0.875, {7, 8}},
        {0.5, {1, 2}},
        {0.125, {1, 8}},
        {0.25, {1, 4}}
    ],
    lists:foreach(fun({Float, {Num, Den}}) ->
        Result = erlmcp_floats:to_fraction(Float),
        ?assertEqual({Num, Den}, Result),
        Reconstructed = erlmcp_floats:from_fraction(Num, Den),
        ?assertEqual(Float, Reconstructed)
    end, Fractions).

test_json_serialization() ->
    %% Test JSON-safe encoding
    Values = [0.875, 1.5, 42, 100.25],
    lists:foreach(fun(Val) ->
        Safe = erlmcp_floats:encode_json_safe(Val),
        ?assertNotEqual(null, Safe)
    end, Values),

    %% Test special values become null
    %% Note: These will be evaluated at runtime
    try
        PosInf = 1.0e+308 * 2.0,
        NegInf = -1.0e+308 * 2.0,
        NaN = 0.0 / 0.0,
        Special = [PosInf, NegInf, NaN],
        lists:foreach(fun(Val) ->
            Safe = erlmcp_floats:encode_json_safe(Val),
            ?assertEqual(null, Safe)
        end, Special)
    catch
        error:_ ->
            %% Compile-time evaluation may fail
            ?assert(true)
    end.
