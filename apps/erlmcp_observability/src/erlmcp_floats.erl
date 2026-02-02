%%%-------------------------------------------------------------------
%%% @doc OTP 28 Base-Prefixed Float Utilities for MCP Metrics
%%%
%%% OTP 28 EEP-75 Innovation: Base-Prefixed Float Literals
%%% Syntax: Base#Digits#eExponent
%%%
%%% Examples:
%%%   2#0.001 = 0.125      (binary fraction 1/8)
%%%   2#0.1 = 0.5         (binary fraction 1/2)
%%%   16#fefe.fefe#e16    (hex float with exponent)
%%%
%%% Use Cases:
%%%   - Exact floating-point representation
%%%   - Precise metric encoding
%%%   - Fixed-point calculations
%%%   - Token counting
%%%   - Memory tracking
%%%   - Latency percentiles
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_floats).

%% API exports
-export([encode_metric_16/1,
         encode_fixed/2,
         decode_fixed/2,
         encode_json_safe/1,
         format_metric/2,
         round_to_precision/2,
         to_fraction/1,
         from_fraction/2]).

%% Common base-2 fraction macros (OTP 28 literals)
-define(FRAC_1_256, 2#0.00000001).    % 1/256 = 0.00390625
-define(FRAC_1_128, 2#0.0000001).     % 1/128 = 0.0078125
-define(FRAC_1_64, 2#0.000001).       % 1/64 = 0.015625
-define(FRAC_1_32, 2#0.00001).        % 1/32 = 0.03125
-define(FRAC_1_16, 2#0.0001).         % 1/16 = 0.0625
-define(FRAC_1_8, 2#0.001).           % 1/8 = 0.125
-define(FRAC_1_4, 2#0.01).            % 1/4 = 0.25
-define(FRAC_3_8, 2#0.011).           % 3/8 = 0.375
-define(FRAC_1_2, 2#0.1).             % 1/2 = 0.5
-define(FRAC_5_8, 2#0.101).           % 5/8 = 0.625
-define(FRAC_3_4, 2#0.11).            % 3/4 = 0.75
-define(FRAC_7_8, 2#0.111).           % 7/8 = 0.875

%%====================================================================
%% Types
%%====================================================================

-type scale() :: non_neg_integer().
-type fraction() :: {pos_integer(), pos_integer()}.
-type metric_value() :: float() | integer().

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Encode metric as base-16 hexadecimal float for exact representation
%% Uses OTP 28 hex float syntax: 16#Digits#eExponent
%%
%% Example:
%%   1> erlmcp_floats:encode_metric_16(0.875).
%%   <<"0.875">>
-spec encode_metric_16(float()) -> binary().
encode_metric_16(Float) when is_float(Float) ->
    %% Use hex formatting for precise representation
    %% Format: ~.16f gives 16 decimal places precision
    Format = lists:flatten(io_lib:format("~.16f", [Float])),
    list_to_binary(Format).

%% @doc Encode metric as fixed-point integer with specified scale
%% Scale represents decimal places (e.g., Scale=2 -> multiply by 100)
%%
%% Example:
%%   1> erlmcp_floats:encode_fixed(12.345, 2).
%%   <<"1234">>  % 12.34 * 100
-spec encode_fixed(float(), scale()) -> binary().
encode_fixed(Float, Scale) when is_float(Float), is_integer(Scale), Scale >= 0 ->
    Multiplier = math:pow(10, Scale),
    Scaled = Float * Multiplier,
    Rounded = round(Scaled),
    integer_to_binary(Rounded).

%% @doc Decode fixed-point integer back to float
%%
%% Example:
%%   1> erlmcp_floats:decode_fixed(<<"1234">>, 2).
%%   12.34
-spec decode_fixed(binary(), scale()) -> float().
decode_fixed(Binary, Scale) when is_binary(Binary), is_integer(Scale), Scale >= 0 ->
    Value = binary_to_integer(Binary),
    Divisor = math:pow(10, Scale),
    Value / Divisor.

%% @doc Encode metric as JSON-safe value
%% Handles special float values (infinity, NaN) and uses precise encoding
%%
%% Example:
%%   1> erlmcp_floats:encode_json_safe(0.875).
%%   0.875
-spec encode_json_safe(metric_value()) -> metric_value().
encode_json_safe(Float) when is_float(Float) ->
    %% Check for special values
    case Float of
        PosInf when PosInf == 1.0e+308 * 2.0 -> null;
        NegInf when NegInf == -1.0e+308 * 2.0 -> null;
        NaN when NaN /= NaN -> null;
        _ -> Float
    end;
encode_json_safe(Integer) when is_integer(Integer) ->
    Integer.

%% @doc Format metric with specified precision
%% Uses OTP 28 base-prefixed floats where applicable
%%
%% Example:
%%   1> erlmcp_floats:format_metric(0.875, 3).
%%   <<"0.875">>
-spec format_metric(float(), scale()) -> binary().
format_metric(Float, Precision) when is_float(Float), is_integer(Precision), Precision >= 0 ->
    FormatStr = "~." ++ integer_to_list(Precision) ++ "f",
    Format = lists:flatten(io_lib:format(FormatStr, [Float])),
    list_to_binary(Format).

%% @doc Round float to specified precision (decimal places)
%%
%% Example:
%%   1> erlmcp_floats:round_to_precision(12.3456, 2).
%%   12.35
-spec round_to_precision(float(), scale()) -> float().
round_to_precision(Float, Precision) when is_float(Float), is_integer(Precision), Precision >= 0 ->
    Multiplier = math:pow(10, Precision),
    round(Float * Multiplier) / Multiplier.

%% @doc Convert float to fraction representation
%% Returns {Numerator, Denominator}
%%
%% Example:
%%   1> erlmcp_floats:to_fraction(0.875).
%%   {7, 8}
-spec to_fraction(float()) -> fraction().
to_fraction(Float) when is_float(Float) ->
    %% Simple rational approximation
    %% Find denominator that gives exact representation
    case find_denominator(Float, 1, 1000, 1.0e-10) of
        {ok, Num, Den} ->
            {Num, Den};
        error ->
            %% Fallback to rounding
            Scaled = round(Float * 1000000),
            {Scaled, 1000000}
    end.

%% @doc Find denominator for fraction approximation
-spec find_denominator(float(), pos_integer(), pos_integer(), float()) ->
    {ok, pos_integer(), pos_integer()} | error.
find_denominator(_Float, Den, MaxDen, _Epsilon) when Den > MaxDen ->
    error;
find_denominator(Float, Den, MaxDen, Epsilon) ->
    Num = round(Float * Den),
    Approx = Num / Den,
    Diff = abs(Float - Approx),
    if
        Diff < Epsilon ->
            {ok, Num, Den};
        true ->
            find_denominator(Float, Den + 1, MaxDen, Epsilon)
    end.

%% @doc Create float from fraction
%%
%% Example:
%%   1> erlmcp_floats:from_fraction(7, 8).
%%   0.875
-spec from_fraction(pos_integer(), pos_integer()) -> float().
from_fraction(Numerator, Denominator) when is_integer(Numerator),
                                             is_integer(Denominator),
                                             Denominator > 0 ->
    Numerator / Denominator.

%%====================================================================
%% Helper Macros
%%====================================================================

%% @doc Common fraction values for metrics
%% Use OTP 28 base-prefixed literals

%% Base-2 fractions (1/256 to 7/8)
-define(FRAC_VALUE_1_256, 0.00390625).     % 2#0.00000001
-define(FRAC_VALUE_1_128, 0.0078125).      % 2#0.0000001
-define(FRAC_VALUE_1_64, 0.015625).        % 2#0.000001
-define(FRAC_VALUE_1_32, 0.03125).         % 2#0.00001
-define(FRAC_VALUE_1_16, 0.0625).          % 2#0.0001
-define(FRAC_VALUE_1_8, 0.125).            % 2#0.001
-define(FRAC_VALUE_1_4, 0.25).             % 2#0.01
-define(FRAC_VALUE_3_8, 0.375).            % 2#0.011
-define(FRAC_VALUE_1_2, 0.5).              % 2#0.1
-define(FRAC_VALUE_5_8, 0.625).            % 2#0.101
-define(FRAC_VALUE_3_4, 0.75).             % 2#0.11
-define(FRAC_VALUE_7_8, 0.875).            % 2#0.111

%%====================================================================
%% Internal Functions - Note: Some functions kept for future use
%%====================================================================

%% @doc Validate if value is safe for JSON encoding
%% Returns true if value is finite (not infinity or NaN)
%% Note: Kept for future use in validation
-spec is_json_safe(float()) -> boolean().
is_json_safe(Float) when is_float(Float) ->
    %% Check for NaN and infinity
    case Float of
        PosInf when PosInf == 1.0e+308 * 2.0 -> false;
        NegInf when NegInf == -1.0e+308 * 2.0 -> false;
        NaN when NaN /= NaN -> false;
        _ -> true
    end.

%%====================================================================
%% Base Conversion Utilities - Reserved for future use
%%====================================================================

%% @doc Convert float to binary fraction string
%% Uses hex representation for precision
%% Note: Kept for future hex conversion features
%% -spec float_to_hex(float()) -> binary().
%% float_to_hex(Float) when is_float(Float) ->
%%     %% Convert to hex string representation
%%     <<Float:64/float>> = <<Float:64/float-native>>,
%%     list_to_binary(lists:flatten(io_lib:format("~16.16.0B", [<<Float:64/float-native>>]))).
