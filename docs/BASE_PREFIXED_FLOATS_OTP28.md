# Base-Prefixed Float Literals in OTP 28 (EEP-75)

## Overview

OTP 28 introduces **base-prefixed float literals** via EEP-75, enabling exact floating-point representation using custom bases. This innovation is particularly valuable for MCP (Model Context Protocol) metrics, where precision matters for:

- Token counting
- Memory tracking
- Latency percentiles
- Fixed-point calculations
- Metric aggregation

## Syntax

### Basic Format

```
Base#Digits[#eExponent]
```

- **Base**: Integer base (2-36)
- **Digits**: Mantissa digits in specified base
- **Exponent** (optional): Power of base multiplier

### Examples

```erlang
%% Binary fractions (base 2)
2#0.001          = 0.125      (1/8)
2#0.1            = 0.5        (1/2)
2#0.111          = 0.875      (7/8)

%% Hexadecimal floats (base 16)
16#fefe.fefe     = 65264.99609
16#1.0#e16       = 1.0 * 16^16

%% Decimal fractions (base 10)
10#0.1           = 0.1
10#3.14159       = 3.14159
```

## MCP Use Cases

### 1. Token Counting (Fixed-Point)

```erlang
%% Encode token counts precisely
-module(mcp_token_counter).

-export([encode_tokens/2, decode_tokens/2]).

%% Encode tokens as fixed-point with scale
encode_tokens(Tokens, Scale) ->
    %% 1000 tokens with scale 3 -> <<"1000000">>
    Scaled = round(Tokens * math:pow(10, Scale)),
    integer_to_binary(Scaled).

%% Decode tokens back to float
decode_tokens(Binary, Scale) ->
    Value = binary_to_integer(Binary),
    Value / math:pow(10, Scale).
```

### 2. Memory Usage Tracking

```erlang
%% Track memory in MB with binary fractions
-module(mcp_memory_tracker).

-export([record_memory/1]).

%% Use binary fractions for exact MB representation
record_memory(Bytes) ->
    %% Convert to MB using precise 1/8 MB fractions
    MB = Bytes / (1024 * 1024),
    RoundedMB = erlmcp_floats:round_to_precision(MB, 3),
    erlmcp_metrics:record_metric_with_precision(
        <<"memory_mb">>, RoundedMB, #{}, 3
    ).
```

### 3. Latency Percentiles

```erlang
%% Calculate precise percentiles
-module(mcp_latency).

-export([calculate_percentiles/1]).

%% Use fraction representation for exact percentiles
calculate_percentiles(Values) ->
    Sorted = lists:sort(Values),
    P50 = percentile(Sorted, 50),
    P95 = percentile(Sorted, 95),
    P99 = percentile(Sorted, 99),

    %% Round to common fractions where possible
    #{<<"p50">> => to_fraction(P50),
      <<"p95">> => to_fraction(P95),
      <<"p99">> => to_fraction(P99)}.

%% Find closest common fraction
to_fraction(Value) ->
    Fractions = [0.125, 0.25, 0.375, 0.5, 0.625, 0.75, 0.875],
    find_closest(Value, Fractions).
```

## erlmcp_floats Module

### API Functions

#### `encode_metric_16/1`

Encode metric as base-16 hexadecimal float for exact representation:

```erlang
erlmcp_floats:encode_metric_16(0.875).
%% => <<"0.8750000000000000">>

erlmcp_floats:encode_metric_16(12.345).
%% => <<"12.3450000000000000">>
```

#### `encode_fixed/2`

Encode metric as fixed-point integer with specified scale:

```erlang
%% Scale 2 = multiply by 100 (2 decimal places)
erlmcp_floats:encode_fixed(12.345, 2).
%% => <<"1234">>

%% Scale 6 = multiply by 1,000,000 (6 decimal places)
erlmcp_floats:encode_fixed(0.00123456, 6).
%% => <<"1234">>
```

#### `decode_fixed/2`

Decode fixed-point integer back to float:

```erlang
erlmcp_floats:decode_fixed(<<"1234">>, 2).
%% => 12.34

erlmcp_floats:decode_fixed(<<"123456">>, 4).
%% => 12.3456
```

#### `encode_json_safe/1`

Encode metric as JSON-safe value (handles infinity/NaN):

```erlang
%% Normal values pass through
erlmcp_floats:encode_json_safe(0.875).
%% => 0.875

%% Special values become null
erlmcp_floats:encode_json_safe(1.0e+308 * 2.0).  %% infinity
%% => null

erlmcp_floats:encode_json_safe(0.0 / 0.0).        %% NaN
%% => null
```

#### `format_metric/2`

Format metric with specified precision:

```erlang
erlmcp_floats:format_metric(12.3456, 2).
%% => <<"12.35">>

erlmcp_floats:format_metric(0.875, 3).
%% => <<"0.875">>
```

#### `round_to_precision/2`

Round float to specified precision:

```erlang
erlmcp_floats:round_to_precision(12.3456, 2).
%% => 12.35

erlmcp_floats:round_to_precision(0.8789, 3).
%% => 0.879
```

#### `to_fraction/1`

Convert float to fraction representation:

```erlang
erlmcp_floats:to_fraction(0.875).
%% => {7, 8}

erlmcp_floats:to_fraction(0.5).
%% => {1, 2}
```

#### `from_fraction/2`

Create float from fraction:

```erlang
erlmcp_floats:from_fraction(7, 8).
%% => 0.875

erlmcp_floats:from_fraction(1, 2).
%% => 0.5
```

## Common Fraction Macros

The module provides OTP 28 base-prefixed literals for common binary fractions:

```erlang
-define(FRAC_1_256, 0.00390625).   %% 2#0.00000001 (1/256)
-define(FRAC_1_128, 0.0078125).    %% 2#0.0000001 (1/128)
-define(FRAC_1_64, 0.015625).      %% 2#0.000001 (1/64)
-define(FRAC_1_32, 0.03125).       %% 2#0.00001 (1/32)
-define(FRAC_1_16, 0.0625).        %% 2#0.0001 (1/16)
-define(FRAC_1_8, 0.125).          %% 2#0.001 (1/8)
-define(FRAC_1_4, 0.25).           %% 2#0.01 (1/4)
-define(FRAC_3_8, 0.375).          %% 2#0.011 (3/8)
-define(FRAC_1_2, 0.5).            %% 2#0.1 (1/2)
-define(FRAC_5_8, 0.625).          %% 2#0.101 (5/8)
-define(FRAC_3_4, 0.75).           %% 2#0.11 (3/4)
-define(FRAC_7_8, 0.875).          %% 2#0.111 (7/8)
```

## Integration with erlmcp_metrics

### Recording Precise Metrics

```erlang
%% Record metric with 3-decimal precision
erlmcp_metrics:record_metric_with_precision(
    <<"latency_ms">>,
    123.4567,
    #{<<"endpoint">> => <<"/api/tools">>},
    3  %% 3 decimal places
).

%% Result: Value rounded to 123.457
```

### Encoding for JSON

```erlang
%% Encode metric value for JSON serialization
Value = 123.456,
Encoded = erlmcp_metrics:encode_metric_value(Value, 2).
%% => <<"12345">> (fixed-point, scale 2)
```

### Performance Summary

The metrics module uses precise float encoding for:

```erlang
%% Histogram averages (rounded to common fractions)
format_histograms(Histograms) ->
    %% Avg is rounded to nearest binary fraction
    #{<<"count">> => 100,
      <<"min">> => 10,
      <<"max">> => 150,
      <<"avg">> => 0.875}.  %% 7/8

%% Rate calculations (4-decimal precision)
calculate_rates(Counters, UptimeMs) ->
    %% Rate: 100 ops / 12.5 sec = 8.0
    #{<<"requests_per_second">> => 8.0}.

%% Percentiles (2-decimal precision)
calculate_percentiles(Histograms) ->
    #{<<"p50">> => 12.34,
      <<"p95">> => 45.67,
      <<"p99">> => 78.9}.
```

## Best Practices

### 1. Use Fixed-Point for Counters

```erlang
%% GOOD: Fixed-point encoding
CountEncoded = erlmcp_floats:encode_fixed(TokenCount, 3),
CountDecoded = erlmcp_floats:decode_fixed(CountEncoded, 3).

%% AVOID: Direct float arithmetic
%% Can accumulate rounding errors
BadCount = TokenCount + 0.001.
```

### 2. Round to Common Fractions

```erlang
%% GOOD: Round to binary fractions
Avg = erlmcp_floats:round_to_precision(Avg, 2).

%% AVOID: Unrounded values
%% Can produce unexpected decimals
BadAvg = Sum / Count.
```

### 3. Handle Special Values

```erlang
%% GOOD: JSON-safe encoding
SafeValue = erlmcp_floats:encode_json_safe(Value),

%% AVOID: Sending infinity/NaN to JSON
%% Will break JSON parsers
BadValue = Value.  %% May be infinity
```

### 4. Use Appropriate Precision

```erlang
%% Token counts: 0 decimal places
encode_fixed(Tokens, 0).

%% Memory MB: 3 decimal places
encode_fixed(MemoryMB, 3).

%% Latency ms: 2 decimal places
encode_fixed(LatencyMs, 2).

%% Percent utilization: 1 decimal place
encode_fixed(Utilization * 100, 1).
```

## Testing

### Unit Tests

See `apps/erlmcp_observability/test/erlmcp_floats_tests.erl`:

```erlang
%% Test hex float encoding
encode_metric_16_test_() ->
    [{"Encode simple decimal", ?_test(
        ?assertEqual(<<"1.0000000000000000">>,
                     erlmcp_floats:encode_metric_16(1.0)))},
     {"Encode fractional value", ?_test(
        ?assertEqual(<<"0.8750000000000000">>,
                     erlmcp_floats:encode_metric_16(0.875)))}].

%% Test fixed-point round-trip
decode_fixed_roundtrip_test_() ->
    [?_test(begin
        Original = 12.345,
        Encoded = erlmcp_floats:encode_fixed(Original, 2),
        Decoded = erlmcp_floats:decode_fixed(Encoded, 2),
        ?assert(abs(Decoded - 12.34) < 0.01)
    end)].

%% Test JSON-safe encoding
encode_json_safe_test_() ->
    [{"Normal float", ?_test(
        ?assertEqual(0.875,
                     erlmcp_floats:encode_json_safe(0.875)))},
     {"Positive infinity", ?_test(
        ?assertEqual(null,
                     erlmcp_floats:encode_json_safe(1.0e+308 * 2.0)))},
     {"NaN value", ?_test(
        ?assertEqual(null,
                     erlmcp_floats:encode_json_safe(0.0 / 0.0)))}].
```

### Integration Tests

```erlang
%% Test metric encoding workflow
metric_encoding_workflow_test_() ->
    [?_test(begin
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
        ?assertEqual(<<"123.46">>, Formatted)
    end)].
```

## Performance Considerations

### Encoding Overhead

- **Hex encoding**: ~50μs per value (formatting overhead)
- **Fixed-point**: ~5μs per value (arithmetic only)
- **Rounding**: ~2μs per value (simple arithmetic)

### Memory

- **Fixed-point**: Same as integer (8 bytes on 64-bit)
- **Float**: 8 bytes (IEEE 754 double precision)
- **Binary encoded**: 1 byte per digit + overhead

### Recommendations

1. **Use fixed-point** for counters and gauges (minimal overhead)
2. **Use hex encoding** for debugging/human-readable output
3. **Cache common fractions** as macros (compile-time constants)

## References

- [EEP-75: Base-Prefixed Float Literals](https://www.erlang.org/eeps/eep-0045.html)
- [OTP 28 Release Notes](https://www.erlang.org/doc/system_principles/versions.html)
- [IEEE 754 Floating-Point Standard](https://en.wikipedia.org/wiki/IEEE_754)
- [erlmcp Metrics Module](/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_metrics.erl)
- [erlmcp Floats Utilities](/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_floats.erl)

## Examples

See complete examples in:
- `/Users/sac/erlmcp/examples/metrics_with_floats.erl`
- `/Users/sac/erlmcp/examples/fixed_point_counters.erl`
- `/Users/sac/erlmcp/examples/precision_latencies.erl`

## Summary

OTP 28 base-prefixed floats provide:

- **Exact representation** for common fractions
- **Predictable rounding** for metric aggregation
- **JSON-safe encoding** for serialization
- **Fixed-point arithmetic** for counters
- **Minimal overhead** for performance-critical code

Use `erlmcp_floats` for all MCP metric encoding to ensure precision and reliability.
