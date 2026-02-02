# ZSTD Compression Tests - Comprehensive Test Suite

## Overview

This document describes the comprehensive test suite for `erlmcp_compression`, which tests the OTP 28 zstd compression implementation for erlmcp.

## Test Suite Structure

**File**: `apps/erlmcp_core/test/erlmcp_compression_SUITE.erl`

**Framework**: Common Test (CT)

**Philosophy**: Chicago School TDD
- Real zstd module (OTP 28)
- Observable behavior verification
- State-based assertions
- No mocks or fakes

## Test Groups

### 1. Basic Compression Tests

**Purpose**: Verify core compression/decompression functionality

**Tests**:
- `test_compress_decompress_roundtrip/1` - Verify data roundtrips correctly
- `test_compress_with_default_level/1` - Test default compression level (3)
- `test_compress_with_custom_level/1` - Test custom compression levels
- `test_compress_with_metadata/1` - Test compression with metadata output

**Verification**:
- Compressed data is smaller than original
- Decompressed data matches original exactly
- Metadata includes encoding, sizes, and ratio

### 2. Compression Level Tests

**Purpose**: Test all zstd compression levels (1-22)

**Tests**:
- `test_all_compression_levels/1` - Test every level 1-22
- `test_fast_compression_levels/1` - Test real-time levels (1-3) for speed
- `test_default_compression_level/1` - Verify default is level 3
- `test_archival_compression_levels/1` - Test maximum compression (16-22)

**Compression Levels**:
- **Levels 1-3**: Fast compression (real-time)
  - Level 1: Fastest
  - Level 2: Fast
  - Level 3: Default (good balance)

- **Levels 4-9**: Good compression (default use)
  - Level 4-6: Good speed/compression tradeoff
  - Level 7-9: Better compression

- **Levels 10-15**: Better compression (archival)
  - Slower but better ratios
  - Good for batch processing

- **Levels 16-22**: Best compression (offline)
  - Maximum compression
  - Very slow, use for archival

**Verification**:
- All levels produce valid compressed data
- Higher levels generally achieve better compression (lower ratio)
- Fast levels complete in < 10ms for 10KB data

### 3. Threshold Tests

**Purpose**: Test conditional compression based on data size

**Tests**:
- `test_compress_threshold_below/1` - Data below threshold is not compressed
- `test_compress_threshold_above/1` - Data above threshold is compressed
- `test_compress_threshold_exact/1` - Data at threshold is compressed
- `test_set_compression_threshold/1` - Test setting various thresholds
- `test_zero_threshold_always_compress/1` - Zero threshold means always compress

**Threshold Behavior**:
- Data size < threshold → Return uncompressed
- Data size >= threshold → Return compressed with metadata
- Threshold = 0 → Always compress (even tiny data)

**Default Threshold**: 1MB (1048576 bytes)

**Verification**:
- Correct behavior for below/at/above threshold
- Threshold can be changed dynamically
- Zero threshold forces compression

### 4. Decompression Tests

**Purpose**: Test decompression of valid and invalid data

**Tests**:
- `test_decompress_valid_data/1` - Decompress valid zstd data
- `test_decompress_invalid_data/1` - Handle invalid data gracefully
- `test_decompress_empty_data/1` - Handle empty data gracefully

**Error Handling**:
- Invalid zstd data → `{error, Reason}`
- Empty data → `{error, Reason}`
- Valid data → `{ok, Decompressed}`

**Verification**:
- Valid data decompresses correctly
- Invalid data returns error tuple
- No crashes on malformed input

### 5. Edge Cases

**Purpose**: Test boundary conditions and unusual inputs

**Tests**:
- `test_empty_binary/1` - Compress/decompress empty binary
- `test_small_binary/1` - Compress 100 bytes
- `test_large_binary/1` - Compress 1MB
- `test_very_large_binary/1` - Compress 10MB
- `test_already_compressed_data/1` - Compress already compressed data

**Edge Case Behavior**:
- Empty data: Compresses to small zstd frame
- Small data: May not compress well (ratio close to 1.0)
- Large data: Should achieve 20%+ compression
- Already compressed: Second compression has minimal effect

**Verification**:
- All edge cases handled without crashes
- Large data compresses efficiently (ratio < 0.8)
- Roundtrip succeeds for all cases

### 6. Statistics Tests

**Purpose**: Test compression statistics tracking

**Tests**:
- `test_get_initial_stats/1` - Verify initial stats are zero
- `test_stats_tracking/1` - Verify stats are updated
- `test_reset_stats/1` - Verify stats can be reset
- `test_stats_average_ratio/1` - Verify average ratio calculation

**Tracked Metrics**:
```erlang
#{
    total_compress_ops => N,            % Number of compressions
    total_decompress_ops => M,          % Number of decompressions
    total_original_bytes => Bytes,      % Original data size
    total_compressed_bytes => Bytes,    % Compressed data size
    avg_ratio => Float                  % Average compression ratio
}
```

**Verification**:
- Initial stats are all zeros
- Stats increment correctly
- Reset clears all stats
- Average ratio is accurate

### 7. Performance Tests

**Purpose**: Benchmark zstd vs zlib and measure throughput

**Tests**:
- `test_benchmark_zstd_vs_zlib/1` - Compare zstd and zlib
- `test_compression_speed/1` - Measure compression throughput
- `test_decompression_speed/1` - Measure decompression throughput

**Benchmark Sizes**: 1KB, 10KB, 100KB, 1MB

**Metrics**:
- Compression time (microseconds)
- Decompression time (microseconds)
- Compression ratio (compressed_size / original_size)
- Throughput (MB/s)

**Expected Performance**:
- 1MB compression: < 100ms
- 1MB decompression: < 50ms
- Compression ratio: 0.2-0.6 (60-80% reduction)
- Decompression is 2-10x faster than compression

**Verification**:
- Performance benchmarks complete successfully
- zstd generally faster than zlib
- zstd generally achieves better compression than zlib

### 8. gen_server Behavior Tests

**Purpose**: Test gen_server lifecycle and concurrent access

**Tests**:
- `test_gen_server_lifecycle/1` - Test start/stop/restart
- `test_concurrent_compression_requests/1` - Test concurrent compressions
- `test_concurrent_threshold_compression/1` - Test concurrent threshold ops

**Concurrent Scenarios**:
- 20 processes compressing simultaneously
- Multiple processes with different data sizes
- Mix of below/at/above threshold data

**Verification**:
- Server starts and stops cleanly
- Concurrent requests succeed without errors
- Statistics track concurrent operations correctly

## Running the Tests

### Run all compression tests:
```bash
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_compression_SUITE.erl
```

### Run specific test group:
```bash
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_compression_SUITE.erl --group=basic_compression
```

### Run with verbose output:
```bash
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_compression_SUITE.erl -v
```

### Generate coverage report:
```bash
rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_compression_SUITE.erl --cover
rebar3 cover
```

## Expected Results

### Test Count
- Total test cases: 30+
- Test groups: 8
- Coverage target: 90%+

### All Tests Should Pass
- 0 failures
- 0 errors
- All assertions succeed

### Performance Benchmarks
Typical results on modern hardware:

| Data Size | Compress Time | Decompress Time | Ratio |
|-----------|---------------|-----------------|-------|
| 1KB       | < 1ms         | < 0.5ms         | ~0.6  |
| 10KB      | < 5ms         | < 2ms           | ~0.4  |
| 100KB     | < 20ms        | < 10ms          | ~0.3  |
| 1MB       | < 100ms       | < 50ms          | ~0.25 |
| 10MB      | < 1s          | < 500ms         | ~0.2  |

### Comparison: zstd vs zlib

zstd advantages:
- **2-5x faster** compression
- **1.5-3x faster** decompression
- **10-30% better** compression ratio
- Better scalability for large data

## Chicago School TDD Principles

### Observable Behavior
- **NOT**: Testing internal zstd module calls
- **YES**: Testing compressed output size and decompressed result

### Real Collaborators
- **NOT**: Mocking zstd module
- **YES**: Using real OTP 28 zstd module

### State Verification
- **NOT**: Verifying internal function calls
- **YES**: Verifying compressed data is smaller and roundtrips correctly

### No Mocks
- **NOT**: Using meck or other mocking frameworks
- **YES**: Real gen_server, real compression, real data

## Test Coverage

The test suite covers:

**Function Coverage**:
- `compress/1` - Default compression
- `compress/2` - Custom level compression
- `decompress/1` - Decompression
- `compress_with_metadata/1` - Compression with metadata
- `compress_threshold/1` - Conditional compression
- `set_compression_threshold/1` - Threshold configuration
- `get_stats/0` - Statistics retrieval
- `reset_stats/0` - Statistics reset
- `benchmark_comparison/1` - Performance benchmarking

**Code Paths**:
- Success cases (normal compression)
- Error cases (invalid data, empty data)
- Edge cases (empty, small, large, very large)
- Boundary cases (threshold values)
- Concurrent access (multiple processes)

## Integration with erlmcp

### Usage in Transport Layer
Compression is used by transports to reduce message size:

```erlang
% In transport implementation
case erlmcp_compression:compress_threshold(Data) of
    {ok, Compressed, #{encoding := <<"zstd">>}} ->
        % Send compressed data with metadata
        send_compressed(Compressed);
    {ok, Uncompressed} ->
        % Send uncompressed (below threshold)
        send_uncompressed(Uncompressed)
end
```

### Usage in Session Storage
Compression reduces session storage size:

```erlang
% Compress session data before storage
{ok, Compressed, _} = erlmcp_compression:compress_with_metadata(SessionData),
session_backend:store(SessionId, Compressed)
```

## Troubleshooting

### Test Failures

**"Compressed data should be smaller than original"**
- Data may be already compressed or random
- Try larger data sizes (10KB+)
- Some data doesn't compress well (random data)

**"Decompressing invalid data should fail"**
- Test expects error, but got success
- Check test data is actually invalid

**Performance timeouts**
- System may be slow
- Increase timeout in test case
- Check system resources (CPU, memory)

### Coverage Below 90%

Run coverage report to find uncovered lines:
```bash
rebar3 cover --verbose
```

Check `_build/test/cover/` HTML report for details.

## Future Enhancements

Potential additions to test suite:
- Streaming compression tests
- Dictionary compression tests
- Multi-threaded compression tests (if supported)
- Memory usage profiling
- Compression level recommendation tests

## References

- **zstd module**: OTP 28 Erlang/OTP documentation
- **erlmcp_compression**: `apps/erlmcp_core/src/erlmcp_compression.erl`
- **Chicago School TDD**: `docs/otp-patterns.md`
- **Testing guidelines**: `CLAUDE.md` (Critical Rules)

## Summary

This comprehensive test suite ensures `erlmcp_compression`:
- ✅ Correctly compresses and decompresses data
- ✅ Handles all compression levels (1-22)
- ✅ Respects compression thresholds
- ✅ Tracks statistics accurately
- ✅ Performs well (fast compression, good ratios)
- ✅ Behaves correctly as a gen_server
- ✅ Handles concurrent requests safely
- ✅ Follows Chicago School TDD principles

**Quality Gate**: All 30+ tests must pass with 90%+ coverage before merging.
