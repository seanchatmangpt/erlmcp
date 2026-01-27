# Gap #37: Icon Metadata Caching with TTL Enforcement

**MCP 2025-11-25 Compliance Feature**

## Overview

Icon metadata caching system with configurable time-to-live (TTL) enforcement. Implements automatic cache expiration, invalidation tracking, and statistics collection.

## Specification Requirements

From MCP 2025-11-25 specification:

1. **Cache icon metadata** (size, MIME type, etc.)
2. **Enforce cache TTL** (time-to-live)
3. **Default TTL: 1 hour** (3600000 milliseconds)
4. **Configurable TTL** via sys.config
5. **Invalidate cache** when icon changes

## Implementation

### Module: `erlmcp_icon_cache`

Located at: `/Users/sac/erlmcp/src/erlmcp_icon_cache.erl`

#### Core Functions

##### Cache Operations

```erlang
%% Cache icon with default TTL
cache_icon(Uri, Metadata, TtlMs) -> ok

%% Cache icon with specific timestamp
cache_icon(Uri, Metadata, TtlMs, CachedAt) -> ok

%% Retrieve cached icon
get_cached_icon(Uri) -> {ok, Metadata} | {expired, Metadata} | not_found

%% Invalidate specific icon
invalidate_icon(Uri) -> ok

%% Clear entire cache
invalidate_all() -> ok
```

#### Configuration Functions

```erlang
%% Get current cache statistics
get_cache_stats() -> #{
    hits => integer(),
    misses => integer(),
    expirations => integer(),
    invalidations => integer(),
    cache_size => integer(),
    current_ttl => integer()
}

%% Update TTL for future entries
set_ttl(TtlMs) -> ok
```

### Configuration

#### sys.config

```erlang
{erlmcp, [
    %% Icon Cache Configuration (Gap #37)
    {icon_cache_ttl_ms, 3600000}  %% 1 hour default TTL in milliseconds
]},

{icons, [
    {enabled, true},
    {cache_enabled, true},
    {cache_ttl_ms, 3600000}       %% 1 hour (3600000 milliseconds)
]}
```

#### Accepted Values

- **Default TTL**: 3600000 ms (1 hour)
- **Minimum TTL**: 1000 ms (1 second)
- **Cleanup Interval**: 300000 ms (5 minutes - automatic)

### Cache Behavior

#### TTL Enforcement

- Each cache entry has an expiration timestamp: `cached_at + ttl_ms`
- Expired entries are detected on access and removed
- Periodic cleanup runs every 5 minutes to remove expired entries
- Returns `{expired, Metadata}` if entry is expired (allows client to see original metadata)

#### Cache Statistics

The cache tracks:

1. **Cache Hits**: Successful retrievals of valid cached entries
2. **Cache Misses**: Requests for non-existent entries
3. **Cache Expirations**: Expired entries detected on access
4. **Cache Invalidations**: Entries manually invalidated
5. **Cache Size**: Current number of cached entries

#### Cache Cleanup

Automatic cleanup runs periodically (every 5 minutes):
- Scans cache and removes expired entries
- Logs number of entries removed
- Non-blocking operation

### Integration with erlmcp_sup

The icon cache is started as a permanent worker in the supervision tree:

```erlang
#{
    id => erlmcp_icon_cache,
    start => {erlmcp_icon_cache, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [erlmcp_icon_cache]
}
```

## Usage Examples

### Basic Caching

```erlang
%% Cache an icon for 1 hour
Uri = <<"https://example.com/icon.png">>,
Metadata = #{
    <<"size">> => 2048,
    <<"mime_type">> => <<"image/png">>,
    <<"width">> => 128,
    <<"height">> => 128
},

erlmcp_icon_cache:cache_icon(Uri, Metadata, 3600000).
```

### Retrieving from Cache

```erlang
%% Attempt to retrieve cached icon
case erlmcp_icon_cache:get_cached_icon(Uri) of
    {ok, Metadata} ->
        %% Valid cache hit
        io:format("Cache hit: ~p~n", [Metadata]);
    {expired, OriginalMetadata} ->
        %% Cache expired, but we have the original metadata
        io:format("Expired cache: ~p~n", [OriginalMetadata]),
        %% Re-cache with new timestamp
        erlmcp_icon_cache:cache_icon(Uri, OriginalMetadata, 3600000);
    not_found ->
        %% Not in cache, fetch from source
        io:format("Cache miss~n")
end.
```

### Custom TTL

```erlang
%% Cache with custom TTL (30 minutes)
erlmcp_icon_cache:cache_icon(Uri, Metadata, 1800000).

%% Change default TTL for future entries
erlmcp_icon_cache:set_ttl(7200000).  %% 2 hours
```

### Cache Invalidation

```erlang
%% Invalidate when icon is updated
erlmcp_icon_cache:invalidate_icon(Uri).

%% Clear entire cache
erlmcp_icon_cache:invalidate_all().
```

### Statistics

```erlang
%% Get cache statistics
Stats = erlmcp_icon_cache:get_cache_stats(),
io:format("Cache hits: ~p~n", [maps:get(hits, Stats)]),
io:format("Cache misses: ~p~n", [maps:get(misses, Stats)]),
io:format("Current cache size: ~p~n", [maps:get(cache_size, Stats)]).
```

## Testing

### Test Module

Located at: `/Users/sac/erlmcp/test/erlmcp_icon_cache_tests.erl`

#### Test Coverage (15 test cases)

1. **test_cache_basic_operations** - Store and retrieve
2. **test_cache_expiration** - TTL enforcement
3. **test_cache_invalidation** - Manual invalidation
4. **test_cache_hits_and_misses** - Statistics tracking
5. **test_cache_ttl_configuration** - TTL configuration
6. **test_cache_stats_tracking** - All stats fields
7. **test_cache_cleanup** - Periodic cleanup
8. **test_cache_custom_ttl** - Per-entry TTL
9. **test_cache_metadata_preservation** - Complex metadata
10. **test_cache_concurrent_access** - Multi-process access
11. **test_cache_invalid_inputs** - Error handling
12. **test_cache_invalidate_all** - Full cache clear
13. **test_cache_get_expired_entry** - Expired entry retrieval
14. **test_cache_multiple_icons** - Multiple entries
15. **test_cache_ttl_enforcement** - TTL boundary testing

#### Running Tests

```bash
# Run all icon cache tests
rebar3 eunit module=erlmcp_icon_cache_tests

# Run with verbose output
rebar3 eunit module=erlmcp_icon_cache_tests --verbose
```

#### Expected Results

All 15 tests should pass:

```
======================== Running erlmcp_icon_cache_tests ========================
test_cache_basic_operations ........................ ok
test_cache_expiration .............................. ok
test_cache_invalidation ............................ ok
test_cache_hits_and_misses ......................... ok
test_cache_ttl_configuration ....................... ok
test_cache_stats_tracking .......................... ok
test_cache_cleanup ................................. ok
test_cache_custom_ttl .............................. ok
test_cache_metadata_preservation .................. ok
test_cache_concurrent_access ....................... ok
test_cache_invalid_inputs .......................... ok
test_cache_invalidate_all .......................... ok
test_cache_get_expired_entry ....................... ok
test_cache_multiple_icons .......................... ok
test_cache_ttl_enforcement ......................... ok

======================== 15 tests, 0 failures ========================
```

## Performance Characteristics

### Time Complexity

- **cache_icon/3**: O(1) - HashMap insertion
- **get_cached_icon/1**: O(1) - HashMap lookup + expiration check
- **invalidate_icon/1**: O(1) - HashMap removal
- **get_cache_stats/0**: O(1) - Stat collection
- **cleanup_expired/0**: O(n) - Linear scan (runs every 5 minutes, non-blocking)

### Space Complexity

- **Memory**: O(n) where n = number of cached icons
- **Each entry**: ~256 bytes (URI, metadata, timestamps)

### Example: 10,000 Icons

- Cache size: ~2.6 MB
- TTL: 1 hour (auto-cleanup removes expired)
- Cleanup cycle: Every 5 minutes

## Configuration Examples

### Development

```erlang
{erlmcp, [
    {icon_cache_ttl_ms, 600000}  % 10 minutes for quick testing
]}
```

### Production

```erlang
{erlmcp, [
    {icon_cache_ttl_ms, 86400000} % 24 hours for production
]}
```

### High-Traffic

```erlang
{erlmcp, [
    {icon_cache_ttl_ms, 300000}  % 5 minutes - frequent refresh
]}
```

## Compliance Checklist

- [x] Cache icon metadata (size, MIME type, etc.)
- [x] Enforce cache TTL (time-to-live)
- [x] Default TTL: 1 hour (3600000 ms)
- [x] Configurable TTL via sys.config
- [x] Invalidate cache when icon changes
- [x] Automatic cleanup of expired entries
- [x] Cache statistics tracking
- [x] Concurrent access support
- [x] Comprehensive test suite (15+ tests)
- [x] Production-ready error handling

## Files Modified/Created

### New Files

1. `/Users/sac/erlmcp/src/erlmcp_icon_cache.erl` - Icon cache module
2. `/Users/sac/erlmcp/test/erlmcp_icon_cache_tests.erl` - Test suite
3. `/Users/sac/erlmcp/docs/gap-37-icon-cache.md` - This documentation

### Modified Files

1. `/Users/sac/erlmcp/src/erlmcp_sup.erl` - Added icon cache to supervision tree
2. `/Users/sac/erlmcp/config/sys.config` - Added icon cache TTL configuration

## References

- **MCP 2025-11-25 Specification**: Icon metadata caching requirements
- **Gap #37**: Icon Metadata Caching - MCP 2025-11-25 Compliance Review
- **Related Gaps**:
  - Gap #27: Icon MIME Type Parsing
  - Gap #36: Icon Validator Module

## See Also

- `/Users/sac/erlmcp/docs/MCP_2025-11-25_COMPLIANCE_GAPS_DETAILED.md` - Full compliance review
- Erlang `gen_server` documentation
- Erlang map/dict performance characteristics
