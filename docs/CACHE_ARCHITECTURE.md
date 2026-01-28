# erlmcp_cache - Multi-Level Intelligent Caching Layer

## Architecture Overview

The `erlmcp_cache` module implements a sophisticated 3-tier caching architecture for erlmcp, providing high-performance resource and tool result caching with intelligent eviction, expiration, and invalidation strategies.

## Three-Level Cache Hierarchy

### L1: ETS (In-Memory, Fast)
- **Technology**: Erlang Term Storage (ETS)
- **Characteristics**:
  - Process-local, in-memory storage
  - Sub-millisecond access latency (<1ms)
  - Read/write concurrency enabled
  - Volatile (lost on process restart)
- **Target Latency**: <1ms
- **Default Size**: 10,000 entries

### L2: Mnesia (Replicated, Durable)
- **Technology**: Mnesia distributed database
- **Characteristics**:
  - Cluster-wide replication
  - Disk-backed persistence
  - ACID transactions
  - Survives node restarts
  - Cross-node cache coherence
- **Target Latency**: <5ms
- **Default Size**: 100,000 entries

### L3: External (Redis/Memcached)
- **Technology**: Pluggable external cache backend
- **Characteristics**:
  - Shared cache across clusters
  - Independent scaling
  - Optional (disabled by default)
  - Network I/O overhead
- **Target Latency**: <50ms (network dependent)
- **Default Size**: Unlimited (external system)

## Cache Strategies

### 1. TTL-Based Expiration
```erlang
erlmcp_cache:put(Key, Value, {ttl, 300})  % 5 minutes
```
- Automatic expiration after specified duration
- Periodic cleanup (default: 1 minute intervals)
- Lazy expiration on access

### 2. LRU Eviction
```erlang
erlmcp_cache:put(Key, Value, {lru, MaxSize})
```
- Least Recently Used eviction when cache is full
- Tracks access time and frequency
- Optimal for hot data workloads

### 3. Write-Through
```erlang
erlmcp_cache:put(Key, Value, write_through)
```
- Synchronous writes to all cache levels (L1, L2, L3)
- Strong consistency guarantee
- Higher write latency, lower read latency

### 4. Write-Back
```erlang
erlmcp_cache:put(Key, Value, write_back)
```
- Asynchronous writes to L2/L3
- Lower write latency
- Eventually consistent
- Risk of data loss on crash (L1 only)

## Advanced Features

### Tag-Based Invalidation
Invalidate all cache entries with a given tag:
```erlang
%% Tag entries during insertion
erlmcp_cache:put(Key, Value, {ttl, 300}, #{tags => [<<"user:123">>]}),

%% Invalidate all entries for user 123
erlmcp_cache:invalidate_by_tag(<<"user:123">>).
```

**Use Cases**:
- User-specific data invalidation
- Resource type invalidation
- Namespace-based cache clearing

### Dependency Tracking
Automatically invalidate dependent cache entries:
```erlang
%% Mark dependencies
erlmcp_cache:put(Key, Value, {ttl, 300},
                 #{dependencies => [<<"primary_resource">>]}),

%% Invalidate all entries depending on primary_resource
erlmcp_cache:invalidate_by_dependency(<<"primary_resource">>).
```

**Use Cases**:
- Cascading invalidation (parent/child resources)
- Derived data invalidation
- Consistency maintenance

### ETag Support (HTTP Cache-Control)
```erlang
%% Generate ETag
ETag = erlmcp_cache:generate_etag(Value),

%% Conditional request
case erlmcp_cache:check_etag(Key, ClientETag) of
    true -> {304, not_modified};
    false -> {200, Value}
end.
```

**Benefits**:
- HTTP 304 Not Modified responses
- Bandwidth savings
- Client-side caching integration

### Probabilistic Cache Warming
Preload cache with expensive computations:
```erlang
erlmcp_cache:warm_cache(Key, fun() ->
    expensive_computation()
end).
```

**Characteristics**:
- Asynchronous execution
- Non-blocking
- Error-tolerant (failures don't affect cache)

## Cache Coherence (Distributed)

When using Mnesia (L2), cache is automatically replicated across nodes:

1. **Write Path**:
   - Write to L1 (local node)
   - Replicate to L2 (all nodes via Mnesia)
   - Optionally write to L3 (external cache)

2. **Read Path**:
   - Check L1 (local node)
   - If miss, check L2 (replicated)
   - If miss, check L3 (external)
   - Promote to L1 on hit

3. **Invalidation Path**:
   - Invalidate L1 (local node)
   - Invalidate L2 (all nodes via Mnesia)
   - Invalidate L3 (external)

## Performance Metrics

The cache tracks comprehensive statistics:

```erlang
Stats = erlmcp_cache:stats().
%% Returns:
%% #{
%%   hits => 1500,
%%   misses => 200,
%%   l1_hits => 1400,
%%   l2_hits => 90,
%%   l3_hits => 10,
%%   evictions => 50,
%%   expirations => 30,
%%   writes => 250,
%%   deletes => 20,
%%   l1_size => 8500,
%%   l2_size => 45000,
%%   hit_rate => 0.88,  % 88%
%%   total_requests => 1700
%% }
```

### Key Metrics
- **Hit Rate**: Percentage of cache hits (target: >80%)
- **L1 Hit Rate**: Percentage hitting L1 (target: >70%)
- **Eviction Rate**: Number of LRU evictions
- **Expiration Rate**: Number of TTL expirations

## Configuration

### Application Environment
Configure cache in `sys.config` or `erlmcp.app.src`:

```erlang
{erlmcp, [
    {cache_defaults, #{
        max_l1_size => 10000,           % Max L1 entries
        max_l2_size => 100000,          % Max L2 entries
        default_ttl_seconds => 300,     % Default TTL (5 min)
        cleanup_interval_ms => 60000,   % Cleanup every 1 min
        allowed_resource_dirs => [<<"/">>]  % Security: allowed paths
    }}
]}
```

### Runtime Configuration
Start cache with custom config:

```erlang
{ok, Pid} = erlmcp_cache:start_link(#{
    max_l1_size => 5000,
    max_l2_size => 50000,
    default_ttl_seconds => 600,
    external_cache => #{
        module => eredis,
        config => #{host => "localhost", port => 6379}
    }
}).
```

## Benchmarks

Run cache benchmarks:

```bash
# Quick benchmark (1K ops)
rebar3 shell
> erlmcp_bench_cache:run(<<"cache_hit_1k">>).

# Full benchmark suite
> erlmcp_bench_cache:run_all().
```

### Expected Performance
- **L1 Hit Latency**: <1ms (target)
- **L2 Hit Latency**: <5ms (target)
- **L3 Hit Latency**: <50ms (network dependent)
- **Throughput Improvement**: 5-10x for cacheable requests
- **Hit Rate**: >80% (typical workloads)

### Benchmark Workloads
1. `cache_hit_1k` - 1K cache hits (1 worker)
2. `cache_hit_10k` - 10K cache hits (10 workers)
3. `cache_hit_100k` - 100K cache hits (100 workers)
4. `cache_mixed_workload` - 70% reads, 30% writes
5. `cache_eviction_stress` - LRU eviction under pressure
6. `cache_write_through` - Write-through strategy
7. `cache_write_back` - Write-back strategy

## Integration with MCP Server

### Resource Caching
Cache resource reads with automatic invalidation:

```erlang
%% In erlmcp_server resource handler
case erlmcp_cache:get(Uri) of
    {ok, CachedContent} ->
        CachedContent;
    {error, not_found} ->
        Content = fetch_resource(Uri),
        erlmcp_cache:put(Uri, Content, {ttl, 300},
                         #{tags => [<<"resources">>]}),
        Content
end.
```

### Tool Result Caching
Cache tool call results (idempotent tools only):

```erlang
%% In tool handler
CacheKey = {tool, ToolName, erlang:phash2(Arguments)},
case erlmcp_cache:get(CacheKey) of
    {ok, Result} -> Result;
    {error, not_found} ->
        Result = execute_tool(Arguments),
        erlmcp_cache:put(CacheKey, Result, {ttl, 60}),
        Result
end.
```

### Subscription Updates
Invalidate cache on resource updates:

```erlang
%% On resource update notification
erlmcp_cache:delete(ResourceUri),
erlmcp_cache:invalidate_by_dependency(ResourceUri).
```

## Testing

Run comprehensive test suite:

```bash
rebar3 eunit --module=erlmcp_cache_tests
```

### Test Coverage
- Basic get/put operations
- TTL expiration (automatic cleanup)
- LRU eviction (max size limits)
- Write-through and write-back strategies
- Tag-based invalidation
- Dependency tracking
- ETag generation and conditional requests
- Distributed cache coherence
- Concurrent access (50 workers)
- Large values (1MB binaries)
- Edge cases (empty values, complex terms)

Target: **80%+ coverage** (Chicago School TDD)

## Security Considerations

### Path Traversal Protection
Cache includes built-in path canonicalization for resource URIs:

```erlang
%% Automatically validates and canonicalizes paths
erlmcp_cache:put(<<"file:///etc/../etc/passwd">>, Content)
%% -> Rejected if outside allowed_resource_dirs
```

### Cache Poisoning Prevention
- ETag validation prevents serving stale content
- TTL ensures bounded staleness
- Tag-based invalidation prevents category-wide poisoning

## Monitoring and Observability

### OpenTelemetry Integration
Cache operations are traced via erlmcp_tracing:

```erlang
%% Automatic span creation for cache operations
SpanCtx = erlmcp_tracing:start_span(<<"cache.get">>),
%% Attributes: cache.key, cache.level, cache.hit
```

### Metrics Export
Export cache metrics to Prometheus/Grafana:

```erlang
erlmcp_metrics:export_cache_stats().
```

### Health Checks
Cache health is monitored via erlmcp_health_monitor:

```erlang
%% Registered in supervision tree
erlmcp_health_monitor:check_cache().
```

## Future Enhancements

### Planned Features (v0.8.0+)
1. **Adaptive TTL**: Adjust TTL based on access patterns
2. **Prefetching**: Predictive cache warming
3. **Compression**: Compress large values in L2/L3
4. **Sharding**: Distribute L2 cache across nodes
5. **Redis Backend**: Full L3 Redis integration
6. **Memcached Backend**: L3 Memcached support
7. **Cache-Aside Pattern**: Automatic origin fallback
8. **Bloom Filters**: Reduce negative lookups

### Under Consideration
- **Write Coalescing**: Batch L2/L3 writes
- **Read-Through**: Automatic origin fetch on miss
- **Refresh-Ahead**: Background refresh before expiration
- **Multi-Region**: Cross-datacenter cache replication

## References

- **Architecture**: `docs/architecture.md`
- **OTP Patterns**: `docs/otp-patterns.md`
- **API Reference**: `docs/api-reference.md`
- **Test Suite**: `test/erlmcp_cache_tests.erl`
- **Benchmarks**: `bench/erlmcp_bench_cache.erl`

## License

Apache-2.0 (same as erlmcp)
