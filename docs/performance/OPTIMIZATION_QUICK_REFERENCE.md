# Performance Optimization Quick Reference

**For**: erlmcp developers and operators  
**Purpose**: Rapid reference for performance tuning and optimization

---

## Performance Hierarchy (Fastest → Slowest)

```
Process Dictionary (µs)     <  Fastest, not concurrent
  ↓
ETS with read_concurrency   <  Best for concurrent reads
  ↓
Message Passing (local)     <  Standard OTP pattern
  ↓
Mnesia (local)              <  Transactional, distributed
  ↓
File I/O                    <  Disk-bound
  ↓
Network I/O                 <  Network-bound
```

---

## Hot Path Optimization Checklist

### 1. ETS Tables (Concurrent Access)

```erlang
%% ✅ DO: Enable concurrency options
ets:new(table, [
    set,
    public,
    {read_concurrency, true},       % Lock-free reads
    {write_concurrency, auto},      % OTP 28+: automatic optimization
    {decentralized_counters, true}  % OTP 28+: faster size/1
]).

%% ❌ DON'T: Use default options for concurrent workloads
ets:new(table, [set, public]).  % Bottlenecks at scale
```

**Impact**: 10-50x improvement for concurrent access patterns.

### 2. Message Size (Avoid Large Message Copies)

```erlang
%% ✅ DO: Use references for large data
gen_server:call(Pid, {process_data, DataRef}).

%% ❌ DON'T: Send large binaries directly
gen_server:call(Pid, {process_data, <<LargeData/binary>>}).
```

**Impact**: Reduces heap pressure, faster GC, 20-30% throughput gain.

### 3. Binary Handling

```erlang
%% ✅ DO: Use binary syntax, avoid lists:append
<<Header/binary, Body/binary>>.

%% ❌ DON'T: Convert to lists unnecessarily
binary_to_list(Data1) ++ binary_to_list(Data2).
```

**Impact**: 2-5x faster binary operations.

### 4. Process Spawning (Always Supervised)

```erlang
%% ✅ DO: Use supervisor for all processes
supervisor:start_child(SupRef, ChildSpec).

%% ❌ DON'T: Spawn unsupervised processes
spawn(fun() -> worker_loop() end).
```

**Impact**: Crash isolation, automatic restart, nine-nines reliability.

---

## Common Bottlenecks & Fixes

### Bottleneck: High Latency Percentiles (p99+)

**Symptoms**: p50 looks good, p99 is 10-100x higher

**Diagnosis**:
```erlang
%% Measure latency distribution
Latencies = [measure_operation() || _ <- lists:seq(1, 10000)],
Sorted = lists:sort(Latencies),
P99 = lists:nth(9900, Sorted),
P999 = lists:nth(9990, Sorted).
```

**Fixes**:
1. **Reduce GC pressure**: Use process dictionary for temporary data
2. **Batch operations**: Process multiple messages per gen_server call
3. **Offload to dirty schedulers**: Heavy CPU work

```erlang
%% Use dirty scheduler for heavy work
erlang:spawn_opt(fun() ->
    heavy_json_encode(Data)
end, [{scheduler, dirty_cpu}]).
```

### Bottleneck: Low Throughput

**Symptoms**: Operations/sec below expected, CPU not saturated

**Diagnosis**:
```erlang
%% Check scheduler utilization
erlang:statistics(scheduler_wall_time).
```

**Fixes**:
1. **Increase parallelism**: More workers
2. **Reduce serialization**: Avoid bottleneck processes
3. **Use poolboy**: Connection pooling for external resources

```erlang
%% Pool configuration
PoolSpec = #{
    name => {local, worker_pool},
    worker_module => worker_module,
    size => 100,            % Pool size
    max_overflow => 50      % Max additional workers
}.
```

### Bottleneck: High Memory Usage

**Symptoms**: Memory grows unbounded, GC pauses increase

**Diagnosis**:
```erlang
%% Find memory hogs
recon:proc_count(memory, 10).  % Top 10 by memory
```

**Fixes**:
1. **Hibernate idle processes**: Compact heap
2. **Use ETS for shared data**: Avoid per-process copies
3. **Limit queue sizes**: Bound mailboxes

```erlang
%% Hibernate idle process
gen_server:cast(Pid, hibernate).

%% In gen_server callback
handle_cast(hibernate, State) ->
    {noreply, State, hibernate}.
```

### Bottleneck: Lock Contention

**Symptoms**: Many processes, low throughput, high CPU

**Diagnosis**:
```erlang
%% Check for lock contention
lcnt:start(),
% Run workload
lcnt:collect(),
lcnt:inspect(locks).
```

**Fixes**:
1. **Shard ETS tables**: Multiple tables, hash-based routing
2. **Use gproc for registry**: O(log N) instead of O(N)
3. **Avoid single bottleneck process**: Distribute work

```erlang
%% Shard ETS tables
ShardCount = erlang:system_info(schedulers_online),
Tables = [ets:new(shard, [Options]) || _ <- lists:seq(1, ShardCount)],
Shard = erlang:phash2(Key, ShardCount),
Table = lists:nth(Shard + 1, Tables).
```

---

## Profiling Commands

### 1. fprof (Function-level profiling)

```erlang
%% Start profiling
fprof:trace([start, {procs, all}]).

%% Run workload
my_module:benchmark().

%% Stop and analyze
fprof:trace(stop),
fprof:profile(),
fprof:analyse([{dest, "profile.txt"}, {totals, true}]).
```

### 2. eprof (Time-based profiling)

```erlang
eprof:start(),
eprof:profile([Pid]),
%% Run workload
eprof:stop_profiling(),
eprof:analyze(total).
```

### 3. recon (Production-safe tracing)

```erlang
%% Trace function calls (top 100)
recon_trace:calls({erlmcp_json_rpc, encode, '_'}, 100).

%% Find memory leaks
recon:proc_window(memory, 10, 5000).  % Top 10 over 5 sec
```

---

## OTP 28 Specific Optimizations

### 1. Automatic Write Concurrency

```erlang
%% OTP 28+: 'auto' adapts to workload
{write_concurrency, auto}
```

### 2. Decentralized Counters

```erlang
%% OTP 28+: Faster ets:info(Tab, size)
{decentralized_counters, true}
```

### 3. Improved Scheduler Bindings

```bash
# More aggressive scheduler binding
erl +sbt db +sbwt very_short
```

---

## Performance Testing Pattern

```erlang
%% Standard benchmark pattern
benchmark(Operations) ->
    Start = erlang:monotonic_time(microsecond),
    
    lists:foreach(fun(I) ->
        %% Operation to benchmark
        operation(I)
    end, lists:seq(1, Operations)),
    
    End = erlang:monotonic_time(microsecond),
    DurationS = (End - Start) / 1_000_000,
    Throughput = Operations / DurationS,
    
    #{
        operations => Operations,
        duration_s => DurationS,
        throughput_ops_per_s => Throughput,
        latency_avg_us => (End - Start) / Operations
    }.
```

---

## Metrics to Track

### Essential Metrics

1. **Throughput**: Operations/second
2. **Latency**: p50, p95, p99, p999 (microseconds)
3. **Memory**: Heap size, RSS, binary memory
4. **GC**: Pause frequency and duration
5. **Schedulers**: Utilization percentage

### Collection Commands

```erlang
%% Throughput
erlang:statistics(run_queue).

%% Memory
erlang:memory(total).
erlang:memory(processes).

%% GC
erlang:statistics(garbage_collection).

%% Scheduler utilization
erlang:statistics(scheduler_wall_time).
```

---

## Quick Wins (High Impact, Low Effort)

1. **Enable ETS concurrency** → +10-50x for reads
2. **Use binary syntax** → +2-5x for data handling
3. **Avoid process bottlenecks** → +5-10x throughput
4. **Hibernate idle processes** → -30% memory
5. **Use dirty schedulers** → +10-20% for CPU-bound

---

## When to Stop Optimizing

**Stop if**:
1. ✅ All SLOs met with 2x margin
2. ✅ CPU utilization <70% under peak load
3. ✅ p99 latency <10ms for critical paths
4. ✅ Memory growth is bounded
5. ✅ No production incidents related to performance

**Armstrong's Law**: "Premature optimization is the root of all evil, but ignoring performance until production is worse."

---

## References

- [Erlang Efficiency Guide](http://erlang.org/doc/efficiency_guide/users_guide.html)
- [recon documentation](http://ferd.github.io/recon/)
- [ETS best practices](http://erlang.org/doc/man/ets.html)
- erlmcp: `docs/performance/OPTIMIZATION_GUIDE.md`
