# Performance Debugging Cheat Sheet

## Quick Commands

### Compilation & Testing
```bash
rebar3 compile                      # Must be 0 errors
rebar3 eunit --verbose             # Unit tests
rebar3 ct --suite=test/*           # Integration tests
make benchmark-quick              # < 2min benchmark
make check                         # Full validation
```

### Interactive Debugging
```bash
make console                       # Erlang shell
observer:start()                    # Process monitor
recon_trace:calls({mod, fun}, 100) # Live trace
```

### Profiling
```erlang
fprof:trace([start, {procs, [self()]}]),
% Run code
fprof:profile(),
fprof:analyse([{dest, "profile.txt"}]).
```

## Performance Metrics

### Expected Baselines (Macbook Pro 2026)
- **Registry**: 553K msg/s
- **Queue**: 971K msg/s  
- **Pool**: 149K msg/s
- **Session**: 242K msg/s
- **Network I/O**: 43K msg/s
- **Sustained Load**: 372K msg/s (60M ops/30min)

### Quality Gates
- **Max latency p99**: <5ms
- **Performance regression**: <10%
- **Memory growth**: <1 MiB/min
- **Connection capacity**: 40-50K active

## Debug Patterns

### Memory Leak Detection
```bash
# Track memory over time
echo "Time,Memory(MiB)" > memory.csv
while true; do
    ps -p $(pgrep -f erlang) -o pid,rss --no-headers | \
    awk '{print strftime("%H:%M"), $2/1024}' >> memory.csv
    sleep 60
done
```

### Bottleneck Identification
```erlang
% Check component performance
erlmcp_bench_core_ops:run(<<"core_ops_100k">>).
erlmcp_bench_network_real:run(<<"tcp_sustained_10k">>).

% Registry <400K msg/s → bottleneck
% Network <30K msg/s → bottleneck
```

### Error Recovery
```erlang
% Supervisor restart strategy
{restart, transient}        % Restart after crash
{shutdown, 5000}           % 5s timeout
```

## Optimization Techniques

### Code Optimizations
```erlang
% Bad: String concatenation
lists:foldl(fun(A, Acc) -> Acc ++ A end, [], L).

% Good: Binary concatenation  
lists:foldl(fun(A, Acc) <<Acc/binary, A/binary>>, <<>>, L).
```

### Data Structure Choice
```erlang
% Small datasets
gb_sets:set()        % Fast membership
gb_trees:empty()    % Ordered access

% Large datasets  
ets:new(table, [set, public, named_table]).
mnesia:create_table(...).
```

### Concurrency Patterns
```erlang
% Pool management
poolboy:start_link([{size, 10}, {max_overflow, 5}])

% Queue management  
queue:start([{capacity, 1000}])
```

## Monitoring Commands

### Process Analysis
```erlang
% Monitor specific process
process_info(Pid, messages).    % Queue length
process_info(Pid, memory).      % Memory usage

% Monitor all processes
[process_info(P, memory) || P <- processes()].
```

### System Analysis
```erlang
system_info(process_count).     # Total processes
system_info(memory).           # Total memory

% With recon
recon:memory(processes, 10).   # Top 10 memory
recon_cpuinfo:top(5).          # Top 5 CPU
```

## Common Issues & Solutions

### High Memory Usage
- **Cause**: Unbounded message queues
- **Solution**: Add queue depth limits
- **Check**: `process_info(Pid, messages)`

### Slow Throughput
- **Registry**: Use ETS instead of gproc
- **Encoding**: Switch from jsx to jiffy
- **Network**: Check socket buffers

### Process Crashes
- **Cause**: Timeout in long operation
- **Solution**: Add timeout and retry logic
- **Check**: Supervisor restart logs

## Script Shortcuts

### Performance Check
```bash
./scripts/health/check.sh      # System health
./scripts/analyze/queue.sh     # Queue analysis
./scripts/profile/fprof.sh     # Quick profile
```

### Benchmarking
```bash
./bench/run_core_benchmarks.sh    # Core operations
./bench/run_stress_tests.sh       # Load testing
./bench/run_chaos_tests.sh       # Failure injection
```

## CLI Integration

### Pre-commit Hook
```bash
#!/bin/bash
rebar3 compile && rebar3 eunit && make benchmark-quick
```

### GitHub Action
```yaml
- name: Performance Check
  run: |
    rebar3 compile
    make benchmark-quick
    ./scripts/performance/check_regression.sh
```

## Performance Timeline

### Immediate Actions (1-2 hours)
1. Run `make benchmark-quick` - establish baseline
2. Check for obvious regressions
3. Verify all tests passing
4. Monitor memory usage over 5 minutes

### Short Term (1-2 days)
1. Profile hot paths with fprof
2. Implement quick wins (jiffy, ETS)
3. Add monitoring dashboards
4. Set up alerting rules

### Medium Term (1-2 weeks)
1. Implement connection pooling
2. Add request queuing
3. Optimize supervision trees
4. Create performance SLAs

### Long Term (ongoing)
1. Continuous monitoring
2. Regular profiling
3. Performance budget management
4. Capacity planning
