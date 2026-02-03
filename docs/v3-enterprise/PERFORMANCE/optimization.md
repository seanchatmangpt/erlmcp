# Performance Optimization Guide

## Overview

This comprehensive guide provides detailed strategies and procedures for optimizing erlmcp v3 performance in production environments. It covers JVM tuning, configuration optimization, query optimization, and infrastructure scaling.

## Performance Architecture

### Performance Characteristics

| Metric | Production Target | Measurement Method |
|--------|------------------|-------------------|
| Response Time | < 10ms p95 | Application metrics |
| Throughput | 10,000+ req/s | Load testing |
| CPU Utilization | < 70% average | System monitoring |
| Memory Usage | < 80% | Memory profiling |
| Error Rate | < 0.1% | Error tracking |

### Performance Monitoring Stack

```
┌─────────────────────────────────────────────────────────┐
│                     Application Layer                     │
├─────────────────────────────────────────────────────────┤
│   Prometheus/Grafana  │   APM Tools   │   Custom Metrics   │
├─────────────────────────────────────────────────────────┤
│              Erlang/OTP Runtime (BEAM VM)               │
├─────────────────────────────────────────────────────────┤
│                Operating System                         │
├─────────────────────────────────────────────────────────┤
│                  Infrastructure                         │
└─────────────────────────────────────────────────────────┘
```

## JVM Tuning

### 1. BEAM VM Configuration

#### Erlang Runtime Tuning

```erlang
# vm.args file
-name erlmcp@$(hostname -i)
-setcookie erlmcp
-kernel inet_dist_use_interface {10.0.0.0,24}
-heart
+pc unicode
+sbwt none
+sbwtdcpu none
+sbwtdio none
```

#### Erlang Configuration

```erlang
# sys.config
[{erlmcp, [
    {runtime, [
        {schedulers, 8},  % Use 8 schedulers
        {schedulers_online, 8},  % Keep all online
        {process_limit, 1048576},  % Max processes
        {port_limit, 65536},  % Max ports
        {max_heap_size, {256, mb}},  % Max heap size
        {min_heap_size, {10, mb}},  % Min heap size
        {max_port_size, {1, gb}},  % Max port size
        {trace_level, 0},  % Trace level
        {async_threads, 64},  % Async threads
        {thread_stack_size, {8, mb}}  % Thread stack size
    ]}
]}].
```

### 2. Memory Management

#### Memory Allocation

```erlang
# Custom memory management
configure_memory_management() ->
    % Set global memory allocator
    erlang:system_flag({schedulers_online, erlang:system_info(schedulers)}),

    % Configure ETS tables
    configure_ets_tables(),

    % Configure mnesia
    configure_mnesia(),

    % Set garbage collection settings
    set_gc_settings().

configure_ets_tables() ->
    % Large lookup tables
    ets:new(resources_cache, [
        named_table,
        public,
        {write_concurrency, true},
        {read_concurrency, true},
        {heir, erlmcp_sup, no_heir}
    ]),

    % Session cache
    ets:new(sessions_cache, [
        named_table,
        public,
        {ordered_set, true},
        {heir, erlmcp_sup, sessions_heir}
    ]).

set_gc_settings() ->
    % GC settings for different process types
    gc_settings = [
        {frequency, 1000},  % GC frequency
        {heap_size, {64, mb}},  % Heap size
        {min_heap_size, {1, mb}},  % Min heap size
        {max_heap_size, {512, mb}}  % Max heap size
    ],

    % Apply to processes
    [erlang:process_flag(gc_settings, gc_settings) || _ <- lists:seq(1, erlang:system_info(process_limit))].
```

### 3. Garbage Collection Optimization

#### GC Strategy

```erlang
optimize_gc() ->
    % Set hybrid garbage collection
    erlang:system_flag({fullsweep_after, 65535}),
    erlang:system_flag({min_heap_size, 1024}),
    erlang:system_flag({max_heap_size, {1024, mb}}),

    % Enable young generation GC
    young_gen_gc_config = #{
        ratio => 1:2,
        size => {64, mb},
        age => 10
    },

    % Enable old generation GC
    old_gen_gc_config = #{
        ratio => 1:10,
        size => {512, mb},
        age => 60
    },

    % Apply configuration
    apply_gc_config(young_gen_gc_config, old_gen_gc_config).

monitor_gc() ->
    % Monitor GC pauses
    register(gc_monitor, spawn(fun gc_monitor_loop/0)),

    % Register for GC notifications
    erlang:system_monitor({gc_monitor, {spawn, process}}),
    erlang:system_monitor({gc_monitor, {spawn, port}}).
```

## Configuration Optimization

### 1. Connection Pool Configuration

#### HTTP Connection Pool

```erlang
{erlmcp, [
    {connections, [
        {pool_size, 100},  % Total connections
        {max_pool_size, 200},  % Max per host
        {timeout, 5000},  % Connection timeout
        {keep_alive, 30000},  % Keep alive
        {max_retries, 3},  % Max retries
        {retry_delay, 1000}  % Retry delay
    ]}
]}.
```

#### Database Connection Pool

```erlang
{erlmcp, [
    {database, [
        {pool_size, 50},  % Connection pool size
        {max_overflow, 20},  % Max overflow
        {timeout, 15000},  % Query timeout
        {idle_timeout, 30000},  % Idle timeout
        {validation_interval, 30000},  % Validation interval
        {test_on_borrow, true}  % Test on borrow
    ]}
]}.
```

### 2. Cache Configuration

#### Multi-level Cache Strategy

```erlang
{erlmcp, [
    {cache, [
        {levels, [
            {l1,  % Local cache
                {type, ets},
                {size, 1000},
                {ttl, 60000},
                {eviction, lru}
            },
            {l2,  % Distributed cache
                {type, redis},
                {nodes, ["redis1", "redis2"]},
                {size, 10000},
                {ttl, 300000},
                {eviction, allkeys_lru}
            }
        ]},
        {strategy, write_through},  % Write-through cache
        {invalidation, % Cache invalidation
            {mode, proactive},  % Proactive invalidation
            {policy, ttl}  % TTL-based invalidation
        }
    ]}
]}.
```

#### Cache Optimization

```erlang
optimize_cache() ->
    % Configure cache for different data types
    configure_resource_cache(),
    configure_session_cache(),
    configure_query_cache(),

    % Enable cache warm-up
    enable_cache_warmup(),

    % Configure cache monitoring
    enable_cache_monitoring().

configure_resource_cache() ->
    % Resource cache with short TTL
    ets:new(resources_cache, [
        named_table,
        public,
        {write_concurrency, true},
        {read_concurrency, true},
        {heir, erlmcp_sup, {cleanup, resources_cache}}
    ]),

    % Configure cache server
    erlmcp_cache_server:start(resources_cache, #{
        size => 10000,
        ttl => 300000,  % 5 minutes
        eviction => lru
    }).
```

### 3. Thread Pool Configuration

```erlang
{erlmcp, [
    {thread_pools, [
        {io_pool,  % I/O intensive tasks
            {size, 64},
            {max_size, 128},
            {queue_size, 1000},
            {thread_stack_size, {8, mb}}
        },
        {compute_pool,  % CPU intensive tasks
            {size, 16},
            {max_size, 32},
            {queue_size, 500},
            {thread_stack_size, {16, mb}}
        },
        {db_pool,  % Database tasks
            {size, 32},
            {max_size, 64},
            {queue_size, 200},
            {thread_stack_size, {4, mb}}
        }
    ]}
]}.
```

## Query Optimization

### 1. Database Query Optimization

#### Query Analysis

```erlang
% Enable query logging
enable_query_logging() ->
    application:set_env(erlmcp, query_log, true),
    application:set_env(erlmcp, slow_query_log, true),
    application:set_env(erlmcp, slow_query_threshold, 1000).  % ms

analyze_queries() ->
    % Get slow queries
    SlowQueries = erlmcp_db:get_slow_queries(),

    % Analyze query patterns
    Patterns = analyze_query_patterns(SlowQueries),

    % Generate recommendations
    Recommendations = generate_query_recommendations(Patterns),

    % Apply optimizations
    apply_query_optimizations(Recommendations).
```

#### Index Optimization

```erlang
optimize_indexes() ->
    % Analyze query patterns
    QueryPatterns = analyze_query_patterns(),

    % Create optimized indexes
    lists:foreach(fun(Pattern) ->
        create_optimized_index(Pattern)
    end, QueryPatterns),

    % Remove unused indexes
    remove_unused_indexes(),

    % Reorganize fragmented indexes
    reorganize_indexes().

create_optimized_index(Pattern) ->
    % Based on query pattern, create appropriate index
    case Pattern#query_pattern.type of
        equality ->
            create_equality_index(Pattern);
        range ->
            create_range_index(Pattern);
        fulltext ->
            create_fulltext_index(Pattern);
        composite ->
            create_composite_index(Pattern)
    end.
```

### 2. JSON Processing Optimization

#### JSON Parsing Caching

```erlang
{erlmcp, [
    {json, [
        {cache_size, 1000},  % Cache size
        {cache_ttl, 3600},  % Cache TTL in seconds
        {lazy_decode, true},  % Lazy decoding
        {compact_output, true}  % Compact JSON output
    ]}
]}.
```

#### JSON Optimization Strategies

```erlang
optimize_json_handling() ->
    % Use jiffy for JSON parsing (faster than built-in)
    application:set_env(jiffy, use_atoms, false),

    % Pre-compile JSON schemas
    compile_json_schemas(),

    % Enable streaming JSON
    enable_streaming_json(),

    % Optimize JSON serialization
    optimize_json_serialization().

compile_json_schemas() ->
    % Compile frequently used schemas
    Schemas = [
        resource_schema,
        tool_schema,
        session_schema,
        error_schema
    ],

    lists:foreach(fun(Schema) ->
        erlmcp_json_schema:compile(Schema)
    end, Schemas).
```

### 3. Regular Expression Optimization

#### Regex Compilation

```erlang
% Pre-compile regex patterns
compile_regex_patterns() ->
    Patterns = [
        {email, "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"},
        {url, "^(http|https)://[^\\s/$.?#].[^\\s]*$"},
        {ip_address, "^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)$"}
    ],

    lists:foreach(fun({Name, Pattern}) ->
        {ok, MP} = re:compile(Pattern, [unicode]),
        put({regex, Name}, MP)
    end, Patterns).
```

## Network Optimization

### 1. TCP Configuration

#### TCP Stack Tuning

```bash
# /etc/sysctl.conf
# TCP optimization
net.core.somaxconn = 65535
net.core.netdev_max_backlog = 10000
net.ipv4.tcp_max_syn_backlog = 65535
net.ipv4.tcp_syncookies = 1
net.ipv4.tcp_tw_reuse = 1
net.ipv4.tcp_fin_timeout = 10
net.ipv4.tcp_keepalive_time = 600
net.ipv4.tcp_keepalive_intvl = 60
net.ipv4.tcp_keepalive_probes = 3
net.core.rmem_max = 16777216
net.core.wmem_max = 16777216
net.ipv4.tcp_rmem = 4096 87380 16777216
net.ipv4.tcp_wmem = 4096 65536 16777216
```

#### Application-level TCP Settings

```erlang
{erlmcp, [
    {tcp, [
        {backlog, 1024},  % Listen backlog
        {nodelay, true},  % TCP_NODELAY
        {keepalive, true},  % SO_KEEPALIVE
        {linger, {true, 0}},  % SO_LINGER
        {buffer_size, 65536},  % Buffer size
        {recbuf_size, 65536},  % Receive buffer
        {sndbuf_size, 65536},  % Send buffer
        {max_connections, 10000}  % Max connections
    ]}
]}.
```

### 2. HTTP/2 Configuration

#### HTTP/2 Settings

```erlang
{erlmcp, [
    {http2, [
        {max_concurrent_streams, 100},  % Max concurrent streams
        {max_frame_size, 16384},  % Max frame size
        {max_header_list_size, 65536},  % Max header list size
        {push_enabled, true},  % Server push
        {compression_level, 5}  % Compression level
    ]}
]}.
```

#### HTTP/2 Optimization

```erlang
optimize_http2() ->
    % Enable multiplexing
    application:set_env(erlmcp, http2_multiplexing, true),

    % Enable header compression
    application:set_env(erlmcp, header_compression, true),

    % Configure priority streams
    configure_stream_priority(),

    % Enable flow control
    configure_flow_control().
```

## Load Testing

### 1. Load Test Configuration

#### Locust Configuration

```python
# locustfile.py
from locust import HttpUser, task, between
import random

class ErLmCpUser(HttpUser):
    wait_time = between(1, 5)

    def on_start(self):
        # Get authentication token
        response = self.client.post("/auth/token", json={
            "username": "testuser",
            "password": "testpass"
        })
        self.token = response.json()["access_token"]

    @task
    def get_resources(self):
        self.client.get(
            "/resources",
            headers={"Authorization": f"Bearer {self.token}"},
            name="get_resources"
        )

    @task
    def call_tool(self):
        self.client.post(
            "/tools/call",
            json={
                "name": "file_search",
                "arguments": {
                    "query": "test query",
                    "max_results": 10
                }
            },
            headers={"Authorization": f"Bearer {self.token}"},
            name="call_tool"
        )

    @task
    def create_session(self):
        self.client.post(
            "/sessions",
            json={
                "name": f"test-session-{random.randint(1, 1000)}",
                "metadata": {"test": True}
            },
            headers={"Authorization": f"Bearer {self.token}"},
            name="create_session"
        )
```

#### JMeter Configuration

```xml
<?xml version="1.0" encoding="UTF-8"?>
<jmeterTestPlan version="1.2" properties="5.0">
    <hashTree>
        <ThreadGroup name="erlmcp_load_test" guiclass="ThreadGroupGui" testclass="ThreadGroup" testname="Load Test">
            <arguments/>
            <CSVDataSet filename="users.csv" delimiter="," quoted="false" recycle="true" shareMode="all"/>
            <ThreadGroup guiclass="ThreadGroupGui" testclass="ThreadGroup" testname="Users" enabled="true">
                <elementProp name="ThreadGroup.main_controller" elementType="LoopController">
                    <boolProp name="LoopController.continue_forever">false</boolProp>
                    <stringProp name="LoopController.loops">10</stringProp>
                </elementProp>
                <stringProp name="ThreadGroup.num_threads">100</stringProp>
                <stringProp name="ThreadGroup.ramp_time">60</stringProp>
                <stringProp name="ThreadGroup.duration">300</stringProp>
            </ThreadGroup>
            <HTTPSampler guiclass="HttpTestSamplerGui" testclass="HTTPSampler" testname="Health Check">
                <elementProp name="HTTPsampler.Arguments" elementType="Arguments"/>
                <boolProp name="HTTPSampler.POST">false</boolProp>
                <stringProp name="HTTPSampler.domain">erlmcp.company.com</stringProp>
                <stringProp name="HTTPSampler.port">443</stringProp>
                <stringProp name="HTTPSampler.protocol">https</stringProp>
                <stringProp name="HTTPSampler.path">/health</stringProp>
            </HTTPSampler>
        </ThreadGroup>
    </hashTree>
</jmeterTestPlan>
```

### 2. Load Test Scenarios

#### Stress Test Scenario

```bash
#!/bin/bash
# stress_test.sh

# Ramp up from 100 to 10,000 users over 30 minutes
for users in 100 500 1000 2000 5000 10000; do
    echo "Starting test with $users users"
    locust -f locustfile.py --users $users --spawn-rate $((users/60)) --headless --run-time 10m --csv results/users_$users
    echo "Completed test with $users users"
    echo "-----"
done
```

#### Spike Test Scenario

```bash
#!/bin/bash
# spike_test.sh

# Start with baseline load
locust -f locustfile.py --users 100 --spawn-rate 10 --headless --run-time 5m --csv results/baseline

# Spike to 10,000 users over 5 minutes
locust -f locustfile.py --users 10000 --spawn-rate 2000 --headless --run-time 5m --csv results/spike

# Measure recovery time
echo "Measuring recovery time..."
sleep 60
locust -f locustfile.py --users 100 --spawn-rate 10 --headless --run-time 10m --csv results/recovery
```

## Performance Monitoring

### 1. Metrics Collection

#### Prometheus Metrics

```yaml
# prometheus-erlmcp.yml
apiVersion: v1
kind: ConfigMap
metadata:
  name: erlmcp-metrics
  namespace: monitoring
data:
  prometheus.yml: |
    global:
      scrape_interval: 15s
      evaluation_interval: 15s

    scrape_configs:
    - job_name: 'erlmcp'
      metrics_path: /metrics
      static_configs:
      - targets: ['erlmcp:8081']
      scrape_interval: 15s
      scrape_timeout: 10s
```

#### Custom Metrics

```erlang
% erlmcp_metrics.erl
-module(erlmcp_metrics).
-export([record_request/2, record_error/1, record_metric/2]).

-record(request, {
    method :: atom(),
    path :: binary(),
    status :: integer(),
    duration :: integer(),
    user :: binary()
}).

-record(error, {
    type :: atom(),
    message :: binary(),
    stacktrace :: list(),
    timestamp :: integer()
}).

-record(metric, {
    name :: binary(),
    value :: integer() | float(),
    labels :: map(),
    timestamp :: integer()
}).

record_request(Method, Path, Status, Duration, User) ->
    erlang:send(metrics_collector,
        #request{
            method = Method,
            path = Path,
            status = Status,
            duration = Duration,
            user = User
        }
    ).

record_error(Type, Message, Stacktrace) ->
    erlang:send(metrics_collector,
        #error{
            type = Type,
            message = Message,
            stacktrace = Stacktrace,
            timestamp = erlang:system_time(millisecond)
        }
    ).

record_metric(Name, Value, Labels) ->
    erlang:send(metrics_collector,
        #metric{
            name = Name,
            value = Value,
            labels = Labels,
            timestamp = erlang:system_time(millisecond)
        }
    ).
```

### 2. Performance Analysis

#### Performance Dashboard

```json
{
  "dashboard": {
    "title": "erlmcp Performance Dashboard",
    "panels": [
      {
        "title": "Request Rate",
        "type": "graph",
        "targets": [
          {
            "expr": "rate(erlmcp_requests_total[5m])",
            "legendFormat": "{{method}} {{status}}"
          }
        ],
        "gridPos": {"h": 8, "w": 12, "x": 0, "y": 0}
      },
      {
        "title": "Response Time",
        "type": "graph",
        "targets": [
          {
            "expr": "histogram_quantile(0.95, rate(erlmcp_request_duration_seconds_bucket[5m]))",
            "legendFormat": "95th percentile"
          },
          {
            "expr": "histogram_quantile(0.99, rate(erlmcp_request_duration_seconds_bucket[5m]))",
            "legendFormat": "99th percentile"
          }
        ],
        "gridPos": {"h": 8, "w": 12, "x": 12, "y": 0}
      },
      {
        "title": "Error Rate",
        "type": "graph",
        "targets": [
          {
            "expr": "rate(erlmcp_errors_total[5m]) / rate(erlmcp_requests_total[5m])",
            "legendFormat": "Error Rate"
          }
        ],
        "gridPos": {"h": 8, "w": 12, "x": 0, "y": 8}
      }
    ]
  }
}
```

#### Performance Analysis Script

```erlang
% performance_analysis.erl
-module(performance_analysis).
-export([analyze/0, generate_report/1]).

analyze() ->
    % Collect metrics
    Metrics = collect_metrics(),

    % Analyze patterns
    Patterns = analyze_patterns(Metrics),

    % Identify bottlenecks
    Bottlenecks = identify_bottlenecks(Metrics),

    % Generate recommendations
    Recommendations = generate_recommendations(Patterns, Bottlenecks),

    % Save analysis
    save_analysis(Metrics, Patterns, Bottlenecks, Recommendations).

collect_metrics() ->
    % Collect various metrics
    [
        get_cpu_metrics(),
        get_memory_metrics(),
        get_disk_metrics(),
        get_network_metrics(),
        get_application_metrics()
    ].

identify_bottlenecks(Metrics) ->
    % Check for high CPU usage
    CpuUsage = get_cpu_usage(Metrics),
    if
        CpuUsage > 80 ->
            [{cpu_usage, CpuUsage}];
        true ->
            []
    end ++
    % Check for memory usage
    MemoryUsage = get_memory_usage(Metrics),
    if
        MemoryUsage > 80 ->
            [{memory_usage, MemoryUsage}];
        true ->
            []
    end ++
    % Check for slow queries
    SlowQueries = get_slow_queries(Metrics),
    if
        SlowQueries > 100 ->
            [{slow_queries, SlowQueries}];
        true ->
            []
    end.
```

## Troubleshooting Performance Issues

### 1. Common Performance Issues

#### High CPU Usage

```bash
#!/bin/bash
# troubleshoot_cpu.sh

# Check top processes
top -o cpu

# Check erlang processes
ps aux | grep beam

# Check BEAM processes
pgrep beam -fl

# Check CPU per pod
kubectl top pods -n erlmcp

# Check erlang scheduler usage
erl -pa /path/to/beam -eval 'io:format("Schedulers: ~p~n", [erlang:system_info(schedulers)]), halt().'
```

#### Memory Leaks

```bash
#!/bin/bash
# troubleshoot_memory.sh

# Check memory usage
free -m

# Check BEAM memory
erl -pa /path/to/beam -eval 'io:format("Memory: ~p~n", [erlang:memory(total)]), halt().'

# Check process memory
kubectl exec -n erlmcp deployment/erlmcp -- erl -eval 'lists:foreach(fun(P) -> {Pid, Memory, _} = process_info(P, memory), io:format("~p: ~p~n", [Pid, Memory]) end, processes()), halt().'

# Check GC stats
kubectl exec -n erlmcp deployment/erlmcp -- erl -eval 'erlang:system_info({garbage_collection, detailed}), halt().'
```

### 2. Performance Profiling

#### BEAM Profiling

```erlang
% Enable profiling
enable_profiling() ->
    % Enable function call profiling
    eprof:start(),
    eprof:start_profiling([erlmcp_server]),

    % Run workload
    run_workload(),

    % Stop profiling
    eprof:stop_profiling(),
    eprof:analyze(procs).

% Enable tracing
enable_trace() ->
    % Trace specific functions
    erlang:trace_pattern({erlmcp_server, handle_call, 3}, true, [local]),

    % Trace messages
    erlang:trace(self(), true, [send, receive]),

    % Run workload
    run_workload().
```

#### Flame Graph Generation

```bash
#!/bin/bash
# generate_flame_graph.sh

# Generate stack trace
perf record -g -e cpu-clock ./erlmcp

# Convert to flame graph
perf script > out.perf
FlameGraph/stackcollapse-perf.pl out.perf > out.folded

# Generate SVG
FlameGraph/flamegraph.pl out.folded > flamegraph.svg
```

### 3. Performance Tuning Checklist

#### Initial Tuning
- [ ] Configure JVM memory settings
- [ ] Set thread pool sizes
- [ ] Configure connection pooling
- [ ] Enable compression
- [ ] Configure caching

#### Advanced Tuning
- [ ] Optimize database queries
- [ ] Implement load balancing
- [ ] Scale horizontally
- [ ] Enable HTTP/2
- [ ] Optimize network settings

#### Ongoing Optimization
- [ ] Monitor performance metrics
- [ ] Regular load testing
- [ ] Analyze logs for patterns
- [ ] Review and update configurations
- [ ] Implement performance budgets

## Performance Testing Checklist

### Pre-Release
- [ ] Run full load test suite
- [ ] Verify SLA compliance
- [ ] Check resource utilization
- [ ] Test peak capacity
- [ ] Validate performance regression testing

### Post-Release
- [ ] Monitor performance in production
- [ ] Compare with baseline
- [ ] Identify any degradation
- [ ] Implement quick fixes if needed
- [ ] Schedule full performance review

## Documentation References

- [Architecture Guide](../ARCHITECTURE.md)
- [Deployment Guide](../DEPLOYMENT/)
- [Operations Manual](../OPERATIONS/procedures.md)
- [Security Configuration](../SECURITY/configuration.md)

## Version History

| Version | Date | Author | Changes |
|---------|------|--------|---------|
| 1.0 | 2024-02-01 | Performance Team | Initial document |
| 1.1 | 2024-02-15 | Performance Team | Added tuning procedures |
| 1.2 | 2024-03-01 | Performance Team | Enhanced monitoring |