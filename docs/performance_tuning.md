# ErlMCP Performance Tuning Guide

## Overview

This guide provides comprehensive recommendations for optimizing the performance of the ErlMCP (Erlang Message Control Protocol) system, targeting high-throughput scenarios with minimal latency and optimal resource utilization.

## Performance Targets

### Primary Objectives
- **Throughput**: 10,000+ messages/second per transport
- **Latency**: <1ms P99 for local operations
- **Memory**: <50MB growth under sustained load
- **CPU**: <80% utilization under peak load
- **Availability**: 99.9% uptime with graceful degradation

## System-Level Optimizations

### Erlang VM Configuration

#### VM Arguments (vm.args)
```bash
# Memory and Process Limits
+P 1048576                    # Max processes (1M)
+Q 65536                     # Max ports
+A 30                        # Async thread pool size
+K true                      # Enable kernel polling

# Scheduler Configuration
+S 4:4                       # 4 schedulers, 4 cores
+sub true                    # Enable scheduler bind type
+sbt db                      # Scheduler bind type: database bound

# Memory Management
+hmbs 1048576               # Heap memory block size (1MB)
+hmqd off_heap              # Message queue data off heap
+hms 2048                   # Heap memory size (2MB initial)
+hmaxs 8192                 # Max heap memory size (8MB)

# Garbage Collection
+MBas aobf                  # Multi-block carrier allocation strategy
+MBlmbcs 512                # Largest multi-block carrier size
+MBsbcs 64                  # Smallest multi-block carrier size

# Distribution
+K true                     # Kernel polling
+A 30                       # Async thread pool
```

#### sys.config Performance Settings
```erlang
[
 {kernel, [
   %% Network buffer sizes
   {inet_default_connect_options, [{sndbuf, 131072}, {recbuf, 131072}]},
   {inet_default_listen_options, [{backlog, 4096}, {sndbuf, 131072}, {recbuf, 131072}]},

   %% Distribution buffer size
   {dist_buf_busy_limit, 16777216}  % 16MB
 ]},

 {erlmcp, [
   %% Registry optimization
   {registry_type, optimized},
   {enable_route_cache, true},
   {cache_ttl_ms, 30000},
   {max_cache_entries, 10000},

   %% Transport optimization
   {transport_buffer_size, 131072},  % 128KB
   {max_message_size, 1048576},      % 1MB
   {connection_pool_size, 20},

   %% Performance monitoring
   {enable_metrics, true},
   {metrics_interval, 5000}
 ]},

 {logger, [
   %% Reduce logging overhead in production
   {handler, default, logger_std_h, #{
     level => warning,
     filters => [
       {progress, {fun logger_filters:progress/2, stop}}
     ]
   }}
 ]}
].
```

### Operating System Tuning

#### Linux Kernel Parameters
```bash
# Network Performance
net.core.somaxconn = 4096
net.core.netdev_max_backlog = 5000
net.core.rmem_max = 33554432
net.core.wmem_max = 33554432
net.ipv4.tcp_rmem = 4096 131072 33554432
net.ipv4.tcp_wmem = 4096 131072 33554432

# File Descriptor Limits
fs.file-max = 1048576
fs.nr_open = 1048576

# Process Limits
kernel.pid_max = 4194304
vm.max_map_count = 1048576

# Memory Management
vm.swappiness = 1
vm.dirty_ratio = 10
vm.dirty_background_ratio = 5
```

#### ulimits Configuration
```bash
# /etc/security/limits.conf
*    soft nofile  1048576
*    hard nofile  1048576
*    soft nproc   1048576
*    hard nproc   1048576
```

## Application-Level Optimizations

### Registry Optimization

#### Use Optimized Registry
```erlang
% In your application startup
{ok, _} = erlmcp_registry_optimized:start_link(#{
    enable_route_cache => true,
    cache_ttl_ms => 30000,
    max_cache_entries => 10000,
    enable_stats_collection => true,
    optimize_ets_tables => true,
    enable_concurrent_access => true
}).
```

#### ETS Table Configuration
```erlang
% Custom ETS configuration for high performance
ETSOptions = [
    set,                        % Hash table for O(1) lookups
    public,                     % Allow concurrent access
    {read_concurrency, true},   % Optimize for concurrent reads
    {write_concurrency, true},  % Optimize for concurrent writes
    compressed                  % Compress data to save memory
].
```

### Transport Optimization

#### STDIO Transport
```erlang
% Optimized STDIO configuration
StdioConfig = #{
    type => stdio,
    buffer_size => 131072,      % 128KB buffer
    read_timeout => 5000,       % 5 second timeout
    write_timeout => 5000,
    enable_flow_control => true,
    batch_messages => true,
    batch_size => 100
}.
```

#### TCP Transport
```erlang
% High-performance TCP configuration
TcpConfig = #{
    type => tcp,
    host => "0.0.0.0",
    port => 8080,

    % Socket options for performance
    socket_opts => [
        binary,
        {active, true},
        {packet, 4},
        {nodelay, true},
        {keepalive, true},
        {reuseaddr, true},
        {sndbuf, 131072},       % 128KB send buffer
        {recbuf, 131072},       % 128KB receive buffer
        {backlog, 4096}         % Connection backlog
    ],

    % Connection pooling
    connection_pool_size => 20,
    max_connections => 10000,

    % Timeouts
    connect_timeout => 5000,
    send_timeout => 30000,

    % Keep-alive settings
    keepalive_interval => 30000,
    keepalive_probes => 3,
    keepalive_timeout => 9000
}.
```

#### HTTP Transport
```erlang
% High-throughput HTTP configuration
HttpConfig = #{
    type => http,
    url => "http://localhost:8000/mcp",
    method => post,

    % Connection management
    connection_timeout => 5000,
    max_connections => 100,
    max_pipeline_size => 10,

    % HTTP/2 optimization
    http_version => 'HTTP/2',
    enable_compression => true,

    % Headers
    headers => #{
        <<"Connection">> => <<"keep-alive">>,
        <<"Keep-Alive">> => <<"timeout=30, max=1000">>,
        <<"Content-Type">> => <<"application/json">>
    },

    % Timeouts
    timeout => 30000,
    idle_timeout => 300000      % 5 minutes
}.
```

### Message Processing Optimization

#### Batch Processing
```erlang
% Implement message batching for high throughput
process_message_batch(Messages) ->
    BatchSize = 100,
    BatchTimeout = 10,  % 10ms

    Batches = batch_messages(Messages, BatchSize),
    lists:foreach(fun process_batch/1, Batches).

batch_messages(Messages, BatchSize) ->
    batch_messages(Messages, BatchSize, []).

batch_messages([], _BatchSize, Batches) ->
    lists:reverse(Batches);
batch_messages(Messages, BatchSize, Batches) ->
    {Batch, Remaining} = lists:split(min(BatchSize, length(Messages)), Messages),
    batch_messages(Remaining, BatchSize, [Batch | Batches]).
```

#### Memory-Efficient Message Handling
```erlang
% Use binary data and avoid copying
handle_message(<<Size:32, Data:Size/binary, Rest/binary>>) ->
    % Process message without copying data
    case process_message_data(Data) of
        {ok, Result} ->
            send_response(Result),
            handle_message(Rest);
        {error, Reason} ->
            logger:warning("Message processing failed: ~p", [Reason]),
            handle_message(Rest)
    end.
```

### Process Pool Optimization

#### Worker Pool Configuration
```erlang
% Configure process pools for optimal concurrency
start_worker_pool() ->
    PoolConfig = #{
        name => mcp_worker_pool,
        worker_count => erlang:system_info(schedulers) * 2,
        max_overflow => 10,
        strategy => fifo,

        % Worker configuration
        worker_args => #{
            hibernate_after => 5000,
            max_message_queue_len => 10000
        }
    },

    {ok, _} = poolboy:start_link(PoolConfig, []).
```

#### Async Processing
```erlang
% Use async processing for non-blocking operations
handle_async_message(Message) ->
    poolboy:transaction(mcp_worker_pool, fun(Worker) ->
        gen_server:cast(Worker, {process_async, Message})
    end).
```

## Memory Optimization

### Garbage Collection Tuning

#### Process-Level GC Settings
```erlang
% Optimize garbage collection for high-throughput processes
start_optimized_process() ->
    spawn_opt(fun process_loop/0, [
        {fullsweep_after, 10},      % Full GC every 10 minor GCs
        {min_heap_size, 2048},      % 2KB initial heap
        {min_bin_vheap_size, 4096}  % 4KB binary heap
    ]).
```

#### Memory Pool Management
```erlang
% Implement memory pools for frequently allocated objects
-module(memory_pool).
-export([get_buffer/1, return_buffer/2]).

get_buffer(Size) ->
    case ets:lookup(buffer_pool, Size) of
        [{Size, Buffer}] ->
            ets:delete_object(buffer_pool, {Size, Buffer}),
            Buffer;
        [] ->
            <<0:(Size*8)>>
    end.

return_buffer(Size, Buffer) ->
    ets:insert(buffer_pool, {Size, Buffer}).
```

### Data Structure Optimization

#### Use Appropriate Data Structures
```erlang
% Maps for small datasets (< 32 keys)
small_config() -> #{key1 => value1, key2 => value2}.

% ETS for larger datasets
large_dataset() ->
    TableId = ets:new(large_data, [set, {read_concurrency, true}]),
    % Populate table
    TableId.

% Binaries for large data
large_binary_data(Data) when is_list(Data) ->
    iolist_to_binary(Data).
```

## Network Optimization

### TCP Socket Optimization

#### Socket Buffer Sizing
```erlang
% Calculate optimal buffer sizes
calculate_buffer_size(Bandwidth, Latency) ->
    % Buffer size = Bandwidth * Latency * 2
    BufferSize = Bandwidth * Latency * 2,
    min(max(BufferSize, 64 * 1024), 16 * 1024 * 1024).
```

#### Connection Management
```erlang
% Implement connection pooling
-module(connection_pool).
-export([get_connection/1, return_connection/2]).

get_connection(HostPort) ->
    case ets:lookup(connection_pool, HostPort) of
        [{HostPort, Socket}] ->
            ets:delete_object(connection_pool, {HostPort, Socket}),
            {ok, Socket};
        [] ->
            establish_connection(HostPort)
    end.

return_connection(HostPort, Socket) ->
    case inet:getstat(Socket, [send_oct, recv_oct]) of
        {ok, _Stats} ->
            ets:insert(connection_pool, {HostPort, Socket});
        {error, _} ->
            gen_tcp:close(Socket)
    end.
```

### HTTP Optimization

#### Connection Keep-Alive
```erlang
% HTTP client with keep-alive and pipelining
http_request_optimized(Url, Data) ->
    httpc:request(post,
        {Url,
         [{"Connection", "keep-alive"},
          {"Content-Type", "application/json"}],
         "application/json",
         Data},
        [{timeout, 30000},
         {connect_timeout, 5000}],
        [{sync, true}]).
```

## Monitoring and Profiling

### Performance Metrics Collection

#### Built-in Metrics
```erlang
% Enable comprehensive metrics
start_metrics_collection() ->
    Metrics = [
        {registry, throughput},
        {registry, latency_p99},
        {transport, connection_count},
        {transport, message_rate},
        {system, memory_usage},
        {system, cpu_usage}
    ],

    lists:foreach(fun({Module, Metric}) ->
        erlmcp_metrics:enable(Module, Metric)
    end, Metrics).
```

#### Custom Metrics
```erlang
% Custom performance counters
-module(perf_counters).
-export([increment/1, get_value/1]).

increment(Counter) ->
    ets:update_counter(perf_counters, Counter, 1).

get_value(Counter) ->
    case ets:lookup(perf_counters, Counter) of
        [{Counter, Value}] -> Value;
        [] -> 0
    end.
```

### Profiling Tools

#### Built-in Profiling
```erlang
% Profile hot code paths
profile_hot_path() ->
    fprof:start(),
    fprof:trace(start),

    % Run hot code
    hot_code_path(),

    fprof:trace(stop),
    fprof:profile(),
    fprof:analyse([{dest, "profile_results.txt"}]).
```

#### Memory Profiling
```erlang
% Monitor memory usage
monitor_memory() ->
    spawn(fun() ->
        timer:send_interval(1000, self(), check_memory),
        memory_monitor_loop()
    end).

memory_monitor_loop() ->
    receive
        check_memory ->
            Memory = erlang:memory(),
            case maps:get(total, Memory) > 1024 * 1024 * 1024 of  % 1GB
                true ->
                    logger:warning("High memory usage: ~p MB",
                                  [maps:get(total, Memory) div (1024*1024)]);
                false ->
                    ok
            end,
            memory_monitor_loop()
    end.
```

## Benchmarking and Testing

### Performance Test Suite

#### Automated Benchmarks
```bash
# Run comprehensive performance benchmarks
rebar3 ct --suite=test/erlmcp_performance_SUITE.erl

# Run specific benchmark
rebar3 ct --suite=test/erlmcp_performance_SUITE.erl --case=throughput_benchmark

# Generate performance report
rebar3 escriptize
./_build/default/bin/erlmcp_benchmark --full-report
```

#### Load Testing
```erlang
% Stress test with concurrent clients
run_load_test(ClientCount, Duration) ->
    Clients = [spawn_link(fun() -> client_loop(Duration) end)
              || _ <- lists:seq(1, ClientCount)],

    timer:sleep(Duration),

    lists:foreach(fun(Pid) -> exit(Pid, normal) end, Clients).
```

### Performance Regression Detection

#### Baseline Comparison
```erlang
% Create performance baseline
create_baseline() ->
    Results = erlmcp_benchmark:run_full_benchmark_suite(),
    erlmcp_benchmark:save_baseline("production_baseline", Results).

% Check for regressions
check_regressions() ->
    {ok, BaselineResults} = erlmcp_benchmark:load_baseline("production_baseline"),
    CurrentResults = erlmcp_benchmark:run_ci_benchmarks(),

    Regressions = erlmcp_benchmark:detect_performance_regression(
        BaselineResults, CurrentResults),

    case Regressions of
        [] -> {ok, no_regressions};
        _ -> {warning, Regressions}
    end.
```

## Production Deployment

### Configuration Management

#### Environment-Specific Settings
```erlang
% production.config
[
 {erlmcp, [
   % High-performance production settings
   {registry_type, optimized},
   {enable_route_cache, true},
   {cache_ttl_ms, 60000},
   {max_cache_entries, 100000},

   % Monitoring
   {enable_metrics, true},
   {metrics_backend, prometheus},
   {health_check_interval, 30000},

   % Logging
   {log_level, warning},
   {enable_performance_logs, true}
 ]},

 {kernel, [
   {logger_level, warning}
 ]}
].
```

#### Dynamic Configuration Updates
```erlang
% Update configuration without restart
update_performance_config() ->
    NewConfig = #{
        cache_ttl_ms => 120000,  % Increase cache TTL
        max_cache_entries => 200000
    },

    erlmcp_registry_optimized:update_config(NewConfig).
```

### Monitoring Integration

#### Prometheus Metrics
```erlang
% Export metrics to Prometheus
setup_prometheus_metrics() ->
    prometheus_counter:declare([
        {name, erlmcp_messages_total},
        {help, "Total number of MCP messages processed"}
    ]),

    prometheus_histogram:declare([
        {name, erlmcp_message_duration_seconds},
        {buckets, [0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0]},
        {help, "Message processing duration"}
    ]).
```

#### Health Checks
```erlang
% Health check endpoint
health_check() ->
    Checks = [
        check_registry_health(),
        check_transport_health(),
        check_memory_usage(),
        check_process_count()
    ],

    case lists:all(fun(Check) -> Check =:= ok end, Checks) of
        true -> {ok, healthy};
        false -> {error, unhealthy}
    end.
```

## Troubleshooting

### Common Performance Issues

#### High Latency
1. Check network configuration and socket buffers
2. Profile message serialization/deserialization
3. Verify GC settings and memory usage
4. Check for blocking operations

#### Low Throughput
1. Increase process pool size
2. Enable message batching
3. Optimize ETS table configuration
4. Check for bottlenecks in hot paths

#### Memory Leaks
1. Monitor process memory usage
2. Check for unclosed resources
3. Verify binary reference handling
4. Profile garbage collection behavior

### Debugging Tools

#### System Information
```erlang
% Get comprehensive system info
get_system_info() ->
    #{
        memory => erlang:memory(),
        process_count => erlang:system_info(process_count),
        port_count => erlang:system_info(port_count),
        scheduler_usage => scheduler_wall_time_usage(),
        ets_tables => ets:all(),
        registered_processes => registered()
    }.
```

#### Performance Analysis
```erlang
% Analyze performance bottlenecks
analyze_performance() ->
    % CPU usage by process
    ProcessInfo = [{Pid, process_info(Pid, [memory, message_queue_len, reductions])}
                  || Pid <- processes()],

    % Sort by memory usage
    SortedByMemory = lists:sort(fun({_, InfoA}, {_, InfoB}) ->
        {memory, MemA} = lists:keyfind(memory, 1, InfoA),
        {memory, MemB} = lists:keyfind(memory, 1, InfoB),
        MemA > MemB
    end, ProcessInfo),

    lists:sublist(SortedByMemory, 10).  % Top 10 memory consumers
```

## Conclusion

This performance tuning guide provides comprehensive optimization strategies for ErlMCP systems. Regular monitoring, profiling, and benchmarking are essential for maintaining optimal performance in production environments.

For specific performance issues or advanced optimization scenarios, consider:

1. **Custom profiling** with tools like `fprof`, `eprof`, or `observer`
2. **Load testing** with realistic traffic patterns
3. **A/B testing** of configuration changes
4. **Continuous monitoring** with automated alerting

Regular performance reviews and optimization cycles ensure your ErlMCP deployment maintains peak efficiency as your system scales.
