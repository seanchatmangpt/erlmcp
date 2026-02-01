# erlmcp Diagnostics & Profiling Guide

Advanced debugging tools for diagnosing issues, profiling performance, tracing execution, and monitoring running systems.

**Table of Contents**:
- [Doctor Command](#doctor-command)
- [Profiling Tools](#profiling-tools)
- [Tracing](#tracing)
- [Watch/Monitor](#watchmonitor)
- [Understanding Output](#understanding-output)
- [Integration with External Tools](#integration-with-external-tools)
- [Troubleshooting Guide](#troubleshooting-guide)

## Doctor Command

The `erlmcp doctor` command performs a comprehensive host readiness check.

### Basic usage

```bash
erlmcp doctor
```

### Output interpretation

**READY status** - All systems operational:
```
======================================================================
ERLMCP DOCTOR - Host Readiness Check
======================================================================
ℹ Running diagnostics...

✓ Erlang/OTP 28.3.1 - OK
✓ rebar3 3.22.0 - OK
✓ Compilation - OK
✓ Memory (16GB) - OK
✓ Disk space (250GB) - OK
✓ STDIO transport - OK
✓ HTTP transport - OK
✓ SSL/TLS - OK (OpenSSL 3.0)
✓ IPv6 - OK

Status: READY - All checks passed
```

**WARNING status** - Non-critical failures:
```
Status: WARNING - IPv6 not available (optional feature)

This means:
- System can still operate normally
- Some features may be unavailable
- Action is optional (but recommended)
```

**FAILED status** - Critical issue:
```
✗ Erlang version - FAILED
  Expected: OTP 28.3.1+
  Found: OTP 27.3

Status: FAILED - Cannot proceed

Action required:
1. Upgrade Erlang/OTP to 28.3.1 or later
2. Run: erl -version  (to verify upgrade)
3. Try: erlmcp doctor (to re-check)
```

### JSON output for scripting

```bash
erlmcp doctor-json
```

Output:
```json
{
  "status": "ready",
  "timestamp": "2026-02-01T12:00:00Z",
  "checks": [
    {
      "name": "erlang_version",
      "status": "pass",
      "value": "28.3.1",
      "required": "28.3.1+"
    },
    {
      "name": "rebar3",
      "status": "pass",
      "value": "3.22.0"
    },
    {
      "name": "memory",
      "status": "pass",
      "available_gb": 16,
      "required_gb": 1
    },
    {
      "name": "compilation",
      "status": "pass",
      "duration_ms": 1234
    }
  ],
  "summary": {
    "passed": 9,
    "warned": 0,
    "failed": 0
  }
}
```

### Using in scripts

```bash
#!/bin/bash

# Check if system is ready
if erlmcp doctor | grep -q "READY"; then
  echo "System ready for production"
  erlmcp start
else
  echo "System not ready, fixing issues..."
  exit 1
fi
```

---

## Profiling Tools

### CPU profiling with fprof

```erlang
% Start Erlang shell
erlmcp start

% Start profiling
1> fprof:start().
{ok, <0.123.0>}

% Trace specific code
2> fprof:trace(start, call, [
2>   {erlmcp_server, '_', '_'},
2>   {erlmcp_client, '_', '_'}
2> ]).
ok

% Run your code to profile
3> {ok, Server} = erlmcp_server:start_link(test, #{}).
{ok,<0.124.0>}

% Stop profiling
4> fprof:stop().
profiling_stopped

% Analyze results
5> fprof:analyse([{dest, "fprof.analysis"}]).
ok

% View results
6> q().
```

Then examine `fprof.analysis` file:
```
FUNCTION CALL COUNT [microseconds]
---------------------------------------
erlmcp_server:start_link/2      1        1234
erlmcp_server:init/2            1        567
...
```

### Memory profiling

```erlang
% In REPL
1> eprof:start().
{ok,<0.100.0>}

2> eprof:start_profiling([self()]).
profiling

3> % Run code here
   {ok, Pid} = erlmcp_server:start_link(test, #{}).

4> eprof:stop_profiling().
profiling_stopped

5> eprof:analyze().
FUNCTION CALLED RUNTIME [microseconds] % of total
erlang:apply/2        234      156         0.5%
erlmcp_server:init    1        1234       4.2%
...
```

### Heap analysis

```erlang
% Check heap size before
1> erlang:memory(heap).
12345678

% Run code
2> {ok, _} = erlmcp_server:start_link(test, #{}).

% Check after
3> erlang:memory(heap).
12456789

% Difference
4> 12456789 - 12345678.
111111  % bytes allocated

% Get percentage
5> Mem = erlang:memory(),
5> (111111 / maps:get(total, Mem)) * 100.
0.234  % percent of total memory
```

### Real-time memory monitoring

```bash
# In terminal (not Erlang)
watch -n 1 'erl -noshell -eval "io:format(\"~p~n\", [erlang:memory()])." -s init stop'
```

This updates every second showing:
```
[{total,123456789},
 {processes,23456789},
 {processes_used,21000000},
 {system,100000000},
 {atom,1234567},
 {atom_used,1100000},
 {binary,234567},
 {code,5000000},
 {ets,789}]
```

---

## Tracing

### Interactive tracing in REPL

**Basic trace setup**:
```erlang
% Enable tracing
1> dbg:tracer().
{ok,<0.100.0>}

% Trace all calls to specific module
2> dbg:tpl(erlmcp_server, []),
   dbg:tp(erlmcp_server, []),
   dbg:p(all, c).
% c = call tracing

% Now execute code - you'll see trace output
3> {ok, Pid} = erlmcp_server:start_link(test, #{}).
(<0.100.0>) <0.201.0> ! {trace, <0.100.0>, call, {erlmcp_server, start_link, [test, #{}]}}
{ok, <0.201.0>}

% Stop tracing
4> dbg:stop_clear().
ok
```

**Trace output format**:
```
(NodeName) CallerId FunctionTrace
```

Example trace output breakdown:
```
(<0.100.0>) <0.201.0> ! {trace, <0.100.0>, call, {erlmcp_server, init, [test, {}]}}
|            |         |  |     |            |      |                    |
Node         Caller    Trace  PID   Module      Function              Arguments
```

### Advanced tracing - file output

```erlang
% Trace to file instead of console
1> dbg:tracer(file, "erlmcp_trace.log").
{ok, <0.100.0>}

% Set up patterns
2> dbg:tpl(erlmcp_server, []),
   dbg:tpl(erlmcp_client, []),
   dbg:p(all, c).

% Run code (output goes to file)
3> {ok, Server} = erlmcp_server:start_link(test, #{}).

% Stop tracing
4> dbg:stop_clear().

% Analyze trace file
5> q().

% Now in shell, view the trace
$ erlmcp_trace.log | head -100
```

### Match specs for selective tracing

```erlang
% Trace only errors
1> MatchSpec = [{
1>   {'$0', '_', error, '_'},
1>   [],
1>   [write]
1> }].
[{'$0','_',error,'_'}, [], [write]]

2> dbg:tracer().
{ok, <0.100.0>}

3> dbg:tpl(erlmcp_server, MatchSpec),
   dbg:p(all, c).

% Only matching traces appear
4> % Run code...
```

### Tracing with prematched functions

```erlang
% Trace specific function with specific arguments
1> dbg:tracer().
{ok, <0.100.0>}

% Trace start_link with any arguments
2> dbg:tp(erlmcp_server, start_link, '_').

% Trace return values too
3> dbg:p(all, [call, return_to]).

% Run code
4> {ok, Pid} = erlmcp_server:start_link(test, #{}).

% See both call and return
(<0.100.0>) <0.201.0> ! {trace, <0.100.0>, call, {erlmcp_server, start_link, [test, {}]}}
(<0.100.0>) <0.201.0> ! {trace, <0.100.0>, return_to, {erlang, apply, 2}}
```

---

## Watch/Monitor

### Real-time process monitoring

```erlang
% In Erlang shell
1> erlang:processes().
[<0.0.0>, <0.1.0>, <0.2.0>, ...]

% Monitor specific process
2> Pid = erlmcp_registry:lookup(my_server).
<0.123.0>

3> erlang:process_info(Pid, [
3>   reductions,
3>   memory,
3>   message_queue_len,
3>   heap_size,
3>   total_heap_size
3> ]).
[{reductions,12345},
 {memory,65536},
 {message_queue_len,0},
 {heap_size,4096},
 {total_heap_size,16384}]
```

### Continuous monitoring script

```erlang
% Erlang script: monitor.erl
monitor_loop(Pid, Interval) ->
    Info = erlang:process_info(Pid, [
        memory,
        message_queue_len,
        reductions
    ]),
    io:format("~w: ~w~n", [erlang:now(), Info]),
    timer:sleep(Interval),
    monitor_loop(Pid, Interval).

start_monitor(PidName, Interval) ->
    Pid = erlmcp_registry:lookup(PidName),
    monitor_loop(Pid, Interval).
```

Run from shell:
```erlang
1> c(monitor).
{ok, monitor}

2> monitor:start_monitor(my_server, 1000).
% Outputs process info every 1000ms
```

### Memory trend monitoring

```erlang
% Track memory growth over time
1> Start = erlang:memory(processes).
12345678

2> {ok, _} = erlmcp_server:start_link(test, #{}).

3> erlang:memory(processes).
12456789

4> Growth = erlang:memory(processes) - Start.
111111

5> % After cleanup
6> erlang:garbage_collect().
true

7> erlang:memory(processes).
12200000

% Memory returned to system
```

---

## Understanding Output

### Analyzing doctor output

**Memory interpretation**:
```
✓ Memory (16GB) - OK
  Available: 16 GB
  Required: 1 GB
  Free percentage: 87%
```

This means:
- Total system RAM: 16 GB
- Required by erlmcp: 1 GB
- Safe to proceed

**Disk space interpretation**:
```
✓ Disk space (250GB) - OK
  Available: 250 GB
  Required: 5 GB
  Free percentage: 92%
```

Safe if:
- Available >= 5 GB (minimum)
- Free percentage >= 10%

### Analyzing performance output

**Throughput metrics**:
```
Throughput: 159235 ops/sec
Expected:   >150000 ops/sec
Status:     PASS ✓
```

Interpretation:
- Excellent: >150K ops/s
- Good: 100-150K ops/s
- Acceptable: 50-100K ops/s
- Poor: <50K ops/s

**Latency metrics**:
```
Per-operation: 0.006 ms
Expected:      <0.01 ms
Status:        PASS ✓
```

Interpretation:
- Excellent: <0.005 ms
- Good: 0.005-0.01 ms
- Acceptable: 0.01-0.1 ms
- Poor: >0.1 ms

### Interpreting trace output

```
(<0.100.0>) <0.201.0> ! {trace, <0.100.0>, call, {erlmcp_server, init, [...]}}
```

Breaking down:
- `(<0.100.0>)` - Node name
- `<0.201.0>` - Process ID being traced
- `!` - Message operator
- `trace` - Trace type
- `call` - Call or return
- `{erlmcp_server, init, [...]}` - Function and args

Common trace types:
- `call` - Function called
- `return_to` - Function returned
- `spawn` - Process created
- `exit` - Process exited
- `send` - Message sent

---

## Integration with External Tools

### Flame graphs

**Generate data with perf**:
```bash
# On Linux with perf support
erlmcp start

# In another terminal
perf record -F 99 -p $(pgrep erl) -g -- sleep 30
perf script | stackcollapse-perf.pl | flamegraph.pl > erlmcp.svg
```

Then open `erlmcp.svg` in browser.

### Jaeger distributed tracing

erlmcp supports OpenTelemetry which exports to Jaeger:

```erlang
% In REPL
1> {ok, _} = erlmcp_otel:start([
1>   {exporter, jaeger},
1>   {jaeger_endpoint, "http://localhost:14268/api/traces"}
1> ]).

% Now all operations are traced to Jaeger
2> {ok, _} = erlmcp_server:start_link(test, #{}).

% View traces at http://localhost:16686
```

### Prometheus metrics

```bash
# From docker-compose.yml
docker-compose -f docker-compose.yml up -d prometheus grafana

# Access Prometheus at http://localhost:9090
# Access Grafana at http://localhost:3000

# erlmcp metrics are exposed at http://localhost:8888/metrics
```

### sysstat integration

```bash
# On Linux, use iostat for I/O
iostat -x 1 | grep -E 'erlang|erl'

# Use vmstat for memory
vmstat 1 5 | tail

# Output shows memory/CPU usage
```

---

## Troubleshooting Guide

### Problem: "Doctor passes but system performs poorly"

```bash
# 1. Check active processes
erlmcp start
1> length(erlang:processes()).
42  % Normal

# 2. Check message queue depth
2> Pid = erlmcp_registry:lookup(my_server),
2> erlang:process_info(Pid, message_queue_len).
{message_queue_len, 0}  % Normal

# 3. Check GC overhead
3> erlang:garbage_collect().
true

# 4. Profile CPU
4> fprof:start().
   % Run code...
   fprof:stop().
```

### Problem: "Memory grows unbounded"

```erlang
% Check what's consuming memory
1> ets:all().  % Check ETS tables
[erlmcp_cache, erlmcp_sessions, ...]

2> lists:foreach(fun(Table) ->
2>   Info = ets:info(Table),
2>   Size = proplists:get_value(size, Info),
2>   Memory = proplists:get_value(memory, Info),
2>   io:format("~w: ~w entries, ~w bytes~n", [Table, Size, Memory])
2> end, ets:all()).

erlmcp_cache: 50000 entries, 4000000 bytes
erlmcp_sessions: 1000 entries, 200000 bytes

% If cache is huge, clear it
3> ets:delete_all_objects(erlmcp_cache).
true

% Check memory now
4> erlang:memory(processes).
```

### Problem: "Some processes crash frequently"

```erlang
% Get crash reports
1> erlang:get_module_info(erlmcp_crash_handler, exports).

% Or check supervisor
2> erlmcp_server_sup:which_children().
[{undefined, <0.100.0>, worker, [erlmcp_server]}]

% Check why restarted
3> supervisor:count_children(erlmcp_server_sup).
#{active => 1, specs => 1, supervisors => 0, workers => 1}

% For detailed crash info, check logs
% (See logging configuration)
```

### Problem: "Throughput lower than expected"

```bash
# 1. Check system load
top
# If load > CPU count, system is saturated

# 2. Check thermal condition
sensors  # On Linux

# 3. Check for other processes
ps aux | grep -v erlang

# 4. Run test again after closing other apps
erlmcp test-100k

# 5. Check baseline
erlmcp benchmark  # Compare with expected values
```

---

## Quick Reference

### Common diagnostic commands

| Task | Command |
|------|---------|
| System readiness | `erlmcp doctor` |
| Performance test | `erlmcp test-100k` |
| Full benchmarks | `erlmcp benchmark` |
| Process list | `1> i().` (in REPL) |
| Memory usage | `1> erlang:memory().` |
| Specific process info | `1> erlang:process_info(Pid).` |
| Trace functions | `1> dbg:tracer(), dbg:tpl(...).` |
| CPU profile | `1> fprof:start(), ..., fprof:analyse().` |

---

## See Also

- [CLI_REFERENCE.md](CLI_REFERENCE.md) - CLI command reference
- [CLI_INTERACTIVE_GUIDE.md](CLI_INTERACTIVE_GUIDE.md) - Interactive REPL guide
- [Erlang Debugging Guide](https://erlang.org/doc/guides/debugging.html)
- [OTP Reference Manual](https://erlang.org/doc/)
