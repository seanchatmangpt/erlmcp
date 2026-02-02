# Troubleshooting Procedures

**Version**: 2.1.0
**Last Updated**: 2026-02-01
**Scope**: OTP upgrade issues and resolutions

---

## Table of Contents

1. [Overview](#overview)
2. [Installation Issues](#installation-issues)
3. [Compilation Errors](#compilation-errors)
4. [Runtime Errors](#runtime-errors)
5. [Performance Regressions](#performance-regressions)
6. [Diagnostic Tools](#diagnostic-tools)
7. [Emergency Procedures](#emergency-procedures)
8. [Known Issues](#known-issues)
9. [Workarounds](#workarounds)

---

## Overview

This guide covers common issues encountered during OTP 28 upgrades and their resolutions.

### Quick Diagnosis

```bash
# Check OTP version
erl -version

# Check erlmcp compilation
rebar3 compile

# Run diagnostic
make doctor

# Full validation
make check
```

---

## Installation Issues

### Issue: OTP 28 Installation Fails

**Symptoms**:
- `kerl build` fails with compilation errors
- Missing system dependencies
- Permission denied errors

**Diagnosis**:
```bash
# Check system prerequisites
./tools/check-prerequisites.sh

# Verify kerl installation
kerl version

# Check build logs
tail -f ~/.kerl/builds/28.3.1/otp_build_28.3.1.log
```

**Resolutions**:

**1. Missing System Dependencies**:
```bash
# Ubuntu/Debian
sudo apt-get install build-essential autoconf libncurses5-dev \
  libssl-dev libwxgtk3.0-gtk3-dev libgl1-mesa-dev \
  libglu1-mesa-dev libpng-dev libssh-dev unixodbc-dev \
  xsltproc fop libxml2-utils

# macOS
brew install autoconf openssl wxwidgets libxml2 fop

# RHEL/CentOS
sudo yum groupinstall "Development Tools"
sudo yum install ncurses-devel openssl-devel wxGTK-devel \
  mesa-libGL-devel mesa-libGLU-devel libpng-devel \
  libssh-devel unixODBC-devel
```

**2. Permission Issues**:
```bash
# Use sudo for system-wide install
sudo kerl install 28.3.1 /usr/local/otp-28.3.1

# Or install in user directory
kerl install 28.3.1 ~/.kerl/28.3.1
```

**3. Build Failure**:
```bash
# Clean build
rm -rf ~/.kerl/builds/28.3.1
kerl build 28.3.1 28.3.1

# With debug output
kerl build 28.3.1 28.3.1 -v
```

---

## Compilation Errors

### Issue: Module Not Found After Upgrade

**Symptoms**:
```
Error: module 'erlmcp_server' not found
```

**Diagnosis**:
```bash
# Check OTP version
erl -eval 'erlang:display(erlang:system_info(otp_release)).' -s init stop -noshell

# Check beam files
ls -la _build/default/lib/erlmcp_core/ebin/

# Verify includes
ls -la apps/erlmcp_core/include/
```

**Resolution**:

**1. Clean Rebuild**:
```bash
make clean
rebar3 compile
```

**2. Check Include Paths**:
```erlang
% In rebar.config
{erl_opts,
 [{i, "include"},
  {i, "apps/erlmcp_core/include"},
  {i, "apps/erlmcp_transports/include"},
  {i, "apps/erlmcp_observability/include"},
  {i, "apps/erlmcp_validation/include"}]}.
```

**3. Verify OTP Version**:
```bash
# Ensure OTP 28+ is active
erl -version

# Should show:
# Erlang/OTP 28 [erts-15.0]
```

### Issue: "function undefined" Error

**Symptoms**:
```
undefined function erlmcp_server:subscribe_resource/4
```

**Diagnosis**:
```bash
# Check function exports
grep -r "subscribe_resource" apps/erlmcp_core/src/
```

**Resolution**:

**1. API Changed**:
```erlang
% Old API (OTP 27-)
subscribe_resource(Uri, Callback) -> ...

% New API (OTP 28+)
subscribe_resource(Uri, Callback, Options) -> ...
subscribe_resource(Uri, Callback, Options, Timeout) -> ...
```

**2. Update Calls**:
```erlang
% Update your code
-ifdef(OTP_MODERN).
erlmcp_server:subscribe_resource(Uri, Callback, #{}, 5000).
-else.
erlmcp_server:subscribe_resource(Uri, Callback).
-endif.
```

### Issue: Dialyzer Warnings

**Symptoms**:
```
dialyzer: Warning: function has no local return
```

**Diagnosis**:
```bash
# Run Dialyzer
rebar3 dialyzer

# Check specific module
rebar3 dialyzer -M erlmcp_server
```

**Resolution**:

**1. Update Specs**:
```erlang
% Add missing spec
-spec subscribe_resource(uri(), callback(), options(), timeout()) ->
    {ok, subscription_id()} | {error, term()}.
```

**2. Fix Return Type**:
```erlang
% Ensure all code paths return correct type
subscribe_resource(Uri, Callback, Options, Timeout) ->
    try
        do_subscribe(Uri, Callback, Options),
        {ok, make_subscription_id()}
    catch
        error:_ -> {error, subscription_failed}
    end.
```

---

## Runtime Errors

### Issue: "badarg" After Upgrade

**Symptoms**:
```
Error: badarg
```

**Diagnosis**:
```bash
# Check crash dumps
ls -la log/crash.dump*

# Enable error logging
logger:set_primary_config(level, all).

# Reproduce in shell
rebar3 shell
1> erlmcp_server:start_link().
```

**Resolution**:

**1. PCRE2 Binary Format**:
```erlang
% OTP 28 uses PCRE2 - recompile regex patterns
% Old:
{ok, MP} = re:compile(Pattern).

% New:
{ok, MP} = re:compile(Pattern, [unicode]).
```

**2. Persistent Term Version**:
```bash
# Clear persistent terms
erl -noshell -s init stop -s erlmcp clear_persistent_terms

# Restart
make start
```

### Issue: Process Heap Overflow

**Symptoms**:
```
Error: process_heap_overflow
```

**Diagnosis**:
```bash
# Check process info
erlang:process_info(Pid, memory).
erlang:process_info(Pid, message_queue_len).
erlang:process_info(Pid, dictionary).
```

**Resolution**:

**1. Enable Hibernation**:
```erlang
-ifdef(OTP_MODERN).
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.
-endif.
```

**2. Increase Max Heap**:
```erlang
% In vm.args
+MH 4MB  % Max heap size per process
```

**3. Limit Message Queue**:
```erlang
% In gen_server
handle_info_msg(State) when length(State.queue) > 1000 ->
    {stop, message_queue_full, State};
handle_info_msg(State) ->
    {noreply, State}.
```

### Issue: Supervisor Restarts Loop

**Symptoms**:
```
supervisor: {shutdown, max_intensity}
```

**Diagnosis**:
```bash
# Check supervisor
observer:start().

% Or in shell
1> supervisor:which_children(erlmcp_sup).
```

**Resolution**:

**1. Check init/1 for Blocking**:
```erlang
% BAD - blocking in init/1
init([]) ->
    Result = heavy_sync_operation(),  % Blocks
    {ok, #{result => Result}}.

% GOOD - async with handle_continue
init([]) ->
    {ok, #{}, {continue, init_async}}.

handle_continue(init_async, State) ->
    Result = heavy_async_operation(),
    {noreply, State#{result => Result}}.
```

**2. Increase Restart Intensity**:
```erlang
% OTP 28+
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,  % Max restarts
                 period => 60},    % Time window (seconds)
    {ok, {SupFlags, ChildSpecs}}.
```

---

## Performance Regressions

### Issue: Throughput Decreased After Upgrade

**Symptoms**:
- Registry: 553K → 400K msg/s
- Queue: 971K → 700K msg/s

**Diagnosis**:
```bash
# Run benchmarks
make benchmark-full

# Check scheduler utilization
erlang:system_info(scheduler_wall_time).

# Check dirty schedulers
erlang:system_info(dirty_cpu_schedulers).
erlang:system_info(dirty_io_schedulers).
```

**Resolution**:

**1. Tune Dirty Schedulers**:
```erlang
% In vm.args
+SDio 128  % Increase dirty I/O schedulers
+SDcpu 128 % Increase dirty CPU schedulers
```

**2. Enable Priority Messages**:
```erlang
-ifdef(OTP_MODERN).
send_priority(Dest, Msg) ->
    erlang:send(Dest, Msg, [priority]).
-endif.
```

**3. Check VM Args**:
```bash
# Verify configuration
cat vm.args

# Should include for OTP 28:
+SDio 128
+SDcpu 128
+SP 8:128
```

### Issue: High Memory Usage

**Symptoms**:
- Memory increased 50% after upgrade
- GC pressure increased

**Diagnosis**:
```bash
# Check memory
erlang:memory(total).
erlang:memory(processes).
erlang:memory(system).
erlang:memory(ets).
```

**Resolution**:

**1. Enable Hibernation**:
```erlang
% Add hibernate/0 to gen_servers
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.
```

**2. Check for Binary Leaks**:
```bash
# Use recon
re:bin_leak(10).

% Or check manually
erlang:memory(binary).
erlang:memory(atom).
```

**3. Tune GC**:
```erlang
% In vm.args
+env.ERL_GC_MAXIMUM_MEMORY_SLOTS 1000000
```

---

## Diagnostic Tools

### Built-in Tools

**1. Observer**:
```bash
erl -s observer start
```

**2. Crash Dump Viewer**:
```bash
# Load crash dump
erl -s crashdump_viewer viewer "log/crash.dump.20250201"
```

**3. fprof Profiler**:
```bash
# Start profiling
erl -s fprof apply module function args

# Analyze
fprof:analyse().
fprof:profile().
```

### erlmcp Tools

**1. Doctor**:
```bash
make doctor
```

**2. Health Check**:
```bash
make health
```

**3. Metrics Dashboard**:
```bash
make dashboard
# Access at http://localhost:4000
```

### OTEL Tracing

**Enable Tracing**:
```erlang
application:set_env(opentelemetry, traces_exporter, otlp),
application:set_env(opentelemetry, otlp_endpoint, "http://localhost:4318"),
opentelemetry:initialize().
```

**View Traces**:
- Jaeger: http://localhost:16686
- Zipkin: http://localhost:9411

---

## Emergency Procedures

### Rollback to Previous OTP Version

**Trigger**: Critical bug, data corruption, complete failure

**Steps**:
```bash
# 1. Stop erlmcp
make stop

# 2. Deactivate current OTP
kerl deactivate

# 3. Activate previous OTP (e.g., 27.3)
. ~/.kerl/27.3/activate

# 4. Verify version
erl -version

# 5. Restore previous code
git checkout v2.0.x

# 6. Rebuild
make clean compile

# 7. Start
make start

# 8. Verify
make health
```

**Time**: < 5 minutes

### Emergency Mode (Limited Functionality)

**Trigger**: Can't rollback immediately, need partial service

**Steps**:
```bash
# 1. Disable non-critical features
export ERLMCP_DISABLE_OBSERVABILITY=true
export ERLMCP_DISABLE_VALIDATION=true

# 2. Start core only
make start-core-only

# 3. Monitor
tail -f log/erlang.log
```

### Data Recovery

**If Data Corruption**:
```bash
# 1. Stop immediately
make stop

# 2. Backup current data
cp -r data/ data.backup.$(date +%Y%m%d)

# 3. Check ETS tables
erl -noshell -s erlmcp_ets_dumper dump -s init stop

# 4. Restore from backup
cp -r data.backup.20250131/ data/

# 5. Start
make start
```

---

## Known Issues

### Issue 1: PCRE2 Pattern Incompatibility

**Affected**: OTP 27 → 28 upgrades

**Symptom**: Regex patterns fail after upgrade

**Workaround**:
```bash
# Clear compiled patterns
rm -rf _build/default/lib/*/ebin/*

# Rebuild
rebar3 compile
```

**Fix**: Recompile all regex patterns with PCRE2

### Issue 2: Persistent Term Version Mismatch

**Affected**: Direct 26 → 28 upgrades

**Symptom**: `persistent_term:get` returns unexpected values

**Workaround**:
```bash
# Clear persistent terms
erl -noshell -eval 'persistent_term:erase()' -s init stop

# Restart
make restart
```

**Fix**: Clear persistent terms before upgrade

### Issue 3: NIF Compatibility

**Affected**: Systems with custom NIFs

**Symptom**: `load_nif` fails

**Workaround**: Recompile NIFs with OTP 28 headers

**Fix**: Update NIF code for OTP 28

---

## Workarounds

### Temporary Disable Priority Messages

**If Priority Messages Cause Issues**:
```erlang
% In erlmcp_server.erl
% Comment out priority message sending
% -ifdef(OTP_MODERN).
% erlang:send(Dest, Msg, [priority]).
% -endif.

% Use normal send instead
erlang:send(Dest, Msg).
```

### Disable Hibernation

**If Hibernation Causes Wake-up Latency**:
```erlang
% Replace hibernate with timeout
handle_cast(_Msg, State) ->
    {noreply, State, 5000}.  % Instead of hibernate
```

### Use Legacy Regex

**If PCRE2 Issues**:
```erlang
% Force PCRE1 (not recommended)
% Set environment variable
export ERL_LIBS="/usr/local/lib/erlang/lib"

% Recompile with legacy regex
rebar3 compile -Dlegacy_regex
```

---

## Quick Reference

### Common Error Messages

| Error | Cause | Resolution |
|-------|-------|------------|
| `badarg` | PCRE2 binary format | Recompile regex patterns |
| `module not found` | Include paths missing | Check `rebar.config` |
| `function undefined` | API changed | Update function calls |
| `process_heap_overflow` | Memory leak | Enable hibernation |
| `supervisor restart` | init/1 blocking | Use `handle_continue` |

### Diagnostic Commands

```bash
# Full diagnosis
make doctor

# Health check
make health

# Crash dump analysis
erl -s crashdump_viewer viewer "log/crash.dump"

# Memory analysis
erl -s recon bin_leak 10

# Profiling
erl -s fprof apply module function args
```

### Emergency Rollback

```bash
# Quick rollback (5 minutes)
make stop
kerl deactivate
. ~/.kerl/27.3/activate
git checkout v2.0.x
make clean compile
make start
```

---

## Getting Help

**Documentation**: https://erlmcp.org/docs
**Issues**: https://github.com/seanchatmangpt/erlmcp/issues
**Discussions**: https://github.com/seanchatmangpt/erlmcp/discussions
**Email**: support@erlmcp.org

**When Reporting Issues**:
1. OTP version: `erl -version`
2. erlmcp version: `git describe --tags`
3. Error message: Full stack trace
4. Steps to reproduce: Detailed commands
5. Crash dump: If available

---

**Document Version**: 1.0
**Author**: SPARC Orchestrator
**Review Date**: 2026-03-01
