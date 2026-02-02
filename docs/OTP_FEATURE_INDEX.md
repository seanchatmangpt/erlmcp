# OTP Feature Index for erlmcp

**Last Updated**: 2026-02-02
**Scope**: Erlang/OTP 26-28 features for erlmcp development

---

## Quick Navigation

| Category | Features | Link |
|----------|----------|------|
| **Core** | JSON, Types, Regex | [Core Features](#core-features) |
| **Performance** | Hibernation, Iterators, Compilation | [Performance](#performance-features) |
| **Messaging** | Priority, Notifications | [Messaging](#messaging-features) |
| **Networking** | TLS, TCP, Socket | [Networking](#networking-features) |
| **Observability** | Tracing, Metrics, Labels | [Observability](#observability-features) |
| **Build System** | Dialyzer, Compilation | [Build System](#build-system-features) |
| **Developer Tools** | Shell, Documentation | [Developer Tools](#developer-tools) |

---

## Feature by OTP Version

### OTP 26 (May 2023)

| Feature | Description | Impact | erlmcp Usage |
|---------|-------------|--------|--------------|
| Incremental Dialyzer | 3-7x faster type analysis | HIGH | Development workflow |
| Concurrent Startup | Parallel application start | HIGH | Faster initialization |
| Persistent Config | Config survives reload | MEDIUM | Production tuning |
| Prep/Stop Callback | Graceful shutdown | MEDIUM | Clean shutdown |
| Enhanced Shell | Better auto-complete | LOW | Developer experience |

**Documentation**: [OTP 26 Features Summary](./OTP_26_FEATURES_SUMMARY.md)

### OTP 27 (May 2024)

| Feature | Description | Impact | erlmcp Usage |
|---------|-------------|--------|--------------|
| Native JSON Module | Built-in JSON encode/decode | HIGH | Protocol handling |
| Triple-Quoted Strings | Multi-line string literals | LOW | Documentation |
| Process Labels | Debug metadata | LOW | Process monitoring |
| Maybe Expression | Default enabled | LOW | Code patterns |
| Type Specs Enhanced | Better type inference | MEDIUM | Dialyzer accuracy |

**Documentation**: [OTP 26 Specific Features Analysis](./OTP_26_SPECIFIC_FEATURES_ANALYSIS.md)

### OTP 28 (May 2025)

| Feature | Description | Impact | erlmcp Usage |
|---------|-------------|--------|--------------|
| Priority Messages (EEP-76) | Critical message handling | HIGH | Health checks |
| Process Iterator | O(1) process enumeration | HIGH | Monitoring at scale |
| Nominal Types (EEP-69) | Named type distinction | MEDIUM | Protocol safety |
| Hibernate/0 | Simplified hibernation | MEDIUM | Memory optimization |
| PCRE2 Regex | Stricter pattern matching | HIGH | Input validation |
| Zip Generators | Parallel list comprehension | LOW | Code clarity |
| Strict Generators | Fail-fast pattern matching | LOW | Error handling |
| Post-Quantum Crypto | MLKEM hybrid algorithms | LOW | Future-proofing |
| MPTCP | Multipath TCP support | MEDIUM | Network resilience |
| Binary join/2 | Efficient binary joining | LOW | String operations |

**Documentation**:
- [OTP 28 Features](./OTP_28_FEATURES.md)
- [OTP 28 Architecture](./OTP_28_ARCHITECTURE.md)
- [OTP 28 Quick Reference](./OTP_28_QUICK_REFERENCE.md)

---

## Core Features

### JSON Handling

| Version | Method | Performance | Migration |
|---------|--------|-------------|----------|
| OTP 26 | `jsx:encode/1` | Baseline | N/A |
| OTP 27+ | `json:encode/1` | 3x faster | Drop-in |
| OTP 28+ | `json:encode/1` | 3x faster | Native |

**Compatibility Macro**: `?JSON_ENCODE(Data)`

**Related Docs**:
- [Migration Guide](./OTP_MIGRATION_GUIDE.md#json-handling)
- [Native JSON Migration](./docs/NATIVE_JSON_MIGRATION.md)

### Type System

| Feature | OTP 26 | OTP 27 | OTP 28 |
|---------|--------|--------|--------|
| Structural Types | Yes | Yes | Yes |
| Opaque Types | Yes | Yes | Enhanced |
| Nominal Types | No | No | **Yes** |

**Key Improvements**:
- EEP-69 nominal types prevent type confusion
- Enhanced opaque type handling
- Better Dialyzer warnings

**Related Docs**:
- [Type Checking Analysis](./OTP_26_28_TYPE_CHECKING_ANALYSIS.md)
- [Best Practices Compliance](./OTP_BEST_PRACTICES_COMPLIANCE.md)

### Regular Expressions

| Library | OTP 26-27 | OTP 28 |
|---------|-----------|--------|
| Engine | PCRE | **PCRE2** |
| Strictness | Loose | Strict |
| Breaking | No | **Yes** |

**Breaking Patterns**:
- `\i` - PCRE-specific
- `\B8` - Invalid in PCRE2
- Octal escapes - Not supported

**Related Docs**:
- [Compatibility Analysis - PCRE2](./OTP_COMPATIBILITY_ANALYSIS.md#pcre2-migration)
- [Migration Guide - Regex](./OTP_MIGRATION_GUIDE.md#pcre2-compatibility)

---

## Performance Features

### Process Hibernation

| Method | OTP 26-27 | OTP 28 |
|--------|-----------|--------|
| gen_server option | `{hibernate_after, MS}` | Same |
| BIF | `proc_lib:hibernate/3` | `erlang:hibernate/0` |

**Memory Savings**:
- Active: 50KB per process
- Hibernating: 5KB per process
- **90% reduction**

**Use Cases**:
- Idle sessions (30s+ inactivity)
- Sparse connections
- Circuit breakers

**Related Docs**:
- [28 Architecture - Hibernation](./OTP_28_ARCHITECTURE.md#process-hibernation)
- [Process Optimization Guide](./guides/PROCESS_HIBERNATION.md)

### Process Iteration

| Method | Memory | Scalability |
|--------|--------|-------------|
| `erlang:processes/0` | O(N) | <100K processes |
| `erlang:processes/1` | O(1) | 100K+ processes |

**Performance**:
| Process Count | Legacy Memory | Iterator Memory |
|---------------|---------------|-----------------|
| 10K | 320 KB | 3.2 KB |
| 100K | 3.2 MB | 3.2 KB |
| 1M | 32 MB | 3.2 KB |

**Related Docs**:
- [28 Features - Process Table](./OTP_28_FEATURES.md#2-process-table-scalability)
- [Build Optimization](./OTP_28_BUILD_SYSTEM_OPTIMIZATION.md)

### Compilation Performance

| Optimization | OTP 26 | OTP 27 | OTP 28 |
|--------------|--------|--------|--------|
| Base speed | 100s | 80-90s | 75-85s |
| Incremental | - | - | **15-30s** |

**Configuration**:
```erlang
{dialyzer_options, [incremental]}  % 3-7x faster
```

**Related Docs**:
- [Build System Optimization](./OTP_28_BUILD_SYSTEM_OPTIMIZATION.md)
- [Type Checking Analysis](./OTP_26_28_TYPE_CHECKING_ANALYSIS.md)

---

## Messaging Features

### Priority Messages (OTP 28)

| Component | Use Case |
|-----------|----------|
| Health Monitor | System health checks |
| Circuit Breaker | State transitions |
| Recovery Manager | Failure notifications |

**Benefits**:
- <1ms p99 latency under load
- Bypasses normal message queue
- FIFO ordering within priority

**Related Docs**:
- [28 Features - Priority Messages](./OTP_28_FEATURES.md#1-priority-messages--eep-76)
- [Quick Reference - Priority](./OTP_COMPATIBILITY_CHEAT_SHEET.md#priority-messages)

### Process Monitoring

| Feature | OTP 26-27 | OTP 28 |
|---------|-----------|--------|
| Monitor with metadata | No | **Yes** |
| Tagged monitors | No | **Yes** |
| Priority DOWN | No | **Yes** |

**Related Docs**:
- [Upgrade Guide - Monitoring](./OTP28_UPGRADE_GUIDE.md#process-monitoring)

---

## Networking Features

### TLS/SSL Improvements

| Feature | OTP 26 | OTP 27 | OTP 28 |
|---------|--------|--------|--------|
| TLS 1.3 optimization | Baseline | +15-25% | Maintained |
| MLKEM hybrid | No | No | **Yes** |
| Cipher config | Basic | Enhanced | **Full** |

**Post-Quantum Support**:
- `x25519mlkem768` - ECDH + MLKEM-768
- `secp384r1mlkem1024` - ECDH + MLKEM-1024
- `secp256r1mlkem768` - ECDH + MLKEM-768

**Related Docs**:
- [28 Features - Post-Quantum](./OTP_28_FEATURES.md#4-post-quantum-cryptography-otp-283)
- [TLS Optimization](./guides/TLS_13_OPTIMIZATION.md)

### Socket Improvements (OTP 28.3)

| Feature | Benefit |
|---------|---------|
| Receive buffer optimization | 94% waste reduction |
| MPTCP support | Network resilience |
| TCP keep-alive tuning | Connection stability |

**Related Docs**:
- [28 Features - Socket](./OTP_28_FEATURES.md#6-socket-optimizations-otp-280-283)

---

## Observability Features

### Process Labels (OTP 27+)

```erlang
proc_lib:set_label({mcp_server, ServerId}),
proc_lib:set_label({connection, ClientId}),
```

### Tracing (OTP 28)

```erlang
%% Session-based tracing
SessionId = trace:session({tracer, Pid}),
trace:function({lists, reverse, 2}, true),
trace:send(true),
trace:stop(SessionId).
```

**Related Docs**:
- [28 Features - Tracing](./OTP_28_FEATURES.md#11-observability--tracing-otp-280)

---

## Build System Features

### Incremental Dialyzer (OTP 26+)

| Mode | First Run | Subsequent | Speedup |
|------|-----------|------------|---------|
| Classic | 90s | 90s | 1x |
| Incremental | 90s | **15-30s** | **3-7x** |

**Configuration**:
```erlang
{dialyzer, [
    {dialyzer_options, [incremental]}
]}.
```

### Concurrent Startup (OTP 26+)

```erlang
application:ensure_all_started(Apps, permanent, concurrent),
```

**Performance**: 64% faster startup (450ms → 160ms)

**Related Docs**:
- [Build System](./OTP_28_BUILD_SYSTEM_OPTIMIZATION.md)
- [26 Features - Concurrent](./OTP_26_FEATURES_SUMMARY.md#1-concurrent-application-startup)

---

## Developer Tools

### Shell Improvements

| Feature | OTP 26 | OTP 27+ |
|---------|--------|---------|
| Auto-complete | Basic | Enhanced |
| ANSI docs | No | Yes |
| Function references | Limited | Full |

### Documentation

| Format | OTP 26 | OTP 27+ |
|--------|--------|---------|
| XML | Yes | Yes |
| Triple-quoted | No | **Yes** |

**Example**:
```erlang
-doc """
Multi-line documentation with **formatting**.

Examples:
    > my_function(test).
    ok
""".
```

---

## Feature Compatibility Matrix

### Cross-Version Compatibility

| Feature | OTP 26 | OTP 27 | OTP 28 | Fallback |
|---------|--------|--------|--------|----------|
| `json:encode/1` | JSX | Native | Native | JSX |
| Priority messages | No | No | Yes | Normal send |
| Process iterator | No | No | Yes | `processes/0` |
| Triple-quotes | No | Yes | Yes | XML format |
| PCRE2 | No | No | Yes | PCRE |
| Zip generators | No | No | Yes | Nested comprehension |
| `hibernate/0` | No | No | Yes | `proc_lib:hibernate/3` |
| Nominal types | No | No | Yes | Structural types |

### Breaking Changes Summary

| Change | Affected Versions | Severity |
|--------|-------------------|----------|
| PCRE → PCRE2 | 26 → 28 | HIGH |
| Opaque type semantics | 27 → 28 | MEDIUM |
| Fun creator pid | 25 → 26 | LOW |
| HiPE removal | 25 → 26 | LOW |

---

## API Quick Reference

### JSON APIs

```erlang
%% Native (OTP 27+)
json:encode(Data) -> Binary
json:decode(Binary) -> Map
json:encode(Data, Options) -> Binary
json:decode(Binary, Options) -> Map

%% Options
[binary, {escape, unicode}, {objects, maps}]
```

### Process Iterator APIs

```erlang
%% Create iterator
erlang:processes(Map :: #{max_iterations => integer()}) -> Iterator

%% Iterate
erlang:processes(Iterator) -> {Pids, NextIterator} | done
```

### Priority Message APIs

```erlang
%% Create alias
erlang:alias([Options]) -> Alias

%% Send with priority
erlang:send(Alias, Message, [priority])

%% Monitor with priority
erlang:monitor(process, Pid, [priority])
```

### Hibernation APIs

```erlang
%% OTP 28 simplified
erlang:hibernate(Module, Function, Args) -> no_return()

%% OTP 26-27
proc_lib:hibernate(Module, Function, Args) -> no_return()
```

---

## Related Documentation Index

### Comprehensive Guides
- [Developer Guide (This Document)](./OTP_26_28_DEVELOPER_GUIDE.md) - **START HERE**
- [Migration Guide](./OTP_MIGRATION_GUIDE.md) - Version migration
- [Compatibility Cheat Sheet](./OTP_COMPATIBILITY_CHEAT_SHEET.md) - Quick reference

### Architecture & Design
- [Architecture Recommendations](./OTP_ARCHITECTURE_RECOMMENDATIONS.md)
- [28 Architecture Reference](./OTP_28_ARCHITECTURE.md)
- [Best Practices Compliance](./OTP_BEST_PRACTICES_COMPLIANCE.md)

### Analysis & Research
- [Compatibility Analysis](./OTP_COMPATIBILITY_ANALYSIS.md)
- [Type Checking Analysis](./OTP_26_28_TYPE_CHECKING_ANALYSIS.md)
- [26 Specific Features](./OTP_26_SPECIFIC_FEATURES_ANALYSIS.md)

### Build & Performance
- [Build System Optimization](./OTP_28_BUILD_SYSTEM_OPTIMIZATION.md)
- [Performance Baselines](./benchmarks/OTP_28_BASELINES.md)
- [Quick Reference](./OTP_28_QUICK_REFERENCE.md)

### Upgrade Guides
- [28 Upgrade Guide](./OTP28_UPGRADE_GUIDE.md)
- [28 Migration Guide](./OTP_28_MIGRATION_GUIDE.md)

### Reference Cards
- [Compatibility Cheat Sheet](./OTP_COMPATIBILITY_CHEAT_SHEET.md)
- [28 Quick Reference](./OTP_28_QUICK_REFERENCE.md)

---

## Change Log

| Date | Version | Changes |
|------|---------|---------|
| 2026-02-02 | 1.0.0 | Initial consolidation of all OTP 26-28 documentation |

---

**Document Maintained By**: erlmcp Development Team
**For Questions**: See [Developer Guide](./OTP_26_28_DEVELOPER_GUIDE.md)
