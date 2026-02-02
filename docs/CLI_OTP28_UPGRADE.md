# CLI OTP 28 Upgrade Guide

## Overview

This document outlines the OTP 28 feature upgrades for the erlmcp_cli application, focusing on native JSON handling, UTF-8 support, and modern process spawning patterns.

## OTP 28 Features to Showcase

### 1. Native JSON Support (OTP 27+)

**Before (jsx)**:
```erlang
%% Slow, external dependency
JsonData = jsx:decode(Bin, [{labels, binary}, return_maps]),
Encoded = jsx:encode(Map)
```

**After (native)**:
```erlang
%% Fast, built-in, validates UTF-8
JsonData = 'JSON':decode(Bin, [maps]),
Encoded = 'JSON':encode(Map)
```

**Benefits**:
- 3-5x faster JSON parsing
- Built-in UTF-8 validation
- No external dependency (jsx removal)
- Better error messages
- Native map support

### 2. Enhanced UTF-8 Support

**OTP 28 UTF-8 Features**:
```erlang
%% New binary module functions
binary:match(Subject, Pattern, [scope, {utf, true}])
binary:replace(Subject, Pattern, Replacement, [global, {utf, true}])

%% Unicode-aware string handling
string:trim(String, both, "\s\t\n\r" ++ [160])  % Includes NBSP
string:casefold(String)  % Better than lowercase for Unicode
string:titlecase(String)  % New title case conversion
```

**CLI Improvements**:
- Proper UTF-8 command parsing
- International tool/resource names
- UTF-8 error messages
- Unicode file path support

### 3. Modern Process Spawning

**Before**:
```erlang
spawn_link(fun() -> parser_loop() end),
spawn_opt(fun() -> executor_loop() end, [link, {priority, high}]).
```

**After (OTP 28)**:
```erlang
%% Spawn with priority and hibernation
spawn_link(fun() -> parser_loop() end),
spawn_opt(fun() -> executor_loop() end,
          [link,
           {priority, max},
           {min_heap_size, 1000},
           {max_heap_size, 1000000},
           {message_queue_data, off_heap}]).

%% Set priority dynamically
process_flag(priority, max)  % low | normal | high | max
```

### 4. OTP 28 Status Output

**CLI Status Command Enhancements**:
```erlang
%% Show OTP 28 features
erlmcp-cli> status

=== OTP Information ===
OTP Release: 28
Emulator: aarch64-apple-darwin25.2.0
Scheduler Threads: 16
Time Correction: enabled

=== JSON Support ===
JSON Engine: native (OTP 27+)
UTF-8 Validation: enabled
Decoder: maps (optimized)
Encoder: [pretty, safe]

=== Process Features ===
Priority Messages: enabled
Process Iterators: enabled
Tagged Monitors: enabled
Memory Guards: enabled
Hibernation: enabled

=== UTF-8 Support ===
String Handling: Unicode-aware
Case Folding: enabled
Title Case: enabled
Binary Matching: UTF-8 mode

=== Session State ===
Active Sessions: 3
Parser Priority: high
Executor Priority: max
Transport Priority: normal
```

## Migration Checklist

### Phase 1: JSON Migration (HIGH PRIORITY)

- [ ] Audit all `jsx:decode/2` calls
- [ ] Replace with `'JSON':decode/2`
- [ ] Audit all `jsx:encode/2` calls
- [ ] Replace with `'JSON':encode/2`
- [ ] Remove jsx dependency from rebar.config
- [ ] Update JSON error handling

**Files to Update**:
- `apps/erlmcp_cli/src/erlmcp_cli_json_rpc.erl`
- `apps/erlmcp_cli/src/erlmcp_cli_config.erl`
- `apps/erlmcp_cli/src/erlmcp_cli_auth.erl`
- `apps/erlmcp_cli/src/erlmcp_cli_secrets.erl`
- `apps/erlmcp_cli/src/erlmcp_cli_otel.erl`
- `apps/erlmcp_cli/src/erlmcp_cli_tool.erl`
- `apps/erlmcp_cli/src/erlmcp_cli_resource.erl`
- `apps/erlmcp_cli/src/erlmcp_cli_errors.erl`
- `apps/erlmcp_cli/src/erlmcp_transport_*.erl` (all transports)

### Phase 2: UTF-8 Enhancements

- [ ] Update `atom_to_binary/2` to use `utf8` encoding explicitly
- [ ] Use `string:casefold/1` for case-insensitive comparisons
- [ ] Use `string:titlecase/1` for display text
- [ ] Add UTF-8 validation for user input
- [ ] Use `binary:match/3` with UTF-8 options

**Files to Update**:
- `apps/erlmcp_cli/src/erlmcp_cli_session.erl` (command parsing)
- `apps/erlmcp_cli/src/erlmcp_cli_registry.erl` (tool names)
- `apps/erlmcp_cli/src/erlmcp_cli_resource.erl` (URIs)
- `apps/erlmcp_cli/src/erlmcp_cli_tool.erl` (tool metadata)

### Phase 3: Process Spawning

- [ ] Update `spawn_link/1` to use `spawn_opt/2` with priority
- [ ] Add `process_flag(priority, max)` where appropriate
- [ ] Use `{message_queue_data, off_heap}` for large queues
- [ ] Add hibernation to idle processes
- [ ] Use `process_flag(trap_exit, true)` for proper shutdown

**Files to Update**:
- `apps/erlmcp_cli/src/erlmcp_cli_session.erl` (parser, executor, transport)

### Phase 4: Status Command

- [ ] Add `erlmcp_cli_status:show_otp_info/0`
- [ ] Add `erlmcp_cli_status:show_json_support/0`
- [ ] Add `erlmcp_cli_status:show_utf8_support/0`
- [ ] Add `erlmcp_cli_status:show_process_features/0`
- [ ] Integrate into `erlmcp-cli status` command

**New Files**:
- `apps/erlmcp_cli/src/erlmcp_cli_status.erl`

### Phase 5: Testing

- [ ] Test JSON decoding/encoding with UTF-8
- [ ] Test international tool names
- [ ] Test priority message handling
- [ ] Test process spawning with options
- [ ] Verify status output

**Test Files**:
- `apps/erlmcp_cli/test/erlmcp_cli_json_tests.erl`
- `apps/erlmcp_cli/test/erlmcp_cli_utf8_tests.erl`
- `apps/erlmcp_cli/test/erlmcp_cli_process_tests.erl`
- `apps/erlmcp_cli/test/erlmcp_cli_status_tests.erl`

## Implementation Notes

### JSON Migration Pattern

**Old Code**:
```erlang
JsonData = jsx:decode(Bin, [{labels, binary}, return_maps]),
MapEncoded = jsx:encode(Map, [indent]).
```

**New Code**:
```erlang
%% Decode with UTF-8 validation
JsonData = 'JSON':decode(Bin, [maps]),

%% Encode with formatting options
MapEncoded = 'JSON':encode(Map, [pretty, safe]).
```

### UTF-8 Pattern

**Old Code**:
```erlang
%% Case-insensitive comparison
string:to_lower(String1) =:= string:to_lower(String2).
```

**New Code**:
```erlang
%% Unicode-aware case folding
string:casefold(String1) =:= string:casefold(String2).
```

### Process Spawning Pattern

**Old Code**:
```erlang
Parser = spawn_link(fun() -> parser_loop() end),
Executor = spawn_link(fun() -> executor_loop() end).
```

**New Code**:
```erlang
%% Parser with high priority
Parser = spawn_opt(fun() -> parser_loop() end,
                    [link,
                     {priority, high},
                     {min_heap_size, 1000},
                     {max_heap_size, 1000000}]),

%% Executor with max priority and off-heap queue
Executor = spawn_opt(fun() -> executor_loop() end,
                      [link,
                       {priority, max},
                       {message_queue_data, off_heap}]).

%% Set hibernation for idle processes
process_flag(priority, max),
erlang:hibernate(module, function, args).
```

## Quality Gates

Run these commands after each phase:

```bash
# Compilation
TERM=dumb rebar3 compile

# Unit tests
rebar3 eunit --module=erlmcp_cli_json_tests
rebar3 eunit --module=erlmcp_cli_utf8_tests
rebar3 eunit --module=erlmcp_cli_process_tests
rebar3 eunit --module=erlmcp_cli_status_tests

# Coverage
rebar3 cover -v

# Dialyzer
rebar3 dialyzer

# Xref
rebar3 xref
```

## Performance Expectations

### JSON Performance

- **Decode**: 3-5x faster than jsx
- **Encode**: 2-3x faster than jsx
- **Memory**: 30-40% less allocation

### UTF-8 Performance

- **Case folding**: 10-20% faster with better accuracy
- **String operations**: Native UTF-8 handling
- **Binary matching**: Faster with UTF-8 mode

### Process Performance

- **Priority messages**: Lower latency for urgent operations
- **Off-heap queues**: Reduced GC pressure
- **Hibernation**: Lower memory footprint

## Rollback Plan

If issues occur:

1. **Revert to jsx**: Keep conditional JSON support
2. **Disable UTF-8**: Use legacy string functions
3. **Lower priority**: Use normal/default priorities
4. **Disable hibernation**: Remove `erlang:hibernate/3` calls

## References

- [JSON Module Documentation](https://www.erlang.org/doc/man/json.html)
- [String Module (Unicode)](https://www.erlang.org/doc/man/string.html)
- [Process Flag (priority)](https://www.erlang.org/doc/man/erlang.html#process_flag-2)
- [Process Flag (message_queue_data)](https://www.erlang.org/doc/man/erlang.html#process_flag-message_queue_data)
- [spawn_opt/2](https://www.erlang.org/doc/man/erlang.html#spawn_opt-2)
- [erlang:hibernate/3](https://www.erlang.org/doc/man/erlang.html#hibernate-3)

## Changelog

### v1.0.0 (2025-02-02)
- Initial OTP 28 upgrade plan
- JSON migration strategy (jsx → native)
- UTF-8 enhancement strategy
- Process spawning improvements
- Status command enhancements
