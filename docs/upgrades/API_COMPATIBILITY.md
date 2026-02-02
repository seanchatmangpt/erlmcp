# API Compatibility Documentation

**Version**: 2.1.0
**Last Updated**: 2026-02-01
**Scope**: Erlang/OTP 26, 27, 28 compatibility

---

## Table of Contents

1. [Overview](#overview)
2. [Compatibility Matrix](#compatibility-matrix)
3. [Standard Library Changes](#standard-library-changes)
4. [OTP Behavior Changes](#otp-behavior-changes)
5. [erlmcp API Changes](#erlmcp-api-changes)
6. [Migration Code Examples](#migration-code-examples)
7. [Backward Compatibility](#backward-compatibility)
8. [Forward Compatibility](#forward-compatibility)

---

## Overview

erlmcp **2.1.0 targets Erlang/OTP 28.3.1** exclusively. This document details:
- API changes across OTP versions
- Backward compatibility guarantees
- Migration patterns for existing code

### Compatibility Strategy

**Platform Defines** in `rebar.config`:
```erlang
{erl_opts,
 [{platform_define, "^2[6-7]", 'OTP_LEGACY'},
  {platform_define, "^2[8-9]|^[3-9]", 'OTP_MODERN'}]}.
```

**Usage in Code**:
```erlang
-ifdef(OTP_LEGACY).
% OTP 26-27 specific code
-endif.

-ifdef(OTP_MODERN).
% OTP 28+ specific code
-endif.
```

---

## Compatibility Matrix

### API Modules

| Module | OTP 26 | OTP 27 | OTP 28 | Notes |
|--------|--------|--------|--------|-------|
| `erlang` | ✓ | ✓ | ✓ | Minor additions |
| `application` | ✓ | ✓ | ✓ | `runtime_dependencies` in 27+ |
| `gen_server` | ✓ | ✓ | ✓ | `hibernate/0` in 28+ |
| `supervisor` | ✓ | ✓ | ✓ | Enhanced in 28+ |
| `logger` | ✓ | ✓ | ✓ | Improved filters in 28+ |
| `maps` | ✓ | ✓ | ✓ | Stable |
| `persistent_term` | ✓ | ✓ | ✓ | Stable |
| `ets` | ✓ | ✓ | ✓ | New counters in 27+ |
| `re` | ✓ | ✓ | ✓ | PCRE2 in 28+ |
| `json` | ✗ | ✓ | ✓ | New in 27+ |

### Deprecated Modules

| Module | Replacement | Since | Removed In |
|--------|-------------|-------|------------|
| `dict` | `maps` | OTP 19 | OTP 27 |
| `orddict` | `maps` | OTP 19 | OTP 27 |
| `lists` (key ops) | `maps` | OTP 17 | OTP 26 |

---

## Standard Library Changes

### 1. gen_server Callbacks

#### OTP 28+ Additions

**`handle_continue/2`** (stable since OTP 21):
```erlang
% Initialize asynchronously
init([]) ->
    {ok, State, {continue, init_async}}.

handle_continue(init_async, State) ->
    % Heavy initialization
    {noreply, State}.
```

**`hibernate/0` Return** (OTP 28+):
```erlang
% Force hibernation after processing
handle_cast(_Request, State) ->
    {noreply, State, hibernate}.
```

**Usage Example**:
```erlang
-ifdef(OTP_MODERN).
handle_cast(long_running, State) ->
    Result = long_operation(),
    {noreply, State#{result => Result}, hibernate}.
-else.
handle_cast(long_running, State) ->
    Result = long_operation(),
    % Manual hibernation for OTP 27-
    proc_lib:hibernate(gen_server, enter_loop,
                      [?MODULE, [], State]),
    {noreply, State#{result => Result}}.
-endif.
```

### 2. Process Messaging

#### Priority Messages (OTP 28+)

```erlang
-ifdef(OTP_MODERN).
% Send with priority
send_priority(Dest, Message) ->
    erlang:send(Dest, Message, [priority]).

% Send with priority + absolute timeout
send_priority_abs(Dest, Message, Timeout) ->
    erlang:send(Dest, Message, [priority, {abs_timeout, Timeout}]).
-else.
% Fallback for OTP 27-
send_priority(Dest, Message) ->
    erlang:send_after(0, Dest, Message).  % Best effort

send_priority_abs(Dest, Message, Timeout) ->
    erlang:send_after(0, Dest, Message).  % No absolute timeout
-endif.
```

#### Enhanced Link Signaling (OTP 27+)

```erlang
% Monitor process with enhanced info
-ifdef(OTP_MODERN).
monitor_process(Pid) ->
    erlang:monitor(process, Pid, [{tag, my_tag}]).
-else.
monitor_process(Pid) ->
    erlang:monitor(process, Pid).
-endif.
```

### 3. ETS Improvements

#### New Counters API (OTP 27+)

```erlang
-ifdef(OTP_MODERN).
% OTP 28+: Fast counters with window
new_counter(Name) ->
    ets:new(counter,
           [named_table,
            public,
            {write_concurrency, true},
            {read_concurrency, true},
            {decentralized_counters, true}]).

increment_counter(Ref, N) ->
    ets:update_counter(Ref, key, {2, N}, {key, 0}).
-else.
% OTP 26-27: Standard update_counter
new_counter(Name) ->
    ets:new(counter,
           [named_table,
            public,
            {write_concurrency, true}]).

increment_counter(Ref, N) ->
    ets:update_counter(Ref, key, {2, N}).
-endif.
```

### 4. Regular Expressions

#### PCRE2 Migration (OTP 28+)

**PCRE2 is now default** - 3-4x faster, different binary format:

```erlang
-ifdef(OTP_MODERN).
% OTP 28+: PCRE2 (default)
compile_pattern(Pattern) ->
    re:compile(Pattern, [unicode, {newline, crlf}]).
-else.
% OTP 27-: PCRE1 (legacy)
compile_pattern(Pattern) ->
    re:compile(Pattern, [unicode, {newline, crlf}, {offset, 0}]).
-endif.
```

**Important**: Recompile all cached regex patterns when upgrading to OTP 28.

### 5. JSON Encoding

#### Native JSON Module (OTP 27+)

```erlang
-ifdef(OTP_MODERN).
% OTP 28+: Use native json module (faster)
encode_json(Term) ->
    json:encode(Term).

decode_json(Bin) ->
    json:decode(Bin).
-else.
% OTP 27-: Use jsx (dependency)
encode_json(Term) ->
    jsx:encode(Term).

decode_json(Bin) ->
    jsx:decode(Bin, [return_maps]).
-endif.
```

---

## OTP Behavior Changes

### 1. application Behavior

#### runtime_dependencies (OTP 27+)

```erlang
% In your.app file
{applications, [kernel, stdlib, sasl]},
{runtime_dependencies, [kernel, stdlib, sasl, crypto]},
```

**Purpose**: Enforce runtime dependency checking at startup.

**Migration**:
```erlang
% OTP 26: No enforcement
% OTP 27+: Add to app file
-ifdef(OTP_MODERN).
{runtime_dependencies, [kernel, stdlib, sasl, crypto, ssl]}.
-endif.
```

### 2. supervisor Behavior

#### Enhanced Restart Intensity (OTP 28+)

```erlang
-ifdef(OTP_MODERN).
% OTP 28+: Max restarts with time window
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 60,
                 auto_shutdown => all_signoffs},  % New option
    {ok, {SupFlags, ChildSpecs}}.
-else.
% OTP 27-
init([]) ->
    SupFlags = {one_for_one, 10, 60},
    {ok, {SupFlags, ChildSpecs}}.
-endif.
```

### 3. logger Behavior

#### Enhanced Filters (OTP 28+)

```erlang
-ifdef(OTP_MODERN).
% OTP 28+: Enhanced metadata filtering
logger:update_primary_config(#{
    filter_default => log,
    filters => [
        {remote_ip, {fun remote_ip_filter/2, no_log}}
    ]
}).
-else.
% OTP 27-
logger:update_primary_config(#{
    filter_default => log
}).
-endif.
```

---

## erlmcp API Changes

### 2.0.x → 2.1.0

#### Session Backend

**New Hibernate Support** (OTP 28+):
```erlang
% erlmcp_session_backend.erl
-ifdef(OTP_MODERN).
handle_cast({set, Key, Value}, State) ->
    ets:insert(?TAB, {Key, Value}),
    {noreply, State, hibernate}.  % Force hibernation
-else.
handle_cast({set, Key, Value}, State) ->
    ets:insert(?TAB, {Key, Value}),
    {noreply, State}.
-endif.
```

#### Transport Layer

**Priority Messages** (OTP 28+):
```erlang
% erlmcp_transport_tcp.erl
-ifdef(OTP_MODERN).
send_message(Socket, Data) ->
    gen_tcp:send(Socket, Data),
    ok.

send_priority(Socket, Data) ->
    gen_tcp:send(Socket, Data),
    % Signal receiver with priority
    case self() of
        Pid when is_pid(Pid) ->
            erlang:send(Pid, transport_data_sent, [priority]);
        _ -> ok
    end.
-endif.
```

#### JSON-RPC Protocol

**Enhanced Parsing** (PCRE2):
```erlang
% erlmcp_json_rpc.erl
-ifdef(OTP_MODERN).
% OTP 28+: PCRE2 (3-4x faster)
parse_request(Bin) ->
    case re:run(Bin, <<...>>, [unicode, {newline, crlf}]) of
        {match, [_, Id, Method, Params]} ->
            {ok, #{id => Id, method => Method, params => Params}};
        nomatch -> {error, invalid_request}
    end.
-else.
% OTP 27-
parse_request(Bin) ->
    case re:run(Bin, <<...>>, [unicode, {newline, crlf}, {offset, 0}]) of
        {match, [_, Id, Module, Params]} ->
            {ok, #{id => Id, method => Module, params => Params}};
        nomatch -> {error, invalid_request}
    end.
-endif.
```

---

## Migration Code Examples

### Example 1: gen_server with Continue

**Before (OTP 27-)**:
```erlang
init([]) ->
    State = initialize_state(),
    {ok, State}.

initialize_state() ->
    % Heavy init
    #{data => load_data()}.
```

**After (OTP 28+)**:
```erlang
init([]) ->
    {ok, #{}, {continue, init_async}}.

handle_continue(init_async, _State) ->
    Data = load_data(),
    {noreply, #{data => Data}}.
```

### Example 2: Priority Messaging

**Before (OTP 27-)**:
```erlang
send_tool_call(Dest, ToolCall) ->
    gen_server:cast(Dest, {tool_call, ToolCall}).
```

**After (OTP 28+)**:
```erlang
-ifdef(OTP_MODERN).
send_tool_call(Dest, ToolCall) ->
    gen_server:cast(Dest, {tool_call, ToolCall}),
    erlang:send(Dest, tool_call_priority, [priority]).
-else.
send_tool_call(Dest, ToolCall) ->
    gen_server:cast(Dest, {tool_call, ToolCall}).
-endif.
```

### Example 3: Persistent Term Configuration

**Before (ETS)**:
```erlang
set_config(Key, Value) ->
    application:set_env(erlmcp, Key, Value).

get_config(Key) ->
    application:get_env(erlmcp, Key).
```

**After (Persistent Term - OTP 26+)**:
```erlang
set_config(Key, Value) ->
    persistent_term:put({erlmcp, config, Key}, Value).

get_config(Key) ->
    case persistent_term:get({erlmcp, config, Key}, undefined) of
        undefined -> {error, not_found};
        Value -> {ok, Value}
    end.
```

---

## Backward Compatibility

### Supported Patterns

**erlmcp maintains backward compatibility for**:
- Public API functions in `erlmcp_core`
- MCP protocol message formats
- Transport layer interfaces
- Configuration file formats

### Unsupported

**NOT backward compatible**:
- Internal module structures
- ETS table layouts
- Process message ordering
- Binary format encodings

### Shim Layer

For OTP 26-27 compatibility (legacy support):

```erlang
% erlmcp_compat.erl
-module(erlmcp_compat).

-ifdef(OTP_MODERN).
-export([hibernate_after/1]).
hibernate_after(_State) -> hibernate.
-else.
-export([hibernate_after/1]).
hibernate_after(_State) -> timeout.
-endif.
```

---

## Forward Compatibility

### Best Practices

**Writing forward-compatible code**:

1. **Use platform defines**:
```erlang
-ifdef(OTP_MODERN).
% New feature
-else.
% Fallback
-endif.
```

2. **Avoid deprecated modules**:
```erlang
% GOOD (stable)
maps:get(Key, Map, Default).

% BAD (deprecated)
dict:get(Key, Dict, Default).
```

3. **Prefer OTP APIs over custom implementations**:
```erlang
% GOOD (native)
json:encode(Term).

% ACCEPTABLE (dependency)
jsx:encode(Term).

% BAD (custom)
my_json:encode(Term).
```

4. **Document version requirements**:
```erlang
%% @doc Requires OTP 28+ for priority messages
%% @end
-spec send_priority(pid(), term()) -> ok.
```

---

## Quick Reference

### Check OTP Version at Runtime

```erlang
otp_version() ->
    list_to_integer(erlang:system_info(otp_release)).

is_otp_28_plus() ->
    otp_version() >= 28.

supports_priority_messages() ->
    is_otp_28_plus().
```

### Feature Detection

```erlang
supports_hibernate_callback() ->
    % Check if hibernate/0 is valid return
    try
        % Compile-time check
        -ifdef(OTP_MODERN).
        true
        -else.
        false
        -endif
    catch _:_ ->
        false
    end.
```

### Conditional Compilation

```erlang
% In rebar.config
{erl_opts,
 [{platform_define, "^2[6-7]", 'OTP_LEGACY'},
  {platform_define, "^2[8-9]|^[3-9]", 'OTP_MODERN'}]}.

% In code
-ifdef(OTP_MODERN).
% OTP 28+ code
-else.
% OTP 27- code
-endif.
```

---

## Testing Compatibility

### Version-Specific Tests

```erlang
% erlmcp_compat_tests.erl
-module(erlmcp_compat_tests).

-include_lib("eunit/include/eunit.hrl").

otp_28_feature_test() ->
    case erlang:system_info(otp_release) of
        "28" ->
            % Test priority messages
            ?assert(test_priority_messages());
        "27" ->
            ?assert(test_fallback_messaging());
        "26" ->
            ?assert(test_legacy_messaging())
    end.

test_priority_messages() ->
    % OTP 28+ implementation
    true.

test_fallback_messaging() ->
    % OTP 27 implementation
    true.

test_legacy_messaging() ->
    % OTP 26 implementation
    true.
```

---

## Support

**API Questions**: https://github.com/seanchatmangpt/erlmcp/discussions
**Bug Reports**: https://github.com/seanchatmangpt/erlmcp/issues

---

**Document Version**: 1.0
**Author**: SPARC Orchestrator
**Review Date**: 2026-03-01
