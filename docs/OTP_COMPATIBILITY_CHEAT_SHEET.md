# OTP Compatibility Cheat Sheet for erlmcp

## Quick Reference

| Version | Status | Key Features | Breaking Changes |
|---------|--------|-------------|-----------------|
| **OTP 26** | ❌ Legacy | Basic shell, JSX JSON | Fun creator pid change |
| **OTP 27** | ⚠️ Transition | Native JSON, triple-quotes | max_handles optimization |
| **OTP 28** | ✅ Recommended | Priority messages, PCRE2, process iterator | Complete PCRE2 migration |

## Build Configuration

### rebar.config Updates
```erlang
% Minimum OTP version requirement
{minimum_otp_vsn, "27"}.  % Support OTP 27+
{minimum_otp_vsn, "28"}.  % Target OTP 28+

% Platform defines
{platform_define, "^2[7-9]", 'OTP_27_PLUS'}.
{platform_define, "^2[8-9]", 'OTP_28_PLUS'}.
```

### Version Detection Macros
```erlang
-ifdef(OTP_28_PLUS).
    % OTP 28+ code
    -define(HAVE_PRIORITY_MESSAGES, true).
    -define(HAVE_PROCESS_ITERATOR, true).
-else.
    % OTP 26-27 fallback
    -define(HAVE_PRIORITY_MESSAGES, false).
    -define(HAVE_PROCESS_ITERATOR, false).
-endif.
```

## Code Snippets by Version

### 1. JSON Handling

#### OTP 26 (JSX only)
```erlang
JSON = jsx:encode(Data),
Decoded = jsx:decode(Binary, [return_maps]).
```

#### OTP 27 (Available choice)
```erlang
case erlang:function_exported(json, encode, 1) of
    true -> json:encode(Data);  % Native
    false -> jsx:encode(Data)   % Fallback
end.
```

#### OTP 28+ (Recommended with otp_compat)
```erlang
JSON = ?JSON_ENCODE(Data),  % Auto-selects best option
Decoded = ?JSON_DECODE(Binary).
```

### 2. Process Enumeration

#### OTP 26 (Legacy - O(N) memory)
```erlang
Pids = erlang:processes(),
Count = length(Pids).  % Allocates large list
```

#### OTP 27 (Same legacy approach)
```erlang
Count = erlang:system_info(process_count).  % O(1) but limited
```

#### OTP 28+ (Optimized - O(1) memory)
```erlang
Iterator = erlang:processes_iterator(),
Count = count_processes_iterator(Iterator, 0).  % Streaming
```

### 3. Priority Messages

#### OTP 26-27 (No support)
```erlang
erlang:send(Pid, Msg, [nosuspend]).  % Normal priority
```

#### OTP 28+ (High priority)
```erlang
PrioAlias = alias([priority]),
erlang:send(PrioAlias, Msg, [priority]).  % High priority
```

### 4. Regex Patterns (PCRE vs PCRE2)

#### OTP 26-27 (PCRE patterns)
```erlang
Pattern = "\\i",        % PCRE-specific
re:run(Text, Pattern).
Pattern = "(\\d+)-(\\d+)";  % Groups
```

#### OTP 28+ (PCRE2 patterns)
```erlang
Pattern = "\\\\w";      % PCRE2-compatible
Pattern = "(\\\\d+)-(\\\\d+)";  % Escaped
% Breaking: "\\i", "\\B", "\\8" no longer work
```

## Migration Commands

### Quick Version Check
```bash
# Check current OTP version
erl +V

# Check available features
erl +features

# Test pattern compilation
erl -eval "io:format(\"~p~n\", [re:compile(\"\\\\w+\", [unicode])])" -s init stop
```

### Build Commands
```bash
# OTP 27 build
OTP_VSN=27.3.4 rebar3 compile

# OTP 28 build
OTP_VSN=28.3.1 rebar3 compile

# Version-specific test
OTP_VSN=28.3.1 rebar3 ct --suite=erlmcp_otp28_compat_SUITE
```

## Breaking Change Checklist

### PCRE2 Migration (HIGH PRIORITY)
- [ ] Audit all `re:run`, `re:split`, `re:compile` calls
- [ ] Replace `\\i`, `\\B`, `\\8` with valid PCRE2 syntax
- [ ] Test Unicode property matches (`\\p{...}`)
- [ ] Check branch reset groups `(?|...)` behavior

### Priority Messages (HIGH PRIORITY)
- [ ] Add priority alias creation in critical supervisors
- [ ] Update message sending in error handlers
- [ ] Implement fallback for OTP <28
- [ ] Test priority message ordering

### Process Iterator (MEDIUM PRIORITY)
- [ ] Replace `erlang:processes()` calls with iterator
- [ ] Add memory usage monitoring
- [ ] Implement fallback for OTP <27
- [ ] Benchmark performance improvement

## Compatibility Layer Usage

### Always Use These Macros
```erlang
% JSON encoding (auto-selects best option)
?JSON_ENCODE(Data)
?JSON_DECODE(Binary)

% Process enumeration (memory safe)
?SAFE_PROCESS_COUNT()
?SAFE_PROCESSES()

% Priority messages (graceful fallback)
?SEND_PRIORITY(Pid, Msg)
?SET_PRIORITY_HIGH()
```

### Runtime Version Detection
```erlang
% Check capabilities at runtime
case erlmcp_otp_compat:have_priority_messages() of
    true ->  % OTP 28+
        use_priority_messaging();
    false ->  % OTP 26-27
        use_normal_messaging()
end.
```

## Test Suite Examples

### Version Detection Test
```erlang
test_otp_version_detection() ->
    OTPVersion = erlmcp_otp_compat:otp_version(),
    ?assert(OTPVersion =/= undefined),
    ?assert(is_integer(OTPVersion)),
    ?assert(OTPVersion >= 26).  % Minimum supported
```

### PCRE2 Compatibility Test
```erlang
test_pcre2_patterns() ->
    % Valid PCRE2 patterns
    ValidPatterns = [
        "\\w+",           % Word characters
        "\\d+",           % Digits
        "[a-z]",          % Character class
        "(start|end)",    % Alternation
        "(\\d+)-(\\d+)"  % Capturing groups
    ],
    lists:foreach(fun test_pattern/1, ValidPatterns).

test_pattern(Pattern) ->
    case re:compile(Pattern, [unicode]) of
        {ok, _} -> ok;
        {error, Reason} ->
            ?assert(false, "Pattern ~s failed: ~p", [Pattern, Reason])
    end.
```

### Priority Message Test
```erlang
test_priority_messages() ->
    case erlmcp_otp_compat:have_priority_messages() of
        true ->
            % Test OTP 28+ priority functionality
            Self = self(),
            PrioAlias = alias([priority]),
            erlang:send(PrioAlias, test_msg, [priority]),
            receive
                test_msg -> ok
            after 1000 ->
                timeout
            end;
        false ->
            skip
    end.
```

## Performance Tips

### OTP 28 Optimizations
```erlang
% Use process iterator for large deployments
-ifndef(OTP_28_PLUS).
    % Fallback: slower but works
    RegistryProcesses = find_registry_processes(),
    Count = length(RegistryProcesses).
-else.
    % Optimized: O(1) memory
    Iterator = erlang:processes_iterator(),
    RegistryProcesses = filter_registry_iterator(Iterator),
    Count = count_processes_iterator(Iterator, 0).
-endif.

% Use priority messages for critical paths
CriticalMsg = {system_alert, overload},
?SEND_PRIORITY(alert_handler, CriticalMsg).
```

## Error Handling

### Version-Specific Errors
```erlang
handle_version_error(Error) ->
    case Error of
        {regex_compile, Pattern, Reason} when otp_version() >= 28 ->
            % PCRE2-specific error handling
            handle_pcre2_error(Pattern, Reason);
        {priority_send, _} when otp_version() < 28 ->
            % Fallback for OTP <28
            handle_fallback_error();
        _ ->
            handle_generic_error(Error)
    end.
```

### Graceful Degradation
```erlang
% Feature availability with fallback
use_native_json_if_available() ->
    case erlang:function_exported(json, encode, 1) of
        true ->
            json:encode(MyData);
        false ->
            jsx:encode(MyData)  % Fallback
    end.
```

## Quick Reference Tables

### Feature Availability by OTP Version

| Feature | OTP 26 | OTP 27 | OTP 28 | Macro Usage |
|---------|--------|--------|--------|-------------|
| Native JSON | ❌ | ✅ | ✅ | `?JSON_ENCODE()` |
| Priority Messages | ❌ | ❌ | ✅ | `?SEND_PRIORITY()` |
| Process Iterator | ❌ | ❌ | ✅ | `?SAFE_PROCESS_COUNT()` |
| Triple-Quotes | ❌ | ✅ | ✅ | `"""text"""` |
| PCRE2 | ❌ | ❌ | ✅ | `re:compile/2` |

### Migration Priority Matrix

| Component | Impact | Effort | Priority |
|----------|--------|--------|----------|
| `erlmcp_tool_router` | HIGH | MEDIUM | CRITICAL |
| `erlmcp_otp_compat` | HIGH | LOW | CRITICAL |
| JSON handling | MEDIUM | LOW | HIGH |
| Process registry | HIGH | HIGH | HIGH |
| Validation module | MEDIUM | MEDIUM | MEDIUM |

## Environment Variables

```bash
# Set for testing
export OTP_VERSION=28.3.1
export ERLMCP_TEST_OTP28=1

# For production
export ERLMCP_OTP_VERSION=28.3.1
export ERLMCP_COMPATIBILITY_MODE=strict
```

## Docker Quick Commands

```dockerfile
# Build for specific OTP version
FROM erlang:28.3.1-alpine as otp28
COPY . .
RUN rebar3 compile

# Multi-stage build for compatibility
FROM erlang:27.3.4-alpine as otp27
COPY . .
RUN OTP_VSN=27.3.4 rebar3 compile

# Final production image
FROM otp28 as production
```

## Common Patterns

### 1. Conditional Module Loading
```erlang
-ifndef(OTP_28_PLUS).
    -module(legacy_fallback).
-else.
    -module(modern_implementation).
-endif.
```

### 2. Feature Toggles
```erlang
-define(USE_PRIORITY_MESSAGES,
    (erlang:system_info(otp_release) >= "28")).

send_critical_msg(Msg) ->
    case ?USE_PRIORITY_MESSAGES of
        true ->
            erlang:send(priority_alias(), Msg, [priority]);
        false ->
            erlang:send(normal_pid(), Msg)
    end.
```

### 3. Version-Specific Configuration
```erlang
get_system_config() ->
    OTPVersion = erlang:system_info(otpr_release),
    case OTPVersion of
        "28" -> otp28_config();
        "27" -> otp27_config();
        _ -> otp26_config()
    end.
```

## Troubleshooting

### Common Issues

1. **"OTP version too old"**
   ```bash
   # Check rebar.config minimum_otp_vsn
   grep minimum_otp_vsn rebar.config
   ```

2. **"PCRE2 pattern error"**
   ```bash
   # Test pattern compilation
   erl -eval "re:compile(\"\\\\w+\", [unicode])" -s init stop
   ```

3. **"Priority message not supported"**
   ```bash
   # Check runtime version
   erl -eval "io:format(\"~p~n\", [erlang:system_info(otp_release)])" -s init stop
   ```

### Quick Fixes

1. **Regex pattern escaping**
   ```erlang
   % Change: "\\i" -> "\\\\w"
   % Change: "\\B" -> "\\\\b"
   % Change: "(?|...)" -> "(?:...)"
   ```

2. **Process enumeration**
   ```erlang
   % Use: ?SAFE_PROCESS_COUNT()
   % Instead: length(erlang:processes())
   ```

3. **JSON encoding**
   ```erlang
   % Use: ?JSON_ENCODE(Data)
   % Instead: jsx:encode(Data) or json:encode(Data)
   ```

---

**Remember: Always test thoroughly before deploying to production!**

**Related Files:**
- [Full Migration Guide](./OTP_MIGRATION_GUIDE.md)
- [Compatibility Analysis](./OTP_COMPATIBILITY_ANALYSIS.md)
- [otp_compat.hrl](https://github.com/ruvnet/erlmcp/blob/main/include/otp_compat.hrl)