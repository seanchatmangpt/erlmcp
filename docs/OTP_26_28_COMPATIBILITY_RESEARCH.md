# Erlang/OTP 26-28 Research Report: Features, Breaking Changes, and Compatibility

## Executive Summary

This research document provides a comprehensive analysis of Erlang/OTP versions 26-28, focusing on features, breaking changes, and compatibility requirements that affect erlmcp. The analysis reveals significant changes in distribution protocols, messaging systems, and security features that require attention for long-term compatibility.

## Version Overview

### Erlang/OTP 26 (May 2023)
**Status**: Production-ready
**Key Changes**: Removed deprecated functionality, enhanced shell features

### Erlang/OTP 27 (May 2024)
**Status**: Production-ready
**Key Changes**: Triple-quoted strings (EEP 64), map support for supervisor specs

### Erlang/OTP 28 (May 2025)
**Status**: Current latest
**Key Changes**: Priority messages (EEP 76), enhanced crypto algorithms

## Breaking Changes Analysis

### 1. Distribution Protocol Changes (OTP 26-28)

#### **Critical**: Old Link Protocol Removed (OTP 26)
```erlang
% BEFORE OTP 26: Old link protocol supported
% OTP 26: Old link protocol removed - nodes will refuse to connect
% without implementing the new link protocol

% Impact on erlmcp:
% - Distributed systems must use new link protocol
% - Custom distribution implementations need updates
% - Backward compatibility with OTP 23-24 nodes lost
```

#### **Warning**: Distribution Control Messages (OTP 28)
```erlang
% OTP 28: ALIAS_SEND and ALIAS_SEND_TT deprecated
% Scheduled for removal in OTP 30
% Replaced by ALTACT_SIG_SEND control message

% Support indicated by DFLAG_ALIAS vs DFLAG_ALTACT_SIG
% Migration needed for custom distribution implementations
```

### 2. Crypto API Changes (OTP 28)

#### **Breaking**: Crypto module functions deprecated
```erlang
% BEFORE (OTP 27 and earlier):
crypto:enable_fips_mode(Enabled)
crypto:start()
crypto:stop()

% AFTER (OTP 28): Use configuration instead
% Replace with:
% - Config parameter: fips_mode
% - Application start: application:start(crypto)
% - Application stop: application:stop(crypto)
```

### 3. Supervisor Behavior Changes

#### **Enhanced**: Map Support for Supervisor Specs (OTP 27)
```erlang
% BEFORE OTP 27: Tuple-based child specifications
ChildSpec = {id, {id, start_link, []}, permanent, 5000, worker, [module]},

% AFTER OTP 27: Maps also supported (more readable)
ChildSpec = #{
    id => id,
    start => {id, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [module]
},
```

#### **New**: Priority Message Integration (OTP 28)
```erlang
% OTP 28: Priority messages (EEP 76) for supervisors
% Allows critical messages to overtake regular messages in queue

% Example usage:
Alias = erlang:alias([{priority}]),
erlang:send(Alias, critical_message, [{priority}])
```

### 4. Deprecated Functions (Complete List)

#### OTP 28 Deprecations
- `crypto:enable_fips_mode/1` → Use config parameter
- `crypto:start/0` → Use `application:start(crypto)`
- `crypto:stop/0` → Use `application:stop(crypto)`

#### OTP 27 Deprecations
- `code:lib_dir/2` → Will be removed in future
- `mnesia_registry:create_table/_` → Use `mnesia:create_table/2`
- `ssl:prf/5` → Use `export_key_materials/4`

#### OTP 26 Deprecations (Already Removed)
- `code:rehash/0` → Code path cache feature removed
- `code:is_module_native/1` → HiPE removed
- Old link protocol → New link protocol mandatory

## Compatibility Matrix

| Component | OTP 26 | OTP 27 | OTP 28 | Elixir 1.18 | Elixir 1.19 |
|-----------|---------|---------|---------|-------------|-------------|
| erlmcp_core | ✅ Compatible | ✅ Compatible | ✅ Compatible | ✅ Compatible | ✅ Compatible |
| Distribution | ⚠️ Old protocol removed | ✅ Stable | ⚠️ Control msg deprecated | ✅ Works | ✅ Works |
| Priority Messages | ❌ Not available | ✅ Available | ✅ Available | ❌ Not supported | ✅ Supported |
| Crypto API | ✅ Old API | ✅ Old API | ⚠️ Some deprecated | ✅ Compatible | ✅ Compatible |
| Supervisor Specs | ✅ Tuples only | ✅ Maps supported | ✅ Enhanced | ✅ Compatible | ✅ Compatible |

## Migration Paths

### 1. Distribution Protocol Migration (OTP 26)
```erlang
% For custom distribution implementations:
% 1. Update to use new link protocol
% 2. Test with OTP 26+ nodes only
% 3. Remove fallback for old protocol

% erlmcp distribution module updates needed:
% - Update distribution handlers
% - Test inter-node communication
% - Update documentation
```

### 2. Crypto API Migration (OTP 28)
```erlang
% Replace crypto:start() calls:
crypto:start() ->
    application:start(crypto).

% Replace crypto:stop() calls:
crypto:stop() ->
    application:stop(crypto).

% Replace fips_mode configuration:
% Instead of: crypto:enable_fips_mode(true)
% Use in sys.config: {crypto, [{fips_mode, true}]}
```

### 3. Supervisor Spec Migration (OTP 27)
```erl
% Gradual migration from tuples to maps:
start_link() ->
    ChildSpecs = [
        #{
            id => erlmcp_server,
            start => {erlmcp_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_server]
        }
    ],
    supervisor:start_link({local, erlmcp_supervisor}, erlmcp_supervisor, ChildSpecs).
```

### 4. Priority Messages Implementation (OTP 28)
```erlang
% For critical message handling in erlmcp:
setup_priority_messaging() ->
    % Create priority alias
    Alias = erlang:alias([{priority}]),

    % Register alias in process registry
    gproc:reg({p, gproc, priority_alias}, Alias),

    % Return alias for other processes to use
    Alias.

send_critical_message(Pid, Message) ->
    case gproc:lookup_value({p, gproc, priority_alias}) of
        Alias ->
            erlang:send(Pid, Message, [{priority, Alias}]);
        undefined ->
            % Fallback to normal message
            erlang:send(Pid, Message)
    end.
```

## Elixir Compatibility

### Elixir 1.18
- **Status**: Tested with OTP 27
- **OTP 28**: May have compatibility issues
- **Recommendation**: Use Elixir 1.19+ for OTP 28 support

### Elixir 1.19
- **Status**: Officially supports OTP 28.1+
- **Recommendation**: Recommended for new projects using OTP 28

## Testing Strategy

### 1. Cross-Version Testing Matrix
```erlang
% Test combinations:
% - OTP 26 + Elixir 1.18
% - OTP 27 + Elixir 1.18/1.19
% - OTP 28 + Elixir 1.19

% Focus areas:
% - Distribution protocol compliance
% - Message queue behavior
% - Crypto functionality
% - Supervisor restart behavior
```

### 2. Specific Test Cases
```erlang
% Test old vs new distribution protocol:
test_distribution_compatibility() ->
    % Test with OTP 26+ nodes only
    % Ensure no old protocol fallbacks

% Test priority messages:
test_priority_messages() ->
    % Setup priority alias
    % Send critical messages
    % Verify message ordering

% Test crypto migration:
test_crypto_compatibility() ->
    % Test both old and new crypto API
    % Ensure FIPS mode configuration works
```

## Recommendations for erlmcp

### Immediate Actions (High Priority)
1. **Update Distribution Modules**: Remove old protocol support, ensure new protocol compliance
2. **Audit Crypto Usage**: Replace deprecated crypto functions with application-based calls
3. **Cross-OTP Testing**: Test against OTP 26, 27, and 28 to ensure compatibility

### Medium-term Actions
1. **Implement Priority Messages**: For critical system messages in supervisors
2. **Upgrade Elixir**: Move to Elixir 1.19+ for OTP 28 support
3. **Update Documentation**: Reflect new APIs and deprecated functions

### Long-term Considerations
1. **Removal Timeline**: Plan for OTP 30 removal of ALIAS_SEND control messages
2. **Performance Testing**: Validate priority message performance impact
3. **Security Audit**: Review distribution protocol security implications

## Conclusion

Erlang/OTP 26-28 introduces significant changes that affect erlmcp:

1. **Distribution Protocol**: Major changes require updating custom implementations
2. **Crypto API**: Migration needed for OTP 28 compatibility
3. **Messaging**: Priority messages offer new capabilities for critical systems
4. **Supervisor**: Enhanced readability with map specifications

While most changes are backward compatible, careful testing and gradual migration are recommended. The most critical change is the removal of the old link protocol in OTP 26, which affects distributed systems.

**Recommendation**: Test thoroughly across all supported OTP versions and plan migration path to Elixir 1.19+ for future-proof support.

## Sources

- [Deprecations — Erlang/OTP v28.3.1](https://www.erlang.org/doc/deprecations.html)
- [Removed Functionality — Erlang/OTP v28.3.1](https://www.erlang.org/doc/removed.html)
- [EEP 76: Priority Messages](https://www.erlang.org/eeps/eep-0076)
- [Erlang/OTP 28 Highlights](https://www.erlang.org/blog/highlights-otp-28/)
- [Elixir Compatibility Guide](https://hexdocs.pm/elixir/compatibility-and-deprecations.html)