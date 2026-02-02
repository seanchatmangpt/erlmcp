# OTP Migration Guide for erlmcp

## Quick Start Migration Path

### For New Projects (Recommended)
```bash
# Target OTP 28.3.1
OTP_VERSION="28.3.1"
./scripts/install-otp.sh $OTP_VERSION
make clean compile OTP_VSN=$OTP_VERSION
```

### For Upgrading from OTP 26
```bash
# Step 1: Backup current system
./scripts/backup-system.sh

# Step 2: Install OTP 27
./scripts/install-otp.sh "27.3.4"

# Step 3: Test compatibility
make test OTP_VSN=27
make verify-fast

# Step 4: Upgrade to OTP 28
./scripts/install-otp.sh "28.3.1"
make clean compile OTP_VSN=28
make test OTP_VSN=28
```

## Version-Specific Migration Details

### OTP 26 → OTP 27 Migration

#### Changes Required

1. **JSON Module Handling**
```erlang
% Before (OTP 26)
JSON = jsx:encode(Data),
Decoded = jsx:decode(JSON, [return_maps])

% After (OTP 27) - Compatible with otp_compat.hrl
JSON = ?JSON_ENCODE(Data),
Decoded = ?JSON_DECODE(JSON)
```

2. **Documentation Format**
```erlang
% Before (OTP 26)
-doc "<p>Function description</p>".
-spec my_function(Arg) -> Result.

% After (OTP 27) - Triple-quoted strings
-doc """
Returns the result for the given argument.

Example:
    > my_function(test).
    ok
""".
-spec my_function(Arg) -> Result.
```

3. **Process Labels (OTP 27+)**
```erlang
% Add to critical processes
proc_lib:set_label({critical_worker, WorkerId}),
```

#### Configuration Updates
```erlang
% rebar.config
{platform_define, "^2[7]", 'OTP_27_PLUS'}.  % For OTP 27 features
```

#### Testing Command
```bash
# Run OTP 27 specific tests
rebar3 ct --suite=erlmcp_otp27_compat_SUITE
```

### OTP 27 → OTP 28 Migration (Recommended)

#### Critical Changes Required

1. **PCRE2 Regex Compatibility**
```erlang
% Before (OTP 27 - PCRE)
Pattern = "\\w+\\s+(?=\\d+)",
re:run(Text, Pattern, [unicode, {capture, all_names, binary}])

% After (OTP 28 - PCRE2) - Updated patterns
Pattern = "\\w\\+\\s\\+\\(\\?\\=\\d\\+\\)",  % Properly escaped
case re:compile(Pattern, [unicode]) of
    {ok, Compiled} -> re:run(Text, Compiled, [unicode, {capture, all_names, binary}]);
    {error, Reason} -> handle_error(Reason)
end
```

2. **Priority Messages Implementation**
```erlang
% In critical path handlers
-ifdef(OTP_28_PLUS).
    % OTP 28+ implementation
    PrioAlias = alias([priority]),
    erlang:send(PrioAlias, critical_msg, [priority]),
-else.
    % OTP 27 fallback
    erlang:send(Pid, critical_msg, [nosuspend]),
-endif.
```

3. **Process Iterator Optimization**
```erlang
% Replace legacy process enumeration
-ifdef(OTP_28_PLUS).
    % OTP 28 - O(1) memory
    Iterator = erlang:processes_iterator(),
    Count = count_processes_iterator(Iterator, 0),
-else.
    % OTP 27 - O(N) memory
    Count = erlang:system_info(process_count),
-endif.
```

#### Module-Specific Migration

##### erlmcp_tool_router.erl
```erlang
% Updated for PCRE2 compliance
compile_regex(Pattern) ->
    case re:compile(Pattern, [unicode]) of
        {ok, Compiled} ->
            case validate_pcre2_compatibility(Pattern) of
                true -> {ok, Compiled};
                false -> {error, "PCRE2 incompatible pattern"}
            end;
        {error, _} = Error -> Error
    end.

% Validate against PCRE2 breaking changes
validate_pcre2_compatibility(Pattern) ->
    % Check for PCRE-specific patterns that break in PCRE2
    case re:run(Pattern, "\\\\[iB8]", [{capture, none}]) of
        nomatch -> true;
        _ -> false
    end.
```

##### erlmcp_otp_compat.erl
```erlang
% Enhanced for OTP 28
-export([send_priority/2]).

send_priority(Dest, Msg) ->
    case is_otp_28_plus() of
        true ->
            % OTP 28+ with priority support
            PrioAlias = alias([priority]),
            erlang:send(PrioAlias, Msg, [priority]);
        false ->
            % OTP 27 fallback
            erlang:send(Dest, Msg, [nosuspend])
    end.
```

## Deployment Strategies

### Phase 1: Compatibility Layer (Immediate)

1. **Update otp_compat.hrl**
```erlang
% Add OTP 28 support macros
-ifdef(OTP_28_PLUS).
-define(HAVE_PRIORITY_MESSAGES, true).
-define(HAVE_PROCESS_ITERATOR, true).
-define(HAVE_NATIVE_JSON, true).
-else.
-define(HAVE_PRIORITY_MESSAGES, false).
-define(HAVE_PROCESS_ITERATOR, false).
-define(HAVE_NATIVE_JSON, erlang:function_exported(json, encode, 1)).
-endif.
```

2. **Add Version Detection**
```erlang
% In your application's supervision tree
{erlmcp_version_supervisor, start_link, []},
```

### Phase 2: Gradual Feature Adoption

1. **Priority Messages (High Impact)**
```erlang
% Implement in critical modules first
-module(erlmcp_critical_handler).
-behaviour(gen_server).

init([]) ->
    % Enable priority message reception for OTP 28+
    case erlmcp_otp_compat:is_otp_28_plus() of
        true ->
            PrioAlias = alias([priority]),
            {ok, #{priority_alias => PrioAlias}};
        false ->
            {ok, #{}}
    end.
```

2. **Process Iterator Performance**
```erlang
% Replace legacy process enumeration
process_registry_enumeration() ->
    case erlmcp_otp_compat:have_process_iterator() of
        true ->
            % OTP 28 - Optimized
            Iterator = erlang:processes_iterator(),
            collect_registry_entries(Iterator, []);
        false ->
            % OTP 27 - Fallback
            Pids = erlang:processes(),
            lists:filtermap(fun is_registry_process/1, Pids)
    end.
```

### Phase 3: Modern Syntax Adoption

1. **Strict Generators (OTP 28)**
```erlang
% Before - Relaxed generator
Results = [X || {ok, X} <- MaybeResults],

% After - Strict generator for better error handling
Results = [X || {ok, X} <:- MaybeResults],
```

2. **Zip Generators (OTP 28)**
```erlang
% Before - Nested enumeration
[{K, V} || K <- Keys, V <- Values],

% After - Zipped enumeration
[{K, V} || K <- Keys && V <- Values],
```

## Testing and Validation

### Version-Specific Test Suite

```bash
# OTP 28 compatibility tests
./scripts/test-otp28-compatibility.sh

# PCRE2 regex validation
./scripts/test-pcre2-compatibility.sh

# Performance regression testing
./scripts/test-performance-regression.sh
```

### Test Cases by Version

#### OTP 28 Specific Tests
```erlang
-module(erlmcp_otp28_specific_tests).

-include_lib("eunit/include/eunit.hrl").

priority_messages_test() ->
    case erlmcp_otp_compat:is_otp_28_plus() of
        true ->
            % Test priority message functionality
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

process_iterator_test() ->
    case erlmcp_otp_compat:have_process_iterator() of
        true ->
            % Test process iterator performance
            Iterator = erlang:processes_iterator(),
            Count = count_processes_iterator(Iterator, 0),
            is_integer(Count);
        false ->
            skip
    end.

pcre2_compatibility_test() ->
    % Test PCRE2 patterns
    Patterns = [
        "\\w+\\s+(?=\\d+)",  % Lookahead
        "(\\d+)-(\\d+)",     % Groups
        "[\\x00-\\x1F]"      % Character classes
    ],
    lists:foreach(fun(Pattern) ->
                     case re:compile(Pattern, [unicode]) of
                         {ok, _} -> ok;
                         {error, _} ->
                             ?assert(false, "Invalid PCRE2 pattern: " ++ Pattern)
                     end
                  end,
                  Patterns).
```

### Performance Benchmarks

```erlang
% Performance comparison benchmarks
-module(erlmcp_bench_version_specific).

-ifdef(OTP_28_PLUS).
bench_process_iteration() ->
    % OTP 28 - Process iterator benchmark
    Bench = fun() ->
                Iterator = erlang:processes_iterator(),
                count_processes_iterator(Iterator, 0)
            end,
    benchmark(Bench, 1000).
-else.
bench_process_iteration() ->
    % OTP 27 - Legacy enumeration benchmark
    Bench = fun() ->
                Pids = erlang:processes(),
                length(Pids)
            end,
    benchmark(Bench, 1000).
-endif.
```

## Common Issues and Solutions

### Issue 1: PCRE2 Regex Breaking Changes

**Symptom:**
```erlang
% Error in OTP 28
re:run("text", "\\i", [caseless]).  % Bad argument
```

**Solution:**
```erlang
% Validate patterns before compilation
validate_pcre2_pattern(Pattern) ->
    % Test pattern compilation
    case re:compile(Pattern, [unicode]) of
        {ok, _} -> ok;
        {error, Reason} ->
            % Convert PCRE2 error to meaningful message
            case Reason of
                {bad_regex, Details} ->
                    logger:warning("Invalid PCRE2 pattern: ~p", [Details]);
                _ -> ok
            end
    end.
```

### Issue 2: Priority Message Compatibility

**Symptom:**
```erlang
% Error when using priority on OTP <28
erlang:send(PrioAlias, Msg, [priority]).  % badarg
```

**Solution:**
```erlang
% Always use compatibility layer
send_message_with_priority(Target, Message) ->
    case erlmcp_otp_compat:have_priority_messages() of
        true ->
            % OTP 28+ implementation
            PrioAlias = erlmcp_otp_compat:get_priority_alias(),
            erlang:send(PrioAlias, Message, [priority]);
        false ->
            % OTP 27 fallback
            erlang:send(Target, Message, [nosuspend])
    end.
```

### Issue 3: Process Iterator Memory Issues

**Symptom:**
```erlang
% Memory leak with large process counts
Iterator = erlang:processes_iterator(),
process_next(Iterator)  % Memory not freed
```

**Solution:**
```erlang
% Proper iterator usage
safe_process_enumeration() ->
    case erlmcp_otp_compat:have_process_iterator() of
        true ->
            Iterator = erlang:processes_iterator(),
            Results = collect_all(Iterator, []),
            ensure_iterator_cleanup(Iterator),
            Results;
        false ->
            erlang:processes()
    end.

ensure_iterator_cleanup(Iterator) ->
    % Force cleanup
    catch erlang:process_next(Iterator),
    catch erlang:process_next(Iterator),
    ok.
```

## Configuration and Deployment

### Environment Variables

```bash
# Set OTP version for deployment
export OTP_VERSION="28.3.1"

# Enable OTP 28 specific features
export ERLMCP_OTP28_FEATURES="1"

# Set compatibility mode for legacy systems
export ERLMCP_COMPATIBILITY_MODE="otp27"
```

### Release Configuration

```erlang
% relx.config - Version-specific release configuration
{relx,
 [{release,
   {erlmcp, "2.2.0"},
   [erlmcp_core, erlmcp_transports, erlmcp_observability, erlmcp_validation]},
  {sys_config, "./config/sys.config." ++ os:getenv("OTP_VERSION", "28")},
  {vm_args, "./vm.args." ++ os:getenv("OTP_VERSION", "28")}
 ]}.
```

### Docker Configuration

```dockerfile
# Dockerfile - Multi-stage build
FROM erlang:28.3.1-alpine as otp28
# OTP 28 specific build

FROM erlang:27.3.4-alpine as otp27
# OTP 27 fallback build

FROM otp28 as production
# Production deployment with OTP 28
```

## Monitoring and Observability

### Version Monitoring

```erlang
% Add version metrics
-module(erlmcp_version_monitor).

-export([start_monitoring/0]).

start_monitoring() ->
    spawn(fun() -> monitor_versions() end).

monitor_versions() ->
    timer:send_interval(60000, monitor_current_version),
    monitor_loop().

monitor_loop() ->
    receive
        monitor_current_version ->
            OTPVersion = erlang:system_info(otpr_release),
            Features = detect_available_features(),
            send_metrics(OTPVersion, Features),
            monitor_loop()
    end.

detect_available_features() ->
    [
        {native_json, erlmcp_otp_compat:have_native_json()},
        {priority_messages, erlmcp_otp_compat:have_priority_messages()},
        {process_iterator, erlmcp_otp_compat:have_process_iterator()}
    ].
```

### Performance Tracking

```erlang
% Track version-specific performance
-record(perf_metrics, {
    otp_version,
    feature_usage,
    process_count,
    memory_usage,
    timing_data
}).

track_performance(Feature, Timing) ->
    Metrics = #perf_metrics{
        otp_version = erlang:system_info(otpr_release),
        feature_usage = Feature,
        process_count = erlang:system_info(process_count),
        memory_usage = erlang:memory(total),
        timing_data = Timing
    },
    send_metrics(Metrics).
```

## Troubleshooting Guide

### Common Error Messages

#### "OTP version too old"
```bash
# Solution: Check minimum version requirement
rebar3 compile
# Look for minimum_otp_vsn in rebar.config
```

#### "PCRE2 pattern error"
```bash
# Solution: Validate patterns
re:compile(Pattern, [unicode])  % Test compilation
```

#### "Priority message not supported"
```bash
# Solution: Check runtime version
erlmcp_otp_compat:have_priority_messages()
```

### Debug Commands

```bash
# Check OTP version
erl +V

# List loaded features
erl +features

# Test regex compilation
erl -eval "io:format(\"~p~n\", [re:compile(\"\\\\w+\", [unicode])])" -s init stop

# Test process iterator availability
erl -eval "io:format(\"~p~n\", [erlang:function_exported(erlang, processes_iterator, 0)])" -s init stop
```

## Conclusion

This migration guide provides a comprehensive path for erlmcp to adopt OTP 28 while maintaining backward compatibility. Key recommendations:

1. **Adopt OTP 28.3.1** for optimal performance and features
2. **Maintain OTP 27 compatibility** for legacy systems
3. **Use compatibility layer** for smooth transitions
4. **Test thoroughly** before production deployment
5. **Monitor performance** and feature usage

By following this guide, erlmcp can leverage modern OTP features while ensuring stability across different versions.

---

**Related Documents:**
- [OTP Compatibility Analysis](./OTP_COMPATIBILITY_ANALYSIS.md)
- [erlmcp otp_compat.hrl](https://github.com/ruvnet/erlmcp/blob/main/include/otp_compat.hrl)
- [Erlang/OTP 28 Release Notes](https://www.erlang.org/blog/highlights-otp-28/)