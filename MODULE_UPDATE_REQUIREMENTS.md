# Module Update Requirements for OTP 26-28 Compatibility

## Implementation Plan

This document provides detailed requirements for updating each erlmcp module to support OTP versions 26, 27, and 28 with graceful degradation and performance optimizations.

## Phase 1: Foundation Updates

### 1.1 Update rebar.config

**File**: `/Users/sac/erlmcp/rebar.config`

**Required Changes**:
```erlang
%% Update minimum OTP version
{minimum_otp_vsn, "26"}.

%% Add platform defines for multi-version support
{platform_define,
    "^2[6]", 'OTP_26'},              % OTP 26 specific features
{platform_define,
    "^2[7]", 'OTP_27'},              % OTP 27 specific features
{platform_define,
    "^2[8-9]|^[3-9]", 'OTP_28_PLUS'} % OTP 28+ features

%% Update dependencies to be version-aware
{deps,
 [% JSON encoding - conditional on OTP version
  {jsx, "3.1.0", {optional, true}},  % Required for OTP <28
  % JSON Schema validation
  {jesse, "1.8.1"},
  % Process registry
  {gproc, "0.9.0"},
  % HTTP client
  {gun, "2.0.1"},
  % TCP acceptor pool
  {ranch, "2.1.0"},
  % Connection pooling
  {poolboy, "1.5.2"},
  % HTTP server
  {cowboy, "2.10.0"},
  % Template engine
  {bbmustache, "1.12.2"},
  % JWT validation
  {jose, "1.11.1"},
  % OpenTelemetry
  {opentelemetry_api, "1.5.0"},
  {opentelemetry, "1.7.0"},
  {opentelemetry_exporter, "1.10.0"}]}.
```

### 1.2 Update otp_compat.hrl

**File**: `/Users/sac/erlmcp/include/otp_compat.hrl`

**Required Changes**:
- Add OTP_26 and OTP_27 support
- Enhance feature detection
- Add version-specific macros

**Additions**:
```erlang
%%====================================================================
%% Version Macros
%%====================================================================
%% These macros are defined via platform_define in rebar.config:
%%
%% {platform_define, "^2[6]", 'OTP_26'}
%% {platform_define, "^2[7]", 'OTP_27'}
%% {platform_define, "^2[8-9]|^[3-9]", 'OTP_28_PLUS'}
%%
%% Available macros:
%%   - OTP_26: Defined for OTP 26.x
%%   - OTP_27: Defined for OTP 27.x
%%   - OTP_28_PLUS: Defined for OTP 28.3.1+

%%-----------------------------------------------------------------------------
%% Version Detection
%%-----------------------------------------------------------------------------
-ifdef(OTP_28_PLUS).
-define(IS_OTP_28_PLUS(), true).
-define(IS_OTP_27_PLUS(), true).
-define(IS_OTP_26_PLUS(), true).
-else.
-ifdef(OTP_27).
-define(IS_OTP_28_PLUS(), false).
-define(IS_OTP_27_PLUS(), true).
-define(IS_OTP_26_PLUS(), true).
-else.
-ifdef(OTP_26).
-define(IS_OTP_28_PLUS(), false).
-define(IS_OTP_27_PLUS(), false).
-define(IS_OTP_26_PLUS(), true).
-else.
-define(IS_OTP_28_PLUS(), false).
-define(IS_OTP_27_PLUS(), false).
-define(IS_OTP_26_PLUS(), false).
-endif.
-endif.
-endif.

%%-----------------------------------------------------------------------------
%% JSON Module Support
%%-----------------------------------------------------------------------------
-ifdef(OTP_27_PLUS).
-define(HAVE_NATIVE_JSON(), true).
-else.
-define(HAVE_NATIVE_JSON(), false).
-endif.

%%-----------------------------------------------------------------------------
%% Process Iterator Support
%%-----------------------------------------------------------------------------
-ifdef(OTP_28_PLUS).
-define(HAVE_PROCESS_ITERATOR(), true).
-else.
-define(HAVE_PROCESS_ITERATOR(), false).
-endif.

%%-----------------------------------------------------------------------------
%% Priority Messages Support
%%-----------------------------------------------------------------------------
-ifdef(OTP_28_PLUS).
-define(HAVE_PRIORITY_MESSAGES(), true).
-else.
-define(HAVE_PRIORITY_MESSAGES(), false).
-endif.
```

## Phase 2: Core Module Updates

### 2.1 erlmcp_json_native.erl

**File**: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_json_native.erl`

**Issue**: OTP 27+ only module breaks on OTP 26

**Solution**: Add version guards and rename for clarity

**Update Requirements**:
```erlang
-module(erlmcp_json_native).

%% @doc Native JSON encoding/decoding using Erlang OTP 27+ json module
%% Provides graceful fallback to jsx for OTP 26
%%
%% OTP 26: Uses jsx
%% OTP 27+: Uses native json module
%%

-include("otp_compat.hrl").

-export([encode/1, encode/2, decode/1, decode/2]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Encode Erlang term to JSON binary
%% Automatically selects native json or jsx based on OTP version
-spec encode(json_term()) -> binary().
encode(Term) ->
    ?JSON_ENCODE(Term).

%% @doc Encode Erlang term to JSON binary with options
-spec encode(json_term(), encode_options()) -> binary().
encode(Term, Opts) ->
    case ?HAVE_NATIVE_JSON() of
        true ->
            %% Native JSON ignores options for API compatibility
            iolist_to_binary(json:encode(Term));
        false ->
            %% Use jsx with options
            jsx:encode(Term, Opts)
    end.

%% @doc Decode JSON binary to Erlang term
-spec decode(binary()) -> json_term().
decode(Binary) when is_binary(Binary) ->
    ?JSON_DECODE(Binary).

%% @doc Decode JSON binary to Erlang term with options
-spec decode(binary(), decode_options()) -> json_term().
decode(Binary, Opts) when is_binary(Binary) ->
    case ?HAVE_NATIVE_JSON() of
        true ->
            %% Native JSON ignores options (always returns maps)
            json:decode(Binary);
        false ->
            %% Use jsx with options
            jsx:decode(Binary, Opts)
    end.
```

### 2.2 erlmcp_process_monitor.erl

**File**: `/Users/sac/erlmcp/apps/erlmcp_observability/src/erlmcp_process_monitor.erl`

**Issue**: Uses OTP 28 `processes_iterator()` directly

**Solution**: Use otp_compat macros for version-agnostic implementation

**Update Requirements**:
```erlang
%% @doc Enumerate all processes with categorization
%% Uses otp_compat macros for cross-version compatibility
%% OTP 26-27: Uses erlang:processes() with memory warning
%% OTP 28+: Uses erlang:processes_iterator() for O(1) memory
%%
%% Returns {ok, Count} to reflect streaming nature.
%% @end
%%--------------------------------------------------------------------
-spec enumerate_processes() -> {ok, non_neg_integer()}.
enumerate_processes() ->
    ?SAFE_PROCESS_COUNT().

%% @doc Categorize processes by type
%% Uses otp_compat macros for cross-version compatibility
%% OTP 26-27: Uses erlang:processes()
%% OTP 28+: Uses erlang:processes_iterator()
%%
%% Returns: {ok, #{erlmcp_count => N, system_count => M}}
%% @end
%%--------------------------------------------------------------------
-spec categorize_processes() -> {ok, map()}.
categorize_processes() ->
    Processes = ?SAFE_PROCESSES(),
    categorize_processes_list(Processes, #{erlmcp_count => 0, system_count => 0}).

%% @private Helper for process categorization using list
-spec categorize_processes_list([pid()], map()) -> {ok, map()}.
categorize_processes_list([], Acc) ->
    {ok, Acc};
categorize_processes_list([Pid | Rest], Acc) ->
    case process_info(Pid, [registered_name, initial_call]) of
        undefined ->
            categorize_processes_list(Rest, Acc#{system_count => Acc#{system_count} := Acc#{system_count} + 1});
        Info ->
            IsErlmcp = is_erlmcp_process(Info),
            Key = case IsErlmcp of
                true -> erlmcp_count;
                false -> system_count
            end,
            categorize_processes_list(Rest, Acc#{Key := Acc#{Key} + 1})
    end.

%% @private Check if process is erlmcp-related
-spec is_erlmcp_process(list()) -> boolean().
is_erlmcp_process(Info) ->
    %% Check for erlmcp-specific process names and modules
    lists:any(fun check_erlmcp_info/1, Info).

check_erlmcp_info({registered_name, Name}) ->
    is_binary(Name) and binary:match(Name, <<"erlmcp">>) =/= nomatch;
check_erlmcp_info({initial_call, {Module, _, _}}) ->
    atom_to_list(Module) =:= "erlmcp" orelse
    case atom_to_list(Module) of
        "erlmcp_" ++ _ -> true;
        _ -> false
    end;
check_erlmcp_info(_) ->
    false.
```

### 2.3 erlmcp_graceful_drain.erl

**File**: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_graceful_drain.erl`

**Issue**: Uses priority messages not available on OTP <28

**Solution**: Use otp_compat macros with graceful degradation

**Update Requirements**:
```erlang
%% @doc Priority shutdown message with version-aware handling
%% OTP 28+: Uses priority messages for immediate preemption
%% OTP 26-27: Uses regular messages with warning log
%%
%% @end
handle_info({priority_shutdown, TimeoutMs}, State) ->
    StartTime = erlang:monotonic_time(microsecond),

    % Log priority level based on OTP version
    case ?HAVE_PRIORITY_MESSAGES() of
        true ->
            ?LOG_WARNING("PRIORITY shutdown initiated (OTP 28+ priority messages, timeout: ~pms)", [TimeoutMs]);
        false ->
            ?LOG_WARNING("PRIORITY shutdown initiated (fallback mode - OTP <28, timeout: ~pms)", [TimeoutMs])
    end,

    % Immediately stop accepting new connections
    NewState = State#state{shutdown_requested = true},

    % Start drain timer
    erlang:send_after(TimeoutMs, self(), shutdown_timeout),

    EndTime = erlang:monotonic_time(microsecond),
    LatencyUs = EndTime - StartTime,

    FinalState =
        NewState#state{priority_messages_delivered = State#state.priority_messages_delivered + 1,
                       priority_latency_sum_us = State#state.priority_latency_sum_us + LatencyUs},

    {noreply, FinalState};

%% @doc Send priority message with version awareness
-spec send_priority_shutdown(term()) -> ok.
send_priority_shutdown(TimeoutMs) ->
    case ?HAVE_PRIORITY_MESSAGES() of
        true ->
            % Use priority message on OTP 28+
            self() ! {priority_shutdown, TimeoutMs},
            ?SET_PRIORITY_HIGH();
        false ->
            % Use regular message on OTP <28
            self() ! {priority_shutdown, TimeoutMs}
    end.
```

## Phase 3: Transport Module Updates

### 3.1 erlmcp_transport_ws.erl

**File**: `/Users/sac/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_ws.erl`

**Issue**: Uses JSON encoding for WebSocket messages

**Solution**: Use otp_compat macros for JSON operations

**Update Requirements**:
```erlang
%% @doc Encode WebSocket message using version-aware JSON
-spec encode_ws_message(map()) -> binary().
encode_ws_message(Message) ->
    ?JSON_ENCODE(Message).

%% @doc Decode WebSocket message using version-aware JSON
-spec decode_ws_message(binary()) -> map().
decode_ws_message(Binary) ->
    ?JSON_DECODE(Binary).
```

### 3.2 erlmcp_transport_http.erl

**File**: `/Users/sac/erlmcp/apps/erlmcp_cli/src/erlmcp_transport_http.erl`

**Issue**: JSON encoding for HTTP payloads

**Solution**: Use otp_compat macros

**Update Requirements**:
```erlang
%% @doc Encode HTTP payload using version-aware JSON
-spec encode_http_payload(map()) -> binary().
encode_http_payload(Payload) ->
    ?JSON_ENCODE(Payload).

%% @doc Decode HTTP payload using version-aware JSON
-spec decode_http_payload(binary()) -> map().
decode_http_payload(Binary) ->
    ?JSON_DECODE(Binary).
```

## Phase 4: CLI Module Updates

### 4.1 erlmcp_cli_json_rpc.erl

**File**: `/Users/sac/erlmcp/apps/erlmcp_cli/src/erlmcp_cli_json_rpc.erl`

**Issue**: Direct JSON module usage

**Solution**: Use otp_compat macros

**Update Requirements**:
```erlang
%% @doc Encode JSON-RPC request using version-aware JSON
-spec encode_json_rpc(map()) -> binary().
encode_json_rpc(Request) ->
    ?JSON_ENCODE(Request).

%% @doc Decode JSON-RPC response using version-aware JSON
-spec decode_json_rpc(binary()) -> map().
decode_json_rpc(Response) ->
    ?JSON_DECODE(Response).
```

## Phase 5: Testing Infrastructure

### 5.1 Create Version-Specific Test Configurations

**File**: `/Users/sac/erlmcp/config/test.config`

**Required**:
```erlang
%% OTP 26 test configuration
{otp_26,
 [{erl_opts, [debug_info, {d, 'OTP_26'}, {i, "include"}]},
  {deps, [{jsx, "3.1.0"}]}]}.

%% OTP 27 test configuration
{otp_27,
 [{erl_opts, [debug_info, {d, 'OTP_27'}, {i, "include"}]},
  {deps, [{jsx, "3.1.0", {optional, true}}]}]}.

%% OTP 28 test configuration
{otp_28,
 [{erl_opts, [debug_info, {d, 'OTP_28_PLUS'}, {i, "include"}]},
  {deps, [{jsx, "3.1.0", {optional, true}}]}]}.
```

### 5.2 Update Common Test Suites

**File**: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_cross_otp_compat_SUITE.erl`

**New Suite Required**:
```erlang
-module(erlmcp_cross_otp_compat_SUITE).

%% Test suite for cross-OTP compatibility
%% Tests behavior across OTP 26, 27, and 28
%% Ensures graceful degradation and feature availability

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_json_compatibility/1, test_process_enumeration/1,
         test_priority_messages/1, test_feature_detection/1]).

all() ->
    [test_json_compatibility, test_process_enumeration,
     test_priority_messages, test_feature_detection].

init_per_suite(Config) ->
    %% Initialize OTP version detection
    OTPVersion = erlang:system_info(otp_release),
    [{otp_version, OTPVersion} | Config].

test_json_compatibility(Config) ->
    %% Test JSON encoding/decoding works on all OTP versions
    TestData = #{<<"test">> => <<"data">>, <<"number">> => 42},
    JSON = ?JSON_ENCODE(TestData),
    Parsed = ?JSON_DECODE(JSON),
    ?assert(Parsed =:= TestData).

test_process_enumeration(Config) ->
    %% Test process enumeration works on all OTP versions
    {ok, Count} = erlmcp_process_monitor:enumerate_processes(),
    ?assert(is_integer(Count) andalso Count >= 0).

test_priority_messages(Config) ->
    %% Test priority message graceful degradation
    case ?HAVE_PRIORITY_MESSAGES() of
        true ->
            %% Test priority message functionality
            erlmcp_graceful_drain:send_priority_shutdown(1000);
        false ->
            %% Test fallback behavior
            ?LOG_INFO("Priority messages not available - testing fallback")
    end.

test_feature_detection(Config) ->
    %% Test feature detection accuracy
    ?assert(?HAVE_NATIVE_JSON() =:= (erlang:system_info(otp_release) >= "27")),
    ?assert(?HAVE_PROCESS_ITERATOR() =:= (erlang:system_info(otp_release) >= "28")),
    ?assert(?HAVE_PRIORITY_MESSAGES() =:= (erlang:system_info(otp_release) >= "28")).
```

## Phase 6: Documentation Updates

### 6.1 Version Compatibility Documentation

**File**: `/Users/sac/erlmcp/docs/VERSION_COMPATIBILITY.md`

**Required Content**:
```markdown
# erlmcp Version Compatibility Guide

## Supported OTP Versions

| OTP Version | Status | Features Available | Notes |
|-------------|--------|-------------------|-------|
| 26.x | ✅ Supported | Basic JSON, Process Enumeration | No priority messages |
| 27.x | ✅ Supported | Native JSON, Process Enumeration | No priority messages |
| 28.x | ✅ Supported | All Features | Full feature set |
| 28.3.1+ | ✅ Recommended | All Features + Optimizations | Recommended for production |

## Feature Availability

### JSON Support
- **OTP 26**: Uses `jsx` library
- **OTP 27**: Uses native `json` module (preferred)
- **OTP 28**: Uses native `json` module with performance optimizations

### Process Enumeration
- **OTP 26-27**: Uses `erlang:processes()` (O(N) memory)
- **OTP 28**: Uses `erlang:processes_iterator()` (O(1) memory)

### Priority Messages
- **OTP 26-27**: Not available (graceful degradation)
- **OTP 28**: Available with `process_flag(priority, high)`

## Migration Guide

### Upgrading from OTP 26 to 27
1. Update `rebar.config` minimum version to "27"
2. Test JSON encoding performance improvements
3. No API changes required

### Upgrading from OTP 27 to 28
1. Update `rebar.config` minimum version to "28"
2. Enable priority message support in configuration
3. Test process enumeration performance improvements

### Downgrading from OTP 28 to 27
1. Update `rebar.config` minimum version to "27"
2. Priority messages will be gracefully degraded
3. Performance impact on process enumeration

## Testing Multi-Version Support

```bash
# Test with OTP 26
export ERLMCP_OTP_VERSION=26
rebar3 ct --suite=erlmcp_cross_otp_compat_SUITE

# Test with OTP 27
export ERLMCP_OTP_VERSION=27
rebar3 ct --suite=erlmcp_cross_otp_compat_SUITE

# Test with OTP 28
export ERLMCP_OTP_VERSION=28
rebar3 ct --suite=erlmcp_cross_otp_compat_SUITE
```
```

## Implementation Timeline

### Week 1: Foundation
- [ ] Update rebar.config with platform defines
- [ ] Enhance otp_compat.hrl with version detection
- [ ] Create cross-version test configuration

### Week 2: Core Modules
- [ ] Update erlmcp_json_native.erl with version guards
- [ ] Update erlmcp_process_monitor.erl with otp_compat macros
- [ ] Update erlmcp_graceful_drain.erl with graceful degradation

### Week 3: Transport and CLI Modules
- [ ] Update all JSON usage to use otp_compat macros
- [ ] Test transport layer compatibility
- [ ] Test CLI compatibility across versions

### Week 4: Testing and Documentation
- [ ] Create cross-OTP test suites
- [ ] Run comprehensive testing on all versions
- [ ] Update documentation and migration guides
- [ ] Create CI/CD pipeline for multi-version testing

## Success Criteria

1. **Compilation**: All modules compile successfully on OTP 26, 27, 28
2. **Functionality**: All features work with graceful degradation
3. **Performance**: Native performance optimizations used where available
4. **Testing**: 100% test coverage across all versions
5. **Documentation**: Complete compatibility documentation

## Risk Mitigation

### High Risk Items
1. **JSON Native Module**: Test extensively on OTP 26
2. **Process Enumeration**: Memory usage testing on large systems
3. **Priority Messages**: System behavior testing without priority

### Mitigation Strategies
1. **Feature Flags**: Gradual rollout of version-specific features
2. **Fallback Systems**: Maintain fallback implementations
3. **Comprehensive Testing**: Multi-version test matrix
4. **Performance Monitoring**: Continuous performance benchmarks

This implementation plan ensures erlmcp maintains full functionality across OTP versions while providing optimal performance on newer versions.