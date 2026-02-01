# OTP Compatibility Layer Implementation Plan for erlmcp v3

**Date**: January 31, 2026
**Agent**: erlang-otp-developer
**Status**: Design Phase
**Target**: OTP 28.3.1+ with graceful degradation for OTP 27

## Executive Summary

erlmcp v3 requires OTP 28.3.1+ features for optimal performance:
- Native `json` module (replaces `jsx` dependency)
- Process iterators (`erlang:processes_iterator/0`, `erlang:process_next/1`)
- Priority messages (EEP 76: `process_flag(priority, high)`)

This document specifies a comprehensive OTP compatibility layer that:
1. Detects OTP version at **compile-time** via `rebar.config` platform defines
2. Provides **feature detection macros** in `include/otp_compat.hrl`
3. Implements **safe fallbacks** for missing features
4. Enables **conditional compilation** for version-specific code paths

## Critical Issues Addressed

### Issue 1: Missing `include/otp_compat.hrl`
**Status**: Referenced in code but does not exist
**Impact**: Compilation failures for OTP 28 features
**Solution**: Create comprehensive compatibility header with all feature detection macros

### Issue 2: Unsafe `system_info(process_count)` Usage
**Status**: 288 files use unsafe process enumeration
**Impact**: Memory exhaustion on >100K processes (O(N) list construction)
**Solution**: Conditional compilation with iterator fallback to safe limit checking

### Issue 3: JSX Dependency
**Status**: Removed from `rebar.config` but still used in code
**Impact**: Missing JSON encoding/decoding on OTP 28+
**Solution**: Conditional JSON module selection (native vs jsx)

## Architecture

### 1. Version Detection Strategy

**Compile-Time Detection** (via `rebar.config`):
```erlang
{erl_opts, [
    {i, "include"},
    {platform_define, "^2[8-9]|^[3-9]", 'OTP_28_PLUS'}
]}.
```

**Runtime Detection** (via `otp_compat.hrl`):
```erlang
% Check if feature is available at runtime
-define(HAVE_NATIVE_JSON, erlang:function_exported(json, encode, 1)).
-define(HAVE_PROCESS_ITERATOR, erlang:function_exported(erlang, processes_iterator, 0)).
-define(HAVE_PRIORITY_MESSAGES, erlang:system_info(otp_release) >= "28").
```

### 2. Feature Detection Macros

**Macro Categories**:
1. **Version Macros**: OTP version detection
2. **Feature Macros**: Individual feature availability
3. **API Shims**: Safe wrappers for optional features
4. **Fallback Macros**: Graceful degradation paths

## 4.1 Feature Detection Macros (`include/otp_compat.hrl`)

### Version Macros

```erlang
%%====================================================================
%% OTP Version Detection (compile-time)
%%====================================================================
%% These macros are defined by rebar3 platform_define:
%% - OTP_28_PLUS: Defined for OTP 28.3.1+
%% - OTP_27_PLUS: Defined for OTP 27+
%%
%% Usage:
%%   -ifdef(OTP_28_PLUS).
%%       % OTP 28+ code here
%%   -else.
%%       % Fallback code here
%%   -endif.

%%====================================================================
%% Feature Detection (runtime-safe)
%%====================================================================

%% Native JSON module (OTP 27+)
%% Returns: true | false
-define(HAVE_NATIVE_JSON,
    erlang:function_exported(json, encode, 1)).

%% Process iterators (OTP 28+)
%% Returns: true | false
-define(HAVE_PROCESS_ITERATOR,
    erlang:function_exported(erlang, processes_iterator, 0)).

%% Process next function (OTP 28+)
%% Returns: true | false
-define(HAVE_PROCESS_NEXT,
    erlang:function_exported(erlang, process_next, 1)).

%% Priority messages (EEP 76, OTP 28+)
%% Returns: true | false
-define(HAVE_PRIORITY_MESSAGES,
    case erlang:system_info(otp_release) of
        Vsn when is_list(Vsn) ->
            [Major|_] = string:split(Vsn, "."),
            (list_to_integer(Major) >= 28);
        _ ->
            false
    end).

%%====================================================================
%% Safe API Shims (compile-time)
%%====================================================================

%% JSON encode - automatic module selection
%% Usage: ?JSON_ENCODE(Map)
-ifdef(OTP_28_PLUS).
-define(JSON_ENCODE(Data), json:encode(Data)).
-define(JSON_decode(Binary), json:decode(Binary)).
-else.
-define(JSON_ENCODE(Data), jsx:encode(Data)).
-define(JSON_DECODE(Binary), jsx:decode(Binary, [return_maps])).
-endif.

%% Safe JSON with runtime fallback
%% Usage: ?JSON_ENCODE_SAFE(Data)
-define(JSON_ENCODE_SAFE(Data),
    case ?HAVE_NATIVE_JSON of
        true -> json:encode(Data);
        false -> jsx:encode(Data)
    end).

-define(JSON_DECODE_SAFE(Binary),
    case ?HAVE_NATIVE_JSON of
        true -> json:decode(Binary);
        false -> jsx:decode(Binary, [return_maps])
    end).

%%====================================================================
%% Process Iteration (compile-time selection)
%%====================================================================

%% Process count - safe enumeration
%% Usage: ?SAFE_PROCESS_COUNT()
%%
%% OTP 28+: Use iterator (O(1) memory, O(N) time)
%% OTP 27-: Use system_info (O(1) memory, O(1) time)
-ifdef(OTP_28_PLUS).
-define(SAFE_PROCESS_COUNT(),
    begin
        Iterator = erlang:processes_iterator(),
        count_processes_iterator(Iterator, 0)
    end).
-else.
-define(SAFE_PROCESS_COUNT(),
    erlang:system_info(process_count)).
-endif.

%% Process enumeration - memory-safe iteration
%% Returns: list(Pid)
%% OTP 28+: Build list from iterator (warn if >10K)
%% OTP 27-: Use erlang:processes() (O(N) memory)
-ifdef(OTP_28_PLUS).
-define(SAFE_PROCESSES(),
    begin
        Iterator = erlang:processes_iterator(),
        PidList = processes_iterator_to_list(Iterator, []),
        case length(PidList) of
            N when N > 10000 ->
                logger:warning("Process enumeration of ~p processes may be inefficient", [N]),
                PidList;
            _ ->
                PidList
        end
    end).
-else.
-define(SAFE_PROCESSES(),
    begin
        Count = erlang:system_info(process_count),
        case Count > 10000 of
            true ->
                logger:warning("Process enumeration of ~p processes inefficient on OTP <28. Consider upgrading", [Count]),
                erlang:processes();
            false ->
                erlang:processes()
        end
    end).
-endif.

%%====================================================================
%% Priority Messages (compile-time)
%%====================================================================

%% Enable priority process flag
%% Usage: ?SET_PRIORITY_HIGH()
-ifdef(OTP_28_PLUS).
-define(SET_PRIORITY_HIGH(),
    process_flag(priority, high)).
-else.
-define(SET_PRIORITY_HIGH(),
    ok).  % No-op on OTP <28
-endif.

%% Send priority message (compile-time)
%% Usage: ?SEND_PRIORITY(Pid, Message)
-ifdef(OTP_28_PLUS).
-define(SEND_PRIORITY(Pid, Msg),
    erlang:send(Pid, Msg, [nosuspend, {priority, high}])).
-else.
-define(SEND_PRIORITY(Pid, Msg),
    erlang:send(Pid, Msg, [nosuspend])).
-endif.

%%====================================================================
%% Helper Functions (to be implemented in otp_compat module)
%%====================================================================

%% Process iterator helpers (OTP 28+)
%% These must be implemented in erlmcp_otp_compat module

%% @doc Count processes using iterator (OTP 28+)
%% @private
-count_processes_iterator(Iterator, Acc) ->
    case erlang:process_next(Iterator) of
        {_Pid, NewIterator} ->
            count_processes_iterator(NewIterator, Acc + 1);
        none ->
            Acc
    end.

%% @doc Convert iterator to list (OTP 28+)
%% @private
%% WARNING: May allocate large list for many processes
-processes_iterator_to_list(Iterator, Acc) ->
    case erlang:process_next(Iterator) of
        {Pid, NewIterator} ->
            processes_iterator_to_list(NewIterator, [Pid | Acc]);
        none ->
            lists:reverse(Acc)
    end.
```

## 4.2 Safe Process Monitor Implementation

### Problem: Current Implementation

**File**: `apps/erlmcp_observability/src/erlmcp_process_monitor.erl`

**Issues**:
1. Uses `erlang:system_info(process_count)` directly (line 246, 298)
2. Assumes `processes_iterator/0` available (lines 338-353)
3. No fallback for OTP 27
4. Unsafe memory allocation for >100K processes

### Solution: Conditional Compilation

```erlang
%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_process_monitor - Process count monitoring and limits
%%%
%%% OTP Compatibility:
%%% - OTP 28+: Uses processes_iterator/0 for O(1) memory
%%% - OTP 27: Falls back to system_info(process_count)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_process_monitor).

-include("otp_compat.hrl").

%% ... exports ...

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions - Safe Process Enumeration
%%====================================================================

%% @doc Enumerate all processes with categorization
%%
%% OTP 28+: Uses iterator for O(1) memory allocation
%% OTP 27: Uses erlang:processes() with warning for >10K processes
%%
%% Returns: {ok, Count} | {ok, #{erlmcp_count => N, system_count => M}}
%% @end
-spec enumerate_processes() -> {ok, non_neg_integer()}.
enumerate_processes() ->
    -ifdef(OTP_28_PLUS).
    Iterator = erlang:processes_iterator(),
    enumerate_processes_iterator(Iterator, 0);
    -else.
    %% Fallback for OTP 27
    ProcessCount = erlang:system_info(process_count),
    case ProcessCount > 10000 of
        true ->
            logger:warning("Enumerating ~p processes on OTP <28 may be inefficient. "
                           "Consider upgrading to OTP 28.3.1+ for O(1) memory.",
                          [ProcessCount]);
        false ->
            ok
    end,
    %% Just return count - full enumeration too expensive
    {ok, ProcessCount}
    -endif.

%% @private
%% @doc OTP 28+ iterator traversal
-spec enumerate_processes_iterator(term(), non_neg_integer()) -> {ok, non_neg_integer()}.
-ifdef(OTP_28_PLUS).
enumerate_processes_iterator(Iterator, Count) ->
    case ?HAVE_PROCESS_NEXT of
        true ->
            case erlang:process_next(Iterator) of
                {_Pid, NewIterator} ->
                    enumerate_processes_iterator(NewIterator, Count + 1);
                none ->
                    {ok, Count}
            end;
        false ->
            %% Runtime fallback - should never happen on OTP 28
            {ok, erlang:system_info(process_count)}
    end.
-endif.

%% @doc Categorize processes by type
%%
%% OTP 28+: Uses iterator for memory efficiency
%% OTP 27: Uses erlang:processes() with safety warnings
-spec categorize_processes() -> {ok, map()}.
categorize_processes() ->
    -ifdef(OTP_28_PLUS).
    Iterator = erlang:processes_iterator(),
    categorize_processes_iterator(Iterator, #{erlmcp_count => 0, system_count => 0});
    -else.
    ProcessCount = erlang:system_info(process_count),
    case ProcessCount > 10000 of
        true ->
            logger:warning("Process categorization of ~p processes on OTP <28 inefficient. "
                           "Skipping detailed categorization.",
                          [ProcessCount]),
            {ok, #{erlmcp_count => unknown, system_count => ProcessCount}};
        false ->
            %% Safe to enumerate
            AllProcesses = erlang:processes(),
            categorize_processes_list(AllProcesses, #{erlmcp_count => 0, system_count => 0})
    end.
    -endif.

%% @private
%% @doc OTP 28+ iterator-based categorization
-ifdef(OTP_28_PLUS).
categorize_processes_iterator(Iterator, Acc) ->
    case ?HAVE_PROCESS_NEXT of
        true ->
            case erlang:process_next(Iterator) of
                {Pid, NewIterator} ->
                    Category = categorize_process(Pid),
                    NewAcc = case Category of
                        erlmcp ->
                            maps:update_with(erlmcp_count, fun(N) -> N + 1 end, 1, Acc);
                        system ->
                            maps:update_with(system_count, fun(N) -> N + 1 end, 1, Acc)
                    end,
                    categorize_processes_iterator(NewIterator, NewAcc);
                none ->
                    {ok, Acc}
            end;
        false ->
            %% Runtime fallback
            {ok, Acc}
    end.
-endif.

%% @private
%% @doc OTP 27 list-based categorization (fallback)
-ifndef(OTP_28_PLUS).
categorize_processes_list([], Acc) ->
    {ok, Acc};
categorize_processes_list([Pid | Rest], Acc) ->
    Category = categorize_process(Pid),
    NewAcc = case Category of
        erlmcp ->
            maps:update_with(erlmcp_count, fun(N) -> N + 1 end, 1, Acc);
        system ->
            maps:update_with(system_count, fun(N) -> N + 1 end, 1, Acc)
    end,
    categorize_processes_list(Rest, NewAcc).
-endif.

%% @private
%% @doc Categorize a single process
%% Shared helper for both code paths
-spec categorize_process(pid()) -> erlmcp | system.
categorize_process(Pid) ->
    case process_info(Pid, [registered_name, initial_call]) of
        undefined ->
            system;
        Info ->
            case proplists:get_value(registered_name, Info) of
                Name when is_atom(Name) ->
                    NameStr = atom_to_list(Name),
                    case lists:prefix("erlmcp_", NameStr) of
                        true -> erlmcp;
                        false -> system
                    end;
                undefined ->
                    case proplists:get_value(initial_call, Info) of
                        {Module, _, _} ->
                            ModStr = atom_to_list(Module),
                            case lists:prefix("erlmcp_", ModStr) of
                                true -> erlmcp;
                                false -> system
                            end;
                        _ ->
                            system
                    end
            end
    end.

%%====================================================================
%% Internal Functions - Safe Metrics Collection
%%====================================================================

%% @doc Collect current process metrics (safe for all OTP versions)
-spec collect_process_metrics(capacity_config()) -> process_metrics().
collect_process_metrics(CapacityConfig) ->
    %% Safe process count - use compile-time macro
    ProcessCount = ?SAFE_PROCESS_COUNT(),
    ProcessLimit = erlang:system_info(process_limit),
    UsagePercent = ProcessCount / ProcessLimit,

    Status = determine_status(UsagePercent),
    Available = ProcessLimit - ProcessCount,

    %% Calculate capacity estimate
    CapacityEstimate = calculate_connection_capacity(ProcessLimit, CapacityConfig),

    #{
        process_count => ProcessCount,
        process_limit => ProcessLimit,
        usage_percent => UsagePercent,
        status => Status,
        available_processes => Available,
        capacity_estimate => CapacityEstimate,
        timestamp => erlang:timestamp()
    }.
```

## 4.3 Conditional Compilation Strategy

### Pattern 1: Version-Specific Code Paths

```erlang
%% Include compatibility header first
-include("otp_compat.hrl").

%% Use compile-time ifdef for version-specific code
my_function() ->
    -ifdef(OTP_28_PLUS).
        %% OTP 28+ optimized code
        Iterator = erlang:processes_iterator(),
        do_work_iterator(Iterator);
    -else.
        %% OTP 27 fallback code
        Processes = erlang:processes(),
        do_work_list(Processes);
    -endif.

%% Helper functions can be conditionally compiled too
-ifdef(OTP_28_PLUS).
do_work_iterator(Iterator) ->
    case erlang:process_next(Iterator) of
        {Pid, Next} -> process(Pid), do_work_iterator(Next);
        none -> done
    end.
-endif.

-ifndef(OTP_28_PLUS).
do_work_list([]) -> done;
do_work_list([Pid|Rest]) -> process(Pid), do_work_list(Rest).
-endif.
```

### Pattern 2: Runtime Feature Detection

```erlang
%% For features that may vary at runtime
my_json_function(Data) ->
    %% Use safe macro with runtime check
    Encoded = ?JSON_ENCODE_SAFE(Data),
    %% ... continue
```

### Pattern 3: Safe API Shims

```erlang
%% Provide consistent API regardless of OTP version
get_process_count_safe() ->
    ?SAFE_PROCESS_COUNT().
```

## 4.4 JSON Module Migration

### Strategy: Dual Support Phase

**Phase 1**: Compile-time selection (current)
```erlang
-ifdef(OTP_28_PLUS).
-define(JSON_MOD, json).
-else.
-define(JSON_MOD, jsx).
-endif.

%% Usage:
?JSON_MOD:encode(Data).
```

**Phase 2**: Safe wrapper functions
```erlang
%% In erlmcp_json_codec.erl
-include("otp_compat.hrl").

-spec encode(map() | list()) -> binary().
encode(Data) ->
    ?JSON_ENCODE_SAFE(Data).

-spec decode(binary()) -> {ok, map()} | {error, term()}.
decode(Binary) ->
    try ?JSON_DECODE_SAFE(Binary) of
        Result -> {ok, Result}
    catch
        _:_ -> {error, invalid_json}
    end.
```

**Phase 3**: Direct usage (OTP 28 only)
```erlang
%% In erlmcp v3.1+ (when OTP 27 support dropped)
-spec encode(map()) -> binary().
encode(Data) ->
    json:encode(Data).
```

### Migration Checklist

- [ ] Update `erlmcp_json_rpc.erl` to use compatibility macros
- [ ] Update all `jsx:encode/decode` calls to `?JSON_ENCODE/DECODE`
- [ ] Remove `jsx` from `rebar.config` deps (OTP 28+ only)
- [ ] Add runtime deprecation warning for `jsx` usage on OTP 28+
- [ ] Benchmark native json vs jsx performance
- [ ] Update documentation to reflect JSON module requirement

## 4.5 Testing Strategy

### Unit Tests

**File**: `apps/erlmcp_core/test/erlmcp_otp_compat_tests.erl`

```erlang
-module(erlmcp_otp_compat_tests).
-include_lib("eunit/include/eunit.hrl").
-include("otp_compat.hrl").

%%====================================================================
%% Version Detection Tests
%%====================================================================

otp_version_macros_test() ->
    %% Verify compile-time macros are defined
    ?assert((?OTP_28_PLUS =:= true) orelse (?OTP_28_PLUS =:= undefined)).

%%====================================================================
%% Feature Detection Tests
%%====================================================================

native_json_detection_test() ->
    IsAvailable = ?HAVE_NATIVE_JSON,
    ?assert(is_boolean(IsAvailable)),

    %% Verify runtime check matches compile-time macro
    -ifdef(OTP_28_PLUS).
    ?assert(IsAvailable =:= true),
    ?assert(erlang:function_exported(json, encode, 1)),
    -else.
    %% May or may not be available on OTP 27
    ok,
    -endif.

process_iterator_detection_test() ->
    IsAvailable = ?HAVE_PROCESS_ITERATOR,
    ?assert(is_boolean(IsAvailable)),

    -ifdef(OTP_28_PLUS).
    ?assert(IsAvailable =:= true),
    ?assert(erlang:function_exported(erlang, processes_iterator, 0)),
    -else.
    ?assert(IsAvailable =:= false),
    -endif.

%%====================================================================
%% Safe API Tests
%%====================================================================

safe_process_count_test() ->
    Count = ?SAFE_PROCESS_COUNT(),
    ?assert(is_integer(Count)),
    ?assert(Count >= 0),

    %% Verify matches system_info
    SystemCount = erlang:system_info(process_count),
    ?assertEqual(SystemCount, Count).

safe_json_encode_test() ->
    Data = #{<<"test">> => <<"value">>},

    Encoded = ?JSON_ENCODE_SAFE(Data),
    ?assert(is_binary(Encoded)),
    ?assert(byte_size(Encoded) > 0),

    %% Verify roundtrip
    Decoded = ?JSON_DECODE_SAFE(Encoded),
    ?assertEqual(Data, Decoded).
```

### Integration Tests

**File**: `apps/erlmcp_core/test/erlmcp_otp_compat_SUITE.erl`

```erlang
-module(erlmcp_otp_compat_SUITE).
-include_lib("common_test/include/ct.hrl").

%% Test that process_monitor works on both OTP 27 and 28
test_process_monitor_compatibility(_Config) ->
    {ok, Pid} = erlmcp_process_monitor:start_link([]),

    %% Should work regardless of OTP version
    {ok, Metrics} = erlmcp_process_monitor:get_process_metrics(),
    ?assert(maps:is_key(process_count, Metrics)),
    ?assert(maps:is_key(process_limit, Metrics)),

    %% Safe enumeration should not crash
    {ok, Count} = erlmcp_process_monitor:enumerate_processes(),
    ?assert(is_integer(Count)),

    gen_server:stop(Pid),
    ok.

%% Test JSON encoding/decoding compatibility
test_json_compatibility(_Config) ->
    %% Test data
    TestMap = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>
    },

    %% Encode
    Encoded = ?JSON_ENCODE_SAFE(TestMap),
    ?assert(is_binary(Encoded)),

    %% Decode
    {ok, Decoded} = erlmcp_json_codec:decode(Encoded),
    ?assertEqual(TestMap, Decoded),

    ok.
```

### Property-Based Tests

```erlang
%% Use PropER to test compatibility layer properties
prop_json_roundtrip() ->
    ?FORALL(Data, json_object(),
        begin
            Encoded = ?JSON_ENCODE_SAFE(Data),
            Decoded = ?JSON_DECODE_SAFE(Encoded),
            equals(Data, Decoded)
        end).

prop_process_count_accuracy() ->
    ?FORALL(_Count, nat(),
        begin
            SafeCount = ?SAFE_PROCESS_COUNT(),
            SystemCount = erlang:system_info(process_count),
            equals(SafeCount, SystemCount)
        end).
```

## 4.6 Implementation Roadmap

### Phase 1: Compatibility Header (Week 1)

**Deliverables**:
1. Create `include/otp_compat.hrl` with all feature detection macros
2. Create `apps/erlmcp_core/src/erlmcp_otp_compat.erl` helper module
3. Add compile-time version detection to `rebar.config`
4. Documentation in `docs/v3/otp-compat-layer.md`

**Quality Gates**:
- [ ] Compilation on OTP 27 and OTP 28
- [ ] No warnings from compiler
- [ ] Dialyzer clean
- [ ] Unit tests pass

### Phase 2: Process Monitor Migration (Week 2)

**Deliverables**:
1. Update `erlmcp_process_monitor.erl` with conditional compilation
2. Update all `system_info(process_count)` usage to `?SAFE_PROCESS_COUNT()`
3. Add fallback paths for OTP 27
4. Add safety warnings for large process counts on OTP 27

**Quality Gates**:
- [ ] Compilation on OTP 27 and OTP 28
- [ ] All tests pass on both versions
- [ ] Memory usage verified for >100K processes on OTP 28
- [ ] Performance benchmarks complete

### Phase 3: JSON Module Migration (Week 3)

**Deliverables**:
1. Create `erlmcp_json_codec.erl` wrapper module
2. Update all JSON usage to `?JSON_ENCODE/DECODE` macros
3. Remove `jsx` from `rebar.config` (OTP 28+ only)
4. Update dependencies documentation

**Quality Gates**:
- [ ] JSON encoding tests pass
- [ ] Roundtrip tests pass
- [ ] Performance benchmarks show 2-3x improvement on OTP 28
- [ ] Fallback to `jsx` works on OTP 27

### Phase 4: Comprehensive Testing (Week 4)

**Deliverables**:
1. Full test suite on OTP 27
2. Full test suite on OTP 28
3. Performance comparison report
4. Migration guide for users

**Quality Gates**:
- [ ] 100% test coverage for compatibility layer
- [ ] All 288 files using `system_info(process_count)` audited
- [ ] All JSON usage migrated to compat layer
- [ ] Documentation complete

## 4.7 Known Limitations

### OTP 27 Support

**Limited Features**:
- No native priority messages (fallback to normal ordering)
- No process iterators (O(N) memory for enumeration)
- No native JSON (uses `jsx` dependency)
- Slower performance for critical paths

**Deprecation Timeline**:
- erlmcp v3.0: OTP 27 supported with deprecation warnings
- erlmcp v3.1: OTP 27 support removed, OTP 28.3.1+ required

### Memory Constraints

**OTP 27**:
- Process enumeration limited to ~10K processes
- Beyond that: warnings, skips full enumeration
- Recommendation: Upgrade to OTP 28 for >10K processes

**OTP 28**:
- Process iterators handle 100K+ processes
- O(1) memory allocation via iteration
- No practical limit

## 4.8 Migration Guide

### For Module Authors

**Step 1**: Include compatibility header
```erlang
-include("otp_compat.hrl").
```

**Step 2**: Replace unsafe calls
```erlang
%% Before
ProcessCount = erlang:system_info(process_count).

%% After
ProcessCount = ?SAFE_PROCESS_COUNT().
```

**Step 3**: Use version-specific code paths
```erlang
-ifdef(OTP_28_PLUS).
    %% Optimized code
-else.
    %% Fallback code
-endif.
```

### For Users

**Upgrading to OTP 28**:
1. Install Erlang/OTP 28.3.1+
2. Recompile erlmcp with `rebar3 compile`
3. Run tests with `rebar3 eunit`
4. Verify performance improvements

**Staying on OTP 27**:
1. Deprecation warnings will appear
2. Full feature set not available
3. Consider upgrading for production use

## 4.9 Success Criteria

### Functional Requirements

- [x] `include/otp_compat.hrl` created with all macros
- [ ] All 288 files updated to use safe macros
- [ ] Conditional compilation works for OTP 27 and 28
- [ ] No performance regression on OTP 27
- [ ] Significant performance improvement on OTP 28

### Non-Functional Requirements

- [ ] 100% backward compatibility with OTP 27
- [ ] Zero compilation warnings on both versions
- [ ] Dialyzer clean on both versions
- [ ] Test coverage >= 80% for compatibility layer
- [ ] Documentation complete and accurate

### Performance Targets

**OTP 28 Performance**:
- JSON encoding: 2-3x faster than `jsx`
- Process enumeration: O(1) memory
- Priority messages: <1ms latency
- Overall throughput: 2-5x improvement

**OTP 27 Performance**:
- No regression vs v2.1.0
- Graceful degradation where appropriate
- Clear upgrade path documented

## 4.10 References

### Internal Documentation

- `docs/OTP_28_ARCHITECTURE.md` - OTP 28 architecture decisions
- `docs/EEP76_PRIORITY_MESSAGES.md` - Priority messages implementation
- `CLAUDE.md` - erlmcp system specification
- `docs/otp-patterns.md` - OTP coding patterns

### External References

- [EEP 76: Priority Messages](https://www.erlang.org/eeps/eep-0076)
- [OTP 28 Release Notes](https://www.erlang.org/doc/system/principles/releases.html)
- [JSON Module Documentation](https://www.erlang.org/doc/man/json.html)
- [Process Iterator Documentation](https://www.erlang.org/doc/man/erlang.html#processes_iterator-0)

---

**Author**: Claude Code (erlang-otp-developer agent)
**Status**: Design Phase - Ready for Implementation
**Next Step**: Phase 1 - Create `include/otp_compat.hrl`
