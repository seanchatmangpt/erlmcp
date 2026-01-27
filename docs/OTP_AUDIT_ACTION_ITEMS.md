# OTP Audit Action Items - Executive Summary

**Report**: ERLANG_OTP_AUDIT_REPORT.md
**Date**: 2026-01-27
**Overall Score**: 87% (B+)
**Critical Issues**: 3
**Major Issues**: 5
**Minor Issues**: 4

---

## Critical Issues - FIX IMMEDIATELY

### ISSUE #1: Blocking File I/O in Gen_Server Handlers

**Status**: CRITICAL
**Severity**: Will cause production outages
**Locations**:
- `tcps_persistence.erl`: file:read_file/1 in handle_call/3
- `erlmcp_path_canonicalizer.erl`: file:read_link_info/1, file:read_link/1
- `rdf_utils.erl`: file:read_file/1 in initialization

**Problem**:
File I/O operations block entire gen_server, causing timeouts and cascading failures.

**Solution**:
```erlang
% WRONG - blocks gen_server
handle_call({read_file, Path}, _From, State) ->
    {ok, Content} = file:read_file(Path),
    {reply, Content, State}.

% CORRECT - async with timeout
handle_call({read_file, Path}, From, State) ->
    spawn(fun() ->
        case file:read_file(Path) of
            {ok, Content} ->
                gen_server:reply(From, {ok, Content});
            {error, Reason} ->
                gen_server:reply(From, {error, Reason})
        end
    end),
    {noreply, State}.

% OR - use file worker pool with timeout
handle_call({read_file, Path}, From, State) ->
    Worker = poolboy:checkout(file_workers),
    erlmcp_file_worker:read_async(Worker, Path, From),
    {noreply, State}.
```

**Test Coverage Needed**:
- Timeout during file read
- Large file read with timeout
- Concurrent file operations
- Recovery after timeout

**Effort**: 3-5 days
**Priority**: CRITICAL
**Owner**: Backend team

---

### ISSUE #2: Missing Timeout on Stdio Read Loop

**Status**: CRITICAL
**Severity**: Can deadlock entire transport
**Location**: `erlmcp_transport_stdio.erl:178-185` (read_loop)

**Problem**:
```erlang
read_loop(Parent, Owner) ->
    case io:get_line("") of  % BLOCKS FOREVER if no input
        eof ->
            Parent ! {eof, self()};
        {error, Reason} ->
            Parent ! {error, self(), Reason};
        Line ->
            Parent ! {line, Line},
            read_loop(Parent, Owner)
    end.
```

Reader process hangs indefinitely waiting for input with no timeout mechanism.

**Solution**:
```erlang
% Use io:get_chars with timeout or select/3
read_loop_with_timeout(Parent, Owner, Timeout) ->
    case select([stdin_fd()], Timeout) of
        {ok, [_]} ->
            case io:get_line("") of
                Line -> Parent ! {line, Line}
            end;
        {timeout, []} ->
            Parent ! {timeout, self()}
    end.

% Or use prim_file for non-blocking I/O
read_loop_nonblocking(Parent, Owner) ->
    case prim_file:read(standard_io, 4096) of
        {ok, Data} -> process_data(Data);
        {error, eagain} -> timer:sleep(10), read_loop_nonblocking(...)
    end.
```

**Test Coverage Needed**:
- Timeout detection and handling
- Hung reader recovery
- Clean shutdown after timeout
- Reconnection after reader death

**Effort**: 1-2 days
**Priority**: CRITICAL
**Owner**: Transport team

---

### ISSUE #3: Bare Catch Statements Suppress Error Context

**Status**: CRITICAL
**Severity**: Silent failures, lost debugging information
**Location**: `erlmcp_server.erl:422`

**Problem**:
```erlang
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{server_id = ServerId}) ->
    catch erlmcp_task_manager:unregister_server(ServerId),  % WRONG: suppresses errors
    logger:info("MCP server ~p (refactored) terminating", [ServerId]),
    ok.
```

Errors during cleanup are silently suppressed, making debugging difficult.

**Solution**:
```erlang
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{server_id = ServerId}) ->
    case catch erlmcp_task_manager:unregister_server(ServerId) of
        ok ->
            logger:info("Server ~p unregistered successfully", [ServerId]);
        {error, Reason} ->
            logger:error("Failed to unregister server ~p: ~p", [ServerId, Reason]);
        Error ->
            logger:warning("Unregister error: ~p", [Error])
    end,
    logger:info("MCP server ~p terminating", [ServerId]),
    ok.
```

**Better Approach**:
```erlang
terminate(Reason, #state{server_id = ServerId}) ->
    logger:info("Server ~p terminating (reason: ~p)", [ServerId, Reason]),

    % Use try-catch with proper error logging
    try
        erlmcp_task_manager:unregister_server(ServerId)
    catch
        Class:Error:Stack ->
            logger:error("Cleanup error in server ~p: ~p:~p~nStack: ~p",
                        [ServerId, Class, Error, Stack])
    end,
    ok.
```

**Locations to Fix**:
- erlmcp_server.erl:422 (terminate)
- Any other catch statements without logging

**Test Coverage Needed**:
- Error context preservation
- Cleanup error logging
- Crash dump analysis for debugging

**Effort**: 1 day
**Priority**: CRITICAL
**Owner**: Quality team

---

## Major Issues - FIX SOON

### ISSUE #4: No Backpressure Handling for Subscriptions

**Status**: MAJOR
**Severity**: Message queue overflow, subscriber loss
**Location**: `erlmcp_resource_subscriptions.erl`, `erlmcp_server.erl:1344`

**Problem**:
```erlang
% Current implementation - no flow control
lists:foreach(fun(Subscriber) ->
    _ = Subscriber ! {resource_updated, Uri, Metadata},
end, sets:to_list(Subs))
```

Subscriberqueues can overflow. Dead subscribers are never cleaned up.

**Solution**:
```erlang
%% Healthy subscription notification with monitoring
notify_subscribers(Uri, Metadata, Subs) ->
    lists:foreach(fun(Subscriber) ->
        case is_process_alive(Subscriber) of
            true ->
                case safe_send(Subscriber, {resource_updated, Uri, Metadata}, 100) of
                    ok -> ok;
                    {error, timeout} ->
                        % Subscriber queue overflowed - remove it
                        erlmcp_resource_subscriptions:unsubscribe(Uri, Subscriber);
                    {error, dead_process} ->
                        % Clean up dead subscriber
                        erlmcp_resource_subscriptions:unsubscribe(Uri, Subscriber)
                end;
            false ->
                % Dead subscriber - clean up
                erlmcp_resource_subscriptions:unsubscribe(Uri, Subscriber)
        end
    end, sets:to_list(Subs)).

%% Safe send with timeout
safe_send(Pid, Message, TimeoutMs) ->
    try
        erlang:send(Pid, Message, [noconnect]),
        ok
    catch
        error:_ -> {error, dead_process}
    end.
```

**Effort**: 3-5 days
**Priority**: HIGH
**Owner**: Server team

---

### ISSUE #5: Configuration Schema Not Validated at Startup

**Status**: MAJOR
**Severity**: Misconfiguration not detected until runtime
**Location**: `erlmcp_config_validation.erl` (exists but not fully integrated)

**Problem**:
Invalid configuration loads silently with defaults, causing mysterious failures.

**Solution**:
```erlang
%% erlmcp_app:start/2 should validate config
start(_StartType, _StartArgs) ->
    case validate_startup_config() of
        ok ->
            erlmcp_logging:init_session_levels(),
            erlmcp_sup:start_link();
        {error, Reason} ->
            {error, {config_validation_failed, Reason}}
    end.

validate_startup_config() ->
    Config = application:get_all_env(erlmcp),
    case erlmcp_config_validation:validate(Config) of
        ok -> ok;
        {error, Issues} ->
            log_config_issues(Issues),
            {error, Issues}
    end.

log_config_issues(Issues) ->
    lists:foreach(fun({Key, Message}) ->
        logger:error("Configuration error ~p: ~p", [Key, Message])
    end, Issues).
```

**Effort**: 2-3 days
**Priority**: HIGH
**Owner**: Configuration team

---

### ISSUE #6: Incomplete Timeout Enforcement

**Status**: MAJOR
**Severity**: Operations can hang indefinitely
**Affected Areas**:
- Transport initialization timeouts
- HTTP request timeouts
- TCP connection timeouts
- Handshake timeouts

**Solution**:
Audit all transport implementations for timeout coverage:
```erlang
%% TCP example - should timeout if connection not established
connect_with_timeout(Host, Port, TimeoutMs) ->
    case gen_tcp:connect(Host, Port, [{active, once}], TimeoutMs) of
        {ok, Socket} -> {ok, Socket};
        {error, timeout} -> {error, connection_timeout};
        {error, Reason} -> {error, Reason}
    end.
```

**Effort**: 3-5 days
**Priority**: HIGH
**Owner**: Transport team

---

### ISSUE #7: erlmcp_server.erl Size Exceeds Maintainability Threshold

**Status**: MAJOR
**Severity**: Complex testing, difficult maintenance
**Size**: 1,520 lines (recommended max: 400-500 lines)

**Problem**:
Large module mixes concerns:
- Initialization
- Request handling
- Resource management
- Tool management
- Prompt management

**Solution** - Split into modules:
```
erlmcp_server_core.erl        (initialization, lifecycle)
erlmcp_server_resources.erl   (resource operations)
erlmcp_server_tools.erl       (tool operations)
erlmcp_server_prompts.erl     (prompt operations)
erlmcp_server_handlers.erl    (request dispatch)
```

**Effort**: 1-2 weeks
**Priority**: MEDIUM
**Owner**: Architecture team

---

### ISSUE #8: Reader Process Lacks Lifecycle Monitoring

**Status**: MAJOR
**Severity**: Orphaned processes, resource leaks
**Location**: `erlmcp_transport_stdio.erl:90`

**Problem**:
```erlang
ReaderPid = spawn_link(fun() -> read_loop(self(), Owner) end),
```

If reader crashes abnormally, no recovery mechanism exists.

**Solution**:
```erlang
% Use spawn_monitor instead of spawn_link
init([Owner]) ->
    process_flag(trap_exit, true),
    TestMode = is_test_environment(),

    State = #state{owner = Owner, test_mode = TestMode},

    case TestMode of
        true -> {ok, State};
        false ->
            {Pid, _Ref} = spawn_monitor(fun() -> read_loop(self(), Owner) end),
            {ok, State#state{reader = Pid}}
    end.

% Handle DOWN messages
handle_info({'DOWN', _Ref, process, Pid, Reason}, #state{reader = Pid} = State) ->
    case Reason of
        normal -> {noreply, State#state{reader = undefined}};
        _ ->
            logger:error("Reader process died: ~p, attempting restart", [Reason]),
            restart_reader(State)
    end.
```

**Effort**: 1-2 days
**Priority**: MEDIUM
**Owner**: Transport team

---

## Minor Issues - FIX WHEN POSSIBLE

### ISSUE #9: Incomplete Type Specifications

**Task Reference**: #75
**Status**: Task #75 completed (504 missing specs added)
**Next Step**: Verify all internal functions have type specs

**Solution**:
```erlang
% Add type specs to all functions, including internal ones
-spec handle_resource_update(binary(), map(), state()) -> state().
handle_resource_update(Uri, Metadata, State) ->
    % Implementation
end.
```

**Effort**: 1-2 days
**Priority**: LOW

---

### ISSUE #10: SSE Event Store Lacks Size Limits

**Status**: MINOR
**Severity**: Memory unbounded growth
**Location**: `erlmcp_sse_event_store.erl`

**Solution**:
```erlang
% Enforce maximum queue size and age limits
-define(MAX_EVENT_QUEUE_SIZE, 10000).
-define(MAX_EVENT_AGE_MS, 3600000).  % 1 hour

add_event(Event, #state{events = Events} = State) ->
    NewEvents = trim_old_events(
        [Event | Events],
        ?MAX_EVENT_QUEUE_SIZE,
        ?MAX_EVENT_AGE_MS
    ),
    {noreply, State#state{events = NewEvents}}.
```

**Effort**: 1 day
**Priority**: LOW

---

### ISSUE #11: No Code Coverage Minimum Enforced

**Status**: MINOR
**Severity**: Coverage regression not prevented
**Location**: rebar.config

**Solution**:
```erlang
% Add coverage minimum check to CI
{alias, [
    {check, [
        xref,
        dialyzer,
        {proper, "-c"},
        cover,
        {coverage_check, "80"}  % Require minimum 80% coverage
    ]}
]}.
```

**Effort**: 1 day
**Priority**: LOW

---

### ISSUE #12: Repetitive Capability Checking

**Status**: MINOR
**Severity**: Code duplication
**Location**: `erlmcp_client.erl` - multiple capability checks

**Solution**:
```erlang
% Extract capability checking to helper
-spec require_capability(state(), atom()) -> ok | {error, term()}.
require_capability(State, Capability) ->
    case validate_capability(State, Capability) of
        ok -> ok;
        {error, _} = Error -> Error
    end.

% Use in handle_call
handle_call(list_resources, From, #state{phase = initialized} = State) ->
    case require_capability(State, resources) of
        ok ->
            send_request(State, <<"resources/list">>, #{}, {list_resources, From});
        Error ->
            {reply, Error, State}
    end.
```

**Effort**: 1 day
**Priority**: LOW

---

## Implementation Timeline

### Week 1: Critical Issues
```
Day 1-2: Fix blocking file I/O
Day 3: Add stdio read timeout
Day 4: Error logging improvements
Day 5: Testing & validation
```

### Week 2: Major Issues
```
Day 1: Backpressure handling
Day 2: Configuration validation
Day 3-4: Timeout enforcement audit
Day 5: Reader process monitoring
```

### Week 3-4: Code Quality
```
Week 3: Module refactoring (erlmcp_server split)
Week 4: Type specs completion, optimizations
```

---

## Success Criteria

### Critical Issues
- [ ] All file operations wrapped in timeout-aware processes
- [ ] Stdio read_loop has timeout mechanism
- [ ] All cleanup code logs errors instead of suppressing them
- [ ] Tests pass without timeout warnings

### Major Issues
- [ ] Dead subscribers automatically removed
- [ ] Invalid configuration rejected at startup
- [ ] All async operations have timeouts
- [ ] Reader process recovery tested

### Code Quality
- [ ] 100% type spec coverage
- [ ] erlmcp_server.erl split into 3+ modules
- [ ] Coverage minimum enforced (80%+)
- [ ] No bare catch statements

---

## Rollout Plan

1. **Immediate**: Begin critical issue fixes
2. **This Sprint**: Complete 2-3 critical issues
3. **Next Sprint**: Complete remaining critical + major issues
4. **Following Sprints**: Code quality improvements

---

## Questions for Team

1. **File I/O**: Should we use thread pool, async task, or side process?
2. **Stdio**: Do we need to support hanging in test environments?
3. **Refactoring**: Can we do erlmcp_server split while maintaining backwards compatibility?
4. **Timeline**: Can we schedule 2-3 week effort for critical+major fixes?

---

**Report**: ERLANG_OTP_AUDIT_REPORT.md
**Questions?** Review full audit report for detailed analysis and recommendations.
