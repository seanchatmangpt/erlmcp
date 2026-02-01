# FSM Implementation Guide
## Armstrong-Style State Machines for erlmcp v2.2.0

**Quick Reference for Developers**

---

## gen_statem Conversion Checklist

Use this checklist when converting a gen_server with manual phase tracking to gen_statem.

### Pre-Conversion Assessment

- [ ] Identify all "phases" or "states" in current code
- [ ] Map out all state transitions (draw diagram)
- [ ] Identify race conditions in current implementation
- [ ] List all events that trigger state changes
- [ ] Document guards/preconditions for transitions

### Step 1: Define States and Data

```erlang
%% OLD (gen_server)
-record(state, {
    phase :: atom(),  % Manual tracking
    transport :: module(),
    pending :: map()
}).

%% NEW (gen_statem)
-type my_state() :: disconnected | connecting | ready | closed.

-record(data, {
    %% NO phase field - state IS the phase
    transport :: module(),
    pending :: map()
}).
```

**Checklist:**
- [ ] Remove `phase` field from record
- [ ] Define `-type` for all legal states
- [ ] Rename record from `state` to `data` (clarity)
- [ ] Add state versioning: `version = v1 :: state_version()`
- [ ] Add audit trail: `state_history = [] :: [{state(), integer()}]`

### Step 2: Implement Behavior Callbacks

```erlang
-module(my_fsm).
-behaviour(gen_statem).

%% Callbacks
-export([callback_mode/0, init/1, terminate/3, code_change/4]).

%% State functions
-export([disconnected/3, connecting/3, ready/3, closed/3]).

callback_mode() ->
    [state_functions, state_enter].  % Use state functions + enter events

init(Args) ->
    Data = #data{transport = Args},
    {ok, disconnected, Data}.  % Initial state = disconnected
```

**Checklist:**
- [ ] Add `-behaviour(gen_statem)`
- [ ] Implement `callback_mode/0` → `state_functions`
- [ ] Export all state function names
- [ ] Convert `init/1` to return `{ok, InitialState, Data}`
- [ ] Implement `terminate/3` (cleanup)
- [ ] Implement `code_change/4` (hot upgrade support)

### Step 3: Convert handle_call/cast/info to State Functions

```erlang
%% OLD (gen_server)
handle_call(connect, _From, State = #state{phase = disconnected}) ->
    NewState = State#state{phase = connecting},
    {reply, ok, NewState};
handle_call(connect, _From, State) ->
    {reply, {error, already_connected}, State}.

%% NEW (gen_statem)
disconnected({call, From}, connect, Data) ->
    %% Transition to connecting state
    {next_state, connecting, Data, [{reply, From, ok}]};

connecting({call, From}, connect, Data) ->
    %% Already connecting - reject
    {keep_state_and_data, [{reply, From, {error, already_connecting}}]}.
```

**Checklist:**
- [ ] Create one function per state: `state_name(EventType, Event, Data)`
- [ ] Handle events: `{call, From}`, `{cast}`, `{info}`
- [ ] Use transitions:
  - `{next_state, NewState, NewData, Actions}` - change state
  - `{keep_state, NewData, Actions}` - same state, update data
  - `{keep_state_and_data, Actions}` - no changes
- [ ] Return actions: `[{reply, From, Reply}]`, `{timeout, Ms, Event}`, etc.

### Step 4: Add State Enter Events (Optional but Recommended)

```erlang
%% Triggered when entering a state
ready(enter, _OldState, Data) ->
    %% Log transition
    ?LOG_INFO("Entered ready state"),
    %% Start heartbeat timer
    {keep_state_and_data, [{state_timeout, 30000, heartbeat}]};

ready(state_timeout, heartbeat, Data) ->
    %% Send heartbeat
    send_heartbeat(Data),
    {keep_state_and_data, [{state_timeout, 30000, heartbeat}]};

ready({call, From}, request, Data) ->
    %% Normal request handling
    {keep_state_and_data, [{reply, From, ok}]}.
```

**Checklist:**
- [ ] Add `state_enter` to `callback_mode/0`
- [ ] Implement `state_name(enter, OldState, Data)` for each state
- [ ] Use enter events for:
  - Logging state transitions
  - Starting timers
  - Cleanup from previous state
  - Recording audit trail

### Step 5: Add Guards for Transitions

```erlang
%% Guard functions (in Data)
can_reconnect(#data{retry_count = Count, max_retries = Max}) ->
    Count < Max.

has_pending_requests(#data{pending = Pending}) ->
    maps:size(Pending) > 0.

%% Use in state function
ready({call, From}, stop, Data) ->
    case has_pending_requests(Data) of
        true ->
            %% Drain pending requests first
            {next_state, draining, Data, [{reply, From, ok}]};
        false ->
            %% Immediate shutdown
            {next_state, closed, Data, [{reply, From, ok}]}
    end.
```

**Checklist:**
- [ ] Extract guard logic into separate functions
- [ ] Use guards in state transitions
- [ ] Document guards in state spec module

### Step 6: Handle Race Conditions

```erlang
%% Race: result vs cancel
running({call, From}, cancel, Data) ->
    %% Cancel requested
    {next_state, cancelling, Data, [{reply, From, ok}]};

running(info, {result, Result}, Data) ->
    %% Result arrived
    {next_state, result_ready, Data#data{result = Result}};

%% CRITICAL: Result arrives during cancel
cancelling(info, {result, Result}, Data) ->
    ?LOG_INFO("Result won race against cancel"),
    %% Result wins - preserve it
    {next_state, result_ready, Data#data{result = Result}}.
```

**Checklist:**
- [ ] Identify all possible race conditions
- [ ] Define race resolution policy (e.g., "result wins")
- [ ] Implement race handlers in state functions
- [ ] Add logging for race occurrences
- [ ] Write property tests for races

### Step 7: Add Audit Trail and Debugging

```erlang
-record(data, {
    %% ... existing fields
    state_entered_at :: integer(),
    state_history = [] :: [{state(), integer()}]
}).

%% In each state function
my_state(enter, _OldState, Data) ->
    Timestamp = erlang:monotonic_time(millisecond),
    NewHistory = [{my_state, Timestamp} | Data#data.state_history],
    {keep_state, Data#data{
        state_entered_at = Timestamp,
        state_history = NewHistory
    }}.

%% Debug API
format_status(Status) ->
    case Status of
        #{state := State, data := Data} ->
            #{
                current_state => State,
                state_duration_ms => erlang:monotonic_time(millisecond) -
                                      Data#data.state_entered_at,
                state_history => format_history(Data#data.state_history)
            }
    end.
```

**Checklist:**
- [ ] Add `state_entered_at` field
- [ ] Add `state_history` field
- [ ] Update history on each transition
- [ ] Implement `format_status/1` for debugging
- [ ] Add OTEL events for state transitions

### Step 8: Implement Hot-Upgrade Support

```erlang
code_change(OldVsn, State, Data, Extra) ->
    ?LOG_INFO("Hot upgrade: ~p -> current, state=~p", [OldVsn, State]),

    %% Detect version
    CurrentVersion = case Data of
        #data{version = V} -> V;
        _ -> v1  %% Old structure
    end,

    %% Migrate
    NewData = my_module_migrate:migrate(CurrentVersion, Data),

    %% Preserve state, upgrade data
    {ok, State, NewData}.
```

**Checklist:**
- [ ] Add `version` field to data record
- [ ] Create migration module: `my_module_migrate.erl`
- [ ] Implement `migrate/2` function (v1→v2→v3)
- [ ] Implement `downgrade/2` for rollback
- [ ] Implement `code_change/4` callback
- [ ] Test upgrade with `sys:suspend/change_code/resume`

### Step 9: Write PropEr Statem Tests

```erlang
-module(my_fsm_statem_tests).
-include_lib("proper/include/proper.hrl").

-record(model, {
    state :: disconnected | connecting | ready | closed
}).

initial_state() ->
    #model{state = disconnected}.

command(#model{state = disconnected}) ->
    {call, my_fsm, connect, []};
command(#model{state = ready}) ->
    oneof([
        {call, my_fsm, send_request, [binary()]},
        {call, my_fsm, stop, []}
    ]).

precondition(#model{state = State}, {call, _M, F, _A}) ->
    erlmcp_state_spec:is_valid_transition(my_fsm, State, F).

next_state(Model, _Result, {call, _M, connect, _A}) ->
    Model#model{state = connecting}.

prop_state_machine() ->
    ?FORALL(Cmds, commands(?MODULE),
        aggregate(command_names(Cmds), run_commands(?MODULE, Cmds) =:= ok)).
```

**Checklist:**
- [ ] Create PropEr statem test module
- [ ] Define model state record
- [ ] Implement `command/1` - generate commands
- [ ] Implement `precondition/2` - validate command
- [ ] Implement `next_state/3` - update model
- [ ] Run 1000+ iterations: `proper:quickcheck(prop_state_machine(), 1000)`

### Step 10: Update Documentation

```erlang
%% @doc Client FSM - manages connection lifecycle
%%
%% States:
%%   - disconnected: No active connection
%%   - connecting: Transport initializing
%%   - ready: Fully initialized, accepting requests
%%   - closed: Terminal state
%%
%% State Transitions:
%%   disconnected --connect--> connecting
%%   connecting --transport_ready--> ready
%%   ready --stop--> closed
%%
%% Race Conditions:
%%   - transport_lost + stop: stop takes precedence
%%
%% Example:
%%   {ok, Pid} = my_fsm:start_link(),
%%   ok = my_fsm:connect(Pid),
%%   {ok, Result} = my_fsm:send_request(Pid, Data).
```

**Checklist:**
- [ ] Add module-level documentation
- [ ] Document all states
- [ ] Document all transitions
- [ ] Document race conditions
- [ ] Add usage examples
- [ ] Generate Mermaid state diagram
- [ ] Update architecture.md

---

## Common Patterns

### Pattern 1: Timeout Handling

```erlang
%% Set timeout when entering state
connecting(enter, _OldState, Data) ->
    {keep_state_and_data, [{state_timeout, 5000, connect_timeout}]};

%% Handle timeout
connecting(state_timeout, connect_timeout, Data) ->
    ?LOG_WARNING("Connection timeout"),
    {next_state, disconnected, Data}.
```

### Pattern 2: Graceful Draining

```erlang
ready({call, From}, stop, Data) ->
    case has_pending_requests(Data) of
        true ->
            %% Drain mode: stop accepting, wait for pending
            {next_state, draining, Data#data{accepting = false},
             [{reply, From, ok}]};
        false ->
            %% No pending, immediate close
            {next_state, closed, Data, [{reply, From, ok}]}
    end.

draining(info, {request_complete, ReqId}, Data) ->
    NewPending = maps:remove(ReqId, Data#data.pending),
    NewData = Data#data{pending = NewPending},
    case maps:size(NewPending) of
        0 ->
            %% All drained, close now
            {next_state, closed, NewData};
        _ ->
            %% Still draining
            {keep_state, NewData}
    end.
```

### Pattern 3: Reconnection with Backoff

```erlang
reconnecting(enter, _OldState, Data) ->
    %% Exponential backoff
    BackoffMs = min(Data#data.backoff_ms * 2, Data#data.max_backoff_ms),
    NewData = Data#data{
        backoff_ms = BackoffMs,
        retry_count = Data#data.retry_count + 1
    },
    {keep_state, NewData, [{state_timeout, BackoffMs, retry}]};

reconnecting(state_timeout, retry, Data) ->
    case can_reconnect(Data) of
        true ->
            {next_state, connecting, Data};
        false ->
            ?LOG_ERROR("Max retries exceeded"),
            {next_state, closed, Data}
    end.
```

### Pattern 4: Suspend/Resume

```erlang
ready({call, From}, suspend, Data) ->
    %% Enter suspended state, buffer new requests
    {next_state, suspended, Data#data{suspended_requests = []},
     [{reply, From, ok}]};

suspended({call, From}, request, Data) ->
    %% Buffer request while suspended
    Buffered = [{request, From} | Data#data.suspended_requests],
    {keep_state, Data#data{suspended_requests = Buffered}};

suspended({call, From}, resume, Data) ->
    %% Resume and flush buffered requests
    spawn(fun() -> flush_buffered_requests(Data#data.suspended_requests) end),
    {next_state, ready, Data#data{suspended_requests = []},
     [{reply, From, ok}]}.
```

### Pattern 5: Priority Events

```erlang
%% Handle priority event in ANY state
handle_event({call, From}, stop, _State, Data) ->
    %% Stop has highest priority - accepted in any state
    {next_state, closed, Data, [{reply, From, ok}]}.

%% Use handle_event mode for cross-state events
callback_mode() ->
    [state_functions, state_enter, handle_event_function].
```

---

## Testing Checklist

### Unit Tests (EUnit)

- [ ] Test each state transition individually
- [ ] Test guards (valid and invalid)
- [ ] Test enter/exit callbacks
- [ ] Test timeout handling
- [ ] Test all actions (reply, timeout, etc.)

### Integration Tests (Common Test)

- [ ] Test complete lifecycle (init → closed)
- [ ] Test recovery from crashes
- [ ] Test reconnection logic
- [ ] Test concurrent operations
- [ ] Test hot-upgrade

### Property Tests (PropEr)

- [ ] Model-based testing (PropEr statem)
- [ ] Race condition testing (concurrent commands)
- [ ] Invariant testing (no illegal states)
- [ ] 1000+ iterations per property

### Chaos Tests

- [ ] Kill transport during each state
- [ ] Inject delays (race conditions)
- [ ] Exhaust resources (OOM, timeouts)
- [ ] Network partitions

---

## Performance Checklist

### Optimization

- [ ] Use `keep_state_and_data` when possible (avoids copying)
- [ ] Limit state history size (keep last 100 transitions)
- [ ] Use ETS for large data structures (reference in #data{})
- [ ] Avoid blocking in state functions
- [ ] Use `hibernate` option for idle processes

### Monitoring

- [ ] Add OTEL spans for state transitions
- [ ] Track state durations (metrics)
- [ ] Alert on long-running states
- [ ] Monitor state transition rate

---

## Common Mistakes to Avoid

### ❌ Mistake 1: Forgetting to Handle All Events in All States

```erlang
%% BAD: Only handles connect in disconnected state
disconnected({call, From}, connect, Data) ->
    {next_state, connecting, Data, [{reply, From, ok}]}.
%% Missing: What if connect is called in other states?

%% GOOD: Handle in all states
disconnected({call, From}, connect, Data) ->
    {next_state, connecting, Data, [{reply, From, ok}]};

connecting({call, From}, connect, Data) ->
    {keep_state_and_data, [{reply, From, {error, already_connecting}}]};

ready({call, From}, connect, Data) ->
    {keep_state_and_data, [{reply, From, {error, already_connected}}]}.
```

### ❌ Mistake 2: Not Using Guards

```erlang
%% BAD: No guard, transition always happens
ready({call, From}, stop, Data) ->
    {next_state, closed, Data, [{reply, From, ok}]}.
%% Problem: Drops pending requests!

%% GOOD: Guard checks pending
ready({call, From}, stop, Data) ->
    case has_pending_requests(Data) of
        true -> {next_state, draining, Data, [{reply, From, ok}]};
        false -> {next_state, closed, Data, [{reply, From, ok}]}
    end.
```

### ❌ Mistake 3: Blocking in State Functions

```erlang
%% BAD: Blocking call
ready({call, From}, request, Data) ->
    Result = httpc:request(get, {"http://slow-api.com", []}, [], []),
    {keep_state_and_data, [{reply, From, Result}]}.
%% Problem: Blocks entire FSM!

%% GOOD: Async with worker
ready({call, From}, request, Data) ->
    Worker = spawn(fun() ->
        Result = httpc:request(get, {"http://slow-api.com", []}, [], []),
        gen_statem:cast(self(), {result, From, Result})
    end),
    {keep_state, Data#data{pending = maps:put(From, Worker, Data#data.pending)}};

ready(cast, {result, From, Result}, Data) ->
    {keep_state, maps:remove(From, Data#data.pending),
     [{reply, From, Result}]}.
```

### ❌ Mistake 4: Not Handling Race Conditions

```erlang
%% BAD: No handling for cancel arriving after result
running(info, {result, Result}, Data) ->
    {next_state, result_ready, Data#data{result = Result}};

running({call, From}, cancel, Data) ->
    {next_state, cancelling, Data, [{reply, From, ok}]}.
%% Problem: If result arrives during cancelling, it's lost!

%% GOOD: Handle race
cancelling(info, {result, Result}, Data) ->
    ?LOG_INFO("Result won race against cancel"),
    {next_state, result_ready, Data#data{result = Result}}.
```

### ❌ Mistake 5: Forgetting code_change/4

```erlang
%% BAD: No code_change implementation
%% Result: Hot upgrade crashes process!

%% GOOD: Implement code_change
code_change(_OldVsn, State, Data, _Extra) ->
    NewData = migrate_data(Data),
    {ok, State, NewData}.
```

---

## Quick Reference Card

### State Function Return Values

| Return | Meaning |
|--------|---------|
| `{next_state, NewState, NewData}` | Change to NewState |
| `{next_state, NewState, NewData, Actions}` | Change + actions |
| `{keep_state, NewData}` | Same state, new data |
| `{keep_state, NewData, Actions}` | Same state, new data, actions |
| `{keep_state_and_data}` | No changes |
| `{keep_state_and_data, Actions}` | No changes, actions |
| `{stop, Reason, NewData}` | Terminate FSM |
| `{stop_and_reply, Reason, Replies, NewData}` | Terminate + replies |

### Common Actions

| Action | Effect |
|--------|--------|
| `{reply, From, Reply}` | Reply to caller |
| `{timeout, Ms, Event}` | Event after Ms (any state) |
| `{state_timeout, Ms, Event}` | Event after Ms (reset on state change) |
| `hibernate` | Hibernate process (saves memory) |
| `{next_event, Type, Event}` | Insert event at front of queue |

### Event Types

| Type | Source |
|------|--------|
| `{call, From}` | gen_statem:call |
| `cast` | gen_statem:cast |
| `info` | Erlang message |
| `timeout` | Timeout action |
| `state_timeout` | State-specific timeout |
| `enter` | State enter event |

---

## Final Checklist Before Commit

- [ ] All states defined in type spec
- [ ] All transitions documented
- [ ] Guards implemented and tested
- [ ] Race conditions handled
- [ ] code_change/4 implemented
- [ ] PropEr statem tests pass (1000+ iterations)
- [ ] EUnit tests pass
- [ ] Common Test integration tests pass
- [ ] Documentation updated
- [ ] Mermaid state diagram created
- [ ] Performance benchmarks run (no regression)
- [ ] Hot-upgrade tested
- [ ] Code review approved

---

**Resources:**

- [gen_statem Documentation](https://www.erlang.org/doc/man/gen_statem.html)
- [PropEr User Guide](https://proper-testing.github.io/)
- [erlmcp State Spec Module](../src/erlmcp_state_spec.erl)
- [erlmcp Circuit Breaker](../apps/erlmcp_core/src/erlmcp_circuit_breaker.erl) - Reference implementation

**Questions?**
- Ask in #erlmcp-dev Slack channel
- Review existing gen_statem: `erlmcp_circuit_breaker.erl`
- Contact: erlang-architect agent
