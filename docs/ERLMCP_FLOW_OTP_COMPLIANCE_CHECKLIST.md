# erlmcp-flow OTP Compliance Checklist

**Version:** 1.0.0
**Date:** 2026-02-01
**Status:** MANDATORY
**Enforcement:** CI/CD + Code Review

---

## Executive Summary

This document provides a comprehensive OTP compliance checklist for erlmcp-flow development. It covers:

1. **gen_server/gen_statem** usage patterns
2. **Supervision** tree design and implementation
3. **Message handling** standards
4. **Error handling** with let-it-crash principle
5. **Testing** with real processes (Chicago School TDD)
6. **Documentation** requirements

**Armstrong Principle:** Build systems where incorrect behavior cannot exist.

---

## 1. gen_server/gen_statem Compliance

### 1.1 Mandatory Callback Implementation

**Rule:** All stateful components MUST be gen_server or gen_statem.

#### gen_server - All 6 Callbacks Required

```erlang
-module(erlmcp_example).
-behaviour(gen_server).

%% ✅ MANDATORY: Export all 6 callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% Optional but recommended for debugging
-export([format_status/2]).
```

#### gen_statem - All 7 Callbacks Required

```erlang
-module(erlmcp_example_statem).
-behaviour(gen_statem).

%% ✅ MANDATORY: Export all 7 callbacks
-export([init/1, callback_mode/0, handle_event/4,
         terminate/3, code_change/4, format_status/2]).
```

**Checklist:**

- [ ] All 6 gen_server callbacks exported (or 7 for gen_statem)
- [ ] No missing callback implementations
- [ ] format_status/2 implemented for sensitive data
- [ ] Proper return types for all callbacks

---

### 1.2 Non-Blocking init/1

**Rule:** init/1 MUST NEVER block. Supervisor tree startup must complete quickly.

#### ❌ VIOLATION: Blocking init/1

```erlang
%% WRONG: Blocks supervisor during database connection
init(Opts) ->
    %% This blocks the entire supervisor tree!
    {ok, Conn} = connect_to_database(Opts),
    {ok, DbHandle} = load_configuration(Conn),
    InitialState = process_data(DbHandle),
    {ok, #state{conn = Conn, data = InitialState}}.
```

**Problems:**
- Supervisor blocked waiting for DB connection
- Timeout crashes if DB slow
- Cascading failures in supervision tree
- Violates OTP principle: init/1 < 5 seconds

#### ✅ CORRECT: Async Initialization

**Pattern 1: handle_continue (OTP 21+, RECOMMENDED)**

```erlang
%% CORRECT: Fast init, async initialization with handle_continue
init(Opts) ->
    %% Fast return, supervisor proceeds immediately
    {ok, #state{opts = Opts}, {continue, initialize}}.

%% Async initialization in handle_continue
handle_continue(initialize, State = #state{opts = Opts}) ->
    %% Safe to block here - supervisor already started
    case connect_to_database(Opts) of
        {ok, Conn} ->
            {ok, DbHandle} = load_configuration(Conn),
            InitialState = process_data(DbHandle),
            {noreply, State#state{conn = Conn, data = InitialState}};
        {error, Reason} ->
            %% Let it crash - supervisor will restart
            {stop, {connection_failed, Reason}, State}
    end.
```

**Pattern 2: self() ! initialize (OTP 20 and earlier)**

```erlang
%% CORRECT: Fast init, async initialization via message
init(Opts) ->
    self() ! initialize,
    {ok, #state{opts = Opts, initialized = false}}.

handle_info(initialize, State = #state{opts = Opts}) ->
    case connect_to_database(Opts) of
        {ok, Conn} ->
            {ok, DbHandle} = load_configuration(Conn),
            InitialState = process_data(DbHandle),
            {noreply, State#state{conn = Conn, data = InitialState, initialized = true}};
        {error, Reason} ->
            {stop, {connection_failed, Reason}, State}
    end;
handle_info(_Msg, State = #state{initialized = false}) ->
    %% Not ready yet, queue or reject
    {noreply, State}.
```

**Real Example from erlmcp_server.erl:**

```erlang
init([ServerId, Capabilities]) ->
    %% Fast init - just store initial state
    State = #state{
        server_id = ServerId,
        capabilities = Capabilities,
        phase = ?MCP_PHASE_INITIALIZATION
    },
    %% Use handle_continue for async work
    {ok, State, {continue, initialize}}.

handle_continue(initialize, State) ->
    %% Async initialization work here
    {noreply, State#state{initialized = true}}.
```

**Checklist:**

- [ ] init/1 returns in < 100ms
- [ ] No external service calls in init/1
- [ ] No file I/O in init/1
- [ ] No network operations in init/1
- [ ] Use {continue, ...} or self() ! msg for async init
- [ ] Handle early messages before initialization complete

---

### 1.3 Message Passing Patterns

**Rule:** Use cast for fire-and-forget, call for request-response.

#### Cast (Fire-and-Forget)

```erlang
%% ✅ CORRECT: Cast for notifications (no response needed)
-spec notify_resource_updated(server(), binary(), map()) -> ok.
notify_resource_updated(Server, Uri, Metadata) ->
    gen_server:cast(Server, {notify_resource_updated, Uri, Metadata}).

handle_cast({notify_resource_updated, Uri, Metadata}, State) ->
    %% Process notification, no reply
    NewState = update_resource(Uri, Metadata, State),
    {noreply, NewState}.
```

**When to use cast:**
- Notifications
- Event broadcasts
- No return value needed
- Asynchronous updates

#### Call (Request-Response)

```erlang
%% ✅ CORRECT: Call for operations requiring response
-spec add_tool(server(), binary(), tool_handler()) -> ok | {error, term()}.
add_tool(Server, Name, Handler) ->
    gen_server:call(Server, {add_tool, Name, Handler}).

handle_call({add_tool, Name, Handler}, _From, State) ->
    case validate_tool(Name, Handler) of
        ok ->
            NewState = register_tool(Name, Handler, State),
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.
```

**When to use call:**
- Need return value
- Need error handling
- Synchronous operations
- Client needs confirmation

#### Call with Timeout

```erlang
%% ✅ CORRECT: Call with explicit timeout
-spec add_tool(server(), binary(), tool_handler(), timeout()) -> ok | {error, term()}.
add_tool(Server, Name, Handler, Timeout) ->
    gen_server:call(Server, {add_tool, Name, Handler}, Timeout).

%% Default timeout pattern
-define(DEFAULT_TIMEOUT, 5000).

add_tool(Server, Name, Handler) ->
    add_tool(Server, Name, Handler, ?DEFAULT_TIMEOUT).
```

**Checklist:**

- [ ] Cast used for fire-and-forget
- [ ] Call used for request-response
- [ ] Timeouts ≥ 5000ms (5 seconds)
- [ ] Explicit timeout for long operations
- [ ] No blocking operations in handle_call

---

## 2. Supervision Tree Compliance

### 2.1 All Processes MUST Be Supervised

**Rule:** Every spawned process MUST have a supervisor. NO unsupervised spawn.

#### ❌ VIOLATION: Unsupervised spawn

```erlang
%% WRONG: Unsupervised process
start_worker(Args) ->
    Pid = spawn(fun() -> worker_loop(Args) end),
    {ok, Pid}.

%% WRONG: spawn_link without supervisor
start_worker(Args) ->
    Pid = spawn_link(fun() -> worker_loop(Args) end),
    {ok, Pid}.
```

**Problems:**
- No crash recovery
- No monitoring
- Resource leaks on crash
- Violates OTP principle: supervised process tree

#### ✅ CORRECT: Supervised processes

**Pattern 1: simple_one_for_one supervisor**

```erlang
%% Supervisor module
-module(erlmcp_worker_sup).
-behaviour(supervisor).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpec = #{
        id => worker,
        start => {erlmcp_worker, start_link, []},
        restart => transient,  % Restart only on abnormal exit
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_worker]
    },

    {ok, {SupFlags, [ChildSpec]}}.

%% Starting worker through supervisor
start_worker(Args) ->
    supervisor:start_child(erlmcp_worker_sup, [Args]).
```

**Pattern 2: Named child in supervision tree**

```erlang
%% Top-level supervisor
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    Children = [
        #{
            id => registry,
            start => {erlmcp_registry, start_link, []},
            restart => permanent,  % Always restart
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_registry]
        },
        #{
            id => server_sup,
            start => {erlmcp_server_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,  % Supervisor
            type => supervisor,
            modules => [erlmcp_server_sup]
        }
    ],

    {ok, {SupFlags, Children}}.
```

**Real Example: erlmcp 3-Tier Supervision**

```erlang
%% TIER 1: Root supervisor (one_for_all)
%% erlmcp_sup supervises:
erlmcp_sup → {erlmcp_core_sup, erlmcp_registry}

%% TIER 2: App supervisors (simple_one_for_one)
%% Isolated per-connection processes:
erlmcp_server_sup → erlmcp_server (one per connection)
erlmcp_client_sup → erlmcp_client (one per connection)
erlmcp_session_sup → erlmcp_session (one per session)

%% TIER 3: Isolated subsystems
erlmcp_observability_sup → {erlmcp_metrics, erlmcp_dashboard, erlmcp_tracing}
```

**Checklist:**

- [ ] All processes started via supervisor:start_child/2
- [ ] No spawn/1 or spawn_link/1 in production code
- [ ] Proper restart strategies defined
- [ ] Supervision tree documented
- [ ] Tests verify supervisor behavior

---

### 2.2 Restart Strategies

**Rule:** Choose appropriate restart strategy for each child.

#### Restart Types

| Strategy | When to Use | Example |
|----------|-------------|---------|
| `permanent` | Must always run | Core services, registry, database pool |
| `transient` | Restart on abnormal exit only | Workers, connections |
| `temporary` | Never restart | One-off tasks, batch jobs |

#### ✅ CORRECT: Restart strategy selection

```erlang
%% permanent - Critical service must always run
#{
    id => registry,
    start => {erlmcp_registry, start_link, []},
    restart => permanent,  % Always restart
    shutdown => 5000,
    type => worker
}

%% transient - Worker restarts only on crash
#{
    id => worker,
    start => {erlmcp_worker, start_link, []},
    restart => transient,  % Restart only if abnormal exit
    shutdown => 5000,
    type => worker
}

%% temporary - One-off task, no restart
#{
    id => batch_job,
    start => {erlmcp_batch, start_link, []},
    restart => temporary,  % Never restart
    shutdown => 5000,
    type => worker
}
```

#### Supervision Strategies

| Strategy | Behavior | When to Use |
|----------|----------|-------------|
| `one_for_one` | Only failed child restarts | Independent children |
| `one_for_all` | All children restart if one fails | Tightly coupled services |
| `rest_for_one` | Failed child + younger siblings restart | Pipeline dependencies |
| `simple_one_for_one` | Dynamic children (all same type) | Connection pools, workers |

```erlang
%% one_for_one - Independent services
SupFlags = #{
    strategy => one_for_one,
    intensity => 5,    % Max 5 restarts
    period => 60       % Within 60 seconds
}

%% one_for_all - Tightly coupled (rare, use carefully)
SupFlags = #{
    strategy => one_for_all,
    intensity => 3,
    period => 60
}

%% simple_one_for_one - Dynamic worker pool
SupFlags = #{
    strategy => simple_one_for_one,
    intensity => 10,
    period => 60
}
```

**Checklist:**

- [ ] `permanent` for core services
- [ ] `transient` for workers
- [ ] `temporary` for one-off tasks
- [ ] `one_for_one` for independent children (most common)
- [ ] `simple_one_for_one` for dynamic pools
- [ ] Intensity/period tuned to expected failure rate

---

### 2.3 Shutdown Strategies

**Rule:** Proper shutdown prevents resource leaks and data loss.

```erlang
%% ✅ CORRECT: Shutdown configuration
#{
    id => worker,
    start => {erlmcp_worker, start_link, []},
    restart => transient,
    shutdown => 5000,        % Worker: milliseconds (brutal_kill after timeout)
    type => worker
}

#{
    id => subsystem_sup,
    start => {erlmcp_subsystem_sup, start_link, []},
    restart => permanent,
    shutdown => infinity,    % Supervisor: wait indefinitely
    type => supervisor
}
```

| Type | Shutdown Value | Behavior |
|------|----------------|----------|
| Worker | `5000` (ms) | Wait 5s, then brutal_kill |
| Worker | `infinity` | Wait forever (use for critical cleanup) |
| Supervisor | `infinity` | Always wait for children |

**Graceful Shutdown Implementation:**

```erlang
%% ✅ CORRECT: Graceful terminate
terminate(_Reason, State = #state{conn = Conn}) ->
    %% Close connections
    case Conn of
        undefined -> ok;
        _ -> close_connection(Conn)
    end,

    %% Flush pending operations
    flush_pending_writes(State),

    %% Cleanup resources
    cleanup_resources(State),
    ok.
```

**Checklist:**

- [ ] Workers: shutdown = 5000ms (or custom timeout)
- [ ] Supervisors: shutdown = infinity
- [ ] terminate/2 implements graceful cleanup
- [ ] No resource leaks on shutdown
- [ ] Tests verify graceful shutdown

---

## 3. Message Handling Compliance

### 3.1 Typed Messages

**Rule:** All messages MUST be typed records or tagged tuples.

#### ❌ VIOLATION: Untyped messages

```erlang
%% WRONG: Ambiguous tuple
handle_info({update, Data}, State) ->
    %% What kind of update? What is Data?
    {noreply, State}.

%% WRONG: Bare atom
handle_info(refresh, State) ->
    {noreply, State}.
```

#### ✅ CORRECT: Typed messages

```erlang
%% Define message types
-type server_msg() ::
    {resource_updated, uri(), metadata()} |
    {tool_added, tool_name(), tool_handler()} |
    {client_connected, client_id()} |
    timeout.

%% Pattern match on typed messages
handle_info({resource_updated, Uri, Metadata}, State) ->
    NewState = update_resource(Uri, Metadata, State),
    {noreply, NewState};

handle_info({tool_added, Name, Handler}, State) ->
    NewState = register_tool(Name, Handler, State),
    {noreply, NewState};

handle_info(timeout, State) ->
    NewState = handle_timeout(State),
    {noreply, NewState}.
```

**Record-Based Messages:**

```erlang
%% Define message records
-record(resource_update, {
    uri :: binary(),
    metadata :: map(),
    timestamp :: integer()
}).

-record(tool_notification, {
    name :: binary(),
    action :: add | remove | update,
    handler :: function()
}).

%% Pattern match on records
handle_info(#resource_update{uri = Uri, metadata = Meta}, State) ->
    NewState = update_resource(Uri, Meta, State),
    {noreply, NewState};

handle_info(#tool_notification{name = Name, action = add, handler = Handler}, State) ->
    NewState = register_tool(Name, Handler, State),
    {noreply, NewState}.
```

**Checklist:**

- [ ] All messages are tagged tuples or records
- [ ] Message types documented
- [ ] Type specs for all message types
- [ ] No bare atoms as messages
- [ ] Clear pattern matching in handle_info

---

### 3.2 Timeout Configuration

**Rule:** All timeouts MUST be ≥ 5000ms. Use infinity for no timeout.

#### ❌ VIOLATION: Too short timeout

```erlang
%% WRONG: 1 second timeout too aggressive
gen_server:call(Server, Request, 1000).

%% WRONG: Default 5s may be too short for slow operations
gen_server:call(Server, {slow_operation, Data}).
```

#### ✅ CORRECT: Appropriate timeouts

```erlang
%% CORRECT: Explicit timeout ≥ 5s
-define(DEFAULT_TIMEOUT, 5000).
-define(SLOW_OP_TIMEOUT, 30000).  % 30 seconds for slow ops
-define(BATCH_TIMEOUT, 60000).    % 1 minute for batch

gen_server:call(Server, Request, ?DEFAULT_TIMEOUT).

gen_server:call(Server, {slow_operation, Data}, ?SLOW_OP_TIMEOUT).

gen_server:call(Server, {batch_process, Items}, ?BATCH_TIMEOUT).

%% For operations that may take arbitrarily long
gen_server:call(Server, {export_database}, infinity).
```

**Timeout in gen_server:**

```erlang
%% Set process timeout (hibernation)
init(Opts) ->
    State = #state{opts = Opts},
    {ok, State, 30000}.  % Hibernate after 30s idle

handle_call(Request, _From, State) ->
    Result = process_request(Request, State),
    {reply, Result, State, 30000}.  % Continue hibernation timer
```

**Checklist:**

- [ ] No timeouts < 5000ms
- [ ] Explicit timeout for gen_server:call
- [ ] Long operations use increased timeout
- [ ] infinity used only when necessary
- [ ] Hibernation timeout configured

---

### 3.3 Queue Depth Bounds

**Rule:** Prevent mailbox overflow with bounded queues.

#### ❌ VIOLATION: Unbounded queue

```erlang
%% WRONG: No queue limit, can cause memory exhaustion
handle_cast({process, Item}, State = #state{queue = Queue}) ->
    %% Queue grows unbounded!
    NewQueue = queue:in(Item, Queue),
    {noreply, State#state{queue = NewQueue}}.
```

#### ✅ CORRECT: Bounded queue with backpressure

```erlang
-define(MAX_QUEUE_DEPTH, 1000).

handle_cast({process, Item}, State = #state{queue = Queue}) ->
    QueueLen = queue:len(Queue),
    if
        QueueLen >= ?MAX_QUEUE_DEPTH ->
            %% Reject with backpressure
            {noreply, State};
        true ->
            NewQueue = queue:in(Item, Queue),
            {noreply, State#state{queue = NewQueue}}
    end.
```

**Synchronous Backpressure:**

```erlang
%% Better: Use call for backpressure
handle_call({process, Item}, _From, State = #state{queue = Queue}) ->
    QueueLen = queue:len(Queue),
    if
        QueueLen >= ?MAX_QUEUE_DEPTH ->
            {reply, {error, queue_full}, State};
        true ->
            NewQueue = queue:in(Item, Queue),
            {reply, ok, State#state{queue = NewQueue}}
    end.
```

**Monitor Mailbox Size:**

```erlang
handle_info(check_mailbox, State) ->
    {message_queue_len, Len} = process_info(self(), message_queue_len),
    if
        Len > 10000 ->
            %% Log warning or trigger alert
            erlmcp_logger:warning("Mailbox overflow: ~p messages", [Len]);
        true ->
            ok
    end,
    erlang:send_after(5000, self(), check_mailbox),
    {noreply, State}.
```

**Checklist:**

- [ ] Queue depth limits defined
- [ ] Backpressure on queue full
- [ ] Mailbox size monitored
- [ ] Alerts on overflow
- [ ] Tests verify bounded behavior

---

## 4. Error Handling Compliance

### 4.1 Let-It-Crash Principle

**Rule:** Let supervisor handle failures. Don't swallow errors.

#### ❌ VIOLATION: Defensive programming

```erlang
%% WRONG: Swallowing errors
handle_call({process, Data}, _From, State) ->
    try
        Result = process_data(Data),
        {reply, {ok, Result}, State}
    catch
        error:Reason ->
            %% WRONG: Hiding the error
            {reply, {error, Reason}, State}
    end.

%% WRONG: Defensive nil checks everywhere
handle_call({lookup, Key}, _From, State = #state{data = Data}) ->
    case Data of
        undefined ->
            {reply, {error, not_initialized}, State};
        _ ->
            case maps:get(Key, Data, undefined) of
                undefined ->
                    {reply, {error, not_found}, State};
                Value ->
                    {reply, {ok, Value}, State}
            end
    end.
```

**Problems:**
- Hides bugs
- Error accumulation
- No automatic recovery
- Violates OTP principle: fail fast

#### ✅ CORRECT: Let-it-crash

```erlang
%% CORRECT: Let it crash on invalid data
handle_call({process, Data}, _From, State) ->
    %% No try/catch - let supervisor restart on error
    Result = process_data(Data),  % Crashes if Data invalid
    {reply, Result, State}.

%% CORRECT: Crash on invalid state
handle_call({lookup, Key}, _From, State = #state{data = Data}) ->
    %% Crash if not initialized - should never happen
    Value = maps:get(Key, Data),  % Crashes if key missing
    {reply, Value, State}.
```

**When to catch errors:**

```erlang
%% ✅ CORRECT: Catch only for expected business logic errors
handle_call({safe_lookup, Key}, _From, State = #state{data = Data}) ->
    %% Expected: key might not exist
    case maps:get(Key, Data, not_found) of
        not_found ->
            {reply, {error, not_found}, State};
        Value ->
            {reply, {ok, Value}, State}
    end.

%% ✅ CORRECT: Catch for external system failures
handle_call({fetch_remote, Url}, _From, State) ->
    %% External system may fail - expected error
    case httpc:request(Url) of
        {ok, {{_, 200, _}, _, Body}} ->
            {reply, {ok, Body}, State};
        {ok, {{_, Status, _}, _, _}} ->
            {reply, {error, {http_error, Status}}, State};
        {error, Reason} ->
            {reply, {error, {network_error, Reason}}, State}
    end.
```

**Checklist:**

- [ ] No unnecessary try/catch
- [ ] Crash on programmer errors
- [ ] Catch only expected business errors
- [ ] Supervisor configured to restart
- [ ] No silent failures

---

### 4.2 Supervisor Restart Strategies

**Rule:** Supervisor catches crashes and restarts with proper strategy.

```erlang
%% ✅ CORRECT: Supervisor handles crashes
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,    % Max 5 restarts
        period => 60       % In 60 seconds
    },

    ChildSpec = #{
        id => worker,
        start => {erlmcp_worker, start_link, []},
        restart => transient,  % Restart on abnormal exit
        shutdown => 5000,
        type => worker
    },

    {ok, {SupFlags, [ChildSpec]}}.
```

**Exponential Backoff (Custom):**

```erlang
%% For frequently failing processes
-record(backoff_state, {
    failures = 0 :: non_neg_integer(),
    last_restart :: erlang:timestamp() | undefined
}).

%% In supervisor callback
handle_info({'EXIT', Pid, Reason}, State = #backoff_state{failures = Failures}) ->
    %% Calculate backoff delay
    Delay = min(1000 * (2 bsl Failures), 60000),  % Max 60s

    %% Restart after delay
    erlang:send_after(Delay, self(), {restart_child, Pid}),

    {noreply, State#backoff_state{failures = Failures + 1}}.
```

**Checklist:**

- [ ] Supervisor intensity/period configured
- [ ] Restart strategy matches use case
- [ ] Backoff for frequently failing processes
- [ ] Tests verify restart behavior
- [ ] Logging on repeated failures

---

### 4.3 Monitor vs Link

**Rule:** Use monitors for cleanup, links for tight coupling only.

#### Links (Bidirectional)

```erlang
%% ✅ CORRECT: Link for tightly coupled processes
%% Both processes exit if either crashes
start_link() ->
    Pid = spawn_link(fun() -> worker_loop() end),
    {ok, Pid}.

%% Process dies if Pid crashes
link(Pid),

%% Under supervision - link is automatic
gen_server:start_link(?MODULE, [], []).
```

**When to use links:**
- Parent-child relationship in supervision tree
- Tightly coupled processes
- Both should die together

#### Monitors (Unidirectional)

```erlang
%% ✅ CORRECT: Monitor for resource cleanup
start_worker(Args) ->
    Pid = spawn(fun() -> worker_loop(Args) end),
    MonitorRef = erlang:monitor(process, Pid),
    {ok, Pid, MonitorRef}.

%% Handle worker death
handle_info({'DOWN', Ref, process, Pid, Reason}, State = #state{workers = Workers}) ->
    %% Cleanup: remove worker from state
    NewWorkers = lists:keydelete(Pid, 1, Workers),
    {noreply, State#state{workers = NewWorkers}}.

%% Demonitor when done
erlang:demonitor(MonitorRef, [flush]).
```

**When to use monitors:**
- Resource cleanup
- Client tracking
- Notification of process death
- One-way dependency

**Real Example from erlmcp_server.erl:**

```erlang
%% Monitor notifier process for cleanup
handle_call({register_notification_handler, Method, HandlerPid}, _From, State) ->
    MonitorRef = erlang:monitor(process, HandlerPid),
    Handlers = State#state.notification_handlers,
    NewHandlers = maps:put(Method, {HandlerPid, MonitorRef}, Handlers),
    {reply, ok, State#state{notification_handlers = NewHandlers}}.

%% Cleanup on handler death
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    %% Remove dead handler
    Handlers = maps:filter(
        fun(_, {HPid, HRef}) -> HPid =/= Pid andalso HRef =/= Ref end,
        State#state.notification_handlers
    ),
    {noreply, State#state{notification_handlers = Handlers}}.
```

**Checklist:**

- [ ] Links for parent-child supervision
- [ ] Monitors for resource cleanup
- [ ] Handle 'DOWN' messages
- [ ] Demonitor when no longer needed
- [ ] No accidental bidirectional coupling

---

## 5. Testing Compliance (Chicago School TDD)

### 5.1 Real Processes Only

**Rule:** Tests MUST use real erlmcp processes. NO mocks.

#### ❌ VIOLATION: Mock/stub usage

```erlang
%% WRONG: Using meck mock framework
test_with_mock() ->
    meck:new(erlmcp_registry),
    meck:expect(erlmcp_registry, lookup, fun(_) -> {ok, self()} end),

    %% Test code
    {ok, Pid} = erlmcp_server:start_link(...),

    meck:unload(erlmcp_registry).

%% WRONG: Dummy process
test_with_dummy() ->
    DummyPid = spawn(fun() -> receive stop -> ok end end),
    %% Test code
    DummyPid ! stop.
```

#### ✅ CORRECT: Real processes

```erlang
%% CORRECT: Use real erlmcp processes
test_with_real_processes() ->
    %% Start real registry
    {ok, RegPid} = erlmcp_registry:start_link(),

    %% Start real server
    {ok, ServerPid} = erlmcp_server:start_link(server_id, #{
        capabilities => #mcp_server_capabilities{}
    }),

    %% Test observable behavior
    ok = erlmcp_server:add_tool(ServerPid, <<"test_tool">>, fun(_) -> {ok, <<"result">>} end),

    %% Cleanup
    erlmcp_server:stop(ServerPid),
    erlmcp_registry:stop(RegPid).
```

**Checklist:**

- [ ] All tests use real erlmcp processes
- [ ] No meck, mock, stub, fake modules
- [ ] No dummy spawn processes
- [ ] Test observable behavior only
- [ ] Proper cleanup after tests

---

### 5.2 No State Inspection

**Rule:** Test observable behavior, not internal state.

#### ❌ VIOLATION: State inspection

```erlang
%% WRONG: Inspecting internal state
test_state_inspection() ->
    {ok, Pid} = erlmcp_server:start_link(...),

    %% VIOLATION: Using sys:get_status
    {status, _, _, [_, _, _, _, Misc]} = sys:get_status(Pid),
    State = proplists:get_value(state, lists:last(Misc)),

    %% VIOLATION: Asserting on internal state
    ?assertMatch(#state{initialized = true}, State).
```

#### ✅ CORRECT: Observable behavior

```erlang
%% CORRECT: Test observable behavior
test_observable_behavior() ->
    {ok, Pid} = erlmcp_server:start_link(...),

    %% Test through public API
    ok = erlmcp_server:add_tool(Pid, <<"tool">>, Handler),

    %% Verify behavior, not state
    {ok, Tools} = erlmcp_server:list_tools(Pid),
    ?assert(lists:member(<<"tool">>, Tools)).
```

**Black-Box Testing Principle:**

```erlang
%% ✅ CORRECT: Black-box test pattern
test_resource_subscription() ->
    {ok, Server} = erlmcp_server:start_link(...),

    %% Subscribe to resource
    ok = erlmcp_server:subscribe_resource(Server, <<"test://resource">>, self()),

    %% Trigger update
    ok = erlmcp_server:notify_resource_updated(Server, <<"test://resource">>, #{}),

    %% Verify observable behavior: message received
    receive
        {resource_updated, <<"test://resource">>, _} ->
            ok
    after 1000 ->
        ?assert(false, "Did not receive update notification")
    end.
```

**Checklist:**

- [ ] No sys:get_status in tests
- [ ] No sys:get_state in tests
- [ ] Test only through public API
- [ ] Assert on observable behavior
- [ ] No access to internal state records

---

### 5.3 Property-Based Testing

**Rule:** Use PropEr for testing invariants and edge cases.

```erlang
-module(erlmcp_server_proper_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Property: Adding and removing tools maintains consistency
prop_tool_lifecycle() ->
    ?FORALL(ToolName, binary(),
        begin
            {ok, Server} = erlmcp_server:start_link(server_id, #{}),

            %% Add tool
            ok = erlmcp_server:add_tool(Server, ToolName, fun(_) -> ok end),
            {ok, Tools1} = erlmcp_server:list_tools(Server),
            Has1 = lists:member(ToolName, Tools1),

            %% Remove tool
            ok = erlmcp_server:delete_tool(Server, ToolName),
            {ok, Tools2} = erlmcp_server:list_tools(Server),
            Has2 = lists:member(ToolName, Tools2),

            erlmcp_server:stop(Server),

            %% Invariant: tool present after add, absent after delete
            Has1 andalso (not Has2)
        end).

%% Run property test
proper_test() ->
    ?assert(proper:quickcheck(prop_tool_lifecycle(), [{numtests, 100}])).
```

**Checklist:**

- [ ] Property tests for invariants
- [ ] Generator-based edge case testing
- [ ] Real processes in property tests
- [ ] Cleanup after each property test iteration
- [ ] 100+ test cases per property

---

## 6. Documentation Compliance

### 6.1 Module Headers

**Rule:** All modules MUST have comprehensive headers.

```erlang
%%% @doc
%%% MCP Server implementation providing resource, tool, and prompt management.
%%%
%%% This module implements the server-side MCP protocol with:
%%% - Resource management and subscriptions
%%% - Tool registration and execution
%%% - Prompt template management
%%% - Progress notifications
%%% - Multi-phase initialization (MCP 2025-11-25)
%%%
%%% ## Supervision
%%%
%%% Each server process is supervised by `erlmcp_server_sup' using
%%% `simple_one_for_one' strategy for per-connection isolation.
%%%
%%% ## Phase Management
%%%
%%% Servers follow MCP initialization protocol:
%%% 1. initialization - Waiting for initialize request
%%% 2. ready - Fully initialized and accepting requests
%%%
%%% @see erlmcp_client
%%% @see erlmcp_server_sup
%%% @end
-module(erlmcp_server).
-behaviour(gen_server).

-author("erlmcp team").
-version("2.1.0").
```

**Checklist:**

- [ ] @doc with module purpose
- [ ] Supervision tree described
- [ ] Key behaviors documented
- [ ] @see references to related modules
- [ ] Version and author information

---

### 6.2 Function Specs

**Rule:** All exported functions MUST have type specs.

```erlang
%% ✅ CORRECT: Complete type specification
-spec add_tool(server(), binary(), tool_handler()) -> ok | {error, term()}.
add_tool(Server, Name, Handler) when is_binary(Name), is_function(Handler, 1) ->
    gen_server:call(Server, {add_tool, Name, Handler}).

%% ✅ CORRECT: With timeout
-spec add_tool(server(), binary(), tool_handler(), timeout()) -> ok | {error, term()}.
add_tool(Server, Name, Handler, Timeout) ->
    gen_server:call(Server, {add_tool, Name, Handler}, Timeout).

%% ✅ CORRECT: Complex return types
-spec list_tools(server()) ->
    {ok, [tool_name()]} |
    {error, not_initialized | timeout}.
list_tools(Server) ->
    gen_server:call(Server, list_tools, 5000).
```

**Type Definitions:**

```erlang
%% ✅ CORRECT: Define custom types
-type server() :: pid().
-type server_id() :: term().
-type tool_name() :: binary().
-type tool_handler() :: fun((map()) -> {ok, term()} | {error, term()}).
-type resource_handler() :: fun((binary()) -> {ok, #mcp_resource{}} | {error, term()}).

%% Export types
-export_type([server/0, server_id/0, tool_name/0, tool_handler/0]).
```

**Checklist:**

- [ ] All exported functions have -spec
- [ ] Custom types defined
- [ ] Types exported with -export_type
- [ ] Return types complete (ok/error tuples)
- [ ] Guards match specs

---

### 6.3 Function Documentation

**Rule:** Complex functions MUST have @doc comments.

```erlang
%%% @doc
%%% Add a tool with full configuration options.
%%%
%%% This is the most flexible tool registration function, allowing:
%%% - Custom description (max 10000 characters)
%%% - JSON schema validation
%%% - Metadata annotations
%%%
%%% @param Server The server process
%%% @param Name Tool name (must be unique)
%%% @param Description Human-readable description
%%% @param Handler Function that executes the tool
%%% @param Options Additional options:
%%%   - `schema' - JSON schema for parameter validation
%%%   - `metadata' - Custom metadata map
%%%
%%% @returns `ok' on success, `{error, Reason}' on failure
%%%
%%% @example
%%% ```
%%% Handler = fun(Params) ->
%%%     Name = maps:get(<<"name">>, Params),
%%%     {ok, #{greeting => <<"Hello, ", Name/binary>>}}
%%% end,
%%%
%%% Options = #{
%%%     schema => #{
%%%         type => object,
%%%         properties => #{
%%%             <<"name">> => #{type => string}
%%%         },
%%%         required => [<<"name">>]
%%%     }
%%% },
%%%
%%% ok = erlmcp_server:add_tool_full(
%%%     Server,
%%%     <<"greet">>,
%%%     <<"Greets a user by name">>,
%%%     Handler,
%%%     Options
%%% ).
%%% '''
%%% @end
-spec add_tool_full(server(), binary(), binary(), tool_handler(), map()) ->
    ok | {error, term()}.
add_tool_full(Server, Name, Description, Handler, Options) ->
    gen_server:call(Server, {add_tool_full, Name, Description, Handler, Options}).
```

**Checklist:**

- [ ] @doc for public API functions
- [ ] @param for all parameters
- [ ] @returns describing all outcomes
- [ ] @example with working code
- [ ] @see references where helpful

---

## 7. Code Review Checklist

Use this checklist for every code review:

### 7.1 gen_server/gen_statem

- [ ] All 6 callbacks exported (gen_server) or 7 (gen_statem)
- [ ] init/1 returns in < 100ms (no blocking)
- [ ] {continue, ...} or self() ! msg for async init
- [ ] handle_call for request-response
- [ ] handle_cast for fire-and-forget
- [ ] handle_info for all message types
- [ ] terminate/2 implements graceful cleanup
- [ ] format_status/2 for sensitive data

### 7.2 Supervision

- [ ] All processes supervised (no bare spawn)
- [ ] Proper restart strategy (permanent/transient/temporary)
- [ ] Supervision strategy matches use case
- [ ] Intensity/period configured
- [ ] Shutdown timeouts appropriate
- [ ] Supervision tree documented

### 7.3 Message Handling

- [ ] All messages typed (records or tagged tuples)
- [ ] Timeouts ≥ 5000ms
- [ ] Queue depth bounded
- [ ] Backpressure on overload
- [ ] Mailbox size monitored

### 7.4 Error Handling

- [ ] Let-it-crash for programmer errors
- [ ] Catch only expected business errors
- [ ] No silent failures
- [ ] Supervisor configured to restart
- [ ] Monitors for resource cleanup

### 7.5 Testing

- [ ] Real erlmcp processes (no mocks)
- [ ] No sys:get_status or sys:get_state
- [ ] Observable behavior testing only
- [ ] Property tests for invariants
- [ ] Proper cleanup after tests

### 7.6 Documentation

- [ ] Module header with @doc
- [ ] All exported functions have -spec
- [ ] Custom types defined and exported
- [ ] @doc for public API
- [ ] Working @example code

### 7.7 Quality Gates

- [ ] Compiles without errors
- [ ] All tests pass
- [ ] Coverage ≥ 80%
- [ ] No dialyzer warnings
- [ ] No xref undefined calls
- [ ] Formatted (rebar3 format)

---

## 8. Anti-Patterns Summary

### Common Violations

| Anti-Pattern | Why Wrong | Fix |
|--------------|-----------|-----|
| Blocking init/1 | Blocks supervisor tree | Use {continue, ...} |
| Unsupervised spawn | No crash recovery | Use supervisor:start_child |
| Mock/stub in tests | Hides real behavior | Use real processes |
| sys:get_status in tests | Tests implementation | Test observable behavior |
| Swallowing errors | Hides bugs | Let it crash |
| Timeout < 5s | False timeouts | Use ≥ 5000ms |
| Unbounded queue | Memory exhaustion | Add queue limit |
| Missing type specs | No type safety | Add -spec for all exports |

---

## 9. Enforcement

### 9.1 Automated (CI/CD)

```bash
# Compilation
TERM=dumb rebar3 compile || exit 1

# Xref (undefined function calls)
rebar3 xref || exit 1

# Dialyzer (type errors)
rebar3 dialyzer || exit 1

# Tests
rebar3 eunit || exit 1
rebar3 ct || exit 1

# Coverage
rebar3 cover --verbose
./scripts/check_coverage_threshold.sh 80 || exit 1

# Chicago TDD anti-patterns
./.github/scripts/chicago-tdd-scan.sh || exit 1
```

### 9.2 Pre-Commit Hooks

```bash
#!/bin/bash
# .git/hooks/pre-commit

echo "🚦 OTP Compliance Check..."

# Check for unsupervised spawn
if grep -r "spawn(fun()" apps/*/src/ > /dev/null 2>&1; then
    echo "❌ Found unsupervised spawn - use supervisor:start_child"
    exit 1
fi

# Check for mocks in tests
if grep -r "meck:new\|meck:expect" apps/*/test/ > /dev/null 2>&1; then
    echo "❌ Found mock usage - use real processes"
    exit 1
fi

# Check for state inspection
if grep -r "sys:get_status\|sys:get_state" apps/*/test/ > /dev/null 2>&1; then
    echo "❌ Found state inspection - test observable behavior"
    exit 1
fi

echo "✅ OTP compliance checks passed"
```

### 9.3 Manual Review

Every PR requires manual code review using checklist in Section 7.

---

## 10. Resources

### Internal Documentation

- `CLAUDE.md` - Project specification
- `docs/ERLMCP_FLOW_QUALITY_STANDARDS.md` - Quality standards
- `docs/architecture/OTP_PATTERNS.md` - OTP patterns guide
- `.claude/TCPS_SYSTEM_COMPLETE.md` - Quality system

### External References

- [Erlang OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
- [Learn You Some Erlang - OTP](http://learnyousomeerlang.com/what-is-otp)
- [Joe Armstrong's Thesis](https://erlang.org/download/armstrong_thesis_2003.pdf)
- [Designing for Scalability with Erlang/OTP](https://www.oreilly.com/library/view/designing-for-scalability/9781449361556/)

---

**Document Version:** 1.0.0
**Last Updated:** 2026-02-01
**Maintained By:** erlmcp-flow Core Team
**Review Cycle:** Quarterly

**CODE LIKE A JOE ARMSTRONG AGI SWARM!!!**
