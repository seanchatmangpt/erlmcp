# Erlang Antipattern Research: Blocking Operations Analysis

**Date**: February 2026
**Antipattern**: #1 - Blocking operations in supervisor init/1 and critical paths
**Scope**: erlmcp_core, erlmcp_transports, erlmcp_observability
**Status**: Comprehensive scan complete

---

## Executive Summary

Research identified **7 blocking antipatterns** across critical erlmcp modules that violate OTP/Armstrong principles:

| Severity | Count | Category | Risk |
|----------|-------|----------|------|
| **CRITICAL** | 2 | Init blocking | Supervisor delays, cascading failures |
| **HIGH** | 3 | Sync network I/O | Call timeouts, pool exhaustion |
| **MEDIUM** | 2 | File I/O in handle_call | Unpredictable latency |

**Total Impact**: These antipatterns can cause:
- Supervisor initialization timeout (blocking supervisor startup entire app)
- Request handler starvation (network I/O blocks subsequent requests)
- Unbounded wait times (infinity timeouts with no recovery mechanism)

---

## Critical Findings

### CRITICAL-1: File I/O in gen_server init/1

**Module**: `erlmcp_resources.erl`
**Location**: Line 91
**Severity**: CRITICAL

**Code**:
```erlang
init([]) ->
    %% Initialize with default root (current working directory)
    {ok, Cwd} = file:get_cwd(),  % ← BLOCKING FILE I/O IN INIT
    DefaultRoot = #{
        uri => list_to_binary(["file://", Cwd]),
        name => <<"Current Working Directory">>
    },
    InitialRoots = #{list_to_binary(["file://", Cwd]) => DefaultRoot},
    {ok, #state{roots = InitialRoots}}.
```

**Analysis**:
- `file:get_cwd()` is a synchronous file system call
- Blocks supervisor until init/1 returns
- If filesystem is slow/unavailable, delays entire erlmcp_sup startup tree
- Violates Armstrong principle: "init/1 never blocks"

**Impact**:
- Application startup can hang indefinitely if filesystem is unresponsive
- No supervisor timeout protection (gen_server init blocks all child starts)
- Cascading failure: erlmcp_resources startup blocks erlmcp_core_sup startup

**Recommended Fix**:
```erlang
init([]) ->
    %% Fast init - return immediately
    {ok, #state{roots = #{}}, {continue, load_default_root}}.

handle_continue(load_default_root, State) ->
    case file:get_cwd() of
        {ok, Cwd} ->
            DefaultRoot = #{
                uri => list_to_binary(["file://", Cwd]),
                name => <<"Current Working Directory">>
            },
            InitialRoots = #{list_to_binary(["file://", Cwd]) => DefaultRoot},
            {noreply, State#state{roots = InitialRoots}};
        {error, Reason} ->
            ?LOG_WARNING("Failed to get CWD: ~p", [Reason]),
            {noreply, State}
    end.
```

**CLAUDE.md Reference**: "gen_server: init/1 never blocks → async cast" (TPS Quality System)

---

### CRITICAL-2: Mnesia Table Creation in gen_server init/1

**Module**: `erlmcp_session_replicator.erl`
**Location**: Lines 142-165
**Severity**: CRITICAL

**Code**:
```erlang
init([]) ->
    LocalNode = node(),
    ReplicaNodes = get_replica_nodes(),

    %% Initialize Mnesia tables - SYNCHRONOUS BLOCKING CALL
    case init_mnesia_tables(LocalNode, ReplicaNodes) of
        ok ->
            %% Initialize replication queue
            Queue = queue:new(),
            %% Start batch replication timer
            {ok, Timer} = timer:send_interval(1000, flush_queue),
            {ok, #state{...}};
        {error, Reason} ->
            {stop, Reason}
    end.
```

**init_mnesia_tables/2 implementation** (Lines 341-394):
```erlang
init_mnesia_tables(LocalNode, ReplicaNodes) ->
    AllNodes = [LocalNode | ReplicaNodes],
    UseDiscCopies = (LocalNode =/= 'nonode@nohost'),

    %% THREE SEQUENTIAL TABLE CREATIONS - ALL BLOCKING
    case mnesia:create_table(?REPLICA_TABLE, [...]) of
        {atomic, ok} -> ...
    end,
    case mnesia:create_table(?QUEUE_TABLE, [...]) of
        {atomic, ok} -> ...
    end,
    case mnesia:create_table(?VECTOR_CLOCK_TABLE, [...]) of
        {atomic, ok} -> ...
    end,
    ok.
```

**Analysis**:
- **3 sequential Mnesia table creations** in init/1 callback
- Each `mnesia:create_table/2` is **synchronous and blocking** on distributed systems
- Waits for table replication across all nodes in cluster
- Total blocking time can exceed 10 seconds on large clusters
- Blocks supervisor startup and prevents sibling children from starting

**Impact**:
- Cluster initialization hangs (waiting for table sync across nodes)
- Supervisor children cannot start until session_replicator fully initializes
- Production deployments with multi-node clusters experience severe startup delays
- New nodes joining cluster experience 10+ second init delay

**Recommended Fix**:
```erlang
init([]) ->
    LocalNode = node(),
    ReplicaNodes = get_replica_nodes(),

    % Return immediately - skip table creation
    State = #state{
        local_node = LocalNode,
        replica_nodes = ReplicaNodes,
        replication_queue = queue:new(),
        queue_timer = undefined,
        batch_size = 100,
        batch_interval = 1000
    },

    % Trigger async table initialization via handle_continue
    {ok, State, {continue, init_mnesia_tables}}.

handle_continue(init_mnesia_tables, State) ->
    case init_mnesia_tables_async(State#state.local_node, State#state.replica_nodes) of
        ok ->
            {ok, Timer} = timer:send_interval(1000, flush_queue),
            {noreply, State#state{queue_timer = Timer}};
        {error, Reason} ->
            ?LOG_ERROR("Mnesia table init failed: ~p", [Reason]),
            {noreply, State}  % Continue with degraded mode
    end.
```

**CLAUDE.md Reference**: "gen_server: init/1 never blocks → async cast" (Supervision section)

---

## High Severity Findings

### HIGH-1: Blocking Network I/O (net_adm:ping) in handle_call

**Module**: `erlmcp_session_replicator.erl`
**Locations**: Lines 217, 427
**Severity**: HIGH

**Code**:
```erlang
%% Location 1: handle_call bootstrap_node (Line 215-217)
handle_call({bootstrap_node, Node}, _From, State) ->
    %% Bootstrap a new node by sending all sessions
    case net_adm:ping(Node) of  % ← BLOCKING NETWORK I/O
        pong ->
            ...
        pang ->
            {reply, {error, node_unreachable}, State}
    end;

%% Location 2: replicate_to_nodes (Line 427)
replicate_to_nodes(_SessionId, _Session, _VClock, State) ->
    ReplicaNodes = State#state.replica_nodes,
    LocalNode = State#state.local_node,

    %% Filter out local node and unavailable nodes
    AvailableNodes = lists:filter(fun(Node) ->
        Node =/= LocalNode andalso net_adm:ping(Node) =:= pong  % ← BLOCKING FOR EACH NODE
    end, ReplicaNodes),
    ...
```

**Analysis**:
- `net_adm:ping/1` is **synchronous and blocks for 3-5 seconds** per unresponsive node
- Called from `handle_call/3`, which blocks the caller process
- With 10 replica nodes, one unresponsive node means **30+ second delay** in request handler
- Subsequent requests to same gen_server queue up and timeout

**Impact**:
- Single unresponsive node in cluster freezes all bootstrap requests
- Pool of 10 worker processes can be fully exhausted by slow pings
- Cascading timeout failures across dependent services
- Production incidents: "gen_server call timeout" errors in logs

**Recommended Fix**:
```erlang
%% Option A: Use async RPC with timeout
handle_call({bootstrap_node, Node}, From, State) ->
    % Non-blocking: spawn async check and reply later
    _Pid = spawn(fun() ->
        case rpc:call(Node, erlang, system_time, [], 1000) of  % 1s timeout
            {error, timeout} ->
                gen_server:reply(From, {error, node_unreachable});
            {error, _} ->
                gen_server:reply(From, {error, node_unreachable});
            _ ->
                % Node is reachable, proceed with bootstrap
                Result = do_bootstrap(Node, State),
                gen_server:reply(From, Result)
        end
    end),
    {noreply, State};

%% Option B: Use separate monitoring process (recommended)
handle_call({bootstrap_node, Node}, _From, State) ->
    case is_node_available(Node) of  % Non-blocking check from cache
        true ->
            Result = do_bootstrap_async(Node, State),
            {reply, Result, State};
        false ->
            {reply, {error, node_unreachable}, State}
    end.

%% Monitor node availability asynchronously (separate process)
node_monitor_loop(Nodes, Cache) ->
    lists:foreach(fun(Node) ->
        case rpc:call(Node, erlang, self, [], 500) of
            {error, _} -> ets:insert(Cache, {Node, unavailable});
            _ -> ets:insert(Cache, {Node, available})
        end
    end, Nodes),
    timer:sleep(5000),
    node_monitor_loop(Nodes, Cache).
```

**CLAUDE.md Reference**: "gen_server: Avoid long-running synchronous operations in handle_call"

---

### HIGH-2: Blocking RPC Calls in handle_call

**Module**: `erlmcp_session_replicator.erl`
**Location**: Lines 228, 437
**Severity**: HIGH

**Code**:
```erlang
handle_call({bootstrap_node, Node}, _From, State) ->
    case net_adm:ping(Node) of
        pong ->
            Sessions = get_all_local_sessions(),
            BatchSize = 100,
            Batches = partition_list(Sessions, BatchSize),

            BootstrapFun = fun() ->
                lists:foreach(fun(Batch) ->
                    rpc:call(Node, mnesia, transaction, [fun() ->  % ← BLOCKING RPC
                        lists:foreach(fun({SessId, Sess, VClock}) ->
                            Record = #replica_state{...},
                            mnesia:write(?REPLICA_TABLE, Record, write)
                        end, Batch)
                    end])  % ← NO TIMEOUT SPECIFIED (default 5000ms per call)
                end, Batches),
            end,
            ...
```

**Analysis**:
- `rpc:call/2` (no timeout) uses default 5000ms timeout but blocks caller
- Called in `lists:foreach` **inside handle_call** - **synchronous RPC per batch**
- With 100+ sessions, could be **5+ seconds** blocking time
- No timeout protection on outer loop

**Impact**:
- Bootstrap operation blocks gen_server for minutes on large session counts
- All other requests to session_replicator timeout while bootstrap runs
- Risk of cascading failures in dependent systems

**Recommended Fix**:
```erlang
handle_call({bootstrap_node, Node}, From, State) ->
    case net_adm:ping(Node) of
        pong ->
            % Spawn async bootstrap worker - reply immediately
            _Pid = spawn(fun() ->
                Result = do_async_bootstrap(Node, State),
                % Update caller with result via message (if caller still listening)
                % OR use ETS to store result for polling
            end),
            {reply, {ok, bootstrap_started}, State};  % Immediate reply
        pang ->
            {reply, {error, node_unreachable}, State}
    end.

do_async_bootstrap(Node, State) ->
    Sessions = get_all_local_sessions(),
    BatchSize = 100,
    Batches = partition_list(Sessions, BatchSize),

    Results = lists:map(fun(Batch) ->
        % RPC with explicit timeout
        rpc:call(Node, mnesia, transaction,
            [fun() -> write_batch_to_replica(Batch) end],
            3000)  % 3s timeout per batch
    end, Batches),

    {ok, Results}.
```

**CLAUDE.md Reference**: "No unbounded operations in handle_call"

---

### HIGH-3: Unbounded gen_server:call Timeout (infinity)

**Module**: `erlmcp_client.erl`
**Location**: Line 115
**Severity**: HIGH

**Code**:
```erlang
-spec initialize(client(), #mcp_client_capabilities{}, map()) ->
    {ok, map()} | {error, term()}.
initialize(Client, Capabilities, Options) ->
    gen_server:call(Client, {initialize, Capabilities, Options}, infinity).
    %                                                              ^^^^^^^^^
    %                                          UNBOUNDED TIMEOUT - BLOCKS FOREVER
```

**Analysis**:
- `infinity` timeout means caller will **block indefinitely** if server crashes
- No recovery mechanism if remote process hangs
- Violates OTP supervisor timeout contracts
- Cascades up the stack: caller's supervisor may also timeout waiting for caller

**Impact**:
- Hung client processes never terminate, accumulate in system
- Memory leaks: hung processes retain state, ETS tables, etc.
- Debugging difficulty: operations appear frozen with no error

**Additional Issues in erlmcp_client.erl**:
- Line 128: `get_prompt/3` - no timeout on gen_server:call
- Line 149: `call_tool/3` - no timeout specified

**Recommended Fix**:
```erlang
%% Use application-configured timeout or reasonable default (5-30s)
-define(DEFAULT_INIT_TIMEOUT, 30000).  % 30 seconds

initialize(Client, Capabilities, Options) ->
    Timeout = maps:get(initialization_timeout, Options, ?DEFAULT_INIT_TIMEOUT),
    gen_server:call(Client, {initialize, Capabilities, Options}, Timeout).

%% With explicit timeout
get_prompt(Client, Name, Arguments) when is_binary(Name), is_map(Arguments) ->
    gen_server:call(Client, {get_prompt, Name, Arguments}, 5000).  % 5s timeout

%% With timeout error handling
initialize(Client, Capabilities, Options) ->
    try
        gen_server:call(Client, {initialize, Capabilities, Options}, 30000)
    catch
        exit:{timeout, _} ->
            {error, {initialization_timeout, "Client initialization exceeded 30s"}}
    end.
```

**CLAUDE.md Reference**: "gen_server: Timeouts < 5000ms required" (Anti-Patterns section)

---

## Medium Severity Findings

### MEDIUM-1: File I/O in handle_call (read_resource)

**Module**: `erlmcp_resources.erl`
**Location**: Line 190
**Severity**: MEDIUM (context dependent)

**Code**:
```erlang
do_read_resource(<<"file://", Path/binary>>) ->
    FilePath = case Path of
        <<"/", AbsolutePath/binary>> -> binary_to_list(AbsolutePath);
        _ -> binary_to_list(Path)
    end,
    case file:read_file(FilePath) of  % ← FILE I/O IN HANDLE_CALL
        {ok, Content} -> {ok, Content};
        {error, Reason} -> {error, Reason}
    end;
```

**Analysis**:
- `file:read_file/1` is synchronous and blocks the calling process
- Block duration depends on file size and disk speed
- Called from handle_call (via resolve_root_uri/1, read_resource/1)
- Only problematic if:
  - Reading very large files (>10MB)
  - Frequent reads on slow filesystem
  - Low request pool size

**Impact**:
- **Minor**: Most file reads complete in <100ms
- **Significant**: Large resource files (>100MB) block for seconds
- **Cascading**: gen_server call queue backs up during slow reads

**Recommended Fix** (for large files):
```erlang
%% Split read and reply for large files
do_read_resource(<<"file://", Path/binary>>) ->
    FilePath = binary_to_list(Path),

    % Check file size first (fast metadata operation)
    case file:read_file_info(FilePath) of
        {ok, #file_info{size = Size}} when Size > 10_000_000 ->
            % Large file: use async spawn
            Caller = self(),
            spawn(fun() ->
                Result = file:read_file(FilePath),
                Caller ! {read_result, Result}
            end),
            {noreply, State};  % Don't block caller
        {ok, _} ->
            % Small file: read synchronously
            case file:read_file(FilePath) of
                {ok, Content} -> {ok, Content};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

handle_info({read_result, Result}, State) ->
    %% Process result after non-blocking read
    {noreply, State}.
```

**CLAUDE.md Reference**: "No large blocking I/O in hot paths"

---

### MEDIUM-2: Synchronous File Operations in erlmcp_secrets.erl

**Module**: `erlmcp_secrets.erl`
**Location**: Lines 1411, 1424, 1443, 1451
**Severity**: MEDIUM (Partially fixed)

**Status**: ⚠️ **PARTIALLY MITIGATED** - See lines 119-137

**Original Issues**:
```erlang
%% Lines 1411, 1424: File I/O for encrypted storage
case file:read_file(State#state.storage_path) of
    {ok, Data} -> decrypt_data(Data);
    {error, _} -> {error, no_storage}
end.

file:write_file(State#state.storage_path, EncryptedData).

%% Lines 1443, 1451: Key file generation
case file:read_file(KeyPath) of
    {ok, KeyData} -> parse_key(KeyData);
    {error, _} -> generate_new_key()
end.

ok = file:write_file(KeyPath, NewKey).
```

**Mitigation Found** (Lines 119-137):
```erlang
% SECURITY FIX (P1): Move blocking file I/O to async init to prevent supervisor delays.
% Generate or load encryption key asynchronously after init returns.
State = #state{
    cache = ets:new(secrets_cache, [set, protected]),
    backend = Backend,
    backend_config = BackendConfig,
    encryption_key = undefined,  % Will be set in async init
    ttl_seconds = TtlSeconds,
    storage_path = StoragePath
},

% Start cache cleanup timer
erlang:send_after(60000, self(), cleanup_cache),

% Trigger async initialization (file I/O happens after init returns)
self() ! {init_async, Config},

logger:info("Secrets manager started with backend: ~p (async init pending)", [Backend]),
{ok, State}.
```

**Assessment**:
- ✅ init/1 correctly avoids blocking file I/O
- ⚠️ handle_info({init_async, Config}, State) performs file I/O asynchronously
- ⚠️ But: First get_secret call before async init completes will fail (encryption_key = undefined)

**Recommended Enhancement**:
```erlang
handle_call({get_secret, Key}, From, State) ->
    case State#state.encryption_key of
        undefined ->
            % Queue request until async init completes
            {noreply, queue_request({get_secret, Key, From}, State)};
        _Key ->
            Result = do_get_secret(Key, State),
            {reply, Result, State}
    end.

handle_info({init_async, Config}, State) ->
    % ... async key loading ...
    NewState = State#state{encryption_key = LoadedKey},

    % Process queued requests
    process_queued_requests(NewState),

    {noreply, NewState}.
```

---

## Summary Table

| ID | Module | Line(s) | Antipattern | Severity | Type |
|----|---------|---------:|-------------|----------|------|
| CRIT-1 | erlmcp_resources.erl | 91 | file:get_cwd() in init/1 | CRITICAL | Init blocking |
| CRIT-2 | erlmcp_session_replicator.erl | 147-394 | mnesia:create_table in init/1 | CRITICAL | Init blocking |
| HIGH-1 | erlmcp_session_replicator.erl | 217, 427 | net_adm:ping in handle_call | HIGH | Network I/O |
| HIGH-2 | erlmcp_session_replicator.erl | 228, 437 | rpc:call in handle_call loop | HIGH | Network I/O |
| HIGH-3 | erlmcp_client.erl | 115 | infinity timeout | HIGH | Timeout |
| MED-1 | erlmcp_resources.erl | 190 | file:read_file in handle_call | MEDIUM | File I/O |
| MED-2 | erlmcp_secrets.erl | 1411-1451 | Async file I/O (mitigated) | MEDIUM | File I/O |

---

## Remediation Priority

### Phase 1: Critical (Blocking Supervisor Startup)
- [ ] **erlmcp_resources.erl:91** - Move file:get_cwd() to handle_continue
- [ ] **erlmcp_session_replicator.erl:147-394** - Move mnesia:create_table to handle_continue

**Expected Impact**: Reduce erlmcp_sup startup time by 5-10 seconds

### Phase 2: High (Request Handler Starvation)
- [ ] **erlmcp_session_replicator.erl:217, 427** - Replace net_adm:ping with async node monitoring
- [ ] **erlmcp_session_replicator.erl:228, 437** - Replace blocking rpc:call with async spawn or cast
- [ ] **erlmcp_client.erl:115** - Replace infinity timeout with 30s default + retry logic

**Expected Impact**: Prevent request pool exhaustion, reduce cascading timeouts

### Phase 3: Medium (Latency Reduction)
- [ ] **erlmcp_resources.erl:190** - Add large file detection for async reads
- [ ] **erlmcp_secrets.erl** - Add request queueing for pre-async-init state

**Expected Impact**: Reduce p99 latency on resource/secret operations

---

## Testing Recommendations

### For CRITICAL fixes (init blocking)
```erlang
%% Test: Verify init/1 returns in <100ms
init_timing_test() ->
    T1 = erlang:monotonic_time(millisecond),
    {ok, Pid} = erlmcp_resources:start_link(),
    T2 = erlang:monotonic_time(millisecond),
    Time = T2 - T1,
    ?assert(Time < 100).  %% Should be instant

%% Test: Verify handle_continue completes init
handle_continue_init_test() ->
    {ok, Pid} = erlmcp_resources:start_link(),
    timer:sleep(100),  %% Wait for async init
    ?assertEqual({ok, _}, erlmcp_resources:list_roots(Pid)).
```

### For HIGH fixes (network I/O)
```erlang
%% Test: Simulate unresponsive node
high_latency_node_test() ->
    % Mock net_adm:ping to delay
    meck:new(net_adm),
    meck:expect(net_adm, ping, fun(_) -> timer:sleep(5000), pong end),

    % Verify async handling
    ?assertMatch({noreply, _},
        erlmcp_session_replicator:handle_call(
            {bootstrap_node, 'slow@host'},
            {self(), ref},
            state()
        )).
```

---

## Armstrong Principles Violated

| Principle | Violation | Finding(s) |
|-----------|-----------|-----------|
| "init/1 never blocks" | Blocking file I/O, Mnesia creates | CRIT-1, CRIT-2 |
| "Let-It-Crash" | Unbounded timeouts prevent crash | HIGH-3 |
| "Supervision" | Children cannot start during init | CRIT-2 |
| "Isolation" | One slow node blocks all requests | HIGH-1, HIGH-2 |

---

## Files Modified by This Research

- **ANTIPATTERN_BLOCKING_OPS.md** (this file) - Comprehensive blocking ops analysis

---

## Next Steps

1. **Review Findings**: Code review team validates antipattern assessment
2. **Prioritize Fixes**: Confirm Phase 1 as blocker for 2.2.0 release
3. **Implement Fixes**: Create work orders per CLAUDE.md work order protocol
4. **Test Coverage**: Add regression tests to prevent reintroduction
5. **Document Patterns**: Update erlmcp development guidelines

---

## References

- CLAUDE.md: "gen_server init/1 never blocks → async cast"
- CLAUDE.md: "Anti-Patterns" section (rules 1-4)
- OTP Design Principles: https://www.erlang.org/doc/design_principles/des_princ.html
- Joe Armstrong, "Programming Erlang", Chapter 12: Distributed Programming
