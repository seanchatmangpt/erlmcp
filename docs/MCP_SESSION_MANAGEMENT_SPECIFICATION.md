# MCP Session Management Specification

**Version**: 2.2.0
**Last Updated**: January 2026
**Audience**: MCP Implementers, erlmcp Developers, Protocol Architects

---

## Table of Contents

1. [Overview](#overview)
2. [Session Creation and Initialization](#1-session-creation-and-initialization)
3. [Session State Tracking](#2-session-state-tracking)
4. [Session Persistence Mechanisms](#3-session-persistence-mechanisms)
5. [Statelessness vs Statefulness](#4-statelessness-vs-statefulness)
6. [Session Cleanup and Termination](#5-session-cleanup-and-termination)
7. [Context and Request Correlation](#6-context-and-request-correlation)
8. [Multi-Session Handling](#7-multi-session-handling)
9. [Session Recovery and Failover](#8-session-recovery-and-failover)
10. [Best Practices](#best-practices)
11. [Architecture Reference](#architecture-reference)

---

## Overview

### What is an MCP Session?

An MCP session represents a logical connection between a client and server, encapsulating:
- **Connection state** - Transport and protocol negotiation
- **User context** - Authentication, authorization, metadata
- **Capability state** - Agreed-upon features between client and server
- **Request correlation** - Tracking request-response pairs across asynchronous communication
- **Subscription state** - Active resource subscriptions and handlers
- **Lifecycle** - Creation, use, and cleanup phases

### Session Scope

Sessions exist at multiple levels:

| Level | Scope | Lifetime | Example |
|-------|-------|----------|---------|
| **Protocol** | JSON-RPC initialization | Connection lifetime | MCP initialize handshake |
| **Transport** | TCP, HTTP, WebSocket, stdio | Connection lifetime | TCP socket, HTTP session |
| **Application** | Business logic state | Variable (minutes to hours) | User session with metadata |
| **Request** | Individual correlations | Request lifetime | Request-response pair |

### Key Design Principles

**Principle 1: Explicit Initialization**
- Sessions require explicit `initialize` handshake before operation
- Capabilities must be negotiated upfront
- Protocol version agreement is mandatory

**Principle 2: Request Correlation**
- All requests have unique IDs for correlation
- Responses, errors, and cancellations reference request IDs
- Enables asynchronous, out-of-order message delivery

**Principle 3: Flexible Durability**
- Sessions may be stateless (ephemeral) or stateful (durable)
- Implementation choice depends on use case
- Multiple persistence backends supported

**Principle 4: Graceful Degradation**
- Expired sessions should terminate cleanly
- Missing sessions return clear error codes
- No implicit session resurrection

---

## 1. Session Creation and Initialization

### 1.1 Initialization Protocol

The MCP initialize handshake establishes a session:

```erlang
%% Client sends initialize request
InitRequest = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,                    % Request correlation ID
    <<"method">> => <<"initialize">>,
    <<"params">> => #{
        <<"protocolVersion">> => <<"2025-11-25">>,  % MCP protocol version
        <<"capabilities">> => #{
            <<"roots">> => #{<<"enabled">> => true},
            <<"sampling">> => #{<<"enabled">> => true}
        },
        <<"clientInfo">> => #{
            <<"name">> => <<"claude">>,
            <<"version">> => <<"1.0.0">>
        }
    }
}.

%% Server responds with session establishment
InitResponse = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,                    % Echoes request ID
    <<"result">> => #{
        <<"protocolVersion">> => <<"2025-11-25">>,  % Confirmed version
        <<"capabilities">> => #{
            <<"resources">> => #{<<"enabled">> => true},
            <<"tools">> => #{<<"enabled">> => true},
            <<"prompts">> => #{<<"enabled">> => true},
            <<"logging">> => #{<<"enabled">> => true}
        },
        <<"serverInfo">> => #{
            <<"name">> => <<"erlmcp_server">>,
            <<"version">> => <<"2.2.0">>
        }
    }
}.

%% Session is now established and ready for use
```

### 1.2 Session Initialization in erlmcp

```erlang
%% Start a client and initialize a session
{ok, ClientPid} = erlmcp_client:start_link({stdio, []}),

%% Create client capabilities
ClientCaps = #mcp_client_capabilities{
    roots = #mcp_capability{enabled = true},
    sampling = #mcp_capability{enabled = true}
},

%% Initialize session
{ok, ServerCaps} = erlmcp_client:initialize(ClientPid, ClientCaps),

%% Client is now in 'initialized' phase - can send protocol methods
ok = erlmcp_client:list_tools(ClientPid).
```

### 1.3 Session Data Structure

```erlang
%% Session record with all metadata
-record(session, {
    id :: binary(),                    % Unique session identifier
    created_at :: integer(),           % Creation timestamp (milliseconds)
    last_accessed :: integer(),        % Last access timestamp
    timeout_ms :: pos_integer() | infinity,  % TTL in milliseconds
    metadata => map(),                 % User-defined metadata

    %% Connection state
    transport_module :: module(),      % Transport implementation
    transport_state :: term(),         % Transport-specific state
    phase :: pre_initialization | initializing | initialized | error | closed,

    %% Capability state
    client_capabilities :: #mcp_client_capabilities{} | undefined,
    server_capabilities :: #mcp_server_capabilities{} | undefined,

    %% Request state
    request_counter :: pos_integer(),  % Next request ID
    pending_requests :: #{request_id() => request_metadata()},

    %% Subscription state
    subscriptions :: sets:set(binary()),  % Active resource URIs
    notification_handlers :: map()    % Handler PIDs for notifications
}).

%% Minimal session metadata stored in backend
-record(persistent_session, {
    session_id :: binary(),
    session :: map(),
    created_at :: integer(),
    last_accessed :: integer(),
    ttl :: integer()                   % Time-to-live in milliseconds
}).
```

### 1.4 Initialization States

Client sessions pass through well-defined states:

```
pre_initialization
    ↓
    erlmcp_client:initialize(ClientPid, ClientCaps)
    ↓
initializing (waiting for server response)
    ↓
    Server responds with capabilities
    ↓
initialized (ready for protocol methods)
    ↓
    Protocol methods can be called
    ↓
error (protocol violation or timeout)
    ↓
closed (cleanup or explicit stop)
```

**State Transitions:**
- `pre_initialization` → `initializing`: On `initialize/2` call
- `initializing` → `initialized`: On successful response with capabilities
- `initializing` → `error`: On timeout, protocol error, or server rejection
- `initialized` → `error`: On protocol violation or transport failure
- Any state → `closed`: On `stop/1` or process termination

### 1.5 Session ID Generation

```erlang
%% Cryptographically secure session ID generation
-spec generate_session_id() -> binary().
generate_session_id() ->
    %% 128 bits of random data = 16 bytes = 32 hex characters
    Rand = crypto:strong_rand_bytes(16),
    binary:encode_hex(Rand).  % <<"a1b2c3d4e5f6g7h8i9j0k1l2m3n4o5p6">>

%% Example: <<"3f7a2e8b1c9d6f4a5e2b8c1d9f3a6e4c">>
```

---

## 2. Session State Tracking

### 2.1 State Model

Sessions track three types of state:

**1. Connection State** (transport and protocol)
```erlang
#{
    transport_module => erlmcp_transport_stdio,
    transport_state => #{fd_in => 0, fd_out => 1},
    phase => initialized,
    last_activity => 1640995300000  % Milliseconds since epoch
}
```

**2. Capability State** (negotiated features)
```erlang
#{
    client_capabilities => #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = true}
    },
    server_capabilities => #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true},
        logging = #mcp_capability{enabled = true}
    }
}
```

**3. Request State** (pending correlations)
```erlang
#{
    pending_requests => #{
        1 => {tools_list, from_pid},        % Waiting for tools/list response
        2 => {resources_read, from_pid},    % Waiting for resources/read response
        3 => {tool_call, from_pid}          % Waiting for tools/call response
    }
}
```

### 2.2 State Updates

Session state is updated through:

1. **Synchronous Updates** (immediate)
   - `update_session(SessionId, UpdateFun)`
   - Updates state in-process with serialization guarantee
   - No intermediate states visible to other processes

2. **Asynchronous Events** (eventual consistency)
   - `handle_info(Message, State)` for incoming data
   - Touch session on access: `touch_session(SessionId)`
   - Batch updates for performance

3. **Background Operations**
   - Replication to failover nodes (erlmcp_session_replicator)
   - Persistence to durable storage (via backend)
   - Cleanup of expired entries

### 2.3 State Consistency

**Serialization**:
- gen_server processes serialize all state updates
- No concurrent mutations to same session
- Multiple sessions can be updated in parallel

**Atomicity**:
- Session updates are atomic at the Erlang message level
- Partial updates return errors with original state unchanged

**Visibility**:
- Updates visible to next caller immediately
- Replication visible to other nodes within 20-100ms (backend dependent)

```erlang
%% Thread-safe state update pattern
-spec update_session(session_id(), fun((session()) -> session())) -> ok | {error, term()}.
update_session(SessionId, UpdateFun) ->
    gen_server:call(erlmcp_session_manager, {update_session, SessionId, UpdateFun}).

%% In handle_call
handle_call({update_session, SessionId, UpdateFun}, _From, State) ->
    case ets:lookup(State#state.table, SessionId) of
        [{SessionData, SessionId}] ->
            try UpdateFun(SessionData) of
                UpdatedData ->
                    Now = erlang:system_time(millisecond),
                    UpdatedDataWithAccess = UpdatedData#{last_accessed => Now},
                    %% Atomic insert (overwrites previous)
                    ets:insert(State#state.table, {UpdatedDataWithAccess, SessionId}),
                    notify_replicator({session_updated, SessionId, UpdatedDataWithAccess}),
                    {reply, ok, State}
            catch
                _:Reason ->
                    {reply, {error, {update_failed, Reason}}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end.
```

### 2.4 Metadata Storage

Sessions support arbitrary metadata for application use:

```erlang
%% Create session with metadata
{ok, SessionId} = erlmcp_session:create(#{
    user_id => <<"user_123">>,
    client_info => #{
        name => <<"claude">>,
        version => <<"2025-01">>,
        ip_address => <<"192.168.1.1">>
    },
    request_count => 0,
    last_tool => undefined
}),

%% Update metadata
{ok, SessionData} = erlmcp_session:retrieve(SessionId),
{ok, Session} = erlmcp_session:update(SessionId, fun(S) ->
    Metadata = maps:get(metadata, S),
    S#{metadata => Metadata#{
        request_count => maps:get(request_count, Metadata, 0) + 1,
        last_tool => <<"weather_tool">>
    }}
end).
```

---

## 3. Session Persistence Mechanisms

### 3.1 Persistence Architecture

erlmcp provides three pluggable session persistence backends:

```
erlmcp_session_manager (gen_server)
    ↓
erlmcp_session_backend (behavior interface)
    ↓
    ├── erlmcp_session_ets    (in-memory, fastest)
    ├── erlmcp_session_dets   (disk-based, persistent)
    └── erlmcp_session_mnesia (distributed, clustered)
```

### 3.2 ETS Backend (In-Memory, Default)

**Use Case**: Development, high-performance, non-critical sessions

```erlang
%% Configuration
{erlmcp_session, [
    {backend, erlmcp_session_ets},
    {backend_opts, #{
        table_name => erlmcp_sessions_ets,
        cleanup_interval => 60000  % 60 seconds
    }}
]}.

%% Performance Characteristics
%% - Lookup: ~1-5 microseconds
%% - Insert: ~1-5 microseconds
%% - Durability: LOST on process restart
%% - Capacity: Limited by available RAM
%% - Concurrency: Excellent (lock-free reads)
```

**Implementation**:
```erlang
-module(erlmcp_session_ets).
-behaviour(erlmcp_session_backend).

%% ETS table structure: {session_data, session_id}
init(Opts) ->
    TableName = maps:get(table_name, Opts, erlmcp_sessions_ets),
    Table = ets:new(TableName, [
        ordered_set,
        public,
        named_table,
        {read_concurrency, true},  % Lock-free reads
        {write_concurrency, true}   % Better write contention
    ]),
    {ok, #state{table = Table}}.

store(SessionId, Session, State) ->
    true = ets:insert(State#state.table, {Session, SessionId}),
    {ok, State}.

fetch(SessionId, State) ->
    case ets:lookup(State#state.table, SessionId) of
        [{Session, SessionId}] -> {ok, Session, State};
        [] -> {error, not_found, State}
    end.

delete(SessionId, State) ->
    ets:delete(State#state.table, SessionId),
    {ok, State}.
```

### 3.3 DETS Backend (Disk-Based)

**Use Case**: Single-node production, session persistence through restarts

```erlang
%% Configuration
{erlmcp_session, [
    {backend, erlmcp_session_dets},
    {backend_opts, #{
        table_name => erlmcp_sessions_dets,
        file_path => "data/erlmcp_sessions.dets",
        auto_save => 60000,        % Flush to disk every 60 seconds
        cleanup_interval => 60000
    }}
]}.

%% Performance Characteristics
%% - Lookup: ~100-500 microseconds
%% - Insert: ~1-5 milliseconds (with auto_save)
%% - Durability: SURVIVES process/system restart
%% - Capacity: ~2 GB per file
%% - Concurrency: Good (file-level locks)
```

**Migration from ETS**:
```erlang
%% Step 1: Backup ETS sessions
{ok, BackendPid} = erlmcp_session_backend:start_link(#{
    backend => erlmcp_session_ets
}),
{ok, SessionIds} = erlmcp_session_backend:list(),

%% Step 2: Update configuration to DETS
{erlmcp_session, [
    {backend, erlmcp_session_dets},
    {backend_opts, #{file_path => "data/erlmcp_sessions.dets"}}
]}.

%% Step 3: Restart application
application:stop(erlmcp_core),
application:start(erlmcp_core).

%% Sessions now persist to disk
```

### 3.4 Mnesia Backend (Distributed, Clustered)

**Use Case**: Multi-node clusters, high availability, automatic failover

```erlang
%% Configuration
{erlmcp_session, [
    {backend, erlmcp_session_mnesia},
    {backend_opts, #{
        table_name => erlmcp_session,
        nodes => [node1@host, node2@host, node3@host],
        disc_copies => true,  % Disk+RAM copies (durable), false = RAM only (fast)
        cleanup_interval => 60000
    }}
]}.

%% Performance Characteristics
%% - Lookup (RAM): ~50-200 microseconds
%% - Lookup (disk): ~1-5 milliseconds
%% - Insert: ~1-10 milliseconds (transactional)
%% - Durability: CONFIGURABLE (ram_copies vs disc_copies)
%% - Capacity: Unlimited (sharded across nodes)
%% - Concurrency: Excellent (sharding by session ID)
```

**Setup**:
```erlang
%% On all nodes in cluster
mnesia:create_schema([node1@host, node2@host, node3@host]),
mnesia:start(),

%% Create distributed table (runs on each node)
{atomic, ok} = mnesia:create_table(erlmcp_session, [
    {disc_copies, [node1@host, node2@host, node3@host]},
    {attributes, record_info(fields, session)},
    {type, set}
]),

%% Wait for table to load
mnesia:wait_for_tables([erlmcp_session], 5000).
```

**Automatic Failover**:
```erlang
%% If node1@host fails:
%% - Sessions still available on node2@host and node3@host
%% - Reads/writes continue transparently
%% - When node1@host recovers:
%%   * Mnesia syncs table from other nodes
%%   * Sessions available again without replication

%% No explicit failover needed - automatic via Mnesia
```

### 3.5 Custom Backend Implementation

You can implement custom backends (e.g., Redis, LevelDB, PostgreSQL):

```erlang
-module(erlmcp_session_redis).
-behaviour(erlmcp_session_backend).

-export([init/1, store/3, fetch/2, delete/2, list/1, cleanup_expired/1]).

init(Opts) ->
    Host = maps:get(host, Opts, "localhost"),
    Port = maps:get(port, Opts, 6379),
    {ok, ConnPid} = redis:start_link(#{host => Host, port => Port}),
    {ok, #state{redis = ConnPid}}.

store(SessionId, Session, State) ->
    TTL = maps:get(timeout_ms, Session, 3600000),
    TTLSeconds = TTL div 1000,
    Value = term_to_binary(Session),
    case redis:set_ex(State#state.redis, SessionId, TTLSeconds, Value) of
        ok -> {ok, State};
        Error -> {error, Error, State}
    end.

fetch(SessionId, State) ->
    case redis:get(State#state.redis, SessionId) of
        {ok, Value} ->
            Session = binary_to_term(Value),
            {ok, Session, State};
        {error, not_found} ->
            {error, not_found, State};
        Error ->
            {error, Error, State}
    end.

delete(SessionId, State) ->
    case redis:del(State#state.redis, SessionId) of
        ok -> {ok, State};
        Error -> {error, Error, State}
    end.

list(State) ->
    %% Get all session keys
    case redis:keys(State#state.redis, "session_*") of
        {ok, Keys} -> {ok, Keys, State};
        Error -> {error, Error, State}
    end.

cleanup_expired(State) ->
    %% Redis handles TTL automatically
    %% Return 0 since Redis manages expiration
    {ok, 0, State}.
```

### 3.6 Persistence Comparison

| Aspect | ETS | DETS | Mnesia | Redis |
|--------|-----|------|--------|-------|
| **Speed** | ⚡ 1-5 µs | 🚀 100-500 µs | ✓ 50-200 µs | ✓ 1-10 ms |
| **Durability** | ❌ None | ✅ File | ✅ Configurable | ✅ File/Memory |
| **Cluster** | ❌ No | ❌ No | ✅ Yes (3+ nodes) | ✅ Yes |
| **Capacity** | RAM limit | 2 GB | Unlimited | RAM limit |
| **Implementation** | Erlang/OTP | Erlang/OTP | Erlang/OTP | External |
| **Use Case** | Dev/Testing | Single-node Prod | Multi-node Prod | High-scale Prod |

---

## 4. Statelessness vs Statefulness

### 4.1 Stateless Sessions

**Philosophy**: Minimal server state, all context in client

```erlang
%% Stateless session - no server-side state
Client sends:
#{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,
    <<"method">> => <<"tools/call">>,
    <<"params">> => #{
        <<"name">> => <<"weather">>,
        <<"arguments">> => #{<<"city">> => <<"San Francisco">>},
        %% All context self-contained - no session lookup needed
        <<"session_context">> => #{
            <<"auth_token">> => <<"jwt:...">>,
            <<"user_id">> => <<"user_123">>,
            <<"capabilities">> => [...]
        }
    }
}.

Server processes without session lookup:
- Validate auth token
- Extract user ID from token
- Execute tool with user context
- No session store needed
```

**Pros**:
- ✅ Horizontal scalability (stateless servers)
- ✅ No session store overhead
- ✅ Stateless = reproducible (same input → same output)
- ✅ No cleanup required

**Cons**:
- ❌ Context size increases with each request
- ❌ Must validate auth on every request
- ❌ Cannot track in-flight requests
- ❌ Loss of request history

### 4.2 Stateful Sessions

**Philosophy**: Maintain server-side session state for efficiency and features

```erlang
%% Stateful session - state maintained on server
Client sends initialization:
#{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,
    <<"method">> => <<"initialize">>,
    <<"params">> => #{...}
}

Server responds with session ID:
#{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,
    <<"result">> => #{
        <<"session_id">> => <<"abc123...">>,  % Server maintains state for this ID
        ...
    }
}

Client sends authenticated request:
#{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 2,
    <<"session_id">> => <<"abc123...">>,    % References server-side session
    <<"method">> => <<"tools/call">>,
    <<"params">> => #{
        <<"name">> => <<"weather">>,
        <<"arguments">> => #{<<"city">> => <<"San Francisco">>}
    }
}

Server process:
1. Look up session <<"abc123...">> in store
2. Validate auth from session state
3. Extract user context from session state
4. Execute tool
5. Update session state (request count, last activity, etc.)
```

**Pros**:
- ✅ Reduced payload size (no repeated context)
- ✅ Efficient auth validation (cached in session)
- ✅ Request history and correlation tracking
- ✅ Subscription state management
- ✅ Per-session rate limiting

**Cons**:
- ❌ Server memory overhead (session store)
- ❌ Not horizontally scalable (unless replicated)
- ❌ Session cleanup required
- ❌ Failure handling more complex

### 4.3 Hybrid Approach (Recommended for MCP)

Most MCP implementations use **hybrid** approach:

**Stateful Components**:
- Connection/transport state (TCP socket, HTTP session cookie)
- Capability negotiation (negotiated features)
- Request correlation (pending requests)
- Subscription tracking (active resource subscriptions)

**Stateless Components**:
- Authentication (JWT token in request)
- Authorization (derive from token, not session)
- Tool arguments (complete in request)

```erlang
%% Hybrid pattern in erlmcp
Session = #{
    %% Stateful: Maintained on server
    id => SessionId,
    created_at => Now,
    last_accessed => Now,
    capabilities => Caps,  % Negotiated upfront
    subscriptions => Sets,  % Active subscriptions

    %% Stateless: Derived from request
    %% Authorization checked via token, not session
    %% Tool arguments self-contained
},

%% Per-request:
%% - Session lookup for capabilities + subscriptions
%% - Auth validation via JWT in request header
%% - Tool execution from request args (no session dependencies)
```

### 4.4 Decision Matrix

Use **stateless** when:
- [ ] Horizontal scalability is critical
- [ ] Authentication is JWT/OAuth based
- [ ] No subscription state needed
- [ ] No request history tracking needed

Use **stateful** when:
- [ ] Subscriptions or state tracking needed
- [ ] Request correlation required
- [ ] Per-connection configuration needed
- [ ] Session affinity is acceptable

Use **hybrid** when:
- [ ] Balancing scalability and features
- [ ] Want best of both worlds
- [ ] Can replicate session state (Mnesia, Redis)

---

## 5. Session Cleanup and Termination

### 5.1 Session Lifecycle

```
Creation → Active → Idle → Expired → Deleted
           ↓       ↓               ↓
        "Touch"  "Access"      "Cleanup"
         (reset timeout)    (background process)
```

### 5.2 Expiration Mechanism

Sessions expire based on **inactivity timeout**:

```erlang
%% Create session with 30-minute TTL
{ok, SessionId} = erlmcp_session:create(#{user_id => <<"user_123">>}, 1800000),

%% Session expires if not accessed for 1800000 milliseconds (30 minutes)
is_expired(Session, Now) ->
    case maps:get(timeout_ms, Session) of
        infinity -> false;  % Never expires
        TimeoutMs ->
            LastAccessed = maps:get(last_accessed, Session),
            (Now - LastAccessed) > TimeoutMs  % Expired if idle > timeout
    end.
```

**Key Points**:
- **Inactivity-based**: Timer resets on access
- **Not absolute**: Session doesn't expire if continuously accessed
- **Configurable**: Per-session TTL at creation
- **Optional**: TTL can be `infinity` (never expires)

### 5.3 Automatic Cleanup

Background process cleans expired sessions at regular intervals:

```erlang
%% Default cleanup every 60 seconds
-define(DEFAULT_CLEANUP_INTERVAL, 60000).

%% In session_manager init
CleanupTimer = erlang:send_after(?DEFAULT_CLEANUP_INTERVAL, self(), cleanup_expired),

%% In handle_info
handle_info(cleanup_expired, State) ->
    Count = do_cleanup_expired(State),
    logger:debug("Cleaned up ~p expired sessions", [Count]),
    NewTimer = erlang:send_after(State#state.cleanup_interval_ms, self(), cleanup_expired),
    {noreply, State#state{cleanup_timer = NewTimer}}.

%% Implementation
do_cleanup_expired(State) ->
    Now = erlang:system_time(millisecond),

    %% Find all expired sessions
    ExpiredSessions = ets:foldl(
        fun({SessionData, SessionId}, Acc) ->
            case is_expired(SessionData, Now) of
                true -> [SessionId | Acc];
                false -> Acc
            end
        end,
        [],
        State#state.table
    ),

    %% Delete expired sessions
    lists:foreach(
        fun(SessionId) ->
            ets:delete(State#state.table, SessionId),
            notify_replicator({session_expired, SessionId})  % For failover
        end,
        ExpiredSessions
    ),

    length(ExpiredSessions).
```

### 5.4 Manual Session Termination

Sessions can be explicitly terminated:

```erlang
%% Client-side termination
ok = erlmcp_client:stop(ClientPid),

%% Server-side termination
ok = erlmcp_session:delete(SessionId),

%% Batch termination
Sessions = erlmcp_session:list_sessions(),
lists:foreach(fun(#{id := Id}) -> erlmcp_session:delete(Id) end, Sessions).
```

### 5.5 Cleanup Strategy

**Recommended approach** (tradeoff between memory and CPU):

| Scenario | Cleanup Interval | TTL | Rationale |
|----------|-----------------|-----|-----------|
| **Development** | 5 min | 5 min | Rapid cleanup |
| **Single-node prod** | 1 min | 30 min | Balance cleanup cost |
| **Multi-node cluster** | 2 min | 1 hour | Reduced cleanup frequency |
| **High-session volume** | 30 sec | 5 min | Aggressive cleanup |
| **Long-running sessions** | 10 min | 8 hours | Infrequent cleanup |

### 5.6 Graceful Shutdown

When stopping the session manager:

```erlang
%% In terminate/2
terminate(_Reason, State) ->
    %% Cancel cleanup timer
    _ = case State#state.cleanup_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,

    %% Optional: Persist sessions to durable storage before shutdown
    % {ok, Sessions} = list_sessions(),
    % lists:foreach(fun(S) -> persist_to_dets(S) end, Sessions),

    %% Cleanup ETS table
    ets:delete(State#state.table),
    ok.
```

---

## 6. Context and Request Correlation

### 6.1 Request-Response Correlation

MCP uses **request IDs** to correlate requests with responses:

```erlang
%% Client sends request
Request = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 42,                % Correlation ID
    <<"method">> => <<"tools/call">>,
    <<"params">> => #{...}
}.

%% Server receives request, processes asynchronously
%% Returns response with same ID

Response = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 42,                % ECHOES request ID
    <<"result">> => #{...}
}.

Error = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 42,                % ECHOES request ID
    <<"error">> => #{...}
}.
```

### 6.2 Pending Request Tracking

Client tracks pending requests while waiting for responses:

```erlang
%% In erlmcp_client state
-record(state, {
    %% Map of request_id => {method, from_pid}
    pending_requests = #{} :: #{request_id() => {atom(), pid()}},
    request_id = 1 :: request_id(),
    ...
}).

%% Sending request
send_request(Method, Params, State) ->
    RequestId = State#state.request_id,

    %% Store pending request
    Pending = maps:put(RequestId, {Method, self()}, State#state.pending_requests),
    NewState = State#state{
        pending_requests = Pending,
        request_id = RequestId + 1
    },

    %% Send request
    Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => erlang_to_binary(Method),
        <<"params">> => Params
    },
    send_transport(Request, NewState),

    {RequestId, NewState}.

%% Receiving response
handle_response(#{<<"id">> := RequestId, <<"result">> := Result}, State) ->
    case maps:take(RequestId, State#state.pending_requests) of
        {{Method, CallerPid}, RemainingPending} ->
            %% Notify caller
            CallerPid ! {response, RequestId, Result},

            %% Update state
            NewState = State#state{pending_requests = RemainingPending},
            {noreply, NewState};
        error ->
            %% Unexpected response (already timed out?)
            logger:warning("Response for unknown request: ~p", [RequestId]),
            {noreply, State}
    end.
```

### 6.3 Request Timeout Handling

Requests can timeout if no response received:

```erlang
%% Send request with timeout
{ok, RequestId} = erlmcp_client:call_tool(Client, ToolName, Args),

%% In implementation (with simplified pattern)
call_tool(Client, ToolName, Args) ->
    gen_server:call(Client, {call_tool, ToolName, Args}, 5000).  % 5s timeout

%% If timeout:
%% - RequestId still in pending_requests
%% - Response arrives later (orphaned)
%% - Periodic cleanup needed

%% Background cleanup
cleanup_stale_correlations() ->
    %% Find requests pending for >2x timeout
    %% Remove from pending_requests
    %% Log warning
    ok.
```

### 6.4 Request Context Structure

Complete context for a request:

```erlang
%% Context at request time
RequestContext = #{
    %% Transport info
    transport => tcp,
    remote_ip => {192, 168, 1, 100},
    remote_port => 54321,

    %% Session info
    session_id => <<"abc123...">>,
    user_id => <<"user_456">>,
    client_name => <<"claude">>,

    %% Request info
    request_id => 42,
    method => <<"tools/call">>,
    started_at => 1640995300000,
    timeout_ms => 5000,

    %% Authentication
    auth_token => <<"jwt:...">>,
    permissions => [<<"read">>, <<"write">>, <<"execute">>],

    %% Tracing
    trace_id => <<"trace:xyz789">>,
    span_id => <<"span:abc123">>,
    parent_span_id => undefined
}.

%% Store in request correlation table
RequestId = 42,
ets:insert(request_correlations, {RequestId, RequestContext}),

%% Later: retrieve context for logging/tracing
{RequestId, RequestContext} = ets:lookup_element(request_correlations, 42, 2).
```

### 6.5 Batch Request Correlation

Multiple requests in one batch:

```erlang
%% Send batch
Batch = [
    {<<"tools/list">>, 1},
    {<<"resources/list">>, 2},
    {<<"prompts/list">>, 3}
],

%% Track batch
{ok, BatchId} = erlmcp_client:with_batch(Client, fun(Batch) ->
    %% All requests in batch share BatchId
    %% Ordered list: [{RequestId, ResponseId}, ...]
    ok
end).

%% Implementation
with_batch(Client, BatchFun) ->
    BatchId = erlang:make_ref(),

    %% Create batch context
    Batch = #{
        batch_id => BatchId,
        request_ids => [],
        results => []
    },

    %% Execute batch function
    %% Returns results correlated by BatchId
    {ok, Results} = BatchFun(Batch),
    {ok, Results}.
```

---

## 7. Multi-Session Handling

### 7.1 Session Multiplexing

Server handles multiple simultaneous sessions:

```erlang
%% Architecture
erlmcp_session_manager (one per server)
    ├── Session 1 (Client A)
    │   ├── request 1
    │   ├── request 2
    │   └── subscriptions
    │
    ├── Session 2 (Client B)
    │   ├── request 1
    │   └── subscriptions
    │
    └── Session 3 (Client C)
        ├── request 1
        ├── request 2
        ├── request 3
        └── subscriptions
```

**Implementation**:
```erlang
%% Session storage - map of SessionId → SessionData
-record(state, {
    %% ETS table: {session_data, session_id}
    table :: ets:tid()
}).

%% Concurrent access patterns:
%% 1. create_session/3 → new SessionId
%% 2. get_session/1 → lookup SessionId
%% 3. update_session/2 → atomic update
%% 4. list_sessions/0 → all sessions
%% 5. delete_session/1 → remove SessionId

%% Each operation is serialized via gen_server:call
%% Multiple sessions updated in parallel via multiple gen_server processes
```

### 7.2 Session Isolation

Sessions are logically isolated:

```erlang
%% Session A's state
SessionA = #{
    id => <<"session_a">>,
    user_id => <<"user_1">>,
    subscriptions => [<<"weather://nyc">>, <<"news://tech">>],
    pending_requests => #{1 => {tools_call, pid_a}}
}.

%% Session B's state (independent)
SessionB = #{
    id => <<"session_b">>,
    user_id => <<"user_2">>,
    subscriptions => [<<"weather://sf">>, <<"sports://nba">>],
    pending_requests => #{1 => {tools_call, pid_b}, 2 => {resources_read, pid_b}}
}.

%% Isolation guarantees:
%% ✅ No cross-session data leakage
%% ✅ No shared subscriptions
%% ✅ No shared pending requests
%% ✅ Independent timeouts
%% ✅ Independent cleanup

%% Sharing:
%% ✅ Shared resource handlers (same tool implementation)
%% ✅ Shared database connections
%% ✅ Shared authentication backend
```

### 7.3 Per-Session Rate Limiting

Each session can have independent rate limits:

```erlang
%% Session with rate limiting
Session = #{
    id => SessionId,
    metadata => #{
        rate_limit => #{
            requests_per_second => 10,
            burst_size => 50,
            window_size_ms => 1000
        }
    }
}.

%% Rate limiter check
handle_request(Method, Params, SessionId, State) ->
    {ok, Session} = get_session(SessionId, State),
    RateLimit = maps:get(rate_limit, maps:get(metadata, Session), #{}),

    case check_rate_limit(SessionId, RateLimit) of
        {ok, _Tokens} ->
            %% Process request
            process_request(Method, Params);
        {error, rate_limited} ->
            %% Return error with retry_after
            {error, {rate_limited, #{<<"retry_after">> => 1000}}}
    end.
```

### 7.4 Resource Subscription Management

Multiple sessions can subscribe to same resource:

```erlang
%% Resource subscribers tracking
ResourceSubscribers = #{
    <<"weather://nyc">> => [
        {session_a, pid_a},
        {session_b, pid_b},
        {session_c, pid_c}
    ],
    <<"weather://sf">> => [
        {session_b, pid_b}
    ]
}.

%% When resource updates
handle_resource_update(<<"weather://nyc">>, UpdateData) ->
    case maps:get(<<"weather://nyc">>, ResourceSubscribers) of
        Subscribers ->
            %% Send notification to all sessions
            lists:foreach(fun({SessionId, SubcriberPid}) ->
                Notification = #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"method">> => <<"resources/updated">>,
                    <<"params">> => #{
                        <<"uri">> => <<"weather://nyc">>,
                        <<"metadata">> => UpdateData
                    }
                },
                send_notification(SubscriberPid, Notification)
            end, Subscribers);
        undefined ->
            %% No subscribers
            ok
    end.

%% Unsubscribe from resource
handle_unsubscribe(SessionId, ResourceUri, State) ->
    case maps:get(ResourceUri, State#state.subscriptions) of
        Subscribers ->
            NewSubscribers = lists:delete(SessionId, Subscribers),
            UpdatedSubscriptions = maps:put(
                ResourceUri,
                NewSubscribers,
                State#state.subscriptions
            ),
            {reply, ok, State#state{subscriptions = UpdatedSubscriptions}};
        undefined ->
            {reply, {error, not_subscribed}, State}
    end.
```

### 7.5 Multi-Session Metrics

Aggregate metrics across sessions:

```erlang
%% Metrics per session
Session1Metrics = #{
    request_count => 1250,
    tool_calls => 450,
    resource_reads => 800,
    avg_latency_ms => 25,
    errors => 2
}.

Session2Metrics = #{
    request_count => 850,
    tool_calls => 350,
    resource_reads => 500,
    avg_latency_ms => 30,
    errors => 0
}.

%% Aggregate metrics
AggregateMetrics = #{
    total_sessions => 2,
    total_requests => 2100,
    total_tool_calls => 800,
    total_resource_reads => 1300,
    avg_latency_ms => 27,
    total_errors => 2,
    requests_per_session => 1050,
    active_sessions => 2
}.

%% Report
report_session_metrics() ->
    Sessions = erlmcp_session_manager:list_sessions(),
    Metrics = [get_session_metrics(S) || S <- Sessions],
    AggregateMetrics = aggregate_metrics(Metrics),
    logger:info("Session metrics: ~p", [AggregateMetrics]).
```

---

## 8. Session Recovery and Failover

### 8.1 Node Failure Detection

In clustered deployments, detect node failures:

```erlang
%% Node monitoring in erlmcp_session_failover
-behaviour(gen_server).

start_link(ClusterNodes) ->
    net_kernel:monitor_nodes(true),  % Monitor node up/down events
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ClusterNodes], []).

%% In handle_info
handle_info({nodedown, Node}, State) ->
    logger:error("Node down detected: ~p", [Node]),

    %% Trigger failover for sessions on failed node
    trigger_failover_for_node(Node, State),

    %% Update node status
    NewNodeStatus = maps:put(Node, down, State#state.node_status),
    {noreply, State#state{node_status = NewNodeStatus}};

handle_info({nodeup, Node}, State) ->
    logger:info("Node recovered: ~p", [Node]),

    %% Sync sessions from backup nodes
    sync_sessions_from_backups(Node, State),

    %% Update node status
    NewNodeStatus = maps:put(Node, up, State#state.node_status),
    {noreply, State#state{node_status = NewNodeStatus}}.
```

### 8.2 Session Replication

Sessions replicated to backup nodes for failover:

```erlang
%% Replication architecture
Primary Node (node1)
    ├── Session A ────→ Backup: node2, node3
    ├── Session B ────→ Backup: node2, node3
    └── Session C ────→ Backup: node2, node3

Backup Node (node2)
    ├── Session A (replica)  ← From node1
    ├── Session D (replica)  ← From node3
    └── Session E (replica)  ← From node1

Backup Node (node3)
    ├── Session A (replica)  ← From node1
    ├── Session B (replica)  ← From node1
    └── Session C (replica)  ← From node1

%% Replication happens asynchronously:
%% 1. Session created/updated on primary
%% 2. Event sent to erlmcp_session_replicator
%% 3. Replicated to backup nodes with 20ms batching
%% 4. Backups acknowledge receipt
```

**Implementation**:
```erlang
%% In session_manager handle_call
handle_call({create_session, Metadata, TimeoutMs, Options}, _From, State) ->
    SessionId = generate_session_id(),
    Now = erlang:system_time(millisecond),

    SessionData = #{
        id => SessionId,
        created_at => Now,
        last_accessed => Now,
        timeout_ms => TimeoutMs,
        metadata => Metadata
    },

    %% Insert locally
    ets:insert(State#state.table, {SessionData, SessionId}),

    %% Replicate asynchronously to backup nodes
    notify_replicator({session_created, SessionId, SessionData}),

    {reply, {ok, SessionId}, State}.

%% In session_replicator
handle_info({session_event, {session_created, SessionId, SessionData}}, State) ->
    %% Batch replication: accumulate in batch buffer
    NewBatch = [
        {session_created, SessionId, SessionData} |
        State#state.replication_batch
    ],

    %% If batch full or timer expired, flush
    case should_flush_batch(NewBatch) of
        true ->
            flush_replication_batch(NewBatch, State);
        false ->
            {noreply, State#state{replication_batch = NewBatch}}
    end.

%% Flush to backup nodes
flush_replication_batch(Events, State) ->
    BackupNodes = State#state.backup_nodes,
    lists:foreach(fun(Node) ->
        %% Async RPC to backup node
        rpc:async_call(Node, erlmcp_session_replicator, store_batch, [Events])
    end, BackupNodes),

    {noreply, State#state{replication_batch = []}}.
```

### 8.3 Failover Promotion

When primary node fails, promote backup node:

```erlang
%% Detect primary failure
handle_info({nodedown, PrimaryNode}, State) ->
    case State#state.local_node of
        PrimaryNode ->
            %% This node is the primary and it's failing
            %% Impossible - this process is on the failing node
            ok;
        _ ->
            %% Primary failed, promote self if backup
            case is_backup_for(PrimaryNode, State) of
                true ->
                    logger:info("Promoting to primary (primary ~p failed)", [PrimaryNode]),
                    promote_to_primary(State);
                false ->
                    ok
            end
    end.

%% Promotion logic
promote_to_primary(State) ->
    %% 1. Flush any pending replications from this node
    flush_pending_replications(State),

    %% 2. Stop replicating to failed primary
    StopReplicating = lists:delete(
        State#state.primary_node,
        State#state.backup_nodes
    ),

    %% 3. Become primary for sessions
    NewState = State#state{
        primary_node => node(),
        backup_nodes => StopReplicating
    },

    logger:info("Promoted to primary. Backup nodes: ~p", [StopReplicating]),
    {noreply, NewState}.
```

### 8.4 Session Failover

When a session's primary node fails:

```erlang
%% Before failover
Session = #{
    id => <<"session_123">>,
    primary_node => node1,        % Failed
    backup_nodes => [node2, node3],
    version => 5,                 % Replication version
    data => #{...}
}.

%% Failover process
%% 1. node1 detected as down
%% 2. Session still exists on node2 and node3
%% 3. Elect new primary (node2 wins via majority voting)
%% 4. node2 becomes primary
%% 5. Clients redirect to node2 for this session
%% 6. Replication continues to node3

%% New state after failover
NewSession = #{
    id => <<"session_123">>,
    primary_node => node2,        % Promoted
    backup_nodes => [node3],      % Removed failed node1
    version => 5,                 % Same version (no loss)
    data => #{...}                % Same data (replicated)
}.
```

### 8.5 Recovery from Backup

When primary recovers:

```erlang
%% node1 recovers
handle_info({nodeup, FailedNode}, State) ->
    logger:info("Node recovered: ~p", [FailedNode]),

    %% 1. Check if this is the failed primary
    case FailedNode == State#state.primary_node of
        true ->
            %% Wait for consensus on failback
            prompt_failback_decision(FailedNode, State);
        false ->
            %% Just another node, add to available nodes
            AddToBackups = [FailedNode | State#state.backup_nodes],
            {noreply, State#state{backup_nodes => AddToBackups}}
    end.

%% Automatic failback (after delay)
prompt_failback_decision(RecoveredNode, State) ->
    %% Option 1: Wait for admin decision
    %% Option 2: Auto-failback if recovered node is original primary
    %% Option 3: Keep current primary, add recovered as backup

    %% Usually: Add recovered node as backup first
    %% Let operator promote back manually if desired

    AddToBackups = [RecoveredNode | State#state.backup_nodes],
    logger:info("Added ~p back as backup node", [RecoveredNode]),
    {noreply, State#state{backup_nodes => AddToBackups}}.
```

### 8.6 Split-Brain Prevention

Prevent split-brain (two nodes thinking they're primary):

```erlang
%% Majority voting
promote_to_primary(State) ->
    %% Check if we have majority
    AllNodes = [node() | State#state.backup_nodes],
    ClusterSize = length(AllNodes),
    RequiredMajority = (ClusterSize div 2) + 1,

    %% Count reachable nodes
    ReachableNodes = lists:filter(fun(N) ->
        net_kernel:connect(N) =/= false
    end, AllNodes),

    case length(ReachableNodes) >= RequiredMajority of
        true ->
            %% We have majority - safe to promote
            logger:info("Majority obtained (~p/~p nodes reachable). Promoting to primary.",
                       [length(ReachableNodes), ClusterSize]),
            NewState = State#state{primary_node => node()},
            {noreply, NewState};
        false ->
            %% No majority - cannot promote (prevent split-brain)
            logger:error("No majority! (~p/~p nodes reachable). Refusing to promote.",
                        [length(ReachableNodes), ClusterSize]),
            {noreply, State}
    end.
```

### 8.7 Version Tracking for Consistency

Track version numbers to detect conflicts:

```erlang
%% Session with version
Session = #{
    id => SessionId,
    version => 10,                 % Incremented on each update
    data => #{...},
    primary_node => node1,
    backup_replicas => #{
        node2 => 10,               % node2 has version 10
        node3 => 9                 % node3 has version 9 (not synced yet)
    }
}.

%% Update increments version
update_session(SessionId, UpdateFun, State) ->
    {ok, Session} = get_session(SessionId, State),

    %% Get current version
    OldVersion = maps:get(version, Session, 0),
    NewVersion = OldVersion + 1,

    %% Apply update
    UpdatedSession = UpdateFun(Session#{version => NewVersion}),

    %% Store and replicate
    store_session(UpdatedSession, State),
    notify_replicator({session_updated, SessionId, UpdatedSession}),

    {reply, ok, State}.

%% Conflict resolution: keep version with highest number
resolve_conflict(Session1, Session2) ->
    V1 = maps:get(version, Session1, 0),
    V2 = maps:get(version, Session2, 0),

    case V1 > V2 of
        true -> Session1;
        false -> Session2
    end.
```

---

## Best Practices

### Session Creation
- ✅ Always initialize before use (explicit initialization)
- ✅ Use strong random IDs (crypto:strong_rand_bytes/1)
- ✅ Store metadata for context (user_id, client_info)
- ✅ Set appropriate TTL for use case
- ❌ Don't rely on session IDs being sequential
- ❌ Don't expose internal session structure to clients

### Session Management
- ✅ Touch sessions on access (update last_accessed)
- ✅ Use TTL for automatic cleanup
- ✅ Monitor session count and memory
- ✅ Clean up stale correlations periodically
- ❌ Don't store sensitive data in sessions (store in secure vault)
- ❌ Don't assume sessions survive restarts (unless using durable backend)

### Persistence
- ✅ Use ETS for development
- ✅ Use DETS or Mnesia for production
- ✅ Test migration paths before deploying
- ✅ Monitor backend performance
- ✅ Back up persistent data regularly
- ❌ Don't switch backends without testing
- ❌ Don't assume DETS file isn't corrupted (use repair flag)

### Failover
- ✅ Replicate sessions to backup nodes
- ✅ Detect failures within 5 seconds
- ✅ Use majority voting to prevent split-brain
- ✅ Test failover scenarios regularly
- ✅ Monitor replication lag
- ❌ Don't assume automatic failback (manual control is safer)
- ❌ Don't promote without majority

### Security
- ✅ Validate auth on every request (don't just trust session)
- ✅ Rotate session IDs periodically
- ✅ Encrypt session data at rest (if sensitive)
- ✅ Use HTTPS for session transmission
- ✅ Clear sessions on logout
- ❌ Don't store passwords in sessions
- ❌ Don't trust client-provided session IDs

---

## Architecture Reference

### Supervision Tree

```
erlmcp_sup (one_for_all)
├── erlmcp_core_sup
│   ├── erlmcp_session_manager
│   │   └── ETS table: erlmcp_sessions
│   │
│   ├── erlmcp_session_replicator (if clustering)
│   │   └── Batches replication to backup nodes
│   │
│   └── erlmcp_session_failover (if clustering)
│       └── Monitors node health, triggers failover
│
├── erlmcp_registry
│   └── Maps servers ↔ transports (gproc)
│
├── erlmcp_server_sup (simple_one_for_one)
│   └── erlmcp_server instances (one per transport)
│
└── erlmcp_observability_sup
    ├── erlmcp_metrics_server
    ├── erlmcp_dashboard_server
    └── erlmcp_tracing
```

### Request Flow with Sessions

```
Client                          Transport               Session Manager              Application
  │                                 │                           │                           │
  ├──initialize──────────────────→  │                           │                           │
  │                                 ├──init req────────────────→│                           │
  │                                 │                           ├─create session────────→ ETS
  │                                 │                           ├←──session_id──────────←
  │                                 │←───init response──────────┤                           │
  │←──initialize response────────────┤                           │                           │
  │                                 │                           │                           │
  ├──tools/call─────────────────→   │                           │                           │
  │ (id=1)                          ├──tools/call──────────────→│                           │
  │                                 │ (id=1)                    ├──get_session────────────→
  │                                 │                           │←──session────────────────
  │                                 │                           │  (incl. capabilities)
  │                                 │                           ├──touch session───────→ ETS
  │                                 │                           │  (update last_accessed)
  │                                 │                           ├──route to handler──────→│
  │                                 │                           │                         ├─execute
  │                                 │                           │                         │
  │                                 │←──tool result─────────────┤←─handler response──────┤
  │←──tools/call response────────────┤                           │                           │
  │ (id=1)                          │                           │                           │
```

### Module Relationships

```
erlmcp_session (public API)
  ├─→ erlmcp_session_manager (session lifecycle)
  │   └─→ erlmcp_session_backend (behavior interface)
  │       ├─→ erlmcp_session_ets
  │       ├─→ erlmcp_session_dets
  │       └─→ erlmcp_session_mnesia
  │
  ├─→ erlmcp_session_replicator (clustering)
  │   └─→ erlmcp_session_failover
  │
  └─→ erlmcp_client (protocol client)
      └─→ erlmcp_registry (message routing)
```

---

## Configuration Examples

### Development Configuration

```erlang
%% sys.config for development
{erlmcp_core, [
    {session_backend, ets},
    {session_timeout_ms, 300000},      % 5 minutes
    {cleanup_interval_ms, 60000},      % 1 minute
    {cluster_nodes, []}                % Single node
]}.
```

### Single-Node Production

```erlang
%% sys.config for single-node production
{erlmcp_core, [
    {session_backend, dets},
    {session_backend_opts, #{
        table_name => erlmcp_sessions_dets,
        file_path => "/var/lib/erlmcp/sessions.dets",
        auto_save => 120000              % 2 minutes
    }},
    {session_timeout_ms, 3600000},      % 1 hour
    {cleanup_interval_ms, 120000},      % 2 minutes
    {cluster_nodes, []}
]}.
```

### Multi-Node Cluster

```erlang
%% sys.config for multi-node cluster
{erlmcp_core, [
    {session_backend, mnesia},
    {session_backend_opts, #{
        table_name => erlmcp_session,
        nodes => ['node1@host1', 'node2@host2', 'node3@host3'],
        disc_copies => true
    }},
    {session_timeout_ms, 3600000},      % 1 hour
    {cleanup_interval_ms, 120000},
    {cluster_nodes, ['node1@host1', 'node2@host2', 'node3@host3']},
    {replication_batch_timeout_ms, 20},
    {failover_detection_interval_ms, 5000}
]}.
```

---

## References

- **MCP Specification**: https://modelcontextprotocol.io/
- **Erlang Sessions**: `docs/SESSION_PERSISTENCE.md`, `docs/SESSION_REPLICATION_SYSTEM.md`
- **Implementation**: `apps/erlmcp_core/src/erlmcp_session*.erl`
- **Tests**: `apps/erlmcp_core/test/erlmcp_session_*_SUITE.ct`

---

**Version History**

| Version | Date | Changes |
|---------|------|---------|
| 2.2.0 | Jan 2026 | Comprehensive specification with 8 key areas |
| 2.1.0 | Dec 2025 | Added session replication and failover |
| 2.0.0 | Nov 2025 | Refactored session manager with ETS backend |
| 1.0.0 | Sep 2025 | Initial session persistence implementation |
