# Consensus-Based Tool Coordination POC

## Overview

This POC demonstrates the **pattern** for consensus-based tool coordination that would be used with ra/raft at production scale. It uses Erlang's built-in global registry to simulate the leader election and coordination mechanisms.

## What It Demonstrates

### 1. Leader Election
- Uses `global:register_name/2` for leader election
- First process to register wins leadership
- Automatic re-election on leader failure
- Followers monitor the leader for availability

### 2. Exactly-Once Tool Execution
- All tool executions happen **only** on the leader
- Followers forward requests to the leader
- No duplicate executions even with multiple nodes
- Audit log maintained for all executions

### 3. Request Forwarding
- Followers transparently forward to leader
- Client doesn't need to know who the leader is
- Automatic retry on leader failure
- Seamless failover experience

### 4. Leader Failover
- When leader dies, followers detect via monitor
- New election triggered automatically
- Service continues with minimal disruption
- No lost tool executions

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Global Registry                          │
│              (erlmcp_consensus_leader)                      │
└─────────────────────────────────────────────────────────────┘
                          │
          ┌───────────────┼───────────────┐
          │               │               │
    ┌─────▼─────┐   ┌─────▼─────┐   ┌─────▼─────┐
    │  Node 1   │   │  Node 2   │   │  Node 3   │
    │  LEADER   │   │ FOLLOWER  │   │ FOLLOWER  │
    ├───────────┤   ├───────────┤   ├───────────┤
    │ Executes  │   │ Forwards  │   │ Forwards  │
    │ Tools     │◄──┤ Requests  │◄──┤ Requests  │
    │           │   │           │   │           │
    │ Audit Log │   │ No Log    │   │ No Log    │
    └───────────┘   └───────────┘   └───────────┘
```

## Usage

### Running the Demo

```erlang
%% Start Erlang shell
rebar3 shell

%% Run the complete demonstration
erlmcp_consensus_poc:run_demo().
```

### Programmatic API

```erlang
%% Start a consensus node
{ok, Pid} = erlmcp_consensus_poc:start_link(node1).

%% Execute a tool (automatically routes to leader)
{ok, Result} = erlmcp_consensus_poc:execute_tool(Pid, <<"add">>, #{<<"a">> => 1, <<"b">> => 2}).
%% Result: 3

%% Get node status
Status = erlmcp_consensus_poc:get_status(Pid).
%% #{node_id => node1, role => leader, execution_count => 1, ...}

%% Stop a node
ok = erlmcp_consensus_poc:stop(Pid).
```

## Demo Flow

The `run_demo/0` function demonstrates the complete lifecycle:

### 1. Initial Setup
```
Starting 3 consensus nodes...
   [node1] Elected as LEADER
   [node2] Following leader: <0.123.0>
   [node3] Following leader: <0.123.0>
```

### 2. Tool Execution
```
Executing tools (all requests routed to leader):
   Node1 -> add(1, 2) = 3
   Node2 -> multiply(3, 4) = 12
   Node3 -> concat(hello, world) = <<"hello world">>
```

All three executions happen on the **leader only**, even though requests came from different nodes.

### 3. Audit Log
```
Audit logs (exactly-once semantics - all executions on leader):
   [node1] (leader) has 3 executions in audit log
   [node2] (follower) has empty audit log
   [node3] (follower) has empty audit log
```

### 4. Leader Failure
```
Simulating leader failure...
   Killing leader: node1 (executed 3 tools)
   [node2] Leader <0.123.0> died (shutdown), attempting re-election
   [node3] Leader <0.123.0> died (shutdown), attempting re-election
```

### 5. Re-election
```
Status after failover (new leader elected):
   [node2] Role: leader, Executions: 0
   [node3] Role: follower, Leader: <0.145.0>, Executions: 0
```

### 6. Continued Operation
```
Executing tools with new leader:
   Node -> divide(10, 2) = 5.0
   Node -> subtract(20, 5) = 15

Final audit logs (new leader has continued execution):
   [node2] (leader) has 2 executions in audit log
   [node3] (follower) has empty audit log
```

## Supported Tools

The POC includes simulated implementations of common tools:

| Tool | Arguments | Example |
|------|-----------|---------|
| `add` | `a`, `b` | `add(1, 2) → 3` |
| `multiply` | `x`, `y` | `multiply(3, 4) → 12` |
| `divide` | `a`, `b` | `divide(10, 2) → 5.0` |
| `subtract` | `a`, `b` | `subtract(20, 5) → 15` |
| `concat` | `s1`, `s2` | `concat("hello", "world") → "hello world"` |

## Testing

Run the EUnit test suite:

```bash
rebar3 eunit --module=erlmcp_consensus_poc_tests
```

Test coverage:
- ✅ Basic consensus (leader election)
- ✅ Tool execution routing
- ✅ Leader failover
- ✅ Exactly-once semantics

## Implementation Details

### State Record

```erlang
-record(state, {
    node_id :: atom(),                    % Unique node identifier
    role :: leader | follower,            % Current role
    leader_pid :: pid() | undefined,      % PID of current leader
    leader_monitor :: reference() | undefined,  % Monitor reference
    audit_log :: [term()],                % Execution history
    execution_count :: non_neg_integer()  % Total executions
}).
```

### Leader Election Algorithm

```erlang
1. On startup, attempt to register with global:register_name/2
2. If successful → become LEADER
3. If failed → become FOLLOWER
   a. Find current leader with global:whereis_name/1
   b. Monitor the leader process
   c. Forward all requests to leader
4. On leader death → goto step 1
```

### Request Handling

**Leader behavior:**
```erlang
handle_call({execute_tool, Tool, Args}, _From, #state{role = leader} = State) ->
    Result = execute_tool_local(Tool, Args),
    AuditEntry = create_audit_entry(Tool, Args, Result),
    NewState = State#state{
        audit_log = [AuditEntry | State#state.audit_log],
        execution_count = State#state.execution_count + 1
    },
    {reply, {ok, Result}, NewState}.
```

**Follower behavior:**
```erlang
handle_call({execute_tool, Tool, Args}, _From, #state{role = follower} = State) ->
    Result = gen_server:call(State#state.leader_pid, {execute_tool, Tool, Args}),
    {reply, Result, State}.
```

## Pattern Mapping to Production (ra/raft)

This POC demonstrates the **pattern**. In production with ra:

| POC Implementation | Production (ra/raft) |
|-------------------|---------------------|
| `global:register_name/2` | Raft consensus algorithm |
| Process monitoring | Raft heartbeat/elections |
| Single leader | Raft leader election |
| Audit log in state | Raft replicated log |
| Manual forwarding | Automatic raft routing |
| In-memory only | Durable log storage |

## Key Guarantees

### ✅ Exactly-Once Execution
Every tool execution happens on **exactly one node** (the leader), preventing duplicate side effects.

### ✅ Request Forwarding
Clients can send requests to **any node** and they will be routed to the leader automatically.

### ✅ Automatic Failover
When the leader fails, a new leader is **elected automatically** and service continues.

### ✅ Audit Trail
All executions are **logged with unique IDs**, timestamps, and results for compliance.

## Performance Characteristics

This POC is intentionally simple to demonstrate the pattern. Performance notes:

- **Single leader bottleneck**: All executions serialize through one process
- **In-memory only**: No persistence across node restarts
- **No log compaction**: Audit log grows unbounded
- **Simple election**: First-to-register wins (not true consensus)

In production with ra, these would be addressed:
- **Leader handles reads**: Can scale read-heavy workloads
- **Replicated log**: Durable across failures
- **Log snapshots**: Compact old entries
- **Raft consensus**: True distributed consensus with quorum

## Extending the POC

### Adding New Tools

```erlang
execute_tool_local(<<"my_tool">>, Args) ->
    %% Your tool implementation
    {ok, Result}.
```

### Adding Persistence

Replace in-memory audit log with:
- ETS table
- Mnesia database
- External log (file, database)

### Adding Replication

Extend to replicate audit log to followers:
- Broadcast executions to all nodes
- Maintain consistent logs
- Use for failover recovery

## Files

- **Implementation**: `apps/erlmcp_core/src/poc/erlmcp_consensus_poc.erl`
- **Tests**: `apps/erlmcp_core/test/erlmcp_consensus_poc_tests.erl`
- **Documentation**: `apps/erlmcp_core/src/poc/README_CONSENSUS_POC.md` (this file)

## References

- **ra**: https://github.com/rabbitmq/ra (Raft implementation for Erlang)
- **Raft**: https://raft.github.io/ (Consensus algorithm)
- **Global Registry**: https://www.erlang.org/doc/man/global.html (Erlang global name registration)

## License

Part of erlmcp - Erlang MCP SDK
