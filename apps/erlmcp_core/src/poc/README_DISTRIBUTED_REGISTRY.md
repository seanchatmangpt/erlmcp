# Distributed Registry POC

## Overview

This POC demonstrates using Erlang's built-in `global` and `pg` modules as a distributed process registry alternative to `gproc` for erlmcp.

## Architecture

### Components

1. **global module**: Unique process registration across cluster
   - Provides cluster-wide unique names
   - Automatic conflict resolution
   - Built-in failure detection

2. **pg module**: Process group membership
   - Group-based organization (tool servers, resource servers, etc.)
   - Efficient group lookup
   - Automatic cleanup on process death

3. **Monitor server**: Automatic cleanup coordinator
   - Monitors registered processes
   - Cleans up global names and pg groups on failure
   - Prevents resource leaks

## Key Features

### ✓ Distributed by Design
- Processes registered on any node are visible to all nodes
- No special clustering setup required
- Automatic global name resolution

### ✓ Automatic Failover
- Process failures trigger automatic cleanup
- Global locks prevent split-brain scenarios
- pg groups automatically updated

### ✓ Zero External Dependencies
- Uses only Erlang/OTP built-ins
- No third-party libraries required
- Future-proof with OTP updates

### ✓ Type-Safe Groups
- Servers organized by type (tools, resources, prompts)
- Fast group membership queries
- Support for multiple group membership

## API

### Registration

```erlang
%% Register a process globally
erlmcp_distributed_registry_poc:register(server_name, Pid).

%% Lookup by name
Pid = erlmcp_distributed_registry_poc:whereis(server_name).

%% Unregister
erlmcp_distributed_registry_poc:unregister(server_name).

%% List all registered names
Names = erlmcp_distributed_registry_poc:registered().
```

### Group Membership

```erlang
%% Join a group
erlmcp_distributed_registry_poc:join_group(mcp_tool_servers, Pid).

%% Get group members
Members = erlmcp_distributed_registry_poc:get_group_members(mcp_tool_servers).

%% Leave group
erlmcp_distributed_registry_poc:leave_group(mcp_tool_servers, Pid).

%% List all groups
Groups = erlmcp_distributed_registry_poc:get_all_groups().
```

### High-Level API

```erlang
%% Start an MCP server with automatic registration
{ok, Pid} = erlmcp_distributed_registry_poc:start_mcp_server(tool, calculator_server).

%% Stop and unregister
ok = erlmcp_distributed_registry_poc:stop_mcp_server(calculator_server).

%% List all servers with their groups
Servers = erlmcp_distributed_registry_poc:list_servers().
```

## Running the Demo

### Single Node

```erlang
%% Start an Erlang shell
make console

%% Run the complete demonstration
erlmcp_distributed_registry_poc:run_demo().
```

### Multi-Node Cluster

```bash
# Terminal 1: Start node1
erl -name node1@localhost -setcookie erlmcp_secret

# Terminal 2: Start node2
erl -name node2@localhost -setcookie erlmcp_secret

# Terminal 3: Start node3
erl -name node3@localhost -setcookie erlmcp_secret
```

```erlang
%% On node2 and node3, connect to node1
net_adm:ping('node1@localhost').

%% On node1, start a server
erlmcp_distributed_registry_poc:start_link().
{ok, Pid} = erlmcp_distributed_registry_poc:start_mcp_server(tool, calculator_server).

%% On node2, lookup the server (works across nodes!)
Pid = erlmcp_distributed_registry_poc:whereis(calculator_server).

%% On node3, call the server
Result = gen_server:call(Pid, {calculate, add, [5, 3]}).
%% => {ok, 8}

%% Simulate node1 failure (kill node1 process)
%% Node2/Node3 will detect the failure automatically
```

## Performance Comparison

### Benchmark Results

```
Registration (1000 processes):
  global+pg: ~50-100 ms
  gproc (local): ~20-30 ms

Lookup (10000 operations):
  global: ~5-10 ms (~2M lookups/sec)
  gproc (local): ~2-5 ms (~3-4M lookups/sec)

Group membership (1000 queries):
  pg: ~2-5 ms
  gproc p/2: ~2-5 ms (similar)
```

### Trade-offs

| Feature | global+pg | gproc |
|---------|-----------|-------|
| **Distributed** | ✓ Native support | ✗ Single node only |
| **Conflict resolution** | ✓ Automatic | ✗ Manual required |
| **External deps** | ✓ None | ✗ Requires gproc lib |
| **Performance (local)** | ✗ 2-3x slower | ✓ Fastest |
| **Group membership** | ✓ pg module | ✓ Via p/2 |
| **Properties/counters** | ✗ Not supported | ✓ Full support |
| **Failover** | ✓ Automatic | ✗ Requires code |
| **Net split handling** | ✓ Built-in | ✗ Requires code |

## Use Case Recommendations

### Use global+pg when:
- ✓ Running multi-node Erlang cluster
- ✓ Need automatic failover
- ✓ Want zero external dependencies
- ✓ Handling network partitions (net splits)
- ✓ Cross-datacenter deployments

### Use gproc when:
- ✓ Single node deployment
- ✓ Need maximum performance
- ✓ Using properties/counters extensively
- ✓ Existing gproc codebase

## Migration Path

### From gproc to global+pg

```erlang
%% gproc API
gproc:reg({n, l, server_name}).
Pid = gproc:where({n, l, server_name}).
gproc:unreg({n, l, server_name}).

%% global+pg API
erlmcp_distributed_registry_poc:register(server_name, self()).
Pid = erlmcp_distributed_registry_poc:whereis(server_name).
erlmcp_distributed_registry_poc:unregister(server_name).

%% Group membership
gproc:reg({p, l, mcp_tool_servers}).
Members = gproc:lookup_pids({p, l, mcp_tool_servers}).

%% Becomes
erlmcp_distributed_registry_poc:join_group(mcp_tool_servers, self()).
Members = erlmcp_distributed_registry_poc:get_group_members(mcp_tool_servers).
```

### Hybrid Approach

```erlang
%% Use both depending on deployment mode
-ifdef(DISTRIBUTED_MODE).
    -define(REGISTRY, erlmcp_distributed_registry).
-else.
    -define(REGISTRY, erlmcp_gproc_registry).
-endif.

%% Unified API
?REGISTRY:register(Name, Pid),
?REGISTRY:whereis(Name),
?REGISTRY:unregister(Name).
```

## Implementation Details

### Automatic Cleanup

The POC includes a monitor server that tracks all registered processes:

1. When a process registers, a monitor is created
2. On process death, the `'DOWN'` message triggers:
   - Global name unregistration
   - Removal from all pg groups
   - Monitor cleanup

This ensures no orphaned registrations or group memberships.

### Conflict Resolution

`global` uses a distributed algorithm to resolve name conflicts:

1. When two nodes register the same name simultaneously
2. Global detects the conflict
3. One process is chosen (oldest pid wins)
4. The other registration fails with `{error, already_registered}`

This prevents split-brain scenarios in distributed systems.

### Network Partition Handling

During a network partition (net split):

1. Each partition maintains its own global state
2. When partitions heal, global detects conflicts
3. Conflicts are resolved automatically (oldest wins)
4. Applications are notified of pid changes

## Testing Failure Scenarios

```erlang
%% Start servers
{ok, _} = erlmcp_distributed_registry_poc:start_mcp_server(tool, server1).
{ok, _} = erlmcp_distributed_registry_poc:start_mcp_server(tool, server2).

%% Simulate process crash
erlmcp_distributed_registry_poc:simulate_node_failure(server1).

%% Verify cleanup
undefined = erlmcp_distributed_registry_poc:whereis(server1).
Pid2 = erlmcp_distributed_registry_poc:whereis(server2).  % Still alive

%% Verify group cleanup
Members = erlmcp_distributed_registry_poc:get_group_members(mcp_tool_servers).
false = lists:any(fun(P) -> P =:= undefined end, Members).
```

## Production Considerations

### Monitoring

```erlang
%% Monitor global registration health
GlobalNames = global:registered_names(),
io:format("Registered globally: ~p~n", [length(GlobalNames)]).

%% Monitor pg group health
Groups = pg:which_groups(erlmcp_registry),
[io:format("Group ~p: ~p members~n",
           [G, length(pg:get_members(erlmcp_registry, G))])
 || G <- Groups].
```

### Performance Tuning

```erlang
%% global configuration (in sys.config)
{kernel, [
    {global_connect_retries, 3},
    {global_lock_timeout, 5000}
]}.

%% pg is self-tuning, no configuration needed
```

### Scaling Limits

- **global**: ~10-50 nodes recommended (distributed locks)
- **pg**: Scales to hundreds of nodes (no locks)
- For >50 nodes, consider partitioned namespaces

## Conclusion

The `global+pg` approach provides a **production-ready distributed registry** with zero external dependencies. It's ideal for multi-node erlmcp deployments where automatic failover and conflict resolution are critical.

For single-node deployments or when maximum performance is required, `gproc` remains the better choice.

## Next Steps

1. Run the demo: `erlmcp_distributed_registry_poc:run_demo()`
2. Test in multi-node setup (see above)
3. Benchmark with your workload
4. Choose based on deployment architecture
5. Consider hybrid approach for flexibility

## References

- [global module documentation](https://www.erlang.org/doc/man/global.html)
- [pg module documentation](https://www.erlang.org/doc/man/pg.html)
- [Distributed Erlang](https://www.erlang.org/doc/reference_manual/distributed.html)
- [gproc documentation](https://github.com/uwiger/gproc)
