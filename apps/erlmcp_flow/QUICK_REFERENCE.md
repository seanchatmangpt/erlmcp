# erlmcp-flow Quick Reference

**Version**: 1.0.0

---

## Message Routing

### Register Agent (gproc O(log N))
```erlang
erlmcp_flow_registry:register_agent(<<"agent-1">>, self(), [<<"erlang">>, <<"testing">>]).
```

### Discover Agents
```erlang
Agents = erlmcp_flow_registry:find_agents_by_capability(<<"erlang">>).
```

### Broadcast to Swarm
```erlang
erlmcp_flow_routing_examples:broadcast_to_swarm(<<"swarm-1">>, {task, <<"data">>}).
```

### Leader Election
```erlang
{Role, Leader} = erlmcp_flow_routing_examples:elect_leader(<<"group-1">>).
```

---

## Consensus Protocols

### Raft (Leader-Based, < 100ms)
```erlang
% Start cluster
[{ok, _} = erlmcp_flow_raft:start_link(#{node_id => Id, peers => Peers}) || ...],

% Submit command
{ok, Index} = erlmcp_flow_raft:submit_command(Leader, {assign_task, ...}).
```

### Byzantine (Quorum 2F+1)
```erlang
Nodes = [node1, ..., node7],  % N=7, F=2
SecretKey = crypto:strong_rand_bytes(32),
{quorum_reached, Value} = erlmcp_flow_routing_examples:byzantine_quorum_consensus(
    Nodes, Request, SecretKey).
```

### Gossip (Eventual Consistency)
```erlang
erlmcp_flow_routing_examples:gossip_disseminate(
    NodeId, {Key, Value}, Peers).
```

---

## Load Balancing

### Round-Robin
```erlang
{Agent, NewIdx} = erlmcp_flow_routing_examples:load_balance_round_robin(Agents, Idx).
```

### Least-Used-First
```erlang
Agent = erlmcp_flow_routing_examples:load_balance_least_used(Agents).
```

### Load-Aware (Comprehensive Metrics)
```erlang
Agent = erlmcp_flow_routing_examples:load_balance_load_aware(Agents).
```

---

## Failover

### Heartbeat Detection
```erlang
IsFailed = erlmcp_flow_routing_examples:detect_heartbeat_failure(AgentId, 5000).
```

### Task Requeue
```erlang
{ok, NewAgent} = erlmcp_flow_routing_examples:requeue_failed_task(TaskId, FailedAgent).
```

### Backup Promotion
```erlang
ok = erlmcp_flow_routing_examples:promote_backup_agent(PrimaryId, BackupId).
```

---

## Network Awareness

### Partition Detection
```erlang
case erlmcp_flow_routing_examples:detect_network_partition(ClusterNodes) of
    ok -> ok;
    {partitioned, Nodes} -> handle_partition(Nodes)
end.
```

### Partition Healing
```erlang
ok = erlmcp_flow_routing_examples:heal_partition(PartitionA, PartitionB).
```

---

## Performance Targets

| Metric | Target | Actual |
|--------|--------|--------|
| Agent lookup (p99) | < 100μs | TBD |
| Routing decision (p99) | < 50ms | TBD |
| Raft consensus (p99) | < 100ms | TBD |
| Byzantine consensus (p99) | < 500ms | TBD |
| Throughput (routing) | > 50K ops/s | TBD |
| Memory (1000 agents) | < 512MB | TBD |

---

## File Locations

- **Examples**: `/apps/erlmcp_flow/src/erlmcp_flow_routing_examples.erl`
- **Supervisor**: `/apps/erlmcp_flow/src/erlmcp_flow_sup.erl`
- **Registry**: `/apps/erlmcp_flow/src/erlmcp_flow_registry.erl`
- **Raft**: `/apps/erlmcp_flow/src/erlmcp_flow_raft.erl`
- **Guide**: `/apps/erlmcp_flow/ROUTING_PROTOCOLS_GUIDE.md`
- **Design**: `/docs/ERLMCP_FLOW_ROUTING_LAYER_DESIGN.md`

---

## Commands

```bash
# Compile
TERM=dumb rebar3 compile

# Test
rebar3 eunit --module=erlmcp_flow_routing_examples

# Benchmark
rebar3 as test do compile, eunit --module=erlmcp_flow_bench

# Full quality check
make check
```
