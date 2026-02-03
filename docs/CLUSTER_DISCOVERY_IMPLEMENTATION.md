# Cluster Service Discovery Implementation

## Overview

This document describes the gproc-based service discovery system implemented for the erlmcp v3 cluster. The implementation provides dynamic service registration, discovery, load balancing, and health monitoring across distributed Erlang nodes.

## Architecture

### Design Pattern

The cluster discovery uses **gproc (Global Process Registry)** as the foundation:

```
┌─────────────────────────────────────────────────────┐
│     erlmcp_cluster_discovery (gen_server)           │
│  ┌──────────────────────────────────────────────┐   │
│  │  Service Registry (gproc)                    │   │
│  │  ┌────────────────────────────────────────┐  │   │
│  │  │ {p, l, {service, Name, Id}} -> Pid     │  │   │
│  │  │ {p, g, {service, Name, Id}} -> Pid     │  │   │
│  │  └────────────────────────────────────────┘  │   │
│  └──────────────────────────────────────────────┘   │
│                                                     │
│  ┌──────────────────────────────────────────────┐   │
│  │  Process Monitoring                          │   │
│  │  ┌────────────────────────────────────────┐  │   │
│  │  │ monitor(process, ServicePid)           │  │   │
│  │  │ gproc:monitor(Key)                     │  │   │
│  │  └────────────────────────────────────────┘  │   │
│  └──────────────────────────────────────────────┘   │
│                                                     │
│  ┌──────────────────────────────────────────────┐   │
│  │  Health Status Cache                         │   │
│  │  #{ServiceId => healthy | unhealthy}        │   │
│  └──────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────┘
```

### Supervision

```
erlmcp_sup (one_for_all)
└── erlmcp_cluster_discovery (gen_server, permanent)
```

## Key Features

### 1. Service Registration

```erlang
%% Local service registration
{ok, ServiceId} = erlmcp_cluster_discovery:register_service(
    local,
    my_service,
    ServicePid,
    my_type,
    #{version => <<"1.0.0">>, region => <<"us-east">>}
).

%% Global service registration (cluster-wide)
{ok, ServiceId} = erlmcp_cluster_discovery:register_service(
    global,
    my_service,
    ServicePid,
    my_type,
    #{capabilities => [http, grpc]}
).
```

**Registration includes:**
- Unique service ID generation
- Service metadata (version, region, tags)
- Capability tracking
- Automatic process monitoring
- Heartbeat timestamp

### 2. Service Discovery

```erlang
%% Discover single service (first available)
{ok, ServiceInfo} = erlmcp_cluster_discovery:discover_service(
    my_service,
    local
).

%% Discover all instances
{ok, Services} = erlmcp_cluster_discovery:discover_services(
    my_service,
    local
).

%% Discover with filters
{ok, Services} = erlmcp_cluster_discovery:discover_services(
    my_service,
    local,
    [{type, my_type}, {version, <<"1.0.0">>}]
).
```

**Discovery filters:**
- `{type, Type}` - Filter by service type
- `{version, Version}` - Filter by version
- `{capability, Capability}` - Filter by capability
- `{metadata, Metadata}` - Filter by metadata fields
- `{health, Status}` - Filter by health status
- `{node, Node}` - Filter by node
- `{local, true}` - Only local services
- `{global, true}` - Cluster-wide services

### 3. Load Balancing Strategies

```erlang
%% Round-robin (default for sequential requests)
{ok, Service} = erlmcp_cluster_discovery:discover_with_strategy(
    my_service,
    local,
    round_robin
).

%% Random selection
{ok, Service} = erlmcp_cluster_discovery:discover_with_strategy(
    my_service,
    local,
    random
).

%% Local-first (prefer same node, then random)
{ok, Service} = erlmcp_cluster_discovery:discover_with_strategy(
    my_service,
    local,
    local_first
).
```

### 4. Health Monitoring

```erlang
%% Mark service as healthy/unhealthy
ok = erlmcp_cluster_discovery:mark_service_healthy(
    my_service,
    ServiceId
).

ok = erlmcp_cluster_discovery:mark_service_unhealthy(
    my_service,
    ServiceId
).

%% Check service health
{ok, healthy} = erlmcp_cluster_discovery:service_health_check(
    my_service,
    ServiceId
).

%% Force health check on all services
ok = erlmcp_cluster_discovery:force_health_check().
```

**Automatic cleanup:**
- Process death automatically unregisters service
- gproc monitors detect failures
- No orphaned registrations

### 5. Cluster Awareness

```erlang
%% Get all cluster nodes
{ok, Nodes} = erlmcp_cluster_discovery:get_cluster_nodes().

%% Get all cluster services
{ok, Services} = erlmcp_cluster_discovery:get_cluster_services().

%% Get statistics for a service
{ok, Stats} = erlmcp_cluster_discovery:get_service_stats(my_service).

%% Get all discovery statistics
{ok, AllStats} = erlmcp_cluster_discovery:get_all_stats().
```

### 6. Event Notifications

```erlang
%% Subscribe to service discovery events
ok = erlmcp_cluster_discovery:subscribe_to_events(SubscriberPid).

%% Unsubscribe
ok = erlmcp_cluster_discovery:unsubscribe_from_events(SubscriberPid).

%% Receive events:
%%   {discovery_event, {service_registered, Name, Id, Info}}
%%   {discovery_event, {service_unregistered, Name, Id}}
%%   {discovery_event, {service_healthy, Name, Id}}
%%   {discovery_event, {service_unhealthy, Name, Id}}
%%   {discovery_event, {node_up, Node}}
%%   {discovery_event, {node_down, Node}}
```

## gproc Key Structure

### Local Services
```erlang
{p, l, {service, ServiceName, ServiceId}}
```

### Global Services
```erlang
{p, g, {service, ServiceName, ServiceId}}
```

### Service ID Format
```erlang
<<"ServiceName_Node_UniqueInteger">>
```

Example: `<<"my_service_node@127.0.0.1_12345">>`

## API Reference

### Registration API

| Function | Description |
|----------|-------------|
| `register_service/3` | Register service locally |
| `register_service/4` | Register with scope |
| `register_service/5` | Register with full spec |
| `unregister_service/2` | Unregister service |
| `update_service/3` | Update service metadata |
| `renew_service/2` | Renew heartbeat |

### Discovery API

| Function | Description |
|----------|-------------|
| `discover_service/2` | Find single service |
| `discover_service/3` | Find with filters |
| `discover_services/2` | Find all instances |
| `discover_services/3` | Find all with filters |
| `discover_all_services/1` | List all services |
| `discover_by_type/2` | Find by type |
| `discover_local/2` | Local services only |
| `discover_global/2` | Cluster-wide services |
| `discover_healthy/2` | Only healthy services |
| `discover_by_capability/3` | Find by capability |
| `discover_by_version/3` | Find by version |
| `discover_by_metadata/3` | Find by metadata |
| `discover_with_fallback/3` | Try alternative service |

### Load Balancing API

| Function | Description |
|----------|-------------|
| `discover_with_strategy/3` | Load-balanced discovery |
| `get_service_count/2` | Count service instances |
| `service_health_check/2` | Check service health |

### Cluster Info API

| Function | Description |
|----------|-------------|
| `get_cluster_nodes/0` | List cluster nodes |
| `get_cluster_services/0` | List all services |
| `get_service_stats/1` | Get service statistics |
| `get_all_stats/0` | Get discovery stats |
| `subscribe_to_events/1` | Subscribe to events |
| `unsubscribe_from_events/1` | Unsubscribe from events |

### Health API

| Function | Description |
|----------|-------------|
| `health_check/1` | Check discovery service |
| `force_health_check/1` | Force check all services |
| `mark_service_healthy/2` | Mark as healthy |
| `mark_service_unhealthy/2` | Mark as unhealthy |
| `service_is_healthy/2` | Check health status |

## Performance Characteristics

### Time Complexity

| Operation | Complexity | Notes |
|-----------|------------|-------|
| Register | O(1) | gproc insert + monitor |
| Unregister | O(1) | gproc delete + demonitor |
| Discover | O(log N) | gproc select |
| Discover all | O(N) | Full scan |
| Health check | O(1) | Cached status |

### Memory Footprint

- **Per service**: ~500 bytes (metadata + monitor)
- **State**: ~1KB base + health cache
- **gproc overhead**: ~200 bytes per key

### Scalability

- **Tested**: 100+ concurrent services
- **Limit**: gproc can handle 100K+ processes
- **Cluster**: Global scope for multi-node

## Use Cases

### 1. RPC Service Discovery

```erlang
%% Register RPC service
{ok, ServiceId} = erlmcp_cluster_discovery:register_service(
    local,
    user_service,
    ServicePid,
    rpc,
    #{version => <<"2.0.0">>, protocol => json_rpc}
).

%% Client discovers service
{ok, ServiceInfo} = erlmcp_cluster_discovery:discover_service(
    user_service,
    local
).

ServicePid = maps:get(pid, ServiceInfo),
gen_server:call(ServicePid, {get_user, UserId}).
```

### 2. Load Balanced Pool

```erlang
%% Register multiple workers
lists:foreach(fun(Index) ->
    {ok, Pid} = worker_sup:start_worker(Index),
    erlmcp_cluster_discovery:register_service(
        local,
        worker_pool,
        Pid,
        worker,
        #{index => Index}
    )
end, lists:seq(1, 10)).

%% Round-robin dispatch
{ok, Worker} = erlmcp_cluster_discovery:discover_with_strategy(
    worker_pool,
    local,
    round_robin
),
gen_server:cast(maps:get(pid, Worker), Work).
```

### 3. Multi-Region Deployment

```erlang
%% Register services in different regions
{ok, _} = erlmcp_cluster_discovery:register_service(
    global,
    api_service,
    EastPid,
    api,
    #{region => <<"us-east">>}
).

{ok, _} = erlmcp_cluster_discovery:register_service(
    global,
    api_service,
    WestPid,
    api,
    #{region => <<"us-west">>}
).

%% Discover local region first
{ok, Services} = erlmcp_cluster_discovery:discover_services(
    api_service,
    global,
    [{metadata, #{region => <<"us-east">>}}]
).
```

### 4. Service Health Monitoring

```erlang
%% Subscribe to health events
erlmcp_cluster_discovery:subscribe_to_events(self()),

loop() ->
    receive
        {discovery_event, {service_unhealthy, Name, Id}} ->
            logger:error("Service ~p (~p) is unhealthy!", [Name, Id]),
            %% Trigger recovery or alerting
            alert_service_failure(Name, Id),
            loop();
        {discovery_event, {node_down, Node}} ->
            logger:warning("Node ~p went down", [Node]),
            %% Handle node failure
            handle_node_failure(Node),
            loop()
    end.
```

## Testing

### Unit Tests

```bash
# Run cluster discovery tests
docker compose run --rm erlmcp-ct \
    rebar3 ct --suite=apps/erlmcp_core/test/erlmcp_cluster_discovery_tests
```

### Test Coverage

- **Basic registration**: 10 tests
- **Discovery**: 5 tests
- **Load balancing**: 4 tests
- **Health monitoring**: 3 tests
- **Cluster info**: 3 tests
- **Event subscriptions**: 1 test
- **Error handling**: 5 tests
- **Advanced discovery**: 4 tests
- **Integration tests**: 2 tests

### Manual Testing

```erlang
%% Start discovery service
{ok, Pid} = erlmcp_cluster_discovery:start_link().

%% Register a test service
TestPid = spawn(fun() -> receive die -> ok end end),
{ok, ServiceId} = erlmcp_cluster_discovery:register_service(
    local, test_service, TestPid, test_type, #{}
).

%% Discover the service
{ok, ServiceInfo} = erlmcp_cluster_discovery:discover_service(
    test_service, local
).

%% Verify
?assertEqual(TestPid, maps:get(pid, ServiceInfo)),
?assertEqual(test_type, maps:get(type, ServiceInfo)),
?assertEqual(ServiceId, maps:get(id, ServiceInfo)).

%% Cleanup
TestPid ! die.
```

## Integration with Cluster Coordinator

The cluster discovery integrates with `erlmcp_cluster_coordinator` for:

1. **Leadership Awareness**: Discover coordinator leader
2. **Membership Events**: React to node joins/leaves
3. **Partition Handling**: Adjust discovery during network splits
4. **Failover**: Automatic failover to healthy nodes

```erlang
%% Combine discovery with coordinator
{ok, Leader} = erlmcp_cluster_coordinator:get_leader(Coordinator),

%% Discover services on leader node
{ok, Services} = erlmcp_cluster_discovery:discover_services(
    my_service,
    global,
    [{node, Leader}]
).
```

## Future Enhancements

1. **Service Version Migration**: Automatic version migration
2. **Distributed Health Checks**: Multi-node health verification
3. **Service Mesh Integration**: Link with proxy-based mesh
4. **DNS Integration**: DNS-based service discovery
5. **Service Discovery Metrics**: OTEL metrics for discovery
6. **Dashboard**: Web UI for service discovery
7. **CLI Commands**: Command-line discovery tools
8. **Transport Integration**: Auto-discovery for HTTP/WebSocket

## Troubleshooting

### Service Not Found

```erlang
%% Check if service is registered
{ok, AllServices} = erlmcp_cluster_discovery:discover_all_services(local),

%% Check service count
{ok, Count} = erlmcp_cluster_discovery:get_service_count(my_service, local),
```

### Process Death Not Detected

```erlang
%% Verify gproc monitoring
gproc:info().

%% Force health check
erlmcp_cluster_discovery:force_health_check().
```

### High Memory Usage

```erlang
%% Get stats
{ok, Stats} = erlmcp_cluster_discovery:get_all_stats(),

%% Check total services
TotalServices = maps:get(total_services, Stats),
```

## References

- [gproc Documentation](https://github.com/uwiger/gproc)
- [Erlang Distributed Programming](https://erlang.org/doc/reference_manual/distributed.html)
- [Service Discovery Patterns](https://microservices.io/patterns/microservices/service-registry.html)
