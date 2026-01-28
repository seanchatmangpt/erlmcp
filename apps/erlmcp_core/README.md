# erlmcp_core - Core MCP Protocol

**Version:** 2.0.0
**Application:** erlmcp_core
**Modules:** 14

Core implementation of the Model Context Protocol (MCP) in Erlang/OTP. Provides JSON-RPC 2.0, client/server gen_servers, gproc-based registry, and protocol primitives.

## Overview

erlmcp_core implements the foundational MCP protocol layer:
- **Client/Server** - OTP-compliant gen_server implementations with supervision
- **JSON-RPC 2.0** - Request/response encoding, notifications, error handling
- **Registry** - gproc-based process discovery and routing
- **Protocol Types** - Resources, tools, prompts, capabilities
- **Request Correlation** - ID-based request tracking and timeout management

## Core Modules (14 total)

### Client & Server
- **erlmcp_client.erl** - MCP client gen_server with request correlation
- **erlmcp_server.erl** - MCP server gen_server with resource/tool/prompt management
- **erlmcp_client_sup.erl** - Client supervisor (simple_one_for_one)
- **erlmcp_server_sup.erl** - Server supervisor (simple_one_for_one)

### Protocol Layer
- **erlmcp_json_rpc.erl** - JSON-RPC 2.0 encoding/decoding
- **erlmcp.hrl** - Protocol records and type definitions
- **erlmcp_capabilities.erl** - Capability negotiation and validation
- **erlmcp_types.erl** - MCP type definitions (resources, tools, prompts)
- **erlmcp_batch.erl** - Request batching and pipelining (2-5x throughput)

### Registry & Routing
- **erlmcp_registry.erl** - gproc-based process registration and discovery (local + global)
- **erlmcp_registry_dist.erl** - Distributed registry with gproc global mode
- **erlmcp_cluster_sup.erl** - Cluster management supervisor
- **erlmcp_node_monitor.erl** - Node connectivity monitoring
- **erlmcp_split_brain_detector.erl** - Network partition detection and resolution
- **erlmcp_router.erl** - Message routing between clients/servers/transports

### Application & Supervision
- **erlmcp_app.erl** - OTP application behavior
- **erlmcp_sup.erl** - Top-level supervisor (rest_for_one strategy)

### Schema Registry & Validation (NEW - Agent 12)
- **erlmcp_schema_registry.erl** - Centralized schema management with versioning
- **erlmcp_schema_validator.erl** - Pooled validation workers (jesse integration)

### Utilities
- **erlmcp_schema.erl** - JSON Schema validation (jesse integration)
- **erlmcp_utils.erl** - Common utilities (binary operations, validation)

## Dependencies

| Library | Version | Purpose |
|---------|---------|---------|
| **jsx** | 3.1.0 | JSON encoding/decoding |
| **jesse** | 1.8.1 | JSON Schema validation |
| **gproc** | 0.9.0 | Process registry with automatic monitoring |
| **poolboy** | 1.5.2 | Connection pooling for validation workers |

## Quick Start

### Client Example

```erlang
%% Connect via stdio
{ok, Client} = erlmcp_client:start_link({stdio, []}, #{strict_mode => false}),

%% Initialize connection
{ok, ServerCaps} = erlmcp_client:initialize(Client, #{
    <<"clientInfo">> => #{
        <<"name">> => <<"my-client">>,
        <<"version">> => <<"1.0.0">>
    }
}),

%% List resources
{ok, #{<<"resources">> := Resources}} = erlmcp_client:list_resources(Client),

%% Call a tool
{ok, Result} = erlmcp_client:call_tool(Client, <<"greet">>, #{
    <<"name">> => <<"World">>
}).
```

### Server Example

```erlang
%% Start server
{ok, Server} = erlmcp_server:start_link({stdio, []}, #{
    <<"serverInfo">> => #{
        <<"name">> => <<"my-server">>,
        <<"version">> => <<"1.0.0">>
    }
}),

%% Add a resource
erlmcp_server:add_resource(Server, <<"hello://world">>,
    fun(_Uri) -> <<"Hello from Erlang!">> end),

%% Add a tool with schema
Schema = #{
    <<"type">> => <<"object">>,
    <<"properties">> => #{
        <<"name">> => #{<<"type">> => <<"string">>}
    }
},
erlmcp_server:add_tool_with_schema(Server, <<"greet">>,
    fun(#{<<"name">> := Name}) ->
        <<"Hello, ", Name/binary, "!">>
    end, Schema).
```

## API Reference

See [../../docs/api-reference.md](../../docs/api-reference.md) for complete API documentation.

### Key Functions

**erlmcp_client:**
- `start_link/2` - Start client with transport
- `initialize/2` - Initialize MCP session
- `list_resources/1`, `read_resource/2` - Resource operations
- `call_tool/3` - Tool invocation
- `batch_call/2`, `pipeline/2` - Batched and pipelined requests
- `enable_auto_batching/2` - Automatic request batching
- `subscribe/2` - Resource change notifications

**erlmcp_server:**
- `start_link/2` - Start server with capabilities
- `add_resource/3`, `add_tool_with_schema/4` - Register handlers
- `notify_resource_changed/2` - Push notifications

**erlmcp_registry:**
- `register_server/2`, `find_server/1` - Server registration
- `route_to_server/3` - Message routing

## Build & Test

```bash
# Compile
rebar3 compile --app erlmcp_core

# Unit tests
rebar3 eunit --app erlmcp_core

# Specific module
rebar3 eunit --module=erlmcp_client_tests

# Type checking
rebar3 dialyzer --app erlmcp_core

# Coverage
rebar3 cover --app erlmcp_core
```

**Coverage Target:** 80% minimum (enforced by TCPS)

## Configuration

Override defaults in `sys.config`:

```erlang
[
    {erlmcp_core, [
        {client_defaults, #{
            timeout => 10000,
            strict_mode => true
        }},
        {server_defaults, #{
            max_subscriptions_per_resource => 2000
        }},
        %% Distributed cluster configuration
        {cluster_enabled, false},
        {cluster_nodes, []},
        {cluster_cookie, erlmcp_cluster},
        {cluster_heartbeat_interval, 10000},
        {split_brain_strategy, winner_takes_all},
        {master_node, undefined}
    ]}
].
```

## Distributed Registry

erlmcp supports multi-node clustering with global process registration via gproc's global mode.

### Architecture

**Components:**
- `erlmcp_registry_dist` - Global registration wrapper using gproc global names
- `erlmcp_node_monitor` - Tracks node connectivity and health
- `erlmcp_split_brain_detector` - Detects network partitions and resolves conflicts
- `erlmcp_cluster_sup` - Supervises cluster infrastructure

**Key Features:**
- Automatic failover when nodes disconnect
- Split-brain detection with configurable resolution strategies
- Global name registration visible across all cluster nodes
- Process monitoring with automatic cleanup

### Configuration

Enable clustering in `sys.config`:

```erlang
{erlmcp_core, [
    {cluster_enabled, true},
    {cluster_nodes, ['erlmcp1@host1', 'erlmcp2@host2']},
    {cluster_cookie, my_secret_cookie},
    {cluster_heartbeat_interval, 10000},        % ms
    {node_check_interval, 5000},                 % ms
    {split_brain_strategy, winner_takes_all},    % or oldest_node, configured_master
    {split_brain_check_interval, 30000},         % ms
    {master_node, 'erlmcp1@host1'}              % for configured_master strategy
]}
```

### Usage

**Register globally:**

```erlang
%% Register server globally across cluster
{ok, Server} = erlmcp_server:start_link(...),
ok = erlmcp_registry:register_server(global, my_server, Server, Config).

%% Find server from any node
{ok, {Node, Pid, Config}} = erlmcp_registry:find_server(global, my_server).

%% List all global servers
Servers = erlmcp_registry:list_servers(global).
```

**Local vs Global:**

```erlang
%% Local (default) - only visible on current node
erlmcp_registry:register_server(local, my_server, Pid, Config).

%% Global - visible across entire cluster
erlmcp_registry:register_server(global, my_server, Pid, Config).
```

### Split-Brain Resolution

Three strategies for handling network partitions:

**1. winner_takes_all** (default)
- Partition with majority of nodes remains master
- Minority partition stops accepting writes

**2. oldest_node**
- Partition containing the oldest node (by creation time) wins
- Other partitions defer to oldest node

**3. configured_master**
- Pre-configured master node wins
- Requires `master_node` configuration
- Partition without master should stop

**Check partition status:**

```erlang
Status = erlmcp_split_brain_detector:get_partition_status(),
#{
    partition_detected => false,
    strategy => winner_takes_all,
    master_node => undefined,
    last_check => 1706320800
}.
```

### Node Monitoring

Track cluster node health:

```erlang
%% Get status of all monitored nodes
Status = erlmcp_node_monitor:get_node_status(),
#{
    'node1@host1' => #{status => up, last_seen => 1706320800},
    'node2@host2' => #{status => down, last_seen => 1706320700}
}.

%% Get specific node health
{ok, Health} = erlmcp_node_monitor:get_node_health('node1@host1').

%% Force connectivity check
ok = erlmcp_node_monitor:force_node_check().
```

### Failover Behavior

When a process registered globally dies:

1. gproc automatically unregisters the global name
2. Registry receives `{gproc, unreg, ...}` notification
3. Name becomes available for re-registration
4. New process can register with same name on any node

**Example:**

```erlang
%% Node1: Register process
{ok, Pid1} = my_server:start_link(),
ok = erlmcp_registry_dist:register_global(server, my_service, Pid1, #{}).

%% Node2: Can find it
{ok, {node1@host, Pid1, _}} = erlmcp_registry_dist:whereis_global({server, my_service}).

%% Node1: Process dies
exit(Pid1, kill).
% After cleanup...

%% Node2: Can register replacement
{ok, Pid2} = my_server:start_link(),
ok = erlmcp_registry_dist:register_global(server, my_service, Pid2, #{}).
```

### Testing

**Unit tests:**
```bash
rebar3 eunit --module=erlmcp_registry_dist_tests
```

**Multi-node CT tests:**
```bash
rebar3 ct --suite=erlmcp_registry_dist_SUITE
```

The CT suite spawns slave nodes and tests:
- Multi-node registration and discovery
- Automatic failover on process death
- Split-brain detection
- Global name conflicts
- Node reconnection

## Request Batching & Pipelining

erlmcp_core includes high-performance request batching for 2-5x throughput improvement:

### Batch Multiple Requests

```erlang
%% Batch multiple tool calls together
Requests = [
    {<<"tool1">>, #{arg => 1}},
    {<<"tool2">>, #{arg => 2}},
    {<<"tool3">>, #{arg => 3}}
],
{ok, Results} = erlmcp_client:batch_call(Client, Requests).
```

### Pipelined Requests (Non-Blocking)

```erlang
%% Send requests without waiting
{ok, Refs} = erlmcp_client:pipeline(Client, [
    {<<"tool1">>, #{}},
    {<<"tool2">>, #{}}
]),

%% Receive results asynchronously
receive
    {pipeline_result, Ref, Result} -> ok
end.
```

### Automatic Batching

```erlang
%% Enable auto-batching with size strategy
erlmcp_client:enable_auto_batching(Client, {size, 10}),

%% Or time-based strategy (batch every 10ms)
erlmcp_client:enable_auto_batching(Client, {time, 10}),

%% Or adaptive strategy (self-adjusting)
erlmcp_client:enable_auto_batching(Client, {adaptive, #{min => 5, max => 50}}).
```

### Batching Strategies

- **Size-based**: `{size, N}` - Batch every N requests
- **Time-based**: `{time, Ms}` - Batch every Ms milliseconds
- **Adaptive**: `{adaptive, #{min => Min, max => Max}}` - Dynamic based on load

### Performance

Batching provides 2-5x throughput improvement over single requests:

```bash
# Run batching benchmark
rebar3 shell
> erlmcp_bench_batch:run().

# Results (example)
Baseline:   2,500 msg/s
Size-based: 8,750 msg/s (3.5x improvement)
Time-based: 7,200 msg/s (2.9x improvement)
Adaptive:  10,100 msg/s (4.0x improvement)
```

## Schema Registry & Validation (Agent 12)

Centralized schema management with versioning support for tools, resources, and prompts.

### Features

**Schema Management:**
- Register schemas with semantic versioning (major.minor.patch)
- Store tool, resource, and prompt schemas
- Version history tracking
- Schema retrieval by name and version
- Get latest version

**Validation:**
- JSON Schema validation using jesse
- Async validation via poolboy workers
- Custom validators (regex, ranges, dependencies)
- Detailed error reporting with paths

**Compatibility:**
- Backward compatibility checking
- Breaking change detection
- Schema evolution support
- Migration helpers

**TCPS Integration:**
- Quality gates for schema validation
- Receipt generation for schema changes
- Error-proofing (poka-yoke) validation

### API Usage

```erlang
%% Start registry (automatic with erlmcp_core)
{ok, _} = erlmcp_schema_registry:start_link().

%% Register a tool schema
ToolSchema = #{
    <<"type">> => <<"object">>,
    <<"properties">> => #{
        <<"query">> => #{
            <<"type">> => <<"string">>,
            <<"minLength">> => 1
        },
        <<"limit">> => #{
            <<"type">> => <<"integer">>,
            <<"minimum">> => 1,
            <<"maximum">> => 100
        }
    },
    <<"required">> => [<<"query">>]
},

ok = erlmcp_schema_registry:register(
    <<"search_tool">>,      % Schema name
    {1, 0, 0},             % Version (major, minor, patch)
    tool,                  % Type: tool | resource | prompt
    ToolSchema
).

%% Validate tool arguments
Arguments = #{
    <<"query">> => <<"erlang otp">>,
    <<"limit">> => 50
},

ok = erlmcp_schema_registry:validate(
    <<"search_tool">>,
    {1, 0, 0},
    Arguments
).

%% Invalid arguments - missing required field
InvalidArgs = #{<<"limit">> => 50},
{error, Errors} = erlmcp_schema_registry:validate(
    <<"search_tool">>,
    {1, 0, 0},
    InvalidArgs
).
%% Errors: [#{path => [<<"query">>],
%%            message => <<"Missing required property: query">>, ...}]

%% Register new version
ToolSchemaV2 = maps:put(<<"properties">>,
    maps:merge(maps:get(<<"properties">>, ToolSchema), #{
        <<"sort">> => #{
            <<"type">> => <<"string">>,
            <<"enum">> => [<<"relevance">>, <<"date">>]
        }
    }),
    ToolSchema
),

ok = erlmcp_schema_registry:register(
    <<"search_tool">>,
    {1, 1, 0},  % Minor version bump (backward compatible)
    tool,
    ToolSchemaV2
).

%% Check compatibility
{ok, compatible} = erlmcp_schema_registry:check_compatibility(
    <<"search_tool">>,
    {1, 0, 0},  % From version
    {1, 1, 0}   % To version
).

%% List all versions
{ok, Versions} = erlmcp_schema_registry:list_versions(<<"search_tool">>).
%% Versions: [{1, 0, 0}, {1, 1, 0}]

%% Get latest version
{ok, Latest} = erlmcp_schema_registry:get_latest(<<"search_tool">>).
%% Latest: #schema{name = <<"search_tool">>, version = {1, 1, 0}, ...}
```

### Schema Evolution

**Compatible Changes (minor/patch version bump):**
- Adding optional properties
- Relaxing constraints (e.g., increasing maximum)
- Adding enum values

**Breaking Changes (major version bump):**
- Adding required properties
- Removing properties
- Changing types
- Tightening constraints

Example:
```erlang
%% v1.0.0 - Initial schema
V1 = #{
    <<"type">> => <<"object">>,
    <<"properties">> => #{
        <<"name">> => #{<<"type">> => <<"string">>}
    },
    <<"required">> => [<<"name">>]
},

%% v1.1.0 - Add optional field (compatible)
V11 = maps:put(<<"properties">>,
    maps:merge(maps:get(<<"properties">>, V1), #{
        <<"email">> => #{<<"type">> => <<"string">>}
    }),
    V1
),

%% v2.0.0 - Make email required (breaking)
V2 = maps:put(<<"required">>, [<<"name">>, <<"email">>], V11),

erlmcp_schema_registry:register(<<"user">>, {1, 0, 0}, V1),
erlmcp_schema_registry:register(<<"user">>, {1, 1, 0}, V11),
erlmcp_schema_registry:register(<<"user">>, {2, 0, 0}, V2),

{ok, compatible} = erlmcp_schema_registry:check_compatibility(
    <<"user">>, {1, 0, 0}, {1, 1, 0}),

{ok, breaking_change} = erlmcp_schema_registry:check_compatibility(
    <<"user">>, {1, 1, 0}, {2, 0, 0}).
```

### Integration with erlmcp_server

Schemas can be registered when adding tools:

```erlang
%% Add tool with schema validation
ToolHandler = fun(Args) ->
    % Tool logic here
    <<"Result">>
end,

ToolSchema = #{
    <<"type">> => <<"object">>,
    <<"properties">> => #{
        <<"input">> => #{<<"type">> => <<"string">>}
    },
    <<"required">> => [<<"input">>]
},

% Register schema
erlmcp_schema_registry:register(<<"my_tool">>, {1, 0, 0}, tool, ToolSchema),

% Add tool to server
erlmcp_server:add_tool_with_schema(Server, <<"my_tool">>, ToolHandler, ToolSchema).
```

### Performance

- **ETS tables** with `read_concurrency` for O(1) lookups
- **Poolboy** workers (5 base + 10 overflow) for parallel validation
- **jesse** for efficient JSON Schema validation
- Supports 100+ concurrent validations

### Architecture

```
erlmcp_schema_registry (gen_server)
├── ETS: erlmcp_schemas (schemas by {Name, Version})
├── ETS: erlmcp_schema_versions (versions by Name)
└── Poolboy: schema_validator_pool
    └── erlmcp_schema_validator workers (5-15)
        └── jesse validator
```

### Testing

```bash
rebar3 eunit --module=erlmcp_schema_registry_tests
```

Tests cover:
- Schema registration
- Version retrieval
- Validation (valid/invalid data)
- Compatibility checking
- Concurrent operations (100 parallel validations)
- Schema deletion

## See Also

- [erlmcp_transports](../erlmcp_transports/README.md) - Transport layer
- [erlmcp_observability](../erlmcp_observability/README.md) - Metrics & traces
- [Architecture](../../docs/architecture.md) - System design
