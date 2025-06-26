# Memory Server Example

A sophisticated MCP (Model Context Protocol) server implementing a knowledge graph with entities, relations, and observations. This example demonstrates proper OTP application design with supervision, persistent storage, and comprehensive CRUD operations.

## Features

### Knowledge Graph Management
- **Entities** with types and observations
- **Relations** between entities with typed connections
- **Full-text search** across all graph elements
- **Persistent storage** with automatic saving

### OTP Design
- Complete OTP application with supervision tree
- Fault-tolerant design with automatic recovery
- Separate processes for business logic and MCP protocol
- Configurable auto-save with dirty state tracking

### MCP Implementation
- **9 Tools** for complete CRUD operations
- JSON Schema validation for all tool inputs
- Proper error handling and reporting
- Transport flexibility (stdio, TCP, HTTP)

## Architecture

```
memory_server_app (application)
    └── memory_server_sup (supervisor, one_for_all)
            ├── memory_server (gen_server - business logic)
            └── memory_server_mcp (gen_server - MCP interface)
```

### Components

- **memory_server_app**: OTP application behavior
- **memory_server_sup**: Supervisor with one_for_all strategy
- **memory_server**: Core knowledge graph management with persistence
- **memory_server_mcp**: MCP protocol handler and tool definitions

### Design Rationale

The architecture separates concerns:
- Business logic (graph operations) is isolated in `memory_server`
- Protocol handling is managed by `memory_server_mcp`
- This allows testing and using the knowledge graph independently of MCP

## Building and Running

### Prerequisites
- Erlang/OTP 24 or later
- rebar3

### Build
```bash
cd examples/memory_server
rebar3 compile
```

### Run with stdio transport (default)
```bash
rebar3 run
```

### Run with custom configuration
```bash
# Create config/sys.config
cat > config/sys.config << EOF
[
  {memory_server, [
    {memory_file_path, "/path/to/custom/memory.json"},
    {transport, {tcp, 8080}},
    {auto_save, true},
    {save_interval, 10000}
  ]}
].
EOF

rebar3 run
```

### Run as escript
```bash
rebar3 escriptize
./memory_server
```

## Configuration Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `memory_file_path` | `string()` | `"memory.json"` | Path to the persistent storage file |
| `transport` | `stdio | {tcp, Port} | {http, Url}` | `stdio` | MCP transport configuration |
| `auto_save` | `boolean()` | `true` | Enable automatic saving of changes |
| `save_interval` | `pos_integer()` | `5000` | Milliseconds between auto-saves |

## Data Model

### Entity
```erlang
-record(entity, {
    name :: binary(),           % Unique identifier
    entity_type :: binary(),    % Type classification
    observations :: [binary()]  % List of observations
}).
```

### Relation
```erlang
-record(relation, {
    from :: binary(),          % Source entity name
    to :: binary(),            % Target entity name
    relation_type :: binary()  % Type of relationship
}).
```

### Knowledge Graph
```erlang
-record(knowledge_graph, {
    entities :: [#entity{}],
    relations :: [#relation{}]
}).
```

## MCP Tools

### 1. create_entities
Create new entities in the knowledge graph.
```json
{
  "name": "create_entities",
  "arguments": {
    "entities": [
      {
        "name": "Alice",
        "entityType": "Person",
        "observations": ["Software engineer", "Team lead"]
      },
      {
        "name": "Acme Corp",
        "entityType": "Company",
        "observations": ["Technology company", "Founded 2020"]
      }
    ]
  }
}
```

### 2. create_relations
Create relations between existing entities.
```json
{
  "name": "create_relations",
  "arguments": {
    "relations": [
      {
        "from": "Alice",
        "to": "Acme Corp",
        "relationType": "works_at"
      }
    ]
  }
}
```

### 3. add_observations
Add observations to existing entities.
```json
{
  "name": "add_observations",
  "arguments": {
    "entityName": "Alice",
    "observations": ["Python expert", "Conference speaker"]
  }
}
```

### 4. delete_entities
Remove entities and their associated relations.
```json
{
  "name": "delete_entities",
  "arguments": {
    "entityNames": ["Old Entity 1", "Old Entity 2"]
  }
}
```

### 5. delete_observations
Remove specific observations from entities.
```json
{
  "name": "delete_observations",
  "arguments": {
    "entityName": "Alice",
    "observations": ["Outdated info"]
  }
}
```

### 6. delete_relations
Remove specific relations.
```json
{
  "name": "delete_relations",
  "arguments": {
    "relations": [
      {
        "from": "Alice",
        "to": "Old Company",
        "relationType": "works_at"
      }
    ]
  }
}
```

### 7. read_graph
Get the complete knowledge graph.
```json
{
  "name": "read_graph",
  "arguments": {}
}
```

### 8. search_nodes
Search for entities and relations by query string.
```json
{
  "name": "search_nodes",
  "arguments": {
    "query": "Alice"
  }
}
```

### 9. open_nodes
Get specific entities and their relations.
```json
{
  "name": "open_nodes",
  "arguments": {
    "entityNames": ["Alice", "Bob"]
  }
}
```

## Storage Format

The memory file uses newline-delimited JSON (NDJSON) format:

```json
{"type": "entity", "name": "Alice", "entityType": "Person", "observations": ["Software engineer"]}
{"type": "entity", "name": "Acme Corp", "entityType": "Company", "observations": ["Tech company"]}
{"type": "relation", "from": "Alice", "to": "Acme Corp", "relationType": "works_at"}
```

Benefits of NDJSON:
- Streaming-friendly for large datasets
- Simple append operations
- Line-by-line parsing
- Human-readable and debuggable

## Testing with MCP Clients

### Using mcp-cli
```bash
# Create entities
mcp-cli --server stdio://rebar3 --server-args "run" tools call create_entities --arguments '{
  "entities": [
    {"name": "Alice", "entityType": "Person", "observations": ["Developer"]},
    {"name": "Acme", "entityType": "Company", "observations": ["Tech"]}
  ]
}'

# Create relations
mcp-cli --server stdio://rebar3 --server-args "run" tools call create_relations --arguments '{
  "relations": [
    {"from": "Alice", "to": "Acme", "relationType": "works_at"}
  ]
}'

# Search the graph
mcp-cli --server stdio://rebar3 --server-args "run" tools call search_nodes --arguments '{"query": "Alice"}'

# Read entire graph
mcp-cli --server stdio://rebar3 --server-args "run" tools call read_graph --arguments '{}'
```

## Design Patterns

### State Management
The server maintains state with automatic persistence:
```erlang
-record(state, {
    knowledge_graph :: #knowledge_graph{},
    memory_file_path :: binary(),
    save_timer :: reference(),
    auto_save :: boolean(),
    save_interval :: pos_integer(),
    dirty :: boolean()  % Track if save needed
}).
```

### Dirty State Tracking
- Operations that modify the graph mark state as dirty
- Auto-save timer periodically saves if dirty
- Manual save on shutdown ensures no data loss

### Atomic File Operations
```erlang
%% Write to temporary file first
TempFile = <<FilePath/binary, ".tmp">>,
file:write_file(TempFile, Data),
%% Atomic rename
file:rename(TempFile, FilePath)
```

### Error Recovery
- Invalid JSON lines are skipped with warnings
- Partial data corruption doesn't lose entire graph
- Failed saves retain in-memory state

## Performance Considerations

### Efficient Operations
- O(n) entity/relation creation with duplicate checking
- O(n) search with full-text matching
- Sets for tracking unique values
- Lazy file I/O with dirty state tracking

### Scalability Limits
- In-memory storage (limited by available RAM)
- Linear search complexity
- Single-writer model

For production use with large datasets, consider:
- Adding indexes for common queries
- Using ETS tables for faster lookups
- Implementing pagination for large results
- Moving to a dedicated graph database

## Extending the Server

### Adding New Entity Types
```erlang
%% Add validation for specific entity types
validate_entity_type(<<"Person">>, Observations) ->
    %% Check for required observations
    ok;
validate_entity_type(<<"Company">>, Observations) ->
    %% Validate company-specific data
    ok.
```

### Adding Computed Properties
```erlang
%% Add derived data to entities
enrich_entity(#entity{entity_type = <<"Person">>} = E) ->
    %% Add computed fields
    E#{connections => count_connections(E)}.
```

### Custom Search Algorithms
```erlang
%% Implement specialized search
search_by_relation_type(RelationType, State) ->
    %% Find all entities connected by specific relation type
    ...
```

## Troubleshooting

### Server won't start
- Check file permissions for memory file path
- Verify no syntax errors in sys.config
- Ensure erlmcp dependency is available

### Data not persisting
- Check auto_save is enabled
- Verify write permissions on memory file
- Look for save errors in logs

### Search not finding entities
- Entity/relation names are case-sensitive
- Search is substring-based, not fuzzy
- Check for extra whitespace in data

### Memory usage growing
- Monitor size of knowledge graph
- Implement data archival for old entities
- Consider external storage for large graphs

## Production Deployment

### Monitoring
```erlang
%% Add metrics collection
handle_call(get_stats, _From, State) ->
    Stats = #{
        entity_count => length(State#state.knowledge_graph#knowledge_graph.entities),
        relation_count => length(State#state.knowledge_graph#knowledge_graph.relations),
        memory_usage => erlang:memory(total)
    },
    {reply, Stats, State}.
```

### Backup Strategy
```bash
# Backup script
#!/bin/bash
cp memory.json "backups/memory-$(date +%Y%m%d-%H%M%S).json"
find backups -name "memory-*.json" -mtime +7 -delete
```

### Health Checks
```erlang
%% Add health check endpoint
handle_call(health_check, _From, State) ->
    Health = #{
        status => <<"healthy">>,
        last_save => State#state.last_save_time,
        dirty => State#state.dirty
    },
    {reply, {ok, Health}, State}.
```

## License

MIT License - See LICENSE file for details.
