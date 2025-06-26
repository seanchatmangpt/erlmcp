# Memory Server Example

This is an Erlang implementation of the MCP Memory Server, providing a knowledge graph with entities, relations, and observations. It demonstrates how to build MCP servers using the erlmcp SDK with proper OTP patterns.

## Features

- **Knowledge Graph Storage**: Entities with observations and relations between entities
- **File-based Persistence**: JSON storage using newline-delimited format
- **9 MCP Tools**: Complete CRUD operations for knowledge graph management
- **OTP Compliance**: Built using gen_server and supervisor patterns
- **JSON Schema Validation**: All tools have proper input validation

## Building

```bash
cd examples/memory_server
rebar3 compile
```

## Running

```bash
./memory_server
```

Or with custom memory file path:

```bash
MEMORY_FILE_PATH=/path/to/memory.json ./memory_server
```

## MCP Tools

### Entity Management
- `create_entities`: Create new entities with observations
- `delete_entities`: Remove entities and their associated relations
- `add_observations`: Add observations to existing entities
- `delete_observations`: Remove specific observations from entities

### Relation Management
- `create_relations`: Create relations between entities
- `delete_relations`: Remove specific relations

### Graph Operations
- `read_graph`: Get the complete knowledge graph
- `search_nodes`: Search entities and relations by query string
- `open_nodes`: Retrieve specific entities and their relations

## Example Usage with mcp-cli

```bash
# Create entities
mcp-cli tool call create_entities --server memory --input '{
  "entities": [
    {"name": "Alice", "entityType": "Person", "observations": ["works at ACME"]},
    {"name": "ACME", "entityType": "Company", "observations": ["tech company"]}
  ]
}'

# Create relations
mcp-cli tool call create_relations --server memory --input '{
  "relations": [
    {"from": "Alice", "to": "ACME", "relationType": "works_at"}
  ]
}'

# Search the graph
mcp-cli tool call search_nodes --server memory --input '{"query": "Alice"}'

# Read entire graph
mcp-cli tool call read_graph --server memory --input '{}'
```

## Data Format

The memory file uses newline-delimited JSON format:

```json
{"type": "entity", "name": "Alice", "entityType": "Person", "observations": ["works at ACME"]}
{"type": "relation", "from": "Alice", "to": "ACME", "relationType": "works_at"}
```

## Architecture

- `memory_server_app`: OTP application behavior
- `memory_server_sup`: OTP supervisor for fault tolerance
- `memory_server`: Main gen_server implementing knowledge graph logic
- Uses erlmcp_server for MCP protocol handling
- stdio transport for communication with MCP clients
