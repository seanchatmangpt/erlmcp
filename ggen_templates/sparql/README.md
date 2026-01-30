# MCP Protocol SPARQL Queries - Documentation

## Overview

This directory contains comprehensive SPARQL queries for extracting Model Context Protocol (MCP) metadata from an RDF ontology. These queries enable code generation for:

- Server request handlers
- Client API methods
- Validation rules
- Error codes
- Transport specifications
- Resource/tool/prompt definitions
- Protocol state machines
- Security and authorization rules

## File: `queries.sparql`

Contains 20 comprehensive SPARQL queries organized into logical sections.

### Query Organization

#### Core Protocol Extraction (Queries 1-6)

1. **server_handlers** - Extract all request methods
   - Extracts: method name, phase requirements, capabilities, validation flags, schemas
   - Use for: Generating server-side handler modules
   - Output: List of all server request methods with metadata

2. **client_api** - Extract client method signatures
   - Extracts: method name, parameters, return types, timeout, phase requirements
   - Use for: Generating client library APIs
   - Output: Complete function signatures for client implementation

3. **validators** - Extract validation rules
   - Extracts: component type, validation type, rules, error codes, severity
   - Use for: Generating validation modules
   - Output: Validation rules organized by component and type

4. **task_lifecycle** - Extract task state machine
   - Extracts: states, transitions, events, guards
   - Use for: Generating state machine implementations
   - Output: Complete state machine definition for request lifecycle

5. **capabilities** - Extract capability definitions
   - Extracts: capability name, methods, experimental flag, version info
   - Use for: Generating capability negotiation modules
   - Output: Capability specifications and negotiation rules

6. **error_codes** - Extract error taxonomy
   - Extracts: code number, name, message template, context, recovery
   - Use for: Generating error handling modules
   - Output: Complete error code mapping and messages

#### Transport and Data Definitions (Queries 7-10)

7. **transports** - Extract transport requirements
   - Extracts: transport type, callbacks, message framing, features
   - Use for: Generating transport layer implementations
   - Output: Transport specifications with callback signatures

8. **resources** - Extract resource definitions
   - Extracts: URI pattern, properties, content types, validation, subscription support
   - Use for: Generating resource handlers
   - Output: Resource type definitions with schemas

9. **tools** - Extract tool definitions
   - Extracts: tool name, input parameters, schema, timeout, execution constraints
   - Use for: Generating tool registration code
   - Output: Tool metadata with complete signatures

10. **prompts** - Extract prompt definitions
    - Extracts: prompt name, arguments, template, examples
    - Use for: Generating prompt template managers
    - Output: Prompt definitions with argument schemas

#### Advanced Protocol Features (Queries 11-20)

11. **request_response_correlation** - Extract request/response mapping
    - Extracts: request ID strategy, timeout mapping, async support, subscriptions
    - Use for: Implementing request correlation layer
    - Output: Configuration for request tracking

12. **message_schemas** - Extract JSON-RPC message schemas
    - Extracts: message type, required fields, constraints, version compatibility
    - Use for: Generating message validation
    - Output: Message schema definitions

13. **initialization_protocol** - Extract initialization handshake
    - Extracts: client/server capabilities, negotiation rules, protocol version
    - Use for: Implementing initialization logic
    - Output: Initialization protocol specification

14. **protocol_versioning** - Extract version compatibility
    - Extracts: version number, breaking changes, deprecated features
    - Use for: Version management and compatibility checking
    - Output: Version compatibility matrix

15. **phase_based_access_control** - Extract phase-based permissions
    - Extracts: phases, allowed methods, legal transitions, errors
    - Use for: Implementing phase-based access control
    - Output: Phase state machine with method permissions

16. **notifications** - Extract notification types
    - Extracts: notification type, parameters, triggering conditions, batching
    - Use for: Implementing server-to-client notifications
    - Output: Notification definitions with parameters

17. **rate_limiting** - Extract flow control requirements
    - Extracts: rate limits, burst allowances, backpressure mechanisms
    - Use for: Implementing rate limiting
    - Output: Rate limit configuration per method

18. **progress_tracking** - Extract long-running operation support
    - Extracts: operation type, progress reporting, cancellation support
    - Use for: Implementing progress tokens
    - Output: Long-running operation specifications

19. **sampling_integration** - Extract LLM sampling configuration
    - Extracts: sampling methods, model parameters, tool use support
    - Use for: Implementing LLM integration
    - Output: Sampling configuration and parameters

20. **security_authorization** - Extract security requirements
    - Extracts: auth methods, RBAC/ABAC, capability-based control
    - Use for: Implementing security modules
    - Output: Security policy specifications

## MCP Ontology Namespace Structure

### Prefixes

```sparql
PREFIX mcp: <http://erlmcp.org/ontology/mcp#>
PREFIX mcp-proto: <http://erlmcp.org/ontology/mcp/protocol#>
PREFIX mcp-server: <http://erlmcp.org/ontology/mcp/server#>
PREFIX mcp-client: <http://erlmcp.org/ontology/mcp/client#>
PREFIX mcp-validation: <http://erlmcp.org/ontology/mcp/validation#>
PREFIX mcp-error: <http://erlmcp.org/ontology/mcp/error#>
PREFIX mcp-transport: <http://erlmcp.org/ontology/mcp/transport#>
```

### Ontology Classes

The queries expect the following RDF classes in the ontology:

**Protocol Core:**
- `mcp-proto:Method` - Individual protocol methods
- `mcp-proto:RequestHandler` - Server-side request handlers
- `mcp-proto:Notification` - Server-to-client notifications
- `mcp-proto:Capability` - Protocol capabilities
- `mcp-proto:Transition` - State machine transitions
- `mcp-proto:State` - Lifecycle states
- `mcp-proto:Phase` - Connection phases (initialization, ready, running)

**Client/Server Specific:**
- `mcp-client:ClientMethod` - Client API methods
- `mcp-server:RequestHandler` - Server message handlers

**Data Definitions:**
- `mcp-proto:ResourceType` - Resource template definitions
- `mcp-proto:Tool` - Tool definitions
- `mcp-proto:Prompt` - Prompt template definitions

**Validation and Error:**
- `mcp-validation:ValidationRule` - Validation constraints
- `mcp-error:ErrorCode` - Error code definitions

**Transport:**
- `mcp-transport:Transport` - Transport implementations

**Advanced:**
- `mcp-proto:MessageSchema` - JSON-RPC message schemas
- `mcp-proto:RateLimit` - Rate limiting rules
- `mcp-proto:LongRunningOperation` - Async operation definitions
- `mcp-proto:LLMSampling` - LLM sampling configuration
- `mcp-proto:Authentication` - Authentication methods

## Usage in Code Generation

### Integration with ggen

Add to `ggen.toml`:

```toml
[queries.named]
server_handlers = "mcp_server_handlers.rq"
client_api = "mcp_client_api.rq"
validators = "mcp_validators.rq"
task_lifecycle = "mcp_task_lifecycle.rq"
capabilities = "mcp_capabilities.rq"
error_codes = "mcp_error_codes.rq"
transports = "mcp_transports.rq"
resources = "mcp_resources.rq"
tools = "mcp_tools.rq"
prompts = "mcp_prompts.rq"
```

### Erlang Code Generation Template Example

```tera
{% for handler in server_handlers %}
%% {{ handler.methodName }}
handle_{{ handler.methodName | snake_case }}(#{
    {% for param in handler.params %}
    <<"{{ param.name }}">> := {{ param.name }}{{ if not loop.last }},{{ endif }}
    {% endfor %}
}) ->
    %% Implementation: {{ handler.methodDescription }}
    %% Phase: {{ handler.phase }}
    %% Validation: {{ handler.validationRequired }}
    ok.

{% endfor %}
```

### Extracting Individual Queries

To extract individual queries from `queries.sparql`:

```bash
# Extract Query 1 (server_handlers)
sed -n '/^# Query 1:/,/^# Query 2:/p' queries.sparql | grep -v "^# Query 2:" > mcp_server_handlers.rq

# Extract Query 3 (validators)
sed -n '/^# Query 3:/,/^# Query 4:/p' queries.sparql | grep -v "^# Query 4:" > mcp_validators.rq
```

## Running Queries Against RDF Data

### Apache Jena

```bash
# Run a single query
arq --data mcp_ontology.ttl --query mcp_server_handlers.rq

# With SPARQL endpoint
arq --service http://localhost:3030/dataset/sparql --query mcp_server_handlers.rq
```

### Python RDFLib

```python
from rdflib import Graph
from rdflib.plugins.sparql import prepareQuery

# Load ontology
g = Graph()
g.parse("mcp_ontology.ttl", format="ttl")

# Run query
query = open("mcp_server_handlers.rq").read()
results = g.query(query)

# Process results
for row in results:
    print(f"Method: {row.methodName}")
    print(f"Phase: {row.phase}")
```

### Blazegraph

```bash
# Load ontology and query
curl -X POST -H 'Content-Type: application/x-turtle' \
  --data-binary @mcp_ontology.ttl \
  http://localhost:9999/bigdata/rdf/data

curl -H 'Content-Type: application/sparql-query' \
  --data-binary @mcp_server_handlers.rq \
  http://localhost:9999/bigdata/sparql
```

## Expected Query Results

### Query 1: server_handlers

| handlerId | methodName | methodDescription | phase | phaseLabel | requiredCapability | validationRequired | timeout |
|-----------|------------|-------------------|-------|------------|-------------------|-------------------|---------|
| h-init | initialize | Establish connection | INIT | initialization | - | true | 5000 |
| h-res-list | resources/list | List available resources | READY | ready | resources | true | 3000 |

### Query 3: validators

| ruleId | componentType | validationType | ruleName | severity | errorCode |
|--------|---------------|----------------|----------|----------|-----------|
| v-req-schema | request | schema | RequestSchema | error | -32602 |
| v-tool-input | tool | semantic | InputValidation | error | -32000 |

### Query 6: error_codes

| errorCode | errorName | messageTemplate | severity | httpStatus |
|-----------|-----------|-----------------|----------|-----------|
| -32600 | InvalidRequest | Invalid Request | error | 400 |
| -32601 | MethodNotFound | Method not found | error | 404 |
| -32602 | InvalidParams | Invalid method parameter(s) | error | 400 |
| -32603 | InternalError | Internal error | error | 500 |

## Ontology Design Requirements

To use these queries effectively, ensure your MCP ontology includes:

### 1. Class Hierarchies

```turtle
mcp-proto:RequestHandler rdfs:subClassOf mcp-proto:Method .
mcp-server:Handler rdfs:subClassOf mcp-proto:RequestHandler .
mcp-client:ClientMethod rdfs:subClassOf mcp-proto:Method .
```

### 2. Properties for Methods

```turtle
mcp-proto:methodName a rdf:Property ;
    rdfs:domain mcp-proto:Method ;
    rdfs:range xsd:string .

mcp-proto:methodId a rdf:Property ;
    rdfs:domain mcp-proto:Method ;
    rdfs:range xsd:string .

mcp-proto:parameter a rdf:Property ;
    rdfs:domain mcp-proto:Method ;
    rdfs:range mcp-proto:Parameter .
```

### 3. Phase-Based Access Control

```turtle
mcp-proto:Phase a rdfs:Class .
mcp-proto:INITIALIZATION a mcp-proto:Phase ;
    rdfs:label "initialization" .
mcp-proto:READY a mcp-proto:Phase ;
    rdfs:label "ready" .
mcp-proto:RUNNING a mcp-proto:Phase ;
    rdfs:label "running" .
```

### 4. Validation Rules

```turtle
mcp-validation:ValidationRule a rdfs:Class ;
    rdfs:subClassOf mcp:Component .

mcp-validation:appliesTo a rdf:Property ;
    rdfs:domain mcp-validation:ValidationRule ;
    rdfs:range rdf:Property .
```

## Best Practices

1. **Use OPTIONAL blocks** - Handle missing properties gracefully
2. **ORDER BY clauses** - Results sorted for consistent code generation
3. **FILTER expressions** - Can be added to filter by phase, capability, etc.
4. **Bind expressions** - Calculate derived values (e.g., `BIND(CONCAT(...) AS ?fullName)`)
5. **Aggregate functions** - Use COUNT, GROUP_CONCAT for summaries

## Related Documentation

- MCP Specification: See `docs/protocol.md`
- Ontology Design: See `ontology/README.md`
- Code Generation: See `ggen.toml`
- Template Examples: See `templates/`

## Version History

### Version 1.0.0 (2026-01-30)

- 20 comprehensive SPARQL queries
- Complete MCP protocol coverage
- Proper SPARQL syntax with PREFIX declarations
- Organized by protocol layer and use case
- Ready for code generation integration

## Contributing

To add new queries:

1. Follow the existing naming conventions
2. Add comprehensive documentation comment block
3. Include result field descriptions
4. Test against sample ontology data
5. Ensure ORDER BY for deterministic results

## Support

For questions about:
- **SPARQL syntax**: See W3C SPARQL 1.1 specification
- **MCP protocol**: See `docs/protocol.md`
- **Ontology structure**: See `ontology/README.md`
- **Code generation**: See `.claude/AGENT_INDEX.md`
