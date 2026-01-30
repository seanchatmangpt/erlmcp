# MCP SPARQL Queries - Quick Reference

## 20 Comprehensive SPARQL Queries at a Glance

### File: `queries.sparql`
- **Size**: 641 lines
- **Queries**: 20 comprehensive extraction queries
- **Format**: SPARQL 1.1 with W3C standard PREFIX declarations
- **Scope**: Complete MCP protocol coverage for code generation

---

## Query Index

| # | Query Name | Purpose | Output | Namespace |
|---|-----------|---------|--------|-----------|
| 1 | **server_handlers** | Extract server request methods | Method handlers with phase requirements | mcp-server |
| 2 | **client_api** | Extract client method signatures | Complete function signatures | mcp-client |
| 3 | **validators** | Extract validation rules | Validation rules by component type | mcp-validation |
| 4 | **task_lifecycle** | Extract state machine | States, transitions, events | mcp-proto |
| 5 | **capabilities** | Extract capability definitions | Capabilities with version info | mcp-proto |
| 6 | **error_codes** | Extract error taxonomy | Error codes and messages | mcp-error |
| 7 | **transports** | Extract transport layer | Transport specs with callbacks | mcp-transport |
| 8 | **resources** | Extract resource definitions | Resource types and schemas | mcp-proto |
| 9 | **tools** | Extract tool definitions | Tool specs with parameters | mcp-proto |
| 10 | **prompts** | Extract prompt definitions | Prompt templates and arguments | mcp-proto |
| 11 | **request_response_correlation** | Request/response mapping | ID strategy, timeout, async | mcp-proto |
| 12 | **message_schemas** | JSON-RPC message schemas | Message types and field constraints | mcp-proto |
| 13 | **initialization_protocol** | Initialization handshake | Capabilities, negotiation rules | mcp-proto |
| 14 | **protocol_versioning** | Version compatibility | Breaking changes, compatibility | mcp-proto |
| 15 | **phase_based_access_control** | Phase permissions | Allowed methods per phase | mcp-proto |
| 16 | **notifications** | Notification types | Server-to-client notifications | mcp-proto |
| 17 | **rate_limiting** | Flow control | Rate limits, burst, backpressure | mcp-proto |
| 18 | **progress_tracking** | Long-running operations | Progress reporting, cancellation | mcp-proto |
| 19 | **sampling_integration** | LLM sampling config | Model parameters, tool use | mcp-proto |
| 20 | **security_authorization** | Security requirements | Auth methods, RBAC, ABAC | mcp-proto |

---

## Query Categories

### Core Protocol (Queries 1-6)
Essential queries for basic MCP server/client implementation.
- Server handlers
- Client APIs
- Validation rules
- State machines
- Capabilities
- Error codes

### Transport & Data (Queries 7-10)
Specifications for data types and transport implementations.
- Transport layer
- Resources
- Tools
- Prompts

### Advanced Features (Queries 11-20)
Protocol features for production use.
- Request correlation
- Message schemas
- Initialization
- Versioning
- Phase control
- Notifications
- Rate limiting
- Progress tracking
- LLM integration
- Security

---

## Namespace URIs

```
Core Protocol    →  http://erlmcp.org/ontology/mcp/protocol#
Server Specific  →  http://erlmcp.org/ontology/mcp/server#
Client Specific  →  http://erlmcp.org/ontology/mcp/client#
Validation       →  http://erlmcp.org/ontology/mcp/validation#
Error Handling   →  http://erlmcp.org/ontology/mcp/error#
Transport Layer  →  http://erlmcp.org/ontology/mcp/transport#
```

---

## Key Query Patterns

### Pattern 1: Basic Extraction
```sparql
SELECT ?id ?name ?description
WHERE {
    ?resource a mcp-proto:ResourceType ;
              mcp-proto:resourceId ?id ;
              mcp-proto:resourceName ?name ;
              rdfs:comment ?description .
}
```

### Pattern 2: With Optional Properties
```sparql
SELECT ?id ?name ?property1 ?property2
WHERE {
    ?resource a mcp-proto:ResourceType ;
              mcp-proto:resourceId ?id ;
              mcp-proto:resourceName ?name .
    OPTIONAL { ?resource mcp-proto:property1 ?property1 . }
    OPTIONAL { ?resource mcp-proto:property2 ?property2 . }
}
```

### Pattern 3: With Nested Objects
```sparql
SELECT ?resourceId ?paramName ?paramType
WHERE {
    ?resource a mcp-proto:ResourceType ;
              mcp-proto:resourceId ?resourceId ;
              mcp-proto:parameter ?paramUri .

    ?paramUri mcp-proto:paramName ?paramName ;
              mcp-proto:paramType ?paramType .
}
```

### Pattern 4: With Aggregation
```sparql
SELECT ?phase (COUNT(?method) AS ?methodCount)
WHERE {
    ?phase a mcp-proto:Phase ;
           mcp-proto:allowedMethod ?method .
}
GROUP BY ?phase
```

---

## Common Query Outputs

### Output Format 1: Method List
```
methodName | description | timeout | phase
------------|------------|---------|--------
initialize | Initialize  | 5000    | INIT
resources/list | List resources | 3000 | READY
```

### Output Format 2: Error Codes
```
errorCode | errorName | messageTemplate | severity
-----------|----------|-----------------|----------
-32600    | InvalidRequest | Invalid Request | error
-32601    | MethodNotFound | Method not found | error
```

### Output Format 3: Tool Definitions
```
toolName | paramName | paramType | paramRequired | timeout
----------|----------|----------|---------------|-------
sql_query | query | string | true | 30000
sql_query | limit | integer | false | 30000
```

---

## Integration Checklist

- [ ] Copy `queries.sparql` to `sparql/mcp/` directory
- [ ] Extract individual queries to separate `.rq` files
- [ ] Register queries in `ggen.toml` under `[queries.named]`
- [ ] Create corresponding Tera templates in `templates/`
- [ ] Test queries against sample RDF data
- [ ] Validate generated Erlang code compiles
- [ ] Document generated modules
- [ ] Add to CI/CD pipeline

---

## Common SPARQL Filters & Functions

### Filtering

```sparql
# Filter by value
FILTER(?phase = "READY")

# Filter by string pattern
FILTER(CONTAINS(?methodName, "resources"))

# Filter by numeric range
FILTER(?timeout > 1000 && ?timeout < 30000)

# Filter for exists
FILTER(EXISTS { ?handler mcp-server:validationRequired true })
```

### String Functions

```sparql
BIND(CONCAT(?first, "-", ?second) AS ?fullName)
BIND(UCASE(?methodName) AS ?methodUpper)
BIND(STRLEN(?description) AS ?descLength)
```

### Aggregation

```sparql
(COUNT(?method) AS ?methodCount)
(SUM(?timeout) AS ?totalTimeout)
(AVG(?paramCount) AS ?avgParams)
(GROUP_CONCAT(?paramName; separator=", ") AS ?params)
```

---

## Example: Extracting Server Handlers

### Query (Query 1 from queries.sparql)

```sparql
SELECT ?handlerId ?methodName ?phase ?requiredCapability ?timeout
WHERE {
    ?handler a mcp-server:RequestHandler ;
             mcp-proto:handlerId ?handlerId ;
             mcp-proto:methodName ?methodName ;
             mcp-server:phase ?phase ;
             mcp-server:requiredCapability ?requiredCapability ;
             mcp-server:timeout ?timeout .
}
ORDER BY ?methodName
```

### Result

```
handlerId | methodName | phase | requiredCapability | timeout
-----------|-----------|-------|-------------------|-------
h-init | initialize | INIT | - | 5000
h-res-list | resources/list | READY | resources | 3000
h-tool-list | tools/list | READY | tools | 3000
```

### Generated Erlang Module

```erlang
%% Auto-generated from ontology
-module(erlmcp_server_handlers).

handle_initialize(_Params, State) ->
    %% Initialize connection
    %% Phase: INIT
    %% Timeout: 5000ms
    {ok, State}.

handle_resources_list(_Params, State) ->
    %% List available resources
    %% Phase: READY
    %% Required Capability: resources
    %% Timeout: 3000ms
    {ok, State}.
```

---

## Validation Checklist

Before using queries in production:

- [ ] All 20 queries have proper PREFIX declarations
- [ ] SPARQL syntax is valid (no typos)
- [ ] SELECT variables match WHERE clause
- [ ] OPTIONAL blocks don't cause NULLs to break logic
- [ ] ORDER BY clauses ensure deterministic output
- [ ] Comments explain each query's purpose
- [ ] Ontology has all required classes and properties
- [ ] Test data validates against SHACL shapes
- [ ] Generated code compiles and tests pass

---

## Troubleshooting

### Issue: Empty Results
- Check that RDF data uses correct namespace URIs
- Verify ontology classes match query expectations
- Test with smaller query to isolate issue

### Issue: NULL Values in Results
- Add OPTIONAL blocks for missing properties
- Use FILTER to exclude NULL results if needed
- Check ontology for property domain/range constraints

### Issue: Slow Queries
- Add FILTER clauses early to reduce search space
- Consider creating database indexes on frequently queried properties
- Break complex queries into smaller ones

### Issue: Duplicate Results
- Add DISTINCT modifier
- Use GROUP BY instead of SELECT *
- Check for multiple ?uri instances in WHERE clause

---

## Performance Tips

1. **Use FILTER early** - Reduce result set size early
2. **Minimize OPTIONAL blocks** - Use only when necessary
3. **Order by indexed properties** - Usually RDF object IDs
4. **Use BIND for calculations** - Don't compute in application
5. **Batch similar queries** - Fewer endpoint calls

---

## Testing Queries

### Using Apache Jena

```bash
# Test single query
arq --data ontology.ttl --query query1.rq

# Test with format
arq --data ontology.ttl --query query1.rq --results JSON
```

### Using Python RDFLib

```python
from rdflib import Graph

g = Graph()
g.parse("ontology.ttl", format="ttl")

query = """
PREFIX mcp: <http://erlmcp.org/ontology/mcp#>
SELECT ?name WHERE { ?x rdf:type mcp-proto:Method ; ?x mcp-proto:methodName ?name . }
"""

results = g.query(query)
for row in results:
    print(row)
```

---

## File Structure

```
ggen_templates/
├── sparql/
│   ├── queries.sparql          ← Master query file (this)
│   ├── README.md               ← Full documentation
│   └── QUICK_REFERENCE.md      ← This file
└── erlang/
    └── (Tera templates for code generation)
```

---

## Related Files

- **Ontology**: `/ontology/` - RDF/TTL ontology definitions
- **SPARQL Queries**: `/sparql/tcps_queries/` - TCPS-specific queries
- **Code Generation**: `ggen.toml` - Configuration for query → code
- **Templates**: `/templates/` - Tera templates for output
- **Documentation**: `/docs/protocol.md` - MCP protocol spec

---

## Version

**Version 1.0.0** (2026-01-30)
- Complete MCP protocol coverage
- 20 comprehensive extraction queries
- Ready for code generation integration
- SPARQL 1.1 compliant

---

## License

Apache License 2.0 (same as erlmcp project)
