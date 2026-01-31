# Transport Capability Matrix

Complete MCP capability support matrix across all erlmcp transports.

## Capability Matrix

| Transport   | resources | tools | prompts | roots | apps | elicitation |
|-------------|-----------|-------|---------|-------|------|-------------|
| **stdio**   | ✅        | ✅    | ✅      | ✅    | ❌   | ❌          |
| **tcp**     | ✅        | ✅    | ✅      | ✅    | ❌   | ❌          |
| **http**    | ✅        | ✅    | ✅      | ✅    | ❌   | ❌          |
| **websocket** | ✅      | ✅    | ✅      | ✅    | ❌   | ❌          |
| **sse**     | ✅        | ❌    | ✅      | ✅    | ❌   | ❌          |

## Legend

- ✅ Fully supported and tested
- ❌ Not supported (technical limitation or future feature)

## Capability Descriptions

### resources
**Status**: Fully supported on all transports

MCP resources capability includes:
- `resources/list` - List available resources
- `resources/read` - Read resource content
- `resources/subscribe` - Subscribe to resource changes
- `resources/unsubscribe` - Unsubscribe from changes

### tools
**Status**: Supported on bidirectional transports only

MCP tools capability includes:
- `tools/list` - List available tools
- `tools/call` - Execute tool with parameters

**Note**: SSE does not support tools because it's unidirectional (server → client only).
Tool execution requires bidirectional communication.

### prompts
**Status**: Fully supported on all transports

MCP prompts capability includes:
- `prompts/list` - List available prompt templates
- `prompts/get` - Get specific prompt template

### roots
**Status**: Fully supported on all transports

MCP roots capability includes:
- `roots/list` - List filesystem roots

### apps (future)
**Status**: Not yet implemented

Future MCP capability for application integration.

### elicitation (future)
**Status**: Not yet implemented

Future MCP capability for interactive data gathering.

## Transport-Specific Notes

### STDIO
- **Bidirectional**: Yes
- **Connection model**: Process I/O (stdin/stdout)
- **Limitations**: Process I/O throughput limits
- **Best for**: CLI tools, local server processes

### TCP
- **Bidirectional**: Yes
- **Connection model**: Ranch acceptors, gen_tcp
- **Limitations**: None
- **Best for**: Network services, high throughput
- **Performance**: 43K msg/s @ 4KB baseline

### HTTP
- **Bidirectional**: Yes (request/response)
- **Connection model**: Gun client, Cowboy server
- **Limitations**: Request overhead per message
- **Best for**: REST APIs, web integrations

### WebSocket
- **Bidirectional**: Yes
- **Connection model**: Gun/Cowboy WebSocket
- **Limitations**: Frame encoding overhead
- **Best for**: Real-time web applications
- **Performance target**: 30K+ msg/s

### SSE (Server-Sent Events)
- **Bidirectional**: No (server → client only)
- **Connection model**: HTTP EventSource
- **Limitations**: Cannot execute tools (requires client → server)
- **Best for**: Server push notifications, monitoring dashboards
- **Performance target**: 20K+ events/s

## Compliance Testing

All transports are validated against MCP specification requirements:

1. **16MB message size limit**: All transports enforce `?MAX_MESSAGE_SIZE = 16777216`
2. **UTF-8 encoding only**: All messages validated for UTF-8 compliance
3. **JSON-RPC 2.0 framing**: All transports use JSON-RPC 2.0 message format
4. **Backpressure handling**: All transports handle flow control gracefully
5. **Lifecycle management**: Clean connection → disconnection state transitions

## Benchmark Results

Run transport benchmarks to measure performance:

```bash
# TCP transport benchmarks
rebar3 ct --suite=erlmcp_bench_tcp

# WebSocket benchmarks
rebar3 ct --suite=erlmcp_bench_websocket

# SSE benchmarks
rebar3 ct --suite=erlmcp_bench_sse

# STDIO benchmarks
rebar3 ct --suite=erlmcp_bench_stdio

# Integration tests (all transports × all capabilities)
rebar3 ct --suite=erlmcp_bench_integration

# Compliance tests (30+ tests across all transports)
rebar3 ct --suite=erlmcp_transport_compliance_SUITE
```

## Performance Baselines

From erlmcp v1.5.0 benchmarks:

| Transport  | Throughput    | Latency p99 | Memory/Conn |
|------------|---------------|-------------|-------------|
| TCP        | 43K msg/s     | 5ms         | 2.5 MiB     |
| WebSocket  | 30K+ msg/s    | 8ms         | 3.0 MiB     |
| SSE        | 20K+ events/s | 10ms        | 2.0 MiB     |
| STDIO      | 10K msg/s     | 15ms        | 1.5 MiB     |
| HTTP       | TBD           | TBD         | TBD         |

## Future Capabilities

### apps
Planned support for application-level MCP integration. Will require:
- Application discovery protocol
- Capability registration
- Event routing

**Timeline**: TBD

### elicitation
Planned support for interactive data gathering workflows. Will require:
- Multi-step interaction protocol
- State management
- Form/schema validation

**Timeline**: TBD

## References

- **MCP Specification**: 2025-11-25 version
- **Transport Behavior**: `apps/erlmcp_transports/src/erlmcp_transport_behavior.erl`
- **Compliance Tests**: `apps/erlmcp_transports/test/erlmcp_transport_compliance_SUITE.erl`
- **Benchmarks**: `apps/erlmcp_transports/src/erlmcp_bench_*.erl`

## Version

Matrix last updated: 2026-01-31
erlmcp version: 2.1.0
