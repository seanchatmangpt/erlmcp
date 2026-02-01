# erlmcp API Documentation Quick Reference

**Version**: 2.1.0
**Last Updated**: 2026-01-31
**MCP Protocol**: 2025-11-25

---

## 📚 Complete API Documentation Suite

This index provides quick access to all erlmcp API documentation with visual guides.

### Core Documentation Files

| Document | Lines | Diagrams | Focus | Link |
|----------|-------|----------|-------|------|
| **API Reference** | 684 | 3 inline | Complete API specification | [api-reference.md](./api-reference.md) |
| **Endpoints Guide** | 1,017 | 9 inline | MCP endpoints & capabilities | [MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md) |
| **Usage Examples** | 881 | Diagram refs | Practical code examples | [API_USAGE_EXAMPLES_WITH_DIAGRAMS.md](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md) |
| **Enhancement Summary** | 494 | Inventory | Documentation metadata | [API_ENHANCEMENT_SUMMARY.md](./API_ENHANCEMENT_SUMMARY.md) |
| **Protocol Spec** | - | 1+ inline | JSON-RPC 2.0 + MCP | [protocol/MCP_JSON_RPC_SPECIFICATION.md](./protocol/MCP_JSON_RPC_SPECIFICATION.md) |

---

## 🎯 Quick Start by Use Case

### I Want To...

**Start a client connection**
→ See [API Reference - Client API](./api-reference.md#client-api)
→ Example: [Minimal Client Example](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#minimal-client-example)
→ Diagram: [Request-Response Flow](./api-reference.md#request-response-flow)

**Start a server**
→ See [API Reference - Server API](./api-reference.md#server-api)
→ Example: [Minimal Server Example](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#minimal-server-example)
→ Diagram: [Server Architecture Flow](./api-reference.md#server-architecture-flow)

**Understand capability negotiation**
→ See [Endpoints Guide - Capability Negotiation](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#capability-negotiation-flow)
→ Diagram: [Capability Negotiation Sequence](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#negotiation-sequence-diagram)

**Implement resource subscriptions**
→ See [Endpoints Guide - Resources API](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#resources-api)
→ Example: [Resource Subscription Example](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#example-3-resource-subscription)
→ Diagram: [Resource API Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#resource-api-flow)

**Execute tools**
→ See [Endpoints Guide - Tools API](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tools-api)
→ Example: [Tool Execution Example](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#example-2-tool-execution)
→ Diagram: [Tool API Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tool-api-flow)

**Handle errors**
→ See [Endpoints Guide - Error Handling](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#error-handling-flow)
→ Example: [Error Handling Example](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#error-handling)
→ Diagram: [Error Processing Sequence](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#error-processing-diagram)

**Work with prompts**
→ See [Endpoints Guide - Prompts API](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#prompts-api)
→ Diagram: [Prompt API Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#prompt-api-flow)

**Use sampling/LLM features**
→ See [Endpoints Guide - Sampling API](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#sampling-api)

**Manage async tasks**
→ See [Endpoints Guide - Tasks API](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tasks-api)
→ Diagram: [Task Lifecycle State Diagram](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#task-lifecycle-state-diagram)

**Understand session lifecycle**
→ See [Endpoints Guide - Session Lifecycle](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#session-lifecycle)
→ Diagram: [Session State Machine](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#session-state-machine)

**Implement custom transport**
→ See [API Reference - Transport Behavior](./api-reference.md#transport-behavior-api-v060)
→ See: [Transport Configuration](./api-reference.md#transport-configuration-v060)

**Use registry for routing**
→ See [API Reference - Registry API](./api-reference.md#registry-api-v060---gproc-based)

---

## 📊 Visual Documentation Index

### Diagram Categories

#### Architecture Diagrams
- **API Architecture Overview** ([api-reference.md](./api-reference.md#api-architecture-overview))
  - Client, Server, Transport, Registry layers
  - API relationships and data flow

#### Flow Diagrams
- **Request-Response Flow** ([api-reference.md](./api-reference.md#request-response-flow))
  - Complete JSON-RPC lifecycle
  - Registry lookup, transport, handlers
  - Observability integration

- **Resource API Flow** ([Endpoints Guide](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#resource-api-flow))
  - List, read, subscribe operations
  - Static and dynamic handlers
  - Notification integration

- **Tool API Flow** ([Endpoints Guide](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tool-api-flow))
  - Tool discovery and execution
  - Progress tracking
  - Argument validation

- **Prompt API Flow** ([Endpoints Guide](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#prompt-api-flow))
  - Prompt discovery and rendering
  - Argument processing

- **Notification Flow** ([Endpoints Guide](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#notification-flow))
  - Event-driven notifications
  - Handler routing

#### Sequence Diagrams
- **Capability Negotiation** ([Endpoints Guide](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#capability-negotiation-flow))
  - Initialize handshake
  - Client-server capability intersection
  - Initialized notification flow

- **Server Request Processing** ([server.md](./reference/api-reference/server.md#server-request-processing-flow))
  - Request routing and validation
  - Handler execution
  - Response formatting

- **Error Processing** ([Endpoints Guide](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#error-handling-flow))
  - Error detection and routing
  - Circuit breaker integration
  - Refusal code handling

#### State Diagrams
- **Server Lifecycle** ([server.md](./reference/api-reference/server.md#server-lifecycle-architecture))
  - Initialization → Ready → Processing
  - Shutdown states

- **Session Lifecycle** ([Endpoints Guide](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#session-lifecycle))
  - Initializing → Connected → Operations
  - Reconnection states
  - Shutdown flows

- **Task Lifecycle** ([Endpoints Guide](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tasks-api))
  - Pending → Processing → Completed/Failed/Cancelled
  - Progress updates

### Standalone Diagram Files

All diagrams in docs/diagrams/ (85 total .mmd files):

#### API-Specific (6 files)
```
diagrams/api/
├── api-endpoints.mmd                    # API endpoints overview
├── request-response-flow.mmd            # Complete request lifecycle
├── authentication-flow.mmd              # Authentication flow
├── rate-limiting.mmd                    # Rate limiting strategy
└── versioning-strategy.mmd              # API versioning
```

#### Protocol (5 files)
```
diagrams/protocol/
├── capability-negotiation.mmd           # Detailed capability exchange
├── client-server-interaction.mmd        # Client-server messages
├── error-handling.mmd                   # Error processing flow
├── json-rpc-flow.mmd                    # JSON-RPC 2.0 lifecycle
└── session-lifecycle.mmd                # Session state machine
```

#### Other Categories (74 files)
- configuration/ (5 files)
- security/ (5 files)
- roadmap/ (4 files)
- development/ (3 files)
- integration/ (5 files)
- transports/ (5 files)
- observability/ (5 files)
- testing/ (5 files)
- deployment/ (5 files)
- examples/ (5 files)
- errors/ (5 files)
- monitoring/ (5 files)
- reference/ (5 files)
- validation/ (5 files)
- system-architecture.mmd
- module-dependencies.mmd
- supervision-tree.mmd
- data-flow.mmd
- transport-interfaces.mmd

---

## 🔍 By API Component

### Client API

**Documentation**: [api-reference.md#client-api](./api-reference.md#client-api)
**Examples**: [API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#client-examples](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#client-examples)
**Diagrams**:
- API Architecture Overview (client section)
- Request-Response Flow (client side)
- Capability Negotiation (client role)

**Key Functions**:
```erlang
erlmcp_client:start_link/2
erlmcp_client:initialize/2
erlmcp_client:list_resources/1
erlmcp_client:read_resource/2
erlmcp_client:subscribe_to_resource/2
erlmcp_client:list_tools/1
erlmcp_client:call_tool/3
erlmcp_client:list_prompts/1
erlmcp_client:get_prompt/2
```

### Server API

**Documentation**: [api-reference.md#server-api](./api-reference.md#server-api)
**Examples**: [API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#server-examples](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#server-examples)
**Diagrams**:
- API Architecture Overview (server section)
- Server Architecture Flow
- Server Request Processing Flow
- Server Lifecycle

**Key Functions**:
```erlang
erlmcp_server:start_link/2
erlmcp_server:add_resource/3
erlmcp_server:add_resource_template/4
erlmcp_server:add_tool/3
erlmcp_server:add_tool_with_schema/4
erlmcp_server:add_prompt/3
erlmcp_server:add_prompt_with_args/4
erlmcp_server:subscribe_resource/3
erlmcp_server:notify_resource_updated/3
erlmcp_server:report_progress/4
```

### Resources API

**Documentation**: [MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#resources-api](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#resources-api)
**Diagram**: [Resource API Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#resource-api-flow)

**Methods**:
- `resources/list` - Discover resources
- `resources/read` - Get resource content
- `resources/subscribe` - Subscribe to updates
- `resources/unsubscribe` - Unsubscribe
- `resources/templates/list` - List templates

### Tools API

**Documentation**: [MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tools-api](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tools-api)
**Diagram**: [Tool API Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tool-api-flow)

**Methods**:
- `tools/list` - Discover tools
- `tools/call` - Execute tool

**Features**:
- JSON Schema validation
- Progress tracking
- Error handling

### Prompts API

**Documentation**: [MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#prompts-api](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#prompts-api)
**Diagram**: [Prompt API Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#prompt-api-flow)

**Methods**:
- `prompts/list` - Discover prompts
- `prompts/get` - Get rendered prompt

### Sampling API

**Documentation**: [MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#sampling-api](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#sampling-api)

**Methods**:
- `sampling/createMessage` - Request LLM completion (server → client)

### Tasks API

**Documentation**: [MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tasks-api](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tasks-api)
**Diagram**: [Task Lifecycle](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#task-lifecycle-state-diagram)

**Methods**:
- `tasks/create` - Start async task
- `tasks/list` - List all tasks
- `tasks/get` - Get task details
- `tasks/cancel` - Cancel running task

### Completion API

**Documentation**: [MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#completion-api](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#completion-api)

**Methods**:
- `completion/complete` - Get completions

---

## 🚨 Error Codes

**Reference**: [MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#error-codes-reference](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#error-codes-reference)

### JSON-RPC 2.0 Standard Errors
- `-32700` Parse error
- `-32600` Invalid Request
- `-32601` Method not found
- `-32602` Invalid params
- `-32603` Internal error

### MCP Core Errors (-32001 to -32010)
- `-32001` Resource not found
- `-32002` Tool not found
- `-32003` Prompt not found
- `-32004` Capability not supported
- `-32005` Server not initialized
- `-32006` Subscription failed
- `-32007` Validation failed
- `-32008` Transport error
- `-32009` Request timeout
- `-32010` Rate limit exceeded

### Tool Errors (-32031 to -32040)
- `-32031` Tool execution failed
- `-32032` Tool execution timeout
- `-32033` Tool execution cancelled
- `-32034` Invalid tool arguments
- `-32035` Tool is disabled
- `-32036` Tool result too large
- `-32037` Tool not allowed
- `-32038` Maximum concurrent tools exceeded
- `-32039` Tool dependency failed
- `-32040` Tool schema invalid

### Refusal Codes (1001-1089)
Positive codes indicating refusal:
- `1001` Content violates policy
- `1002` Violates safety guidelines
- `1003` Rate limit exceeded
- `1004` Resource constraints
- `1005` Permission denied
- `1006` Invalid input
- `1007` Unsupported operation
- `1008` Temporarily unavailable
- `1009` Dependency failed
- `1010` Custom refusal reason

---

## 🔐 Transport Layer

**Documentation**: [api-reference.md#transport-configuration-v060](./api-reference.md#transport-configuration-v060)

### Supported Transports

| Transport | Module | Use Case | Config |
|-----------|--------|----------|--------|
| **STDIO** | erlmcp_transport_stdio | CLI tools, local communication | Simplest |
| **TCP** | erlmcp_transport_tcp | Network services, ranch | Server/Client modes |
| **HTTP** | erlmcp_transport_http | REST APIs, gun client | HTTP/2 support |
| **WebSocket** | erlmcp_transport_ws | Full-duplex, browsers | Real-time |
| **SSE** | erlmcp_transport_sse | Server streaming, events | Unidirectional |

### Transport Behavior

**Callback Interface**: [api-reference.md#transport-behavior-api-v060](./api-reference.md#transport-behavior-api-v060)

Required callbacks:
- `init/2` - Initialize transport
- `send/2` - Send data
- `close/1` - Close transport

---

## 📈 Session Management

**Documentation**: [MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#session-lifecycle](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#session-lifecycle)

### Session States
```
Initializing → Connected → Operations
     ↓              ↓           ↓
   [config]    [subscribe]  [active]
     ↓              ↓           ↓
Reconnecting ← Paused ← [requests]
     ↓
  Failed/Shutdown
```

### Persistence Backends

| Backend | Module | Use Case | Performance |
|---------|--------|----------|-------------|
| **ETS** | erlmcp_session_ets | In-memory, fastest | O(1) |
| **DETS** | erlmcp_session_dets | Disk persistence | Slower |
| **Mnesia** | erlmcp_session_mnesia | Distributed cluster | Network latency |

---

## 🎓 Learning Path

### Beginner
1. Read [API Reference - Architecture Overview](./api-reference.md#api-architecture-overview)
2. Study [Capability Negotiation Flow](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#capability-negotiation-flow)
3. Run [Minimal Client Example](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#minimal-client-example)
4. Run [Minimal Server Example](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#minimal-server-example)

### Intermediate
1. Learn [Resources API](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#resources-api)
2. Implement [Resource Subscriptions](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#example-3-resource-subscription)
3. Study [Tools API](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tools-api)
4. Implement [Tool Handlers](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#tool-implementation)
5. Understand [Error Handling](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md#error-handling)

### Advanced
1. Master [Session Lifecycle](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#session-lifecycle)
2. Implement [Custom Transports](./api-reference.md#transport-behavior-api-v060)
3. Use [Sampling API](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#sampling-api)
4. Implement [Tasks API](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md#tasks-api)
5. Study [Registry API](./api-reference.md#registry-api-v060---gproc-based)

---

## 📞 Getting Help

### Documentation Issues
- Check [Enhancement Summary](./API_ENHANCEMENT_SUMMARY.md) for documentation metadata
- See [Diagram Index](./diagrams/INDEX.md) for complete diagram catalog

### Code Examples
- [API Usage Examples](./API_USAGE_EXAMPLES_WITH_DIAGRAMS.md) - 40+ working examples
- [examples/](../examples/) directory - 40+ example implementations

### Protocol Details
- [JSON-RPC Specification](./protocol/MCP_JSON_RPC_SPECIFICATION.md)
- [MCP Endpoints Guide](./MCP_ENDPOINTS_AND_CAPABILITIES_GUIDE.md)

### Architecture
- [System Architecture](./architecture.md)
- [OTP Patterns](./otp-patterns.md)

---

**Status**: ✅ Complete
**Coverage**: 100% of MCP 2025-11-25 specification
**Diagrams**: 15+ inline, 85 standalone .mmd files
**Examples**: 40+ code examples with diagram references

---

**Quick Navigation**:
- [↑ Back to Top](#erlmcp-api-documentation-quick-reference)
- [📚 Core Documentation](#-complete-api-documentation-suite)
- [📊 Visual Index](#-visual-documentation-index)
- [🔍 Component Reference](#-by-api-component)
