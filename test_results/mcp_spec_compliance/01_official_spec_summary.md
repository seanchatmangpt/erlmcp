# MCP 2025-11-25 Official Specification Summary

## Overview
Model Context Protocol (MCP) is an open protocol that enables seamless integration between LLM applications and external data sources and tools. The current version (2025-11-25) uses JSON-RPC 2.0 messages for standardized communication between hosts and servers.

## Protocol Architecture

### Communication Roles
- **Hosts**: LLM applications that initiate connections
- **Clients**: Connectors within the host application
- **Servers**: Services that provide context and capabilities

### Base Protocol Requirements
- JSON-RPC 2.0 message format
- Stateful connections
- Server and client capability negotiation
- All implementations MUST support base protocol and lifecycle management

## Core Methods (7 Required)

### 1. Initialize
**Method**: `initialize`
**Purpose**: Establish connection and negotiate capabilities
**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-11-25",
    "capabilities": {
      "resources": {
        "subscribe": true,
        "listChanged": true
      },
      "tools": {
        "listChanged": true
      },
      "prompts": {},
      "logging": {}
    },
    "clientInfo": {
      "name": "client-name",
      "version": "1.0.0"
    }
  }
}
```
**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2025-11-25",
    "capabilities": {
      "resources": {
        "subscribe": true,
        "listChanged": true
      },
      "tools": {
        "listChanged": true
      },
      "tasks": {
        "list": {},
        "cancel": {},
        "requests": {
          "tools": {
            "call": {}
          }
        }
      }
    },
    "serverInfo": {
      "name": "server-name",
      "version": "1.0.0",
      "description": "Optional human-readable description"
    }
  }
}
```

### 2. Ping
**Method**: `ping`
**Purpose**: Test connection liveliness
**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "ping"
}
```
**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "type": "string",
    "text": "pong"
  }
}
```

### 3. Tools List
**Method**: `tools/list`
**Purpose**: Discover available tools from server
**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/list",
  "params": {
    "cursor": "optional-cursor-value"
  }
}
```
**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "tools": [
      {
        "name": "get_weather",
        "title": "Weather Information Provider",
        "description": "Get current weather information for a location",
        "inputSchema": {
          "type": "object",
          "properties": {
            "location": {
              "type": "string",
              "description": "City name or zip code"
            }
          },
          "required": ["location"]
        },
        "outputSchema": {
          "type": "object",
          "properties": {
            "temperature": {"type": "number"},
            "conditions": {"type": "string"}
          }
        },
        "icons": [
          {
            "src": "https://example.com/weather-icon.png",
            "mimeType": "image/png",
            "sizes": ["48x48"]
          }
        ],
        "annotations": {
          "audience": ["user", "assistant"],
          "priority": 0.8
        }
      }
    ],
    "nextCursor": "next-page-cursor"
  }
}
```

### 4. Tools Call
**Method**: `tools/call`
**Purpose**: Invoke a tool function
**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/call",
  "params": {
    "name": "get_weather",
    "arguments": {
      "location": "New York"
    },
    "task": {
      "ttl": 60000
    }
  }
}
```
**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Current weather in New York:\nTemperature: 72°F\nConditions: Partly cloudy"
      },
      {
        "type": "image",
        "data": "base64-encoded-data",
        "mimeType": "image/png"
      },
      {
        "type": "resource_link",
        "uri": "file:///project/weather.json",
        "name": "weather.json",
        "description": "Weather data file"
      }
    ],
    "structuredContent": {
      "temperature": 72,
      "conditions": "Partly cloudy"
    },
    "isError": false,
    "_meta": {
      "io.modelcontextprotocol/related-task": {
        "taskId": "786512e2-9e0d-44bd-8f29-789f320fe840"
      }
    }
  }
}
```

### 5. Resources List
**Method**: `resources/list`
**Purpose**: Discover available resources
**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "resources/list",
  "params": {
    "cursor": "optional-cursor-value"
  }
}
```
**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "resources": [
      {
        "uri": "file:///project/src/main.rs",
        "name": "main.rs",
        "title": "Rust Software Application Main File",
        "description": "Primary application entry point",
        "mimeType": "text/x-rust",
        "size": 2048,
        "icons": [
          {
            "src": "https://example.com/rust-file-icon.png",
            "mimeType": "image/png",
            "sizes": ["48x48"]
          }
        ],
        "annotations": {
          "audience": ["user"],
          "priority": 0.9,
          "lastModified": "2025-05-03T14:30:00Z"
        }
      }
    ],
    "nextCursor": "next-page-cursor"
  }
}
```

### 6. Resources Read
**Method**: `resources/read`
**Purpose**: Retrieve resource contents
**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "resources/read",
  "params": {
    "uri": "file:///project/src/main.rs"
  }
}
```
**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "contents": [
      {
        "uri": "file:///project/src/main.rs",
        "mimeType": "text/x-rust",
        "text": "fn main() {\n    println!(\"Hello world!\");\n}",
        "annotations": {
          "audience": ["user", "assistant"],
          "priority": 0.8
        }
      }
    ]
  }
}
```

### 7. Prompts List
**Method**: `prompts/list`
**Purpose**: Discover available prompt templates
**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "prompts/list",
  "params": {
    "cursor": "optional-cursor-value"
  }
}
```
**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "prompts": [
      {
        "name": "code_review",
        "title": "Code Review Assistant",
        "description": "Generate a comprehensive code review",
        "arguments": {
          "type": "object",
          "properties": {
            "code": {"type": "string"},
            "language": {"type": "string"}
          }
        }
      }
    ],
    "nextCursor": "next-page-cursor"
  }
}
```

## Experimental Features

### Tasks API (Experimental)
Introduced in 2025-11-25 for async operations:

#### Task Support Declaration
```json
{
  "capabilities": {
    "tasks": {
      "list": {},
      "cancel": {},
      "requests": {
        "tools": {
          "call": {}
        }
      }
    }
  }
}
```

#### Task Creation
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "get_weather",
    "arguments": {
      "city": "New York"
    },
    "task": {
      "ttl": 60000
    }
  }
}
```

#### Task Status Response
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "task": {
      "taskId": "786512e2-9e0d-44bd-8f29-789f320fe840",
      "status": "working",
      "statusMessage": "The operation is now in progress.",
      "createdAt": "2025-11-25T10:30:00Z",
      "lastUpdatedAt": "2025-11-25T10:40:00Z",
      "ttl": 60000,
      "pollInterval": 5000
    }
  }
}
```

#### Task Operations
- `tasks/get` - Poll task status
- `tasks/result` - Retrieve final result
- `tasks/list` - List all tasks
- `tasks/cancel` - Cancel a task

### Completion API
For handling completion requests with tool support:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "completion/create",
  "params": {
    "conversation": [...],
    "tools": {
      "list": true,
      "choice": "auto"
    },
    "toolChoice": {
      "type": "specific",
      "name": "search_database"
    }
  }
}
```

### Elicitation API
For interactive user input:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "elicitation/create",
  "params": {
    "conversation": [...],
    "request": "Please provide your API key",
    "schema": {
      "type": "string",
      "format": "password",
      "default": "default_key"
    }
  }
}
```

### Sampling API
For server-initiated LLM interactions:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "sampling/createMessage",
  "params": {
    "conversation": [...],
    "toolChoice": "auto",
    "tools": ["search", "calculate"]
  }
}
```

## Error Code Taxonomy

### JSON-RPC Error Codes (-32700 to -32603)
- **-32700**: Parse Error - JSON parse error
- **-32600**: Invalid Request - JSON-RPC request invalid
- **-32601**: Method Not Found - Method not found
- **-32602**: Invalid Params - Invalid parameters
- **-32603**: Internal Error - Internal JSON-RPC error

### MCP Refusal Codes (1001-1089)
- **1001-1099**: Reserved for future use
- **Custom error codes** for business logic failures

### Tool Execution Errors
- Returned in tool results with `isError: true`
- Enable model self-correction
- Example: Input validation errors

### Protocol Errors
- Standard JSON-RPC errors for protocol issues
- Example: Unknown tools, malformed requests

## Transport Requirements

### Supported Transports
1. **STDIO**
   - Bidirectional pipes
   - Used by default
   - No HTTP authorization required

2. **TCP**
   - Raw TCP sockets
   - Uses Ranch acceptor pool
   - Custom authentication possible

3. **HTTP**
   - HTTP/1.1 and HTTP/2
   - Uses Cowboy/Gun servers
   - OAuth 2.0 support
   - CORS support

4. **WebSocket**
   - Full-duplex communication
   - Real-time updates
   - Backward compatibility

5. **Server-Sent Events (SSE)**
   - Unidirectional server-to-client
   - Streaming updates
   - Support for polling

### Transport Behavior Interface
All transports MUST implement `erlmcp_transport` behavior:
```erlang
-callback init(TransportType, Options) -> {ok, State}.
-callback send(Data, State) -> {ok, NewState} | {error, Reason}.
-callback close(State) -> ok.
```

### Transport Capability Requirements
- **HTTP transports**: MUST support OAuth 2.0
- **STDIO transports**: MUST validate input sizes
- **All transports**: MUST support connection health monitoring
- **All transports**: MUST implement proper error handling

## Capability Negotiation

### Server Capabilities Declaration
```json
{
  "capabilities": {
    "resources": {
      "subscribe": true,
      "listChanged": true
    },
    "tools": {
      "listChanged": true
    },
    "prompts": {},
    "logging": {},
    "tasks": {
      "list": {},
      "cancel": {},
      "requests": {
        "tools": {
          "call": {}
        }
      }
    },
    "sampling": {},
    "elicitation": {}
  }
}
```

### Client Capabilities Declaration
```json
{
  "capabilities": {
    "resources": {
      "subscribe": true
    },
    "tools": {},
    "prompts": {},
    "logging": {},
    "tasks": {
      "list": {},
      "cancel": {},
      "requests": {
        "sampling": {
          "createMessage": {}
        },
        "elicitation": {
          "create": {}
        }
      }
    },
    "roots": {},
    "elicitation": {},
    "sampling": {}
  }
}
```

## JSON Schema Requirements

### Schema Dialect Support
- **Default**: JSON Schema 2020-12 (when `$schema` not specified)
- **Explicit**: Any supported dialect via `$schema` field
- **Required**: MUST support at least JSON Schema 2020-12

### Tool Schema Examples
```json
{
  "name": "calculate_sum",
  "inputSchema": {
    "type": "object",
    "properties": {
      "a": {"type": "number"},
      "b": {"type": "number"}
    },
    "required": ["a", "b"]
  }
}
```

### Resource Schema Examples
```json
{
  "uri": "file:///example.txt",
  "mimeType": "text/plain",
  "text": "Resource content"
}
```

## Security and Trust & Safety

### Key Principles
1. **User Consent and Control**
   - Explicit consent required for all operations
   - User retains control over data access
   - Clear UIs for authorization

2. **Data Privacy**
   - User data protection required
   - No transmission without consent
   - Access controls mandatory

3. **Tool Safety**
   - Tools represent code execution
   - User consent required for invocation
   - Descriptions treated as untrusted

4. **LLM Sampling Controls**
   - User approval for sampling requests
   - Control over prompts and results

### Implementation Guidelines
- Build robust consent flows
- Provide clear documentation
- Implement access controls
- Follow security best practices
- Consider privacy implications

## Metadata and Annotations

### Reserved `_meta` Fields
- Format: `optional-prefix/name`
- Reserved prefixes: `io.modelcontextprotocol/`, `dev.mcp/`
- Client-specific metadata allowed

### Resource Annotations
```json
{
  "annotations": {
    "audience": ["user", "assistant"],
    "priority": 0.8,
    "lastModified": "2025-01-12T15:00:58Z"
  }
}
```

### Icons Support
```json
{
  "icons": [
    {
      "src": "https://example.com/icon.png",
      "mimeType": "image/png",
      "sizes": ["48x48", "96x96"],
      "theme": "light"
    }
  ]
}
```

## Experimental Features Matrix

| Feature | Status | Support Level | Description |
|---------|--------|---------------|-------------|
| Tasks API | Experimental | Server & Client | Async operations with polling |
| Completion API | Experimental | Server | LLM completion with tool support |
| Elicitation API | Experimental | Server | Interactive user input |
| Sampling API | Experimental | Server | Server-initiated LLM interactions |
| Resource Templates | Stable | Server | Parameterized resource access |
| Progress Tracking | Stable | Server | Long-running operation progress |
| Cancellation | Stable | Server | Request cancellation support |
| Logging | Stable | Server & Client | Structured logging support |

## Version Control

- **Current Version**: 2025-11-25
- **Version Format**: YYYY-MM-DD (date of last breaking change)
- **Backward Compatibility**: Non-breaking changes allowed in current version
- **Version Negotiation**: During initialization phase
- **Error Handling**: Graceful termination for version mismatches

## Compliance Checklist

### Required Implementation
- [ ] JSON-RPC 2.0 message format
- [ ] All 7 core methods implemented
- [ ] Capability negotiation during initialization
- [ ] Error handling with proper codes
- [ ] Transport behavior compliance
- [ ] Security and consent mechanisms

### Experimental Implementation (Optional)
- [ ] Tasks API support
- [ ] Completion API with tool choice
- [ ] Elicitation for user input
- [ ] Sampling for server-initiated requests
- [ ] Resource templates
- [ ] Progress tracking
- [ ] Cancellation support

### Quality Gates
- 0 compilation errors
- 100% test pass rate
- ≥80% code coverage
- Proper error handling
- Security validation
- Performance benchmarks

---

*This summary is based on the official MCP 2025-11-25 specification from modelcontextprotocol.io*