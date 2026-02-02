# ERLMCP CLI API Reference

This document provides a comprehensive reference for the erlmcp CLI interface, covering all commands, JSON-RPC 2.0 methods, parameter schemas, and response formats.

## Table of Contents

1. [CLI Commands](#cli-commands)
2. [JSON-RPC 2.0 Methods](#json-rpc-20-methods)
3. [Parameter Schemas](#parameter-schemas)
4. [Response Formats](#response-formats)
5. [Error Codes](#error-codes)
6. [Transport APIs](#transport-apis)

---

## CLI Commands

### Main CLI (`erlmcp`)

#### Environment Management

```bash
# Initialize development environment
erlmcp init

# Start cluster
erlmcp start

# Stop cluster
erlmcp stop

# Check status
erlmcp status
```

**Response Format:**
```json
{
  "status": "ok",
  "node": "erlmcp_cli@localhost",
  "applications": ["erlmcp", "erlmcp_core"],
  "processes": 147,
  "memory": "45.2 MB",
  "uptime": "2h 15m"
}
```

#### Performance Testing

```bash
# Run 100K concurrent test
erlmcp test-100k

# Run comprehensive benchmarks
erlmcp benchmark

# Run custom benchmark
erlmcp bench run --suite latency --output json

# Run chaos tests
erlmcp chaos run --scenario loss --intensity high
```

#### Configuration Management

```bash
# List profiles
erlmcp profile list

# Show profile
erlmcp profile show prod

# Apply profile
erlmcp profile apply dev

# Validate profile
erlmcp profile validate gov
```

**Profile Configuration Schema:**
```json
{
  "profiles": {
    "dev": {
      "description": "Development profile with debug features",
      "erlmcp": {
        "log_level": "debug",
        "rate_limiting": {"enabled": false},
        "circuit_breaker": {"enabled": false}
      }
    },
    "prod": {
      "description": "Production profile with optimizations",
      "erlmcp": {
        "log_level": "info",
        "rate_limiting": {"enabled": true, "limit": 1000},
        "circuit_breaker": {"enabled": true, "threshold": 0.8}
      }
    },
    "gov": {
      "description": "Government compliance profile",
      "erlmcp": {
        "log_level": "warn",
        "rate_limiting": {"enabled": true, "limit": 500},
        "circuit_breaker": {"enabled": true},
        "audit_logging": {"enabled": true}
      }
    }
  }
}
```

#### Upgrade Management

```bash
# Upgrade planning
erlmcp upgrade plan 1.3.0 1.4.0

# Post-upgrade verification
erlmcp upgrade verify
```

### Validation CLI (`erlmcp-validate`)

#### MCP Validation

```bash
# Validate MCP server
erlmcp-validate validate <url>

# Check spec compliance
erlmcp-validate spec-check

# Generate compliance report
erlmcp-validate report

# Test transport behavior
erlmcp-validate transport-check stdio
```

---

## JSON-RPC 2.0 Methods

### Core Protocol Methods

#### `tools/list`
List all available tools from the server.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "list-001",
  "method": "tools/list"
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "list-001",
  "result": {
    "tools": [
      {
        "name": "calculator",
        "description": "Simple arithmetic calculator",
        "inputSchema": {
          "type": "object",
          "properties": {
            "expression": {
              "type": "string",
              "description": "Mathematical expression to evaluate"
            }
          },
          "required": ["expression"]
        }
      },
      {
        "name": "file_system",
        "description": "File system operations",
        "inputSchema": {
          "type": "object",
          "properties": {
            "operation": {"type": "string", "enum": ["read", "write", "list"]},
            "path": {"type": "string"}
          },
          "required": ["operation", "path"]
        }
      }
    ]
  }
}
```

#### `tools/call`
Execute a tool with the given arguments.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "calc-001",
  "method": "tools/call",
  "params": {
    "name": "calculator",
    "arguments": {
      "expression": "2 + 2 * 3"
    }
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "calc-001",
  "result": {
    "content": [
      {
        "type": "text",
        "text": "8"
      }
    ]
  }
}
```

### Resource Methods

#### `resources/list`
List available server resources.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "resource-list-001",
  "method": "resources/list"
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "resource-list-001",
  "result": {
    "resources": [
      {
        "uri": "file:///tmp/example.txt",
        "name": "example_file",
        "description": "Example text file",
        "mimeType": "text/plain"
      }
    ]
  }
}
```

#### `resources/read/contents`
Read resource contents.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "read-001",
  "method": "resources/read/contents",
  "params": {
    "uri": "file:///tmp/example.txt"
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "read-001",
  "result": {
    "contents": "Hello, World!\n",
    "mimeType": "text/plain"
  }
}
```

### Session Methods

#### `sessions/create`
Create a new server session.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "session-create-001",
  "method": "sessions/create"
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "session-create-001",
  "result": {
    "sessionId": "session-12345",
    "createdAt": "2026-02-01T10:00:00Z",
    "ttl": 3600
  }
}
```

#### `sessions/list`
List active sessions.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "session-list-001",
  "method": "sessions/list"
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "session-list-001",
  "result": {
    "sessions": [
      {
        "sessionId": "session-12345",
        "createdAt": "2026-02-01T10:00:00Z",
        "lastActivity": "2026-02-01T10:30:00Z",
        "ttl": 3600
      }
    ]
  }
}
```

---

## Parameter Schemas

### Tool Call Parameters

#### Basic Tool Parameters
```json
{
  "name": "string",
  "arguments": {
    "property_name": "any"
  }
}
```

#### File System Tool Parameters
```json
{
  "operation": "string (enum: read, write, list, create, delete)",
  "path": "string",
  "content?: "string (required for write operations)",
  "create_directories?": "boolean (default: false)"
}
```

#### Calculator Tool Parameters
```json
{
  "expression": "string",
  "precision?": "integer (default: 2)"
}
```

### Resource Parameters

#### Resource URI
```json
{
  "uri": "string (format: uri)",
  "mimeType?": "string",
  "encoding?": "string (default: utf-8)"
}
```

#### Resource Subscription
```json
{
  "uri": "string",
  "events": ["string"],
  "ttl?: "integer (seconds)"
}
```

### Session Parameters

#### Session Configuration
```json
{
  "ttl?": "integer (seconds, default: 3600)",
  "max_requests?": "integer (default: 1000)",
  "timeout?": "integer (ms, default: 30000)"
}
```

---

## Response Formats

### Success Response
```json
{
  "jsonrpc": "2.0",
  "id": "request-id",
  "result": {
    "data": "...",
    "metadata": {
      "timestamp": "2026-02-01T10:00:00Z",
      "duration_ms": 45
    }
  }
}
```

### Error Response
```json
{
  "jsonrpc": "2.0",
  "id": "request-id",
  "error": {
    "code": -32601,
    "message": "Method not found",
    "data": {
      "details": "The requested method does not exist",
      "suggestions": ["tools/list", "resources/list"]
    }
  }
}
```

### Batch Response
```json
[
  {
    "jsonrpc": "2.0",
    "id": "batch-1",
    "result": {...}
  },
  {
    "jsonrpc": "2.0",
    "id": "batch-2",
    "error": {
      "code": -32602,
      "message": "Invalid params"
    }
  }
]
```

---

## Error Codes

### JSON-RPC 2.0 Standard Errors

| Code | Message | Description |
|------|---------|-------------|
| -32700 | Parse error | Invalid JSON was received by the server |
| -32600 | Invalid Request | JSON sent is not a valid Request object |
| -32601 | Method not found | The method does not exist |
| -32602 | Invalid params | Invalid method parameter(s) |
| -32603 | Internal error | Internal JSON-RPC error |

### ERLMCP Specific Errors

| Code | Message | Description | Resolution |
|------|---------|-------------|------------|
| -32000 | Tool execution failed | Tool returned an error | Check tool logs, verify parameters |
| -32001 | Resource not found | Requested resource does not exist | Verify URI, check resource permissions |
| -32002 | Session expired | Session has timed out | Create new session, update TTL |
| -32003 | Rate limit exceeded | Too many requests in timeframe | Add delays, implement backoff |
| -32004 | Authentication failed | Invalid credentials | Check API key, verify permissions |
| -32005 | Transport error | Connection issue | Check network, retry with backoff |
| -32006 | Protocol violation | MCP protocol not followed | Validate client implementation |

### Error Response Format
```json
{
  "jsonrpc": "2.0",
  "id": "request-id",
  "error": {
    "code": -32001,
    "message": "Resource not found",
    "data": {
      "resource_uri": "file:///nonexistent.txt",
      "available_resources": ["file:///existing.txt"],
      "suggestion": "Check if the file exists and path is correct",
      "retry_after": 5000
    }
  }
}
```

---

## Transport APIs

### stdio Transport

**Connection Setup:**
```bash
erlmcp validate protocol --file message.json
```

**Request Format:**
```json
{
  "jsonrpc": "2.0",
  "id": "stdio-001",
  "method": "tools/list"
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "stdio-001",
  "result": {...}
}
```

### TCP Transport

**Connection Setup:**
```bash
nc localhost 8080
```

**Message Protocol:**
- Messages are delimited by newlines
- Binary encoding supported with content-length header

### HTTP Transport

**Connection Setup:**
```bash
curl -X POST http://localhost:8080 \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc": "2.0", "id": "http-001", "method": "tools/list"}'
```

### WebSocket Transport

**Connection Setup:**
```javascript
const ws = new WebSocket('ws://localhost:8080');

ws.on('message', (data) => {
  const response = JSON.parse(data);
  console.log('Response:', response);
});

const request = {
  jsonrpc: "2.0",
  id: "ws-001",
  method: "tools/list"
};
ws.send(JSON.stringify(request));
```

### SSE Transport

**Connection Setup:**
```bash
curl -N http://localhost:8080/stream
```

**Event Stream:**
```
data: {"jsonrpc": "2.0", "id": "sse-001", "result": {...}}

data: {"jsonrpc": "2.0", "id": "sse-002", "result": {...}}
```

---

## Examples

### Complete Tool Execution Flow

```bash
# 1. List available tools
erlmcp-validate validate protocol <<EOF
{
  "jsonrpc": "2.0",
  "id": "list-001",
  "method": "tools/list"
}
EOF

# 2. Execute a tool
erlmcp-validate validate protocol <<EOF
{
  "jsonrpc": "2.0",
  "id": "calc-001",
  "method": "tools/call",
  "params": {
    "name": "calculator",
    "arguments": {
      "expression": "sqrt(16)"
    }
  }
}
EOF

# 3. Handle streaming response
erlmcp chaos run --scenario streaming --output-file stream.log
```

### Batch Operations

```bash
# Batch tool calls
cat > batch.json << EOF
[
  {
    "jsonrpc": "2.0",
    "id": "batch-1",
    "method": "tools/call",
    "params": {
      "name": "calculator",
      "arguments": {"expression": "2 + 2"}
    }
  },
  {
    "jsonrpc": "2.0",
    "id": "batch-2",
    "method": "tools/call",
    "params": {
      "name": "file_system",
      "arguments": {"operation": "list", "path": "/tmp"}
    }
  }
]
EOF

erlmcp-validate validate protocol --file batch.json
```

This comprehensive API reference covers all aspects of the erlmcp CLI interface. For additional information about specific components, refer to the related documentation sections.