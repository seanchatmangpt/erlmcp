# erlmcp JSON-RPC Protocol Reference

## Overview

erlmcp implements the Model Context Protocol (MCP) specification version 2025-11-25, using JSON-RPC 2.0 as the transport protocol. All messages between client and server follow the JSON-RPC 2.0 specification.

### JSON-RPC 2.0 Message Format

All JSON-RPC messages share a common structure:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "method/name",
  "params": {}
}
```

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `jsonrpc` | string | Yes | Must be "2.0" |
| `id` | integer/string | Request only | Request identifier |
| `method` | string | Request/Notification | Method name |
| `params` | object/array | Optional | Method parameters |
| `result` | any | Response only | Success result |
| `error` | object | Response only | Error object |

### Message Types

1. **Request**: Has `id` and `method`, expects response
2. **Response**: Has `id` and either `result` or `error`
3. **Notification**: Has `method` but NO `id`, no response expected
4. **Batch**: Array of requests/responses

---

## Core Protocol Methods

### initialize

The first message sent by the client. Establishes the protocol version and exchanges capabilities.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-11-25",
    "capabilities": {
      "roots": {},
      "sampling": {
        "modelPreferences": {}
      }
    },
    "clientInfo": {
      "name": "erlmcp-client",
      "version": "3.0.0"
    }
  }
}
```

**Response:**
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
      "prompts": {}
    },
    "serverInfo": {
      "name": "erlmcp",
      "version": "3.0.0"
    }
  }
}
```

### notifications/initialized

Sent by the server after `initialize` completes. Signals that the server is ready to process requests.

**Notification:**
```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized"
}
```

### ping

Simple liveness check. Returns empty object on success.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "ping"
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {}
}
```

---

## Resource Methods

### resources/list

Lists all available resources.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "resources/list"
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "resources": [
      {
        "uri": "mcp://config",
        "name": "Configuration",
        "description": "Server configuration",
        "mimeType": "application/json"
      },
      {
        "uri": "file:///logs/app.log",
        "name": "Application Logs",
        "mimeType": "text/plain"
      }
    ]
  }
}
```

### resources/read

Reads the contents of a specific resource.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "resources/read",
  "params": {
    "uri": "mcp://config"
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "result": {
    "contents": [
      {
        "uri": "mcp://config",
        "type": "text",
        "text": "{\"version\": \"3.0.0\", \"features\": [\"resources\", \"tools\"]}"
      }
    ]
  }
}
```

### resources/templates/list

Lists resource URI templates for dynamic resource access.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "resources/templates/list"
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "result": {
    "resourceTemplates": [
      {
        "uriTemplate": "logs://{date}",
        "name": "Daily Logs",
        "description": "Access logs by date",
        "mimeType": "text/plain"
      }
    ]
  }
}
```

### resources/subscribe

Subscribe to change notifications for a resource.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "resources/subscribe",
  "params": {
    "uri": "mcp://config"
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "result": {}
}
```

### resources/unsubscribe

Unsubscribe from resource change notifications.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "method": "resources/unsubscribe",
  "params": {
    "uri": "mcp://config"
  }
}
```

---

## Tool Methods

### tools/list

Lists all available tools.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "method": "tools/list"
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "result": {
    "tools": [
      {
        "name": "calculate",
        "description": "Perform arithmetic operations",
        "inputSchema": {
          "type": "object",
          "properties": {
            "a": {"type": "number"},
            "b": {"type": "number"},
            "op": {
              "type": "string",
              "enum": ["add", "subtract", "multiply", "divide"]
            }
          },
          "required": ["a", "b", "op"]
        }
      }
    ]
  }
}
```

### tools/call

Invokes a tool with the provided arguments.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 9,
  "method": "tools/call",
  "params": {
    "name": "calculate",
    "arguments": {
      "a": 10,
      "b": 5,
      "op": "multiply"
    }
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 9,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "50"
      }
    ]
  }
}
```

**With progress token:**
```json
{
  "jsonrpc": "2.0",
  "id": 10,
  "method": "tools/call",
  "params": {
    "name": "long_running_task",
    "arguments": {},
    "_progressToken": "progress-123"
  }
}
```

---

## Prompt Methods

### prompts/list

Lists all available prompt templates.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 11,
  "method": "prompts/list"
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 11,
  "result": {
    "prompts": [
      {
        "name": "write_essay",
        "description": "Generate an essay writing prompt",
        "arguments": [
          {
            "name": "topic",
            "description": "The essay topic",
            "required": true
          }
        ]
      }
    ]
  }
}
```

### prompts/get

Get a prompt template with arguments filled in.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 12,
  "method": "prompts/get",
  "params": {
    "name": "write_essay",
    "arguments": {
      "topic": "climate change"
    }
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 12,
  "result": {
    "description": "Generate an essay about climate change",
    "messages": [
      {
        "role": "user",
        "content": {
          "type": "text",
          "text": "Write a persuasive essay about climate change."
        }
      }
    ]
  }
}
```

---

## Root Methods (v3.0)

### roots/list

Lists all root directories the server has access to.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 13,
  "method": "roots/list"
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 13,
  "result": {
    "roots": [
      {
        "uri": "file:///Users/user/project",
        "name": "project"
      }
    ]
  }
}
```

### roots/add

Add a root directory to the server's workspace.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 14,
  "method": "roots/add",
  "params": {
    "uri": "file:///Users/user/new-project"
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 14,
  "result": {}
}
```

### roots/remove

Remove a root directory from the server's workspace.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 15,
  "method": "roots/remove",
  "params": {
    "uri": "file:///Users/user/old-project"
  }
}
```

---

## Completion Methods

### completion/complete

Request completion for a reference.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 16,
  "method": "completion/complete",
  "params": {
    "ref": {
      "type": "resource",
      "uri": "mcp://config"
    },
    "argument": {
      "name": "feature",
      "value": "tools"
    }
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 16,
  "result": {
    "completion": {
      "text": "tools",
      "items": [
        {"text": "tools"}
      ]
    }
  }
}
```

---

## Task Methods

### tasks/create

Create an asynchronous task.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 17,
  "method": "tasks/create",
  "params": {
    "task": {
      "action": {
        "type": "tool",
        "name": "long_operation"
      },
      "timeoutMs": 30000
    }
  }
}
```

**Response:**
```json
{
  "jsonrpc": "2.0",
  "id": 17,
  "result": {
    "taskId": "task-123",
    "status": "pending"
  }
}
```

### tasks/list

List all tasks.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 18,
  "method": "tasks/list"
}
```

### tasks/get

Get task details.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 19,
  "method": "tasks/get",
  "params": {
    "taskId": "task-123"
  }
}
```

### tasks/cancel

Cancel a running task.

**Request:**
```json
{
  "jsonrpc": "2.0",
  "id": 20,
  "method": "tasks/cancel",
  "params": {
    "taskId": "task-123"
  }
}
```

---

## Notifications

Server-to-client notifications (no response expected).

### notifications/progress

Progress update for long-running operations.

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/progress",
  "params": {
    "progressToken": "progress-123",
    "progress": 0.5,
    "total": 1.0,
    "message": "Halfway complete"
  }
}
```

### resources/updated

Notification that a resource has changed.

```json
{
  "jsonrpc": "2.0",
  "method": "resources/updated",
  "params": {
    "uri": "mcp://config"
  }
}
```

### resources/list_changed

Notification that the resource list has changed.

```json
{
  "jsonrpc": "2.0",
  "method": "resources/list_changed",
  "params": {}
}
```

### tools/list_changed

Notification that the tool list has changed.

```json
{
  "jsonrpc": "2.0",
  "method": "tools/list_changed",
  "params": {}
}
```

### prompts/list_changed

Notification that the prompt list has changed.

```json
{
  "jsonrpc": "2.0",
  "method": "prompts/list_changed",
  "params": {}
}
```

### roots/list_changed

Notification that the roots list has changed (v3.0).

```json
{
  "jsonrpc": "2.0",
  "method": "roots/list_changed",
  "params": {}
}
```

### notifications/cancelled

Notification that a request was cancelled.

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/cancelled",
  "params": {
    "requestId": "req-123",
    "reason": "user_requested"
  }
}
```

### notifications/message

Logging/notification message.

```json
{
  "jsonrpc": "2.0",
  "method": "notifications/message",
  "params": {
    "level": "info",
    "logger": "erlmcp",
    "data": {
      "message": "Server started successfully"
    }
  }
}
```

---

## Batch Requests

Multiple requests can be sent in a single batch:

```json
[
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "resources/list"
  },
  {
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/list"
  }
]
```

**Batch Response:**
```json
[
  {
    "jsonrpc": "2.0",
    "id": 1,
    "result": {"resources": []}
  },
  {
    "jsonrpc": "2.0",
    "id": 2,
    "result": {"tools": []}
  }
]
```

---

## Error Response Format

All errors follow this structure:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32002,
    "message": "Tool not found",
    "data": {
      "tool": "nonexistent_tool"
    }
  }
}
```

| Field | Type | Description |
|-------|------|-------------|
| `code` | integer | Error code |
| `message` | string | Human-readable error message |
| `data` | any | Additional error details (optional) |

See `error-codes.md` for complete error code reference.
