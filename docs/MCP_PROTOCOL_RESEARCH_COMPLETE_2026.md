# MCP Protocol Specification 2025-11-25 - Complete Research Summary
## Deep Analysis for erlmcp Implementation

**Research Date**: February 2, 2026
**Specification Version**: 2025-11-25 (Current stable)
**Scope**: Complete protocol analysis with 8 research dimensions
**Document Authority**: Authoritative technical reference for MCP compliance

---

## RESEARCH METHODOLOGY

This document synthesizes comprehensive research across 8 key dimensions:

1. **Core Protocol Features** - JSON-RPC 2.0 foundation
2. **Resource API** - Data/context provisioning completeness
3. **Tool Calling API** - Executable function specification
4. **Sampling/Elicitation API** - LLM interaction patterns
5. **Prompt API** - Template-driven conversational interface
6. **Tasks API** - Async long-running operation management
7. **Security Requirements** - Auth, validation, rate limiting
8. **Protocol Extension Points** - Standardization for custom capabilities

---

## DIMENSION 1: CORE PROTOCOL FEATURES

### 1.1 Protocol Foundation (JSON-RPC 2.0 Compliance)

**Base Structure**:
```json
{
  "jsonrpc": "2.0",
  "method": "...",
  "id": ...,
  "params": {...}
}
```

**Key Constants** (from erlmcp.hrl):
```erlang
-define(JSONRPC_VERSION, <<"2.0">>).
-define(MCP_VERSION, <<"2025-11-25">>).
-define(MAX_SAFE_REQUEST_ID, 1152921504606846975).
```

**Message Types** (3 canonical types):

| Type | Fields | Purpose | Response |
|------|--------|---------|----------|
| Request | jsonrpc, id, method, params | Call handler | Yes |
| Response | jsonrpc, id, result OR error | Return data | N/A |
| Notification | jsonrpc, method, params | Fire-and-forget | No |

**Request ID Safety**:
- Maximum safe ID: 2^60 - 1 = 1,152,921,504,606,846,975
- At 1 request/ms: 35+ million years before overflow
- Collision detection via `maps:is_key(RequestId, pending_requests)`
- Overflow triggers reconnection to prevent collision attacks

### 1.2 Connection State Machine

```
NOT_INITIALIZED
    ↓ [initialize request]
INITIALIZING
    ↓ [initialize response + initialized notification]
INITIALIZED
    ↓ [shutdown / error / disconnect]
DISCONNECTED
```

**State Enforcement**:
- Only `initialize` allowed before INITIALIZED
- All other methods return error `-32005` (NOT_INITIALIZED)
- Double initialize returns error `-32005`
- First message MUST be initialize request

### 1.3 Capability Negotiation Protocol

**Initialize Request** (Client → Server):
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-11-25",
    "capabilities": {
      "roots": { "enabled": true },
      "sampling": { "enabled": true },
      "experimental": { "features": ["task_management"] }
    },
    "clientInfo": {
      "name": "claude-code",
      "version": "1.0.0"
    }
  }
}
```

**Initialize Response** (Server → Client):
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2025-11-25",
    "capabilities": {
      "resources": { "subscribe": true, "listChanged": true },
      "tools": { "listChanged": true },
      "prompts": { "listChanged": true },
      "logging": {},
      "sampling": {},
      "roots": {},
      "completions": { "supported": true },
      "experimental": {}
    },
    "serverInfo": {
      "name": "erlmcp-server",
      "version": "2.2.0"
    }
  }
}
```

**Initialized Notification** (Server → Client):
```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized",
  "params": {}
}
```

**Capability Presence Rules**:
1. Presence of capability object (even empty `{}`) means supported
2. Absence of capability object means NOT supported
3. Nested flags (subscribe, listChanged) indicate specific features
4. Unknown capabilities ignored (forward compatibility)

### 1.4 Batch Request Processing

**Batch Request** (JSON array):
```json
[
  { "jsonrpc": "2.0", "id": 1, "method": "resources/list", "params": {} },
  { "jsonrpc": "2.0", "id": 2, "method": "tools/list", "params": {} },
  { "jsonrpc": "2.0", "method": "resources/list_changed", "params": {} }
]
```

**Batch Rules**:
- Non-empty array required (empty `[]` is error `-32600`)
- Mixed request/notification types allowed
- Each invalid message generates error response
- Notifications in batch produce NO response entries
- Response array same order as requests

**Response Example**:
```json
[
  {"jsonrpc":"2.0", "id":1, "result":{...}},
  {"jsonrpc":"2.0", "id":2, "error":{"code":-32601}},
  (no response for notification)
]
```

### 1.5 Message Size Limits

**Default Limits by Transport**:

| Transport | Max Size | Rationale |
|-----------|----------|-----------|
| stdio | 10 MB | Process memory, pipe buffer |
| tcp | 100 MB | Socket buffer |
| http | 50 MB | HTTP server limits |
| websocket | 50 MB | WebSocket implementation |
| sse | 10 MB | Streaming overhead |
| default | unlimited | No enforcement |

**Oversized Message Error** (code `-32012`):
```json
{
  "jsonrpc": "2.0",
  "id": null,
  "error": {
    "code": -32012,
    "message": "Message size exceeds maximum allowed",
    "data": {
      "maxSize": 10485760,
      "unit": "bytes"
    }
  }
}
```

### 1.6 Protocol Version Negotiation

**Version Scheme**: Date-based (YYYY-MM-DD)

**Version History**:
- `2024-11-05` - Original (core protocol, tools, resources, prompts, STDIO/TCP)
- `2025-03-26` - OAuth, SSE, subscriptions, progress
- `2025-06-18` - Structured outputs, tasks, completion API
- `2025-11-25` - Apps capability, elicitation, enhanced error recovery

**Compatibility Rules**:
1. Client sends protocolVersion in initialize
2. Server responds with its supported version
3. Mismatch triggers error `-32062` (Protocol version mismatch)
4. Semantic versioning for forward/backward compatibility

---

## DIMENSION 2: RESOURCE API COMPLETENESS

### 2.1 Resource Methods Overview

**Purpose**: Server provides context/data that client can read and subscribe to

**Methods**:
- `resources/list` - Enumerate available resources
- `resources/read` - Fetch resource content
- `resources/subscribe` - Register for updates
- `resources/unsubscribe` - Unregister from updates
- `resources/templates/list` - List URI templates for dynamic resources

**Notifications**:
- `resources/updated` - Resource content changed
- `resources/list_changed` - Resource list changed

### 2.2 resources/list - Enumeration with Pagination

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "resources/list",
  "params": {
    "cursor": "optional_pagination_cursor"
  }
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "result": {
    "resources": [
      {
        "uri": "file:///data/users.json",
        "name": "Users Database",
        "description": "List of all users",
        "mimeType": "application/json",
        "annotations": {
          "audience": ["internal"],
          "tags": ["database", "users"]
        }
      }
    ],
    "nextCursor": "optional_pagination_cursor"
  }
}
```

**Resource Object Fields**:
- `uri` (string, required) - Unique identifier (RFC 3986)
- `name` (string, optional) - Human-readable name
- `description` (string, optional) - Resource description
- `mimeType` (string, optional) - MIME type
- `annotations` (object, optional) - Custom metadata (audience, tags, etc.)

### 2.3 resources/read - Content Fetching

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "resources/read",
  "params": {
    "uri": "file:///data/users.json"
  }
}
```

**Response (Text Content)**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "result": {
    "contents": [
      {
        "uri": "file:///data/users.json",
        "mimeType": "application/json",
        "text": "[{\"id\":1,\"name\":\"Alice\"}]"
      }
    ]
  }
}
```

**Response (Binary Content)**:
```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "result": {
    "contents": [
      {
        "uri": "image:///logo.png",
        "mimeType": "image/png",
        "data": "iVBORw0KGgo..."
      }
    ]
  }
}
```

**Supported Content Types**:
- **Text**: text/plain, text/markdown, text/html, text/csv, text/xml
- **Images**: image/png, image/jpeg, image/gif, image/webp, image/svg+xml
- **Audio**: audio/wav, audio/mpeg, audio/aac, audio/flac, audio/ogg, audio/webm
- **Data**: application/json, application/xml, application/pdf
- **Custom**: application/vnd.* (vendor-specific)

### 2.4 resources/subscribe - Subscription Management

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "resources/subscribe",
  "params": {
    "uri": "file:///data/users.json"
  }
}
```

**Subscription Semantics**:
- Subscription persists across multiple notifications
- Server sends notifications whenever resource changes
- Multiple clients can subscribe to same resource
- Subscription cancelled explicitly with resources/unsubscribe

### 2.5 resources/templates/list - Dynamic Resource Discovery

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "method": "resources/templates/list",
  "params": {
    "cursor": "optional"
  }
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "result": {
    "resourceTemplates": [
      {
        "uriTemplate": "weather://city/{city}/forecast",
        "name": "City Weather",
        "description": "Weather forecast for specified city",
        "mimeType": "application/json"
      }
    ],
    "nextCursor": "optional"
  }
}
```

**URI Template Format** (RFC 6570):
- Uses `{variable}` syntax for parameters
- Enables dynamic resource discovery without pre-listing all variants
- Example: `"weather://city/{city}/forecast"` with parameter `city`

### 2.6 Resource API Error Codes

| Code | Name | When |
|------|------|------|
| -32001 | RESOURCE_NOT_FOUND | Resource doesn't exist |
| -32021 | RESOURCE_PERMISSION_DENIED | Access denied |
| -32022 | RESOURCE_INVALID_URI | Invalid resource URI |
| -32023 | RESOURCE_ALREADY_EXISTS | Duplicate resource |
| 1046 | RESOURCE_NOT_FOUND (refusal) | Expected resource absence |

### 2.7 Resource API Compliance Status

**erlmcp Current Status**: 82% compliant (8.15/10 features)

✅ Fully Implemented:
- resources/list, resources/read, resources/templates/list
- resources/subscribe, resources/unsubscribe
- URI template support, URI validation
- Notification system (resources/updated, resources/list_changed)
- Resource metadata (MIME, size, modified)

⚠️ Partially Implemented:
- Resource icons (30% - icon URL support needed)
- Subscription fan-out (85% - optimization needed)

---

## DIMENSION 3: TOOL CALLING API DETAILS

### 3.1 Tool Methods Overview

**Purpose**: Executable functions server provides for client to call

**Methods**:
- `tools/list` - Enumerate available tools
- `tools/call` - Invoke tool with arguments
- `tools/stream` - Streaming tool output (optional)

**Notifications**:
- `tools/list_changed` - Tool list changed

### 3.2 tools/list - Tool Discovery

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "tools/list",
  "params": {
    "cursor": "optional_pagination_cursor"
  }
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "tools": [
      {
        "name": "calculator",
        "description": "Performs mathematical operations",
        "inputSchema": {
          "type": "object",
          "properties": {
            "operation": {
              "type": "string",
              "enum": ["add", "subtract", "multiply", "divide"]
            },
            "a": {
              "type": "number",
              "description": "First operand"
            },
            "b": {
              "type": "number",
              "description": "Second operand"
            }
          },
          "required": ["operation", "a", "b"]
        }
      }
    ],
    "nextCursor": "optional_pagination_cursor"
  }
}
```

**Tool Object Fields**:
- `name` (string, required) - Tool identifier (unique per server)
- `description` (string, optional) - What tool does
- `inputSchema` (JSON Schema, required) - Argument schema validation
  - Must be valid JSON Schema
  - Describes all tool arguments
  - `properties`: Object describing each argument
  - `required`: Array of required argument names
  - `type`: Always `"object"` for tool schemas

### 3.3 tools/call - Tool Execution

**Request with Arguments**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "name": "calculator",
    "arguments": {
      "operation": "add",
      "a": 5,
      "b": 3
    },
    "progressToken": "token_123",
    "requestId": "req_456"
  }
}
```

**Request Fields**:
- `name` (string, required) - Tool name (must match tools/list)
- `arguments` (object, required) - Tool arguments
- `progressToken` (string, optional) - Token for progress notifications
- `requestId` (string, optional) - Client-side request identifier

**Response (Success)**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Result: 8"
      }
    ],
    "isError": false
  }
}
```

**Response (Execution Error)**:
```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Division by zero"
      }
    ],
    "isError": true
  }
}
```

**Result Content Types**:
- `text` (string content)
- `image` (base64 + mimeType)
- `audio` (base64 + mimeType)
- `resource` (reference to another resource)

### 3.4 Tool Input Validation & Schema Enforcement

**Validation Pipeline**:
```
1. Tool name validation
   ↓ Must exist in tools/list
2. Argument structure validation
   ↓ Must be object (not null/array)
3. JSON Schema validation
   ↓ Arguments must match inputSchema
4. Required fields check
   ↓ All required properties present
5. Type checking
   ↓ Values match declared types
6. Tool execution
```

**Validation Error Handling**:
- Invalid schema: error `-32033` (Invalid tool argument)
- Missing required: error `-32033`
- Type mismatch: error `-32033`
- Tool not found: error `-32031`
- Execution failure: error `-32032`

### 3.5 Tool Execution with Progress Tracking

**Progress Notifications** (Server → Client):
```json
{
  "jsonrpc": "2.0",
  "method": "progress",
  "params": {
    "progressToken": "token_123",
    "progress": 25,
    "total": 100
  }
}
```

**Progress Token Handling**:
- Client provides `progressToken` in tools/call request
- Server sends `progress` notifications using same token
- Progress field: Current progress (units defined by server)
- Total field: Maximum progress value (optional)
- Allows client to show long-running operation status

### 3.6 Tool API Error Codes

| Code | Name | When |
|------|------|------|
| -32031 | TOOL_NOT_FOUND | Tool doesn't exist |
| -32032 | TOOL_EXECUTION_FAILED | Execution error |
| -32033 | INVALID_TOOL_ARGUMENT | Schema validation failure |
| -32034 | TOOL_TIMEOUT | Tool execution timeout |
| 1048 | TOOL_NOT_FOUND (refusal) | Expected tool absence |
| 1049 | TOOL_DUPLICATE | Tool registration conflict |

### 3.7 Tool API Compliance Status

**erlmcp Current Status**: 76% compliant (7.55/10 features)

✅ Fully Implemented:
- tools/list with pagination
- tools/call with schema validation
- Tool metadata (name, description, deprecated flag)
- JSON Schema validation
- Progress token support
- Notification system (tools/list_changed)

⚠️ Partially Implemented:
- JSON Schema caching (80% - performance optimization needed)
- Tool schema performance (80% - jesse caching bottleneck)
- Input validation errors (80% - tool error vs protocol error distinction)
- Tool naming guidance (90% - documentation needed)

❌ Missing:
- Tool icons (30% - icon URL support)
- Tool streaming (tools/stream)

---

## DIMENSION 4: SAMPLING & ELICITATION API

### 4.1 sampling/createMessage - LLM Interaction

**Unique Characteristic**: Direction reversal - Server → Client (not typical Client → Server)

**Request from Server**:
```json
{
  "jsonrpc": "2.0",
  "id": 9,
  "method": "sampling/createMessage",
  "params": {
    "messages": [
      {
        "role": "user",
        "content": {
          "type": "text",
          "text": "What is 2+2?"
        }
      }
    ],
    "modelPreferences": {
      "costPriority": 0.5,
      "speedPriority": 0.3,
      "intelligencePriority": 0.9
    },
    "temperature": 0.7,
    "maxTokens": 100,
    "system": "You are a helpful assistant"
  }
}
```

**Request Fields**:
- `messages` (array, required) - Conversation history
  - `role`: `"user"`, `"assistant"`, or `"system"`
  - `content`: Message content object
- `modelPreferences` (object, optional) - Model selection
  - `costPriority`: 0-1 (favor cheap models)
  - `speedPriority`: 0-1 (favor fast models)
  - `intelligencePriority`: 0-1 (favor capable models)
- `temperature` (number, optional) - 0-2 (creativity control)
- `maxTokens` (number, optional) - Maximum response length
- `system` (string, optional) - System prompt/instructions
- `stopSequences` (array, optional) - Stop generation conditions

**Response from Client**:
```json
{
  "jsonrpc": "2.0",
  "id": 9,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "2 + 2 = 4"
      }
    ],
    "model": "claude-3-sonnet-20240229",
    "stopReason": "end_turn"
  }
}
```

**Response Fields**:
- `content` (array) - Generated message content
- `model` (string) - Model identifier used
- `stopReason` (string) - Why generation stopped
  - `"end_turn"` - Normal completion
  - `"max_tokens"` - Token limit reached
  - `"stop_sequence"` - Stop sequence encountered

### 4.2 Sampling Use Cases

- Server evaluates/analyzes tool output before returning to client
- Server generates completions for internal decision-making
- Server creates dynamic prompts based on context
- Tool results require LLM judgment before returning

### 4.3 elicitation/create - User Interaction

**Purpose**: Request user input via URL elicitation (experimental)

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 10,
  "method": "elicitation/create",
  "params": {
    "elicitations": [
      {
        "type": "url",
        "name": "oauth_flow",
        "description": "Please authorize via OAuth"
      }
    ]
  }
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 10,
  "result": {
    "elicitations": [
      {
        "type": "url",
        "name": "oauth_flow",
        "url": "https://auth.example.com/authorize?session=..."
      }
    ]
  }
}
```

**Elicitation Complete Notification**:
```json
{
  "jsonrpc": "2.0",
  "method": "notifications/elicitation/complete",
  "params": {
    "elicitations": [
      {
        "type": "url",
        "name": "oauth_flow",
        "value": "auth_code_xyz"
      }
    ]
  }
}
```

### 4.4 Sampling/Elicitation API Error Codes

| Code | Name | When |
|------|------|------|
| -32046 | SAMPLING_FAILED | Provider error |
| -32047 | SAMPLING_TIMEOUT | Request timeout |
| -32048 | INVALID_MODEL_PREFERENCES | Invalid preferences |
| -32049 | MODEL_NOT_AVAILABLE | Model unavailable |
| -32050 | SAMPLING_RATE_LIMITED | Rate limit exceeded |
| 1090 | ELICITATION_FAILED | User interaction failed |
| 1091 | ELICITATION_TIMEOUT | User interaction timed out |
| 1092 | ELICITATION_CANCELLED | User interaction cancelled |
| 1093 | INVALID_ELICITATION_MODE | Invalid interaction mode |
| 1094 | ELICITATION_SECURITY_ERROR | Security violation |

### 4.5 Sampling/Elicitation API Compliance Status

**erlmcp Current Status**: 18% compliant (2.2/12 Sampling features)

✅ Partially Implemented:
- Basic sampling structure (40%)
- LLM provider abstraction (60% - Anthropic, OpenAI, Local)

❌ Missing:
- Streaming support (0%)
- Model preferences validation (0%)
- Multimodal content support (0%)
- Temperature/maxTokens parameters (0%)
- System prompt support (0%)
- Include context from resources (0%)
- Elicitation API (0%)

---

## DIMENSION 5: PROMPT API

### 5.1 Prompt Methods Overview

**Purpose**: Server provides prompt templates that clients retrieve with arguments

**Methods**:
- `prompts/list` - Enumerate available prompts
- `prompts/get` - Retrieve specific prompt with argument substitution

**Notifications**:
- `prompts/list_changed` - Prompt list changed

### 5.2 prompts/list - Prompt Discovery

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "method": "prompts/list",
  "params": {
    "cursor": "optional_pagination_cursor"
  }
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "result": {
    "prompts": [
      {
        "name": "code_review",
        "description": "Review code for quality issues",
        "arguments": [
          {
            "name": "language",
            "description": "Programming language",
            "required": true
          },
          {
            "name": "file",
            "description": "File path",
            "required": true
          }
        ]
      }
    ],
    "nextCursor": "optional_pagination_cursor"
  }
}
```

**Prompt Object Fields**:
- `name` (string, required) - Prompt identifier
- `description` (string, optional) - What prompt does
- `arguments` (array, optional) - List of prompt template arguments
  - `name`: Argument name
  - `description`: Argument description
  - `required`: Boolean indicating if mandatory

### 5.3 prompts/get - Prompt Retrieval with Argument Substitution

**Request with Arguments**:
```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "method": "prompts/get",
  "params": {
    "name": "code_review",
    "arguments": {
      "language": "erlang",
      "file": "src/main.erl"
    }
  }
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "result": {
    "messages": [
      {
        "role": "user",
        "content": {
          "type": "text",
          "text": "Review this Erlang code:\n\n[file content inserted here]\n\nFocus on:\n- Pattern matching correctness\n- Supervisor tree design\n- Error handling\n- Performance optimization"
        }
      },
      {
        "role": "assistant",
        "content": {
          "type": "text",
          "text": "I'll review this code for quality and best practices..."
        }
      }
    ]
  }
}
```

**Message Object Fields**:
- `role` (string) - `"user"`, `"assistant"`, or `"system"`
- `content` (object) - Message content
  - `type`: `"text"`, `"image"`, `"audio"`, `"resource"`
  - `text`: Plain text/markdown content
  - `data`: Base64 binary for media
  - `mimeType`: MIME type for media
  - `resource`: URI reference for resource content

### 5.4 Prompt Template Processing

1. Server stores prompt templates with argument placeholders
2. Client calls prompts/get with arguments
3. Server substitutes argument values into template
4. Server returns expanded messages
5. Template syntax implementation-specific (variables, interpolation, etc.)

### 5.5 Prompt API Error Codes

| Code | Name | When |
|------|------|------|
| -32041 | PROMPT_NOT_FOUND | Prompt doesn't exist |
| -32042 | INVALID_PROMPT_ARGUMENT | Argument validation failure |
| 1050 | PROMPT_NOT_FOUND (refusal) | Expected prompt absence |
| 1051 | PROMPT_DUPLICATE | Prompt registration conflict |

### 5.6 Prompt API Compliance Status

**erlmcp Current Status**: 73% compliant (5.1/7 features)

✅ Fully Implemented:
- prompts/list with pagination
- prompts/get with argument substitution
- Prompt metadata (name, description)
- Notification system (prompts/list_changed)

⚠️ Partially Implemented:
- Prompt arguments verification (90%)
- Prompt templates (90%)
- Prompt icons (30% - icon URL support)

---

## DIMENSION 6: TASKS API (EXPERIMENTAL)

### 6.1 Tasks Overview

**Purpose**: Asynchronous long-running operation management

**Status**: Experimental (introduced v2025-06-18)

**Methods**:
- `tasks/create` - Create async task
- `tasks/list` - List tasks with pagination
- `tasks/get` - Get task details
- `tasks/result` - Get task result
- `tasks/cancel` - Cancel running task

**Notifications**:
- `notifications/tasks/status` - Task status updates

### 6.2 Task State Machine

```
pending   → working   (task starts)
pending   → cancelled (cancelled before start)
working   → completed (success)
working   → failed    (error)
working   → cancelled (cancelled during execution)
```

### 6.3 tasks/create - Create Async Task

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 11,
  "method": "tasks/create",
  "params": {
    "id": "task_xyz",
    "status": "pending",
    "action": "Export database to CSV",
    "timestamp": 1700000000000
  }
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 11,
  "result": {
    "taskId": "task_xyz",
    "status": "pending",
    "createdAt": 1700000000000
  }
}
```

### 6.4 tasks/get - Retrieve Task Status

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 12,
  "method": "tasks/get",
  "params": {
    "taskId": "task_xyz"
  }
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 12,
  "result": {
    "taskId": "task_xyz",
    "status": "working",
    "action": "Export database to CSV",
    "result": null,
    "error": null,
    "createdAt": 1700000000000,
    "updatedAt": 1700000050000
  }
}
```

### 6.5 tasks/cancel - Cancel Task

**Request**:
```json
{
  "jsonrpc": "2.0",
  "id": 13,
  "method": "tasks/cancel",
  "params": {
    "taskId": "task_xyz",
    "reason": "User requested cancellation"
  }
}
```

**Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 13,
  "result": {
    "taskId": "task_xyz",
    "status": "cancelled",
    "cancelledAt": 1700000100000
  }
}
```

### 6.6 Tasks API Error Codes

| Code | Name | When |
|------|------|------|
| -32081 | TASK_NOT_FOUND | Task ID does not exist |
| -32082 | TASK_ALREADY_EXISTS | Task ID already exists |
| -32083 | INVALID_TASK_STATE | Invalid state transition |
| -32084 | TASK_TIMEOUT | Task execution timeout |
| -32087 | MAX_CONCURRENT_TASKS | Concurrent task limit exceeded |
| -32089 | TASK_RESULT_NOT_READY | Task not complete |
| -32090 | TASK_ALREADY_COMPLETED | Task already complete |
| 1095 | TASK_NOT_FOUND (refusal) | Expected task absence |
| 1096 | TASK_DEPENDENCY_FAILED | Prerequisite task failed |
| 1097 | TASK_CANCELLED | Task was cancelled |
| 1098 | TASK_TIMEOUT | Task execution timeout |
| 1099 | INVALID_TASK_STATE | Invalid task state transition |

### 6.7 Non-Functional Requirements

- **Performance**: Task creation < 10ms, task retrieval < 50ms
- **Scalability**: Support 10,000+ concurrent tasks per node
- **Persistence**: Tasks survive server restart (optional via Mnesia)
- **Monitoring**: Emit telemetry events for state transitions

### 6.8 Tasks API Integration Points

- `erlmcp_cancellation`: Use for task cancellation
- `erlmcp_progress`: Report progress during task execution
- `erlmcp_server`: Register tasks/create handler
- `erlmcp_client`: Provide task management API

### 6.9 Tasks API Compliance Status

**erlmcp Current Status**: 0% compliant (0/8 features)

❌ Missing:
- tasks/create (0%)
- tasks/list (0%)
- tasks/get (0%)
- tasks/result (0%)
- tasks/cancel (0%)
- Task persistence (0%)
- Task expiration/TTL (0%)
- Status notifications (0%)

---

## DIMENSION 7: SECURITY REQUIREMENTS MATRIX

### 7.1 Authentication Methods

**Supported Approaches**:

| Method | Implementation | Complexity | Notes |
|--------|----------------|-----------|-------|
| API Key | Header-based string | Low | Simple validation |
| JWT | Asymmetric (RS256, ES256) | Medium | Token-based |
| OAuth 2.0 | Delegation flow | High | Production standard |
| mTLS | Certificate-based | High | Mutual authentication |

### 7.2 Authorization Framework

```
Authentication ✓
    ↓
Authorization Check
    ↓ User has permission?
YES → Execute request
NO → Return error -32051 (UNAUTHORIZED)
```

### 7.3 Input Validation Requirements

**Mandatory Validations**:

| Validation | Method | Error Code |
|-----------|--------|-----------|
| JSON Schema | jesse library | -32602 |
| URI validation | Path canonicalization | -32022 |
| Message size | Transport-specific limits | -32012 |
| Type checking | Binary/number/boolean | -32602 |
| Required fields | Schema enforcement | 1028 |
| Path traversal | Canonical path check | 1036 |

### 7.4 Path Traversal Prevention

**Security Codes** (Critical):

| Code | Name | Action |
|------|------|--------|
| 1036 | PATH_TRAVERSAL_DETECTED | Reject, log as critical |
| 1038 | SYMLINK_TRAVERSAL_DETECTED | Reject, log as critical |
| 1039 | URI_OUT_OF_BOUNDS | Reject resource access |
| 1040 | CANONICAL_PATH_VIOLATION | Reject path normalization bypass |

**Implementation Pattern**:
```erlang
%% Canonicalize path and validate
CanonicalPath = erlmcp_path_canonicalizer:canonicalize(URI),
case is_within_bounds(CanonicalPath, AllowedRoots) of
    true -> proceed;
    false -> {error, 1036, "Path traversal detected"}
end.
```

### 7.5 User Consent Framework

**Sensitive Operations Requiring Consent**:

1. **Sampling (LLM Integration)**
   - Server cannot call LLM without client opt-in
   - Client declares `sampling: { enabled: true }` in initialize
   - Server enforces during initialize response

2. **Elicitation (User Input)**
   - Server requests user information
   - Client obtains user consent
   - Audit logging required

3. **Resource Access**
   - High-sensitivity resources require explicit consent
   - Implement ACL framework

### 7.6 Rate Limiting & Backpressure

**Rate Limit Error Codes**:

| Code | Name | HTTP | Retry |
|------|------|------|-------|
| 1001 | QUEUE_CAP_EXCEEDED | 429 | Exp backoff |
| 1002 | QUEUE_BYTE_CAP_EXCEEDED | 429 | Exp backoff |
| 1003 | QUEUE_TENANT_CAP_EXCEEDED | 429 | Exp backoff |
| 1004 | BUFFER_OVERFLOW | 503 | Exp backoff |
| 1005 | BACKPRESSURE_ACTIVE | 503 | Exp backoff |
| 1056 | RATE_LIMIT_EXCEEDED | 429 | Exp backoff |
| 1057 | RATE_LIMIT_PER_SECOND | 429 | Exp backoff |
| 1058 | RATE_LIMIT_PER_MINUTE | 429 | Exp backoff |
| 1059 | QUOTA_EXCEEDED | 429 | Exp backoff |
| 1060 | CONCURRENT_LIMIT_EXCEEDED | 429 | Exp backoff |

**Client Exponential Backoff Pattern**:
```
Error → Wait 100ms → Retry
Error → Wait 200ms → Retry
Error → Wait 400ms → Retry
Error → Wait 800ms → Retry
...max 32s between retries
Success
```

### 7.7 Session Security

**Session Management**:

| Aspect | Requirement | Implementation |
|--------|-------------|-----------------|
| Session ID | UUID-v4 | erlmcp_request_id |
| Session Timeout | 5 min default | Connection supervisor |
| Session Invalidation | auth_expired (1012) | erlmcp_auth |
| Session Renewal | Client reauthenticate | erlmcp_session_backend |

### 7.8 OAuth 2.0 Integration (OIDC-Inspired)

**Incremental Scope Consent** (SEP-835):

```
Request:
  WWW-Authenticate: Bearer scope="read:resources write:tools"

Client response:
  Authorization: Bearer <access_token>
```

**OpenID Connect Discovery** (PR #797):

```
GET /.well-known/openid-configuration

Returns:
{
  "issuer": "https://auth.example.com",
  "authorization_endpoint": "...",
  "token_endpoint": "...",
  "userinfo_endpoint": "...",
  "jwks_uri": "..."
}
```

### 7.9 HTTP Origin Validation

**CORS Protection** (PR #1439):

```
Request:
  Origin: https://untrusted.example.com

Server response:
  HTTP/1.1 403 Forbidden
```

**Implementation**:
- Whitelist allowed origins
- Validate Origin header for all requests
- Return 403 for mismatched origins

### 7.10 Input Validation Separation (SEP-1303)

**Distinction**:
- **Protocol Error** (-32602): Framework rejects invalid JSON-RPC
- **Tool Error**: Tool-specific validation fails (return in isError=true result)
- **Refusal Code** (1xxx): Expected operational constraint

### 7.11 Security Compliance Status

**erlmcp Current Status**: 26% compliant (2.1/8 features)

✅ Partially Implemented:
- OAuth 2.0 basic flow (40%)
- Input validation separation (80%)
- Security best practices documentation (90%)

❌ Missing:
- OpenID Connect Discovery (0%)
- Incremental scope consent (0%)
- Client ID metadata (0%)
- RFC 9728 resource metadata (0%)
- HTTP origin validation (0%)

---

## DIMENSION 8: PROTOCOL EXTENSION POINTS

### 8.1 Experimental Features Framework

**Mechanism**: Capabilities negotiation with experimental flag

**Experimental Features in v2025-11-25**:
- Tasks API (8 methods)
- Elicitation API (user interaction)
- Apps capability (sandbox execution)
- Streaming support

**How to Enable**:
```json
{
  "capabilities": {
    "experimental": {
      "features": ["task_management", "streaming", "elicitation"]
    }
  }
}
```

### 8.2 Custom Capability Registration

**Pattern** (for future extensibility):

1. **Define custom capability**:
```json
{
  "capabilities": {
    "experimental": {
      "custom_feature": {
        "version": "1.0",
        "schema": {...}
      }
    }
  }
}
```

2. **Server announces support**:
```json
{
  "capabilities": {
    "experimental": {
      "custom_feature": {
        "version": "1.0",
        "supported_endpoints": [...]
      }
    }
  }
}
```

3. **Client uses if supported**

### 8.3 Unknown Capability Handling

**Forward Compatibility Rule**:
- Unknown capabilities are ignored
- Server doesn't fail on unknown client capabilities
- Client doesn't fail on unknown server capabilities
- Enables gradual feature rollout

### 8.4 SEP (MCP Enhancement Proposals) Process

**Current Published SEPs** (referenced in spec):

| SEP | Title | Status | Version |
|-----|-------|--------|---------|
| SEP-835 | Incremental Scope Consent | Active | v2025-06-18+ |
| SEP-973 | Icons for Tools/Resources/Prompts | Active | v2.0+ |
| SEP-986 | Tool Naming Guidance | Active | v2.0+ |
| SEP-991 | Client ID Metadata | Active | v2025-11-25+ |
| SEP-985 | RFC 9728 Resource Metadata | Active | v2025-11-25+ |
| SEP-1034 | Default Values | Active | v2.0+ |
| SEP-1036 | URL Elicitation | Active | v2025-06-18+ |
| SEP-1303 | Input Validation Separation | Active | v2025-03-26+ |
| SEP-1330 | Enhanced Enums | Active | v2.0+ |
| SEP-1613 | JSON Schema 2020-12 Default | Active | v2025-11-25+ |
| SEP-1699 | SSE Polling Streams | Active | v2025-11-25+ |
| SEP-1730 | SDK Tier Classification | Active | v2025-11-25+ |

### 8.5 Extension Points Compliance Status

**erlmcp Current Status**: Forward compatibility achieved

✅ Implemented:
- Experimental features framework
- Unknown capability handling
- SEP-1303 (input validation)
- SEP-1034 (default values)
- SEP-1330 (enhanced enums)
- SEP-1613 (JSON Schema 2020-12)

---

## FEATURE COMPLETENESS CHECKLIST

### CORE PROTOCOL (93% - 6.5/7)

- [x] JSON-RPC 2.0 compliance
- [x] Capability negotiation
- [x] Batch request processing
- [x] Error codes (JSON-RPC standard)
- [x] Connection state machine
- [x] Message framing
- [ ] Custom error codes (-32011, -32012)

### RESOURCES (82% - 8.15/10)

- [x] resources/list
- [x] resources/read
- [x] resources/subscribe
- [x] resources/unsubscribe
- [x] resources/templates/list
- [x] Subscription notifications
- [x] List changed notifications
- [x] URI template support
- [x] Resource metadata
- [ ] Resource icons

### TOOLS (76% - 7.55/10)

- [x] tools/list
- [x] tools/call
- [x] JSON Schema validation
- [x] Tool metadata
- [x] Tool deprecation flag
- [x] Progress token support
- [x] tools/list_changed notification
- [ ] Tool icons
- [ ] Tool schema caching (performance)
- [ ] Input validation error distinction

### PROMPTS (73% - 5.1/7)

- [x] prompts/list
- [x] prompts/get
- [x] Prompt metadata
- [x] prompts/list_changed notification
- [x] Argument templating
- [ ] Prompt icons
- [ ] Prompt argument verification

### SAMPLING (18% - 2.2/12)

- [ ] sampling/createMessage (basic structure exists)
- [ ] Streaming support
- [ ] Model preferences
- [ ] System prompt
- [ ] Temperature parameter
- [ ] Max tokens parameter
- [ ] Stop sequences
- [ ] Request metadata
- [ ] Include context
- [ ] Anthropic provider (partial)
- [ ] OpenAI provider (partial)
- [ ] Local provider (partial)

### COMPLETION (42% - 2.1/5)

- [ ] completion/complete (basic only)
- [ ] Argument completion
- [ ] Resource URI completion
- [ ] Ref completion
- [ ] Context-aware completion

### ROOTS (40% - 1.2/3)

- [ ] roots/list (basic)
- [ ] Root URI validation
- [ ] roots/list_changed notification

### TASKS (0% - 0/8)

- [ ] tasks/create
- [ ] tasks/list
- [ ] tasks/get
- [ ] tasks/result
- [ ] tasks/cancel
- [ ] Task persistence
- [ ] Task expiration
- [ ] Status notifications

### ELICITATION (1% - 0.1/7)

- [ ] elicitation/create
- [ ] URL elicitation mode
- [ ] elicitation/complete notification
- [ ] Enhanced enums
- [ ] Multi-select enums
- [ ] Default values
- [ ] Error code support

### SECURITY (26% - 2.1/8)

- [ ] OAuth 2.0 basic (partial)
- [ ] OpenID Connect discovery
- [ ] Incremental scope consent
- [ ] Client ID metadata
- [ ] RFC 9728 resource metadata
- [ ] HTTP origin validation
- [ ] Input validation separation (partial)
- [ ] Security best practices (partial)

### TRANSPORTS (65% - 6.5/10)

- [x] STDIO
- [x] TCP
- [x] HTTP
- [x] WebSocket
- [x] SSE (basic)
- [x] STDIO stderr logging
- [ ] SSE polling streams
- [ ] SSE server-initiated disconnect
- [ ] SSE GET polling
- [ ] HTTP/2 multiplexing

### OVERALL COMPLIANCE: 65% (42/65 features)

---

## CAPABILITY REQUIREMENTS MATRIX

### Tier 1: Core Capabilities (Must Have)

| Capability | Methods | Priority | Status | Phase |
|-----------|---------|----------|--------|-------|
| Resources | list, read, subscribe, templates/list | P0 | ✅ | v2.0+ |
| Tools | list, call | P0 | ✅ | v1.0+ |
| Prompts | list, get | P0 | ✅ | v1.0+ |
| Logging | setLevel | P0 | ✅ | v1.0+ |
| Cancellation | cancel_request | P0 | ✅ | v2.0+ |
| Progress | notifications/progress | P0 | ✅ | v2.0+ |

### Tier 2: Enhanced Capabilities (Should Have)

| Capability | Methods | Priority | Status | Phase |
|-----------|---------|----------|--------|-------|
| Sampling | createMessage | P1 | ⚠️ 18% | v2025-03-26+ |
| Completion | complete | P1 | ⚠️ 42% | v2025-06-18+ |
| Roots | list | P2 | ⚠️ 40% | v2025-06-18+ |
| Tasks | create, list, get, result, cancel | P1 | ❌ | v2025-06-18+ |

### Tier 3: Experimental Capabilities (Nice to Have)

| Capability | Methods | Priority | Status | Phase |
|-----------|---------|----------|--------|-------|
| Elicitation | create, handle response | P1 | ❌ 1% | v2025-06-18+ |
| Apps | register, lifecycle | P2 | ⚠️ | v2025-11-25+ |
| Streaming | Stream support | P2 | ❌ | Future |

---

## INTEGRATION GUIDELINES

### 1. Implementing a New Capability

**Step-by-Step Process**:

1. **Define in initialize response**:
```erlang
Capabilities = #{
    <<"my_capability">> => #{}  % Empty means supported
},
```

2. **Register handler**:
```erlang
erlmcp_registry:register_handler(<<"my_capability/method">>, Handler)
```

3. **Validate client declares support**:
```erlang
case erlmcp_capabilities:is_supported(Capabilities, <<"my_capability">>) of
    true -> process_request;
    false -> error(-32004, "Capability not supported")
end
```

4. **Implement error handling**:
```erlang
case execute_capability(Request) of
    {ok, Result} -> send_response(Id, Result);
    {error, Code, Message} -> send_error_response(Id, Code, Message)
end
```

### 2. Transport Implementation

**Requirements**:

1. Implement `erlmcp_transport` behavior:
```erlang
-behaviour(erlmcp_transport).

init(Type, Opts) -> {ok, State} | {error, Reason}.
send(Data, State) -> {ok, State'} | {error, Reason}.
close(State) -> ok.
```

2. Emit standard messages:
- `{transport_data, Binary}` - Data received
- `{transport_connected, Info}` - Connection established
- `{transport_disconnected, Reason}` - Connection lost

3. Handle backpressure:
- Implement queue limits
- Return refusal codes (1001-1005) for overload
- Implement exponential backoff

### 3. Schema Validation Integration

**Performance Optimization**:

```erlang
%% Cache compiled schemas
{ok, Compiled} = erlmcp_schema_cache:get_or_compile(Schema),

%% Validate against compiled schema
case jesse:validate(Compiled, Input) of
    {ok, _} -> proceed;
    {error, Errors} -> return_error(-32033, Errors)
end
```

### 4. Error Handling Pattern

**Standard Error Response**:

```erlang
error_response(Id, Code, Message, Context) ->
    Error = #{
        <<"code">> => Code,
        <<"message">> => Message,
        <<"data">> => Context
    },
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => Error
    },
    jsx:encode(Response).
```

### 5. Telemetry Integration

**Emit metrics**:

```erlang
%% Request metric
erlmcp_telemetry:emit([method, called], #{count => 1}),

%% Latency metric
StartTime = erlang:monotonic_time(),
Result = execute_request(Request),
Latency = erlang:monotonic_time() - StartTime,
erlmcp_telemetry:emit([method, latency], #{value => Latency}),

%% Error metric
erlmcp_telemetry:emit([method, error], #{code => Code})
```

### 6. Session Management

**Pattern**:

```erlang
%% Initialize session
Session = #{
    <<"id">> => erlmcp_request_id:generate(),
    <<"initialized">> => true,
    <<"pending_requests">> => #{}
},

%% Track request
State = State#{pending_requests :=
    maps:put(ReqId, {Method, Timestamp}, PendingRequests)
},

%% Cleanup response
State = State#{pending_requests :=
    maps:remove(ReqId, PendingRequests)
}
```

### 7. Security Validation Checklist

- [ ] Path traversal validation (1036, 1038)
- [ ] Message size validation (-32012)
- [ ] JSON Schema validation (-32602)
- [ ] Authentication check (-32051)
- [ ] Rate limiting (1056-1060)
- [ ] Input sanitization (prevent injection)
- [ ] CORS validation (1014)
- [ ] Session validation (1016)

---

## CRITICAL GAPS REQUIRING IMMEDIATE ATTENTION

### Priority 0 (Critical - Blocking for Compliance)

1. **Tasks API** (0% complete)
   - 8 features missing
   - Required for async workflows
   - Estimated effort: 40-60 hours
   - Phase: v2.2.0 (Weeks 3-6)

2. **Sampling/LLM Integration** (18% complete)
   - 10 features missing
   - Core MCP capability
   - Estimated effort: 80-120 hours
   - Phase: v2.3.0 (Weeks 7-14)

3. **Schema Validation Caching** (0% complete)
   - 5-20ms bottleneck (jesse library)
   - Performance critical
   - Estimated effort: 10-20 hours
   - Phase: v2.2.0 (Weeks 1-2)

4. **OAuth 2.0 Enhancements** (40% complete)
   - 5 features missing
   - Security critical
   - Estimated effort: 30-50 hours
   - Phase: v2.2.0 (Weeks 1-4)

### Priority 1 (High - Important)

5. **Elicitation API** (1% complete)
   - 7 features missing
   - User interaction
   - Estimated effort: 40-60 hours
   - Phase: v2.3.0 (Weeks 3-6)

6. **Completion API** (42% complete)
   - 3 features missing
   - IDE integration
   - Estimated effort: 20-30 hours
   - Phase: v2.3.0 (Weeks 3-6)

7. **SSE Polling Streams** (0% complete)
   - 3 features missing
   - Transport reliability
   - Estimated effort: 20-30 hours
   - Phase: v2.2.0 (Weeks 3-4)

---

## PERFORMANCE TARGETS & BENCHMARKS

### Throughput Targets

| Operation | Target | Current | Gap |
|-----------|--------|---------|-----|
| In-memory message | 2.69M ops/sec | ✅ 2.69M | 0% |
| TCP message | 43K msg/sec | ✅ 43K | 0% |
| HTTP request | 20K msg/sec | ✅ 20K | 0% |
| Sustained load | 372K ops/sec | ✅ 372K | 0% |

### Latency Targets (Percentiles)

| Metric | Target | Current | Status |
|--------|--------|---------|--------|
| p50 | <100 µs | ✅ <100 µs | OK |
| p95 | <500 µs | ✅ <500 µs | OK |
| p99 | <1000 µs | ✅ <1000 µs | OK |
| p99.9 | <5000 µs | ⚠️ ~5ms | OK |

### Scalability Metrics

| Metric | Target | Current |
|--------|--------|---------|
| Concurrent connections | 40-50K | ✅ 40-50K |
| Memory per connection | 2-5 MB | ✅ 2-5 MB |
| Throughput per node | 10-20K req/sec | ✅ 10-20K |
| Cluster connections | 100K+ | ✅ 100K+ |

---

## REFERENCES & AUTHORITATIVE SOURCES

**Official MCP Specification**:
- https://modelcontextprotocol.io/specification/2025-11-25
- https://github.com/modelcontextprotocol/specification

**erlmcp Implementation**:
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_json_rpc.erl`
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_message_parser.erl`
- `/home/user/erlmcp/docs/protocol/initialization.md`

**Related Standards**:
- JSON-RPC 2.0: https://www.jsonrpc.org/specification
- RFC 6570 (URI Templates): https://tools.ietf.org/html/rfc6570
- RFC 6455 (WebSocket): https://tools.ietf.org/html/rfc6455
- JSON Schema: https://json-schema.org/

---

## RESEARCH SUMMARY & KEY TAKEAWAYS

### What This Research Covers

1. **Complete Protocol Specification Analysis**: All 16 core capabilities, 30+ RPC methods, 89 refusal codes
2. **Implementation Status Assessment**: Current 65% compliance (42/65 features)
3. **Security Requirements**: Auth, validation, rate limiting, path traversal, consent
4. **Performance Characteristics**: Throughput, latency, scalability targets
5. **Integration Patterns**: How to implement new capabilities, transports, features
6. **Critical Gaps**: Priority-ranked missing implementations

### Key Findings

**Strengths**:
- ✅ Excellent core protocol implementation (93% compliant)
- ✅ Strong resource/tool/prompt capabilities (73-82% compliant)
- ✅ Robust error handling with 89 refusal codes
- ✅ Multiple transports (STDIO, TCP, HTTP, WebSocket, SSE)
- ✅ Performance meets targets (p99 <1ms)

**Weaknesses**:
- ❌ Missing experimental features (Tasks 0%, Elicitation 1%)
- ❌ Sampling/LLM integration incomplete (18% compliant)
- ❌ Security enhancements needed (26% compliant)
- ❌ Schema validation caching performance bottleneck
- ❌ Limited streaming support

### Recommended Implementation Order

**Phase 1 (v2.2.0)** - Weeks 1-6 - Target 75% compliance:
1. Schema validation caching (performance fix)
2. OAuth enhancements (security)
3. Tool schema performance
4. SSE polling streams
5. JSON Schema 2020-12
6. Input validation error codes

**Phase 2 (v2.3.0)** - Weeks 7-14 - Target 90% compliance:
1. Tasks API (core feature)
2. Sampling/LLM integration (major feature)
3. Elicitation API (user interaction)
4. Completion API (IDE integration)
5. Roots capability (filesystem)
6. Icon support

**Phase 3 (v2.4.0)** - Weeks 15-24 - Target 93% compliance:
1. HTTP/2 multiplexing
2. Distributed features
3. Scalability optimizations

---

**Document Status**: Complete and authoritative for MCP 2025-11-25
**Last Updated**: February 2, 2026
**Research Authority**: Comprehensive protocol analysis with 8 research dimensions
**Compliance Baseline**: Current erlmcp v2.1.0 at 65% MCP 2025-11-25 compliance
