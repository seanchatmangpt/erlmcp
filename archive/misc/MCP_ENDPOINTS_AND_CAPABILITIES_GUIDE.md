# MCP Core Capabilities and Endpoints - Complete Research Guide

**MCP Protocol Version**: 2025-11-25
**erlmcp Implementation**: v2.2.0
**Document Date**: 2026-01-31

## Table of Contents

1. [Protocol Overview](#protocol-overview)
2. [Core Methods and Endpoints](#core-methods-and-endpoints)
3. [Resources API](#resources-api)
4. [Tools API](#tools-api)
5. [Prompts API](#prompts-api)
6. [Sampling API](#sampling-api)
7. [Tasks API](#tasks-api)
8. [Completion API](#completion-api)
9. [Server Capabilities](#server-capabilities)
10. [Client Capabilities](#client-capabilities)
11. [Notification Methods](#notification-methods)
12. [Error Codes and Responses](#error-codes-and-responses)

---

## Protocol Overview

The Model Context Protocol (MCP) uses **JSON-RPC 2.0** for communication over various transports (stdio, TCP, HTTP, WebSocket, SSE). All messages follow the JSON-RPC 2.0 specification.

### Message Structure

#### Request Format
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "method/name",
  "params": { /* parameters */ }
}
```

#### Response Format
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": { /* response data */ }
}
```

#### Error Response Format
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32601,
    "message": "Method not found",
    "data": { /* optional additional info */ }
  }
}
```

#### Notification Format (no response expected)
```json
{
  "jsonrpc": "2.0",
  "method": "notification/method",
  "params": { /* parameters */ }
}
```

---

## Core Methods and Endpoints

### 1. Initialize

**Method**: `initialize` (request)
**Direction**: Client → Server
**Required**: Yes, must be first request
**Since**: MCP 2025-11-25

**Request Parameters**:
```erlang
#{
  <<"protocolVersion">> => <<"2025-11-25">>,
  <<"capabilities">> => #{
    <<"roots">> => #{<<"listChanged">> => true},
    <<"sampling">> => #{},
    <<"tools">> => #{<<"listChanged">> => true}
  },
  <<"clientInfo">> => #{
    <<"name">> => <<"client-name">>,
    <<"version">> => <<"1.0.0">>
  }
}
```

**Response**:
```erlang
#{
  <<"protocolVersion">> => <<"2025-11-25">>,
  <<"capabilities">> => #{
    <<"resources">> => #{
      <<"subscribe">> => true,
      <<"listChanged">> => true
    },
    <<"tools">> => #{
      <<"listChanged">> => true
    },
    <<"prompts">> => #{
      <<"listChanged">> => true
    },
    <<"logging">> => #{},
    <<"sampling">> => #{
      <<"modelPreferences">> => #{...}
    }
  },
  <<"serverInfo">> => #{
    <<"name">> => <<"erlmcp">>,
    <<"version">> => <<"2.2.0">>
  },
  <<"instructions">> => <<"Server instructions (optional)">>
}
```

### 2. Ping

**Method**: `ping` (request)
**Direction**: Client → Server
**Response**: Empty result object `{}`
**Since**: MCP 2025-11-25

**Usage**: Verify connection is alive

```erlang
Request = #{
  <<"jsonrpc">> => <<"2.0">>,
  <<"id">> => 1,
  <<"method">> => <<"ping">>,
  <<"params">> => #{}
}

Response = #{
  <<"jsonrpc">> => <<"2.0">>,
  <<"id">> => 1,
  <<"result">> => #{}
}
```

### 3. Shutdown

**Method**: `shutdown` (request/notification)
**Direction**: Client → Server
**Since**: MCP 2025-11-25

**Semantics**: Gracefully shutdown the server

---

## Resources API

### List Resources

**Method**: `resources/list`
**Direction**: Client → Server
**Capability Required**: `resources`
**Feature**: Basic resource discovery

**Request Parameters**:
```erlang
#{
  <<"cursor">> => <<"optional-cursor">>,  % For pagination
  <<"limit">> => 100  % Optional limit
}
```

**Response**:
```erlang
#{
  <<"resources">> => [
    #{
      <<"uri">> => <<"doc://readme">>,
      <<"name">> => <<"README">>,
      <<"description">> => <<"Application README">>,
      <<"mimeType">> => <<"text/markdown">>,
      <<"metadata">> => #{},
      <<"lastModified">> => 1640995200000
    }
  ],
  <<"cursor">> => <<"next-cursor">>  % Optional for pagination
}
```

### Read Resource

**Method**: `resources/read`
**Direction**: Client → Server
**Capability Required**: `resources`

**Request Parameters**:
```erlang
#{
  <<"uri">> => <<"doc://readme">>
}
```

**Response**:
```erlang
#{
  <<"contents">> => [
    #{
      <<"type">> => <<"text">>,
      <<"text">> => <<"Content here...">>,
      <<"mimeType">> => <<"text/markdown">>
    }
  ]
}
```

**Content Types Supported**:
- `text` - Plain text content (use `text` field)
- `image` - Image content (use `data` field with base64)
- `audio` - Audio content (use `data` field with base64)
- `resource/link` - Link to another resource

**Example with Image**:
```erlang
#{
  <<"contents">> => [
    #{
      <<"type">> => <<"image">>,
      <<"data">> => <<"base64-encoded-image">>,
      <<"mimeType">> => <<"image/png">>
    }
  ]
}
```

### List Resource Templates

**Method**: `resources/templates/list`
**Direction**: Client → Server
**Capability Required**: `resources`

**Request Parameters**: None or pagination params

**Response**:
```erlang
#{
  <<"resourceTemplates">> => [
    #{
      <<"uriTemplate">> => <<"user://{username}/profile">>,
      <<"name">> => <<"User Profile">>,
      <<"description">> => <<"User profile resource">>,
      <<"mimeType">> => <<"application/json">>
    }
  ]
}
```

### Subscribe to Resource

**Method**: `resources/subscribe`
**Direction**: Client → Server
**Capability Required**: `resources.subscribe = true`

**Request Parameters**:
```erlang
#{
  <<"uri">> => <<"weather://city">>
}
```

**Response**:
```erlang
#{}  % Empty object
```

**Notifications Sent**:
- `resources/updated` - When subscribed resource changes
- `resources/list_changed` - When resource list changes

### Unsubscribe from Resource

**Method**: `resources/unsubscribe`
**Direction**: Client → Server
**Capability Required**: `resources.subscribe = true`

**Request Parameters**:
```erlang
#{
  <<"uri">> => <<"weather://city">>
}
```

**Response**:
```erlang
#{}  % Empty object
```

---

## Tools API

### List Tools

**Method**: `tools/list`
**Direction**: Client → Server
**Capability Required**: `tools`

**Request Parameters**:
```erlang
#{
  <<"cursor">> => <<"optional-cursor">>,
  <<"limit">> => 100
}
```

**Response**:
```erlang
#{
  <<"tools">> => [
    #{
      <<"name">> => <<"sql_query">>,
      <<"description">> => <<"Execute SQL queries against the database">>,
      <<"inputSchema">> => #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"query">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"SQL query to execute">>
          },
          <<"limit">> => #{
            <<"type">> => <<"integer">>,
            <<"minimum">> => 1,
            <<"maximum">> => 1000,
            <<"default">> => 100
          }
        },
        <<"required">> => [<<"query">>]
      },
      <<"deprecated">> => false,
      <<"version">> => <<"1.0.0">>
    }
  ],
  <<"cursor">> => <<"next-cursor">>
}
```

**Tool Object Fields**:
- `name` (required) - Unique tool identifier
- `description` (required) - Human-readable description (≤10,000 characters)
- `inputSchema` (optional) - JSON Schema for input validation
- `deprecated` (optional) - Boolean flag
- `version` (optional) - Version string
- `metadata` (optional) - Implementation-specific metadata
- `experimental` (optional) - Experimental features object

### Call Tool

**Method**: `tools/call`
**Direction**: Client → Server
**Capability Required**: `tools`

**Request Parameters**:
```erlang
#{
  <<"name">> => <<"sql_query">>,
  <<"arguments">> => #{
    <<"query">> => <<"SELECT * FROM users">>,
    <<"limit">> => 10
  }
}
```

**Response**:
```erlang
#{
  <<"content">> => [
    #{
      <<"type">> => <<"text">>,
      <<"text">> => <<"[{id:1, name:\"Alice\"}, ...]">>,
      <<"mimeType">> => <<"application/json">>
    }
  ],
  <<"isError">> => false
}
```

**Response with Error**:
```erlang
#{
  <<"content">> => [
    #{
      <<"type">> => <<"text">>,
      <<"text">> => <<"SQL syntax error">>,
      <<"mimeType">> => <<"text/plain">>
    }
  ],
  <<"isError">> => true
}
```

---

## Prompts API

### List Prompts

**Method**: `prompts/list`
**Direction**: Client → Server
**Capability Required**: `prompts`

**Request Parameters**:
```erlang
#{
  <<"cursor">> => <<"optional-cursor">>,
  <<"limit">> => 100
}
```

**Response**:
```erlang
#{
  <<"prompts">> => [
    #{
      <<"name">> => <<"code_review">>,
      <<"description">> => <<"Review code for quality issues">>,
      <<"arguments">> => [
        #{
          <<"name">> => <<"language">>,
          <<"description">> => <<"Programming language">>,
          <<"required">> => true
        },
        #{
          <<"name">> => <<"style">>,
          <<"description">> => <<"Code style guide">>,
          <<"required">> => false
        }
      ]
    }
  ],
  <<"cursor">> => <<"next-cursor">>
}
```

**Prompt Object Fields**:
- `name` (required) - Unique prompt identifier
- `description` (optional) - Human-readable description
- `arguments` (optional) - List of prompt arguments

### Get Prompt

**Method**: `prompts/get`
**Direction**: Client → Server
**Capability Required**: `prompts`

**Request Parameters**:
```erlang
#{
  <<"name">> => <<"code_review">>,
  <<"arguments">> => #{
    <<"language">> => <<"erlang">>,
    <<"style">> => <<"stricter">>
  }
}
```

**Response**:
```erlang
#{
  <<"messages">> => [
    #{
      <<"role">> => <<"system">>,
      <<"content">> => #{
        <<"type">> => <<"text">>,
        <<"text">> => <<"You are an Erlang code reviewer...">>
      }
    },
    #{
      <<"role">> => <<"user">>,
      <<"content">> => #{
        <<"type">> => <<"text">>,
        <<"text">> => <<"Please review my code for...">>
      }
    }
  ]
}
```

**Message Content Types**:
- `text` - Text content
- `image` - Image content with base64 data
- `resource` - Reference to a resource (by URI)
- `resource/link` - Link to resource metadata

---

## Sampling API

**Method**: `sampling/createMessage`
**Direction**: Client → Server (request sent from server to client)
**Capability Required**: Client must have `sampling` capability
**Since**: MCP 2025-11-25

### Create Message for Sampling

**Request Parameters** (sent by server to client):
```erlang
#{
  <<"messages">> => [
    #{
      <<"role">> => <<"user">>,
      <<"content">> => #{
        <<"type">> => <<"text">>,
        <<"text">> => <<"What should I do?\n\n[context...]">>
      }
    }
  ],
  <<"modelPreferences">> => #{
    <<"costPriority">> => 0.5,      % 0.0-1.0
    <<"speedPriority">> => 0.3,     % 0.0-1.0
    <<"intelligencePriority">> => 0.8,  % 0.0-1.0
    <<"temperature">> => 0.7,       % 0.0-2.0
    <<"maxTokens">> => 1024,
    <<"stopSequences">> => [<<"Human:">>, <<"Assistant:">>]
  },
  <<"systemPrompt">> => <<"You are a helpful assistant...">>,
  <<"includedContext">> => <<"this_request">>  % Optional
}
```

**Response from client**:
```erlang
#{
  <<"model">> => <<"claude-3-sonnet-20240229">>,
  <<"stopReason">> => <<"end_turn">>,  % end_turn | max_tokens | stop_sequence
  <<"content">> => [
    #{
      <<"type">> => <<"text">>,
      <<"text">> => <<"Here's what I recommend...\">>
    }
  ]
}
```

**Model Preferences**:
- `costPriority` (0.0-1.0) - Optimize for cost
- `speedPriority` (0.0-1.0) - Optimize for speed
- `intelligencePriority` (0.0-1.0) - Optimize for intelligence
- `temperature` (0.0-2.0) - Sampling temperature
- `maxTokens` (int) - Maximum output tokens
- `stopSequences` (array) - Sequences that stop generation

**Supported Models**:
- OpenAI: GPT-4, GPT-3.5-turbo
- Anthropic: Claude 3 family
- Local: Ollama, LM Studio
- Mock: For testing

---

## Tasks API

**Since**: MCP 2025-11-25
**Capability Required**: `tasks`

Tasks enable asynchronous long-running operations with progress tracking and cancellation support.

### Create Task

**Method**: `tasks/create`
**Direction**: Client → Server

**Request Parameters**:
```erlang
#{
  <<"action">> => #{
    <<"type">> => <<"operation_name">>,
    <<"parameters">> => #{
      <<"key">> => <<"value">>
    }
  },
  <<"metadata">> => #{
    <<"progressToken">> => <<"progress-123">>,
    <<"timeout">> => 30000  % milliseconds
  }
}
```

**Response**:
```erlang
#{
  <<"taskId">> => <<"task-uuid-here">>
}
```

### List Tasks

**Method**: `tasks/list`
**Direction**: Client → Server

**Request Parameters**:
```erlang
#{
  <<"cursor">> => <<"optional-cursor">>,
  <<"limit">> => 100
}
```

**Response**:
```erlang
#{
  <<"tasks">> => [
    #{
      <<"taskId">> => <<"task-uuid">>,
      <<"status">> => <<"processing">>,  % pending|processing|completed|failed|cancelled
      <<"action">> => #{...},
      <<"result">> => undefined,
      <<"error">> => undefined,
      <<"createdAt">> => 1640995200000,
      <<"updatedAt">> => 1640995205000
    }
  ],
  <<"cursor">> => <<"next-cursor">>
}
```

### Get Task

**Method**: `tasks/get`
**Direction**: Client → Server

**Request Parameters**:
```erlang
#{
  <<"taskId">> => <<"task-uuid">>
}
```

**Response**:
```erlang
#{
  <<"task">> => #{
    <<"taskId">> => <<"task-uuid">>,
    <<"status">> => <<"completed">>,
    <<"action">> => #{...},
    <<"result">> => #{...},
    <<"error">> => undefined,
    <<"createdAt">> => 1640995200000,
    <<"updatedAt">> => 1640995305000
  }
}
```

### Get Task Result

**Method**: `tasks/result`
**Direction**: Client → Server

**Request Parameters**:
```erlang
#{
  <<"taskId">> => <<"task-uuid">>
}
```

**Response**:
```erlang
#{
  <<"result">> => #{...}  % Task result data
}
```

### Cancel Task

**Method**: `tasks/cancel`
**Direction**: Client → Server

**Request Parameters**:
```erlang
#{
  <<"taskId">> => <<"task-uuid">>,
  <<"reason">> => <<"User requested cancellation">>  % Optional
}
```

**Response**:
```erlang
#{}  % Empty object
```

---

## Completion API

**Since**: MCP 2025-11-25
**Capability Required**: `completions`

The Completion API provides code/text completion for tools, resources, and prompts.

### Complete

**Method**: `completion/complete`
**Direction**: Client → Server

**Request Parameters**:
```erlang
#{
  <<"ref">> => #{
    <<"type">> => <<"tool">>,  % tool|resource|prompt
    <<"name">> => <<"sql_query">>
  },
  <<"argument">> => #{
    <<"name">> => <<"query">>,
    <<"value">> => <<"SELECT * FR">>  % Partial input
  },
  <<"context">> => #{
    <<"arguments">> => #{
      <<"limit">> => 10
    }
  }
}
```

**Response**:
```erlang
#{
  <<"completions">> => [
    #{
      <<"value">> => <<"OM users">>,
      <<"label">> => <<"Complete SQL FROM clause">>
    },
    #{
      <<"value">> => <<"OM tables">>,
      <<"label">> => <<"Complete with FROM tables">>
    }
  ],
  <<"hasMore">> => false,
  <<"total">> => 2
}
```

---

## Server Capabilities

Servers declare capabilities during initialization. These control what features clients can use.

### Capability Structure

```erlang
-record(mcp_server_capabilities, {
    resources = #mcp_resources_capability{} ::
        #mcp_resources_capability{},
    tools = #mcp_tools_capability{} ::
        #mcp_tools_capability{},
    prompts = #mcp_prompts_capability{} ::
        #mcp_prompts_capability{},
    logging = #mcp_logging_capability{} ::
        #mcp_logging_capability{},
    sampling = #mcp_sampling_capability{} ::
        #mcp_sampling_capability{},
    roots = #mcp_roots_capability{} ::
        #mcp_roots_capability{},
    completions = undefined :: #mcp_capability{} | undefined,
    experimental = undefined :: map() | undefined
}).
```

### Resources Capability

```erlang
-record(mcp_resources_capability, {
    subscribe = false :: boolean(),     % Can subscribe to updates
    listChanged = false :: boolean()    % Can notify on list changes
}).
```

**Features**:
- `subscribe` - `resources/subscribe` and `resources/unsubscribe` available
- `listChanged` - Server can send `resources/list_changed` notifications

### Tools Capability

```erlang
-record(mcp_tools_capability, {
    listChanged = false :: boolean()    % Can notify on list changes
}).
```

**Features**:
- `listChanged` - Server can send `tools/list_changed` notifications

### Prompts Capability

```erlang
-record(mcp_prompts_capability, {
    listChanged = false :: boolean()    % Can notify on list changes
}).
```

**Features**:
- `listChanged` - Server can send `prompts/list_changed` notifications

### Logging Capability

```erlang
-record(mcp_logging_capability, {}).
```

No features. Presence indicates logging is supported.

**Method**: `logging/setLevel`

### Sampling Capability

```erlang
-record(mcp_sampling_capability, {
    modelPreferences = undefined :: map() | undefined
}).
```

Server can request LLM sampling from client.

### Roots Capability

```erlang
-record(mcp_roots_capability, {}).
```

No features. Client can provide workspace roots.

### Completions Capability

```erlang
#mcp_capability{enabled = true}
```

Server supports the Completion API.

---

## Client Capabilities

Clients declare what they support during initialization.

### Capability Structure

```erlang
-record(mcp_client_capabilities, {
    roots = undefined :: map() | undefined,
    sampling = undefined :: #mcp_sampling_capability{} | undefined,
    experimental = undefined :: map() | undefined,
    tools = #mcp_tools_capability{} :: #mcp_tools_capability{}
}).
```

### Roots Capability

Indicates client can provide workspace roots.

```erlang
#{
  <<"roots">> => #{
    <<"listChanged">> => true  % Client can track root changes
  }
}
```

### Sampling Capability

Client can perform LLM sampling (completion generation).

```erlang
#{
  <<"sampling">> => #{
    <<"modelPreferences">> => #{
      <<"costPriority">> => 0.5,
      <<"speedPriority">> => 0.3,
      <<"intelligencePriority">> => 0.8
    }
  }
}
```

### Tools Capability

```erlang
#{
  <<"tools">> => #{
    <<"listChanged">> => true  % Client can track tool list changes
  }
}
```

---

## Notification Methods

Servers send notifications to clients about state changes. These are sent without request IDs (one-way messages).

### Resources Updated

**Method**: `resources/updated`
**Direction**: Server → Client
**Sent When**: Resource changes (if subscribed)

**Parameters**:
```erlang
#{
  <<"uri">> => <<"weather://city">>,
  <<"metadata">> => #{
    <<"updated_at">> => 1640995200000,
    <<"version">> => <<"2024-01-01">>
  }
}
```

### Resources List Changed

**Method**: `resources/list_changed`
**Direction**: Server → Client
**Sent When**: Resources are added/removed (if capability enabled)

**Parameters**:
```erlang
#{}  % No parameters
```

### Tools List Changed

**Method**: `tools/list_changed`
**Direction**: Server → Client
**Sent When**: Tools are added/removed (if capability enabled)

**Parameters**:
```erlang
#{}  % No parameters
```

### Prompts List Changed

**Method**: `prompts/list_changed`
**Direction**: Server → Client
**Sent When**: Prompts are added/removed (if capability enabled)

**Parameters**:
```erlang
#{}  % No parameters
```

### Roots List Changed

**Method**: `roots/list_changed`
**Direction**: Client → Server
**Sent When**: Workspace roots change

**Parameters**:
```erlang
#{}  % No parameters
```

### Progress

**Method**: `notifications/progress`
**Direction**: Server → Client
**Sent When**: Progress updates during long operations

**Parameters**:
```erlang
#{
  <<"progressToken">> => <<"progress-123">>,
  <<"progress">> => 45.5,    % Current progress
  <<"total">> => 100.0       % Total (optional)
}
```

### Message

**Method**: `notifications/message`
**Direction**: Server → Client
**Sent When**: Server wants to send a message

**Parameters**:
```erlang
#{
  <<"role">> => <<"user">>,  % user|assistant
  <<"content">> => #{
    <<"type">> => <<"text">>,
    <<"text">> => <<"Message content...">>
  }
}
```

### Task Status

**Method**: `notifications/tasks/status`
**Direction**: Server → Client
**Sent When**: Task status changes

**Parameters**:
```erlang
#{
  <<"taskId">> => <<"task-uuid">>,
  <<"status">> => <<"completed">>,  % pending|processing|completed|failed|cancelled
  <<"result">> => #{...},
  <<"error">> => undefined
}
```

### Initialized

**Method**: `notifications/initialized` (client sends) or `initialized` (notification)
**Direction**: Client → Server
**Sent When**: Client completes initialization

**Parameters**:
```erlang
#{}  % No parameters
```

---

## Error Codes and Responses

MCP uses JSON-RPC 2.0 error codes plus additional MCP-specific codes.

### JSON-RPC 2.0 Standard Errors

| Code | Message | Meaning |
|------|---------|---------|
| -32700 | Parse error | Invalid JSON received |
| -32600 | Invalid Request | Request is not valid JSON-RPC |
| -32601 | Method not found | Method doesn't exist |
| -32602 | Invalid params | Parameters don't match method signature |
| -32603 | Internal error | Server internal error |

### MCP Core Errors (-32001 to -32010)

| Code | Message | Meaning |
|------|---------|---------|
| -32001 | Resource not found | Resource URI not found |
| -32002 | Tool not found | Tool name doesn't exist |
| -32003 | Prompt not found | Prompt name doesn't exist |
| -32004 | Capability not supported | Server doesn't support requested capability |
| -32005 | Server not initialized | Must call initialize first |
| -32006 | Subscription failed | Failed to subscribe to resource |
| -32007 | Validation failed | Input validation error |
| -32008 | Transport error | Transport-level error |
| -32009 | Request timeout | Request took too long |
| -32010 | Rate limit exceeded | Too many requests |

### Content and Message Errors (-32011 to -32020)

| Code | Message | Meaning |
|------|---------|---------|
| -32011 | Tool description exceeds max | Description > 10,000 chars |
| -32012 | Message size exceeds maximum | Payload > 16 MB (configurable) |
| -32013 | Invalid content type | Unsupported content type |
| -32014 | Content too large | Content exceeds size limit |
| -32015 | Invalid encoding | Encoding error |
| -32016 | Binary data too large | Base64 data too large |
| -32017 | Text too long | Text exceeds limits |
| -32018 | Invalid MIME type | Unknown MIME type |
| -32019 | Unsupported media type | Media type not supported |
| -32020 | Media type not acceptable | Media type not acceptable |

### Resource and Template Errors (-32021 to -32030)

| Code | Message | Meaning |
|------|---------|---------|
| -32021 | Resource template not found | URI template not found |
| -32022 | Invalid URI | URI format invalid |
| -32023 | URI syntax error | URI doesn't parse |
| -32024 | URI too long | URI exceeds limits |
| -32025 | Resource access denied | Permission denied |
| -32026 | Resource already exists | URI already registered |
| -32027 | Resource locked | Resource is locked |
| -32028 | Resource version mismatch | Version conflict |
| -32029 | Template render failed | URI template expansion failed |
| -32030 | Invalid URI template | Template syntax invalid |

### Tool and Execution Errors (-32031 to -32040)

| Code | Message | Meaning |
|------|---------|---------|
| -32031 | Tool execution failed | Tool raised an error |
| -32032 | Tool execution timeout | Tool took too long |
| -32033 | Tool execution cancelled | Tool was cancelled |
| -32034 | Invalid tool arguments | Arguments don't match schema |
| -32035 | Tool is disabled | Tool is disabled |
| -32036 | Tool result too large | Result exceeds limits |
| -32037 | Tool not allowed | Tool not allowed in context |
| -32038 | Maximum concurrent tools exceeded | Too many concurrent calls |
| -32039 | Tool dependency failed | Tool dependency failed |
| -32040 | Tool schema invalid | Schema validation failed |

### Prompt and Sampling Errors (-32041 to -32050)

| Code | Message | Meaning |
|------|---------|---------|
| -32042 | URL elicitation required | Need user interaction (MCP 2025-11-25) |
| -32043 | Prompt argument missing | Required argument not provided |
| -32044 | Prompt render failed | Template rendering failed |
| -32045 | Invalid prompt arguments | Arguments invalid |
| -32046 | Sampling failed | LLM sampling failed |
| -32047 | Sampling timeout | LLM sampling timed out |
| -32048 | Invalid model preferences | Model preferences invalid |
| -32049 | Model not available | Requested model unavailable |
| -32050 | Sampling rate limited | Rate limit exceeded |

### Authentication Errors (-32051 to -32060)

| Code | Message | Meaning |
|------|---------|---------|
| -32051 | Authentication failed | Auth failed |
| -32052 | Authorization failed | Not authorized |
| -32053 | Invalid credentials | Bad credentials |
| -32054 | Token expired | Auth token expired |
| -32055 | Insufficient permissions | Insufficient permissions |
| -32056 | Access denied | Access denied |
| -32057 | Session expired | Session expired |
| -32058 | Session not found | Session doesn't exist |
| -32059 | Invalid token | Token is invalid |
| -32060 | Unauthorized operation | Operation not authorized |

### Pagination and Cursor Errors (-32071 to -32080)

| Code | Message | Meaning |
|------|---------|---------|
| -32071 | Invalid cursor | Cursor is invalid |
| -32072 | Cursor expired | Cursor expired |
| -32073 | Pagination not supported | Pagination unavailable |
| -32074 | Page size too large | Limit exceeds maximum |
| -32075 | Invalid page size | Limit invalid |
| -32076 | Invalid offset | Offset invalid |

### Task Errors (-32081 to -32090)

| Code | Message | Meaning |
|------|---------|---------|
| -32081 | Task not found | Task ID doesn't exist |
| -32082 | Task already exists | Task already registered |
| -32083 | Task failed | Task execution failed |
| -32084 | Task cancelled | Task was cancelled |
| -32085 | Task timeout | Task timed out |
| -32086 | Task state invalid | Task in wrong state |
| -32087 | Maximum concurrent tasks exceeded | Too many tasks |
| -32088 | Task dependency failed | Dependency failed |
| -32089 | Task result not ready | Result not available yet |
| -32090 | Task already completed | Task already finished |

### Progress Errors (-32091 to -32100)

| Code | Message | Meaning |
|------|---------|---------|
| -32091 | Invalid progress token | Token invalid |
| -32092 | Progress token expired | Token expired |
| -32093 | Progress update failed | Update failed |
| -32094 | Notification failed | Notification send failed |
| -32095 | Notification queue full | Queue full |
| -32096 | Invalid notification type | Unknown type |
| -32097 | Notification not delivered | Delivery failed |

### Completion Errors (-32110 to -32113)

| Code | Message | Meaning |
|------|---------|---------|
| -32110 | Completion not found | Completion reference invalid |
| -32111 | Invalid completion reference | Reference invalid |
| -32112 | Invalid completion argument | Argument invalid |
| -32113 | Completion failed | Completion operation failed |

### Refusal Codes (1001-1089)

These positive error codes indicate the server/tool refuses to execute:

| Code | Meaning |
|------|---------|
| 1001 | Content violates policy |
| 1002 | Violates safety guidelines |
| 1003 | Rate limit exceeded |
| 1004 | Resource constraints |
| 1005 | Permission denied |
| 1006 | Invalid input |
| 1007 | Unsupported operation |
| 1008 | Temporarily unavailable |
| 1009 | Dependency failed |
| 1010 | Custom refusal reason |

---

## Implementation Notes

### Message Size Limits

Default message size: **16 MB (16,777,216 bytes)**

Configurable per transport:
- TCP: `tcp_message_size_limit`
- HTTP: `http_body_size_limit`
- WebSocket: `ws_message_size_limit`
- SSE: `sse_event_size_limit`

### Timeouts

- Default request timeout: 5000 ms
- Initialization timeout: 30000 ms
- Configurable per client/server

### Pagination

Results that support pagination include:
- `resources/list` - List resources
- `resources/templates/list` - List resource templates
- `tools/list` - List tools
- `prompts/list` - List prompts
- `tasks/list` - List tasks

Pagination uses:
- `limit` - Number of items (default varies)
- `cursor` - Opaque pagination cursor

### Content Type MIME Types

Supported MIME types:
- `text/plain` - Plain text
- `text/markdown` - Markdown
- `application/json` - JSON data
- `image/png`, `image/jpeg`, `image/gif` - Images
- `audio/mp3`, `audio/wav`, `audio/aac` - Audio
- Custom types supported

---

## Type Definitions (Erlang Records)

### Content

```erlang
-record(mcp_content, {
    type :: binary(),              % text|image|audio|resource|resource/link
    text :: binary() | undefined,  % For text content
    data :: binary() | undefined,  % Base64 for binary
    mime_type :: binary() | undefined,
    annotations = [] :: [#mcp_annotation{}],
    resource_link = undefined :: #mcp_resource_link{} | undefined
}).
```

### Resource

```erlang
-record(mcp_resource, {
    uri :: binary(),
    name :: binary(),
    description :: binary() | undefined,
    mime_type :: binary() | undefined,
    metadata :: map() | undefined,
    audience :: binary() | undefined,
    priority :: integer() | undefined,
    last_modified :: integer() | undefined,
    annotations :: map() | undefined,
    size :: integer() | undefined
}).
```

### Tool

```erlang
-record(mcp_tool, {
    name :: binary(),
    description :: binary(),
    input_schema :: map() | undefined,    % JSON Schema
    metadata :: map() | undefined,
    experimental = undefined :: map() | undefined,
    version :: binary() | undefined,
    deprecated = false :: boolean()
}).
```

### Prompt

```erlang
-record(mcp_prompt, {
    name :: binary(),
    description :: binary() | undefined,
    arguments :: [#mcp_prompt_argument{}] | undefined,
    input_schema :: map() | undefined     % JSON Schema for validation
}).

-record(mcp_prompt_argument, {
    name :: binary(),
    description :: binary() | undefined,
    required :: boolean()
}).
```

### Task

```erlang
-record(mcp_task, {
    id :: binary(),
    status :: pending | processing | completed | failed | cancelled,
    action :: map(),
    metadata :: map(),
    result :: term() | undefined,
    error :: #mcp_error{} | undefined,
    created_at :: integer(),
    updated_at :: integer(),
    expires_at :: integer() | undefined,
    client_pid :: pid() | undefined,
    progress :: number() | undefined,
    total :: number() | undefined,
    timeout_ms :: integer()
}).
```

---

## Quick Reference: Common Methods

| Method | Direction | Purpose |
|--------|-----------|---------|
| `initialize` | C→S | Establish connection |
| `ping` | C→S | Keep-alive check |
| `resources/list` | C→S | Discover resources |
| `resources/read` | C→S | Get resource content |
| `resources/subscribe` | C→S | Subscribe to updates |
| `resources/unsubscribe` | C→S | Unsubscribe from updates |
| `tools/list` | C→S | Discover tools |
| `tools/call` | C→S | Execute tool |
| `prompts/list` | C→S | Discover prompts |
| `prompts/get` | C→S | Get prompt template |
| `sampling/createMessage` | S→C | Request LLM completion |
| `tasks/create` | C→S | Start async task |
| `tasks/list` | C→S | List all tasks |
| `tasks/get` | C→S | Get task details |
| `tasks/cancel` | C→S | Cancel running task |
| `completion/complete` | C→S | Get completions |
| `resources/updated` | S→C | Notify resource change |
| `resources/list_changed` | S→C | Notify list change |
| `tools/list_changed` | S→C | Notify tool list change |
| `prompts/list_changed` | S→C | Notify prompt list change |
| `notifications/progress` | S→C | Report progress |

---

## References

- **MCP Specification**: Version 2025-11-25
- **JSON-RPC 2.0**: https://www.jsonrpc.org/specification
- **erlmcp Source**: `/home/user/erlmcp/apps/erlmcp_core/`
- **Documentation**: `/home/user/erlmcp/docs/`
