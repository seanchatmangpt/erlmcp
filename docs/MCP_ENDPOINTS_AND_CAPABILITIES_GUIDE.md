# MCP Endpoints and Capabilities Guide

**MCP Protocol Version**: 2025-11-25
**erlmcp Implementation**: v2.1.0
**Document Date**: 2026-01-31

## Table of Contents

1. [Protocol Overview](#protocol-overview)
2. [Capability Negotiation Flow](#capability-negotiation-flow)
3. [Core Methods and Endpoints](#core-methods-and-endpoints)
4. [Resources API](#resources-api)
5. [Tools API](#tools-api)
6. [Prompts API](#prompts-api)
7. [Sampling API](#sampling-api)
8. [Tasks API](#tasks-api)
9. [Completion API](#completion-api)
10. [Notification Methods](#notification-methods)
11. [Error Handling Flow](#error-handling-flow)
12. [Error Codes Reference](#error-codes-reference)

---

## Protocol Overview

The Model Context Protocol (MCP) uses **JSON-RPC 2.0** for communication over various transports (stdio, TCP, HTTP, WebSocket, SSE). All messages follow the JSON-RPC 2.0 specification.

### Message Flow Architecture

```mermaid
graph TB
    subgraph "Client Side"
        C[erlmcp_client]
        C_INIT[initialize]
        C_REQ[send_request]
        C_NOTIF[send_notification]
    end

    subgraph "Transport Layer"
        T_STDIO[stdio]
        T_TCP[tcp]
        T_HTTP[http]
        T_WS[websocket]
        T_SSE[sse]
    end

    subgraph "Server Side"
        S[erlmcp_server]
        S_HANDLER[request_handlers]
        S_CAPS[capabilities]
    end

    subgraph "Handler Modules"
        H_RES[resources_handler]
        H_TOOL[tools_handler]
        H_PROMPT[prompts_handler]
    end

    C_INIT --> S
    C_REQ --> S
    C_NOTIF --> S
    S --> S_HANDLER
    S_HANDLER --> H_RES
    S_HANDLER --> H_TOOL
    S_HANDLER --> H_PROMPT

    C -.->|stdio| T_STDIO
    C -.->|tcp| T_TCP
    C -.->|http| T_HTTP
    S -.->|stdio| T_STDIO
    S -.->|tcp| T_TCP
    S -.->|http| T_HTTP

    style C fill:#e1f5fe
    style S fill:#c8e6c9
    style S_CAPS fill:#fff9c4
```

---

## Capability Negotiation Flow

Capability negotiation is the first step in establishing an MCP connection. The client and server exchange information about supported features.

### Negotiation Sequence Diagram

```mermaid
sequenceDiagram
    participant Client as erlmcp_client
    participant Server as erlmcp_server
    participant Caps as Capability Module
    participant Resources as Resource Handler
    participant Tools as Tool Handler
    participant Prompts as Prompt Handler

    Note over Client,Prompts: MCP Capability Negotiation Flow

    %% Phase 1: Initialize Request
    Client->>Server: initialize request
    Note right of Client: {<br/>  "jsonrpc": "2.0",<br/>  "id": "init-1",<br/>  "method": "initialize",<br/>  "params": {<br/>    "protocolVersion": "2025-11-25",<br/>    "capabilities": {<br/>      "resources": {},<br/>      "tools": {},<br/>      "prompts": {},<br/>      "logging": {}<br/>    },<br/>    "clientInfo": {<br/>      "name": "my-client",<br/>      "version": "1.0.0"<br/>    }<br/>  }<br/>}

    activate Server
    Server->>Caps: erlmcp_capabilities:negotiate(ClientCaps)
    activate Caps

    %% Phase 2: Server Capability Resolution
    Caps->>Caps: Parse client capabilities
    Caps->>Caps: Intersect with server capabilities
    Note right of Caps: ServerCaps ∩ ClientCaps<br/>= NegotiatedCaps

    %% Resource Capability Check
    opt Client declares resources support
        Caps->>Resources: check_resources_capability()
        Resources-->>Caps: {supported, true}
    end

    %% Tool Capability Check
    opt Client declares tools support
        Caps->>Tools: check_tools_capability()
        Tools-->>Caps: {supported, true}
    end

    %% Prompt Capability Check
    opt Client declares prompts support
        Caps->>Prompts: check_prompts_capability()
        Prompts-->>Caps: {supported, true}
    end

    %% Logging Capability Check
    opt Client declares logging support
        Caps->>Caps: configure_logging_level()
    end

    %% Sampling Capability Check
    opt Server offers sampling support
        Caps->>Caps: add_sampling_capability()
    end

    %% Progress Support Check
    opt Client declares progress support
        Caps->>Caps: enable_progress_tokens()
    end

    Caps-->>Server: {ok, NegotiatedCaps}
    deactivate Caps

    Server->>Server: Store negotiated capabilities in state
    Server->>Server: Initialize enabled handlers

    %% Phase 3: Initialize Response
    Server-->>Client: initialize response
    Note left of Server: {<br/>  "jsonrpc": "2.0",<br/>  "id": "init-1",<br/>  "result": {<br/>    "protocolVersion": "2025-11-25",<br/>    "capabilities": {<br/>      "resources": {<br/>        "subscribe": true,<br/>        "listChanged": true<br/>      },<br/>      "tools": {},<br/>      "prompts": {<br/>        "listChanged": true<br/>      }<br/>    },<br/>    "serverInfo": {<br/>      "name": "erlmcp-server",<br/>      "version": "2.1.0"<br/>    }<br/>  }<br/>}
    deactivate Server

    %% Phase 4: Initialized Notification (REQUIRED)
    Note over Client,Prompts: Critical: Client must send initialized notification

    Client->>Server: initialized notification
    Note right of Client: {<br/>  "jsonrpc": "2.0",<br/>  "method": "notifications/initialized"<br/>}

    activate Server
    Server->>Server: Mark session as ready for requests
    Server-->>Client: ready state
    deactivate Server
```

**See also:** [Detailed Capability Negotiation](./diagrams/protocol/capability-negotiation.mmd)

### Server Capability Declaration

Servers declare capabilities during initialization:

```erlang
#{<<"protocolVersion">> => <<"2025-11-25">>,
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
    <<"sampling">> => #{}
  },
  <<"serverInfo">> => #{
    <<"name">> => <<"erlmcp-server">>,
    <<"version">> => <<"2.1.0">>
  }
}.
```

---

## Core Methods and Endpoints

### Initialize

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
    <<"version">> => <<"2.1.0">>
  },
  <<"instructions">> => <<"Server instructions (optional)">>
}
```

### Ping

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

---

## Resources API

### Resource API Flow

```mermaid
graph LR
    subgraph "Resource Operations"
        LIST[resources/list]
        READ[resources/read]
        SUB[resources/subscribe]
        UNSUB[resources/unsubscribe]
        TEMPLIST[resources/templates/list]
    end

    subgraph "Resource Handlers"
        H_STATIC[static_handler]
        H_DYNAMIC[dynamic_handler]
        H_TEMPLATE[template_handler]
    end

    subgraph "Notifications"
        N_UPDATED[resources/updated]
        N_LIST_CHANGED[resources/list_changed]
    end

    LIST --> H_STATIC
    READ --> H_DYNAMIC
    TEMPLIST --> H_TEMPLATE
    SUB -.->|enables| N_UPDATED
    SUB -.->|enables| N_LIST_CHANGED
    UNSUB -.->|disables| N_UPDATED

    style LIST fill:#e1f5fe
    style READ fill:#e1f5fe
    style SUB fill:#fff9c4
    style N_UPDATED fill:#c8e6c9
    style N_LIST_CHANGED fill:#c8e6c9
```

### List Resources

**Method**: `resources/list`
**Direction**: Client → Server
**Capability Required**: `resources`

**Request**:
```erlang
#{
  <<"cursor">> => <<"optional-cursor">>,
  <<"limit">> => 100
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
  <<"cursor">> => <<"next-cursor">>
}
```

### Subscribe to Resource

**Method**: `resources/subscribe`
**Direction**: Client → Server
**Capability Required**: `resources.subscribe = true`

**Request**:
```erlang
#{
  <<"uri">> => <<"weather://city">>
}
```

**Response**: `{}` (empty object)

**Notifications Sent**:
- `resources/updated` - When subscribed resource changes
- `resources/list_changed` - When resource list changes

---

## Tools API

### Tool API Flow

```mermaid
graph TB
    subgraph "Tool Operations"
        LIST[tools/list]
        CALL[tools/call]
    end

    subgraph "Tool Execution"
        VALIDATE[Validate Arguments]
        EXECUTE[Execute Handler]
        RESPONSE[Encode Response]
    end

    subgraph "Progress Tracking"
        PROG_TOKEN[Progress Token]
        PROG_NOTIF[progress notification]
    end

    LIST --> |discover| CALL
    CALL --> VALIDATE
    VALIDATE --> EXECUTE
    EXECUTE --> RESPONSE

    CALL -.->|optional| PROG_TOKEN
    EXECUTE -.->|during| PROG_NOTIF

    style LIST fill:#e1f5fe
    style CALL fill:#fff9c4
    style VALIDATE fill:#ffe0b2
    style EXECUTE fill:#c8e6c9
```

### List Tools

**Method**: `tools/list`
**Direction**: Client → Server
**Capability Required**: `tools`

**Request**:
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
      <<"description">> => <<"Execute SQL queries">>,
      <<"inputSchema">> => #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
          <<"query">> => #{
            <<"type">> => <<"string">>,
            <<"description">> => <<"SQL query to execute">>
          }
        },
        <<"required">> => [<<"query">>]
      }
    }
  ]
}
```

### Call Tool

**Method**: `tools/call`
**Direction**: Client → Server
**Capability Required**: `tools`

**Request**:
```erlang
#{
  <<"name">> => <<"sql_query">>,
  <<"arguments">> => #{
    <<"query">> => <<"SELECT * FROM users">>
  }
}
```

**Response**:
```erlang
#{
  <<"content">> => [
    #{
      <<"type">> => <<"text">>,
      <<"text">> <<"[{id:1, name:\"Alice\"}, ...]">>
    }
  ],
  <<"isError">> => false
}
```

---

## Prompts API

### Prompt API Flow

```mermaid
graph LR
    subgraph "Prompt Operations"
        LIST[prompts/list]
        GET[prompts/get]
    end

    subgraph "Prompt Rendering"
        RENDER[Render Template]
        ARGS[Process Arguments]
        MESSAGES[Generate Messages]
    end

    LIST --> |discover| GET
    GET --> ARGS
    ARGS --> RENDER
    RENDER --> MESSAGES

    style LIST fill:#e1f5fe
    style GET fill:#fff9c4
    style RENDER fill:#c8e6c9
```

### List Prompts

**Method**: `prompts/list`
**Direction**: Client → Server
**Capability Required**: `prompts`

**Response**:
```erlang
#{
  <<"prompts">> => [
    #{
      <<"name">> => <<"code_review">>,
      <<"description">> => <<"Review code for quality">>,
      <<"arguments">> => [
        #{
          <<"name">> => <<"language">>,
          <<"description">> => <<"Programming language">>,
          <<"required">> => true
        }
      ]
    }
  ]
}
```

---

## Sampling API

**Method**: `sampling/createMessage`
**Direction**: Client → Server (request sent from server to client)
**Capability Required**: Client must have `sampling` capability
**Since**: MCP 2025-11-25

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
    <<"costPriority">> => 0.5,
    <<"speedPriority">> => 0.3,
    <<"intelligencePriority">> => 0.8,
    <<"temperature">> => 0.7,
    <<"maxTokens">> => 1024
  },
  <<"systemPrompt">> => <<"You are a helpful assistant...">>,
  <<"includedContext">> => <<"this_request">>
}
```

**Response from client**:
```erlang
#{
  <<"model">> => <<"claude-3-sonnet-20240229">>,
  <<"stopReason">> => <<"end_turn">>,
  <<"content">> => [
    #{
      <<"type">> => <<"text">>,
      <<"text">> => <<"Here's what I recommend...">>
    }
  ]
}
```

---

## Tasks API

**Since**: MCP 2025-11-25
**Capability Required**: `tasks`

Tasks enable asynchronous long-running operations with progress tracking.

### Task Lifecycle State Diagram

```mermaid
stateDiagram-v2
    [*] --> Pending: tasks/create

    Pending --> Processing: Start execution
    Processing --> Completed: Success
    Processing --> Failed: Error
    Processing --> Cancelled: tasks/cancel

    Completed --> [*]
    Failed --> [*]
    Cancelled --> [*]

    Processing --> Processing: progress update

    note right of Processing
        Progress notifications sent
        via notifications/progress
    end note
```

### Create Task

**Method**: `tasks/create`
**Direction**: Client → Server

**Request**:
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
    <<"timeout">> => 30000
  }
}
```

**Response**:
```erlang
#{
  <<"taskId">> => <<"task-uuid-here">>
}
```

---

## Completion API

**Since**: MCP 2025-11-25
**Capability Required**: `completions`

The Completion API provides code/text completion for tools, resources, and prompts.

### Complete

**Method**: `completion/complete`
**Direction**: Client → Server

**Request**:
```erlang
#{
  <<"ref">> => #{
    <<"type">> => <<"tool">>,
    <<"name">> => <<"sql_query">>
  },
  <<"argument">> => #{
    <<"name">> => <<"query">>,
    <<"value">> => <<"SELECT * FR">>
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
    }
  ],
  <<"hasMore">> => false,
  <<"total">> => 1
}
```

---

## Notification Methods

Servers send notifications to clients about state changes.

### Notification Flow

```mermaid
graph LR
    subgraph "Server Events"
        EV_RES[Resource Change]
        EV_TOOL[Tool Change]
        EV_PROMPT[Prompt Change]
    end

    subgraph "Notifications"
        N_RES[resources/updated]
        N_RES_LIST[resources/list_changed]
        N_TOOL_LIST[tools/list_changed]
        N_PROMPT_LIST[prompts/list_changed]
        N_PROG[notifications/progress]
    end

    subgraph "Client Handlers"
        H_RES[resource_handler]
        H_TOOL[tool_handler]
        H_PROMPT[prompt_handler]
        H_PROG[progress_handler]
    end

    EV_RES --> N_RES
    EV_RES --> N_RES_LIST
    EV_TOOL --> N_TOOL_LIST
    EV_PROMPT --> N_PROMPT_LIST

    N_RES --> H_RES
    N_RES_LIST --> H_RES
    N_TOOL_LIST --> H_TOOL
    N_PROMPT_LIST --> H_PROMPT
    N_PROG --> H_PROG

    style EV_RES fill:#fff9c4
    style N_RES fill:#c8e6c9
    style N_PROG fill:#e1f5fe
```

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

### Progress

**Method**: `notifications/progress`
**Direction**: Server → Client
**Sent When**: Progress updates during long operations

**Parameters**:
```erlang
#{
  <<"progressToken">> => <<"progress-123">>,
  <<"progress">> => 45.5,
  <<"total">> => 100.0
}
```

---

## Error Handling Flow

### Error Processing Diagram

```mermaid
sequenceDiagram
    participant Client as erlmcp_client
    participant Server as erlmcp_server
    participant Validator as Error Validator
    participant Circuit as Circuit Breaker
    participant Monitor as Health Monitor
    participant Registry as Refusal Registry

    Note over Client,Registry: MCP Error Handling and Refusal Code Flow

    %% JSON-RPC Parse Errors
    Client->>Server: Malformed JSON (invalid syntax)
    activate Server
    Server->>Validator: validate_json(Binary)
    Validator-->>Server: {error, parse_error}
    Server-->>Client: Parse Error (-32700)
    Note left of Server: {<br/>  "jsonrpc": "2.0",<br/>  "id": null,<br/>  "error": {<br/>    "code": -32700,<br/>    "message": "Parse error"<br/>  }<br/>}
    deactivate Server

    %% JSON-RPC Method Not Found
    Client->>Server: Call unknown method
    activate Server
    Server->>Validator: validate_method(Method)
    Validator-->>Server: {error, method_not_found}
    Server-->>Client: Method Not Found (-32601)
    deactivate Server

    %% MCP Tool Not Found
    Client->>Server: tools/call (unknown tool)
    activate Server
    Server->>Registry: lookup_tool(ToolId)
    Registry-->>Server: {error, not_found}
    Server-->>Client: Tool Not Found (-32002)
    Note left of Server: {<br/>  "error": {<br/>    "code": -32002,<br/>    "message": "Tool not found",<br/>    "data": {"toolId": "unknown_tool"}<br/>  }<br/>}
    deactivate Server

    %% Rate Limited
    Client->>Server: High-frequency requests
    activate Server
    Server->>Circuit: check_rate_limit(ClientID)
    activate Circuit
    Circuit->>Circuit: Calculate request rate
    Circuit-->>Server: {error, rate_exceeded}
    deactivate Circuit
    Server-->>Client: Rate Limited (-32010)
    deactivate Server
```

**See also:** [Detailed Error Handling Flow](./diagrams/protocol/error-handling.mmd)

---

## Error Codes Reference

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

### Tool Errors (-32031 to -32040)

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

## Session Lifecycle

### Session State Machine

```mermaid
stateDiagram-v2
    [*] --> Initializing: start_link(Options)

    state Initializing {
        [*] --> LoadConfig
        LoadConfig --> InitTransport
        InitTransport --> EstablishConnection
        EstablishConnection --> SendInitialize
        SendInitialize --> WaitForInitialized
        WaitForInitialized --> [*]
    }

    Initializing --> Connected: initialized received

    state Connected {
        [*] --> Active

        Active --> ResourceOperations: list/read/subscribe
        Active --> ToolOperations: call_tool
        Active --> PromptOperations: list/get

        ResourceOperations --> Active
        ToolOperations --> Active
        PromptOperations --> Active

        Active --> Subscribing: subscribe_resource
        Subscribing --> Active: subscription_confirmed
    }

    Connected --> Paused: pause_requested
    Paused --> Connected: resume_requested

    Connected --> Reconnecting: transport_error
    Reconnecting --> Connected: reconnect_success
    Reconnecting --> Failed: reconnect_failed(max_retries)

    Connected --> GracefulShutdown: shutdown notification
    GracefulShutdown --> [*]: cleanup_complete

    Connected --> AbnormalShutdown: critical_error
    AbnormalShutdown --> [*]: cleanup_complete

    note right of Connected
        Session Persistence:
        - ETS: In-memory
        - DETS: Disk-based
        - Mnesia: Cluster
    end note

    state Reconnecting {
        [*] --> Backoff
        Backoff --> ReconnectAttempt
        ReconnectAttempt --> Success
        ReconnectAttempt --> Failure

        Success --> [*]
        Failure --> IncrementRetryCount
        IncrementRetryCount --> CheckMaxRetries

        CheckMaxRetries --> Backoff: below_max
        CheckMaxRetries --> [*]: exceeded_max
    }

    Connected --> SessionValidation: periodic_health_check
    SessionValidation --> Connected: healthy
    SessionValidation --> Failed: unhealthy(timeout)

    note right of Reconnecting
        Failover Modes:
        - Local: Restart process
        - Remote: Mnesia replica
        - Recovery: Load from DETS
    end note

    state Subscribing {
        [*] --> RegisterSubscription
        RegisterSubscription --> SetupWatcher
        SetupWatcher --> ConfirmSubscription
        ConfirmSubscription --> [*]
    }

    state GracefulShutdown {
        [*] --> UnsubscribeResources
        UnsubscribeResources --> DrainPendingRequests
        DrainPendingRequests --> CloseTransport
        CloseTransport --> PersistSession
        PersistSession --> TerminateProcess
        TerminateProcess --> [*]
    }

    state AbnormalShutdown {
        [*] --> LogError
        LogError --> PersistCrashDump
        PersistCrashDump --> TerminateProcess
        TerminateProcess --> [*]
    }
```

**See also:** [Detailed Session Lifecycle](./diagrams/protocol/session-lifecycle.mmd)

---

## Quick Reference: Common Methods

| Method | Direction | Purpose | Capability |
|--------|-----------|---------|------------|
| `initialize` | C→S | Establish connection | Required |
| `ping` | C→S | Keep-alive check | - |
| `resources/list` | C→S | Discover resources | resources |
| `resources/read` | C→S | Get resource content | resources |
| `resources/subscribe` | C→S | Subscribe to updates | resources.subscribe |
| `resources/unsubscribe` | C→S | Unsubscribe from updates | resources.subscribe |
| `tools/list` | C→S | Discover tools | tools |
| `tools/call` | C→S | Execute tool | tools |
| `prompts/list` | C→S | Discover prompts | prompts |
| `prompts/get` | C→S | Get prompt template | prompts |
| `sampling/createMessage` | S→C | Request LLM completion | sampling (client) |
| `tasks/create` | C→S | Start async task | tasks |
| `tasks/list` | C→S | List all tasks | tasks |
| `tasks/get` | C→S | Get task details | tasks |
| `tasks/cancel` | C→S | Cancel running task | tasks |
| `completion/complete` | C→S | Get completions | completions |
| `resources/updated` | S→C | Notify resource change | - |
| `resources/list_changed` | S→C | Notify list change | resources.listChanged |
| `tools/list_changed` | S→C | Notify tool list change | tools.listChanged |
| `prompts/list_changed` | S→C | Notify prompt list change | prompts.listChanged |
| `notifications/progress` | S→C | Report progress | - |

---

## References

- **MCP Specification**: Version 2025-11-25
- **JSON-RPC 2.0**: https://www.jsonrpc.org/specification
- **erlmcp API Reference**: ./api-reference.md
- **Protocol Specification**: ./protocol/MCP_JSON_RPC_SPECIFICATION.md
- **Mermaid Diagrams**: ./diagrams/
