# MCP Message Format Guide - Practical Examples

## Overview

This guide provides practical examples of MCP protocol messages for common use cases, with actual JSON payloads, Erlang code examples, and error scenarios.

---

## 1. Initialization Handshake

### 1.1 Client Sends Initialize Request

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "initialize",
  "params": {
    "protocolVersion": "2025-11-25",
    "capabilities": {
      "roots": {
        "supported": true
      },
      "sampling": {
        "modelPreferences": {
          "costPriority": "balanced",
          "speedPriority": "balanced",
          "intelligencePriority": "balanced"
        }
      }
    },
    "clientInfo": {
      "name": "claude-client",
      "version": "1.0.0"
    }
  }
}
```

**Erlang Code:**

```erlang
{ok, Pid} = erlmcp_client:start_link(client_1, erlmcp_transport_stdio),

InitParams = #{
    <<"protocolVersion">> => <<"2025-11-25">>,
    <<"capabilities">> => #{
        <<"roots">> => #{<<"supported">> => true},
        <<"sampling">> => #{
            <<"modelPreferences">> => #{
                <<"costPriority">> => <<"balanced">>,
                <<"speedPriority">> => <<"balanced">>,
                <<"intelligencePriority">> => <<"balanced">>
            }
        }
    },
    <<"clientInfo">> => #{
        <<"name">> => <<"claude-client">>,
        <<"version">> => <<"1.0.0">>
    }
},

{ok, Result} = erlmcp_client:initialize(Pid, InitParams),
io:format("Server capabilities: ~p~n", [Result]).
```

### 1.2 Server Responds with Capabilities

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
      "prompts": {
        "listChanged": true
      },
      "logging": {},
      "sampling": {},
      "roots": {},
      "completions": {
        "supported": true
      },
      "experimental": {
        "features": ["task_management"]
      }
    },
    "serverInfo": {
      "name": "erlmcp-server",
      "version": "2.2.0"
    }
  }
}
```

**Erlang Code (Server Side):**

```erlang
{ok, ServerPid} = erlmcp_server:start_link(
    test_server,
    #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = true,
            listChanged = true
        },
        tools = #mcp_tools_capability{
            listChanged = true
        },
        prompts = #mcp_prompts_capability{
            listChanged = true
        },
        completions = #mcp_capability{enabled = true}
    }
),

% Initialization is handled automatically by erlmcp_server
% Response is created internally
```

---

## 2. Resource Management

### 2.1 List Resources Request

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "resources/list",
  "params": {}
}
```

**Erlang Code:**

```erlang
% Client side
{ok, Resources} = erlmcp_client:list_resources(ClientPid),

% Server side
erlmcp_server:add_resource(ServerPid, <<"doc://readme">>,
    fun(_Uri) ->
        {ok, Content} = file:read_file("README.md"),
        Content
    }).
```

### 2.2 Resources List Response

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "result": {
    "resources": [
      {
        "uri": "doc://readme",
        "name": "README",
        "description": "Project documentation",
        "mimeType": "text/markdown"
      },
      {
        "uri": "file://data.json",
        "name": "Data File",
        "description": "Configuration data",
        "mimeType": "application/json"
      }
    ]
  }
}
```

### 2.3 Read Resource Request

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "resources/read",
  "params": {
    "uri": "doc://readme"
  }
}
```

**Erlang Code:**

```erlang
{ok, Contents} = erlmcp_client:read_resource(ClientPid, <<"doc://readme">>).
```

### 2.4 Read Resource Response

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "result": {
    "contents": [
      {
        "type": "text",
        "text": "# Project Documentation\n\nThis is the main README file."
      }
    ]
  }
}
```

### 2.5 Subscribe to Resource Updates

```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "resources/subscribe",
  "params": {
    "uri": "weather://city"
  }
}
```

**Response:**

```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "result": {}
}
```

### 2.6 Resource Updated Notification (Server → Client)

```json
{
  "jsonrpc": "2.0",
  "method": "resources/updated",
  "params": {
    "uri": "weather://city",
    "metadata": {
      "updated_at": 1640995200000,
      "version": "2024-01-01",
      "temperature": 72
    }
  }
}
```

**Erlang Code (Server Broadcasting):**

```erlang
% Subscribe client to updates
ok = erlmcp_server:subscribe_resource(ServerPid, <<"weather://city">>, ClientPid),

% Later, when resource updates:
ok = erlmcp_server:notify_resource_updated(ServerPid,
    <<"weather://city">>,
    #{
        <<"updated_at">> => erlang:system_time(millisecond),
        <<"temperature">> => 72
    }).
```

---

## 3. Tool Management

### 3.1 Register Tool on Server

```erlang
Schema = #{
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

Handler = fun(#{<<"query">> := Query} = Args) ->
    Limit = maps:get(<<"limit">>, Args, 100),
    execute_sql_query(Query, Limit)
end,

erlmcp_server:add_tool_with_schema(ServerPid, <<"sql_query">>,
    <<"Execute SQL queries on the database">>,
    Handler, Schema).
```

### 3.2 List Tools Request

```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "tools/list",
  "params": {}
}
```

### 3.3 Tools List Response

```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "result": {
    "tools": [
      {
        "name": "sql_query",
        "description": "Execute SQL queries on the database",
        "inputSchema": {
          "type": "object",
          "properties": {
            "query": {
              "type": "string",
              "description": "SQL query to execute"
            },
            "limit": {
              "type": "integer",
              "minimum": 1,
              "maximum": 1000,
              "default": 100
            }
          },
          "required": ["query"]
        }
      }
    ]
  }
}
```

### 3.4 Call Tool Request

```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "tools/call",
  "params": {
    "name": "sql_query",
    "arguments": {
      "query": "SELECT * FROM users WHERE active = true",
      "limit": 50
    }
  }
}
```

**Erlang Code:**

```erlang
{ok, Result} = erlmcp_client:call_tool(ClientPid, <<"sql_query">>, #{
    <<"query">> => <<"SELECT * FROM users WHERE active = true">>,
    <<"limit">> => 50
}).
```

### 3.5 Tool Result Response

```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Query executed successfully. Returned 42 rows."
      }
    ]
  }
}
```

### 3.6 Tool Call Error

When tool execution fails:

```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "error": {
    "code": -32031,
    "message": "Tool execution failed",
    "data": {
      "tool": "sql_query",
      "reason": "SQL syntax error: invalid column 'active'"
    }
  }
}
```

---

## 4. Prompt Management

### 4.1 Register Prompt on Server

```erlang
Arguments = [
    #mcp_prompt_argument{
        name = <<"language">>,
        description = <<"Target programming language">>,
        required = true
    },
    #mcp_prompt_argument{
        name = <<"style">>,
        description = <<"Code style guide (e.g., Google, PEP8)">>,
        required = false
    }
],

Handler = fun(#{<<"language">> := Lang} = Args) ->
    Style = maps:get(<<"style">>, Args, <<"default">>),
    [#{
        <<"role">> => <<"system">>,
        <<"content">> => generate_review_prompt(Lang, Style)
    }]
end,

erlmcp_server:add_prompt_with_args(ServerPid, <<"code_review">>,
    <<"Generate a code review prompt for a specific language">>,
    Handler, Arguments).
```

### 4.2 List Prompts Request

```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "method": "prompts/list",
  "params": {}
}
```

### 4.3 Prompts List Response

```json
{
  "jsonrpc": "2.0",
  "id": 7,
  "result": {
    "prompts": [
      {
        "name": "code_review",
        "description": "Generate a code review prompt for a specific language",
        "arguments": [
          {
            "name": "language",
            "description": "Target programming language",
            "required": true
          },
          {
            "name": "style",
            "description": "Code style guide (e.g., Google, PEP8)",
            "required": false
          }
        ]
      }
    ]
  }
}
```

### 4.4 Get Prompt Request

```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "method": "prompts/get",
  "params": {
    "name": "code_review",
    "arguments": {
      "language": "python",
      "style": "PEP8"
    }
  }
}
```

### 4.5 Get Prompt Response

```json
{
  "jsonrpc": "2.0",
  "id": 8,
  "result": {
    "messages": [
      {
        "role": "system",
        "content": "You are an expert Python code reviewer. Review the following code according to PEP8 style guidelines...",
        "annotations": {
          "audience": ["user"]
        }
      }
    ]
  }
}
```

---

## 5. Error Scenarios

### 5.1 Request Before Initialize

**Request:**

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "resources/list",
  "params": {}
}
```

**Error Response:**

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32005,
    "message": "Cannot execute operation before server initialization. Call initialize first."
  }
}
```

**Erlang Code (Rejection):**

```erlang
%% Server automatically rejects with NOT_INITIALIZED
-define(MCP_ERROR_NOT_INITIALIZED, -32005).
```

### 5.2 Method Not Found

**Request:**

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "method": "invalid/method",
  "params": {}
}
```

**Error Response:**

```json
{
  "jsonrpc": "2.0",
  "id": 2,
  "error": {
    "code": -32601,
    "message": "Method not found",
    "data": {
      "method": "invalid/method"
    }
  }
}
```

### 5.3 Invalid Parameters

**Request (Missing Required Field):**

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "method": "tools/call",
  "params": {
    "arguments": {"x": 10}
  }
}
```

**Error Response:**

```json
{
  "jsonrpc": "2.0",
  "id": 3,
  "error": {
    "code": -32602,
    "message": "Invalid params",
    "data": {
      "details": "Missing required parameter: name"
    }
  }
}
```

### 5.4 Resource Not Found

**Request:**

```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "method": "resources/read",
  "params": {
    "uri": "missing://resource"
  }
}
```

**Error Response:**

```json
{
  "jsonrpc": "2.0",
  "id": 4,
  "error": {
    "code": -32001,
    "message": "Resource not found",
    "data": {
      "uri": "missing://resource"
    }
  }
}
```

### 5.5 Tool Not Found

**Request:**

```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "method": "tools/call",
  "params": {
    "name": "nonexistent_tool",
    "arguments": {}
  }
}
```

**Error Response:**

```json
{
  "jsonrpc": "2.0",
  "id": 5,
  "error": {
    "code": -32002,
    "message": "Tool not found",
    "data": {
      "tool": "nonexistent_tool"
    }
  }
}
```

### 5.6 Tool Execution Failure

**Request:**

```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "method": "tools/call",
  "params": {
    "name": "sql_query",
    "arguments": {
      "query": "INVALID SQL"
    }
  }
}
```

**Error Response:**

```json
{
  "jsonrpc": "2.0",
  "id": 6,
  "error": {
    "code": -32031,
    "message": "Tool execution failed",
    "data": {
      "tool": "sql_query",
      "reason": "SQL syntax error: unexpected token at position 0"
    }
  }
}
```

### 5.7 Message Too Large

**Request:** (16 MB message, but limit is 10 MB for stdio)

**Error Response:**

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

### 5.8 Parse Error

**Request:** (Invalid JSON)

```
{invalid json}
```

**Error Response:**

```json
{
  "jsonrpc": "2.0",
  "id": null,
  "error": {
    "code": -32700,
    "message": "Parse error"
  }
}
```

---

## 6. Batch Requests

### 6.1 Batch Request Example

```json
[
  {
    "jsonrpc": "2.0",
    "id": 1,
    "method": "resources/list",
    "params": {}
  },
  {
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/list",
    "params": {}
  },
  {
    "jsonrpc": "2.0",
    "id": 3,
    "method": "prompts/list",
    "params": {}
  },
  {
    "jsonrpc": "2.0",
    "method": "resources/list_changed",
    "params": {}
  }
]
```

**Erlang Code:**

```erlang
% Detect batch
IsBatch = erlmcp_json_rpc:is_batch_request(JsonBinary),

% Decode batch
{ok, Messages} = erlmcp_json_rpc:decode_batch(JsonBinary),

% Encode batch response
ResponseMessages = [
    #json_rpc_response{id = 1, result = #{...}},
    #json_rpc_response{id = 2, result = #{...}},
    #json_rpc_response{id = 3, result = #{...}}
],
ResponseBinary = erlmcp_json_rpc:encode_batch(ResponseMessages).
```

### 6.2 Batch Response Example

```json
[
  {
    "jsonrpc": "2.0",
    "id": 1,
    "result": {
      "resources": [...]
    }
  },
  {
    "jsonrpc": "2.0",
    "id": 2,
    "result": {
      "tools": [...]
    }
  },
  {
    "jsonrpc": "2.0",
    "id": 3,
    "result": {
      "prompts": [...]
    }
  }
]
```

### 6.3 Batch with Mixed Success/Failure

```json
[
  {
    "jsonrpc": "2.0",
    "id": 1,
    "result": {
      "resources": [...]
    }
  },
  {
    "jsonrpc": "2.0",
    "id": 2,
    "error": {
      "code": -32601,
      "message": "Method not found",
      "data": {
        "method": "invalid/method"
      }
    }
  },
  {
    "jsonrpc": "2.0",
    "id": 3,
    "result": {
      "tools": [...]
    }
  }
]
```

---

## 7. Content Type Examples

### 7.1 Text Content

```json
{
  "type": "text",
  "text": "This is plain text content"
}
```

### 7.2 Markdown Content

```json
{
  "type": "text",
  "text": "# Heading\n\nThis is **bold** text with a [link](https://example.com)",
  "mimeType": "text/markdown"
}
```

### 7.3 JSON Content

```json
{
  "type": "text",
  "text": "{\"key\": \"value\", \"nested\": {\"field\": 123}}",
  "mimeType": "application/json"
}
```

### 7.4 Binary Content (Image)

```json
{
  "type": "image",
  "data": "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNk+M9QDwADhgGAWjR9awAAAABJRU5ErkJggg==",
  "mimeType": "image/png"
}
```

### 7.5 Binary Content (PDF)

```json
{
  "type": "document",
  "data": "JVBERi0xLjQKJeLjz9MNCjEgMCBvYmogICUgRW50cnkgcG9pbnQKPDwgL1R5cGUgL0NhdGFsb2cgL1BhZ2VzIDIgMCBSID4+IGVuZG9iag==",
  "mimeType": "application/pdf"
}
```

---

## 8. Annotations and Resource Links

### 8.1 Content with Annotations

```json
{
  "type": "text",
  "text": "User-generated content",
  "annotations": [
    {
      "name": "audience",
      "value": "user"
    },
    {
      "name": "certified",
      "value": true
    }
  ]
}
```

### 8.2 Content with Resource Link

```json
{
  "type": "text",
  "text": "Click here to download the file",
  "resource_link": {
    "uri": "s3://bucket/file.pdf",
    "name": "Document",
    "mime_type": "application/pdf",
    "size": 1048576
  }
}
```

---

## 9. Sampling and LLM Preferences

### 9.1 Sampling Request with Model Preferences

```json
{
  "jsonrpc": "2.0",
  "id": 10,
  "method": "prompts/get",
  "params": {
    "name": "code_review",
    "arguments": {"language": "python"},
    "modelPreferences": {
      "costPriority": "speed",
      "speedPriority": "balanced",
      "intelligencePriority": "cost"
    }
  }
}
```

**Erlang Code:**

```erlang
ModelPrefs = #{
    <<"costPriority">> => <<"speed">>,
    <<"speedPriority">> => <<"balanced">>,
    <<"intelligencePriority">> => <<"cost">>
},

Params = #{
    <<"name">> => <<"code_review">>,
    <<"arguments">> => #{<<"language">> => <<"python">>},
    <<"modelPreferences">> => ModelPrefs
},

{ok, Result} = erlmcp_client:get_prompt(ClientPid, Params).
```

---

## 10. Completion/Elicitation Examples

### 10.1 Completion Request

```json
{
  "jsonrpc": "2.0",
  "id": 11,
  "method": "completion/complete",
  "params": {
    "reference": {
      "type": "ref/tool",
      "name": "sql_query"
    },
    "argument": "query"
  }
}
```

### 10.2 Completion Response

```json
{
  "jsonrpc": "2.0",
  "id": 11,
  "result": {
    "completion": {
      "values": ["SELECT", "INSERT", "UPDATE", "DELETE"],
      "hasMore": true,
      "total": 4000
    }
  }
}
```

---

## 11. Progress Tracking

### 11.1 Tool Call with Progress Token

```json
{
  "jsonrpc": "2.0",
  "id": 12,
  "method": "tools/call",
  "params": {
    "name": "long_running_operation",
    "arguments": {"operation": "backup"},
    "progressToken": "progress_12345"
  }
}
```

### 11.2 Progress Notifications (from Server)

**First Progress Update:**

```json
{
  "jsonrpc": "2.0",
  "method": "progress",
  "params": {
    "progressToken": "progress_12345",
    "progress": 25,
    "total": 100
  }
}
```

**Second Progress Update:**

```json
{
  "jsonrpc": "2.0",
  "method": "progress",
  "params": {
    "progressToken": "progress_12345",
    "progress": 50,
    "total": 100
  }
}
```

**Final Notification + Tool Result:**

```json
{
  "jsonrpc": "2.0",
  "method": "progress",
  "params": {
    "progressToken": "progress_12345",
    "progress": 100,
    "total": 100
  }
}
```

Then the response:

```json
{
  "jsonrpc": "2.0",
  "id": 12,
  "result": {
    "content": [
      {
        "type": "text",
        "text": "Backup completed successfully"
      }
    ]
  }
}
```

---

## 12. Best Practices

### 12.1 Request ID Management

```erlang
% Generate unique request IDs
RequestId = erlang:system_time(nanosecond),

% For string IDs
RequestId = <<"client-", (integer_to_binary(erlang:unique_integer()))/binary>>,

% Track pending requests
PendingMap = #{1 => {timer_ref, callback_fun}},

% Clean up on response
PendingMap2 = maps:remove(ResponseId, PendingMap).
```

### 12.2 Error Handling Pattern

```erlang
handle_message(#json_rpc_response{id = Id, error = Error}) when is_map(Error) ->
    Code = maps:get(<<"code">>, Error),
    Message = maps:get(<<"message">>, Error),
    Data = maps:get(<<"data">>, Error, undefined),
    case Code of
        -32005 -> handle_not_initialized(Message);
        -32001 -> handle_resource_not_found(Data);
        -32002 -> handle_tool_not_found(Data);
        -32031 -> handle_tool_execution_failed(Data);
        -32700 -> handle_parse_error(Message);
        _ -> handle_other_error(Code, Message, Data)
    end.
```

### 12.3 Timeout Handling

```erlang
% Send request with timeout
send_request_with_timeout(Pid, Method, Params, TimeoutMs) ->
    RequestId = get_next_request_id(),
    ok = send_to_server(RequestId, Method, Params),

    % Set up timeout
    TimerRef = erlang:send_after(TimeoutMs, self(), {request_timeout, RequestId}),
    {ok, RequestId, TimerRef}.

% Handle timeout
handle_info({request_timeout, RequestId}, State) ->
    PendingMap = State#state.pending,
    case maps:get(RequestId, PendingMap, undefined) of
        undefined ->
            {noreply, State};
        {_Ref, Callback} ->
            Callback({error, timeout}),
            {noreply, State#state{pending = maps:remove(RequestId, PendingMap)}}
    end.
```

---

## 13. Encoding/Decoding Examples

### 13.1 Encoding a Request

```erlang
% Create request record
Request = #json_rpc_request{
    id = 1,
    method = <<"tools/call">>,
    params = #{
        <<"name">> => <<"sql_query">>,
        <<"arguments">> => #{
            <<"query">> => <<"SELECT * FROM users">>
        }
    }
},

% Encode to JSON
Binary = erlmcp_json_rpc:encode_request(1, <<"tools/call">>, #{
    <<"name">> => <<"sql_query">>,
    <<"arguments">> => #{<<"query">> => <<"SELECT * FROM users">>}
}).
```

### 13.2 Decoding a Request

```erlang
Json = <<"{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"sql_query"}}">>,

{ok, #json_rpc_request{id = Id, method = Method, params = Params}} =
    erlmcp_json_rpc:decode_message(Json),

io:format("ID: ~p, Method: ~p, Params: ~p~n", [Id, Method, Params]).
```

### 13.3 Encoding an Error Response

```erlang
ErrorBinary = erlmcp_json_rpc:error_tool_not_found(1, <<"missing_tool">>),

% Or with custom data
ErrorBinary = erlmcp_json_rpc:encode_error_response(
    1,
    -32031,
    <<"Tool execution failed">>,
    #{
        <<"tool">> => <<"calc">>,
        <<"reason">> => <<"Division by zero">>
    }
).
```

---

## References

- **MCP JSON-RPC Specification**: `/home/user/erlmcp/docs/protocol/MCP_JSON_RPC_SPECIFICATION.md`
- **Implementation**: `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_json_rpc.erl`
- **Tests**: `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_json_rpc_tests.erl`
