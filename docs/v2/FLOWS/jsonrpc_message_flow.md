# JSON-RPC Message Flow in erlmcp

**Status**: Derived from source code trace + grep analysis
**Version**: 0.6.0
**Last Updated**: 2026-01-27

## Overview

This document traces the complete journey of a JSON-RPC message through the erlmcp codebase, from initial binary reception through decoding, routing, handling, and encoding of responses.

The message flow follows the **JSON-RPC 2.0 specification** with **MCP-specific extensions** for resources, tools, prompts, and tasks. Three message types are supported:
- **Requests** (requires `id` + `method` + optional `params`)
- **Responses** (requires `id` + either `result` or `error`)
- **Notifications** (requires `method`, no `id`)

---

## End-to-End Message Flow (Sequence Diagram)

```
┌─────────────────────────────────────────────────────────────────────────┐
│                         TRANSPORT LAYER                                 │
└─────────────────────────────────────────────────────────────────────────┘
         ▲                                                   │
         │ Binary JSON                                       │ Binary JSON
         │ (UTF-8)                                          │ (UTF-8)
         │                                                  ▼
    ┌────┴─────────────────────────────────────────────────┐
    │  erlmcp_transport_stdio/tcp/http/ws                   │
    │  - receive()                                          │
    │  - extract_message_lines()                            │
    │  - {transport_message, Binary} → gen_server:info()    │
    └────┬─────────────────────────────────────────────────┘
         │
         │ Async message {transport_message, Binary}
         │
    ┌────▼────────────────────────────────────────────────────────────┐
    │  erlmcp_client.erl / erlmcp_server.erl                          │
    │  handle_info({transport_message, Data}, State)                  │
    │  ─────────────────────────────────────────────────────────────  │
    │  Line 414 (client): erlmcp_client.erl:handle_info/2             │
    └────┬──────────────────────────────────────────────────────────────┘
         │
         │ Binary data
         │
    ┌────▼───────────────────────────────────────────────────────────┐
    │  erlmcp_json_rpc:decode_message(Data)                          │
    │  ─────────────────────────────────────────────────────────────  │
    │  File: src/erlmcp_json_rpc.erl:90                              │
    │  1. Size validation (Gap #45: Message Size Limits)             │
    │  2. JSX decode (jsx:decode/2)                                  │
    │  3. Delegate parsing to erlmcp_message_parser:parse_json_rpc/1 │
    └────┬──────────────────────────────────────────────────────────┘
         │
         │ Decoded map: #{
         │   <<"jsonrpc">> => <<"2.0">>,
         │   <<"id">> => 1,
         │   <<"method">> => <<"tools/list">>,
         │   <<"params">> => #{}
         │ }
         │
    ┌────▼───────────────────────────────────────────────────────────┐
    │  erlmcp_message_parser:parse_json_rpc(Map)                     │
    │  ─────────────────────────────────────────────────────────────  │
    │  File: src/erlmcp_message_parser.erl:39                        │
    │  1. validate_jsonrpc_version(Data)                             │
    │  2. parse_by_type(Data)                                        │
    │     - Request: method + id detected                            │
    │     - Response: (result | error) + id detected                 │
    │     - Notification: method, no id                              │
    └────┬───────────────────────────────────────────────────────────┘
         │
         │ ┌─────────────────────────────────────────────────────────┐
         │ │ PARSED MESSAGE RECORD (one of three types):            │
         │ │                                                          │
         │ │ Request:                                                │
         │ │   #json_rpc_request{                                    │
         │ │     id = 1,                                             │
         │ │     method = <<"tools/list">>,                          │
         │ │     params = #{}                                        │
         │ │   }                                                      │
         │ │                                                          │
         │ │ Response:                                               │
         │ │   #json_rpc_response{                                   │
         │ │     id = 1,                                             │
         │ │     result = [...tools...],                             │
         │ │     error = undefined                                   │
         │ │   }                                                      │
         │ │                                                          │
         │ │ Notification:                                           │
         │ │   #json_rpc_notification{                               │
         │ │     method = <<"resources/updated">>,                   │
         │ │     params = #{<<"uri">> => <<"...>>}                   │
         │ │   }                                                      │
         │ └─────────────────────────────────────────────────────────┘
         │
         ├─────────────────┬──────────────────────┬──────────────────┐
         │                 │                      │                  │
         │ (Request)       │ (Response)           │ (Notification)  │
         │                 │                      │                  │
    ┌────▼──────┐    ┌─────▼────┐         ┌────────▼────────┐
    │  HANDLE    │    │ HANDLE   │         │  HANDLE         │
    │ REQUEST    │    │ RESPONSE │         │ NOTIFICATION    │
    └────┬──────┘    └─────┬────┘         └────────┬────────┘
         │                 │                      │
         │ (Client side)   │ (Client side)        │ (Both sides)
         │                 │                      │
    ┌────▼──────────────────────────────────────────────────────────┐
    │  erlmcp_client:handle_info/2 (Line 414-425)                   │
    │  ─────────────────────────────────────────────────────────────  │
    │  REQUEST NOT HANDLED ON CLIENT                                │
    │  (Client only sends requests)                                 │
    │                                                                │
    │  RESPONSE HANDLING (Line 416-419):                            │
    │    {ok, #json_rpc_response{id = Id, result = Result}}         │
    │      → handle_response(Id, {ok, Result}, State)               │
    │                                                                │
    │    {ok, #json_rpc_response{id = Id, error = Error}}           │
    │      → handle_response(Id, {error, Error}, State)             │
    │                                                                │
    │  NOTIFICATION HANDLING (Line 420-421):                        │
    │    {ok, #json_rpc_notification{method = M, params = P}}       │
    │      → handle_notification(M, P, State)                       │
    │                                                                │
    │  Function: handle_response/3 (Line 581)                       │
    │  ─────────────────────────────────────────────────────────────  │
    │  Correlates response to pending request via ID:               │
    │    pending_requests = #{RequestId => {RequestType, FromPid}} │
    │  Takes from map: maps:take(Id, State#state.pending_requests) │
    │  Replies to original caller: gen_server:reply(From, Result)  │
    │                                                                │
    │  Function: handle_notification/3 (Line 636)                  │
    │  ─────────────────────────────────────────────────────────────  │
    │  Special notifications:                                       │
    │    - <<"notifications/initialized">> → phase transition       │
    │    - <<"sampling/createMessage">> → spawn_handler()          │
    │    - <<"resources/updated">> → trigger_handler()             │
    │    - <<"prompts/listChanged">> → trigger_handler()           │
    │    - <<"tools/listChanged">> → trigger_handler()             │
    └────┬──────────────────────────────────────────────────────────┘
         │
         └──────────────────► Response sent back to client caller
                               (Original request process unblocked)


┌─────────────────────────────────────────────────────────────────────────┐
│                      SERVER-SIDE HANDLING                               │
│                    (erlmcp_server.erl)                                   │
└─────────────────────────────────────────────────────────────────────────┘

    REQUEST RECEIVED
         │
    ┌────▼──────────────────────────────────────────────────────────────┐
    │  erlmcp_server.erl → erlmcp_message_handler:process_message/3     │
    │  ─────────────────────────────────────────────────────────────────  │
    │  File: src/erlmcp_message_handler.erl:31                          │
    │  Handles: requests + notifications                                │
    │  Returns: {ok, State} | {error, Reason} | {reply, Binary, State}  │
    └────┬───────────────────────────────────────────────────────────────┘
         │
         ├─────────────┬──────────────────┬───────────────────┐
         │             │                  │                   │
    ┌────▼─────┐ ┌────▼──────┐    ┌──────▼──────┐    ┌───────▼────────┐
    │ initialize│ │resources/ │    │ tools/list  │    │prompts/list    │
    │           │ │list       │    │             │    │                │
    │ handler   │ │ handler   │    │ handler     │    │ handler        │
    │ (Line 105)│ │ (Line 107)│    │ (Line 110)  │    │ (Line 113)     │
    └────┬─────┘ └────┬──────┘    └──────┬──────┘    └───────┬────────┘
         │             │                  │                   │
         │ Each calls erlmcp_json_rpc:encode_response/2        │
         │                                                     │
         ▼─────────────────────────────────────────────────────▼
    ┌──────────────────────────────────────────────────────────┐
    │  erlmcp_json_rpc:encode_response(Id, Result)             │
    │  ──────────────────────────────────────────────────────── │
    │  File: src/erlmcp_json_rpc.erl:54                        │
    │  Creates: #json_rpc_response{id = Id, result = Result}   │
    │  Calls: encode_message(Response)                         │
    │    → build_message_map(Response) (Line 308)              │
    │    → jsx:encode(Map) returns binary JSON                 │
    └──────────────────────────────────────────────────────────┘
         │
         │ Binary: {
         │   "jsonrpc": "2.0",
         │   "id": 1,
         │   "result": {
         │     "tools": [{"name": "...", ...}]
         │   }
         │ }
         │
    ┌────▼──────────────────────────────────────────────────────┐
    │  Transport:send(TransportPid, BinaryResponse)             │
    │  ──────────────────────────────────────────────────────── │
    │  File: src/erlmcp_transport_*.erl (stdio/tcp/http)        │
    │  Sends to connected peer                                  │
    └────┬────────────────────────────────────────────────────────┘
         │
         └──► Wire transmission to client
```

---

## Data Structure Transformations at Each Stage

### Stage 1: Reception (Transport Layer)

**Transport**: stdio, TCP, HTTP, WebSocket
**Input**: Wire bytes
**Output**: Binary (UTF-8 encoded JSON)

```erlang
%% erlmcp_transport_stdio.erl:98
read_loop(TransportPid, OwnerPid, MaxSize) ->
    case io:get_line("") of
        {error, _} -> ok;
        eof -> ok;
        Line ->
            Binary = iolist_to_binary(Line),
            OwnerPid ! {transport_message, Binary},
            read_loop(TransportPid, OwnerPid, MaxSize)
    end.
```

**Result**: Async message `{transport_message, Binary}` sent to owner process (client/server).

---

### Stage 2: Decoding (JSON-RPC Parser)

**File**: `src/erlmcp_json_rpc.erl:90`
**Function**: `decode_message(Binary) → {ok, Message} | {error, Reason}`

```erlang
decode_message(Json, TransportType) when is_binary(Json) ->
    case erlmcp_message_size:validate_message_size(TransportType, Json) of
        ok ->
            try jsx:decode(Json, [return_maps]) of
                Data when is_map(Data) ->
                    erlmcp_message_parser:parse_json_rpc(Data);
                _ -> {error, {invalid_json, not_object}}
            catch
                error:badarg -> {error, {parse_error, invalid_json}}
            end;
        {error, {message_too_large, ErrorResponse}} ->
            {error, {message_too_large, ErrorResponse}}
    end.
```

**Data Transform**:
```
Binary JSON
  ↓
  jsx:decode(Json, [return_maps])
  ↓
Erlang Map (decoded but unparsed)
  #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,
    <<"method">> => <<"tools/list">>,
    <<"params">> => #{}
  }
```

---

### Stage 3: Message Type Detection & Parsing

**File**: `src/erlmcp_message_parser.erl:39`
**Function**: `parse_json_rpc(Map) → {ok, Record} | {error, Reason}`

**Fast-path type detection** (Line 57-71):

```erlang
parse_by_type(#{?JSONRPC_FIELD_ID := Id, ?JSONRPC_FIELD_METHOD := Method} = Data) ->
    %% Request: has both id and method
    parse_request(Id, Method, Data);

parse_by_type(#{?JSONRPC_FIELD_ID := Id, ?JSONRPC_FIELD_RESULT := Result}) ->
    %% Response with result: has id and result
    parse_response(Id, Result, undefined);

parse_by_type(#{?JSONRPC_FIELD_ID := Id, ?JSONRPC_FIELD_ERROR := Error}) ->
    %% Response with error: has id and error
    parse_response(Id, undefined, Error);

parse_by_type(#{?JSONRPC_FIELD_METHOD := Method} = Data) ->
    %% Notification: has method but no id
    parse_notification(Method, Data);

parse_by_type(_) ->
    {error, {invalid_request, unknown_message_type}}.
```

**Data Transform**:
```
Erlang Map
  ↓
  Pattern match on required fields
  ↓
One of three record types:

1. #json_rpc_request{
     id = json_rpc_id(),
     method = binary(),
     params = json_rpc_params()
   }

2. #json_rpc_response{
     id = json_rpc_id(),
     result = term() | undefined,
     error = map() | undefined
   }

3. #json_rpc_notification{
     method = binary(),
     params = json_rpc_params()
   }
```

**Record Definitions** (from `include/erlmcp.hrl:156-175`):

```erlang
-record(json_rpc_request, {
    id :: json_rpc_id(),
    method :: binary(),
    params :: json_rpc_params()
}).

-record(json_rpc_response, {
    id :: json_rpc_id(),
    result :: term() | undefined,
    error :: map() | undefined
}).

-record(json_rpc_notification, {
    method :: binary(),
    params :: json_rpc_params()
}).
```

---

### Stage 4: Request/Response Routing & Handling

#### Client Side: Handling Responses

**File**: `src/erlmcp_client.erl:414`

```erlang
handle_info({transport_message, Data}, State) ->
    case erlmcp_json_rpc:decode_message(Data) of
        {ok, #json_rpc_response{id = Id, result = Result, error = undefined}} ->
            handle_response(Id, {ok, Result}, State);
        {ok, #json_rpc_response{id = Id, error = Error}} ->
            handle_response(Id, {error, Error}, State);
        {ok, #json_rpc_notification{method = Method, params = Params}} ->
            handle_notification(Method, Params, State);
        {error, Reason} ->
            logger:error("Failed to decode message: ~p", [Reason]),
            {noreply, State}
    end.
```

**Request-Response Correlation** (File: `src/erlmcp_client.erl:581`):

```erlang
handle_response(Id, Result, State) ->
    case maps:take(Id, State#state.pending_requests) of
        {{initialize, From}, NewPending} ->
            gen_server:reply(From, Result),
            %% Extract server capabilities from result
            ServerCapabilities = extract_server_capabilities(Result),
            NewState = State#state{
                pending_requests = NewPending,
                capabilities = ServerCapabilities,
                initialized = true,
                phase = initializing
            },
            {noreply, NewState};
        {{_RequestType, From}, NewPending} ->
            gen_server:reply(From, Result),
            {noreply, State#state{pending_requests = NewPending}};
        error ->
            logger:warning("Received response for unknown request ID: ~p", [Id]),
            {noreply, State}
    end.
```

**Data Flow**:
```
#json_rpc_response{id = 1, result = {...}}
  ↓
maps:take(1, pending_requests)
  ↓ (lookup found)
{RequestType, FromPid} (e.g., {list_resources, <0.42.0>})
  ↓
gen_server:reply(FromPid, {ok, Result})
  ↓
Original caller process unblocked with response
```

#### Client Side: Handling Notifications

**File**: `src/erlmcp_client.erl:636`

```erlang
%% INITIALIZED notification transitions to initialized phase
handle_notification(<<"notifications/initialized">> = _Method, _Params,
                    #state{phase = initializing} = State) ->
    logger:info("Client received initialized notification, transitioning..."),
    {noreply, State#state{phase = initialized}};

%% Sampling request from server
handle_notification(<<"sampling/createMessage">> = Method, Params, State) ->
    spawn_handler(State#state.sampling_handler, Method, Params),
    {noreply, State};

%% Resource update notification
handle_notification(<<"resources/updated">> = Method, Params, State) ->
    %% Trigger registered resource update handlers
    {noreply, State};

%% ... other notifications
```

#### Server Side: Request Handling

**File**: `src/erlmcp_message_handler.erl:31`

```erlang
process_message(TransportId, Data, State) ->
    case erlmcp_json_rpc:decode_message(Data, default) of
        {ok, #json_rpc_request{method = Method, params = Params, id = Id}} ->
            handle_request(Method, Params, Id, TransportId, State);
        {ok, #json_rpc_notification{method = Method, params = Params}} ->
            handle_notification(Method, Params, TransportId, State);
        {error, Reason} ->
            {error, Reason}
    end.
```

**Method Router** (File: `src/erlmcp_message_handler.erl:103-118`):

```erlang
handle_request(<<"initialize">>, _Params, Id, _TransportId, State) ->
    {reply, erlmcp_json_rpc:encode_response(Id, #{<<"result">> => <<"ok">>}), State};

handle_request(<<"resources/list">>, _Params, Id, _TransportId, State) ->
    Resources = maps:keys(State#mcp_server_state.resources),
    {reply, erlmcp_json_rpc:encode_response(Id, #{<<"resources">> => Resources}), State};

handle_request(<<"tools/list">>, _Params, Id, _TransportId, State) ->
    Tools = maps:keys(State#mcp_server_state.tools),
    {reply, erlmcp_json_rpc:encode_response(Id, #{<<"tools">> => Tools}), State};

handle_request(Method, _Params, Id, _TransportId, State) ->
    Error = erlmcp_json_rpc:error_method_not_found(Id, Method),
    {reply, Error, State}.
```

---

### Stage 5: Response Encoding

**File**: `src/erlmcp_json_rpc.erl:54`

```erlang
encode_response(Id, Result) ->
    Response = #json_rpc_response{
        id = Id,
        result = Result
    },
    encode_message(Response).
```

**Encoding Pipeline**:

```erlang
encode_message(Message) ->
    Map = build_message_map(Message),  %% Line 299
    jsx:encode(Map).
```

**Map Building** (Line 308-313):

```erlang
build_message_map(#json_rpc_response{id = Id, result = Result, error = Error}) ->
    Base = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,  %% "2.0"
        ?JSONRPC_FIELD_ID => encode_id(Id)
    },
    add_result_or_error(Base, Result, Error).

add_result_or_error(Map, _Result, Error) when is_map(Error) ->
    Map#{?JSONRPC_FIELD_ERROR => Error};
add_result_or_error(Map, Result, undefined) ->
    Map#{?JSONRPC_FIELD_RESULT => Result}.
```

**Data Transform**:
```
Erlang Record
  #json_rpc_response{
    id = 1,
    result = #{<<"tools">> => [...]},
    error = undefined
  }
  ↓
  build_message_map/1
  ↓
Erlang Map
  #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,
    <<"result">> => #{<<"tools">> => [...]}
  }
  ↓
  jsx:encode/1
  ↓
Binary JSON (UTF-8)
  <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"tools\":[...]}}">>
```

---

### Stage 6: Transport Transmission

**File**: `src/erlmcp_transport_stdio.erl:49`

```erlang
send(_, Message) ->
    case is_binary(Message) of
        true ->
            io:format("~s~n", [Message]);  %% Write to stdout
        false ->
            io:format("~s~n", [iolist_to_binary(Message)])
    end,
    ok.
```

**Similar implementations** for TCP, HTTP, WebSocket transports.

---

## Error Handling Pipeline

### Error Code Validation

**File**: `src/erlmcp_json_rpc.erl:176`

```erlang
validate_error_code(Code) when is_integer(Code) ->
    lists:member(Code, ?VALID_ERROR_CODES).
```

**Valid Error Codes** (from `include/erlmcp.hrl:48-66`):
- `-32700`: Parse error
- `-32600`: Invalid Request
- `-32601`: Method not found
- `-32602`: Invalid params
- `-32603`: Internal error
- `-32001` to `-32010`: MCP-specific errors (resources, tools, prompts, etc.)

### Error Response Functions

**File**: `src/erlmcp_json_rpc.erl:185-256`

```erlang
%% Standard JSON-RPC errors
error_method_not_found(Id, Method) ->
    Data = #{<<"method">> => Method},
    encode_error_response(Id, ?JSONRPC_METHOD_NOT_FOUND,
                         ?JSONRPC_MSG_METHOD_NOT_FOUND, Data).

%% MCP-specific errors
error_tool_not_found(Id, ToolName) ->
    Data = #{<<"tool">> => ToolName},
    encode_error_response(Id, ?MCP_ERROR_TOOL_NOT_FOUND,
                         ?MCP_MSG_TOOL_NOT_FOUND, Data).

error_message_too_large(Id, MaxSize) when is_integer(MaxSize), MaxSize > 0 ->
    Data = #{
        <<"maxSize">> => MaxSize,
        <<"unit">> => <<"bytes">>
    },
    encode_error_response(Id, ?MCP_ERROR_MESSAGE_TOO_LARGE,
                         ?MCP_MSG_MESSAGE_TOO_LARGE, Data).
```

---

## Message Type Details

### Type 1: Requests

**Characteristics**:
- Has `id` (required for correlation)
- Has `method` (required)
- May have `params` (optional)

**Example**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/list",
  "params": {}
}
```

**Parsed Record** (Line 78-87 of `erlmcp_message_parser.erl`):
```erlang
parse_request(Id, Method, Data) when is_binary(Method) ->
    Params = maps:get(?JSONRPC_FIELD_PARAMS, Data, undefined),
    {ok, #json_rpc_request{
        id = decode_id(Id),
        method = Method,
        params = validate_params(Params)
    }}.
```

**Server Handlers**:
- `<<"initialize">>` → initialization handshake
- `<<"resources/list">>` → list available resources
- `<<"resources/read">>` → read resource content
- `<<"tools/list">>` → list available tools
- `<<"tools/call">>` → execute tool
- `<<"prompts/list">>` → list available prompts
- `<<"prompts/get">>` → get prompt content
- Custom methods → method_not_found error

---

### Type 2: Responses

**Characteristics**:
- Has `id` (correlates to request)
- Has either `result` (success) or `error` (failure), never both

**Success Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "tools": [
      {"name": "calculator", "description": "..."}
    ]
  }
}
```

**Error Response**:
```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32601,
    "message": "Method not found",
    "data": {"method": "invalid_method"}
  }
}
```

**Parsed Records** (Lines 90-96 of `erlmcp_message_parser.erl`):
```erlang
parse_response(Id, Result, Error) ->
    {ok, #json_rpc_response{
        id = decode_id(Id),
        result = Result,
        error = Error
    }}.
```

---

### Type 3: Notifications

**Characteristics**:
- No `id` (fire-and-forget, no response expected)
- Has `method` (required)
- May have `params` (optional)

**Example**:
```json
{
  "jsonrpc": "2.0",
  "method": "notifications/initialized",
  "params": {}
}
```

**Parsed Record** (Lines 100-107 of `erlmcp_message_parser.erl`):
```erlang
parse_notification(Method, Data) when is_binary(Method) ->
    Params = maps:get(?JSONRPC_FIELD_PARAMS, Data, undefined),
    {ok, #json_rpc_notification{
        method = Method,
        params = validate_params(Params)
    }}.
```

**Common Notifications**:
- `<<"notifications/initialized">>` → server/client ready
- `<<"resources/updated">>` → resource content changed
- `<<"resources/listChanged">>` → resource list changed
- `<<"tools/listChanged">>` → tool list changed
- `<<"prompts/listChanged">>` → prompt list changed
- `<<"sampling/createMessage">>` → server requests model sampling
- `<<"logging/message">>` → server logging message

---

## Key Modules & Their Roles

| Module | File | Responsibility | Lines |
|--------|------|-----------------|-------|
| `erlmcp_json_rpc` | `src/erlmcp_json_rpc.erl` | Encoding/decoding, error creation | 45-373 |
| `erlmcp_message_parser` | `src/erlmcp_message_parser.erl` | Fast-path message type detection, record creation | 39-125 |
| `erlmcp_message_handler` | `src/erlmcp_message_handler.erl` | Server-side request routing, handler invocation | 31-127 |
| `erlmcp_client` | `src/erlmcp_client.erl` | Client request correlation, response handling | 414-612 |
| `erlmcp_server` | `src/erlmcp_server.erl` | Server state management (resources, tools, prompts) | 160+ |
| `erlmcp_registry` | `src/erlmcp_registry.erl` | Central routing registry (gproc-based) | 1-100 |
| `erlmcp_router` | `src/erlmcp_router.erl` | Load balancing, circuit breakers, metrics | 1-100 |
| `erlmcp_transport_*` | `src/erlmcp_transport_*.erl` | Physical I/O (stdio, TCP, HTTP, WebSocket) | 1-300 |

---

## Request ID Correlation

**Problem**: How does the client match responses to requests when multiple requests are in-flight?

**Solution**: Request ID map stored in client state (File: `src/erlmcp_client.erl:44-61`):

```erlang
-record(state, {
    %% ... other fields ...
    request_id = 1 :: request_id(),
    pending_requests = #{} :: #{request_id() => {atom(), pid()}},
    %% Maps: request_id -> {RequestType, OriginalCaller}
    %% Example: 1 -> {list_resources, <0.42.0>}
    %% ... rest of fields ...
}).
```

**Flow**:
1. Client sends request with ID `N`:
   ```erlang
   NewState = State#state{
       request_id = SafeNextId,
       pending_requests = maps:put(RequestId, RequestInfo, State#state.pending_requests)
   }
   ```

2. Response arrives with matching ID `N`:
   ```erlang
   case maps:take(N, State#state.pending_requests) of
       {{RequestType, FromPid}, NewPending} ->
           gen_server:reply(FromPid, {ok, Result})
   end
   ```

3. Original caller unblocked immediately.

**Safety Checks** (Line 489-495 of `erlmcp_client.erl`):
- Request ID collision detection
- Request ID overflow protection
- Logging of orphaned responses (unknown IDs)

---

## Message Size Limits (Gap #45)

**File**: `src/erlmcp_message_size.erl`
**Transport Types**: stdio (16 MB), TCP (64 MB), HTTP (100 MB)

**Validation Point** (Line 99 of `erlmcp_json_rpc.erl`):

```erlang
case erlmcp_message_size:validate_message_size(TransportType, Json) of
    ok ->
        %% Proceed with decoding
        try jsx:decode(Json, [return_maps]) of
            %% ...
    {error, {message_too_large, ErrorResponse}} ->
        {error, {message_too_large, ErrorResponse}}
end.
```

**Error Response**: Returns JSON-RPC error with code `-32012`.

---

## Batch Requests

**File**: `src/erlmcp_json_rpc.erl:119-288`
**Function**: `decode_batch(Binary) → {ok, [Message]} | {error, Reason}`

**Usage** (File: `src/erlmcp_client.erl:151-163`):

```erlang
with_batch(Client, BatchFun) when is_function(BatchFun, 1) ->
    BatchId = make_ref(),
    ok = gen_server:call(Client, {start_batch, BatchId}),
    try
        Result = BatchFun(BatchId),
        {ok, _Count} = gen_server:call(Client, {execute_batch, BatchId}),
        Result
    catch
        Class:Reason:Stacktrace ->
            gen_server:call(Client, {cancel_batch, BatchId}),
            erlang:raise(Class, Reason, Stacktrace)
    end.
```

**Batch Format**:
```json
[
  {"jsonrpc": "2.0", "id": 1, "method": "tools/list", "params": {}},
  {"jsonrpc": "2.0", "id": 2, "method": "resources/list", "params": {}},
  {"jsonrpc": "2.0", "method": "notifications/initialized", "params": {}}
]
```

---

## Performance Optimizations

### Hot-Path Optimizations (erlmcp_message_parser.erl)

1. **Inline Pattern Matching**: Type detection via direct map pattern match (lines 58-70)
   - Request: `id + method` → fast match
   - Response: `id + result/error` → fast match
   - Notification: `method, no id` → fast match

2. **No Intermediate Data Structures**: Direct record creation from map fields

3. **Fast Parameter Validation**: Simple type checks (line 121-125)
   ```erlang
   validate_params(undefined) -> undefined;
   validate_params(Params) when is_map(Params) -> Params;
   validate_params(Params) when is_list(Params) -> Params;
   validate_params(_) -> undefined.
   ```

### Message Parsing Benchmark (v1.5.0)

From `CLAUDE.md`:
- **Registry**: 553K msg/sec
- **Queue**: 971K msg/sec
- **Core ops (in-memory)**: 2.69M ops/sec
- **Sustained load** (60M ops/30s): 372K msg/sec effective

---

## Derived Patterns

### 1. Type Detection Via Pattern Matching
The parser uses Erlang's pattern matching (not if-then-else) for maximum performance:
```erlang
parse_by_type(#{?JSONRPC_FIELD_ID := Id, ?JSONRPC_FIELD_METHOD := Method} = Data) ->
    %% Compiler optimizes to single hash table lookup
    %% Extremely fast for request detection
    parse_request(Id, Method, Data).
```

### 2. Error Code Validation via List Membership
All error codes validated against a predefined list (include/erlmcp.hrl:48-66):
```erlang
validate_error_code(Code) when is_integer(Code) ->
    lists:member(Code, ?VALID_ERROR_CODES).
```

### 3. Request-Response Correlation via Maps
Client maintains map of in-flight requests indexed by ID:
```erlang
pending_requests = #{RequestId => {RequestType, FromPid}},
%% When response arrives:
maps:take(ResponseId, pending_requests) -> {RequestInfo, UpdatedMap}
```

### 4. Handler Registration Pattern
Both client and server support pluggable handlers:
- Client: notification handlers per method
- Server: handler functions for resources, tools, prompts
- Handlers are stored as `fun/1` or `{Module, Function}`

### 5. Notification Dispatch (Fire-and-Forget)
No response correlation needed:
- Client spawns handler concurrently
- Server processes and moves on (no reply sent)

---

## References

- **MCP Specification**: https://modelcontextprotocol.io/
- **JSON-RPC 2.0**: https://www.jsonrpc.org/specification
- **Architecture Doc**: `docs/architecture.md`
- **OTP Patterns**: `docs/otp-patterns.md`
- **API Reference**: `docs/api-reference.md`

---

## Changelog

| Date | Change |
|------|--------|
| 2026-01-27 | Initial flow diagram and detailed stage-by-stage trace |
| | Documented all 6 stages: reception, decode, parse, route, handle, encode |
| | Added record definitions and type transformations |
| | Included error handling, batch requests, performance notes |
