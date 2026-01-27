# Tool Progress Notifications and Completion Context

## Overview

This document describes the tool progress notification system and completion context feature for MCP (Model Context Protocol) in erlmcp. These features enable:

1. **Progress Tracking** - Monitor long-running tool calls with real-time progress updates
2. **Duration Metrics** - Automatic measurement of tool execution time
3. **Completion Context** - Intelligent argument completion based on previous values and schema constraints
4. **Reference Resolution** - Resolve argument references from context data

## Architecture

### Components

#### 1. erlmcp_progress.erl - Progress Notification Manager

A gen_server that manages progress tokens for tool calls:

- **Token Generation**: Creates unique, timestamped progress tokens
- **Tool Tracking**: Maintains ETS table of in-flight tool calls
- **Progress Updates**: Sends progress notifications to clients
- **Timeout Detection**: Automatically cleans up stale tokens
- **Client Monitoring**: Monitors client process lifespan

**Key Functions:**

```erlang
%% Generate unique progress token
Token = erlmcp_progress:generate_token().

%% Track a tool call
ok = erlmcp_progress:track_tool_call(Token, ToolName, ClientPid).

%% Send progress update
ok = erlmcp_progress:send_progress(
    Token,
    #{percentage => 50.0, message => <<"Processing...">>},
    TransportPid,
    TransportId
).

%% Get current progress
{ok, Metadata} = erlmcp_progress:get_progress(Token).

%% List active tokens
Active = erlmcp_progress:list_active_tokens().

%% Cleanup after completion
erlmcp_progress:cleanup_completed(Token).

%% Check for timeout
ok = erlmcp_progress:check_timeout(Token).
```

#### 2. erlmcp_completion_context.erl - Smart Completion System

Provides schema-driven completion with context awareness:

- **Schema Parsing**: Generates completions from JSON Schema
- **Enum Matching**: Filters enum values based on partial input
- **Pattern Support**: Matches string patterns and generates examples
- **Context Resolution**: Resolves argument references (`$.field` syntax)
- **Type-Specific Completion**: Number ranges, object properties, array items

**Key Functions:**

```erlang
%% Complete with schema
{ok, Response} = erlmcp_completion_context:complete(
    ToolName,
    #{
        partial_argument => <<"opt">>,
        arguments => #{<<"param1">> => <<"value1">>},
        context => #{}
    },
    Schema
).

%% Resolve context references
Resolved = erlmcp_completion_context:resolve_context(Arguments, Context).

%% Resolve single reference
{ok, Value} = erlmcp_completion_context:resolve_argument_reference(
    <<"$.username">>,
    Arguments,
    Context
).

%% Filter completions
Filtered = erlmcp_completion_context:filter_completions(
    [<<"apple">>, <<"banana">>, <<"apricot">>],
    <<"ap">>
).

%% Generate from schema
Completions = erlmcp_completion_context:generate_completions_from_schema(
    Schema,
    PartialArg,
    Context
).
```

### Server Integration

The `erlmcp_server.erl` module integrates progress tokens into tool call handling:

```erlang
handle_tool_call(Id, Name, Arguments, TransportId, State) ->
    % Generate progress token
    ProgressToken = erlmcp_progress:generate_token(),
    ok = erlmcp_progress:track_tool_call(ProgressToken, Name, self()),

    % Execute tool
    StartTime = erlang:system_time(millisecond),
    Result = Handler(Arguments),
    Duration = erlang:system_time(millisecond) - StartTime,

    % Build response with metadata
    Response = #{
        content => ContentList,
        _meta => #{
            progressToken => ProgressToken,
            duration => Duration
        }
    },

    send_response(State, TransportId, Id, Response),
    erlmcp_progress:cleanup_completed(ProgressToken).
```

## Protocol Examples

### Progress Notification Flow

**1. Initial Tool Call Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "req-123",
  "result": {
    "content": [{"type": "text", "text": "Result data"}],
    "_meta": {
      "progressToken": "1706302400000-12345",
      "duration": 245
    }
  }
}
```

**2. Progress Update Notification:**
```json
{
  "jsonrpc": "2.0",
  "method": "notifications/progress",
  "params": {
    "progressToken": "1706302400000-12345",
    "percentage": 50.0,
    "message": "Processing features...",
    "context": {
      "current_stage": "extraction",
      "models_processed": 3
    }
  }
}
```

**3. Completion Request:**
```json
{
  "jsonrpc": "2.0",
  "id": "req-456",
  "method": "completion/complete",
  "params": {
    "name": "analyze_document",
    "partial_argument": "pdf",
    "arguments": {
      "format": "pdf"
    },
    "context": {
      "previous_file": "document.pdf"
    }
  }
}
```

**4. Completion Response:**
```json
{
  "jsonrpc": "2.0",
  "id": "req-456",
  "result": {
    "completion": {
      "values": ["pdf", "docx", "txt"],
      "total": 3,
      "hasMore": false
    }
  }
}
```

## Progress Data Formats

### Percentage-Based Progress

```erlang
ProgressData = #{
    percentage => 75.5,
    message => <<"Processing stage 3 of 4">>
}
```

### Absolute Progress

```erlang
ProgressData = #{
    absolute => {300, 1000},  % {current, total}
    message => <<"Downloaded 300 of 1000 items">>
}
```

### Context-Rich Progress

```erlang
ProgressData = #{
    percentage => 60.0,
    message => <<"Analyzing features">>,
    context => #{
        <<"stage">> => <<"feature_extraction">>,
        <<"models">> => 5,
        <<"elapsed_time">> => 12500
    }
}
```

## JSON Schema Completion Examples

### Enum Type
```erlang
Schema = #{
    <<"type">> => <<"string">>,
    <<"enum">> => [<<"red">>, <<"green">>, <<"blue">>]
},

{ok, #{values := [<<"red">>, <<"green">>]}} =
    erlmcp_completion_context:complete(tool, Request, Schema).
```

### Object Properties
```erlang
Schema = #{
    <<"type">> => <<"object">>,
    <<"properties">> => #{
        <<"firstName">> => #{<<"type">> => <<"string">>},
        <<"lastName">> => #{<<"type">> => <<"string">>},
        <<"email">> => #{<<"type">> => <<"string">>}
    }
},

{ok, #{values := [<<"firstName">>, <<"lastName">>, <<"email">>]}} =
    erlmcp_completion_context:complete(tool, Request, Schema).
```

### Number Range
```erlang
Schema = #{
    <<"type">> => <<"integer">>,
    <<"minimum">> => 0,
    <<"maximum">> => 100
},

{ok, #{values := Values}} =
    erlmcp_completion_context:complete(tool, Request, Schema).
```

### Pattern Matching
```erlang
Schema = #{
    <<"type">> => <<"string">>,
    <<"pattern">> => <<"^[a-z_]+$">>
},

{ok, #{values := [<<"snake_case">>, <<"example_name">>]}} =
    erlmcp_completion_context:complete(tool, Request, Schema).
```

## Context Reference Resolution

### Syntax

References use the `$.fieldname` syntax to reference arguments:

```erlang
Context = #{
    <<"author_ref">> => <<"$.user_id">>,
    <<"category">> => <<"documents">>
},

Arguments = #{
    <<"user_id">> => <<"alice">>,
    <<"timestamp">> => <<"2024-01-01">>
},

Resolved = erlmcp_completion_context:resolve_context(Arguments, Context),
% Result: #{
%   <<"author_ref">> => <<"alice">>,
%   <<"category">> => <<"documents">>
% }
```

## Timeout Behavior

Progress tokens have automatic cleanup:

1. **Active Token Lifetime**: Maintained while tool call is in progress
2. **Timeout Period**: 30 seconds of inactivity triggers cleanup
3. **Automatic Monitoring**: Periodic cleanup runs every 5 seconds
4. **Client Monitoring**: Token cleanup when client process dies
5. **Explicit Cleanup**: Tool completion triggers immediate cleanup

## Testing

Comprehensive test suites are provided:

### Test Coverage

- **erlmcp_progress_tests.erl**: 10+ tests covering:
  - Token generation and uniqueness
  - Tool call tracking
  - Progress notification sending
  - Timeout detection
  - Completion context
  - Integration scenarios
  - Concurrent operations

### Running Tests

```bash
# Unit tests only
make test-unit

# Specific test suite
rebar3 eunit --module=erlmcp_progress_tests

# All tests with coverage
make test
```

## Performance Characteristics

### Progress Manager

- **ETS-based storage**: O(1) lookups for token metadata
- **Scalability**: Handles 1000+ concurrent tokens efficiently
- **Memory**: ~500 bytes per token including metadata
- **Cleanup**: Asynchronous timer-based cleanup (non-blocking)

### Completion Engine

- **Schema parsing**: O(n) where n = schema complexity
- **Filtering**: O(m) where m = completion candidates
- **Caching**: None (recompute on each request)
- **Typical response**: <10ms for most schema sizes

## Error Handling

### Progress Operations

```erlang
%% Invalid token
{error, token_not_found} = erlmcp_progress:send_progress(BadToken, Data, Pid, Id),

%% Timeout
{error, timeout} = erlmcp_progress:check_timeout(StaleToken),

%% No client PID
{error, no_client_pid} = erlmcp_progress:send_progress(Token, Data, Pid, Id)
```

### Completion Operations

```erlang
%% Invalid reference
{error, not_found} = erlmcp_completion_context:resolve_argument_reference(
    <<"$.missing">>, Arguments, Context
),

%% Completion failure
{error, {completion_failed, Reason}} =
    erlmcp_completion_context:complete(Tool, Request, BadSchema)
```

## Best Practices

### For Tool Developers

1. **Use Progress Tokens**: Always generate tokens for tools that may take >1s
2. **Send Progress Updates**: Emit progress every 1-5 seconds during long operations
3. **Clean Up Tokens**: Always cleanup tokens, even on error
4. **Include Context**: Provide context in progress updates for better UX

### For Client Applications

1. **Monitor Progress**: Listen for `notifications/progress` messages
2. **Handle Timeouts**: Implement 30+ second timeout handling
3. **Use Completions**: Leverage schema completions for better UI
4. **Parse Metadata**: Extract `progressToken` and `duration` from responses

### For Schema Design

1. **Define Enums**: Use `enum` for fixed value sets
2. **Use Patterns**: Define `pattern` for format validation
3. **Range Constraints**: Set `minimum`/`maximum` for numbers
4. **Document Properties**: Include `description` fields
5. **Type Hints**: Always specify `type` field

## Integration Example

```erlang
%% Tool with progress support
my_tool(Arguments) ->
    Token = get_token_from_context(),

    % Step 1
    erlmcp_progress:send_progress(Token,
        #{percentage => 25.0, message => <<"Step 1: Initialization">>},
        Transport, TransportId),
    step1_work(Arguments),

    % Step 2
    erlmcp_progress:send_progress(Token,
        #{percentage => 50.0, message => <<"Step 2: Processing">>},
        Transport, TransportId),
    step2_work(Arguments),

    % Step 3
    erlmcp_progress:send_progress(Token,
        #{percentage => 75.0, message => <<"Step 3: Analysis">>},
        Transport, TransportId),
    step3_work(Arguments),

    {ok, <<"Result">>}.

%% Completion-aware tool
filtered_tool(Arguments) ->
    case maps:get(<<"format">>, Arguments) of
        undefined ->
            %% Use schema for completion
            case erlmcp_completion_context:complete(
                <<"filtered_tool">>,
                #{partial_argument => <<"">>, arguments => #{}},
                #{<<"enum">> => [<<"json">>, <<"xml">>, <<"csv">>]}
            ) of
                {ok, #{values := Formats}} ->
                    select_best_format(Formats);
                _ ->
                    <<"json">>
            end;
        Format ->
            process_with_format(Arguments, Format)
    end.
```

## Troubleshooting

### Token Not Found

**Symptom**: `{error, token_not_found}` when sending progress

**Causes**:
- Token was already cleaned up
- Invalid token format
- Token timeout exceeded

**Solution**: Verify token generation and tracking within same process

### Progress Not Received

**Symptom**: Client doesn't receive `notifications/progress`

**Causes**:
- Transport PID not available
- Progress timeout elapsed
- Client process died

**Solution**: Implement client monitoring and handle connection loss

### Timeout Errors

**Symptom**: `{error, timeout}` on token check

**Causes**:
- No progress updates for 30+ seconds
- Long-running operation without updates

**Solution**: Send progress updates at regular intervals

## References

- MCP Specification: https://modelcontextprotocol.io/
- JSON Schema: https://json-schema.org/
- Erlang/OTP Pattern: https://www.erlang.org/doc/

