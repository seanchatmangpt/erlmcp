# MCP 2025-11-25 Specification Gaps - Requirements Document

**Project**: erlmcp (Erlang/OTP MCP SDK)
**Version**: 0.6.0 → 0.7.0 target
**Date**: 2026-01-29
**Status**: Specification Phase (SPARC Phase 1)

## Executive Summary

This document defines requirements for implementing missing features from the MCP 2025-11-25 specification. The analysis identifies **5 major feature gaps** that must be implemented to achieve full specification compliance.

## Gap Analysis Summary

| Gap ID | Feature | Priority | Complexity | Status |
|--------|---------|----------|------------|--------|
| #107-142 | Task Management | CRITICAL | High | Not Implemented |
| #142 | Request Cancellation | HIGH | Medium | Partially Implemented |
| #Progress | Progress Tokens | HIGH | Low | Implemented |
| #Sampling | Sampling Capability | MEDIUM | Medium | Partially Implemented |
| #Completion | Completions API | HIGH | High | Not Implemented |
| #Elicitation | Elicitation Features | MEDIUM | Medium | Not Implemented |

---

## Gap #1: Task Management (Tasks #107-142)

### Overview
Implement asynchronous task lifecycle management per MCP 2025-11-25 specification.

### Functional Requirements

#### 1.1 Task Creation (tasks/create)

**Method**: `tasks/create`

**Request Parameters**:
```erlang
#{
    <<"id">> => binary(),                    % Unique task identifier (optional)
    <<"status">> => <<"pending">>,           % Initial status
    <<"action">> => binary(),                % Action description
    <<"timestamp">> => integer()             % Creation timestamp
}
```

**Response**:
```erlang
#{
    <<"taskId">> => binary(),                % Generated or provided task ID
    <<"status">> => <<"pending">>,           % Initial status
    <<"createdAt">> => integer()             % ISO 8601 timestamp
}
```

**Error Codes**:
- `-32082` (MCP_ERROR_TASK_ALREADY_EXISTS): Task ID already exists
- `-32087` (MCP_ERROR_MAX_CONCURRENT_TASKS): Concurrent task limit exceeded

#### 1.2 Task Listing (tasks/list)

**Method**: `tasks/list`

**Request Parameters**:
```erlang
#{
    <<"cursor">> => binary() | undefined,   % Pagination cursor
    <<"limit">> => integer()                 % Max results (default: 50)
}
```

**Response**:
```erlang
#{
    <<"tasks">> => [#{
        <<"taskId">> => binary(),
        <<"status">> => binary(),
        <<"createdAt">> => integer()
    }],
    <<"nextCursor">> => binary() | undefined
}
```

#### 1.3 Task Retrieval (tasks/get)

**Method**: `tasks/get`

**Request Parameters**:
```erlang
#{
    <<"taskId">> => binary()                 % Task identifier
}
```

**Response**:
```erlang
#{
    <<"taskId">> => binary(),
    <<"status">> => <<"pending">> | <<"working">> | <<"completed">> | <<"failed">> | <<"cancelled">>,
    <<"action">> => binary(),
    <<"result">> => term() | undefined,      % Present if completed
    <<"error">> => term() | undefined,       % Present if failed
    <<"createdAt">> => integer(),
    <<"updatedAt">> => integer()
}
```

**Error Codes**:
- `-32081` (MCP_ERROR_TASK_NOT_FOUND): Task ID does not exist

#### 1.4 Task Result (tasks/result)

**Method**: `tasks/result`

**Request Parameters**:
```erlang
#{
    <<"taskId">> => binary()                 % Task identifier
}
```

**Response**:
```erlang
#{
    <<"taskId">> => binary(),
    <<"result">> => term(),                  % Task result
    <<"completedAt">> => integer()
}
```

**Error Codes**:
- `-32081` (MCP_ERROR_TASK_NOT_FOUND): Task does not exist
- `-32089` (MCP_ERROR_TASK_RESULT_NOT_READY): Task not complete

#### 1.5 Task Cancellation (tasks/cancel)

**Method**: `tasks/cancel`

**Request Parameters**:
```erlang
#{
    <<"taskId">> => binary(),
    <<"reason">> => binary() | undefined     % Cancellation reason
}
```

**Response**:
```erlang
#{
    <<"taskId">> => binary(),
    <<"status">> => <<"cancelled">>,
    <<"cancelledAt">> => integer()
}
```

**Error Codes**:
- `-32081` (MCP_ERROR_TASK_NOT_FOUND): Task does not exist
- `-32090` (MCP_ERROR_TASK_ALREADY_COMPLETED): Task already complete

### State Machine

```erlang
%% Task State Transitions
pending   -> working   (task starts)
pending   -> cancelled (cancelled before start)
working   -> completed (success)
working   -> failed    (error)
working   -> cancelled (cancelled during execution)
```

### Non-Functional Requirements

- **Performance**: Task creation < 10ms, task retrieval < 50ms
- **Scalability**: Support 10,000+ concurrent tasks per node
- **Persistence**: Tasks survive server restart (optional via Mnesia)
- **Monitoring**: Emit telemetry events for state transitions

### Integration Points

- **erlmcp_cancellation**: Use for task cancellation
- **erlmcp_progress**: Report progress during task execution
- **erlmcp_server**: Register tasks/create handler
- **erlmcp_client**: Provide task management API

---

## Gap #2: Request Cancellation (Task #142)

### Overview
Implement cancellation of in-flight MCP requests per specification.

### Functional Requirements

#### 2.1 Cancel Request (requests/cancel)

**Method**: `requests/cancel`

**Request Parameters**:
```erlang
#{
    <<"requestId">> => binary() | integer(), % Original request ID
    <<"reason">> => binary() | undefined      % Cancellation reason
}
```

**Response**:
```erlang
#{
    <<"requestId">> => binary() | integer(),
    <<"cancelled">> => true,
    <<"timestamp">> => integer()
}
```

#### 2.2 Cancellation Notification

**Method**: `notifications/cancelled`

**Notification**:
```erlang
#{
    <<"jsonrpc">> => <<"2.0">>,
    <<"method">> => <<"notifications/cancelled">>,
    <<"params">> => #{
        <<"requestId">> => binary() | integer(),
        <<"reason">> => binary(),
        <<"timestamp">> => integer()
    }
}
```

### Implementation Status

**Existing Module**: `erlmcp_cancellation.erl` (implemented)

**Integration Required**:
1. Register `requests/cancel` handler in `erlmcp_server`
2. Emit `notifications/cancelled` from `erlmcp_client`
3. Wire cancellation tokens to tool calls, resource reads

### Edge Cases

- Cancel already completed request → Return error, don't crash
- Cancel with invalid request ID → Return not found
- Concurrent cancellation attempts → Idempotent handling
- Transport failure during cancellation → Best-effort notification

---

## Gap #3: Progress Tokens

### Overview
Incremental progress reporting during long-running operations.

### Functional Requirements

#### 3.1 Progress Token in _meta Field

**Request Format**:
```erlang
#{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,
    <<"method">> => <<"tools/call">>,
    <<"params">> => #{
        <<"name">> => <<"long_running_tool">>,
        <<"arguments">> => #{...},
        <<"_meta">> => #{
            <<"progressToken">> => 12345      % Client-provided token
        }
    }
}
```

#### 3.2 Progress Notification

**Method**: `notifications/progress`

**Notification**:
```erlang
#{
    <<"jsonrpc">> => <<"2.0">>,
    <<"method">> => <<"notifications/progress">>,
    <<"params">> => #{
        <<"progressToken">> => 12345,
        <<"progress">> => 50.0,               % 0-100 or absolute value
        <<"total">> => 100.0,                 % Total for percentage
        <<"message">> => <<"Processing...">>
    }
}
```

### Implementation Status

**Existing Module**: `erlmcp_progress.erl` (implemented)

**Integration Required**:
1. Extract `_meta.progressToken` from request params in `erlmcp_server`
2. Send progress notifications during tool execution
3. Handle progress token expiry and cleanup

---

## Gap #4: Sampling Capability

### Overview
LLM message sampling via `sampling/createMessage` with model preferences.

### Functional Requirements

#### 4.1 Create Message (sampling/createMessage)

**Method**: `sampling/createMessage`

**Request Parameters**:
```erlang
#{
    <<"messages">> => [#{
        <<"role">> => <<"user">> | <<"assistant">> | <<"system">>,
        <<"content">> => binary() | #{      % Text or multimodal content
            <<"type">> => <<"text">> | <<"image">> | <<"resource">>,
            <<"text">> => binary(),
            <<"data">> => binary(),
            <<"mimeType">> => binary()
        }
    }],
    <<"modelPreferences">> => #{
        <<"costPriority">> => float(),       % 0.0-1.0
        <<"speedPriority">> => float(),      % 0.0-1.0
        <<"intelligencePriority">> => float() % 0.0-1.0
    } | undefined,
    <<"includeContext">> => boolean(),       % Include resource context
    <<"maxTokens">> => integer() | undefined,
    <<"temperature">> => float() | undefined, % 0.0-2.0
    <<"stopSequences">> => [binary()] | undefined
}
```

**Response**:
```erlang
#{
    <<"role">> => <<"assistant">>,
    <<"content"> => #{
        <<"type">> => <<"text">>,
        <<"text">> => <<"Generated response...">>
    },
    <<"model">> => <<"gpt-4">>,
    <<"stopReason">> => <<"end_turn">> | <<"max_tokens">> | <<"stop_sequence">>,
    <<"_meta">> => #{
        <<"tokensUsed">> => integer(),
        <<"cost">> => float()
    }
}
```

**Error Codes**:
- `-32046` (MCP_ERROR_SAMPLING_FAILED): Provider error
- `-32047` (MCP_ERROR_SAMPLING_TIMEOUT): Request timeout
- `-32048` (MCP_ERROR_INVALID_MODEL_PREFERENCES): Invalid preferences
- `-32049` (MCP_ERROR_MODEL_NOT_AVAILABLE): Model unavailable
- `-32050` (MCP_ERROR_SAMPLING_RATE_LIMITED): Rate limit exceeded

### Implementation Status

**Existing Module**: `erlmcp_sampling.erl` (partial)

**Missing Features**:
1. Model preferences validation (cost/speed/intelligence priority)
2. Multimodal content support (images, resources)
3. Include context from resources
4. Provider abstraction for multiple LLM backends

### Provider Interface

```erlang
-callback create_message(Messages :: [map()], Params :: map()) ->
    {ok, map()} | {error, term()}.

-callback available_models() -> [binary()].

-callback estimate_cost(Messages :: [map()], Model :: binary()) ->
    {ok, float()} | {error, term()}.
```

---

## Gap #5: Completions API

### Overview
Text completion and suggestion generation.

### Functional Requirements

#### 5.1 Complete (completion/complete)

**Method**: `completion/complete`

**Request Parameters**:
```erlang
#{
    <<"ref"> => #{
        <<"type">> => <<"ref/resource">>,
        <<"uri">> => binary()                % Resource URI
    } | #{
        <<"type">> => <<"ref/command">>,
        <<"name">> => binary()               % Command name
    },
    <<"argument"> => #{
        <<"name">> => binary(),              % Argument name
        <<"value">> => binary()              % Partial value
    }
}
```

**Response**:
```erlang
#{
    <<"completion"> => #{
        <<"values">> => [binary()],          % Suggested completions
        <<"total">> => integer(),            % Total available
        <<"hasMore">> => boolean()           % More completions available
    }
}
```

**Error Codes**:
- `-32021` (MCP_ERROR_RESOURCE_TEMPLATE_NOT_FOUND): Invalid ref
- `-32022` (MCP_ERROR_INVALID_URI): Invalid resource URI

### Use Cases

1. **Resource Path Completion**: Complete file paths, database keys
2. **Tool Argument Completion**: Suggest enum values, known IDs
3. **Command Name Completion**: Available commands/namespaces

### Algorithm Requirements

- **Performance**: < 100ms response time
- **Relevance**: Rank by frequency, recency, context
- **Caching**: Cache completions for performance
- **Fuzzy Matching**: Support typo tolerance

---

## Gap #6: Elicitation Features

### Overview
URL elicitation for client-side permission flows.

### Functional Requirements

#### 6.1 Create Elicitation (elicitation/create)

**Method**: `elicitation/create`

**Request Parameters**:
```erlang
#{
    <<"elicitations">> => [#{
        <<"type">> => <<"url">>,
        <<"name">> => binary(),              % Elicitation identifier
        <<"description">> => binary()        % User-facing description
    }]
}
```

**Response**:
```erlang
#{
    <<"elicitations">> => [#{
        <<"type">> => <<"url">>,
        <<"name">> => binary(),
        <<"url">> => binary()                % Generated URL
    }]
}
```

#### 6.2 Elicitation Complete Notification

**Method**: `notifications/elicitation/complete`

**Notification**:
```erlang
#{
    <<"jsonrpc">> => <<"2.0">>,
    <<"method">> => <<"notifications/elicitation/complete">>,
    <<"params">> => #{
        <<"elicitations">> => [#{
            <<"type">> => <<"url">>,
            <<"name">> => binary(),
            <<"value">> => binary()          % User-provided value
        }]
    }
}
```

### Use Cases

1. **OAuth Flows**: Request user authorization URL
2. **File Selection**: Request user-selected file path
3. **Configuration**: Request user-provided settings

### Security Requirements

- **URL Validation**: Ensure URLs use https:// scheme
- **Expiry**: Elicitations expire after timeout (default: 5 min)
- **Rate Limiting**: Limit elicitation creation per client
- **Audit Logging**: Log all elicitation requests

---

## Cross-Cutting Concerns

### Error Handling

All new features MUST:
1. Use defined MCP error codes (-32001 to -32099)
2. Include descriptive error messages
3. Provide error context in `data` field
4. Log errors with telemetry

### Validation

All new features MUST:
1. Validate input parameters against JSON Schema
2. Return specific validation error codes
3. Sanitize user input to prevent injection
4. Check capability flags before processing

### Performance

All new features MUST:
1. Complete critical operations in < 100ms (p99)
2. Support high concurrency (1000+ req/s)
3. Use non-blocking I/O
4. Implement backpressure for overload protection

### Testing

All new features MUST have:
1. Unit tests (EUnit) with ≥80% coverage
2. Integration tests (CT) for end-to-end flows
3. Property-based tests (Proper) for data structures
4. Performance benchmarks for critical paths

### Documentation

All new features MUST include:
1. API documentation in `docs/api-reference.md`
2. Protocol documentation in `docs/protocol.md`
3. Examples in `examples/` directory
4. Type specifications in module headers

---

## Implementation Phases

### Phase 1: Foundation (Week 1-2)
1. Task management core (erlmcp_task_manager.erl)
2. Completion API (erlmcp_completion.erl)
3. Elicitation API (erlmcp_elicitation.erl)

### Phase 2: Integration (Week 3-4)
1. Wire handlers in erlmcp_server.erl
2. Wire clients in erlmcp_client.erl
3. Update erlmcp_json_rpc.erl for new message types

### Phase 3: Testing & Validation (Week 5-6)
1. Comprehensive test suite
2. Performance benchmarks
3. Integration tests
4. Documentation updates

### Phase 4: Production Readiness (Week 7-8)
1. Dialyzer validation (0 warnings)
2. Xref analysis (0 issues)
3. Code review and QA
4. Release preparation

---

## Acceptance Criteria

Each feature MUST meet ALL criteria:

1. ✅ Compiles without errors or warnings
2. ✅ Passes all unit tests (EUnit)
3. ✅ Passes all integration tests (CT)
4. ✅ Achieves ≥80% code coverage
5. ✅ Passes Dialyzer type checking (0 warnings)
6. ✅ Passes Xref analysis (0 issues)
7. ✅ Performance benchmarks show <10% regression
8. ✅ Documentation is complete and accurate
9. ✅ Examples demonstrate key use cases
10. ✅ Code review approved by 2+ reviewers

---

## Success Metrics

### Functional Metrics
- 100% of MCP 2025-11-25 required features implemented
- 0 critical bugs in production
- 100% test pass rate

### Performance Metrics
- Task creation < 10ms (p99)
- Task retrieval < 50ms (p99)
- Sampling latency < 2s (p99)
- Completion response < 100ms (p99)

### Quality Metrics
- Dialyzer: 0 warnings
- Xref: 0 issues
- Coverage: ≥80%
- Documentation: 100% API coverage

---

## Risks and Mitigations

| Risk | Impact | Probability | Mitigation |
|------|--------|-------------|------------|
| Task persistence complexity | High | Medium | Use Mnesia with proper schema migration |
| Sampling provider API changes | High | Low | Abstract provider interface, version contracts |
| Completion ranking quality | Medium | High | Implement feedback loop for relevance tuning |
| Elicitation security | High | Medium | Strict URL validation, rate limiting, audit logging |
| Performance regression | Medium | Low | Benchmark before/after, optimize hot paths |

---

## References

1. [MCP 2025-11-25 Specification](https://modelcontextprotocol.io/specification/)
2. [erlmcp Architecture](./architecture.md)
3. [erlmcp OTP Patterns](./otp-patterns.md)
4. [erlmcp API Reference](./api-reference.md)
5. [SPARC Methodology](../.claude/SPARC_QUICK_REFERENCE.md)
