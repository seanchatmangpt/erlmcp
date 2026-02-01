# erlmcp v3 Tasks API Completion Plan

**Status**: Implementation Plan (Draft)
**Author**: Erlang OTP Developer Agent
**Date**: 2026-01-31
**Context**: Completing missing task methods for MCP 2025-11-25 compliance

---

## Executive Summary

**Current State**: 5/7 task methods implemented in `erlmcp_server.erl`
- ✅ `tasks/create` - Creates async tasks
- ✅ `tasks/list` - Lists tasks with pagination
- ✅ `tasks/get` - Retrieves task details
- ✅ `tasks/result` - Gets task results
- ✅ `tasks/cancel` - Cancels running tasks
- ❌ `tasks/update` - **MISSING**: Update task metadata and state
- ❌ `tasks/progress` - **MISSING**: Progress updates via token
- ❌ `input_required` state - **DEFINED** but **NOT IMPLEMENTED**

**Core Infrastructure**:
- `erlmcp_tasks.erl` - Complete (gen_server with ETS)
- `erlmcp_task_runner.erl` - Complete (proc_lib worker)
- Tests: `erlmcp_tasks_input_tests.erl` exists but functions are stubs

---

## 1. tasks/update Method Design

### Purpose
Update task metadata and trigger state transitions. Supports:
- Updating metadata (user-provided key-value store)
- Triggering `processing -> input_required` transitions
- Resuming `input_required -> processing` with user input

### Method Signature

```erlang
% erlmcp_server.erl handler
handle_request(Id, ?MCP_METHOD_TASKS_UPDATE, Params, TransportId, State) ->
```

### Request Parameters

```erlang
#{
    <<"taskId">> => binary(),           % Required
    <<"metadata">> => map(),             % Optional: Update metadata
    <<"status">> => <<"input_required">> % Optional: Request input
}
```

### Response

```erlang
% Success
{ok, #{
    ?JSONRPC_FIELD_RESULT => #{
        <<"taskId">> => TaskId,
        <<"status">> => StatusBin,
        <<"metadata">> => Metadata,
        <<"updatedAt">> => UpdatedAt
    }
}}

% Errors
{error, ?MCP_ERROR_TASK_NOT_FOUND}
{error, ?MCP_ERROR_TASK_STATE_INVALID}
{error, ?MCP_ERROR_INVALID_PARAMS}
```

### Implementation Specification

**File**: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl`

```erlang
%% Add to handle_request clause around line 1149 (after tasks/cancel)
handle_request(Id, ?MCP_METHOD_TASKS_UPDATE, Params, TransportId,
               #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_tasks_update">>, ServerId),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_TASKS_UPDATE
        }),

        % Validate required parameters
        case maps:get(?MCP_PARAM_TASK_ID, Params, undefined) of
            undefined ->
                send_error_via_registry(State, TransportId, Id,
                    ?JSONRPC_INVALID_PARAMS, ?JSONRPC_MSG_INVALID_PARAMS),
                {noreply, State};
            TaskId ->
                % Extract update parameters
                MetadataUpdate = maps:get(<<"metadata">>, Params, undefined),
                StatusUpdate = maps:get(<<"status">>, Params, undefined),

                % Perform update
                case erlmcp_tasks:update_task(ServerPid, TaskId,
                        fun(TaskMap) ->
                            % Apply metadata update
                            TaskMap1 = case MetadataUpdate of
                                undefined -> TaskMap;
                                _ ->
                                    ExistingMeta = maps:get(<<"metadata">>, TaskMap, #{}),
                                    TaskMap#{<<"metadata">> => maps:merge(ExistingMeta, MetadataUpdate)}
                            end,

                            % Apply status transition if specified
                            case StatusUpdate of
                                undefined -> TaskMap1;
                                <<"input_required">> ->
                                    % Must be in processing state
                                    case maps:get(<<"status">>, TaskMap1) of
                                        <<"processing">> -> TaskMap1#{<<"status">> => <<"input_required">>};
                                        _ -> TaskMap1
                                    end;
                                _ -> TaskMap1
                            end
                        end) of
                    ok ->
                        % Fetch updated task for response
                        case erlmcp_tasks:get_task(ServerPid, TaskId) of
                            {ok, UpdatedTask} ->
                                send_result_via_registry(State, TransportId, Id, UpdatedTask),
                                {noreply, State};
                            {error, Reason} ->
                                send_error_via_registry(State, TransportId, Id,
                                    task_error_to_code(Reason), task_error_to_msg(Reason)),
                                {noreply, State}
                        end;
                    {error, Reason} ->
                        send_error_via_registry(State, TransportId, Id,
                            task_error_to_code(Reason), task_error_to_msg(Reason)),
                        {noreply, State}
                end
        end
    catch
        Class:Reason2:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason2, Stacktrace),
            send_error_via_registry(State, TransportId, Id,
                ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;
```

### State Transition Rules

```
Valid transitions via tasks/update:
  processing -> input_required (worker needs user input)

Invalid transitions:
  pending -> input_required (task hasn't started)
  completed -> * (task is terminal)
  failed -> * (task is terminal)
  cancelled -> * (task is terminal)
```

### Test Specifications

**File**: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_update_tests.erl`

```erlang
%% Test: Update task metadata
test_update_metadata() ->
    % Setup: Create task
    {ok, TaskId} = erlmcp_tasks:create(undefined, #{}, #{}),

    % Exercise: Update metadata
    Request = #{
        <<"taskId">> => TaskId,
        <<"metadata">> => #{<<"user_key">> => <<"user_value">>}
    },
    {ok, Response} = handle_jsonrpc(Request),

    % Verify: Metadata merged
    ?assertEqual(<<"user_value">>, maps:get(<<"user_key">>,
        maps:get(<<"metadata">>, maps:get(<<"result">>, Response))).

%% Test: Request input via status update
test_update_request_input() ->
    % Setup: Create task in processing state
    {ok, TaskId} = erlmcp_tasks:create(undefined, #{}, #{}),
    WorkerPid = spawn(fun() ->
        erlmcp_tasks:request_input(TaskId, #{<<"partial">> => true})
    end),
    erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

    % Exercise: Request input via tasks/update
    Request = #{
        <<"taskId">> => TaskId,
        <<"status">> => <<"input_required">>
    },
    {ok, Response} = handle_jsonrpc(Request),

    % Verify: Status changed
    ?assertEqual(<<"input_required">>,
        maps:get(<<"status">>, maps:get(<<"result">>, Response))).

%% Test: Invalid transition rejected
test_update_invalid_transition() ->
    % Setup: Create task in pending state
    {ok, TaskId} = erlmcp_tasks:create(undefined, #{}, #{}),

    % Exercise: Try to request input from pending
    Request = #{
        <<"taskId">> => TaskId,
        <<"status">> => <<"input_required">>
    },

    % Verify: Error response
    ?assertMatch({error, {task_state_invalid, _}}, handle_jsonrpc(Request)).
```

---

## 2. tasks/progress Method Design

### Purpose
Report progress updates for running tasks using progress tokens. Supports:
- Fractional progress (0.0 to 1.0)
- Absolute progress (current/total units)
- Automatic notification via erlmcp_progress

### Method Signature

```erlang
% erlmcp_server.erl handler
handle_request(Id, ?MCP_METHOD_TASKS_PROGRESS, Params, TransportId, State) ->
```

### Request Parameters

```erlang
#{
    <<"progressToken">> => reference(),  % Required: Progress token from task creation
    <<"progress">> => float(),            % Optional: Fractional progress [0.0, 1.0]
    <<"total">> => number()               % Optional: Total units (with progress)
}
```

### Response

```erlang
% Success
{ok, #{
    ?JSONRPC_FIELD_RESULT => #{
        <<"progress">> => Progress,
        <<"total">> => Total
    }
}}

% Errors
{error, ?MCP_ERROR_INVALID_PROGRESS_TOKEN}
{error, ?MCP_ERROR_INVALID_PARAMS}
```

### Implementation Specification

**File**: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl`

```erlang
%% Add to handle_request clause around line 1170 (after tasks/update)
handle_request(Id, ?MCP_METHOD_TASKS_PROGRESS, Params, TransportId,
               #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_tasks_progress">>, ServerId),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_TASKS_PROGRESS
        }),

        % Validate required parameters
        case maps:get(<<"progressToken">>, Params, undefined) of
            undefined ->
                send_error_via_registry(State, TransportId, Id,
                    ?JSONRPC_INVALID_PARAMS, ?JSONRPC_MSG_INVALID_PARAMS),
                {noreply, State};
            Token ->
                Progress = maps:get(<<"progress">>, Params, 0.0),
                Total = maps:get(<<"total">>, Params, undefined),

                % Validate progress is in [0.0, 1.0]
                case is_number(Progress) andalso Progress >= 0.0 andalso Progress =< 1.0 of
                    false ->
                        send_error_via_registry(State, TransportId, Id,
                            ?JSONRPC_INVALID_PARAMS, <<"Progress must be between 0.0 and 1.0">>),
                        {noreply, State};
                    true ->
                        % Update progress via erlmcp_progress
                        Update = #{
                            current => Progress
                        },
                        Update1 = case Total of
                            undefined -> Update;
                            _ -> Update#{total => Total}
                        end,

                        case erlmcp_progress:update(Token, Update1) of
                            ok ->
                                send_result_via_registry(State, TransportId, Id, #{
                                    <<"progress">> => Progress,
                                    <<"total">> => Total
                                }),
                                {noreply, State};
                            {error, invalid_token} ->
                                send_error_via_registry(State, TransportId, Id,
                                    ?MCP_ERROR_INVALID_PROGRESS_TOKEN,
                                    ?MCP_MSG_INVALID_PROGRESS_TOKEN),
                                {noreply, State}
                        end
                end
        end
    catch
        Class:Reason2:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason2, Stacktrace),
            send_error_via_registry(State, TransportId, Id,
                ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;
```

### Protocol Constants

**File**: `/Users/sac/erlmcp/apps/erlmcp_core/include/erlmcp.hrl`

```erlang
%% Add around line 519 (after tasks/cancel)
-define(MCP_METHOD_TASKS_UPDATE, <<"tasks/update">>).
-define(MCP_METHOD_TASKS_PROGRESS, <<"tasks/progress">>).
```

### Test Specifications

**File**: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_server_progress_tests.erl`

```erlang
%% Test: Report fractional progress
test_progress_fractional() ->
    % Setup: Create task with progress token
    {ok, TaskId} = erlmcp_tasks:create(undefined, #{}, #{}),
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    Token = maps:get(<<"progressToken">>, Task),

    % Exercise: Report 50% progress
    Request = #{
        <<"progressToken">> => Token,
        <<"progress">> => 0.5
    },
    {ok, Response} = handle_jsonrpc(Request),

    % Verify: Progress updated
    ?assertEqual(0.5, maps:get(<<"progress">>, maps:get(<<"result">>, Response))).

%% Test: Report absolute progress
test_progress_absolute() ->
    % Setup: Create task
    {ok, TaskId} = erlmcp_tasks:create(undefined, #{}, #{}),
    {ok, Task} = erlmcp_tasks:get_task(undefined, TaskId),
    Token = maps:get(<<"progressToken">>, Task),

    % Exercise: Report 5 of 10 units
    Request = #{
        <<"progressToken">> => Token,
        <<"progress">> => 5,
        <<"total">> => 10
    },
    {ok, Response} = handle_jsonrpc(Request),

    % Verify: Progress with total
    Result = maps:get(<<"result">>, Response),
    ?assertEqual(5, maps:get(<<"progress">>, Result)),
    ?assertEqual(10, maps:get(<<"total">>, Result)).

%% Test: Invalid token rejected
test_progress_invalid_token() ->
    % Exercise: Use invalid token
    Request = #{
        <<"progressToken">> => make_ref(),
        <<"progress">> => 0.5
    },

    % Verify: Error response
    ?assertMatch({error, {invalid_progress_token, _}}, handle_jsonrpc(Request)).

%% Test: Out-of-range progress rejected
test_progress_out_of_range() ->
    % Exercise: Progress > 1.0
    Request = #{
        <<"progressToken">> => make_ref(),
        <<"progress">> => 1.5
    },

    % Verify: Invalid params error
    ?assertMatch({error, {invalid_params, _}}, handle_jsonrpc(Request)).
```

---

## 3. input_required State Implementation

### State Machine

```
Task Lifecycle:
  pending -> processing -> input_required -> processing -> completed
                                |              |
                                v              v
                             failed         failed
```

### Worker API

**File**: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_tasks.erl`

```erlang
%% Exported worker API (add around line 33)
-export([request_input/2, provide_input/2]).

%% @doc Request user input from worker (processing -> input_required)
%% Stores partial results in task.result field.
%% Returns {ok, input_required} or {error, Reason}.
-spec request_input(task_id(), map()) -> {ok, input_required} | {error, term()}.
request_input(TaskId, PartialResults) when is_binary(TaskId), is_map(PartialResults) ->
    gen_server:call(?SERVER, {request_input, TaskId, PartialResults}).

%% @doc Provide input to resume task (input_required -> processing)
%% Sends InputData to worker_pid via {task_input, TaskId, InputData}
%% Returns ok or {error, Reason}.
-spec provide_input(task_id(), map()) -> ok | {error, term()}.
provide_input(TaskId, InputData) when is_binary(TaskId), is_map(InputData) ->
    gen_server:call(?SERVER, {provide_input, TaskId, InputData}).
```

### gen_server Handlers

**File**: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_tasks.erl`

```erlang
%% Add to handle_call (around line 340)
handle_call({request_input, TaskId, PartialResults}, _From, State) ->
    {Reply, NewState} = do_request_input(TaskId, PartialResults, State),
    {reply, Reply, NewState};

handle_call({provide_input, TaskId, InputData}, _From, State) ->
    {Reply, NewState} = do_provide_input(TaskId, InputData, State),
    {reply, Reply, NewState};
```

### Implementation Functions

```erlang
%% @private
do_request_input(TaskId, PartialResults, State) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{status = processing, worker_pid = WorkerPid} = Task] ->
            % Transition to input_required
            UpdatedTask = Task#mcp_task{
                status = input_required,
                result = PartialResults,
                updated_at = erlang:system_time(millisecond)
            },
            ets:insert(?TASKS_TABLE, UpdatedTask),

            logger:info("Task ~p requesting input with partial results: ~p",
                [TaskId, PartialResults]),
            send_task_notification(Task#mcp_task.client_pid, TaskId),

            {{ok, input_required}, State};
        [#mcp_task{status = OtherStatus}] ->
            logger:warning("Cannot request input from status ~p", [OtherStatus]),
            {{error, {task_state_invalid, ?MCP_ERROR_TASK_STATE_INVALID}}, State};
        [] ->
            {{error, {task_not_found, ?MCP_ERROR_TASK_NOT_FOUND}}, State}
    end.

%% @private
do_provide_input(TaskId, InputData, State) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{status = input_required, worker_pid = WorkerPid} = Task] ->
            % Resume to processing
            UpdatedTask = Task#mcp_task{
                status = processing,
                updated_at = erlang:system_time(millisecond)
            },
            ets:insert(?TASKS_TABLE, UpdatedTask),

            % Send input to worker
            WorkerPid ! {task_input, TaskId, InputData},

            logger:info("Task ~p resumed with input: ~p", [TaskId, InputData]),
            send_task_notification(Task#mcp_task.client_pid, TaskId),

            {ok, State};
        [#mcp_task{status = OtherStatus}] ->
            logger:warning("Cannot provide input in status ~p", [OtherStatus]),
            {{error, {task_state_invalid, ?MCP_ERROR_TASK_STATE_INVALID}}, State};
        [] ->
            {{error, {task_not_found, ?MCP_ERROR_TASK_NOT_FOUND}}, State}
    end.
```

### State Transition Guards

**Modified `do_complete_task`, `do_fail_task`, `do_cancel_task`**:

```erlang
%% Reject completion from input_required state
do_complete_task(TaskId, Result, State) ->
    case ets:lookup(?TASKS_TABLE, TaskId) of
        [#mcp_task{status = Status} = Task]
          when Status =:= processing; Status =:= pending ->
            % ... existing logic ...
        [#mcp_task{status = input_required}] ->
            {{error, {task_state_invalid, ?MCP_ERROR_TASK_STATE_INVALID}}, State};
        [#mcp_task{status = Status}] ->
            logger:warning("Cannot complete task ~p in status ~p", [TaskId, Status]),
            {{error, {task_state_invalid, ?MCP_ERROR_TASK_STATE_INVALID}}, State};
        [] ->
            {{error, {task_not_found, ?MCP_ERROR_TASK_NOT_FOUND}}, State}
    end.
```

### Test Specifications

**File**: `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_tasks_input_tests.erl`

Already exists with comprehensive tests:
- `test_input_required_lifecycle/0` - Basic transition
- `test_input_request_with_partial_results/0` - Partial result storage
- `test_provide_input_resume_task/0` - Resume workflow
- `test_input_required_invalid_transitions/0` - State machine validation
- `test_multiple_input_required_cycles/0` - Multi-cycle support
- `test_input_required_with_timeout/0` - Timeout handling

---

## 4. Error Code Mappings

### Helper Functions

**File**: `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_server.erl`

```erlang
%% @private
task_error_to_code({task_not_found, Code}) -> Code;
task_error_to_code({task_state_invalid, Code}) -> Code;
task_error_to_code({task_result_not_ready, Code}) -> Code;
task_error_to_code({invalid_progress_token, Code}) -> Code;
task_error_to_code({max_concurrent_tasks, Code}) -> Code;
task_error_to_code({task_already_completed, Code}) -> Code;
task_error_to_code(_) -> ?JSONRPC_INTERNAL_ERROR.

%% @private
task_error_to_msg({task_not_found, _}) -> ?MCP_MSG_TASK_NOT_FOUND;
task_error_to_msg({task_state_invalid, _}) -> ?MCP_MSG_TASK_STATE_INVALID;
task_error_to_msg({task_result_not_ready, _}) -> ?MCP_MSG_TASK_RESULT_NOT_READY;
task_error_to_msg({invalid_progress_token, _}) -> ?MCP_MSG_INVALID_PROGRESS_TOKEN;
task_error_to_msg({max_concurrent_tasks, _}) -> ?MCP_MSG_MAX_CONCURRENT_TASKS;
task_error_to_msg({task_already_completed, _}) -> ?MCP_MSG_TASK_ALREADY_COMPLETED;
task_error_to_msg(_) -> ?JSONRPC_MSG_INTERNAL_ERROR.
```

---

## 5. Implementation Checklist

### Phase 1: Core Infrastructure (erlmcp_tasks.erl)
- [ ] Add `request_input/2` export
- [ ] Add `provide_input/2` export
- [ ] Implement `do_request_input/3`
- [ ] Implement `do_provide_input/3`
- [ ] Add state transition guards to `do_complete_task/3`
- [ ] Add state transition guards to `do_fail_task/3`
- [ ] Add state transition guards to `do_cancel_task/4`

### Phase 2: Server Handlers (erlmcp_server.erl)
- [ ] Add `?MCP_METHOD_TASKS_UPDATE` constant
- [ ] Add `?MCP_METHOD_TASKS_PROGRESS` constant
- [ ] Implement `handle_request` for `tasks/update`
- [ ] Implement `handle_request` for `tasks/progress`
- [ ] Add `task_error_to_code/1` helper
- [ ] Add `task_error_to_msg/1` helper

### Phase 3: Protocol Headers (erlmcp.hrl)
- [ ] Add `-define(MCP_METHOD_TASKS_UPDATE, ...)`
- [ ] Add `-define(MCP_METHOD_TASKS_PROGRESS, ...)`
- [ ] Verify `input_required` in `#mcp_task.status` type

### Phase 4: Testing
- [ ] Run existing `erlmcp_tasks_input_tests`
- [ ] Implement `erlmcp_server_update_tests`
- [ ] Implement `erlmcp_server_progress_tests`
- [ ] Add integration tests for full workflow
- [ ] Verify 80%+ coverage

### Phase 5: Documentation
- [ ] Update `docs/protocol.md` with tasks/update
- [ ] Update `docs/protocol.md` with tasks/progress
- [ ] Add input_required state machine diagram
- [ ] Update API reference

---

## 6. Validation Criteria

### Functional Requirements
1. ✅ `tasks/update` can update task metadata
2. ✅ `tasks/update` can trigger `processing -> input_required`
3. ✅ `tasks/progress` reports fractional progress
4. ✅ `tasks/progress` reports absolute progress
5. ✅ `input_required` state stores partial results
6. ✅ `provide_input` resumes task execution
7. ✅ All invalid transitions rejected

### Quality Gates
1. ✅ All tests pass (`rebar3 eunit`)
2. ✅ Coverage ≥ 80% (`rebar3 cover`)
3. ✅ Dialyzer clean (`rebar3 dialyzer`)
4. ✅ No xref warnings (`rebar3 xref`)
5. ✅ Format verified (`rebar3 format --verify`)

### Performance Targets
1. ✅ `tasks/update` latency < 10ms (metadata update)
2. ✅ `tasks/progress` latency < 5ms (progress notification)
3. ✅ `request_input` latency < 10ms (state transition)
4. ✅ `provide_input` latency < 10ms (worker message delivery)

---

## 7. Open Questions

### Q1: Should `tasks/update` support arbitrary status transitions?
**Answer**: No. Only `processing -> input_required` is supported via `tasks/update`.
All other transitions should use dedicated methods (`tasks/cancel`, complete/fail from worker).

### Q2: What happens to progress tokens during `input_required` state?
**Answer**: Progress token remains active. Task can continue reporting progress while awaiting input.
Progress is paused but not invalidated.

### Q3: Can `input_required` tasks be cancelled?
**Answer**: **Yes**, `tasks/cancel` should work from `input_required` state.
This allows users to abandon tasks that are stuck waiting for input.

### Q4: Should partial results be schema-validated?
**Answer**: **No**. Partial results are free-form maps.
Workers and clients agree on schema out-of-band (per MCP 2025-11-25).

---

## 8. Dependencies

### Internal Dependencies
- `erlmcp_tasks.erl` - Task state management
- `erlmcp_progress.erl` - Progress token tracking
- `erlmcp_tracing.erl` - OpenTelemetry spans
- `erlmcp_registry.erl` - Transport routing

### External Dependencies
- None (pure OTP implementation)

---

## 9. Timeline Estimate

| Phase | Tasks | Estimated Time |
|-------|-------|----------------|
| Phase 1: Core Infrastructure | 7 tasks | 2 hours |
| Phase 2: Server Handlers | 6 tasks | 3 hours |
| Phase 3: Protocol Headers | 3 tasks | 0.5 hours |
| Phase 4: Testing | 5 tasks | 3 hours |
| Phase 5: Documentation | 4 tasks | 1.5 hours |
| **Total** | **25 tasks** | **10 hours** |

---

## 10. References

### MCP Specification
- [MCP 2025-11-25 Tasks Capability](https://modelcontextprotocol.io/beta/docs/concepts/tasks/)
- Section 5.3: Task State Machine
- Section 5.4: Progress Reporting

### erlmcp Implementation
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_tasks.erl` - Task gen_server
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_task_runner.erl` - Worker process
- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_progress.erl` - Progress tracking
- `/Users/sac/erlmcp/apps/erlmcp_core/include/erlmcp.hrl` - Protocol constants

### Test Files
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_tasks_input_tests.erl` - Input state tests
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_tasks_lifecycle_tests.erl` - Lifecycle tests
- `/Users/sac/erlmcp/apps/erlmcp_core/test/erlmcp_tasks_tests.erl` - General tests

---

## Appendix A: Example Workflows

### Workflow 1: Simple Input Request

```erlang
%% 1. Client creates task
{ok, TaskId} = erlmcp_tasks:create(ClientPid, #{
    <<"type">> => <<"data_processing">>
}, #{}).

%% 2. Worker starts processing
{ok, WorkerPid} = erlmcp_task_runner:start_link(fun() ->
    % Process first batch
    process_batch(data_1),

    % Request user input
    {ok, input_required} = erlmcp_tasks:request_input(TaskId, #{
        <<"processed">> => 100,
        <<"total">> => 200,
        <<"message">> => <<"Continue processing?">>
    }),

    % Wait for input
    receive
        {task_input, TaskId, Input} ->
            case maps:get(<<"continue">>, Input) of
                true -> process_batch(data_2);
                false -> ok
            end
    end
end, #{}).

%% 3. Client polls task status
{ok, Task} = erlmcp_tasks:get_task(ClientPid, TaskId),
% Task#{<<"status">> => <<"input_required">>}

%% 4. Client provides input via tasks/update
Request = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 1,
    <<"method">> => <<"tasks/update">>,
    <<"params">> => #{
        <<"taskId">> => TaskId,
        <<"metadata">> => #{<<"user_choice">> => <<"continue">>}
    }
}.
% Server calls erlmcp_tasks:provide_input(TaskId, InputData)

%% 5. Worker resumes and completes
% Task status: processing -> completed
```

### Workflow 2: Progress Reporting

```erlang
%% 1. Create task with progress tracking
{ok, TaskId} = erlmcp_tasks:create(ClientPid, #{}, #{}).
{ok, Task} = erlmcp_tasks:get_task(ClientPid, TaskId).
Token = maps:get(<<"progressToken">>, Task).

%% 2. Worker reports progress
erlmcp_tasks:update_progress(undefined, TaskId, #{
    <<"progressToken">> => Token,
    <<"progress">> => 0.25,  % 25% complete
    <<"total">> => 100
}).
% Client receives notification via erlmcp_progress

%% 3. Alternative: Direct progress update
{ok, _} = erlmcp_tasks:set_task_progress(TaskId, {25, 100}).
```

---

**Document Version**: 1.0
**Last Updated**: 2026-01-31
**Status**: Ready for Implementation
