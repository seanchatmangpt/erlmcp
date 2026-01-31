# erlmcp_task_runner - Supervised Task Execution with proc_lib

## Overview

`erlmcp_task_runner` provides OTP-compatible task execution for long-running MCP operations using `proc_lib`. This module enables proper supervision, progress reporting, and graceful cancellation of tasks.

## Key Features

- **proc_lib Integration**: Proper OTP process initialization for supervisor compatibility
- **Progress Reporting**: Integration with `erlmcp_progress` for incremental updates
- **Graceful Cancellation**: Support for clean task termination
- **Timeout Handling**: Configurable timeouts with automatic cleanup
- **System Message Support**: Full `sys` module debugging support
- **erlmcp_tasks Integration**: Seamless integration with task lifecycle management

## Architecture

### Process Lifecycle

```
┌─────────────────┐
│  Parent Process │
│   (Supervisor)  │
└────────┬────────┘
         │ start_link/start_task
         ▼
┌─────────────────────────────────┐
│    erlmcp_task_runner Process   │
├─────────────────────────────────┤
│ 1. proc_lib:init_ack            │
│ 2. Notify parent (task_started) │
│ 3. Register with erlmcp_tasks   │
│ 4. Set timeout timer            │
│ 5. Initialize progress tracking │
│ 6. Execute task function        │
│ 7. Handle result/error          │
│ 8. Notify completion            │
│ 9. Clean shutdown               │
└─────────────────────────────────┘
         │
         ├─► {task_complete, TaskId, Result}
         ├─► {task_failed, TaskId, Error}
         ├─► {task_cancelled, TaskId, Reason}
         └─► {task_timeout, TaskId}
```

### Integration with erlmcp_tasks

```
erlmcp_tasks (gen_server)          erlmcp_task_runner (proc_lib)
     │                                      │
     │ create_task                          │
     ├──────────────────────────────────────┤
     │                                      │
     │                     start_task(Fun, TaskId, Opts)
     │◄─────────────────────────────────────┤
     │                                      │
     │ start_task_execution(TaskId, Pid)   │
     ├─────────────────────────────────────►│
     │                                      │
     │                           [Task Executes]
     │                                      │
     │ complete_task(TaskId, Result)       │
     │◄─────────────────────────────────────┤
     │                                      │
     │ fail_task(TaskId, Error)            │
     │◄─────────────────────────────────────┤
     │                                      │
```

## API Reference

### Starting Tasks

#### start_task/2

Start a task with simple function and options.

```erlang
TaskFun = fun() ->
    % Long-running operation
    timer:sleep(1000),
    {ok, result}
end,

{ok, TaskId, Pid} = erlmcp_task_runner:start_task(TaskFun, #{
    timeout => 5000,
    progress_token => ProgressToken,
    metadata => #{operation => <<"example">>}
}).
```

#### start_task/3

Start a task with explicit task ID.

```erlang
{ok, TaskId} = erlmcp_tasks:create(self(), Action, Metadata),
TaskFun = fun() -> perform_work() end,
{ok, Pid} = erlmcp_task_runner:start_task(TaskFun, TaskId, #{timeout => 10000}).
```

#### start_link/1

Start a supervised task (supervisor-compatible).

```erlang
TaskSpec = #{
    fun => fun() -> supervised_work() end,
    timeout => 30000,
    progress_token => erlmcp_progress:generate_token(),
    task_id => <<"my-task-id">>,
    parent => self(),
    metadata => #{}
},
{ok, Pid} = erlmcp_task_runner:start_link(TaskSpec).
```

#### start_link/2

Start a supervised task with function and options.

```erlang
{ok, Pid} = erlmcp_task_runner:start_link(
    fun() -> work() end,
    #{timeout => 5000, task_id => TaskId}
).
```

### Controlling Tasks

#### cancel_task/1

Cancel a running task with default reason.

```erlang
ok = erlmcp_task_runner:cancel_task(Pid).
```

#### cancel_task/2

Cancel a running task with specific reason.

```erlang
ok = erlmcp_task_runner:cancel_task(Pid, <<"User requested cancellation">>).
```

#### get_status/1

Get current task status (for debugging).

```erlang
{ok, Status} = erlmcp_task_runner:get_status(Pid).
% Status = #{
%     task_id => <<"...">>,
%     status => running,
%     elapsed_ms => 1234
% }
```

## Usage Examples

### Example 1: Simple Task with Result

```erlang
% Create a task that fetches data
TaskFun = fun() ->
    Data = fetch_from_api(),
    process_data(Data),
    {ok, Data}
end,

{ok, TaskId, Pid} = erlmcp_task_runner:start_task(TaskFun, #{timeout => 30000}),

% Wait for result
receive
    {task_complete, TaskId, {ok, Data}} ->
        io:format("Task completed with data: ~p~n", [Data]);
    {task_failed, TaskId, Error} ->
        io:format("Task failed: ~p~n", [Error])
after 35000 ->
    io:format("Task timed out (no notification)~n")
end.
```

### Example 2: Task with Progress Reporting

```erlang
% Create progress token
ProgressToken = erlmcp_progress:generate_token(),

% Task that reports progress
TaskFun = fun() ->
    lists:foreach(fun(N) ->
        % Do work
        process_item(N),

        % Report progress
        erlmcp_progress:update(ProgressToken, #{
            current => N,
            total => 100,
            message => iolist_to_binary(io_lib:format("Processing item ~p", [N]))
        })
    end, lists:seq(1, 100)),

    completed
end,

{ok, TaskId, Pid} = erlmcp_task_runner:start_task(TaskFun, #{
    progress_token => ProgressToken,
    timeout => 60000
}),

% Progress notifications will be sent to parent process automatically
receive
    {task_complete, TaskId, completed} ->
        io:format("All items processed~n")
end.
```

### Example 3: Cancellable Task

```erlang
% Start a long-running task
TaskFun = fun() ->
    lists:foreach(fun(N) ->
        timer:sleep(1000),
        io:format("Step ~p~n", [N])
    end, lists:seq(1, 100))
end,

{ok, TaskId, Pid} = erlmcp_task_runner:start_task(TaskFun, #{timeout => 120000}),

% Wait a bit, then cancel
timer:sleep(5000),
ok = erlmcp_task_runner:cancel_task(Pid, <<"User interrupted">>),

% Wait for cancellation confirmation
receive
    {task_cancelled, TaskId, Reason} ->
        io:format("Task cancelled: ~s~n", [Reason])
end.
```

### Example 4: Integration with erlmcp_tasks

```erlang
% Create task via erlmcp_tasks
{ok, TaskId} = erlmcp_tasks:create(self(), #{
    type => <<"computation">>,
    operation => <<"calculate_pi">>
}, #{timeout => 10000}),

% Start task runner
TaskFun = fun() ->
    % Long computation
    Pi = calculate_pi(1000000),
    {ok, Pi}
end,

{ok, Pid} = erlmcp_task_runner:start_task(TaskFun, TaskId, #{}),

% Task status is automatically synchronized with erlmcp_tasks
timer:sleep(100),
{ok, Task} = erlmcp_tasks:get(self(), TaskId),
% Task status will be 'processing'

% Wait for completion
receive
    {task_complete, TaskId, {ok, Pi}} ->
        % Verify status updated in erlmcp_tasks
        {ok, CompletedTask} = erlmcp_tasks:get(self(), TaskId),
        <<"completed">> = maps:get(<<"status">>, CompletedTask)
end.
```

### Example 5: Supervised Task in Supervision Tree

```erlang
% In your supervisor init/1:
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 5,
        period => 60
    },

    TaskWorkerSpec = #{
        id => task_worker,
        start => {erlmcp_task_runner, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_task_runner]
    },

    {ok, {SupFlags, [TaskWorkerSpec]}}.

% Start a supervised task:
TaskSpec = #{
    fun => fun() -> supervised_work() end,
    timeout => 30000
},
{ok, Pid} = supervisor:start_child(my_task_supervisor, [TaskSpec]).
```

## Error Handling

### Task Failures

Tasks can fail in several ways:

1. **Exception**: `error(Reason)` → `{task_failed, TaskId, {error, Reason, Stacktrace}}`
2. **Throw**: `throw(Reason)` → `{task_failed, TaskId, {throw, Reason}}`
3. **Exit**: `exit(Reason)` → `{task_failed, TaskId, {exit, Reason}}`
4. **Timeout**: No completion within timeout → `{task_failed, TaskId, #{code => ?MCP_ERROR_TIMEOUT}}`

### Integration Failures

If `erlmcp_tasks` is not running, the task runner continues to work but won't synchronize state. This is by design for maximum resilience.

## System Message Support

The task runner supports the `sys` module for debugging:

```erlang
% Get current state
{ok, State} = erlmcp_task_runner:get_status(Pid).

% Using sys module directly
sys:get_state(Pid).

% Suspend/resume for debugging
sys:suspend(Pid),
State = sys:get_state(Pid),
sys:resume(Pid).
```

## Performance Characteristics

- **Startup overhead**: ~1-2ms per task (proc_lib initialization)
- **Memory per task**: ~5-10 KB (process heap + state record)
- **Cancellation latency**: <10ms (direct message to process)
- **Progress reporting overhead**: ~100-200μs per update

## Testing

Comprehensive test coverage in `apps/erlmcp_core/test/erlmcp_task_runner_tests.erl`:

- Basic task execution
- Result propagation
- Cancellation handling
- Timeout triggering
- Exception handling (error, throw, exit)
- Progress reporting
- Supervised task startup
- erlmcp_tasks integration
- Concurrent task execution
- Parent death cleanup
- System message debugging
- Long-running tasks with periodic updates

Run tests:

```bash
rebar3 eunit --module=erlmcp_task_runner_tests
```

## Quality Gates

### Compilation

```bash
TERM=dumb rebar3 compile
```

Expected: No errors, all modules compile successfully.

### Unit Tests

```bash
rebar3 eunit --module=erlmcp_task_runner_tests
```

Expected: 15/15 tests passing, 0 failures.

### Dialyzer

```bash
rebar3 dialyzer
```

Expected: 0 type warnings.

### Code Coverage

```bash
rebar3 eunit --cover --module=erlmcp_task_runner_tests
rebar3 cover --verbose
```

Expected: ≥80% coverage for `erlmcp_task_runner.erl`.

## Integration Points

### Dependencies

- **erlmcp_tasks**: Task lifecycle management (optional)
- **erlmcp_progress**: Progress reporting (optional)
- **proc_lib**: OTP process spawning (required)
- **sys**: System message handling (required)

### Dependents

- **erlmcp_server**: Can use task_runner for long-running tool execution
- **erlmcp_client**: Can use task_runner for async request handling
- **Custom tool handlers**: Any MCP tool that needs supervised execution

## Migration from Direct spawn/1

Before (unsafe):

```erlang
Pid = spawn(fun() ->
    Result = long_operation(),
    Parent ! {result, Result}
end).
```

After (safe, supervised):

```erlang
{ok, TaskId, Pid} = erlmcp_task_runner:start_task(
    fun() -> long_operation() end,
    #{timeout => 30000}
),
receive
    {task_complete, TaskId, Result} -> handle_result(Result)
end.
```

## Benefits of proc_lib Approach

1. **Supervisor Compatibility**: Can be supervised with `start_link/1`
2. **Proper OTP Integration**: Uses `proc_lib:init_ack` for startup synchronization
3. **Debugging Support**: Full `sys` module support for state inspection
4. **Crash Reports**: OTP generates proper crash reports on failure
5. **System Message Handling**: Can be suspended/resumed for debugging
6. **Clean Shutdown**: Traps exits and performs cleanup

## Limitations

1. **No Hot Code Loading**: Task functions are closures, can't be upgraded
2. **Parent Dependency**: Task dies if parent dies (by design for cleanup)
3. **No State Persistence**: Task state is in-memory only
4. **Timeout Granularity**: Minimum practical timeout is ~100ms

## Future Enhancements

- [ ] Task result caching
- [ ] Automatic retry with backoff
- [ ] Task priority scheduling
- [ ] Resource limit enforcement (memory, CPU)
- [ ] Distributed task execution
- [ ] Task dependency management
- [ ] Checkpoint/resume support

## References

- [proc_lib documentation](https://www.erlang.org/doc/man/proc_lib.html)
- [sys module documentation](https://www.erlang.org/doc/man/sys.html)
- [OTP Design Principles](https://www.erlang.org/doc/design_principles/des_princ.html)
- [MCP Task Specification](../docs/protocol.md#tasks)
