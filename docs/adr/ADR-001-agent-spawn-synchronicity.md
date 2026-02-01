# ADR-001: Agent Spawn Synchronicity

**Status**: ACCEPTED | **Date**: 2026-02-01 | **Supersedes**: None

---

## Context

When spawning agents in erlmcp-flow, we face a fundamental decision about whether spawn operations should be synchronous (blocking, returns immediately) or asynchronous (non-blocking, spawns in background).

The agent spawning operation must:
1. Create a new gen_server process
2. Register it in the global registry (gproc)
3. Initialize its state backend (ETS/DETS/Mnesia)
4. Return agent metadata to the caller

The caller (e.g., routing engine, orchestrator) needs this information to:
- Make immediate routing decisions
- Send first task to newly-spawned agent
- Track agent PID for supervision
- Handle spawn failures

---

## Decision

**Spawn is synchronous**: `spawn_agent(config) -> {ok, agent()} | {error, reason()}` blocks until agent is ready.

### Implementation Strategy

```erlang
-spec spawn_agent(agent_config()) -> {ok, agent()} | {error, term()}.
spawn_agent(Config) ->
    %% Step 1: Validate config
    case validate_agent_config(Config) of
        {error, Reason} ->
            {error, {config_invalid, Reason}};
        ok ->
            %% Step 2: Ask supervisor to start child
            %% Uses supervisor:start_child (synchronous)
            case erlmcp_agent_spawner_sup:start_child(Config) of
                {ok, Pid} ->
                    %% Step 3: Register in gproc
                    AgentId = maps:get(id, Config),
                    case gproc:reg({n, l, {agent, AgentId}}, Pid) of
                        true ->
                            %% Step 4: Initialize state
                            case init_agent_state(Pid, Config) of
                                ok ->
                                    {ok, agent_record(Pid, Config)};
                                {error, StateErr} ->
                                    terminate_agent(Pid),
                                    {error, {state_init_failed, StateErr}}
                            end;
                        Error ->
                            terminate_agent(Pid),
                            {error, {registration_failed, Error}}
                    end;
                {error, StartErr} ->
                    {error, {spawn_failed, StartErr}}
            end
    end.

%% Helper: Initialize agent state (calls gen_server:call with timeout)
-spec init_agent_state(pid(), agent_config()) -> ok | {error, term()}.
init_agent_state(Pid, Config) ->
    try
        gen_server:call(Pid, {init_state, Config}, 5000)
    catch
        exit:{timeout, _} ->
            {error, init_timeout};
        exit:{Reason, _} ->
            {error, {init_crashed, Reason}}
    end.
```

### Invariants Maintained

- **I-1**: No orphan processes (caller has PID or error, never partial)
- **I-2**: Registry consistency (registration happens before return)
- **I-3**: State is ready (init_agent_state called and succeeded)
- **I-4**: Caller has actionable information (can route immediately)

---

## Rationale

### Why Synchronous?

**1. Caller Simplicity**
- Pattern: `{ok, Agent} = spawn_agent(Config)`
- Caller receives PID immediately → can route task immediately
- No result channel, callback, or polling needed
- Idiomatic Erlang (similar to supervisor:start_child)

**2. Race Condition Prevention**
```
ASYNC (vulnerable):
    {ok, AgentId} = spawn_agent_async(...)
    % Agent may not be registered yet!
    route_to_agent(AgentId, Task)    % ← Race: agent not found

SYNC (safe):
    {ok, Agent} = spawn_agent(...)
    % Agent is guaranteed ready
    route_to_agent(Agent.id, Task)   % ← Safe: agent exists
```

**3. Error Handling**
- Synchronous → immediate error feedback
- Async → error hidden, discovered later (harder to debug)
- Principle: **Fail fast and loudly**

**4. State Consistency**
- Registry lookup is consistent with spawn time
- No window where agent PID exists but state is uninitialized
- Backpressure: if spawn takes 100ms, that's visible (not hidden)

### Trade-offs

**Cons** (accepted costs):
- Higher latency for caller (100-500ms)
- Spawner cannot parallelize multiple spawns in single call
- Caller blocks until agent init completes

**Pros** (benefits):
- Simple caller code (no async result handling)
- Race-condition free
- Error visible immediately
- Registry consistency guaranteed

---

## Alternatives Considered

### Alternative 1: Asynchronous Spawn (Actor Model)

**Proposal**:
```erlang
spawn_agent_async(Config, ResultChannelPid) ->
    spawn_link(fun() ->
        Result = do_spawn(Config),
        ResultChannelPid ! {spawn_result, Result}
    end).
```

**Pros**:
- Spawner never blocks
- Multiple spawns parallelized

**Cons**:
- Caller must implement result channel
- Race: caller routes before agent ready
- Error handling harder (async exception)
- **REJECTED**: Adds complexity, doesn't match erlmcp patterns

### Alternative 2: Lazy Spawn (Create on First Task)

**Proposal**:
```erlang
spawn_agent(Config) -> {ok, AgentId}.  % No actual process yet
assign_task(AgentId, Task) ->          % Create process here
    case erlmcp_agent_registry:get(AgentId) of
        not_found -> create_agent_now(AgentId);
        {ok, Pid} -> ok
    end,
    send_task(AgentId, Task).
```

**Pros**:
- Zero-cost if agent never used
- Parallelizes unused spawns

**Cons**:
- Unpredictable latency on first task
- Hidden complexity in assign_task
- Registry inconsistency window
- **REJECTED**: Violates "fail fast" principle

### Alternative 3: Batched Async Spawn

**Proposal**:
```erlang
spawn_agents_async(Configs) ->
    {ok, AgentIds} = erlmcp_agent_spawner:spawn_batch(Configs),
    % Results in 1-10ms via load-balanced pool
```

**Pros**:
- Parallelize multiple spawns
- Still returns immediately

**Cons**:
- Only useful for bulk operations (N > 10)
- Adds complexity for single-spawn case
- **DEFERRED to v1.1**: Batch API for bulk ops

---

## Implementation Notes

### Supervision Tree

```
erlmcp_agent_spawner_sup (simple_one_for_one)
├─ agent_pool_worker_1 → spawns agents
├─ agent_pool_worker_2
└─ agent_pool_worker_3
```

The spawner is **NOT** the agent gen_server itself, but a pool worker that:
1. Calls supervisor:start_child(erlmcp_agent_sup, ...)
2. Waits for child to initialize
3. Returns {ok, Pid} to caller

### Timeout Handling

- **init_agent_state**: 5 second timeout (per RTO requirements)
- **If timeout**: terminate agent, return error to caller
- **Caller retries**: implement exponential backoff (if desired)

### Error Recovery

```
Spawn fails at step X:
  Config invalid    → return {error, config_invalid}
  Child start fails → return {error, spawn_failed}
  Registry fails    → terminate child, return {error, registration_failed}
  State init fails  → terminate child, return {error, state_init_failed}

Caller responsibility:
  - Retry with exponential backoff
  - Log failure for observability
  - Alert if spawn_rate drops
```

---

## Consequences

### Positive

✓ Caller code is simple (match on {ok, Agent})
✓ No race conditions (registry ready before return)
✓ Error handling is straightforward
✓ Consistent with Erlang patterns (supervisor:start_child)
✓ Fail-fast principle maintained

### Negative

✗ Caller blocks (100-500ms per spawn)
✗ Cannot spawn 1000 agents in parallel
✗ Spawner must handle load (multiple pool workers needed)

### Mitigation

- Use agent pool (simple_one_for_one) with 3-5 workers
- If bulk spawn needed, batch API in v1.1
- Load testing to size pool correctly

---

## Related ADRs

- ADR-005: Agent State Backend (ETS vs DETS vs Mnesia)
- ADR-007: Registry Implementation (gproc vs custom)

## Testing

```erlang
%% test/erlmcp_agent_spawner_tests.erl
-spec spawn_valid_agent_test() -> ok.
spawn_valid_agent_test() ->
    Config = agent_config([{role, worker}]),
    {ok, Agent} = erlmcp_agent_spawner:spawn_agent(Config),
    ?assertMatch(#{id := _, pid := _, state := idle}, Agent),
    ?assert(is_pid(Agent#{pid})),
    % Verify registry entry exists
    {ok, {Pid, _}} = erlmcp_agent_registry:get(Agent#{id}),
    ?assertEqual(Pid, Agent#{pid}).

-spec spawn_invalid_config_test() -> ok.
spawn_invalid_config_test() ->
    BadConfig = #{},  % Missing required fields
    {error, {config_invalid, _}} = erlmcp_agent_spawner:spawn_agent(BadConfig).

-spec spawn_concurrent_agents_test() -> ok.
spawn_concurrent_agents_test() ->
    Configs = [agent_config([{role, worker}]) || _ <- lists:seq(1, 10)],
    Results = pmap(fun erlmcp_agent_spawner:spawn_agent/1, Configs),
    % All should succeed
    ?assertEqual(10, lists:count(fun({ok, _}) -> true; (_) -> false end, Results)),
    % All PIDs should be unique
    Pids = [Pid || {ok, #{pid := Pid}} <- Results],
    ?assertEqual(10, length(lists:usort(Pids))).

-spec spawn_timeout_test() -> ok.
spawn_timeout_test() ->
    Config = agent_config([{timeout_ms, 1}]),  % Very short timeout
    % Should fail or return, never hang
    {Result, _} = timer:tc(
        erlmcp_agent_spawner, spawn_agent, [Config]
    ),
    ?assert(Result < 5000).  % Must complete in <5s even if times out
```

---

## Approval

- [ ] Agent Foundation Team Lead: _______
- [ ] Coordination Lead: _______
- [ ] Architecture Review: _______
- [ ] Security Review (if applicable): _______

---

**Date Accepted**: 2026-02-01
**Implemented by**: erlmcp-flow v1.0.0
**Last Updated**: 2026-02-01

