# PQC Case Registry - Implementation Documentation

## Overview

`pqc_case_registry` is a gen_server that provides:
- **ETS-based registry** for CaseId → Pid mapping
- **pg-based pubsub** for Case event streaming
- **Automatic cleanup** on Case process termination
- **Concurrent access** with read_concurrency optimization

## Architecture

### State Management

```erlang
-record(state, {
    table :: ets:table(),              % ETS table for lookups
    monitors :: #{reference() => binary()}  % MonRef → CaseId mapping
}).
```

### ETS Table Configuration

- **Name**: `pqc_case_registry_tab`
- **Type**: `set`
- **Access**: `public`
- **Options**: `{read_concurrency, true}` for O(1) concurrent reads
- **Schema**: `{CaseId :: binary(), Pid :: pid()}`

### PubSub Configuration

- **Scope**: `pqc_case_pg`
- **Groups**: `{case, CaseId}` per Case
- **Library**: `pg` (OTP 23+)
- **Distribution**: Local scope (can be extended to distributed)

## API Functions

### Registry Operations

#### `start_link() -> {ok, pid()} | {error, term()}`

Starts the registry gen_server.

**Example:**
```erlang
{ok, _Pid} = pqc_case_registry:start_link().
```

#### `ensure_case(CaseId, Net, SigningKey) -> {ok, pid()} | {error, term()}`

Ensures a Case exists, creating it if necessary.

**Idempotent**: Multiple calls with the same CaseId return the same Pid.

**Example:**
```erlang
CaseId = <<"case-12345">>,
Net = my_workflow_net,
SigningKey = my_signing_key,

{ok, CasePid} = pqc_case_registry:ensure_case(CaseId, Net, SigningKey),

% Second call returns same Pid
{ok, CasePid} = pqc_case_registry:ensure_case(CaseId, Net, SigningKey).
```

#### `lookup(CaseId) -> {ok, pid()} | {error, not_found}`

Looks up a Case process by ID.

**Performance**: O(1) ETS lookup with read_concurrency

**Example:**
```erlang
case pqc_case_registry:lookup(<<"case-12345">>) of
    {ok, Pid} ->
        % Case exists
        interact_with_case(Pid);
    {error, not_found} ->
        % Case doesn't exist
        create_new_case()
end.
```

#### `list_cases() -> [binary()]`

Returns a list of all active Case IDs.

**Example:**
```erlang
Cases = pqc_case_registry:list_cases(),
io:format("Active Cases: ~p~n", [Cases]).
```

#### `get_case_count() -> non_neg_integer()`

Returns the number of active Cases.

**Example:**
```erlang
Count = pqc_case_registry:get_case_count(),
io:format("Total Cases: ~p~n", [Count]).
```

### PubSub Operations

#### `subscribe(CaseId) -> ok | {error, term()}`

Subscribes to Case events via pg group.

Events are delivered as: `{pqc_case_event, CaseId, Event}`

**Example:**
```erlang
CaseId = <<"case-12345">>,
ok = pqc_case_registry:subscribe(CaseId),

receive
    {pqc_case_event, CaseId, {status, StatusMap}} ->
        handle_status_event(StatusMap);
    {pqc_case_event, CaseId, {artifact, ArtifactMap}} ->
        handle_artifact_event(ArtifactMap);
    {pqc_case_event, CaseId, {case_terminated, Reason}} ->
        handle_termination(Reason)
end.
```

#### `unsubscribe(CaseId) -> ok | {error, term()}`

Unsubscribes from Case events.

**Example:**
```erlang
ok = pqc_case_registry:unsubscribe(<<"case-12345">>).
```

#### `publish(CaseId, Event) -> ok`

Publishes an event to all Case subscribers.

**Event Types:**
- `{status, map()}` - Case status updates
- `{artifact, map()}` - Artifact creation/updates
- `{snapshot, map()}` - State snapshots
- `{case_terminated, term()}` - Termination notification

**Example:**
```erlang
CaseId = <<"case-12345">>,
Event = {status, #{
    state => <<"running">>,
    progress => 50,
    updated_at => erlang:system_time(millisecond)
}},

ok = pqc_case_registry:publish(CaseId, Event).
```

## Process Monitoring

### Automatic Cleanup

When a Case process terminates:

1. **Monitor fires**: Registry receives `{'DOWN', MonRef, process, Pid, Reason}`
2. **ETS cleanup**: Entry removed from `pqc_case_registry_tab`
3. **Publish notification**: `{case_terminated, Reason}` sent to subscribers
4. **State update**: MonRef removed from monitors map

### Example Cleanup Flow

```erlang
% Case terminates
exit(CasePid, shutdown),

% Registry receives DOWN message
handle_info({'DOWN', MonRef, process, Pid, Reason}, State) ->
    {CaseId, NewMonitors} = maps:take(MonRef, State#state.monitors),

    % Remove from ETS
    true = ets:delete(?TABLE, CaseId),

    % Notify subscribers
    publish(CaseId, {case_terminated, Reason}),

    {noreply, State#state{monitors = NewMonitors}}.
```

## Integration Patterns

### A2A Streaming Integration

```erlang
% A2A bridge subscribes to Case events
CaseId = <<"case-12345">>,
ok = pqc_case_registry:subscribe(CaseId),

% Forward events as SSE to A2A clients
handle_info({pqc_case_event, CaseId, Event}, State) ->
    SSE = format_as_sse(Event),
    a2a_bridge:send_sse(State#state.session_id, SSE),
    {noreply, State}.
```

### MCP Notifications Integration

```erlang
% MCP bridge subscribes to Case events
CaseId = <<"case-12345">>,
ok = pqc_case_registry:subscribe(CaseId),

% Forward events as JSON-RPC notifications
handle_info({pqc_case_event, CaseId, Event}, State) ->
    Notification = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"notifications/case/status">>,
        <<"params">> => Event
    },
    mcp_bridge:send_notification(State#state.connection, Notification),
    {noreply, State}.
```

### Multi-Subscriber Pattern

```erlang
% Multiple processes can subscribe to same Case
CaseId = <<"case-12345">>,

% Web dashboard
spawn(fun() ->
    ok = pqc_case_registry:subscribe(CaseId),
    dashboard_event_loop(CaseId)
end),

% Metrics collector
spawn(fun() ->
    ok = pqc_case_registry:subscribe(CaseId),
    metrics_event_loop(CaseId)
end),

% Audit logger
spawn(fun() ->
    ok = pqc_case_registry:subscribe(CaseId),
    audit_event_loop(CaseId)
end).
```

## OTP Patterns

### gen_server Callbacks

All 6 gen_server callbacks are implemented:

1. **`init/1`**: Creates ETS table, starts pg scope
2. **`handle_call/3`**: Synchronous operations (ensure_case, lookup, list_cases)
3. **`handle_cast/2`**: Asynchronous operations (publish)
4. **`handle_info/2`**: Process monitoring (DOWN messages)
5. **`terminate/2`**: Cleanup (currently no-op, ETS cleaned by VM)
6. **`code_change/3`**: Hot code reload support

### Supervision

Should be supervised with:

```erlang
#{
    id => pqc_case_registry,
    start => {pqc_case_registry, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [pqc_case_registry]
}
```

## Testing

### Test Coverage

Comprehensive EUnit tests in `pqc_case_registry_tests.erl`:

- **Registry lifecycle**: start_link, initialization
- **Case operations**: ensure_case, lookup, list_cases, get_case_count
- **Idempotency**: Multiple ensure_case calls return same Pid
- **PubSub**: subscribe, unsubscribe, publish
- **Monitoring**: Case termination triggers cleanup
- **Concurrency**: Multiple subscribers receive events
- **Race conditions**: Concurrent ensure_case calls

### Running Tests (via Docker)

```bash
# Compile
docker compose run --rm erlmcp-build rebar3 compile

# Run EUnit tests
docker compose run --rm erlmcp-unit rebar3 eunit --module=pqc_case_registry_tests

# Run all quality gates
docker compose run --rm erlmcp-check rebar3 dialyzer
docker compose run --rm erlmcp-check rebar3 xref
docker compose run --rm erlmcp-check rebar3 format --verify
```

### Test Example

```erlang
test_subscribe_and_publish({_, _}) ->
    fun() ->
        CaseId = <<"case-pubsub-001">>,
        {ok, _Pid} = pqc_case_registry:ensure_case(CaseId, test_net, test_key),

        % Subscribe
        ok = pqc_case_registry:subscribe(CaseId),

        % Publish event
        Event = {status, #{state => <<"running">>}},
        ok = pqc_case_registry:publish(CaseId, Event),

        % Verify receipt
        receive
            {pqc_case_event, CaseId, Event} -> ok
        after 1000 ->
            ?assert(false, "Did not receive event")
        end
    end.
```

## Performance Characteristics

### ETS Read Performance

- **Lookup complexity**: O(1)
- **Concurrency**: Read-concurrent, multiple simultaneous readers
- **Contention**: Writes block only during insert/delete

### PubSub Performance

- **Publish complexity**: O(N) where N = number of subscribers
- **Group membership**: O(1) average for join/leave
- **Delivery**: Asynchronous message passing

### Memory Usage

- **ETS overhead**: ~8 bytes per entry + key/value size
- **Monitor overhead**: ~32 bytes per monitor
- **Expected usage**: ~100-200 bytes per active Case

## Future Enhancements

1. **Distributed registry**: Use pg distributed scope for multi-node
2. **Persistence**: Optional ETS disk backup via `ets:tab2file/2`
3. **Metrics**: Integration with erlmcp_metrics for observability
4. **Rate limiting**: Backpressure on publish operations
5. **Event filtering**: Selective subscription by event type
6. **History buffer**: Keep recent events for late subscribers

## Dependencies

- **OTP**: 23+ (for pg module)
- **ETS**: Built-in
- **pg**: Built-in (OTP 23+)
- **pqchain.hrl**: PQChain record definitions

## Files

- **Implementation**: `apps/swarmflow_pqchain/src/pqc_case_registry.erl`
- **Tests**: `apps/swarmflow_pqchain/test/pqc_case_registry_tests.erl`
- **Header**: `apps/swarmflow_pqchain/include/pqchain.hrl`
- **Documentation**: `apps/swarmflow_pqchain/docs/pqc_case_registry.md`

## References

- [OTP Patterns](../../../docs/otp-patterns.md)
- [pg documentation](https://www.erlang.org/doc/man/pg.html)
- [ETS documentation](https://www.erlang.org/doc/man/ets.html)
- [gen_server documentation](https://www.erlang.org/doc/man/gen_server.html)
