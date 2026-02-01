# SSE Resumption Implementation Checklist

**Track progress**: Mark items with `[x]` when complete

---

## Phase 1: Transport Layer Changes

### File: `apps/erlmcp_transports/src/erlmcp_transport_sse.erl`

#### 1.1 Add Configuration Defines

- [ ] Add `?EVENT_TTL` macro (3600000 ms = 1 hour)
- [ ] Add `?REPLAY_WINDOW` macro (86400000 ms = 24 hours)
- [ ] Add `?MAX_SESSION_EVENTS` macro (10000 events)

**Location**: Top of file, after existing defines

```erlang
-define(EVENT_TTL, 3600000).        %% 1 hour event TTL
-define(REPLAY_WINDOW, 86400000).   %% 24 hour replay window
-define(MAX_SESSION_EVENTS, 10000). %% Max events per session
```

#### 1.2 Add Helper Functions

- [ ] Implement `get_event_ttl/0`
- [ ] Implement `get_replay_window/0`
- [ ] Implement `get_max_session_events/0`

**Location**: After `format_sse_event/2`, before `generate_session_id/1`

```erlang
%% @doc Read event TTL from configuration
-spec get_event_ttl() -> pos_integer().
get_event_ttl() ->
    case application:get_env(erlmcp, sse) of
        {ok, SSEConfig} ->
            proplists:get_value(event_ttl, SSEConfig, ?EVENT_TTL);
        undefined ->
            ?EVENT_TTL
    end.

%% @doc Read replay window from configuration
-spec get_replay_window() -> pos_integer().
get_replay_window() ->
    case application:get_env(erlmcp, sse) of
        {ok, SSEConfig} ->
            proplists:get_value(replay_window, SSEConfig, ?REPLAY_WINDOW);
        undefined ->
            ?REPLAY_WINDOW
    end.

%% @doc Read max session events from configuration
-spec get_max_session_events() -> pos_integer().
get_max_session_events() ->
    case application:get_env(erlmcp, sse) of
        {ok, SSEConfig} ->
            proplists:get_value(max_session_events, SSEConfig, ?MAX_SESSION_EVENTS);
        undefined ->
            ?MAX_SESSION_EVENTS
    end.
```

#### 1.3 Add format_sse_event_with_id Function

- [ ] Implement `format_sse_event_with_id/3`

**Location**: After `format_sse_event/2`

```erlang
%% @doc Format SSE event with ID for resumption
%% Format: "id: <id>\nevent: <type>\ndata: <json>\n\n"
-spec format_sse_event_with_id(binary(), binary(), binary()) -> binary().
format_sse_event_with_id(EventType, EventId, Data) ->
    <<"id: ", EventId/binary, "\nevent: ", EventType/binary,
      "\ndata: ", Data/binary, "\n\n">>.
```

#### 1.4 Implement replay_missed_events Function

- [ ] Implement `replay_missed_events/4`
- [ ] Add OTEL tracing
- [ ] Add error handling
- [ ] Add logging

**Location**: Before `handle_sse_stream/3`

```erlang
%% @doc Replay missed events from event store
-spec replay_missed_events(
    cowboy_req:req(),
    binary(),
    binary() | undefined,
    otel:span_ctx()
) -> ok.
replay_missed_events(Req, SessionId, LastEventId, SpanCtx) ->
    case LastEventId of
        undefined ->
            erlmcp_tracing:set_attributes(SpanCtx, #{<<"replay">> => <<"none">>}),
            ok;
        _ ->
            erlmcp_tracing:set_attributes(SpanCtx, #{<<"replay">> => <<"active">>}),

            case erlmcp_sse_event_store:get_events_since(SessionId, LastEventId) of
                {ok, []} ->
                    erlmcp_tracing:set_attributes(SpanCtx, #{<<"missed_events">> => 0}),
                    ok;
                {ok, MissedEvents} ->
                    MissedCount = length(MissedEvents),
                    erlmcp_tracing:set_attributes(SpanCtx, #{<<"missed_events">> => MissedCount}),

                    lists:foreach(fun(EventData) ->
                        FormattedEvent = format_sse_event(<<"message">>, EventData),
                        cowboy_req:stream_body(FormattedEvent, nofin, Req)
                    end, MissedEvents),

                    ok;
                {error, Reason} ->
                    erlmcp_tracing:record_error_details(SpanCtx, replay_error, Reason),
                    logger:error("Failed to replay events for session ~s: ~p", [SessionId, Reason]),
                    ok
            end
    end.
```

#### 1.5 Update handle_sse_stream Function

- [ ] Parse `Last-Event-ID` header
- [ ] Call `replay_missed_events/4`
- [ ] Initialize `event_counter` in StreamState
- [ ] Add OTEL attributes for Last-Event-ID

**Location**: In `handle_sse_stream/3` function

```erlang
handle_sse_stream(Req, TransportId, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"transport_sse.handle_stream">>),

    %% Parse Last-Event-ID header
    LastEventId = cowboy_req:header(<<"last-event-id">>, Req),

    %% Generate unique client ID and session ID
    ClientId = erlang:list_to_binary(erlang:pid_to_list(self())),
    SessionId = generate_session_id(ClientId),

    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"client_id">> => ClientId,
        <<"session_id">> => SessionId,
        <<"transport_id">> => TransportId,
        <<"last_event_id">> => LastEventId
    }),

    %% Set SSE headers
    Headers = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>,
        <<"x-accel-buffering">> => <<"no">>
    },

    Req2 = cowboy_req:stream_reply(200, Headers, Req),

    %% Fetch missed events if Last-Event-ID present
    ok = replay_missed_events(Req2, SessionId, LastEventId, SpanCtx),

    %% Register with registry
    RegistryPid = erlmcp_registry:get_pid(),
    RegistryPid ! {transport_connected, TransportId, #{
        client_id => ClientId,
        session_id => SessionId,
        last_event_id => LastEventId
    }},

    %% Start ping timer
    {ok, PingRef} = timer:send_interval(?PING_INTERVAL, ping),

    %% Initialize event counter for this session
    EventCounter = case LastEventId of
        undefined -> 0;
        _ -> erlmcp_sse_event_store:parse_event_id(LastEventId)
    end,

    sse_event_loop(Req2, #{
        transport_id => TransportId,
        client_id => ClientId,
        session_id => SessionId,
        registry_pid => RegistryPid,
        ping_timer => PingRef,
        span_ctx => SpanCtx,
        event_counter => EventCounter,
        last_event_id => LastEventId
    }, State).
```

#### 1.6 Update sse_event_loop Function

- [ ] Add `event_counter` pattern matching
- [ ] Increment counter on each event
- [ ] Store event in event store
- [ ] Use `format_sse_event_with_id/3`
- [ ] Update StreamState with new counter

**Location**: In `sse_event_loop/3` function

```erlang
sse_event_loop(Req, StreamState = #{
    event_counter := Counter,
    session_id := SessionId
}, State) ->
    receive
        ping ->
            PingData = <<":\n\n">>,
            cowboy_req:stream_body(PingData, nofin, Req),
            sse_event_loop(Req, StreamState, State);

        {send_event, EventType, Data} ->
            %% Increment event counter
            NewCounter = Counter + 1,

            %% Store event for resumption
            EventId = case erlmcp_sse_event_store:add_event(SessionId, NewCounter, Data) of
                {ok, GeneratedId} -> GeneratedId;
                {error, StoreReason} ->
                    logger:error("Failed to store event: ~p", [StoreReason]),
                    %% Fallback: generate ID locally
                    <<SessionId/binary, "_", (integer_to_binary(NewCounter))/binary>>
            end,

            %% Format as SSE event WITH ID
            EventData = format_sse_event_with_id(EventType, EventId, Data),
            cowboy_req:stream_body(EventData, nofin, Req),

            %% Update stream state
            UpdatedState = StreamState#{event_counter => NewCounter},
            sse_event_loop(Req, UpdatedState, State);

        close ->
            cowboy_req:stream_body(<<>>, fin, Req),
            {ok, Req, State};

        _ ->
            sse_event_loop(Req, StreamState, State)
    after 300000 ->
        cowboy_req:stream_body(<<>>, fin, Req),
        {ok, Req, State}
    end.
```

---

## Phase 2: SSE Manager Changes

### File: `apps/erlmcp_transports/src/erlmcp_transport_sse_manager.erl`

#### 2.1 Update conn_info Record

- [ ] Add `last_event_number` field
- [ ] Add `connected_at` field
- [ ] Update record definition

**Location**: Record definition at top of file

```erlang
-record(conn_info, {
    session_id :: binary(),
    client_pid :: pid(),          %% ADD THIS
    req :: term(),
    ping_timer :: reference(),
    last_event_number :: non_neg_integer(),  %% ADD THIS
    connected_at :: integer()                %% ADD THIS
}).
```

#### 2.2 Update register_connection Handler

- [ ] Parse Last-Event-ID
- [ ] Initialize `last_event_number`
- [ ] Set `connected_at` timestamp
- [ ] Add client_pid tracking

**Location**: In `handle_cast/2` function

```erlang
handle_cast({register_connection, SessionId, Req}, State) ->
    ?LOG_DEBUG("Registering SSE connection: ~p", [SessionId]),

    %% Parse last event number (assuming Last-Event-ID passed via metadata)
    %% For now, initialize to 0
    NewConnections = maps:put(SessionId,
                              #conn_info{
                                  session_id => SessionId,
                                  client_pid => self(),  %% Get caller PID
                                  req => Req,
                                  ping_timer => undefined,
                                  last_event_number => 0,
                                  connected_at => erlang:system_time(millisecond)
                              },
                              State#state.connections),
    {noreply, State#state{connections = NewConnections}};
```

#### 2.3 Add update_event_counter Handler

- [ ] Implement `update_event_counter` cast
- [ ] Update `last_event_number` in conn_info

**Location**: In `handle_cast/2` function

```erlang
handle_cast({update_event_counter, SessionId, EventNumber}, State) ->
    NewConnections = maps:update_with(
        SessionId,
        fun(ConnInfo) -> ConnInfo#conn_info{last_event_number = EventNumber} end,
        State#state.connections
    ),
    {noreply, State#state{connections = NewConnections}};
```

#### 2.4 Add get_session_info Handler

- [ ] Implement `get_session_info` call
- [ ] Return session metadata

**Location**: In `handle_call/3` function

```erlang
handle_call({get_session_info, SessionId}, _From, State) ->
    case maps:get(SessionId, State#state.connections, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ConnInfo ->
            Info = #{
                session_id => ConnInfo#conn_info.session_id,
                last_event_number => ConnInfo#conn_info.last_event_number,
                connected_at => ConnInfo#conn_info.connected_at,
                connected_duration => erlang:system_time(millisecond) - ConnInfo#conn_info.connected_at
            },
            {reply, {ok, Info}, State}
    end;
```

---

## Phase 3: Configuration

### File: `config/sys.config`

- [ ] Add `event_ttl` to SSE config
- [ ] Add `replay_window` to SSE config
- [ ] Add `max_session_events` to SSE config

**Location**: In `{erlmcp, [...]}` section

```erlang
{erlmcp, [
    %% ... other config ...

    %% Server-Sent Events (SSE) Transport Configuration
    {sse, [
        {enabled, true},
        {port, 8081},
        {path, "/mcp/sse"},
        {keepalive_interval, 30000},
        {stream_timeout, 300000},
        {retry_timeout, 5000},
        {event_ttl, 3600000},           %% 1 hour - event retention
        {replay_window, 86400000},      %% 24 hours - max replay window
        {max_session_events, 10000},    %% Max events per session
        {cleanup_interval, 300000}      %% 5 minutes - event cleanup
    ]}
]}.
```

---

## Phase 4: Testing

### File: `apps/erlmcp_transports/test/erlmcp_sse_resumption_integration_tests.erl`

- [ ] Create test file with module header
- [ ] Add setup/0 fixture
- [ ] Add cleanup/1 fixture
- [ ] Implement `add_events/2` helper

#### Test Suites

- [ ] **Last-Event-ID Header Tests** (3 tests)
  - [ ] `test_parse_last_event_id_header/0`
  - [ ] `test_parse_missing_last_event_id/0`
  - [ ] `test_parse_malformed_last_event_id/0`

- [ ] **Event Replay Tests** (3 tests)
  - [ ] `test_replay_after_disconnect/0`
  - [ ] `test_replay_with_multiple_disconnects/0`
  - [ ] `test_replay_event_ordering/0`

- [ ] **Session Continuity Tests** (3 tests)
  - [ ] `test_session_id_reuse/0`
  - [ ] `test_event_counter_continuity/0`
  - [ ] `test_session_state_tracking/0`

- [ ] **Configuration Tests** (3 tests)
  - [ ] `test_event_ttl_config/0`
  - [ ] `test_replay_window_config/0`
  - [ ] `test_max_session_events_config/0`

- [ ] **Edge Cases** (5 tests)
  - [ ] `test_replay_empty_session/0`
  - [ ] `test_replay_single_event/0`
  - [ ] `test_replay_expired_events/0`
  - [ ] `test_event_store_failure/0`
  - [ ] `test_invalid_event_id_format/0`

#### Test Execution

- [ ] Run tests: `rebar3 eunit --module=erlmcp_sse_resumption_integration_tests`
- [ ] Verify all tests pass
- [ ] Check coverage: `rebar3 cover --verbose`
- [ ] Ensure >= 85% coverage

---

## Phase 5: Compilation & Verification

### Compilation

- [ ] Run `rebar3 compile`
- [ ] Verify no compilation errors
- [ ] Verify no warnings
- [ ] Check Dialyzer: `rebar3 dialyzer`
- [ ] Check Xref: `rebar3 xref`

### Testing

- [ ] Run event store tests: `rebar3 eunit --module=erlmcp_sse_event_store_tests`
- [ ] Run replay tests: `rebar3 eunit --module=erlmcp_sse_event_store_replay_tests`
- [ ] Run resumption tests: `rebar3 eunit --module=erlmcp_sse_resumption_integration_tests`
- [ ] Run all transport tests: `rebar3 eunit --app=erlmcp_transports`

### Coverage

- [ ] Generate coverage report: `rebar3 cover`
- [ ] Verify coverage >= 85%
- [ ] Review uncovered lines
- [ ] Add tests for uncovered code if needed

---

## Phase 6: Documentation

- [ ] Update `docs/sse-stream-resumability.md`
- [ ] Update MCP compliance matrix
- [ ] Add integration examples
- [ ] Document configuration options
- [ ] Update architecture diagrams

---

## Phase 7: Performance Testing

- [ ] Benchmark replay latency (100 events)
- [ ] Benchmark header parsing
- [ ] Benchmark event ID generation
- [ ] Benchmark event storage
- [ ] Check for memory leaks

**Performance Targets**:
- Replay latency (p95): < 50ms for 100 events
- Header parsing: < 1ms
- Event ID generation: < 1ms
- Event storage: < 5ms per event

---

## Final Verification

### Functional Requirements

- [ ] Last-Event-ID header parsed correctly
- [ ] Missed events replayed on reconnection
- [ ] Event IDs present in all SSE events
- [ ] Session continuity across disconnects
- [ ] Configuration (TTL, replay window) respected

### Non-Functional Requirements

- [ ] Replay latency < 50ms (100 events)
- [ ] No memory leaks (ETS cleanup verified)
- [ ] No performance regression in live streaming
- [ ] Comprehensive error handling
- [ ] Full OTEL observability

### MCP 2025-11-25 Compliance

- [ ] HTTP Streamable Transport support
- [ ] SSE event IDs for resumption
- [ ] Last-Event-ID header handling
- [ ] SSE retry field (Gap #29)
- [ ] Session management (Gap #2)

### Quality Gates

- [ ] All tests passing (100% pass rate)
- [ ] Test coverage >= 85%
- [ ] No Dialyzer warnings
- [ ] No Xref errors
- [ ] OTEL tracing integrated
- [ ] Chicago School TDD followed (no mocks)

---

## Progress Summary

**Total Items**: 70
**Completed**: 0
**Remaining**: 70
**Progress**: 0%

---

## Notes

- Each item should be completed and tested before marking as done
- Run tests after each phase to catch issues early
- Commit changes after each phase for easy rollback
- Document any deviations from this checklist
- Update this checklist as needed during implementation

---

**Implementation Estimate**: 60-90 minutes
**Priority**: P0 - MCP 2025-11-25 Compliance
**Status**: 📋 Ready to Start
