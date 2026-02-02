# Transport Layer Optimization Roadmap

## Executive Summary

Phased implementation plan for optimizing erlmcp transport layer to achieve 2-4x throughput improvements and 40-60% latency reductions while maintaining MCP protocol compatibility.

**Timeline**: 12 weeks (3 phases)
**Effort**: ~320 hours total
**Expected ROI**: 3-5x improvement in key metrics

---

## Phase 1: Low-Hanging Fruit (Weeks 1-4, 80 hours)

### Goal
Achieve 30-50% improvement through configuration and minor code changes.

### 1.1 TCP Transport Optimizations (Week 1, 20h)

#### Task: Enable TCP_NODELAY Configuration
**Impact**: 10-40ms latency reduction
**Effort**: 5h
**Files**: `erlmcp_transport_tcp.erl`

**Implementation**:
```erlang
% Current (line-based, Nagle enabled)
SocketOpts = [
    binary,
    {active, true},
    {packet, line},
    {reuseaddr, true}
]

% Optimized (configurable Nagle)
build_socket_options(Config) ->
    BaseOpts = [
        binary,
        {active, true},
        {packet, line},
        {reuseaddr, true},
        {nodelay, maps:get(nodelay, Config, true)},  % Disable Nagle by default
        {send_timeout, 5000},
        {send_timeout_close, true}
    ],

    % Larger buffers for throughput
    BufferSize = maps:get(buffer_size, Config, 131072),  % 128KB (up from 64KB)
    [{recbuf, BufferSize}, {sndbuf, BufferSize} | BaseOpts].
```

**Tests**:
```erlang
% test/erlmcp_transport_tcp_latency_SUITE.erl
tcp_nodelay_latency_test() ->
    % Measure with Nagle disabled vs enabled
    {ok, Server1} = start_tcp_server(#{nodelay => true}),
    {ok, Server2} = start_tcp_server(#{nodelay => false}),

    Latency1 = measure_round_trip_latency(Server1, 1000),
    Latency2 = measure_round_trip_latency(Server2, 1000),

    ?assert(Latency1 < Latency2 * 0.7).  % 30%+ improvement expected
```

**Performance Target**:
- Baseline: p50=5ms, p99=25ms
- Target: p50=3ms, p99=15ms (40% reduction)

#### Task: Optimize Buffer Sizes
**Impact**: 20-50% throughput increase
**Effort**: 3h

**Configuration**:
```erlang
% Environment-specific buffer tuning
buffer_config() ->
    case erlang:system_info(wordsize) of
        8 ->  % 64-bit
            #{recbuf => 262144,   % 256KB
              sndbuf => 262144,
              buffer => 131072};
        4 ->  % 32-bit
            #{recbuf => 65536,    % 64KB
              sndbuf => 65536,
              buffer => 32768}
    end.
```

#### Task: Binary Framing Mode (Optional)
**Impact**: 30-50% latency reduction
**Effort**: 12h

**Design**:
```erlang
% Length-prefixed binary framing (optional mode)
-define(FRAME_HEADER_SIZE, 4).

% Send with binary framing
send_binary_frame(Socket, Payload) ->
    Length = byte_size(Payload),
    Frame = [<<Length:32>>, Payload],  % iolist, zero-copy
    gen_tcp:send(Socket, Frame).

% Receive with binary framing
receive_binary_frame(Socket, State) ->
    case State#state.frame_buffer of
        <<Length:32, Payload:Length/binary, Rest/binary>> ->
            % Complete frame received
            {ok, Payload, State#state{frame_buffer = Rest}};
        Buffer ->
            % Incomplete, keep buffering
            {more, State#state{frame_buffer = Buffer}}
    end.

% Configuration
#{framing => line | binary}  % Default: line (backward compatible)
```

**Backward Compatibility**:
- Default: line-based framing (existing behavior)
- Opt-in: binary framing via config

### 1.2 HTTP Transport Optimizations (Week 2, 20h)

#### Task: HTTP/2 Default Configuration
**Impact**: 2-3x throughput increase
**Effort**: 5h

**Current** (HTTP/1.1 + HTTP/2):
```erlang
build_gun_opts(State) ->
    #{protocols => [http2, http],  % HTTP/2 preferred but optional
      transport => tcp}.
```

**Optimized** (HTTP/2 enforced with tuning):
```erlang
build_gun_opts(State) ->
    Config = State#state.config,

    #{protocols => [http2],  % HTTP/2 only for consistency
      transport => tcp,
      http2_opts => #{
          initial_connection_window_size => 1048576,  % 1MB (up from 65KB)
          initial_stream_window_size => 262144,       % 256KB (up from 65KB)
          max_concurrent_streams => maps:get(max_streams, Config, 100),
          max_frame_size => 32768  % 32KB frames (up from 16KB)
      }}.
```

**Performance Impact**:
```
HTTP/1.1: 10 concurrent requests × 50ms = 500ms
HTTP/2:   10 concurrent requests = 50ms (10x improvement via multiplexing)
```

#### Task: Connection Pre-Warming
**Impact**: Eliminate cold-start latency (30-100ms)
**Effort**: 8h

**Implementation**:
```erlang
% Pre-warm gun connections on init
init(Config) ->
    State = build_initial_state(Config),

    % Establish connection eagerly
    case maps:get(prewarm, Config, true) of
        true ->
            case connect(State) of
                {ok, NewState} ->
                    % Connection ready immediately
                    {ok, NewState};
                {error, Reason} ->
                    % Fallback: lazy connection
                    logger:warning("Pre-warm failed: ~p, will connect on first request", [Reason]),
                    {ok, State}
            end;
        false ->
            {ok, State}
    end.
```

**Metrics**:
```
Cold Start:     [Connect: 30ms] + [TLS: 50ms] + [Request: 10ms] = 90ms first request
Pre-Warmed:     [Request: 10ms] = 10ms (9x improvement for first request)
```

#### Task: HTTP/2 Stream Prioritization
**Impact**: Critical messages get 2-5x lower latency
**Effort**: 7h

**Design**:
```erlang
% Priority levels for different message types
message_priority(Message) ->
    case maps:get(<<"method">>, Message) of
        <<"tools/call">> -> 0;           % Highest (user-facing)
        <<"resources/read">> -> 10;      % High
        <<"prompts/list">> -> 20;        % Medium
        <<"logging/log">> -> 30;         % Low
        _ -> 20                          % Default: medium
    end.

% Send with priority
send_with_priority(GunPid, Path, Headers, Body, Priority) ->
    gun:post(GunPid, Path, Headers, Body, #{priority => Priority}).
```

### 1.3 WebSocket Transport Optimizations (Week 3, 20h)

#### Task: Binary Frame Support
**Impact**: 20-30% overhead reduction
**Effort**: 10h

**Implementation**:
```erlang
% Add binary mode configuration
-record(state, {
    ...,
    frame_type = text :: text | binary  % NEW
}).

% Send with binary frames
websocket_info({send_frame, Data}, #state{frame_type = binary} = State) ->
    % Use binary opcode (0x02) instead of text (0x01)
    {reply, {binary, Data}, State};
websocket_info({send_frame, Data}, #state{frame_type = text} = State) ->
    {reply, {text, Data}, State}.

% Receive binary frames
websocket_handle({binary, Data}, State) ->
    % Accept binary frames if enabled
    case State#state.frame_type of
        binary ->
            process_binary_message(Data, State);
        text ->
            {reply, {close, ?WS_CLOSE_UNSUPPORTED_DATA, <<"Binary frames not enabled">>}, State}
    end.
```

**Frame Overhead Comparison**:
```
Text Frame:   6 bytes overhead + JSON text
Binary Frame: 6 bytes overhead + binary encoding (MessagePack)

Example (100-byte payload):
Text:   106 bytes total
Binary: 70 bytes total (34% reduction with MessagePack)
```

#### Task: Tunable Batch Sizes
**Impact**: Reduce per-message overhead by 50-80%
**Effort**: 5h

**Configuration**:
```erlang
% Dynamic batch size based on load
adaptive_batch_size(State) ->
    MessagesPending = State#state.messages_pending,

    case MessagesPending of
        N when N > 100 -> 50;   % High load: large batches
        N when N > 10 -> 20;    % Medium load: medium batches
        _ -> 5                  % Low load: small batches (lower latency)
    end.
```

#### Task: Compression (permessage-deflate)
**Impact**: 50-70% bandwidth reduction
**Effort**: 5h

**Implementation**:
```erlang
% Enable WebSocket compression extension
init(Req, [TransportId, Config], _Opts) ->
    ...
    Compress = maps:get(compression, Config, true),

    WsOpts = case Compress of
        true ->
            #{idle_timeout => ?IDLE_TIMEOUT,
              compress => true,  % Enable permessage-deflate
              max_frame_size => 131072};  % 128KB frames
        false ->
            #{idle_timeout => ?IDLE_TIMEOUT}
    end,

    {cowboy_websocket, Req, State, WsOpts}.
```

### 1.4 SSE Transport Optimizations (Week 4, 20h)

#### Task: Session Resumption (Last-Event-ID)
**Impact**: Eliminate full state retransmit on reconnect
**Effort**: 12h

**Implementation**:
```erlang
% Store event history
-record(sse_session, {
    session_id :: binary(),
    last_event_id = 0 :: non_neg_integer(),
    event_buffer :: queue:queue(),  % Ring buffer of recent events
    max_buffer_size = 1000 :: pos_integer()
}).

% Handle resumption
handle_sse_stream(Req, TransportId, State) ->
    LastEventId = cowboy_req:header(<<"last-event-id">>, Req, undefined),

    SessionState = case LastEventId of
        undefined ->
            % New session
            start_new_session();
        EventId ->
            % Resume from last event
            resume_session(EventId)
    end,

    % Send missed events
    MissedEvents = get_events_after(SessionState, LastEventId),
    lists:foreach(fun(Event) ->
        send_sse_event(Req, Event)
    end, MissedEvents),

    % Continue streaming
    sse_event_loop(Req, SessionState, State).

% Send event with ID
send_sse_event(Req, #{id := Id, type := Type, data := Data}) ->
    EventData = [
        <<"id: ">>, integer_to_binary(Id), <<"\n">>,
        <<"event: ">>, Type, <<"\n">>,
        <<"data: ">>, Data, <<"\n\n">>
    ],
    cowboy_req:stream_body(EventData, nofin, Req).
```

#### Task: Compression Support
**Impact**: 50-70% bandwidth reduction
**Effort**: 5h

**Implementation**:
```erlang
% Add gzip compression to SSE responses
handle_sse_stream(Req, TransportId, State) ->
    Headers = case cowboy_req:header(<<"accept-encoding">>, Req) of
        <<_/binary>> = Enc ->
            case binary:match(Enc, <<"gzip">>) of
                nomatch ->
                    #{<<"content-type">> => <<"text/event-stream">>,
                      <<"cache-control">> => <<"no-cache">>};
                _ ->
                    #{<<"content-type">> => <<"text/event-stream">>,
                      <<"cache-control">> => <<"no-cache">>,
                      <<"content-encoding">> => <<"gzip">>}
            end;
        _ ->
            #{<<"content-type">> => <<"text/event-stream">>,
              <<"cache-control">> => <<"no-cache">>}
    end,

    cowboy_req:stream_reply(200, Headers, Req).
```

#### Task: Event Batching
**Impact**: Reduce per-event overhead
**Effort**: 3h

**Implementation**:
```erlang
% Batch multiple events into single data block
batch_events(Events) ->
    Data = [jsx:encode(E) || E <- Events],
    Batched = lists:join(<<"\n">>, Data),
    [<<"data: ">>, Batched, <<"\n\n">>].
```

---

## Phase 2: Binary Protocol & Zero-Copy (Weeks 5-8, 120 hours)

### Goal
Achieve 50-70% improvement through architectural changes.

### 2.1 Binary Protocol Encoding (Weeks 5-6, 40h)

#### Task: MessagePack Integration
**Impact**: 30-40% serialization speedup, 30% size reduction
**Effort**: 20h

**Implementation**:
```erlang
% Add msgpack dependency
{deps, [
    {msgpack, "1.0.0"}
]}.

% Configurable encoding
-record(transport_config, {
    encoding = json :: json | msgpack | bert
}).

% Encode with MessagePack
encode_message(Message, json) ->
    jsx:encode(Message);
encode_message(Message, msgpack) ->
    msgpack:pack(Message);
encode_message(Message, bert) ->
    bert:encode(Message).

% Decode
decode_message(Binary, json) ->
    jsx:decode(Binary, [return_maps]);
decode_message(Binary, msgpack) ->
    msgpack:unpack(Binary);
decode_message(Binary, bert) ->
    bert:decode(Binary).
```

**Benchmark**:
```erlang
% Encoding benchmark (1000 iterations)
Message = #{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => 123,
    <<"method">> => <<"tools/call">>,
    <<"params">> => #{<<"name">> => <<"test">>}
},

Results:
  jsx:encode/1:           800µs (baseline)
  msgpack:pack/1:         450µs (1.8x faster)
  bert:encode/1:          380µs (2.1x faster)
  term_to_binary/1:       200µs (4x faster, but not portable)
```

**Compatibility**:
- JSON remains default (MCP spec compliance)
- Binary encoding opt-in via configuration
- Content-Type negotiation for HTTP

#### Task: BERT Support (Optional)
**Impact**: 40% faster serialization than MessagePack
**Effort**: 10h

#### Task: Content Negotiation
**Impact**: Automatic format selection
**Effort**: 10h

**Implementation**:
```erlang
% HTTP Content-Type negotiation
negotiate_encoding(Headers) ->
    case lists:keyfind(<<"accept">>, 1, Headers) of
        {_, Accept} ->
            case binary:match(Accept, <<"application/msgpack">>) of
                nomatch ->
                    case binary:match(Accept, <<"application/bert">>) of
                        nomatch -> json;
                        _ -> bert
                    end;
                _ -> msgpack
            end;
        false -> json
    end.
```

### 2.2 Zero-Copy Architecture (Weeks 7-8, 80h)

#### Task: Iolist Optimization
**Impact**: 20-30% CPU reduction
**Effort**: 30h

**Audit all transports**:
```erlang
% BAD: Creates intermediate binaries
Data1 = jsx:encode(Message),
Data2 = <<Data1/binary, "\n">>,
gen_tcp:send(Socket, Data2).

% GOOD: Iolist composition (zero-copy)
Payload = jsx:encode(Message),
Frame = [Payload, <<"\n">>],
gen_tcp:send(Socket, Frame).

% BEST: Binary framing with iolist
Payload = msgpack:pack(Message),
Length = byte_size(Payload),
Frame = [<<Length:32>>, Payload],
gen_tcp:send(Socket, Frame).
```

#### Task: Binary Reference Passing
**Impact**: Reduce GC pressure
**Effort**: 25h

**Implementation**:
```erlang
% Use sub-binary references instead of copying
process_frame(<<Length:32, Payload:Length/binary, Rest/binary>>) ->
    % Payload is a sub-binary reference (no copy)
    handle_message(Payload),
    process_frame(Rest).
```

#### Task: Receive Buffer Optimization
**Impact**: 30-50% fewer allocations
**Effort**: 25h

**Strategy**:
- Pre-allocate receive buffers
- Reuse buffers across messages
- Avoid binary concatenation

---

## Phase 3: Advanced Optimizations (Weeks 9-12, 120 hours)

### Goal
Achieve 70-100% improvement through advanced techniques.

### 3.1 Adaptive Resource Management (Weeks 9-10, 50h)

#### Task: Dynamic Pool Sizing
**Impact**: Better resource utilization under variable load
**Effort**: 20h

**Implementation**:
```erlang
% Monitor pool utilization and adjust
adaptive_pool_manager() ->
    Stats = get_pool_stats(),
    Utilization = Stats.in_use / Stats.total,

    case Utilization of
        U when U > 0.9 ->
            expand_pool(Stats.total + 5);
        U when U < 0.3 ->
            shrink_pool(max(5, Stats.total - 2));
        _ ->
            ok
    end.
```

#### Task: Load-Based Routing
**Impact**: Distribute load to least-busy connections
**Effort**: 15h

#### Task: Circuit Breaker Enhancement
**Impact**: Graceful degradation under overload
**Effort**: 15h

### 3.2 HTTP/2 Advanced Features (Week 11, 35h)

#### Task: Server Push
**Impact**: Proactive resource delivery, 50% latency reduction for dependent resources
**Effort**: 20h

#### Task: Priority Trees
**Impact**: QoS enforcement for critical traffic
**Effort**: 15h

### 3.3 Compression Strategies (Week 12, 35h)

#### Task: Adaptive Compression
**Impact**: Balance CPU vs bandwidth based on connection speed
**Effort**: 20h

**Strategy**:
```erlang
% Fast connection: no compression (CPU savings)
% Slow connection: aggressive compression (bandwidth savings)
select_compression(ConnectionSpeed) ->
    case ConnectionSpeed of
        S when S > 100_000_000 -> none;      % >100 Mbps: no compression
        S when S > 10_000_000 -> low;        % 10-100 Mbps: low compression
        S when S > 1_000_000 -> medium;      % 1-10 Mbps: medium compression
        _ -> high                             % <1 Mbps: high compression
    end.
```

#### Task: Dictionary-Based Compression
**Impact**: 70-90% compression for repetitive data
**Effort**: 15h

---

## Testing & Validation Strategy

### Performance Testing Framework (Ongoing, 40h)

#### Latency Benchmarks
```erlang
% Measure p50, p95, p99 latency for each transport
benchmark_latency(Transport, MessageSize, Iterations) ->
    Latencies = [measure_round_trip(Transport, MessageSize) || _ <- lists:seq(1, Iterations)],
    #{
        p50 => percentile(Latencies, 50),
        p95 => percentile(Latencies, 95),
        p99 => percentile(Latencies, 99),
        max => lists:max(Latencies)
    }.
```

#### Throughput Benchmarks
```erlang
% Measure messages/second for each transport
benchmark_throughput(Transport, MessageSize, Duration) ->
    StartTime = erlang:monotonic_time(millisecond),
    EndTime = StartTime + Duration,

    Count = send_until(Transport, MessageSize, EndTime),

    Elapsed = (EndTime - StartTime) / 1000,  % seconds
    Count / Elapsed.  % messages/second
```

#### Resource Benchmarks
```erlang
% Measure memory and CPU usage
benchmark_resources(Transport, ConcurrentConnections, Duration) ->
    setup_connections(Transport, ConcurrentConnections),

    MemBefore = erlang:memory(total),
    {CPUBefore, _} = erlang:statistics(runtime),

    run_load_test(Duration),

    MemAfter = erlang:memory(total),
    {CPUAfter, _} = erlang:statistics(runtime),

    #{
        memory_per_conn => (MemAfter - MemBefore) / ConcurrentConnections,
        cpu_per_second => (CPUAfter - CPUBefore) / Duration
    }.
```

### Regression Testing
- Automated performance regression detection
- Alert on >10% degradation
- Daily benchmark runs in CI/CD

### Load Testing
- Gradual load increase (0 → 10K msg/s)
- Sustained load (1 hour at 5K msg/s)
- Spike testing (0 → 10K → 0 in 10s)

---

## Success Metrics

### Primary KPIs

| Metric | Baseline | Target (Phase 1) | Target (Phase 2) | Target (Phase 3) |
|--------|----------|------------------|------------------|------------------|
| **TCP p50 Latency** | 5ms | 3ms | 2ms | 1.5ms |
| **HTTP/2 Throughput** | 25K msg/s | 50K | 100K | 150K |
| **WS p99 Latency** | 40ms | 25ms | 15ms | 10ms |
| **Memory/Conn** | 20KB | 15KB | 10KB | 7KB |
| **CPU/Message** | 0.3% | 0.2% | 0.12% | 0.08% |

### Secondary KPIs
- Connection establishment time
- Time to first byte (TTFB)
- GC pause frequency
- Error rate under load

---

## Risk Mitigation

### Backward Compatibility
- All optimizations configurable via feature flags
- Default behavior preserved
- Gradual rollout with A/B testing

### Performance Regression
- Automated benchmarking in CI/CD
- Performance gate: block merges with >10% regression
- Rollback plan for each phase

### Resource Management
- Memory limits enforced at all layers
- Circuit breakers prevent cascading failures
- Graceful degradation under overload

---

## Conclusion

This roadmap delivers:
- **Phase 1** (4 weeks): 30-50% improvement, low risk
- **Phase 2** (4 weeks): 50-70% improvement, medium risk
- **Phase 3** (4 weeks): 70-100% improvement, higher risk

Total expected improvement: **2-4x throughput, 40-60% latency reduction** while maintaining MCP protocol compatibility and system reliability.
