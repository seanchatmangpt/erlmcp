# Cowboy HTTP Transport Optimization Guide
## Prioritized Implementation Recommendations for ErlMCP v0.7+

**Document:** Implementation Guide for Critical HTTP Transport Improvements
**Scope:** Cowboy 2.10, Gun 2.0.1, WebSocket RFC 6455, SSE WHATWG
**Timeline:** 4-6 weeks for full implementation
**Priority Levels:** Critical, High, Medium, Low

---

## Quick Reference: Implementation Priority Matrix

```
┌─────────────────────────────────────────────────────────────┐
│ CRITICAL (Week 1)                                           │
├─────────────────────────────────────────────────────────────┤
│ 1. WebSocket backpressure handling          [2-3h] HIGH   │
│ 2. Cowboy listener timeout configuration    [1h]   HIGH   │
│ 3. Request queue size limits                [1h]   HIGH   │
│ 4. Connection pool resource tracking        [2h]   HIGH   │
├─────────────────────────────────────────────────────────────┤
│ HIGH (Weeks 2-3)                                            │
├─────────────────────────────────────────────────────────────┤
│ 5. Middleware chain framework               [4h]   MEDIUM │
│ 6. Compression support (gzip)               [3h]   MEDIUM │
│ 7. Complete security headers                [1h]   MEDIUM │
│ 8. Enhanced Gun configuration               [2h]   MEDIUM │
├─────────────────────────────────────────────────────────────┤
│ MEDIUM (Week 4-5)                                           │
├─────────────────────────────────────────────────────────────┤
│ 9. Performance testing suite                [6h]   LOW    │
│ 10. Event store persistence layer           [6h]   LOW    │
│ 11. HTTP/2 server push (optional)           [4h]   LOW    │
└─────────────────────────────────────────────────────────────┘
```

---

## CRITICAL FIXES - Week 1

### CRITICAL FIX #1: WebSocket Backpressure Handling

**Current Issue:** No mechanism to prevent client buffer overflow when messages arrive faster than client can consume.

**Risk Level:** HIGH - Can cause out-of-memory on pathological clients
**Time Estimate:** 2-3 hours
**Files to Modify:** `src/erlmcp_transport_ws.erl`

#### Implementation Steps:

**Step 1: Update state record**

```erlang
% Before:
-record(state, {
    transport_id :: binary(),
    session_id :: binary(),
    ...
}).

% After: Add frame queue tracking
-record(state, {
    transport_id :: binary(),
    session_id :: binary(),
    pending_send_queue = 0 :: non_neg_integer(),  % NEW
    max_pending_sends = 100 :: non_neg_integer(), % NEW
    ...
}).
```

**Step 2: Implement send frame queue tracking**

```erlang
% In websocket_info/2 callback:
websocket_info({send_frame, Data}, State) ->
    MaxPending = State#state.max_pending_sends,
    case State#state.pending_send_queue >= MaxPending of
        true ->
            % Backpressure: queue is full
            logger:warning("WebSocket send queue full (~p pending), "
                          "rejecting frame to prevent overflow",
                          [State#state.pending_send_queue]),
            %% Could optionally queue the message and retry later,
            %% or close connection with 1008 policy violation
            {reply, {close, 1008, <<"Send queue full">>}, State};
        false ->
            %% Send frame and increment counter
            NewQueue = State#state.pending_send_queue + 1,
            {reply, {text, Data},
             State#state{pending_send_queue = NewQueue}}
    end;

% Add handler for when frame successfully sent:
websocket_info({frame_ack, _StreamRef}, State) ->
    %% This would require Cowboy enhancement to receive ACK callbacks
    %% For now, implement credit-based flow control via rate limiting
    NewQueue = max(0, State#state.pending_send_queue - 1),
    {ok, State#state{pending_send_queue = NewQueue}};

websocket_info(_Other, State) ->
    {ok, State}.
```

**Step 3: Configuration in sys.config**

```erlang
{websocket, [
    {enabled, true},
    {port, 8080},
    {path, "/mcp/ws"},
    {max_pending_sends, 100},      % NEW: max frames in flight
    {send_queue_timeout, 5000},    % NEW: time to drain queue
    {backpressure_mode, close}     % NEW: close|queue|drop
]}
```

**Step 4: Testing**

```erlang
% test/erlmcp_ws_backpressure_test.erl
-module(erlmcp_ws_backpressure_test).
-include_lib("eunit/include/eunit.hrl").

backpressure_close_on_full_queue_test() ->
    %% Connect WebSocket client
    {ok, Ws} = connect_ws(<<"ws://localhost:8080/mcp/ws">>),

    %% Send max_pending_sends frames
    send_n_frames(Ws, 100),

    %% Try to send one more - should be rejected
    {error, backpressure} = ws:send(Ws, {text, <<"frame 101">>}),

    {ok, {close, 1008, <<"Send queue full">>}} = ws:recv(Ws),

    ok.
```

#### Benefits:
- ✅ Prevents out-of-memory scenarios
- ✅ Detects slow/stalled clients
- ✅ Configurable per deployment
- ✅ Proper error reporting (1008 close code)

---

### CRITICAL FIX #2: Cowboy Listener Configuration

**Current Issue:** Missing timeout configurations, no connection limits, listener name collision risk.

**Risk Level:** HIGH - Can hang connections, exhaust resources
**Time Estimate:** 1 hour
**Files to Modify:** `src/erlmcp_transport_ws.erl:83-85`, `src/erlmcp_transport_sse.erl:60-62`

#### Implementation:

**Current (Line 83-85):**
```erlang
{ok, _} = cowboy:start_clear(erlmcp_ws_listener,
    [{port, Port}],
    #{env => #{dispatch => Dispatch}}).
```

**Recommended:**
```erlang
%% Generate unique listener name to prevent collisions
ListenerId = {erlmcp_ws_listener, TransportId, erlang:unique_integer()},

{ok, _} = cowboy:start_clear(
    ListenerId,
    [
        {port, Port},
        {ip, {127, 0, 0, 1}},  % Localhost binding (Gap #32)
        {backlog, 1024}        % TCP SYN backlog
    ],
    #{
        %% Connection settings
        connection_type => supervisor,  % Use supervisors for each connection
        max_connections => 1000,        % Prevent resource exhaustion

        %% Header validation
        max_header_value_length => 8192,
        max_skip_body_length => 1000000,

        %% Timeouts (milliseconds)
        request_timeout => 30000,       % 30 seconds for HTTP upgrade
        keepalive_timeout => 65000,     % 65 seconds for keep-alive

        %% WebSocket specific
        idle_timeout => 300000,         % 5 minutes for WebSocket idle

        %% HTTP/2 settings
        enable_http2 => true,
        http2_opts => #{
            keepalive => 65000,
            settings_timeout => 5000,
            max_concurrent_streams => 100
        },

        %% Environment
        env => #{dispatch => Dispatch}
    }
).
```

#### Testing Validation:

```bash
# Check listener started
erl -eval '
    Pid = rpc:call(erlmcp@localhost, cowboy, all_connections, []),
    io:format("Running listeners: ~p~n", [Pid])
' -s init stop

# Test timeout enforcement
(with slow client: send partial headers, wait 30s - should timeout)
```

---

### CRITICAL FIX #3: Request Queue Size Limits

**Current Issue:** `message_queue` in erlmcp_transport_http_server.erl unbounded, can exhaust memory.

**Risk Level:** HIGH - Memory exhaustion under load
**Time Estimate:** 1 hour
**Files to Modify:** `src/erlmcp_transport_http_server.erl:25-26, 422-443`

#### Implementation:

**Current State Record:**
```erlang
-record(state, {
    ...
    message_queue :: queue:queue({binary(), {pid(), reference()}}),
    ...
}).
```

**Enhanced State Record:**
```erlang
-record(state, {
    ...
    message_queue :: queue:queue(term()),
    max_queue_size = 10000 :: non_neg_integer(),     % NEW
    current_queue_size = 0 :: non_neg_integer(),     % NEW
    queue_overflows = 0 :: non_neg_integer(),        % Metrics
    ...
}).
```

**Update enqueue_request/3:**

```erlang
-spec enqueue_request(binary(), {pid(), term()}, state()) -> state().
enqueue_request(Data, From, State) ->
    MaxSize = State#state.max_queue_size,
    CurrentSize = State#state.current_queue_size,

    case CurrentSize >= MaxSize of
        true ->
            %% Queue is full
            logger:error(
                "HTTP request queue full (~p/~p), rejecting request",
                [CurrentSize, MaxSize]
            ),
            gen_server:reply(From, {error, queue_full}),
            Metrics = {erlmcp_http_queue_overflow, 1},
            telemetry:increment(Metrics),
            State#state{queue_overflows = State#state.queue_overflows + 1};

        false ->
            %% Add to queue
            NewQueue = queue:in({Data, From}, State#state.message_queue),
            State#state{
                message_queue = NewQueue,
                current_queue_size = CurrentSize + 1
            }
    end.
```

**Update process_request_queue/1:**

```erlang
-spec process_request_queue(state()) -> state().
process_request_queue(#state{active_requests = Active,
                             pool_size = PoolSize} = State)
  when Active >= PoolSize ->
    %% Pool is full, don't process more
    State;
process_request_queue(#state{message_queue = Queue,
                             current_queue_size = Size} = State) ->
    case queue:out(Queue) of
        {{value, {Data, From}}, NewQueue} ->
            NewState = State#state{
                message_queue = NewQueue,
                current_queue_size = Size - 1
            },
            process_request_queue(send_request(Data, From, NewState));
        {empty, _} ->
            State
    end.
```

**Configuration:**
```erlang
{http_client, [
    {max_queue_size, 10000},      % NEW
    {pool_size, 10},
    {max_retries, 3}
]}
```

#### Benefits:
- ✅ Prevents unbounded queue growth
- ✅ Explicit rejection when full
- ✅ Metrics for monitoring
- ✅ Fair request distribution

---

### CRITICAL FIX #4: Connection Pool Resource Tracking

**Current Issue:** No metrics on pool utilization, can't detect saturation.

**Risk Level:** MEDIUM - Operational blindness
**Time Estimate:** 2 hours
**Files to Modify:** `src/erlmcp_transport_http_server.erl`

#### Implementation:

**Add Pool Stats Record:**

```erlang
-record(pool_stats, {
    total_connections :: non_neg_integer(),
    active_connections :: non_neg_integer(),
    idle_connections :: non_neg_integer(),
    pending_requests :: non_neg_integer(),
    total_requests :: non_neg_integer(),
    total_errors :: non_neg_integer(),
    avg_latency_ms :: float(),
    p95_latency_ms :: float(),
    p99_latency_ms :: float()
}).
```

**Add Metrics Export:**

```erlang
%% In handle_info for gun_response:
handle_gun_response(StreamRef, StatusCode, Headers, Body,
                    #state{} = State) ->
    %% ... existing response handling ...

    %% NEW: Record metrics
    case process_response(StatusCode, Headers, Body) of
        {ok, Response} ->
            StartTime = maps:get(request_start_time, State, erlang:monotonic_time()),
            Latency = erlang:monotonic_time() - StartTime,
            LatencyMs = erlang:convert_time_unit(Latency, native, millisecond),

            telemetry:observe(
                [erlmcp, http, request, duration],
                LatencyMs,
                #{status_code => StatusCode}
            ),

            telemetry:counter_add(
                [erlmcp, http, requests, success],
                1
            ),

            %% ... rest of response handling ...
            {ok, Response};

        {error, Reason} ->
            telemetry:counter_add(
                [erlmcp, http, requests, error],
                1,
                #{reason => Reason}
            ),

            %% ... rest of error handling ...
            {error, Reason}
    end.
```

**Add Pool Stats Query Function:**

```erlang
%% Exported API for monitoring
pool_stats() ->
    case erlang:whereis(erlmcp_http_pool) of
        undefined -> {error, pool_not_running};
        PoolPid ->
            State = gen_server:call(PoolPid, get_pool_stats),
            {ok, State}
    end.
```

#### Benefits:
- ✅ Real-time pool visibility
- ✅ Detect saturation early
- ✅ Performance metrics baseline
- ✅ Integration with OTEL telemetry

---

## HIGH PRIORITY - Weeks 2-3

### FIX #5: Middleware Chain Framework

**Current Status:** No middleware support
**Impact:** Enable request pre/post processing
**Time Estimate:** 4 hours
**Files to Create:** `src/erlmcp_http_middleware.erl` (new), update router

#### Create Middleware Framework:

```erlang
% src/erlmcp_http_middleware.erl
-module(erlmcp_http_middleware).

-export([
    execute_chain/2,
    add_middleware/2,
    remove_middleware/1
]).

-type middleware() :: {module(), map()}.
-type chain() :: [middleware()].

%% Middleware execution order (evaluated left-to-right)
default_chain() ->
    [
        {erlmcp_middleware_logging, #{}},
        {erlmcp_middleware_compression, #{}},
        {erlmcp_middleware_cors, #{}},
        {erlmcp_middleware_rate_limit, #{}},
        {erlmcp_middleware_auth, #{}},
        {erlmcp_middleware_metrics, #{}}
    ].

-spec execute_chain(chain(), cowboy_req:req()) ->
    {ok, cowboy_req:req()} | {error, integer(), binary()}.
execute_chain([], Req) ->
    {ok, Req};
execute_chain([{Middleware, Opts} | Rest], Req) ->
    case Middleware:execute(Req, Opts) of
        {ok, Req2} ->
            execute_chain(Rest, Req2);
        {stop, Status, Req2} ->
            {error, Status, Req2};
        {error, Status, Message} ->
            {error, Status, cowboy_req:reply(Status, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{<<"error">> => Message}), Req)}
    end.
```

**Create Individual Middleware Modules:**

```erlang
% src/erlmcp_middleware_compression.erl
-module(erlmcp_middleware_compression).
-export([execute/2]).

execute(Req0, _Opts) ->
    %% Check Accept-Encoding header
    AcceptEncoding = cowboy_req:header(<<"accept-encoding">>, Req0, <<"identity">>),

    Req1 = case binary:match(AcceptEncoding, <<"gzip">>) of
        nomatch -> Req0;
        _ ->
            %% Mark for gzip compression
            cowboy_req:set_resp_header(<<"content-encoding">>, <<"gzip">>, Req0)
    end,

    {ok, Req1}.
```

#### Benefits:
- ✅ Cleaner request handling
- ✅ Modular architecture
- ✅ Easy to enable/disable features
- ✅ Standard pattern for web frameworks

---

### FIX #6: Compression Support

**Current Status:** No compression
**Impact:** 50-80% response size reduction
**Time Estimate:** 3 hours
**Files to Create:** `src/erlmcp_compress.erl` (new)

#### Implementation:

```erlang
% src/erlmcp_compress.erl
-module(erlmcp_compress).

-export([maybe_compress_body/2]).

-spec maybe_compress_body(binary(), cowboy_req:req()) ->
    {ok, binary(), cowboy_req:req()}.
maybe_compress_body(Body, Req0) ->
    AcceptEncoding = cowboy_req:header(<<"accept-encoding">>, Req0, <<"identity">>),

    case select_encoding(AcceptEncoding) of
        gzip ->
            Compressed = zlib:gzip(Body),
            Size = byte_size(Body),
            CompressedSize = byte_size(Compressed),
            Ratio = CompressedSize / Size,

            %% Log compression metrics
            logger:debug("GZIP compression: ~pB -> ~pB (~.1f%)",
                        [Size, CompressedSize, Ratio * 100]),

            Req1 = cowboy_req:set_resp_header(
                <<"content-encoding">>, <<"gzip">>, Req0
            ),
            {ok, Compressed, Req1};

        deflate ->
            Compressed = zlib:compress(Body),
            Req1 = cowboy_req:set_resp_header(
                <<"content-encoding">>, <<"deflate">>, Req0
            ),
            {ok, Compressed, Req1};

        identity ->
            {ok, Body, Req0}
    end.

-spec select_encoding(binary()) -> gzip | deflate | identity.
select_encoding(Encoding) ->
    case Encoding of
        _ when byte_size(Encoding) > 100 -> identity;  % Malformed
        E when binary:match(E, <<"gzip">>) =/= nomatch -> gzip;
        E when binary:match(E, <<"deflate">>) =/= nomatch -> deflate;
        _ -> identity
    end.
```

**Update Cowboy Handler:**

```erlang
% In HTTP handler where response is sent:
{ok, Body} = get_response_body(...),

case erlmcp_compress:maybe_compress_body(Body, Req) of
    {ok, CompressedBody, Req2} ->
        Req3 = cowboy_req:set_resp_header(
            <<"content-length">>,
            integer_to_binary(byte_size(CompressedBody)),
            Req2
        ),
        cowboy_req:reply(200, #{}, CompressedBody, Req3)
end.
```

#### Benefits:
- ✅ Reduce bandwidth 50-80%
- ✅ Faster transmission
- ✅ Standard gzip/deflate support
- ✅ Configurable per handler

---

### FIX #7: Complete Security Headers

**Current Status:** Partial (HSTS present, others missing)
**Impact:** XSS/Clickjacking protection
**Time Estimate:** 1 hour
**Files to Modify:** `src/erlmcp_http_security.erl`, all HTTP handlers

#### Create Security Header Module:

```erlang
% src/erlmcp_http_security_headers.erl
-module(erlmcp_http_security_headers).

-export([get_security_headers/0, apply_to_response/1]).

-spec get_security_headers() -> #{binary() => binary()}.
get_security_headers() ->
    #{
        %% HSTS (HTTP Strict-Transport-Security)
        <<"strict-transport-security">> =>
            <<"max-age=31536000; includeSubDomains">>,

        %% XSS Protection
        <<"x-xss-protection">> => <<"1; mode=block">>,

        %% Clickjacking protection
        <<"x-frame-options">> => <<"DENY">>,

        %% MIME sniffing protection
        <<"x-content-type-options">> => <<"nosniff">>,

        %% Referrer Policy
        <<"referrer-policy">> => <<"strict-origin-when-cross-origin">>,

        %% Permissions Policy (formerly Feature-Policy)
        <<"permissions-policy">> =>
            <<"geolocation=(), microphone=(), camera=(), payment=()">>,

        %% Additional headers
        <<"x-permitted-cross-domain-policies">> => <<"none">>
    }.

-spec apply_to_response(cowboy_req:req()) -> cowboy_req:req().
apply_to_response(Req) ->
    Headers = get_security_headers(),
    maps:fold(
        fun(Key, Value, ReqAcc) ->
            cowboy_req:set_resp_header(Key, Value, ReqAcc)
        end,
        Req,
        Headers
    ).
```

**Apply in All Handlers:**

```erlang
% In each HTTP handler (SSE, WebSocket init, etc.):
handle(Req0, State) ->
    Req1 = erlmcp_http_security_headers:apply_to_response(Req0),
    %% ... rest of handling with Req1 ...
```

#### Benefits:
- ✅ Protection against XSS
- ✅ Clickjacking defense
- ✅ Standards compliance
- ✅ Zero performance overhead

---

### FIX #8: Enhanced Gun Configuration

**Current Status:** Basic configuration, missing modern features
**Time Estimate:** 2 hours
**Files to Modify:** `src/erlmcp_transport_http_server.erl:398-416`

#### Update Gun Options:

```erlang
-spec build_gun_opts(state()) -> map().
build_gun_opts(State) ->
    BaseOpts = #{
        protocols => [http2, http],
        transport => get_transport(State),
        tls_opts => get_tls_options(State),

        %% Timeouts
        connect_timeout => State#state.connect_timeout,

        %% HTTP/1.1 options
        http_opts => #{
            keepalive => State#state.timeout,
            flow => 65535,  % Window size for flow control
            transform_header_name => fun erlmcp_http:normalize_header/1
        },

        %% HTTP/2 options (RFC 7540)
        http2_opts => #{
            keepalive => State#state.timeout,
            max_concurrent_streams => 100,
            initial_window_size => 65535,
            max_frame_size => 16384,
            %% RFC 7540 8.1.2.2 - Pseudo headers
            pseudo_headers => true
        },

        %% Retry and resilience
        retry => State#state.max_retries,
        retry_timeout => State#state.retry_delay,

        %% Connection pooling
        pool => erlmcp_http_pool,
        pool_size => State#state.pool_size,

        %% Telemetry/Metrics
        event_handler => {erlmcp_gun_telemetry, #{}}
    },

    %% Add SSL options if HTTPS
    case State#state.scheme of
        https ->
            BaseOpts#{
                tls_opts => [
                    {verify, verify_peer},
                    {cacertfile, State#state.ssl_options},
                    {versions, ['tlsv1.2', 'tlsv1.3']},
                    {ciphers, get_strong_ciphers()}
                ]
            };
        http ->
            BaseOpts
    end.

-spec get_strong_ciphers() -> [string()].
get_strong_ciphers() ->
    [
        "ECDHE-RSA-AES256-GCM-SHA384",
        "ECDHE-RSA-AES128-GCM-SHA256",
        "ECDHE-RSA-CHACHA20-POLY1305",
        "DHE-RSA-AES256-GCM-SHA384",
        "DHE-RSA-AES128-GCM-SHA256"
    ].
```

#### Benefits:
- ✅ Better HTTP/2 utilization
- ✅ Modern TLS configuration
- ✅ Telemetry integration
- ✅ Flow control awareness

---

## MEDIUM PRIORITY - Weeks 4-5

### FIX #9: Performance Testing Suite

**Current Status:** No load/performance tests
**Impact:** Catch regressions early
**Time Estimate:** 6 hours
**Files to Create:** `test/erlmcp_http_performance_SUITE.erl` (new)

#### Test Suite Structure:

```erlang
% test/erlmcp_http_performance_SUITE.erl
-module(erlmcp_http_performance_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([
    test_concurrent_ws_connections/1,
    test_sse_event_throughput/1,
    test_http_request_latency/1,
    test_backpressure_handling/1,
    test_memory_under_load/1
]).

suite() ->
    [{require, all_apps},
     {require, tcp_server}].

groups() ->
    [
        {connection_tests, [parallel],
         [test_concurrent_ws_connections]},
        {throughput_tests, [],
         [test_sse_event_throughput]},
        {latency_tests, [parallel],
         [test_http_request_latency]},
        {stress_tests, [],
         [test_backpressure_handling, test_memory_under_load]}
    ].

all() ->
    [
        {group, connection_tests},
        {group, throughput_tests},
        {group, latency_tests},
        {group, stress_tests}
    ].

%% Tests:
test_concurrent_ws_connections(Config) ->
    %% Start 1000 concurrent WebSocket connections
    %% Measure:
    %% - Connection setup time
    %% - Memory per connection
    %% - Latency under load
    ok.

test_sse_event_throughput(Config) ->
    %% Send 10,000 events/sec to multiple SSE connections
    %% Measure:
    %% - Event delivery latency (P50, P95, P99)
    %% - CPU utilization
    %% - Memory growth
    ok.

test_http_request_latency(Config) ->
    %% Send sustained HTTP requests at 1000 req/sec
    %% Measure:
    %% - Request latency distribution
    %% - Error rate
    %% - Connection pool utilization
    ok.

test_backpressure_handling(Config) ->
    %% Slow WebSocket client
    %% Measure:
    %% - Server response to backpressure
    %% - Queue buildup
    %% - Memory stabilization
    ok.

test_memory_under_load(Config) ->
    %% 100 concurrent connections, 5 minute duration
    %% Check for memory leaks via garbage collection
    ok.
```

#### Benefits:
- ✅ Regression detection
- ✅ Performance baseline
- ✅ Capacity planning data
- ✅ Production readiness validation

---

### FIX #10: Event Store Persistence

**Current Status:** In-memory ETS only
**Impact:** SSE resumption across restarts
**Time Estimate:** 6 hours
**Files to Create:** `src/erlmcp_sse_event_log.erl` (new)

#### Implementation Approach:

```erlang
% src/erlmcp_sse_event_log.erl
-module(erlmcp_sse_event_log).

-export([
    append/3,
    read_since/2,
    cleanup_old_events/2
]).

-spec append(binary(), binary(), binary()) -> ok | {error, term()}.
append(SessionId, EventId, EventData) ->
    %% Option 1: RocksDB (production)
    case application:get_env(erlmcp, sse_persistence, undefined) of
        {rocksdb, DbPath} ->
            append_rocksdb(DbPath, SessionId, EventId, EventData);

        {sqlite, DbPath} ->
            append_sqlite(DbPath, SessionId, EventId, EventData);

        {append_log, LogPath} ->
            append_log(LogPath, SessionId, EventId, EventData);

        _ ->
            %% Disabled, only use ETS
            ok
    end.

%% Choose one backend:

append_rocksdb(DbPath, SessionId, EventId, EventData) ->
    Timestamp = erlang:system_time(millisecond),
    Entry = jsx:encode(#{
        session_id => SessionId,
        event_id => EventId,
        data => EventData,
        timestamp => Timestamp
    }),
    rocksdb:put(erlmcp_sse_db, EventId, Entry, []).

append_sqlite(DbPath, SessionId, EventId, EventData) ->
    Timestamp = erlang:system_time(millisecond),
    Query = "INSERT INTO sse_events (session_id, event_id, data, timestamp) "
            "VALUES (?, ?, ?, ?)",
    SqliteModule:execute(erlmcp_sse_db, Query, [SessionId, EventId, EventData, Timestamp]).

append_log(LogPath, SessionId, EventId, EventData) ->
    Entry = erlang:term_to_binary(#{
        session_id => SessionId,
        event_id => EventId,
        data => EventData,
        timestamp => erlang:system_time(millisecond)
    }),
    {ok, File} = file:open(LogPath, [append]),
    file:write(File, <<(byte_size(Entry)):32, Entry/binary>>),
    file:close(File).

-spec cleanup_old_events(SessionId, MaxAge) -> ok.
cleanup_old_events(SessionId, MaxAgeMs) ->
    %% Remove events older than MaxAgeMs
    ok.
```

#### Configuration:

```erlang
{sse, [
    {persistence_enabled, true},
    {persistence_backend, rocksdb},  % or sqlite, append_log
    {persistence_path, "priv/sse_events"},
    {event_ttl_ms, 3600000},  % Keep events for 1 hour
    {cleanup_interval_ms, 300000}  % Cleanup every 5 minutes
]}
```

#### Benefits:
- ✅ SSE resumption across restarts
- ✅ Event history audit trail
- ✅ Configurable backends
- ✅ TTL-based cleanup

---

### FIX #11: HTTP/2 Server Push (Optional)

**Current Status:** Not implemented
**Impact:** Reduce round trips for static resources
**Time Estimate:** 4 hours
**Priority:** LOW - Marginal benefit for most use cases

#### Implementation:

```erlang
% In HTTP handler:
handle_http2_request(Req0, State) ->
    %% Check if client supports server push
    case cowboy_req:header(<<"h2-settings">>, Req0) of
        undefined ->
            %% HTTP/1.1 or HTTP/2 without push support
            {ok, Req0, State};
        _ ->
            %% HTTP/2 with push capable
            Req1 = cowboy_req:push(
                <<"/.well-known/mcp/schema.json">>,
                #{
                    <<"content-type">> => <<"application/json">>,
                    <<"cache-control">> => <<"public, max-age=3600">>
                },
                Req0
            ),
            {ok, Req1, State}
    end.
```

---

## Testing & Validation

### Integration Test Plan

```erlang
% test/erlmcp_cowboy_integration_test.erl
-module(erlmcp_cowboy_integration_test).

integration_tests() ->
    [
        %% Critical fixes
        test_backpressure_rejection,
        test_listener_timeout_enforcement,
        test_queue_overflow_handling,
        test_pool_limit_tracking,

        %% High priority
        test_middleware_chain_order,
        test_gzip_compression,
        test_security_headers_present,
        test_gun_configuration,

        %% Medium priority
        test_performance_benchmarks,
        test_event_store_persistence
    ].
```

### Rollout Strategy

#### Phase 1 (Week 1-2): Critical Fixes
1. Deploy backpressure handling
2. Deploy timeout configuration
3. Deploy queue limits
4. Validate with load tests

#### Phase 2 (Week 3-4): High Priority
1. Deploy middleware chain
2. Deploy compression
3. Deploy security headers
4. Full integration testing

#### Phase 3 (Week 5-6): Medium Priority
1. Deploy performance testing
2. Deploy event store persistence
3. Optimize based on metrics
4. Production hardening

---

## Success Metrics

### Targets After Implementation

| Metric | Current | Target | Impact |
|--------|---------|--------|--------|
| **Backpressure handling** | None | 100 frame queue limit | Prevents OOM |
| **WebSocket conn timeout** | None | 30s request timeout | Prevents hanging |
| **HTTP queue size** | Unbounded | 10K max | Memory stability |
| **Response compression** | 0% | 60% avg | Bandwidth savings |
| **Security header coverage** | 33% | 100% | Security hardening |
| **Performance test coverage** | 0% | >80% | Regression detection |
| **Memory per connection** | TBD | <1MB | Capacity planning |

---

## Monitoring & Alerting Setup

```erlang
% Setup Prometheus metrics
prometheus:register_counter(
    erlmcp_ws_backpressure_rejections,
    "Count of WebSocket frames rejected due to backpressure"
),

prometheus:register_gauge(
    erlmcp_http_queue_size,
    "Current HTTP request queue size"
),

prometheus:register_histogram(
    erlmcp_http_response_time,
    "HTTP response time in milliseconds",
    [{buckets, [10, 50, 100, 500, 1000, 5000]}]
),

prometheus:register_gauge(
    erlmcp_connection_pool_active,
    "Number of active connections in pool"
).
```

---

## Maintenance & Future Work

### After Initial Implementation

1. **Monitor metrics** - Track performance over time
2. **User feedback** - Gather deployment experiences
3. **Optimization** - Tune configuration based on metrics
4. **Security updates** - Stay current with Cowboy/Gun releases
5. **Feature additions** - Add HTTP/2 server push, custom compression

### Quarterly Review Checklist

- [ ] Review error logs for new patterns
- [ ] Analyze performance metrics trends
- [ ] Check for security advisories
- [ ] Validate capacity planning assumptions
- [ ] Plan next optimization cycle

---

## Conclusion

These recommendations provide a **clear, phased approach** to hardening the erlmcp HTTP transport layer. The prioritization balances **critical stability fixes** with **performance optimizations**, enabling confident production deployment.

**Expected Outcome:**
- ✅ Stable under load (1000+ concurrent connections)
- ✅ Predictable resource usage
- ✅ Production-grade observability
- ✅ Industry-standard security
- ✅ 4-5 stars architectural quality

---

**Document:** Cowboy HTTP Transport Recommendations
**Version:** 1.0
**Last Updated:** January 27, 2026
**Maintained By:** ErlMCP Core Team
**Review Cycle:** Quarterly
