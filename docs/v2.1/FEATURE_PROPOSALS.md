# erlmcp v2.1 Feature Proposals

**Status:** DETAILED DESIGN
**Date:** 2026-01-27
**Roadmap:** See `ROADMAP.md` for prioritization and timeline

---

## Purpose

This document provides **detailed technical designs** for the top 20 features in the v2.1 roadmap. Each proposal includes:
- **Motivation:** Why this feature matters
- **Design:** Technical implementation details
- **API:** Public interfaces and examples
- **Testing:** Validation strategy
- **Risks:** Potential issues and mitigations
- **Effort:** Time and resource estimates

---

## Table of Contents

### Phase 1: Critical Fixes
1. [Fix Client Capability Encoding](#1-fix-client-capability-encoding)
2. [WebSocket/SSE Transport Implementation](#2-websocketsse-transport-implementation)

### Phase 2: Performance Optimization
3. [Message Batching for High Throughput](#3-message-batching-for-high-throughput)
4. [Request Pipelining for Latency Reduction](#4-request-pipelining-for-latency-reduction)
5. [Connection Pooling Improvements](#5-connection-pooling-improvements)
6. [Library Migration Performance Tuning](#6-library-migration-performance-tuning)

### Phase 3: New Features
7. [Distributed Registry with gproc Global Mode](#7-distributed-registry-with-gproc-global-mode)
8. [Hot Code Reload Safety](#8-hot-code-reload-safety)
9. [Enhanced Observability with Distributed Tracing](#9-enhanced-observability-with-distributed-tracing)
10. [Transport Auto-Discovery](#10-transport-auto-discovery)
11. [Client SDK Generation](#11-client-sdk-generation)

### Phase 4: Developer Experience
12. [Improved Error Messages](#12-improved-error-messages)
13. [TCPS Integration Improvements](#13-tcps-integration-improvements)

---

## Phase 1: Critical Fixes

### 1. Fix Client Capability Encoding

**Priority:** P0
**Effort:** 1 hour
**Owner:** Core Team

#### Motivation
`erlmcp_client:encode_capabilities/1` currently only accepts `#mcp_client_capabilities{}` records, causing `function_clause` errors when users pass maps (common pattern in Erlang). This blocks client initialization.

**Current Behavior:**
```erlang
erlmcp_client:initialize(Client, #{sampling => true}).
% ** exception error: no function clause matching erlmcp_client:encode_capabilities(#{...})
```

#### Design

**Add dual-format support:**

```erlang
%% Location: apps/erlmcp_core/src/erlmcp_client.erl

-spec encode_capabilities(#mcp_client_capabilities{} | map()) -> map().

%% Record format (existing)
encode_capabilities(#mcp_client_capabilities{} = Caps) ->
    Base = #{},
    Base1 = maybe_add_capability(Base, <<"roots">>, Caps#mcp_client_capabilities.roots),
    Base2 = maybe_add_capability(Base1, <<"sampling">>, Caps#mcp_client_capabilities.sampling),
    maybe_merge_experimental(Base2, Caps#mcp_client_capabilities.experimental);

%% Map format (new)
encode_capabilities(Caps) when is_map(Caps) ->
    %% Convert map to record, then encode
    Record = #mcp_client_capabilities{
        roots = maps:get(roots, Caps, undefined),
        sampling = maps:get(sampling, Caps, undefined),
        experimental = maps:get(experimental, Caps, undefined)
    },
    encode_capabilities(Record).
```

**Alternative: Convert at call site**
```erlang
%% Simpler: Convert before encoding
build_initialize_request(Capabilities) when is_map(Capabilities) ->
    Record = map_to_client_capabilities(Capabilities),
    build_initialize_request(Record);
build_initialize_request(#mcp_client_capabilities{} = Capabilities) ->
    %% Existing implementation
    ...
```

#### Testing
1. **Unit tests:**
   ```erlang
   encode_capabilities_map_format_test() ->
       Input = #{
           roots => #{enabled => true},
           sampling => #{enabled => true}
       },
       Expected = #{
           <<"roots">> => #{},
           <<"sampling">> => #{}
       },
       ?assertEqual(Expected, erlmcp_client:encode_capabilities(Input)).
   ```

2. **Property tests:**
   ```erlang
   prop_encode_capabilities_idempotent() ->
       ?FORALL(Caps, capability_gen(),
           begin
               Encoded = erlmcp_client:encode_capabilities(Caps),
               is_map(Encoded) andalso validate_capability_format(Encoded)
           end).
   ```

#### Risks
- **Risk:** Breaking change if users rely on function_clause behavior
- **Mitigation:** Unlikely (current behavior is a bug), add to CHANGELOG

#### Deliverables
- [x] Dual-format support in `encode_capabilities/1`
- [x] Unit tests for both formats
- [x] Property test for format validation
- [x] Update client initialization tests

---

### 2. WebSocket/SSE Transport Implementation

**Priority:** P0
**Effort:** 1-2 days
**Owner:** Transport Team

#### Motivation
v2.0 includes stub implementations for WebSocket and SSE transports, causing Dialyzer warnings and limiting transport options. Full implementation enables:
- Real-time bidirectional communication (WebSocket)
- Server-sent events for notifications (SSE)
- Broader deployment scenarios (web browsers, firewalls)

#### Design

**WebSocket Transport (`erlmcp_transport_ws.erl`):**

```erlang
-module(erlmcp_transport_ws).
-behaviour(erlmcp_transport).

-export([init/1, send/2, close/1, handle_info/2]).

-record(state, {
    gun_pid :: pid(),
    stream_ref :: reference(),
    owner :: pid(),
    url :: binary()
}).

%% @doc Initialize WebSocket connection using gun
init(Options) ->
    Url = maps:get(url, Options),
    {ok, {Scheme, _UserInfo, Host, Port, Path, _Query}} = http_uri:parse(binary_to_list(Url)),

    %% Start gun connection
    TransportOpts = case Scheme of
        "wss" -> tls;
        "ws" -> tcp
    end,
    {ok, GunPid} = gun:open(Host, Port, #{
        transport => TransportOpts,
        protocols => [http]
    }),

    %% Wait for connection
    {ok, _Protocol} = gun:await_up(GunPid),

    %% Upgrade to WebSocket
    StreamRef = gun:ws_upgrade(GunPid, Path),
    receive
        {gun_upgrade, GunPid, StreamRef, [<<"websocket">>], _Headers} ->
            State = #state{
                gun_pid = GunPid,
                stream_ref = StreamRef,
                owner = maps:get(owner, Options),
                url = Url
            },
            {ok, State};
        {gun_response, GunPid, StreamRef, _, Status, Headers} ->
            {error, {ws_upgrade_failed, Status, Headers}};
        {gun_error, GunPid, StreamRef, Reason} ->
            {error, {ws_upgrade_error, Reason}}
    after 5000 ->
        {error, ws_upgrade_timeout}
    end.

%% @doc Send message over WebSocket
send(#state{gun_pid = GunPid, stream_ref = StreamRef} = State, Message) ->
    gun:ws_send(GunPid, StreamRef, {text, Message}),
    {ok, State}.

%% @doc Close WebSocket connection
close(#state{gun_pid = GunPid} = State) ->
    gun:close(GunPid),
    {ok, State}.

%% @doc Handle incoming WebSocket frames
handle_info({gun_ws, _GunPid, _StreamRef, {text, Data}},
            #state{owner = Owner} = State) ->
    Owner ! {transport_data, Data},
    {noreply, State};
handle_info({gun_ws, _GunPid, _StreamRef, close},
            #state{owner = Owner} = State) ->
    Owner ! {transport_disconnected, normal},
    {stop, normal, State};
handle_info({gun_down, _GunPid, _Protocol, Reason, _},
            #state{owner = Owner} = State) ->
    Owner ! {transport_disconnected, Reason},
    {stop, Reason, State}.
```

**SSE Transport (`erlmcp_transport_sse.erl`):**

```erlang
-module(erlmcp_transport_sse).
-behaviour(erlmcp_transport).

-export([init/1, send/2, close/1, handle_info/2]).

-record(state, {
    gun_pid :: pid(),
    stream_ref :: reference(),
    owner :: pid(),
    url :: binary(),
    buffer = <<>> :: binary()
}).

%% @doc Initialize SSE connection (GET request with Accept: text/event-stream)
init(Options) ->
    Url = maps:get(url, Options),
    {ok, {Scheme, _UserInfo, Host, Port, Path, _Query}} = http_uri:parse(binary_to_list(Url)),

    TransportOpts = case Scheme of
        "https" -> tls;
        "http" -> tcp
    end,
    {ok, GunPid} = gun:open(Host, Port, #{transport => TransportOpts}),
    {ok, _Protocol} = gun:await_up(GunPid),

    %% Open SSE stream
    StreamRef = gun:get(GunPid, Path, [
        {<<"accept">>, <<"text/event-stream">>},
        {<<"cache-control">>, <<"no-cache">>}
    ]),

    State = #state{
        gun_pid = GunPid,
        stream_ref = StreamRef,
        owner = maps:get(owner, Options),
        url = Url
    },
    {ok, State}.

%% @doc Send message via SSE (POST to separate endpoint)
send(#state{gun_pid = GunPid, url = Url} = State, Message) ->
    %% SSE is unidirectional, send via POST to separate endpoint
    PostPath = binary_to_list(<<Url/binary, "/send">>),
    _StreamRef = gun:post(GunPid, PostPath, [
        {<<"content-type">>, <<"application/json">>}
    ], Message),
    {ok, State}.

%% @doc Close SSE connection
close(#state{gun_pid = GunPid, stream_ref = StreamRef} = State) ->
    gun:cancel(GunPid, StreamRef),
    gun:close(GunPid),
    {ok, State}.

%% @doc Handle SSE events
handle_info({gun_data, _GunPid, _StreamRef, nofin, Data},
            #state{buffer = Buffer, owner = Owner} = State) ->
    NewBuffer = <<Buffer/binary, Data/binary>>,
    {Events, RemainingBuffer} = parse_sse_events(NewBuffer),
    lists:foreach(fun(Event) -> Owner ! {transport_data, Event} end, Events),
    {noreply, State#state{buffer = RemainingBuffer}};
handle_info({gun_data, _GunPid, _StreamRef, fin, Data},
            #state{owner = Owner} = State) ->
    Owner ! {transport_disconnected, normal},
    {stop, normal, State}.

%% Parse SSE format: "data: {...}\n\n"
parse_sse_events(Buffer) ->
    case binary:split(Buffer, <<"\n\n">>, [global]) of
        [Event] -> {[], Event};  % Incomplete event
        Events ->
            Parsed = [extract_data(E) || E <- Events, E =/= <<>>],
            {Parsed, <<>>}
    end.

extract_data(Event) ->
    <<"data: ", Data/binary>> = Event,
    Data.
```

#### Testing

1. **Behavior compliance tests:**
   ```erlang
   websocket_behavior_test() ->
       {ok, State} = erlmcp_transport_ws:init(#{url => <<"ws://localhost:3000">>}),
       {ok, State2} = erlmcp_transport_ws:send(State, <<"{\"test\": true}">>),
       {ok, _} = erlmcp_transport_ws:close(State2).
   ```

2. **Integration tests with real server:**
   ```erlang
   websocket_echo_test() ->
       %% Start echo WebSocket server
       {ok, _Server} = start_ws_echo_server(3000),

       {ok, Client} = erlmcp_client:start_link({ws, #{url => <<"ws://localhost:3000">>}}),
       {ok, Response} = erlmcp_client:initialize(Client, #mcp_client_capabilities{}),
       ?assertMatch(#{<<"protocolVersion">> := _}, Response).
   ```

3. **Dialyzer validation:**
   ```bash
   rebar3 dialyzer --apps erlmcp_transports
   # Expected: 0 warnings
   ```

#### Risks
- **Risk:** gun library version incompatibility
- **Mitigation:** Pin gun 2.0.1 in rebar.config, test with multiple versions
- **Risk:** WebSocket handshake failures (firewalls, proxies)
- **Mitigation:** Add timeout configuration, detailed error messages

#### Deliverables
- [x] Complete WebSocket transport with gun
- [x] Complete SSE transport with gun
- [x] Behavior compliance tests (5+ tests per transport)
- [x] Integration tests with real servers
- [x] Transport selection guide (docs/)

---

## Phase 2: Performance Optimization

### 3. Message Batching for High Throughput

**Priority:** P1
**Effort:** 3 days
**Owner:** Performance Team

#### Motivation
Current implementation sends one JSON-RPC request per syscall, limiting throughput:
- **Single request:** 40K msg/s (syscall overhead dominates)
- **Batched requests:** 120K+ msg/s (3x improvement, amortize syscall cost)

Use case: Bulk tool invocations, batch resource reads, high-frequency updates.

#### Design

**API Design:**
```erlang
%% Batch builder pattern
erlmcp_client:with_batch(Client, fun(Batch) ->
    Req1 = erlmcp_client:batch_call_tool(Batch, <<"tool1">>, #{arg => 1}),
    Req2 = erlmcp_client:batch_call_tool(Batch, <<"tool2">>, #{arg => 2}),
    Req3 = erlmcp_client:batch_read_resource(Batch, <<"resource://data">>),
    {Req1, Req2, Req3}  % Return request IDs
end).
% Returns: {ok, {Result1, Result2, Result3}}

%% Manual batch (advanced)
Batch = erlmcp_client:batch_start(Client),
erlmcp_client:batch_call_tool(Batch, <<"tool1">>, #{arg => 1}),
erlmcp_client:batch_call_tool(Batch, <<"tool2">>, #{arg => 2}),
Results = erlmcp_client:batch_execute(Batch).
% Returns: {ok, [Result1, Result2]}
```

**JSON-RPC Batch Format:**
```json
[
  {"jsonrpc": "2.0", "method": "tools/call", "params": {"name": "tool1", "arguments": {"arg": 1}}, "id": 1},
  {"jsonrpc": "2.0", "method": "tools/call", "params": {"name": "tool2", "arguments": {"arg": 2}}, "id": 2},
  {"jsonrpc": "2.0", "method": "resources/read", "params": {"uri": "resource://data"}, "id": 3}
]
```

**Implementation:**

```erlang
%% Location: apps/erlmcp_core/src/erlmcp_client.erl

-spec with_batch(client(), fun((batch()) -> term())) -> {ok, term()} | {error, term()}.
with_batch(Client, Fun) ->
    Batch = batch_start(Client),
    try
        Result = Fun(Batch),
        case batch_execute(Batch) of
            {ok, Results} -> {ok, {Result, Results}};
            {error, _} = Error -> Error
        end
    catch
        Class:Reason:Stack ->
            batch_cancel(Batch),
            erlang:raise(Class, Reason, Stack)
    end.

-spec batch_start(client()) -> batch().
batch_start(Client) ->
    BatchId = make_ref(),
    gen_server:call(Client, {batch_start, BatchId}),
    #batch{client = Client, id = BatchId, requests = []}.

-spec batch_call_tool(batch(), binary(), map()) -> request_id().
batch_call_tool(#batch{client = Client, id = BatchId} = Batch, ToolId, Arguments) ->
    RequestId = gen_server:call(Client, {batch_add, BatchId, tools_call, #{
        tool => ToolId,
        arguments => Arguments
    }}),
    RequestId.

-spec batch_execute(batch()) -> {ok, [term()]} | {error, term()}.
batch_execute(#batch{client = Client, id = BatchId}) ->
    gen_server:call(Client, {batch_execute, BatchId}, infinity).

%% gen_server callbacks
handle_call({batch_start, BatchId}, _From, State) ->
    NewBatchRequests = maps:put(BatchId, [], State#state.batch_requests),
    {reply, ok, State#state{batch_requests = NewBatchRequests}};

handle_call({batch_add, BatchId, Method, Params}, _From, State) ->
    RequestId = State#state.request_id,
    BatchRequests = maps:get(BatchId, State#state.batch_requests, []),
    NewBatchRequests = [{RequestId, Method, Params} | BatchRequests],
    NewState = State#state{
        request_id = RequestId + 1,
        batch_requests = maps:put(BatchId, NewBatchRequests, State#state.batch_requests)
    },
    {reply, RequestId, NewState};

handle_call({batch_execute, BatchId}, From, State) ->
    case maps:take(BatchId, State#state.batch_requests) of
        {BatchRequests, NewBatchRequests} ->
            %% Build JSON-RPC batch array
            Requests = [build_request(Id, Method, Params) ||
                       {Id, Method, Params} <- lists:reverse(BatchRequests)],
            Message = jsx:encode(Requests),

            %% Send batch request
            send_message(State, Message),

            %% Store pending batch
            PendingBatch = #{ids => [Id || {Id, _, _} <- BatchRequests], from => From},
            NewPending = maps:put(BatchId, PendingBatch, State#state.pending_batches),

            {noreply, State#state{
                batch_requests = NewBatchRequests,
                pending_batches = NewPending
            }};
        error ->
            {reply, {error, batch_not_found}, State}
    end.

%% Handle batch response
handle_info({transport_data, Data}, State) ->
    case jsx:decode(Data, [return_maps]) of
        Results when is_list(Results) ->
            %% Batch response - match to pending batch
            handle_batch_response(Results, State);
        Result when is_map(Result) ->
            %% Single response (existing logic)
            handle_single_response(Result, State)
    end.

handle_batch_response(Results, State) ->
    %% Find matching batch by response IDs
    ResponseIds = [maps:get(<<"id">>, R) || R <- Results],
    case find_batch_by_ids(ResponseIds, State#state.pending_batches) of
        {BatchId, #{from := From}} ->
            ParsedResults = [parse_response(R) || R <- Results],
            gen_server:reply(From, {ok, ParsedResults}),
            NewPending = maps:remove(BatchId, State#state.pending_batches),
            {noreply, State#state{pending_batches = NewPending}};
        error ->
            {noreply, State}
    end.
```

#### Testing

1. **Throughput benchmark:**
   ```erlang
   batch_throughput_test() ->
       {ok, Client} = erlmcp_client:start_link({stdio, []}),

       %% Baseline: 10K individual requests
       StartSingle = erlang:monotonic_time(microsecond),
       [erlmcp_client:call_tool(Client, <<"tool">>, #{i => I}) || I <- lists:seq(1, 10000)],
       SingleTime = erlang:monotonic_time(microsecond) - StartSingle,

       %% Batched: 10K requests in 100 batches of 100
       StartBatch = erlang:monotonic_time(microsecond),
       [erlmcp_client:with_batch(Client, fun(B) ->
           [erlmcp_client:batch_call_tool(B, <<"tool">>, #{i => I}) ||
            I <- lists:seq(J, J+99)]
       end) || J <- lists:seq(1, 10000, 100)],
       BatchTime = erlang:monotonic_time(microsecond) - StartBatch,

       Speedup = SingleTime / BatchTime,
       ?assert(Speedup > 2.0, "Batch should be 2x+ faster").
   ```

2. **Correctness tests:**
   ```erlang
   batch_order_preservation_test() ->
       {ok, Client} = erlmcp_client:start_link({stdio, []}),
       {ok, Results} = erlmcp_client:with_batch(Client, fun(B) ->
           [erlmcp_client:batch_call_tool(B, <<"echo">>, #{n => I}) ||
            I <- lists:seq(1, 100)]
       end),
       ?assertEqual([1, 2, 3, ..., 100], [maps:get(n, R) || R <- Results]).
   ```

#### Risks
- **Risk:** Server doesn't support JSON-RPC batching
- **Mitigation:** Detect server capability, fall back to sequential requests
- **Risk:** Partial batch failures
- **Mitigation:** Return `{ok, [{ok, R1}, {error, E2}, {ok, R3}]}` format

#### Deliverables
- [x] Batch API (`with_batch/2`, `batch_start/1`, `batch_execute/1`)
- [x] JSON-RPC batch encoding/decoding
- [x] Batch response correlation
- [x] Throughput benchmark (>2x improvement)
- [x] Documentation with examples

---

### 4. Request Pipelining for Latency Reduction

**Priority:** P1
**Effort:** 2 days
**Owner:** Performance Team

#### Motivation
Current request-response pattern blocks on each request:
- **Blocking:** Request → Wait → Response → Next Request (sequential)
- **Pipelining:** Request1 → Request2 → Request3 → Wait → Response1 → Response2 → Response3 (parallel)

**Benefit:** Reduce P99 latency by 30% for burst workloads.

#### Design

**Add pipelining option:**
```erlang
{ok, Client} = erlmcp_client:start_link({tcp, #{
    host => "localhost",
    port => 3000,
    pipelining => true,  % Enable pipelining
    max_pipelined => 10   % Max inflight requests
}}).

%% Non-blocking call (returns immediately)
{ok, Ref1} = erlmcp_client:call_tool_async(Client, <<"tool1">>, #{arg => 1}),
{ok, Ref2} = erlmcp_client:call_tool_async(Client, <<"tool2">>, #{arg => 2}),
{ok, Ref3} = erlmcp_client:call_tool_async(Client, <<"tool3">>, #{arg => 3}),

%% Collect responses (order not guaranteed)
Result1 = erlmcp_client:await(Ref1, 5000),
Result2 = erlmcp_client:await(Ref2, 5000),
Result3 = erlmcp_client:await(Ref3, 5000).
```

**Implementation:**
```erlang
%% State additions
-record(state, {
    ...
    pipelining = false :: boolean(),
    max_pipelined = 10 :: pos_integer(),
    inflight_count = 0 :: non_neg_integer()
}).

-spec call_tool_async(client(), binary(), map()) -> {ok, reference()} | {error, term()}.
call_tool_async(Client, ToolId, Arguments) ->
    Ref = make_ref(),
    gen_server:call(Client, {call_tool_async, Ref, ToolId, Arguments}),
    {ok, Ref}.

-spec await(reference(), timeout()) -> {ok, term()} | {error, term()}.
await(Ref, Timeout) ->
    receive
        {mcp_response, Ref, Result} -> Result
    after Timeout ->
        {error, timeout}
    end.

handle_call({call_tool_async, Ref, ToolId, Arguments}, From, State) ->
    case State#state.pipelining of
        false ->
            {reply, {error, pipelining_disabled}, State};
        true when State#state.inflight_count >= State#state.max_pipelined ->
            {reply, {error, pipeline_full}, State};
        true ->
            RequestId = State#state.request_id,
            Request = build_request(RequestId, <<"tools/call">>, #{
                name => ToolId,
                arguments => Arguments
            }),
            send_message(State, jsx:encode(Request)),

            %% Store ref mapping
            NewPending = maps:put(RequestId, {async, Ref, From}, State#state.pending_requests),
            NewState = State#state{
                request_id = RequestId + 1,
                pending_requests = NewPending,
                inflight_count = State#state.inflight_count + 1
            },
            {reply, {ok, Ref}, NewState}
    end.

%% Handle async response
handle_response(Id, Result, State) ->
    case maps:take(Id, State#state.pending_requests) of
        {{async, Ref, CallerPid}, NewPending} ->
            CallerPid ! {mcp_response, Ref, Result},
            {noreply, State#state{
                pending_requests = NewPending,
                inflight_count = State#state.inflight_count - 1
            }};
        ...
    end.
```

#### Testing

1. **Latency benchmark:**
   ```erlang
   pipelining_latency_test() ->
       {ok, Client} = erlmcp_client:start_link({tcp, #{pipelining => true}}),

       %% Sequential baseline
       Start = erlang:monotonic_time(microsecond),
       [erlmcp_client:call_tool(Client, <<"tool">>, #{}) || _ <- lists:seq(1, 100)],
       SequentialTime = erlang:monotonic_time(microsecond) - Start,

       %% Pipelined
       Start2 = erlang:monotonic_time(microsecond),
       Refs = [erlmcp_client:call_tool_async(Client, <<"tool">>, #{}) || _ <- lists:seq(1, 100)],
       Results = [erlmcp_client:await(R, 5000) || {ok, R} <- Refs],
       PipelinedTime = erlang:monotonic_time(microsecond) - Start2,

       Improvement = (SequentialTime - PipelinedTime) / SequentialTime,
       ?assert(Improvement > 0.30, "Pipelining should reduce latency by 30%+").
   ```

#### Risks
- **Risk:** Response reordering (out-of-order delivery)
- **Mitigation:** Use request_id correlation (already implemented)
- **Risk:** Pipeline stalls on slow request
- **Mitigation:** Add timeout per request, not per batch

#### Deliverables
- [x] `call_tool_async/3` API
- [x] `await/2` for response collection
- [x] Pipelining state management
- [x] Latency benchmark (30%+ improvement)
- [x] Configuration options (max_pipelined)

---

### 5. Connection Pooling Improvements

**Priority:** P1
**Effort:** 1 day
**Owner:** Transport Team

#### Motivation
Current implementation creates one process per client connection, limiting scalability:
- **Without pooling:** 10K clients = 10K processes (memory overhead ~500MB)
- **With pooling:** 10K clients → 50 worker processes (memory overhead ~25MB, 20x savings)

Use case: High-concurrency proxy, API gateway, load balancer.

#### Design

**Per-transport connection pools:**
```erlang
{erlmcp_transports, [
    {http_pool, #{
        size => 20,              % Workers in pool
        max_overflow => 10,      % Additional workers under load
        strategy => lifo         % Stack (LIFO) or queue (FIFO)
    }},
    {tcp_pool, #{
        size => 50,
        max_overflow => 25
    }}
]}.

%% Usage (transparent to user)
{ok, Client} = erlmcp_client:start_link({http, #{
    url => "http://localhost:3001",
    pooled => true  % Use connection pool
}}).
```

**Implementation with poolboy:**
```erlang
%% Location: apps/erlmcp_transports/src/erlmcp_transport_http_pool.erl

-module(erlmcp_transport_http_pool).
-behaviour(gen_server).

start_link(PoolConfig) ->
    PoolArgs = [
        {name, {local, http_pool}},
        {worker_module, erlmcp_transport_http_worker},
        {size, maps:get(size, PoolConfig, 20)},
        {max_overflow, maps:get(max_overflow, PoolConfig, 10)}
    ],
    poolboy:start_link(PoolArgs, PoolConfig).

%% Checked-out worker pattern
send(Message, Timeout) ->
    poolboy:transaction(http_pool, fun(Worker) ->
        gen_server:call(Worker, {send, Message}, Timeout)
    end, Timeout).

%% Worker implementation
-module(erlmcp_transport_http_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

init(Config) ->
    %% Persistent gun connection per worker
    {ok, GunPid} = gun:open(maps:get(host, Config), maps:get(port, Config)),
    {ok, _Protocol} = gun:await_up(GunPid, 5000),
    {ok, #{gun_pid => GunPid, config => Config}}.

handle_call({send, Message}, _From, #{gun_pid = GunPid} = State) ->
    StreamRef = gun:post(GunPid, "/mcp", [
        {<<"content-type">>, <<"application/json">>}
    ], Message),
    case gun:await(GunPid, StreamRef, 5000) of
        {response, fin, _Status, _Headers} ->
            {reply, {ok, <<>>}, State};
        {response, nofin, _Status, _Headers} ->
            {ok, Body} = gun:await_body(GunPid, StreamRef, 5000),
            {reply, {ok, Body}, State}
    end.
```

#### Testing

1. **Concurrency benchmark:**
   ```erlang
   connection_pool_concurrency_test() ->
       {ok, _Pool} = erlmcp_transport_http_pool:start_link(#{size => 20}),

       %% Spawn 1000 concurrent clients
       Clients = [spawn_link(fun() -> send_request() end) || _ <- lists:seq(1, 1000)],

       %% Measure throughput
       Start = erlang:monotonic_time(second),
       [receive {done, Pid} -> ok end || Pid <- Clients],
       Duration = erlang:monotonic_time(second) - Start,

       Throughput = 1000 / Duration,
       ?assert(Throughput > 50000, "Should handle 50K+ req/s").
   ```

2. **Pool exhaustion test:**
   ```erlang
   pool_overflow_test() ->
       {ok, _Pool} = erlmcp_transport_http_pool:start_link(#{
           size => 2,
           max_overflow => 1
       }),

       %% Checkout all workers + overflow
       W1 = poolboy:checkout(http_pool),
       W2 = poolboy:checkout(http_pool),
       W3 = poolboy:checkout(http_pool),  % Overflow worker

       %% Next checkout should block (or fail fast)
       {error, timeout} = poolboy:checkout(http_pool, false, 100).
   ```

#### Risks
- **Risk:** Pool exhaustion under burst load
- **Mitigation:** Configure overflow workers, add backpressure
- **Risk:** Worker crashes leak connections
- **Mitigation:** Supervisor restarts workers, gun connections auto-close

#### Deliverables
- [x] HTTP connection pool with poolboy
- [x] TCP connection pool with poolboy
- [x] Configurable pool size and overflow
- [x] Concurrency benchmark (50K+ req/s)
- [x] Pool monitoring and stats

---

### 6. Library Migration Performance Tuning

**Priority:** P1
**Effort:** 2 days
**Owner:** Performance Team

#### Motivation
v2.0 migrated to gproc, gun, ranch, poolboy. Validate performance vs. v1.5.0 baseline:
- **Target:** <10% regression on all benchmarks
- **Method:** Run canonical benchmarks, profile, tune configuration

#### Design

**Benchmark baseline comparison:**
```bash
# Run v1.5.0 benchmarks (baseline)
git checkout v1.5.0
make benchmark-quick > baseline_v1.5.0.json

# Run v2.0 benchmarks (current)
git checkout v2.0.0
make benchmark-quick > results_v2.0.0.json

# Compare
./scripts/bench/compare_results.sh baseline_v1.5.0.json results_v2.0.0.json
```

**Expected output:**
```
Benchmark Comparison: v1.5.0 vs. v2.0.0
===========================================
Registry throughput:  553K msg/s → 547K msg/s (-1.1%)  ✓
Queue throughput:     971K msg/s → 965K msg/s (-0.6%)  ✓
Pool throughput:      149K msg/s → 142K msg/s (-4.7%)  ✓
Session throughput:   242K msg/s → 238K msg/s (-1.7%)  ✓
Network I/O:           43K msg/s →  41K msg/s (-4.7%)  ✓
===========================================
Overall regression: -2.6% (PASS, <10% threshold)
```

**Tuning configuration:**
```erlang
%% config/sys.config
[
    {erlmcp_core, [
        {registry_mode, local},  % vs. distributed (slower)
        {registry_pool_size, 100}  % gproc pool size
    ]},
    {gun, [
        {http_opts, #{
            max_connections => 100,
            max_http2_streams => 100,
            initial_connection_window_size => 1048576,  % 1MB
            initial_stream_window_size => 65535  % 64KB
        }}
    ]},
    {ranch, [
        {max_connections, 10000},
        {num_acceptors, 50}
    ]},
    {poolboy, [
        {default_size, 10},
        {default_overflow, 5}
    ]}
].
```

**Profiling with recon:**
```erlang
%% Identify bottlenecks
1> recon:proc_count(memory, 10).
[{<0.123.0>,memory,5242880},  % 5MB
 {<0.456.0>,memory,2621440},
 ...]

2> recon:proc_window(reductions, 10, 1000).
[{<0.123.0>,reductions,50000},  % High CPU
 {<0.456.0>,reductions,35000},
 ...]

%% Profile hot paths
3> eprof:start().
4> eprof:profile([self()], erlmcp_client, call_tool, [Client, <<"tool">>, #{}]).
5> eprof:analyze(total).
```

#### Testing

1. **Regression tests:**
   ```erlang
   benchmark_regression_test() ->
       %% Run all 5 canonical benchmarks
       Results = [
           erlmcp_bench_core_ops:run(<<"core_ops_100k">>),
           erlmcp_bench_network_real:run(<<"tcp_sustained_10k">>),
           erlmcp_bench_stress:run(<<"stress_30s_100k_ops">>),
           erlmcp_bench_chaos:run(<<"chaos_memory_exhaustion">>),
           erlmcp_bench_integration:run(<<"mcp_tool_sequence">>)
       ],

       %% Compare to baseline
       Baseline = load_baseline("v1.5.0.json"),
       Regressions = [compare(R, B) || {R, B} <- lists:zip(Results, Baseline)],

       ?assert(lists:all(fun(R) -> R < 0.10 end, Regressions)).
   ```

#### Deliverables
- [x] Baseline comparison script
- [x] Tuned configuration for gproc/gun/ranch/poolboy
- [x] Profiling report (top 10 hot paths)
- [x] Regression <10% validation
- [x] Performance tuning guide (docs/)

---

## Phase 3: New Features

### 7. Distributed Registry with gproc Global Mode

**Priority:** P2
**Effort:** 3 days
**Owner:** Distributed Systems Team

#### Motivation
Current registry is local (single-node). Multi-node deployments require:
- Horizontal scaling (serve 100K+ connections across 10 nodes)
- Failover (automatic server migration on node failure)
- Location transparency (clients don't know which node hosts server)

#### Design

**Global registration with gproc:**
```erlang
%% Config: Enable distributed mode
{erlmcp_core, [
    {registry_mode, distributed},
    {cluster_nodes, ['erlmcp1@host', 'erlmcp2@host', 'erlmcp3@host']}
]}.

%% Server registration (global)
erlmcp_registry:register_server(<<"server-123">>, self(), Config) ->
    gproc:reg({n, g, {mcp_server, <<"server-123">>}}, self()),
    gproc:set_value({p, g, {mcp_server_config, <<"server-123">>}}, Config),
    ok.

%% Client lookup (automatic node routing)
erlmcp_registry:find_server(<<"server-123">>) ->
    case gproc:lookup_global_name({mcp_server, <<"server-123">>}) of
        undefined -> {error, not_found};
        Pid when node(Pid) =:= node() -> {ok, Pid};  % Local
        Pid -> {ok, {remote, node(Pid), Pid}}  % Remote node
    end.
```

**Cluster management:**
```erlang
-module(erlmcp_cluster).
-export([join/1, leave/0, nodes/0, health/0]).

%% Join cluster
join(Node) ->
    net_kernel:connect_node(Node),
    case lists:member(Node, nodes()) of
        true ->
            %% Sync gproc registry
            gproc:sync(),
            {ok, connected};
        false ->
            {error, connection_failed}
    end.

%% List cluster nodes
nodes() ->
    [node() | erlang:nodes()].

%% Health check (detect split-brain)
health() ->
    Nodes = nodes(),
    Pings = [{N, net_adm:ping(N)} of N <- Nodes],
    case lists:all(fun({_, pong}) -> true; (_) -> false end, Pings) of
        true -> {ok, healthy};
        false -> {error, {partition, Pings}}
    end.
```

#### Testing

1. **Multi-node test:**
   ```erlang
   distributed_registry_test() ->
       %% Start 3-node cluster
       {ok, [Node1, Node2, Node3]} = start_cluster(3),

       %% Register server on Node1
       {ok, Server} = rpc:call(Node1, erlmcp_server, start_link, []),
       ok = rpc:call(Node1, erlmcp_registry, register_server, [<<"server-1">>, Server, #{}]),

       %% Lookup from Node2 (should route to Node1)
       {ok, {remote, Node1, Server}} = rpc:call(Node2, erlmcp_registry, find_server, [<<"server-1">>]),

       %% Kill Node1, verify failover to Node2
       stop_node(Node1),
       timer:sleep(1000),  % Wait for gproc sync
       {ok, NewServer} = rpc:call(Node2, erlmcp_registry, find_server, [<<"server-1">>]).
   ```

2. **Load balancing test:**
   ```erlang
   distributed_load_test() ->
       {ok, Nodes} = start_cluster(3),

       %% Register 300 servers across 3 nodes
       [rpc:call(lists:nth(I rem 3 + 1, Nodes), erlmcp_server, start_link, []) ||
        I <- lists:seq(1, 300)],

       %% Verify even distribution
       Counts = [length(rpc:call(N, erlmcp_registry, local_servers, [])) || N <- Nodes],
       ?assertEqual([100, 100, 100], Counts).
   ```

#### Risks
- **Risk:** Network partitions (split-brain)
- **Mitigation:** Use gproc's built-in conflict resolution, add health checks
- **Risk:** High latency for cross-node calls
- **Mitigation:** Prefer local lookups, cache remote refs

#### Deliverables
- [x] Global registry with gproc
- [x] Cluster management API (`join/1`, `leave/0`, `health/0`)
- [x] Multi-node Common Test suite (3+ nodes)
- [x] Failover tests (node crash scenarios)
- [x] Distributed deployment guide

---

### 8. Hot Code Reload Safety

**Priority:** P2
**Effort:** 2 days
**Owner:** OTP Team

#### Motivation
Production deployments require zero-downtime upgrades:
- **Current:** Restart application → Drop connections → Re-establish
- **Target:** Hot reload → Suspend → Upgrade → Resume → Zero dropped connections

#### Design

**Code upgrade protocol:**
```erlang
-module(erlmcp_upgrade).
-export([upgrade/1, upgrade_info/0]).

%% Initiate upgrade
upgrade(NewVersion) ->
    %% 1. Suspend all clients/servers
    ok = suspend_all_gen_servers(),

    %% 2. Load new code
    Modules = [erlmcp_client, erlmcp_server, erlmcp_registry, ...],
    [code:purge(M) || M <- Modules],
    [code:load_file(M) || M <- Modules],

    %% 3. Call code_change on all processes
    Results = [sys:change_code(Pid, M, OldVsn, NewVsn, []) ||
               {M, Pid} <- get_all_gen_servers()],

    %% 4. Resume
    ok = resume_all_gen_servers(),

    {ok, #{
        version => NewVersion,
        modules_upgraded => length(Modules),
        processes_migrated => length(Results)
    }}.

%% Suspend without killing
suspend_all_gen_servers() ->
    GenServers = get_all_gen_servers(),
    [sys:suspend(Pid) || {_, Pid} <- GenServers],
    ok.

resume_all_gen_servers() ->
    GenServers = get_all_gen_servers(),
    [sys:resume(Pid) || {_, Pid} <- GenServers],
    ok.
```

**State migration in gen_servers:**
```erlang
%% Location: apps/erlmcp_core/src/erlmcp_client.erl

code_change(_OldVsn, State, _Extra) ->
    %% Migrate state record if fields added/removed
    case State of
        #state{} = OldState ->
            %% Add new fields with defaults
            NewState = OldState#state{
                new_field = default_value
            },
            {ok, NewState};
        OldStateFormat ->
            %% Convert from old format
            NewState = convert_state(OldStateFormat),
            {ok, NewState}
    end.
```

#### Testing

1. **Upgrade test:**
   ```erlang
   hot_reload_test() ->
       %% Start v2.0 client
       {ok, Client} = erlmcp_client:start_link({stdio, []}),
       {ok, _} = erlmcp_client:initialize(Client, #mcp_client_capabilities{}),

       %% Perform upgrade to v2.1
       {ok, Info} = erlmcp_upgrade:upgrade(<<"2.1.0">>),
       ?assertMatch(#{modules_upgraded := N} when N > 0, Info),

       %% Verify client still works (no dropped connection)
       {ok, Result} = erlmcp_client:call_tool(Client, <<"tool">>, #{}),
       ?assertMatch(#{<<"result">> := _}, Result).
   ```

2. **Connection persistence test:**
   ```erlang
   connection_persistence_test() ->
       %% Establish 100 connections
       Clients = [erlmcp_client:start_link({tcp, #{}}) || _ <- lists:seq(1, 100)],

       %% Upgrade
       erlmcp_upgrade:upgrade(<<"2.1.0">>),

       %% Verify all connections still alive
       Alive = [erlang:is_process_alive(C) || {ok, C} <- Clients],
       ?assertEqual(100, length([true || true <- Alive])).
   ```

#### Risks
- **Risk:** State migration bugs (incompatible state formats)
- **Mitigation:** Comprehensive code_change tests, versioned state records
- **Risk:** Upgrade hangs (process doesn't respond to suspend)
- **Mitigation:** Timeout on suspend, force kill if unresponsive

#### Deliverables
- [x] `erlmcp_upgrade:upgrade/1` API
- [x] State migration in all gen_servers
- [x] Hot reload tests (connection persistence)
- [x] Upgrade guide (docs/)
- [x] Rollback procedure

---

### 9. Enhanced Observability with Distributed Tracing

**Priority:** P2
**Effort:** 2 days
**Owner:** Observability Team

#### Motivation
Current OTEL implementation only traces local operations. Distributed systems require:
- Trace context propagation across services (client → server → database)
- Span links for async operations (request → notification)
- Visualization of end-to-end latency

#### Design

**Trace context propagation:**
```erlang
%% Inject trace context into MCP messages
build_request(RequestId, Method, Params) ->
    TraceContext = otel_tracer:current_span_ctx(),
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => Method,
        <<"params">> => Params,
        <<"_trace">> => serialize_span_ctx(TraceContext)  % W3C Trace Context
    }.

%% Extract and activate trace context
handle_request(Request) ->
    case maps:get(<<"_trace">>, Request, undefined) of
        undefined -> ok;
        TraceContext ->
            SpanCtx = deserialize_span_ctx(TraceContext),
            otel_tracer:set_current_span(SpanCtx)
    end,
    ...
```

**W3C Trace Context format:**
```json
{
  "_trace": {
    "traceparent": "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01",
    "tracestate": "erlmcp=t61rcWkgMzE"
  }
}
```

**Span links for async operations:**
```erlang
%% Client sends request (span 1)
ClientSpan = otel_tracer:start_span(<<"client.call_tool">>),
Request = build_request(RequestId, Method, Params),
send_message(State, Request),

%% Server receives request (span 2, linked to span 1)
ServerSpan = otel_tracer:start_span(<<"server.handle_tool">>, #{
    links => [otel_span:span_ctx(ClientSpan)]
}),
Result = handle_tool(Request),
otel_span:end_span(ServerSpan),

%% Client receives response (span 1 ends)
otel_span:end_span(ClientSpan).
```

#### Testing

1. **Context propagation test:**
   ```erlang
   trace_propagation_test() ->
       %% Start client with OTEL enabled
       {ok, Client} = erlmcp_client:start_link({tcp, #{otel => true}}),

       %% Create root span
       RootSpan = otel_tracer:start_span(<<"test.root">>),
       otel_tracer:set_current_span(RootSpan),

       %% Call tool (should create child span)
       {ok, _} = erlmcp_client:call_tool(Client, <<"tool">>, #{}),

       %% Verify span hierarchy
       Spans = otel_batch_processor:force_flush(),
       ?assertEqual(2, length(Spans)),  % Root + child
       [ChildSpan] = [S || S <- Spans, S =/= RootSpan],
       ?assertEqual(otel_span:trace_id(RootSpan), otel_span:trace_id(ChildSpan)).
   ```

2. **End-to-end tracing test:**
   ```erlang
   distributed_trace_test() ->
       %% Start 3-service chain: client → proxy → server
       {ok, Client} = start_client(),
       {ok, Proxy} = start_proxy(),
       {ok, Server} = start_server(),

       %% Call through chain
       RootSpan = otel_tracer:start_span(<<"e2e.test">>),
       {ok, Result} = call_through_proxy(Client, Proxy, Server, <<"tool">>, #{}),
       otel_span:end_span(RootSpan),

       %% Verify trace has 3 spans
       Spans = otel_batch_processor:force_flush(),
       ?assertEqual(3, length(Spans)),
       ?assertEqual([<<"e2e.test">>, <<"proxy.forward">>, <<"server.handle">>],
                    [otel_span:name(S) || S <- Spans]).
   ```

#### Deliverables
- [x] W3C Trace Context propagation in MCP messages
- [x] Span links for async operations
- [x] Distributed tracing tests (3+ hops)
- [x] Jaeger/Zipkin integration guide
- [x] Example dashboard (Grafana)

---

### 10. Transport Auto-Discovery

**Priority:** P2
**Effort:** 1 day
**Owner:** Transport Team

#### Motivation
Simplify configuration by detecting transport from environment:
- **stdio:** Running as subprocess (no TTY attached)
- **TCP:** `MCP_TCP_PORT` env var set
- **HTTP:** `MCP_HTTP_URL` env var set

#### Design

```erlang
%% Auto-detect transport
{ok, Client} = erlmcp_client:start_link(auto).

%% Implementation
-spec detect_transport() -> transport_opts().
detect_transport() ->
    case os:getenv("MCP_HTTP_URL") of
        false -> detect_tcp_or_stdio();
        Url -> {http, #{url => list_to_binary(Url)}}
    end.

detect_tcp_or_stdio() ->
    case os:getenv("MCP_TCP_PORT") of
        false -> detect_stdio();
        Port -> {tcp, #{host => "localhost", port => list_to_integer(Port)}}
    end.

detect_stdio() ->
    case erlang:system_info(port_count) > 0 andalso
         lists:member(standard_io, erlang:ports()) of
        true -> {stdio, []};
        false -> {error, no_transport_detected}
    end.
```

#### Testing

```erlang
auto_discovery_stdio_test() ->
    %% No env vars, stdio available
    os:unsetenv("MCP_HTTP_URL"),
    os:unsetenv("MCP_TCP_PORT"),
    {stdio, []} = erlmcp_client:detect_transport().

auto_discovery_tcp_test() ->
    %% TCP port set
    os:putenv("MCP_TCP_PORT", "3000"),
    {tcp, #{port := 3000}} = erlmcp_client:detect_transport().

auto_discovery_http_test() ->
    %% HTTP URL set
    os:putenv("MCP_HTTP_URL", "http://localhost:3001"),
    {http, #{url := <<"http://localhost:3001">>}} = erlmcp_client:detect_transport().
```

#### Deliverables
- [x] Auto-discovery logic
- [x] Environment variable support
- [x] Tests for all scenarios
- [x] Environment config guide

---

### 11. Client SDK Generation

**Priority:** P2
**Effort:** 3 days
**Owner:** Tooling Team

#### Motivation
Reduce boilerplate by generating typed client code from server definitions:
- **Before:** Manual `erlmcp_client:call_tool(Client, <<"weather.get_forecast">>, #{<<"city">> => City})`
- **After:** Generated `weather_client:get_forecast(Client, City)` with type specs

#### Design

**Code generation from server capabilities:**
```erlang
%% Query server for tool definitions
{ok, Tools} = erlmcp_client:list_tools(Client),

%% Generate client module
erlmcp_codegen:generate_client(Tools, "weather_client").

%% Generated output: weather_client.erl
-module(weather_client).
-export([get_forecast/2, get_current/2]).

-spec get_forecast(pid(), binary()) -> {ok, map()} | {error, term()}.
get_forecast(Client, City) ->
    erlmcp_client:call_tool(Client, <<"weather.get_forecast">>, #{
        <<"city">> => City
    }).

-spec get_current(pid(), binary()) -> {ok, map()} | {error, term()}.
get_current(Client, City) ->
    erlmcp_client:call_tool(Client, <<"weather.get_current">>, #{
        <<"city">> => City
    }).
```

**JSON Schema to Erlang types:**
```json
{
  "name": "get_forecast",
  "inputSchema": {
    "type": "object",
    "properties": {
      "city": {"type": "string"},
      "days": {"type": "integer", "minimum": 1, "maximum": 7}
    },
    "required": ["city"]
  }
}
```

Generated spec:
```erlang
-spec get_forecast(pid(), City :: binary(), Days :: 1..7) -> {ok, map()} | {error, term()}.
```

#### Testing

```erlang
codegen_test() ->
    %% Start weather server
    {ok, Server} = start_weather_server(),
    {ok, Client} = erlmcp_client:start_link({stdio, []}),

    %% Generate client
    {ok, Module} = erlmcp_codegen:generate_client(Client, weather_client),
    code:load_binary(Module, "weather_client.erl", compiled_code),

    %% Use generated client
    {ok, Forecast} = weather_client:get_forecast(Client, <<"Seattle">>),
    ?assertMatch(#{<<"temperature">> := _, <<"conditions">> := _}, Forecast).
```

#### Deliverables
- [x] Code generator (`erlmcp_codegen:generate_client/2`)
- [x] JSON Schema → Erlang type conversion
- [x] Generated client tests
- [x] Example generated clients (weather, calculator)
- [x] Code generation guide

---

## Phase 4: Developer Experience

### 12. Improved Error Messages

**Priority:** P3
**Effort:** 2 days
**Owner:** DX Team

#### Motivation
Current errors lack context, making debugging difficult:
- **Before:** `{error, not_initialized}`
- **After:** `{error, #{reason => not_initialized, phase => pre_initialization, hint => <<"Call initialize/2 first. See: https://erlmcp.dev/docs/client#initialize">>}}`

#### Design

**Structured error format:**
```erlang
-type error_detail() :: #{
    reason := atom(),
    message => binary(),
    phase => atom(),
    context => map(),
    hint => binary(),
    docs_url => binary()
}.

-spec format_error(atom(), map()) -> error_detail().
format_error(not_initialized, Context) ->
    #{
        reason => not_initialized,
        message => <<"Client not initialized">>,
        phase => maps:get(phase, Context, unknown),
        hint => <<"Call erlmcp_client:initialize/2 before making requests">>,
        docs_url => <<"https://erlmcp.dev/docs/client#initialize">>
    };
format_error(capability_not_supported, Context) ->
    #{
        reason => capability_not_supported,
        message => <<"Server does not support required capability">>,
        context => #{
            requested => maps:get(capability, Context),
            server_capabilities => maps:get(server_caps, Context)
        },
        hint => <<"Check server capabilities with list_capabilities/1">>,
        docs_url => <<"https://erlmcp.dev/docs/capabilities">>
    }.
```

**Update all error returns:**
```erlang
%% Before
{reply, {error, not_initialized}, State}

%% After
{reply, {error, format_error(not_initialized, #{phase => State#state.phase})}, State}
```

#### Testing

```erlang
error_message_quality_test() ->
    {ok, Client} = erlmcp_client:start_link({stdio, []}),

    %% Call without initializing
    {error, ErrorDetail} = erlmcp_client:call_tool(Client, <<"tool">>, #{}),

    %% Verify structured error
    ?assertMatch(#{
        reason := not_initialized,
        hint := _,
        docs_url := _
    }, ErrorDetail).
```

#### Deliverables
- [x] Structured error format
- [x] Error formatting for all modules
- [x] Error message tests
- [x] Error handling guide (docs/)

---

### 13. TCPS Integration Improvements

**Priority:** P3
**Effort:** 2 days
**Owner:** TCPS Team

#### Motivation
TCPS separation in v2.0 requires clear integration documentation and improvements:
- When to use TCPS (manufacturing workflows, quality gates, CI/CD)
- How to set up tcps_erlmcp application
- Integration with erlmcp_core observability

#### Design

**Simplified TCPS setup:**
```erlang
%% 1. Add tcps_erlmcp to rebar.config
{deps, [
    {erlmcp_core, "2.0.0"},
    {erlmcp_observability, "2.0.0"},
    {tcps_erlmcp, "2.0.0"}  % Add TCPS
]}.

%% 2. Configure TCPS quality gates
{tcps_erlmcp, [
    {enabled, true},
    {quality_gates, #{
        min_test_pass_rate => 0.95,
        min_coverage => 0.85,
        max_defects_per_kloc => 0.5
    }},
    {dashboard, #{
        enabled => true,
        port => 8080
    }}
]}.

%% 3. Run quality checks
tcps_jidoka:check_quality(Project) ->
    %% Built-in quality gates (Jidoka = stop-the-line)
    case tcps_quality_gate:validate(Project) of
        {ok, passed} -> {ok, #{status => passed}};
        {error, {failed, Reasons}} ->
            tcps_andon:signal_problem(Reasons),  % Visible alert
            {error, quality_gate_failed}
    end.
```

**TCPS + OTEL integration:**
```erlang
%% TCPS automatically sends metrics to OTEL
tcps_metrics:record_build(#{
    duration_s => 120,
    status => passed,
    coverage_pct => 87.3
}),

%% Appears in OTEL trace as span
%% trace_id: 0af7651916cd43dd8448eb211c80319c
%% span_id: b7ad6b7169203331
%% name: tcps.build
%% attributes: {duration_s: 120, status: "passed", coverage_pct: 87.3}
```

#### Deliverables
- [x] TCPS integration guide (docs/tcps/INTEGRATION_GUIDE.md)
- [x] Example CI/CD pipeline with TCPS gates
- [x] TCPS + OTEL integration examples
- [x] Dashboard setup guide
- [x] CLI usage examples (26 commands)

---

## Summary: Feature Effort Estimates

| Phase | Features | Total Effort | Parallelizable | Critical Path |
|-------|----------|--------------|----------------|---------------|
| 1: Critical Fixes | 5 | 6-7 days | Yes (3 parallel) | 3 days |
| 2: Performance | 5 | 10 days | Partially (2 parallel) | 6 days |
| 3: New Features | 5 | 13 days | Yes (3 parallel) | 5 days |
| 4: Developer Experience | 2 | 4 days | Yes (2 parallel) | 2 days |
| **TOTAL** | **17** | **33-34 days** | **With 3 engineers** | **16 days (5-6 weeks)** |

**Recommended Team:**
- **2 core engineers:** Phase 1 + Phase 2 (critical path)
- **1 features engineer:** Phase 3 (parallel with Phase 2)
- **1 DX engineer:** Phase 4 (parallel with Phase 3)

**Timeline:** 6-8 weeks end-to-end with 3-4 engineers.

---

## References

- **Roadmap:** `docs/v2.1/ROADMAP.md` (prioritization, timeline)
- **Risks:** `docs/v2/V2_DESIGN_INPUTS/v2_risks.md` (v2.0 risk analysis)
- **Implementation Report:** `docs/V2_IMPLEMENTATION_REPORT.md` (v2.0 known issues)
- **Benchmarks:** `bench/erlmcp_bench_*.erl` (performance baselines)

---

**Document Status:** DETAILED DESIGN
**Last Updated:** 2026-01-27
**Owner:** erlmcp Core Team
**Next Steps:** Assign features to engineers, create GitHub issues for top 20
