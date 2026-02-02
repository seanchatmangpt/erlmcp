# erlmcp Transport Optimization - Implementation Guide

## Quick Reference

**Design Document**: `/home/user/erlmcp/docs/TRANSPORT_OPTIMIZATION_DESIGN.md`
**Current Transport Files**:
- `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_tcp.erl` (893 LOC)
- `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_stdio.erl` (376 LOC)
- `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_ws.erl` (678 LOC)
- `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_sse.erl` (239 LOC)
- `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_pool.erl` (188 LOC)
- `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_health.erl` (543 LOC)

---

## 1. Message Batching Implementation

### File: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_batcher.erl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc Message Batching Module
%%% Batches messages to reduce system call overhead and improve throughput
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_batcher).
-behaviour(gen_server).

-export([start_link/2, enqueue/2, flush/1, get_stats/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    transport_pid :: pid(),
    transport_type :: atom(),
    buffer = [] :: [binary()],
    buffer_size_bytes = 0 :: non_neg_integer(),
    max_batch_size :: pos_integer(),
    max_batch_bytes :: pos_integer(),
    max_batch_delay_ms :: pos_integer(),
    batch_timer :: reference() | undefined,
    compression = none :: none | lz4 | zstd | gzip,
    stats = #{
        batches_sent => 0,
        messages_batched => 0,
        bytes_compressed => 0,
        compression_time_us => 0
    } :: map()
}).

%% API
start_link(TransportPid, Config) ->
    gen_server:start_link(?MODULE, [TransportPid, Config], []).

enqueue(BatcherPid, Message) ->
    gen_server:cast(BatcherPid, {enqueue, Message}).

flush(BatcherPid) ->
    gen_server:call(BatcherPid, flush).

get_stats(BatcherPid) ->
    gen_server:call(BatcherPid, get_stats).

%% gen_server callbacks
init([TransportPid, Config]) ->
    State = #state{
        transport_pid = TransportPid,
        transport_type = maps:get(transport_type, Config, tcp),
        max_batch_size = maps:get(max_batch_size, Config, 100),
        max_batch_bytes = maps:get(max_batch_bytes, Config, 65536),
        max_batch_delay_ms = maps:get(max_batch_delay_ms, Config, 10),
        compression = maps:get(compression, Config, none)
    },
    {ok, State}.

handle_cast({enqueue, Message}, State) ->
    NewBuffer = [Message | State#state.buffer],
    NewSize = State#state.buffer_size_bytes + byte_size(Message),

    %% Check flush conditions
    ShouldFlush =
        length(NewBuffer) >= State#state.max_batch_size orelse
        NewSize >= State#state.max_batch_bytes,

    case ShouldFlush of
        true ->
            NewState = do_flush(State#state{buffer = NewBuffer, buffer_size_bytes = NewSize}),
            {noreply, NewState};
        false ->
            %% Start timer if not running
            Timer = case State#state.batch_timer of
                undefined ->
                    erlang:send_after(State#state.max_batch_delay_ms, self(), flush);
                T -> T
            end,
            {noreply, State#state{
                buffer = NewBuffer,
                buffer_size_bytes = NewSize,
                batch_timer = Timer
            }}
    end.

handle_call(flush, _From, State) ->
    NewState = do_flush(State),
    {reply, ok, NewState};

handle_call(get_stats, _From, State) ->
    {reply, State#state.stats, State}.

handle_info(flush, State) ->
    NewState = do_flush(State#state{batch_timer = undefined}),
    {noreply, NewState}.

terminate(_Reason, State) ->
    do_flush(State),
    ok.

%% Internal functions
do_flush(#state{buffer = []} = State) ->
    State;
do_flush(State) ->
    Messages = lists:reverse(State#state.buffer),
    Batch = create_batch(Messages, State#state.transport_type, State#state.compression),

    %% Send batch to transport
    ok = send_batch(State#state.transport_pid, Batch),

    %% Update stats
    NewStats = update_stats(State#state.stats, length(Messages), State#state.buffer_size_bytes),

    %% Cancel timer if running
    case State#state.batch_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,

    State#state{
        buffer = [],
        buffer_size_bytes = 0,
        batch_timer = undefined,
        stats = NewStats
    }.

create_batch(Messages, tcp, Compression) ->
    %% TCP: Length-prefixed batch
    Payload = iolist_to_binary([iolist_to_binary(M) || M <- Messages]),
    Compressed = compress_payload(Payload, Compression),
    Length = byte_size(Compressed),
    <<Length:32, Compressed/binary>>;

create_batch(Messages, websocket, Compression) ->
    %% WebSocket: JSON array
    Payload = jsx:encode(Messages),
    compress_payload(Payload, Compression);

create_batch(Messages, stdio, _Compression) ->
    %% Stdio: Newline-delimited (no compression)
    iolist_to_binary([<<M/binary, "\n">> || M <- Messages]).

compress_payload(Payload, none) ->
    Payload;
compress_payload(Payload, lz4) ->
    {ok, Compressed} = lz4:compress(Payload),
    Compressed;
compress_payload(Payload, zstd) ->
    ezstd:compress(Payload);
compress_payload(Payload, gzip) ->
    zlib:gzip(Payload).

send_batch(TransportPid, Batch) ->
    TransportPid ! {send_batch, Batch},
    ok.

update_stats(Stats, MessageCount, ByteCount) ->
    Stats#{
        batches_sent => maps:get(batches_sent, Stats, 0) + 1,
        messages_batched => maps:get(messages_batched, Stats, 0) + MessageCount,
        bytes_compressed => maps:get(bytes_compressed, Stats, 0) + ByteCount
    }.
```

---

## 2. Enhanced Connection Pool

### File: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_pool_v2.erl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc Enhanced Connection Pool with Multiple Strategies
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_pool_v2).
-behaviour(gen_server).

-export([start_link/2, acquire/2, release/2, get_stats/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(connection, {
    pid :: pid(),
    monitor_ref :: reference(),
    created_at :: integer(),
    last_used :: integer(),
    request_count = 0 :: non_neg_integer(),
    error_count = 0 :: non_neg_integer(),
    avg_latency_us = 0.0 :: float(),
    health_score = 1.0 :: float()
}).

-record(state, {
    pool_id :: atom(),
    strategy :: round_robin | least_loaded | least_latency | adaptive,
    min_size :: pos_integer(),
    max_size :: pos_integer(),
    current_size = 0 :: non_neg_integer(),
    available = queue:new() :: queue:queue(#connection{}),
    in_use = #{} :: #{pid() => #connection{}},
    config :: map(),
    next_index = 0 :: non_neg_integer(),  % For round-robin
    metrics = #{
        acquisitions => 0,
        releases => 0,
        timeouts => 0,
        errors => 0
    } :: map()
}).

%% API
start_link(PoolId, Config) ->
    gen_server:start_link({local, PoolId}, ?MODULE, [PoolId, Config], []).

acquire(PoolId, Timeout) ->
    gen_server:call(PoolId, {acquire, self()}, Timeout).

release(PoolId, ConnPid) ->
    gen_server:cast(PoolId, {release, ConnPid}).

get_stats(PoolId) ->
    gen_server:call(PoolId, get_stats).

%% gen_server callbacks
init([PoolId, Config]) ->
    Strategy = maps:get(strategy, Config, round_robin),
    MinSize = maps:get(min_size, Config, 10),
    MaxSize = maps:get(max_size, Config, 100),

    State = #state{
        pool_id = PoolId,
        strategy = Strategy,
        min_size = MinSize,
        max_size = MaxSize,
        config = Config
    },

    %% Pre-warm pool to min_size
    NewState = ensure_min_connections(State),

    %% Start health check timer
    erlang:send_after(30000, self(), health_check),

    {ok, NewState}.

handle_call({acquire, ClientPid}, From, State) ->
    case select_connection(State) of
        {ok, Connection, NewState} ->
            MonitorRef = erlang:monitor(process, ClientPid),
            UpdatedConn = Connection#connection{
                last_used = erlang:system_time(millisecond),
                request_count = Connection#connection.request_count + 1
            },
            NewInUse = maps:put(Connection#connection.pid, {UpdatedConn, MonitorRef}, NewState#state.in_use),
            NewMetrics = maps:update_with(acquisitions, fun(V) -> V + 1 end, 1, State#state.metrics),
            {reply, {ok, Connection#connection.pid}, NewState#state{in_use = NewInUse, metrics = NewMetrics}};
        {error, no_connections} ->
            %% Try to create new connection if under max_size
            case State#state.current_size < State#state.max_size of
                true ->
                    case create_connection(State) of
                        {ok, NewConn, NewState2} ->
                            handle_call({acquire, ClientPid}, From, NewState2);
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                false ->
                    NewMetrics = maps:update_with(timeouts, fun(V) -> V + 1 end, 1, State#state.metrics),
                    {reply, {error, pool_full}, State#state{metrics = NewMetrics}}
            end
    end;

handle_call(get_stats, _From, State) ->
    Stats = #{
        pool_id => State#state.pool_id,
        strategy => State#state.strategy,
        available => queue:len(State#state.available),
        in_use => maps:size(State#state.in_use),
        current_size => State#state.current_size,
        min_size => State#state.min_size,
        max_size => State#state.max_size,
        metrics => State#state.metrics
    },
    {reply, {ok, Stats}, State}.

handle_cast({release, ConnPid}, State) ->
    case maps:take(ConnPid, State#state.in_use) of
        {{Connection, MonitorRef}, NewInUse} ->
            erlang:demonitor(MonitorRef, [flush]),
            NewAvailable = queue:in(Connection, State#state.available),
            NewMetrics = maps:update_with(releases, fun(V) -> V + 1 end, 1, State#state.metrics),
            {noreply, State#state{available = NewAvailable, in_use = NewInUse, metrics = NewMetrics}};
        error ->
            {noreply, State}
    end.

handle_info({'DOWN', _MonitorRef, process, ConnPid, _Reason}, State) ->
    %% Connection died, remove from pool
    NewInUse = maps:remove(ConnPid, State#state.in_use),
    NewSize = State#state.current_size - 1,
    NewMetrics = maps:update_with(errors, fun(V) -> V + 1 end, 1, State#state.metrics),

    %% Ensure min connections
    NewState = ensure_min_connections(State#state{
        in_use = NewInUse,
        current_size = NewSize,
        metrics = NewMetrics
    }),

    {noreply, NewState};

handle_info(health_check, State) ->
    NewState = perform_health_checks(State),
    erlang:send_after(30000, self(), health_check),
    {noreply, NewState}.

%% Internal functions
select_connection(#state{available = Available, strategy = Strategy} = State) ->
    case queue:is_empty(Available) of
        true ->
            {error, no_connections};
        false ->
            case Strategy of
                round_robin ->
                    select_round_robin(State);
                least_loaded ->
                    select_least_loaded(State);
                least_latency ->
                    select_least_latency(State);
                adaptive ->
                    select_adaptive(State)
            end
    end.

select_round_robin(State) ->
    {{value, Connection}, NewAvailable} = queue:out(State#state.available),
    {ok, Connection, State#state{available = NewAvailable}}.

select_least_loaded(State) ->
    %% Find connection with lowest request_count
    Connections = queue:to_list(State#state.available),
    SortedConns = lists:keysort(#connection.request_count, Connections),
    case SortedConns of
        [BestConn | Rest] ->
            NewAvailable = queue:from_list(Rest),
            {ok, BestConn, State#state{available = NewAvailable}};
        [] ->
            {error, no_connections}
    end.

select_least_latency(State) ->
    %% Find connection with lowest avg_latency_us
    Connections = queue:to_list(State#state.available),
    SortedConns = lists:keysort(#connection.avg_latency_us, Connections),
    case SortedConns of
        [BestConn | Rest] ->
            NewAvailable = queue:from_list(Rest),
            {ok, BestConn, State#state{available = NewAvailable}};
        [] ->
            {error, no_connections}
    end.

select_adaptive(State) ->
    %% Score-based selection using health_score
    Connections = queue:to_list(State#state.available),
    SortedConns = lists:reverse(lists:keysort(#connection.health_score, Connections)),
    case SortedConns of
        [BestConn | Rest] ->
            NewAvailable = queue:from_list(Rest),
            {ok, BestConn, State#state{available = NewAvailable}};
        [] ->
            {error, no_connections}
    end.

create_connection(State) ->
    %% Start new connection based on config
    TransportModule = maps:get(transport_module, State#state.config),
    TransportConfig = maps:get(transport_config, State#state.config),

    case TransportModule:start_link(TransportConfig) of
        {ok, Pid} ->
            MonitorRef = erlang:monitor(process, Pid),
            Connection = #connection{
                pid = Pid,
                monitor_ref = MonitorRef,
                created_at = erlang:system_time(millisecond),
                last_used = erlang:system_time(millisecond)
            },
            NewAvailable = queue:in(Connection, State#state.available),
            {ok, Connection, State#state{
                available = NewAvailable,
                current_size = State#state.current_size + 1
            }};
        {error, Reason} ->
            {error, Reason}
    end.

ensure_min_connections(State) when State#state.current_size >= State#state.min_size ->
    State;
ensure_min_connections(State) ->
    case create_connection(State) of
        {ok, _Conn, NewState} ->
            ensure_min_connections(NewState);
        {error, _Reason} ->
            State
    end.

perform_health_checks(State) ->
    %% Update health scores for all connections
    Connections = queue:to_list(State#state.available),
    UpdatedConns = lists:map(fun update_health_score/1, Connections),
    State#state{available = queue:from_list(UpdatedConns)}.

update_health_score(Conn) ->
    %% Calculate health score based on multiple factors
    LatencyScore = calculate_latency_score(Conn#connection.avg_latency_us),
    ErrorScore = calculate_error_score(Conn#connection.error_count, Conn#connection.request_count),
    AgeScore = calculate_age_score(Conn#connection.created_at),

    HealthScore = 0.5 * LatencyScore + 0.3 * ErrorScore + 0.2 * AgeScore,

    Conn#connection{health_score = HealthScore}.

calculate_latency_score(Latency) ->
    max(0.0, 1.0 - (Latency / 50000)).  % Normalize to 50ms

calculate_error_score(Errors, Requests) when Requests > 0 ->
    max(0.0, 1.0 - (Errors / Requests));
calculate_error_score(_, _) ->
    1.0.

calculate_age_score(CreatedAt) ->
    Age = erlang:system_time(millisecond) - CreatedAt,
    AgeHours = Age / 3600000,
    case AgeHours of
        H when H < 1 -> 1.0;    % Fresh connection
        H when H < 24 -> 0.8;   % Recent connection
        _ -> 0.5                % Old connection
    end.
```

---

## 3. Circuit Breaker Integration

### File: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_circuit_breaker.erl`

```erlang
%%%-------------------------------------------------------------------
%%% @doc Circuit Breaker Pattern Implementation
%%% Prevents cascading failures by opening circuit after threshold
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_circuit_breaker).
-behaviour(gen_server).

-export([start_link/0, record_success/1, record_failure/1, is_available/1, get_state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(circuit, {
    transport_id :: atom(),
    state = closed :: closed | open | half_open,
    failure_count = 0 :: non_neg_integer(),
    success_count = 0 :: non_neg_integer(),
    failure_threshold = 5 :: pos_integer(),
    success_threshold = 2 :: pos_integer(),
    timeout_ms = 30000 :: pos_integer(),
    last_failure_time :: integer() | undefined,
    half_open_time :: integer() | undefined
}).

-record(state, {
    circuits = #{} :: #{atom() => #circuit{}}
}).

%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

record_success(TransportId) ->
    gen_server:cast(?MODULE, {success, TransportId}).

record_failure(TransportId) ->
    gen_server:cast(?MODULE, {failure, TransportId}).

is_available(TransportId) ->
    gen_server:call(?MODULE, {is_available, TransportId}).

get_state(TransportId) ->
    gen_server:call(?MODULE, {get_state, TransportId}).

%% gen_server callbacks
init([]) ->
    {ok, #state{}}.

handle_call({is_available, TransportId}, _From, State) ->
    Circuit = get_or_create_circuit(TransportId, State),
    Available = case Circuit#circuit.state of
        closed -> true;
        half_open -> true;  % Allow probe requests
        open ->
            %% Check if timeout has passed
            Now = erlang:system_time(millisecond),
            case Circuit#circuit.last_failure_time of
                undefined -> false;
                LastFailure ->
                    Elapsed = Now - LastFailure,
                    case Elapsed >= Circuit#circuit.timeout_ms of
                        true ->
                            %% Transition to half_open
                            NewCircuit = Circuit#circuit{
                                state = half_open,
                                half_open_time = Now,
                                success_count = 0
                            },
                            NewCircuits = maps:put(TransportId, NewCircuit, State#state.circuits),
                            gen_server:cast(self(), {update_state, State#state{circuits = NewCircuits}}),
                            true;
                        false ->
                            false
                    end
            end
    end,
    {reply, Available, State};

handle_call({get_state, TransportId}, _From, State) ->
    Circuit = get_or_create_circuit(TransportId, State),
    {reply, Circuit, State}.

handle_cast({success, TransportId}, State) ->
    Circuit = get_or_create_circuit(TransportId, State),
    NewCircuit = handle_success(Circuit),
    NewCircuits = maps:put(TransportId, NewCircuit, State#state.circuits),

    %% Log state transitions
    case {Circuit#circuit.state, NewCircuit#circuit.state} of
        {OldState, NewState} when OldState =/= NewState ->
            logger:info("Circuit breaker for ~p: ~p -> ~p", [TransportId, OldState, NewState]);
        _ ->
            ok
    end,

    {noreply, State#state{circuits = NewCircuits}};

handle_cast({failure, TransportId}, State) ->
    Circuit = get_or_create_circuit(TransportId, State),
    NewCircuit = handle_failure(Circuit),
    NewCircuits = maps:put(TransportId, NewCircuit, State#state.circuits),

    %% Log state transitions
    case {Circuit#circuit.state, NewCircuit#circuit.state} of
        {OldState, NewState} when OldState =/= NewState ->
            logger:warning("Circuit breaker for ~p: ~p -> ~p (failures: ~p)",
                [TransportId, OldState, NewState, NewCircuit#circuit.failure_count]);
        _ ->
            ok
    end,

    {noreply, State#state{circuits = NewCircuits}};

handle_cast({update_state, NewState}, _State) ->
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

%% Internal functions
get_or_create_circuit(TransportId, State) ->
    case maps:get(TransportId, State#state.circuits, undefined) of
        undefined ->
            #circuit{transport_id = TransportId};
        Circuit ->
            Circuit
    end.

handle_success(Circuit) ->
    case Circuit#circuit.state of
        closed ->
            %% Reset failure count
            Circuit#circuit{failure_count = 0};
        half_open ->
            NewSuccessCount = Circuit#circuit.success_count + 1,
            case NewSuccessCount >= Circuit#circuit.success_threshold of
                true ->
                    %% Close circuit
                    Circuit#circuit{
                        state = closed,
                        success_count = 0,
                        failure_count = 0,
                        half_open_time = undefined
                    };
                false ->
                    Circuit#circuit{success_count = NewSuccessCount}
            end;
        open ->
            Circuit
    end.

handle_failure(Circuit) ->
    Now = erlang:system_time(millisecond),
    NewFailureCount = Circuit#circuit.failure_count + 1,

    case Circuit#circuit.state of
        closed ->
            case NewFailureCount >= Circuit#circuit.failure_threshold of
                true ->
                    %% Open circuit
                    Circuit#circuit{
                        state = open,
                        failure_count = NewFailureCount,
                        last_failure_time = Now
                    };
                false ->
                    Circuit#circuit{
                        failure_count = NewFailureCount,
                        last_failure_time = Now
                    }
            end;
        half_open ->
            %% Back to open
            Circuit#circuit{
                state = open,
                failure_count = NewFailureCount,
                success_count = 0,
                last_failure_time = Now,
                half_open_time = undefined
            };
        open ->
            Circuit#circuit{
                failure_count = NewFailureCount,
                last_failure_time = Now
            }
    end.
```

---

## 4. Integration with Existing TCP Transport

### Modified File: `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_tcp.erl`

Add these enhancements to the existing file:

```erlang
%% Add to state record
-record(state, {
    % ... existing fields ...
    batcher_pid :: pid() | undefined,
    circuit_breaker_enabled = false :: boolean()
}).

%% Enhanced send with batching and circuit breaker
send_with_optimizations(State, Data) ->
    TransportId = State#state.transport_id,

    %% Check circuit breaker
    case State#state.circuit_breaker_enabled of
        true ->
            case erlmcp_circuit_breaker:is_available(TransportId) of
                false ->
                    {error, circuit_breaker_open};
                true ->
                    do_optimized_send(State, Data)
            end;
        false ->
            do_optimized_send(State, Data)
    end.

do_optimized_send(State, Data) ->
    %% Use batcher if enabled
    case State#state.batcher_pid of
        undefined ->
            %% Direct send
            Result = send(State, Data),
            record_send_result(State, Result),
            Result;
        BatcherPid ->
            %% Send via batcher
            erlmcp_transport_batcher:enqueue(BatcherPid, Data),
            ok
    end.

record_send_result(State, Result) ->
    case {State#state.circuit_breaker_enabled, Result} of
        {true, ok} ->
            erlmcp_circuit_breaker:record_success(State#state.transport_id);
        {true, {error, _}} ->
            erlmcp_circuit_breaker:record_failure(State#state.transport_id);
        _ ->
            ok
    end.

%% Initialization with batching
init_with_optimizations(Opts) ->
    State = init_original(Opts),

    %% Start batcher if configured
    BatcherPid = case maps:get(batching, Opts, false) of
        true ->
            BatchConfig = maps:get(batch_config, Opts, #{}),
            {ok, Pid} = erlmcp_transport_batcher:start_link(self(), BatchConfig),
            Pid;
        false ->
            undefined
    end,

    CircuitBreakerEnabled = maps:get(circuit_breaker, Opts, false),

    State#state{
        batcher_pid = BatcherPid,
        circuit_breaker_enabled = CircuitBreakerEnabled
    }.
```

---

## 5. Performance Testing Suite

### File: `/home/user/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_bench_SUITE.erl`

```erlang
-module(erlmcp_transport_bench_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [
        bench_tcp_throughput,
        bench_tcp_latency,
        bench_tcp_with_batching,
        bench_tcp_with_compression,
        bench_connection_pool,
        bench_circuit_breaker_overhead
    ].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

%% Benchmark: TCP throughput without optimizations
bench_tcp_throughput(_Config) ->
    {ok, ServerPid} = start_tcp_server(5555),
    {ok, ClientPid} = start_tcp_client("localhost", 5555),

    MessageCount = 100000,
    MessageSize = 1024,
    Message = binary:copy(<<"X">>, MessageSize),

    StartTime = erlang:monotonic_time(microsecond),

    %% Send messages
    [erlmcp_transport_tcp:send(ClientPid, Message) || _ <- lists:seq(1, MessageCount)],

    EndTime = erlang:monotonic_time(microsecond),
    DurationSec = (EndTime - StartTime) / 1000000,
    Throughput = MessageCount / DurationSec,
    ThroughputMB = (MessageCount * MessageSize) / DurationSec / 1024 / 1024,

    ct:pal("TCP Throughput: ~.2f msg/s, ~.2f MB/s", [Throughput, ThroughputMB]),

    cleanup([ServerPid, ClientPid]),

    %% Assertion: Should achieve at least 50K msg/s
    ?assert(Throughput > 50000, "TCP throughput below baseline"),

    ok.

%% Benchmark: TCP latency (p50, p95, p99)
bench_tcp_latency(_Config) ->
    {ok, ServerPid} = start_tcp_server(5556),
    {ok, ClientPid} = start_tcp_client("localhost", 5556),

    MessageCount = 10000,
    Message = <<"test message">>,

    Latencies = lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        ok = erlmcp_transport_tcp:send(ClientPid, Message),
        _Reply = receive_reply(ClientPid),
        End = erlang:monotonic_time(microsecond),
        End - Start
    end, lists:seq(1, MessageCount)),

    SortedLatencies = lists:sort(Latencies),
    P50 = percentile(SortedLatencies, 50),
    P95 = percentile(SortedLatencies, 95),
    P99 = percentile(SortedLatencies, 99),

    ct:pal("TCP Latency: p50=~.2fms, p95=~.2fms, p99=~.2fms",
           [P50/1000, P95/1000, P99/1000]),

    cleanup([ServerPid, ClientPid]),

    %% Assertions
    ?assert(P50 < 10000, "p50 latency above 10ms"),
    ?assert(P99 < 50000, "p99 latency above 50ms"),

    ok.

%% Benchmark: TCP with message batching
bench_tcp_with_batching(_Config) ->
    BatchConfig = #{
        max_batch_size => 100,
        max_batch_bytes => 65536,
        max_batch_delay_ms => 10,
        compression => none
    },

    {ok, ServerPid} = start_tcp_server(5557),
    {ok, ClientPid} = start_tcp_client("localhost", 5557, #{batching => true, batch_config => BatchConfig}),

    MessageCount = 100000,
    MessageSize = 1024,
    Message = binary:copy(<<"X">>, MessageSize),

    StartTime = erlang:monotonic_time(microsecond),

    [erlmcp_transport_tcp:send(ClientPid, Message) || _ <- lists:seq(1, MessageCount)],

    %% Flush batcher
    erlmcp_transport_batcher:flush(ClientPid),

    EndTime = erlang:monotonic_time(microsecond),
    DurationSec = (EndTime - StartTime) / 1000000,
    Throughput = MessageCount / DurationSec,

    ct:pal("TCP Throughput (with batching): ~.2f msg/s", [Throughput]),

    cleanup([ServerPid, ClientPid]),

    %% Assertion: Batching should improve throughput by 2-3x
    ?assert(Throughput > 100000, "Batching throughput below target"),

    ok.

%% Benchmark: TCP with compression
bench_tcp_with_compression(_Config) ->
    BatchConfig = #{
        max_batch_size => 100,
        max_batch_bytes => 65536,
        max_batch_delay_ms => 10,
        compression => lz4
    },

    {ok, ServerPid} = start_tcp_server(5558),
    {ok, ClientPid} = start_tcp_client("localhost", 5558,
        #{batching => true, batch_config => BatchConfig}),

    %% Use compressible data
    Message = binary:copy(<<"AAAA">>, 256),  % 1KB of repeated data
    MessageCount = 10000,

    StartTime = erlang:monotonic_time(microsecond),

    [erlmcp_transport_tcp:send(ClientPid, Message) || _ <- lists:seq(1, MessageCount)],
    erlmcp_transport_batcher:flush(ClientPid),

    EndTime = erlang:monotonic_time(microsecond),
    DurationSec = (EndTime - StartTime) / 1000000,
    Throughput = MessageCount / DurationSec,

    {ok, Stats} = erlmcp_transport_batcher:get_stats(ClientPid),
    CompressionRatio = maps:get(compression_ratio, Stats, 1.0),

    ct:pal("TCP Throughput (with LZ4): ~.2f msg/s, compression ratio: ~.2f",
           [Throughput, CompressionRatio]),

    cleanup([ServerPid, ClientPid]),

    %% Assertion: Compression should achieve 2:1 ratio on repetitive data
    ?assert(CompressionRatio >= 2.0, "Compression ratio below target"),

    ok.

%% Benchmark: Connection pool overhead
bench_connection_pool(_Config) ->
    PoolConfig = #{
        strategy => round_robin,
        min_size => 10,
        max_size => 100,
        transport_module => erlmcp_transport_tcp,
        transport_config => #{mode => client, host => "localhost", port => 5559}
    },

    {ok, ServerPid} = start_tcp_server(5559),
    {ok, PoolPid} = erlmcp_transport_pool_v2:start_link(test_pool, PoolConfig),

    RequestCount = 10000,

    StartTime = erlang:monotonic_time(microsecond),

    lists:foreach(fun(_) ->
        {ok, ConnPid} = erlmcp_transport_pool_v2:acquire(test_pool, 5000),
        erlmcp_transport_tcp:send(ConnPid, <<"test">>),
        erlmcp_transport_pool_v2:release(test_pool, ConnPid)
    end, lists:seq(1, RequestCount)),

    EndTime = erlang:monotonic_time(microsecond),
    DurationSec = (EndTime - StartTime) / 1000000,
    RequestsPerSec = RequestCount / DurationSec,

    ct:pal("Pool Throughput: ~.2f req/s", [RequestsPerSec]),

    cleanup([ServerPid, PoolPid]),

    %% Assertion: Pool should add < 10% overhead
    ?assert(RequestsPerSec > 90000, "Pool overhead too high"),

    ok.

%% Benchmark: Circuit breaker overhead
bench_circuit_breaker_overhead(_Config) ->
    {ok, CBPid} = erlmcp_circuit_breaker:start_link(),

    Iterations = 1000000,

    %% Benchmark success recording
    StartTime1 = erlang:monotonic_time(microsecond),
    [erlmcp_circuit_breaker:record_success(test_transport) || _ <- lists:seq(1, Iterations)],
    EndTime1 = erlang:monotonic_time(microsecond),
    SuccessOverhead = (EndTime1 - StartTime1) / Iterations,

    %% Benchmark availability check
    StartTime2 = erlang:monotonic_time(microsecond),
    [erlmcp_circuit_breaker:is_available(test_transport) || _ <- lists:seq(1, Iterations)],
    EndTime2 = erlang:monotonic_time(microsecond),
    CheckOverhead = (EndTime2 - StartTime2) / Iterations,

    ct:pal("Circuit Breaker Overhead: success=~.2fμs, check=~.2fμs",
           [SuccessOverhead, CheckOverhead]),

    cleanup([CBPid]),

    %% Assertion: Overhead should be < 1μs per operation
    ?assert(SuccessOverhead < 1.0, "Success recording overhead too high"),
    ?assert(CheckOverhead < 1.0, "Availability check overhead too high"),

    ok.

%% Helper functions
start_tcp_server(Port) ->
    erlmcp_transport_tcp:start_server(#{mode => server, port => Port}).

start_tcp_client(Host, Port) ->
    start_tcp_client(Host, Port, #{}).

start_tcp_client(Host, Port, Opts) ->
    erlmcp_transport_tcp:start_client(maps:merge(#{
        mode => client,
        host => Host,
        port => Port
    }, Opts)).

receive_reply(ClientPid) ->
    receive
        {transport_message, _Msg} -> ok
    after 1000 ->
        timeout
    end.

cleanup(Pids) ->
    [exit(Pid, kill) || Pid <- Pids, is_pid(Pid)],
    timer:sleep(100).

percentile(SortedList, Percentile) ->
    Index = round(length(SortedList) * Percentile / 100),
    lists:nth(max(1, Index), SortedList).
```

---

## 6. Quick Start Guide

### Step 1: Enable Optimizations in Configuration

```erlang
%% config/sys.config
[
    {erlmcp, [
        {transports, [
            {tcp, #{
                port => 9000,
                batching => true,
                batch_config => #{
                    max_batch_size => 100,
                    max_batch_bytes => 65536,
                    max_batch_delay_ms => 10,
                    compression => lz4
                },
                circuit_breaker => true,
                pool => #{
                    strategy => adaptive,
                    min_size => 10,
                    max_size => 100
                }
            }}
        ]}
    ]}
].
```

### Step 2: Start Optimized Transport

```erlang
%% Start with optimizations
{ok, TransportPid} = erlmcp_transport_tcp:start_client(#{
    host => "localhost",
    port => 9000,
    batching => true,
    batch_config => #{
        max_batch_size => 100,
        max_batch_bytes => 65536,
        max_batch_delay_ms => 10,
        compression => lz4
    },
    circuit_breaker => true
}).
```

### Step 3: Monitor Performance

```erlang
%% Get batcher stats
{ok, BatchStats} = erlmcp_transport_batcher:get_stats(TransportPid),
io:format("Batches sent: ~p~n", [maps:get(batches_sent, BatchStats)]),
io:format("Messages batched: ~p~n", [maps:get(messages_batched, BatchStats)]).

%% Get circuit breaker state
CircuitState = erlmcp_circuit_breaker:get_state(my_transport),
io:format("Circuit state: ~p~n", [CircuitState#circuit.state]).

%% Get pool stats
{ok, PoolStats} = erlmcp_transport_pool_v2:get_stats(my_pool),
io:format("Pool utilization: ~p/~p~n",
    [maps:get(in_use, PoolStats), maps:get(current_size, PoolStats)]).
```

---

## 7. Key Files Summary

| File | Purpose | LOC | Status |
|------|---------|-----|--------|
| `/home/user/erlmcp/docs/TRANSPORT_OPTIMIZATION_DESIGN.md` | Design document | 1200+ | ✅ Created |
| `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_batcher.erl` | Message batching | 200 | 🔨 To implement |
| `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_transport_pool_v2.erl` | Enhanced pooling | 350 | 🔨 To implement |
| `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_circuit_breaker.erl` | Circuit breaker | 250 | 🔨 To implement |
| `/home/user/erlmcp/apps/erlmcp_transports/src/erlmcp_compression.erl` | Compression | 150 | 🔨 To implement |
| `/home/user/erlmcp/apps/erlmcp_transports/test/erlmcp_transport_bench_SUITE.erl` | Benchmarks | 400 | 🔨 To implement |

---

## 8. Next Steps

1. Review the design document: `/home/user/erlmcp/docs/TRANSPORT_OPTIMIZATION_DESIGN.md`
2. Prioritize implementation phases
3. Set up benchmarking infrastructure
4. Implement Phase 1 (Foundation)
5. Run performance tests and iterate

For questions or clarifications, refer to the main design document or contact the erlmcp development team.
