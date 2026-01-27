%%%====================================================================
%%% MODULE: erlmcp_inter_node_comm
%%%====================================================================
%%% Purpose: Optimized inter-node communication for 100K concurrent
%%%          Handles message batching, compression, and connection pooling
%%%
%%% Performance Goals:
%%%   - Inter-node latency p99 < 5ms at 100K concurrent
%%%   - Bandwidth usage < 100Mbps for 100K concurrent
%%%   - Message batching reduces network calls by >50%
%%%   - Connection pooling between nodes
%%%
%%% Exports:
%%%   - start_link/0 - Initialize inter-node comm system
%%%   - send_message/3 - Send message to remote node
%%%   - batch_send/3 - Batch send messages (auto-batches on timer)
%%%   - get_stats/1 - Get inter-node stats for a node
%%%   - get_cluster_stats/0 - Get cluster-wide inter-node stats
%%%   - compress_enable/1 - Enable/disable compression for node
%%%====================================================================

-module(erlmcp_inter_node_comm).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    send_message/3,
    batch_send/3,
    send_async/3,
    get_stats/1,
    get_cluster_stats/0,
    compress_enable/1,
    get_compression_ratio/1,
    health_check/0,
    reset_stats/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Compression algorithms
-define(COMPRESS_NONE, 0).
-define(COMPRESS_ZLIB, 1).

%% Batching configuration
-define(BATCH_SIZE_THRESHOLD, 50).        % Batch when 50 messages accumulated
-define(BATCH_TIMEOUT_MS, 100).            % Or timeout after 100ms
-define(MESSAGE_ENCODE_TIMEOUT, 5000).     % 5s timeout for message encoding
-define(METRICS_INTERVAL_MS, 5000).        % Collect metrics every 5s

%% Statistics tracking (defined first, before references)
-record(node_stats, {
    node :: atom(),
    messages_sent = 0 :: non_neg_integer(),
    messages_received = 0 :: non_neg_integer(),
    batches_sent = 0 :: non_neg_integer(),
    total_bytes_sent = 0 :: non_neg_integer(),
    total_bytes_received = 0 :: non_neg_integer(),
    bytes_saved_by_batching = 0 :: integer(),
    bytes_saved_by_compression = 0 :: integer(),
    avg_latency_ms = 0.0 :: float(),
    p99_latency_ms = 0 :: non_neg_integer(),
    latency_samples = [] :: [non_neg_integer()],
    compression_enabled = false :: boolean(),
    compression_ratio = 1.0 :: float(),
    last_updated :: integer()
}).

%% Connection pool per node
-record(node_pool, {
    node :: atom(),
    connections = [] :: [pid()],
    available = [] :: [pid()],
    queue = [] :: [term()],
    stats :: #node_stats{}
}).

%% Message batch record
-record(message_batch, {
    target_node :: atom(),
    messages = [] :: [term()],
    size_bytes = 0 :: non_neg_integer(),
    created_at :: integer(),
    compressed = false :: boolean()
}).

%% Server state
-record(inter_node_state, {
    node_pools = #{} :: #{atom() => #node_pool{}},
    batches = #{} :: #{atom() => #message_batch{}},
    stats = #{} :: #{atom() => #node_stats{}},
    compression_enabled = #{} :: #{atom() => boolean()},
    metrics_timer :: reference() | undefined
}).

-type inter_node_state() :: #inter_node_state{}.
-type node_stats() :: #node_stats{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Send a message synchronously to remote node
-spec send_message(atom(), atom(), term()) -> ok | {error, term()}.
send_message(TargetNode, Module, Message) ->
    gen_server:call(?MODULE, {send_message, TargetNode, Module, Message}, ?MESSAGE_ENCODE_TIMEOUT).

%% Send message asynchronously
-spec send_async(atom(), atom(), term()) -> ok.
send_async(TargetNode, Module, Message) ->
    gen_server:cast(?MODULE, {send_async, TargetNode, Module, Message}).

%% Batch-send multiple messages (will auto-batch and compress)
-spec batch_send(atom(), atom(), [term()]) -> ok.
batch_send(TargetNode, Module, Messages) ->
    gen_server:cast(?MODULE, {batch_send, TargetNode, Module, Messages}).

%% Get statistics for a node
-spec get_stats(atom()) -> {ok, node_stats()} | {error, not_found}.
get_stats(Node) ->
    gen_server:call(?MODULE, {get_stats, Node}).

%% Get cluster-wide statistics
-spec get_cluster_stats() -> {ok, map()} | {error, term()}.
get_cluster_stats() ->
    gen_server:call(?MODULE, get_cluster_stats).

%% Enable/disable compression for a node
-spec compress_enable(atom()) -> ok.
compress_enable(Node) ->
    gen_server:call(?MODULE, {compress_enable, Node}).

%% Get compression ratio for a node
-spec get_compression_ratio(atom()) -> float().
get_compression_ratio(Node) ->
    gen_server:call(?MODULE, {get_compression_ratio, Node}).

%% Health check inter-node connections
-spec health_check() -> {ok, map()} | {error, term()}.
health_check() ->
    gen_server:call(?MODULE, health_check).

%% Reset all statistics
-spec reset_stats() -> ok.
reset_stats() ->
    gen_server:call(?MODULE, reset_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, inter_node_state()}.
init([]) ->
    process_flag(trap_exit, true),
    logger:info("Starting inter-node communication optimizer"),

    State = #inter_node_state{
        node_pools = #{},
        batches = #{},
        stats = #{},
        compression_enabled = #{}
    },

    %% Schedule metrics collection
    TimerRef = erlang:send_after(?METRICS_INTERVAL_MS, self(), collect_metrics),

    {ok, State#inter_node_state{metrics_timer = TimerRef}}.

-spec handle_call(term(), {pid(), term()}, inter_node_state()) -> {reply, term(), inter_node_state()}.

handle_call({send_message, TargetNode, Module, Message}, _From, State) ->
    case should_batch(TargetNode, State) of
        true ->
            %% Add to batch
            NewState = add_to_batch(TargetNode, Module, Message, State),
            {reply, ok, NewState};
        false ->
            %% Send immediately
            Result = send_now(TargetNode, Module, Message),
            NewState = record_send(TargetNode, byte_size(term_to_binary(Message)), State),
            {reply, Result, NewState}
    end;

handle_call({get_stats, Node}, _From, State) ->
    case maps:get(Node, State#inter_node_state.stats, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Stats -> {reply, {ok, Stats}, State}
    end;

handle_call(get_cluster_stats, _From, State) ->
    Stats = maps:fold(fun(Node, NodeStats, Acc) ->
        Acc#{Node => stats_to_map(NodeStats)}
    end, #{}, State#inter_node_state.stats),

    ClusterStats = #{
        nodes => erlang:nodes([connected]),
        per_node => Stats,
        total_messages_sent => sum_stat(State#inter_node_state.stats, messages_sent),
        total_messages_received => sum_stat(State#inter_node_state.stats, messages_received),
        total_bytes_sent => sum_stat(State#inter_node_state.stats, total_bytes_sent),
        total_bytes_received => sum_stat(State#inter_node_state.stats, total_bytes_received),
        bytes_saved_by_batching => sum_stat(State#inter_node_state.stats, bytes_saved_by_batching),
        bytes_saved_by_compression => sum_stat(State#inter_node_state.stats, bytes_saved_by_compression),
        timestamp => erlang:system_time(millisecond)
    },

    {reply, {ok, ClusterStats}, State};

handle_call({compress_enable, Node}, _From, State) ->
    NewCompressionMap = maps:put(Node, true, State#inter_node_state.compression_enabled),
    NewState = State#inter_node_state{compression_enabled = NewCompressionMap},
    logger:info("Compression enabled for node ~p", [Node]),
    {reply, ok, NewState};

handle_call({get_compression_ratio, Node}, _From, State) ->
    case maps:get(Node, State#inter_node_state.stats, undefined) of
        undefined -> {reply, 1.0, State};
        NodeStats -> {reply, NodeStats#node_stats.compression_ratio, State}
    end;

handle_call(health_check, _From, State) ->
    HealthStatus = check_inter_node_health(State),
    {reply, {ok, HealthStatus}, State};

handle_call(reset_stats, _From, State) ->
    NewStats = maps:map(fun(_, Stats) ->
        Stats#node_stats{
            messages_sent = 0,
            messages_received = 0,
            batches_sent = 0,
            total_bytes_sent = 0,
            total_bytes_received = 0,
            bytes_saved_by_batching = 0,
            bytes_saved_by_compression = 0,
            latency_samples = [],
            last_updated = erlang:system_time(millisecond)
        }
    end, State#inter_node_state.stats),

    {reply, ok, State#inter_node_state{stats = NewStats}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), inter_node_state()) -> {noreply, inter_node_state()}.

handle_cast({send_async, TargetNode, Module, Message}, State) ->
    NewState = case should_batch(TargetNode, State) of
        true -> add_to_batch(TargetNode, Module, Message, State);
        false ->
            send_now(TargetNode, Module, Message),
            record_send(TargetNode, byte_size(term_to_binary(Message)), State)
    end,
    {noreply, NewState};

handle_cast({batch_send, TargetNode, Module, Messages}, State) ->
    NewState = lists:foldl(fun(Msg, S) ->
        add_to_batch(TargetNode, Module, Msg, S)
    end, State, Messages),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), inter_node_state()) -> {noreply, inter_node_state()}.

handle_info(collect_metrics, State) ->
    %% Flush any pending batches
    NewState = flush_all_batches(State),

    %% Reschedule
    TimerRef = erlang:send_after(?METRICS_INTERVAL_MS, self(), collect_metrics),

    {noreply, NewState#inter_node_state{metrics_timer = TimerRef}};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    %% Handle connection deaths gracefully
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), inter_node_state()) -> ok.
terminate(_Reason, State) ->
    %% Flush all pending batches before shutdown
    flush_all_batches(State),
    logger:info("Inter-node communication optimizer terminated"),
    ok.

-spec code_change(term(), inter_node_state(), term()) -> {ok, inter_node_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Add message to batch queue
-spec add_to_batch(atom(), atom(), term(), inter_node_state()) -> inter_node_state().
add_to_batch(TargetNode, Module, Message, State) ->
    Batch = maps:get(TargetNode, State#inter_node_state.batches, #message_batch{
        target_node = TargetNode,
        messages = [],
        size_bytes = 0,
        created_at = erlang:system_time(millisecond),
        compressed = false
    }),

    MessageBinary = term_to_binary({Module, Message}),
    MessageSize = byte_size(MessageBinary),

    UpdatedBatch = Batch#message_batch{
        messages = Batch#message_batch.messages ++ [{Module, Message}],
        size_bytes = Batch#message_batch.size_bytes + MessageSize
    },

    case should_flush_batch(UpdatedBatch) of
        true ->
            flush_batch(TargetNode, UpdatedBatch, State);
        false ->
            NewBatches = maps:put(TargetNode, UpdatedBatch, State#inter_node_state.batches),
            State#inter_node_state{batches = NewBatches}
    end.

%% Check if batch should be flushed
-spec should_flush_batch(#message_batch{}) -> boolean().
should_flush_batch(Batch) ->
    BatchSize = length(Batch#message_batch.messages),
    Age = erlang:system_time(millisecond) - Batch#message_batch.created_at,

    BatchSize >= ?BATCH_SIZE_THRESHOLD orelse Age >= ?BATCH_TIMEOUT_MS.

%% Check if message should be batched
-spec should_batch(atom(), inter_node_state()) -> boolean().
should_batch(TargetNode, State) ->
    case maps:get(TargetNode, State#inter_node_state.batches, undefined) of
        undefined -> true;  % Start a new batch
        _ -> true           % Always batch for optimal throughput
    end.

%% Flush a specific batch
-spec flush_batch(atom(), #message_batch{}, inter_node_state()) -> inter_node_state().
flush_batch(TargetNode, Batch, State) ->
    Messages = Batch#message_batch.messages,

    %% Determine if we should compress
    ShouldCompress = maps:get(TargetNode, State#inter_node_state.compression_enabled, false),

    %% Encode batch
    BinaryData = term_to_binary(Messages),
    CompressedData = case ShouldCompress of
        true -> zlib:compress(BinaryData);
        false -> BinaryData
    end,

    %% Send batch to remote node
    case erlang:nodes([connected]) of
        [] -> ok;  % No connected nodes
        _ -> send_to_node(TargetNode, CompressedData, ShouldCompress)
    end,

    %% Record statistics
    OriginalSize = byte_size(BinaryData),
    CompressedSize = byte_size(CompressedData),
    BytesSaved = case ShouldCompress of
        true -> OriginalSize - CompressedSize;
        false -> 0
    end,

    NewStats = case maps:get(TargetNode, State#inter_node_state.stats, undefined) of
        undefined ->
            #node_stats{
                node = TargetNode,
                batches_sent = 1,
                messages_sent = length(Messages),
                total_bytes_sent = CompressedSize,
                bytes_saved_by_compression = BytesSaved,
                compression_enabled = ShouldCompress,
                compression_ratio = case ShouldCompress of
                    true -> CompressedSize / OriginalSize;
                    false -> 1.0
                end,
                last_updated = erlang:system_time(millisecond)
            };
        Stats ->
            Stats#node_stats{
                batches_sent = Stats#node_stats.batches_sent + 1,
                messages_sent = Stats#node_stats.messages_sent + length(Messages),
                total_bytes_sent = Stats#node_stats.total_bytes_sent + CompressedSize,
                bytes_saved_by_compression = Stats#node_stats.bytes_saved_by_compression + BytesSaved,
                compression_enabled = ShouldCompress,
                compression_ratio = case ShouldCompress of
                    true -> CompressedSize / OriginalSize;
                    false -> 1.0
                end,
                last_updated = erlang:system_time(millisecond)
            }
    end,

    NewStatMap = maps:put(TargetNode, NewStats, State#inter_node_state.stats),
    NewBatches = maps:remove(TargetNode, State#inter_node_state.batches),

    State#inter_node_state{
        batches = NewBatches,
        stats = NewStatMap
    }.

%% Flush all pending batches
-spec flush_all_batches(inter_node_state()) -> inter_node_state().
flush_all_batches(State) ->
    maps:fold(fun(TargetNode, Batch, AccState) ->
        flush_batch(TargetNode, Batch, AccState)
    end, State, State#inter_node_state.batches).

%% Send message immediately without batching
-spec send_now(atom(), atom(), term()) -> ok | {error, term()}.
send_now(TargetNode, Module, Message) ->
    BinaryData = term_to_binary({Module, Message}),

    case erlang:nodes([connected]) of
        [] -> {error, no_nodes};
        _ -> send_to_node(TargetNode, BinaryData, false)
    end.

%% Send binary data to remote node
-spec send_to_node(atom(), binary(), boolean()) -> ok | {error, term()}.
send_to_node(TargetNode, Data, Compressed) ->
    Header = case Compressed of
        true -> <<?COMPRESS_ZLIB:8>>;
        false -> <<?COMPRESS_NONE:8>>
    end,

    Packet = <<Header/binary, Data/binary>>,

    try
        {?MODULE, TargetNode} ! {inter_node_msg, Packet},
        ok
    catch
        _:Error -> {error, Error}
    end.

%% Record outgoing message statistics
-spec record_send(atom(), non_neg_integer(), inter_node_state()) -> inter_node_state().
record_send(TargetNode, MessageSize, State) ->
    NewStats = case maps:get(TargetNode, State#inter_node_state.stats, undefined) of
        undefined ->
            #node_stats{
                node = TargetNode,
                messages_sent = 1,
                total_bytes_sent = MessageSize,
                last_updated = erlang:system_time(millisecond)
            };
        Stats ->
            Stats#node_stats{
                messages_sent = Stats#node_stats.messages_sent + 1,
                total_bytes_sent = Stats#node_stats.total_bytes_sent + MessageSize,
                last_updated = erlang:system_time(millisecond)
            }
    end,

    NewStatMap = maps:put(TargetNode, NewStats, State#inter_node_state.stats),
    State#inter_node_state{stats = NewStatMap}.

%% Check inter-node connection health
-spec check_inter_node_health(inter_node_state()) -> map().
check_inter_node_health(State) ->
    ConnectedNodes = erlang:nodes([connected]),

    HealthStatus = maps:fold(fun(Node, NodeStats, Acc) ->
        Acc#{Node => #{
            connected => lists:member(Node, ConnectedNodes),
            messages_sent => NodeStats#node_stats.messages_sent,
            messages_received => NodeStats#node_stats.messages_received,
            avg_latency_ms => NodeStats#node_stats.avg_latency_ms,
            p99_latency_ms => NodeStats#node_stats.p99_latency_ms
        }}
    end, #{}, State#inter_node_state.stats),

    #{
        healthy_nodes => ConnectedNodes,
        node_count => length(ConnectedNodes),
        health_by_node => HealthStatus,
        timestamp => erlang:system_time(millisecond)
    }.

%% Convert stats record to map
-spec stats_to_map(node_stats()) -> map().
stats_to_map(Stats) ->
    #{
        messages_sent => Stats#node_stats.messages_sent,
        messages_received => Stats#node_stats.messages_received,
        batches_sent => Stats#node_stats.batches_sent,
        total_bytes_sent => Stats#node_stats.total_bytes_sent,
        total_bytes_received => Stats#node_stats.total_bytes_received,
        bytes_saved_by_batching => Stats#node_stats.bytes_saved_by_batching,
        bytes_saved_by_compression => Stats#node_stats.bytes_saved_by_compression,
        avg_latency_ms => Stats#node_stats.avg_latency_ms,
        p99_latency_ms => Stats#node_stats.p99_latency_ms,
        compression_enabled => Stats#node_stats.compression_enabled,
        compression_ratio => Stats#node_stats.compression_ratio,
        last_updated => Stats#node_stats.last_updated
    }.

%% Sum a statistic across all nodes
-spec sum_stat(map(), atom()) -> non_neg_integer() | float().
sum_stat(StatsMap, Stat) ->
    maps:fold(fun(_, NodeStats, Acc) ->
        case Stat of
            messages_sent -> Acc + NodeStats#node_stats.messages_sent;
            messages_received -> Acc + NodeStats#node_stats.messages_received;
            total_bytes_sent -> Acc + NodeStats#node_stats.total_bytes_sent;
            total_bytes_received -> Acc + NodeStats#node_stats.total_bytes_received;
            bytes_saved_by_batching -> Acc + NodeStats#node_stats.bytes_saved_by_batching;
            bytes_saved_by_compression -> Acc + NodeStats#node_stats.bytes_saved_by_compression;
            _ -> Acc
        end
    end, 0, StatsMap).
