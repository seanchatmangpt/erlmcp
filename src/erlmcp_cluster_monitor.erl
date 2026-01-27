%%%====================================================================
%%% MODULE: erlmcp_cluster_monitor
%%%====================================================================
%%% Purpose: Monitor cluster health, inter-node communication, and
%%%          concurrent connection metrics across all nodes
%%%
%%% Exports:
%%%   - get_cluster_status/0 - Current cluster state and node info
%%%   - get_node_connections/1 - Connection count for specific node
%%%   - get_global_connections/0 - Total connections across cluster
%%%   - get_cluster_throughput/0 - Messages/sec across cluster
%%%   - get_latency_stats/0 - P50/P95/P99 latency stats
%%%====================================================================

-module(erlmcp_cluster_monitor).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    stop/0,
    get_cluster_status/0,
    get_node_connections/1,
    get_global_connections/0,
    get_cluster_throughput/0,
    get_latency_stats/0,
    get_node_latency/1,
    record_message/2,
    record_connection/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("erlmcp.hrl").

%% Monitor state (lightweight, separate from server state)
-record(monitor_state, {
    start_time :: integer(),              % System start time (ms)
    connections :: #{atom() => integer()}, % Connections per node
    messages :: #{atom() => integer()},    % Messages per node
    latencies :: #{atom() => list()},      % Latency samples per node
    last_sync :: integer()                 % Last inter-node sync (ms)
}).

-define(SYNC_INTERVAL, 5000).      % Sync metrics every 5 seconds
-define(MAX_LATENCY_SAMPLES, 1000). % Keep last 1000 samples

%%%====================================================================
%%% API
%%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).

%% Get current cluster status
-spec get_cluster_status() -> {ok, map()} | {error, term()}.
get_cluster_status() ->
    gen_server:call(?MODULE, get_cluster_status).

%% Get connection count for a node
-spec get_node_connections(atom()) -> non_neg_integer().
get_node_connections(Node) ->
    gen_server:call(?MODULE, {get_node_connections, Node}).

%% Get total connections across cluster
-spec get_global_connections() -> non_neg_integer().
get_global_connections() ->
    gen_server:call(?MODULE, get_global_connections).

%% Get cluster-wide throughput (messages/sec)
-spec get_cluster_throughput() -> float().
get_cluster_throughput() ->
    gen_server:call(?MODULE, get_cluster_throughput).

%% Get latency statistics
-spec get_latency_stats() -> map().
get_latency_stats() ->
    gen_server:call(?MODULE, get_latency_stats).

%% Get latency stats for specific node
-spec get_node_latency(atom()) -> map().
get_node_latency(Node) ->
    gen_server:call(?MODULE, {get_node_latency, Node}).

%% Record a message event
-spec record_message(atom(), integer()) -> ok.
record_message(Node, LatencyMs) ->
    gen_server:cast(?MODULE, {record_message, Node, LatencyMs}).

%% Record a connection event
-spec record_connection(atom(), integer()) -> ok.
record_connection(Node, 1) ->
    gen_server:cast(?MODULE, {add_connection, Node});
record_connection(Node, -1) ->
    gen_server:cast(?MODULE, {remove_connection, Node}).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

init([]) ->
    %% Initialize metrics state
    State = #monitor_state{
        start_time = erlang:system_time(millisecond),
        connections = #{},
        messages = #{},
        latencies = #{},
        last_sync = erlang:system_time(millisecond)
    },

    %% Schedule periodic sync of metrics across nodes
    erlang:send_after(?SYNC_INTERVAL, self(), sync_metrics),

    {ok, State}.

handle_call(get_cluster_status, _From, State) ->
    Status = cluster_status_map(State),
    {reply, {ok, Status}, State};

handle_call({get_node_connections, Node}, _From, State) ->
    Connections = maps:get(connections, State, #{}),
    Count = maps:get(Node, Connections, 0),
    {reply, Count, State};

handle_call(get_global_connections, _From, State) ->
    Connections = maps:get(connections, State, #{}),
    Total = maps:fold(fun(_, Count, Acc) -> Acc + Count end, 0, Connections),
    {reply, Total, State};

handle_call(get_cluster_throughput, _From, State) ->
    Messages = maps:get(messages, State, #{}),
    Total = maps:fold(fun(_, Count, Acc) -> Acc + Count end, 0, Messages),
    Uptime = erlang:system_time(millisecond) - State#monitor_state.start_time,
    UptimeSec = max(1, Uptime div 1000),
    Throughput = Total / UptimeSec,
    {reply, Throughput, State};

handle_call(get_latency_stats, _From, State) ->
    Stats = compute_latency_stats(State),
    {reply, Stats, State};

handle_call({get_node_latency, Node}, _From, State) ->
    Latencies = maps:get(latencies, State, #{}),
    Samples = maps:get(Node, Latencies, []),
    Stats = compute_node_latency_stats(Samples),
    {reply, Stats, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({add_connection, Node}, State) ->
    Connections = maps:get(connections, State, #{}),
    Current = maps:get(Node, Connections, 0),
    NewConnections = Connections#{Node => Current + 1},
    {noreply, State#monitor_state{connections = NewConnections}};

handle_cast({remove_connection, Node}, State) ->
    Connections = maps:get(connections, State, #{}),
    Current = maps:get(Node, Connections, 0),
    NewCount = max(0, Current - 1),
    NewConnections = Connections#{Node => NewCount},
    {noreply, State#monitor_state{connections = NewConnections}};

handle_cast({record_message, Node, LatencyMs}, State) ->
    Messages = maps:get(messages, State, #{}),
    MessageCount = maps:get(Node, Messages, 0),
    NewMessages = Messages#{Node => MessageCount + 1},

    Latencies = maps:get(latencies, State, #{}),
    CurrentSamples = maps:get(Node, Latencies, []),
    NewSamples = trim_samples([LatencyMs | CurrentSamples]),
    NewLatencies = Latencies#{Node => NewSamples},

    {noreply, State#monitor_state{
        messages = NewMessages,
        latencies = NewLatencies
    }}.

handle_info(sync_metrics, State) ->
    %% Synchronize metrics across cluster nodes
    sync_cluster_metrics(State),
    %% Schedule next sync
    erlang:send_after(?SYNC_INTERVAL, self(), sync_metrics),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% Convert state to cluster status map
cluster_status_map(State) ->
    Connections = maps:get(connections, State, #{}),
    Messages = maps:get(messages, State, #{}),
    Uptime = erlang:system_time(millisecond) - State#monitor_state.start_time,

    TotalConnections = maps:fold(fun(_, Count, Acc) -> Acc + Count end, 0, Connections),
    TotalMessages = maps:fold(fun(_, Count, Acc) -> Acc + Count end, 0, Messages),

    #{
        nodes => erlang:nodes([connected]),
        total_connections => TotalConnections,
        total_messages => TotalMessages,
        uptime_seconds => Uptime div 1000,
        uptime_ms => Uptime,
        node_details => node_details_map(Connections, Messages),
        timestamp => erlang:system_time(millisecond)
    }.

%% Create node details map
node_details_map(Connections, Messages) ->
    AllNodes = [node() | erlang:nodes([connected])],
    lists:foldl(fun(Node, Acc) ->
        Conns = maps:get(Node, Connections, 0),
        Msgs = maps:get(Node, Messages, 0),
        Acc#{Node => #{
            connections => Conns,
            messages => Msgs
        }}
    end, #{}, AllNodes).

%% Compute latency statistics
compute_latency_stats(State) ->
    Latencies = maps:get(latencies, State, #{}),
    AllSamples = maps:fold(fun(_, Samples, Acc) -> Samples ++ Acc end, [], Latencies),

    case AllSamples of
        [] ->
            #{
                avg => 0.0,
                min => 0,
                max => 0,
                p50 => 0,
                p95 => 0,
                p99 => 0,
                count => 0
            };
        _ ->
            Sorted = lists:sort(AllSamples),
            Count = length(Sorted),
            Avg = lists:sum(Sorted) / Count,
            Min = lists:min(Sorted),
            Max = lists:max(Sorted),
            P50 = percentile(Sorted, 0.50),
            P95 = percentile(Sorted, 0.95),
            P99 = percentile(Sorted, 0.99),

            #{
                avg => Avg,
                min => Min,
                max => Max,
                p50 => P50,
                p95 => P95,
                p99 => P99,
                count => Count
            }
    end.

%% Compute per-node latency statistics
compute_node_latency_stats(Samples) ->
    case Samples of
        [] ->
            #{
                avg => 0.0,
                min => 0,
                max => 0,
                p50 => 0,
                p95 => 0,
                p99 => 0,
                count => 0
            };
        _ ->
            Sorted = lists:sort(Samples),
            Count = length(Sorted),
            Avg = lists:sum(Sorted) / Count,
            Min = lists:min(Sorted),
            Max = lists:max(Sorted),
            P50 = percentile(Sorted, 0.50),
            P95 = percentile(Sorted, 0.95),
            P99 = percentile(Sorted, 0.99),

            #{
                avg => Avg,
                min => Min,
                max => Max,
                p50 => P50,
                p95 => P95,
                p99 => P99,
                count => Count
            }
    end.

%% Calculate percentile from sorted list
percentile(Sorted, Percent) when is_list(Sorted), Percent >= 0.0, Percent =< 1.0 ->
    Len = length(Sorted),
    Index = max(1, round(Len * Percent)),
    lists:nth(Index, Sorted).

%% Trim latency samples to keep memory bounded
trim_samples(Samples) when length(Samples) > ?MAX_LATENCY_SAMPLES ->
    lists:sublist(Samples, ?MAX_LATENCY_SAMPLES);
trim_samples(Samples) ->
    Samples.

%% Synchronize metrics across cluster nodes
sync_cluster_metrics(State) ->
    %% This is called periodically to sync metrics
    %% In a production system, this would broadcast metrics to other nodes
    case erlang:nodes([connected]) of
        [] -> ok;  % No other nodes
        _ConnectedNodes ->
            %% For now, just log cluster status
            Status = cluster_status_map(State),
            logger:debug("Cluster sync: ~p", [Status])
    end.
