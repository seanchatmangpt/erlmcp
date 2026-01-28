%%%-------------------------------------------------------------------
%%% @doc
%%% Memory Accounting Module - Eliminates "MiB/conn" Ambiguity
%%%
%%% This module provides detailed memory decomposition to prevent ambiguous
%%% memory reporting. It breaks down "memory per connection" into clear
%%% components:
%%%
%%% - Per-connection process heap (gen_server process itself)
%%% - Per-connection state data (state record + buffers + queues)
%%% - Per-node base overhead (supervisors, registry, ETS tables)
%%% - Per-node total RSS (operating system perspective)
%%% - Cluster total (aggregate across all nodes)
%%%
%%% All memory reports MUST use decompose/1 to provide component breakdown.
%%% Reports containing "MiB/conn" without decomposition will be rejected.
%%%
%%% Usage:
%%%   %% Measure single connection
%%%   Decomposition = erlmcp_memory_accounting:decompose(#{
%%%       connection_pids => [Pid1, Pid2, ...],
%%%       server_pid => ServerPid,
%%%       registry_pid => RegistryPid
%%%   }),
%%%
%%%   %% Verify components sum correctly
%%%   ok = erlmcp_memory_accounting:validate_decomposition(Decomposition),
%%%
%%%   %% Get human-readable report
%%%   Report = erlmcp_memory_accounting:format_report(Decomposition).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_accounting).

-export([
    % Core measurement functions
    measure_per_connection_heap/1,
    measure_per_connection_state/1,
    measure_per_node_rss/0,
    measure_per_node_base_overhead/1,
    measure_cluster_total/1,
    
    % Decomposition and validation
    decompose/1,
    validate_decomposition/1,
    
    % Reporting
    format_report/1,
    format_compact/1,
    
    % Utility functions
    get_connection_process_info/1,
    estimate_state_size/1,
    aggregate_measurements/1
]).

-include_lib("kernel/include/logger.hrl").

-type bytes() :: non_neg_integer().
-type mib() :: float().
-type connection_count() :: pos_integer().
-type node_count() :: pos_integer().

-record(memory_decomposition, {
    % Per-connection measurements (average per connection)
    per_connection_heap_mib :: mib(),
    per_connection_state_mib :: mib(),
    per_connection_total_mib :: mib(),
    
    % Per-node measurements (single node)
    per_node_base_overhead_mib :: mib(),
    per_node_total_rss_mib :: mib(),
    per_node_process_count :: non_neg_integer(),
    
    % Cluster measurements (all nodes)
    connections :: connection_count(),
    cluster_nodes :: node_count(),
    cluster_total_rss_mib :: mib(),
    
    % Scope indicators
    scope_per_node :: boolean(),
    scope_per_cluster :: boolean(),
    
    % Metadata
    timestamp :: integer(),
    node :: node(),
    measured_pids :: [pid()]
}).

-type memory_decomposition() :: #memory_decomposition{}.

-export_type([memory_decomposition/0, bytes/0, mib/0]).

%%====================================================================
%% Core Measurement Functions
%%====================================================================

%% @doc Measure process heap size for a connection gen_server
%% This measures the actual Erlang process memory, including:
%% - Stack
%% - Heap
%% - Old heap
%% - Message queue
-spec measure_per_connection_heap(pid()) -> {ok, bytes()} | {error, term()}.
measure_per_connection_heap(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid, memory) of
        {memory, Bytes} ->
            {ok, Bytes};
        undefined ->
            {error, process_not_alive};
        Error ->
            {error, Error}
    end.

%% @doc Measure per-connection state data size
%% This measures the state record + queues + buffers using erts_debug:flat_size
%% which gives the total size of all data referenced by the state term.
-spec measure_per_connection_state(pid()) -> {ok, bytes()} | {error, term()}.
measure_per_connection_state(Pid) when is_pid(Pid) ->
    try
        % Get the current state from the gen_server
        % Note: This requires the process to be a gen_server
        case sys:get_state(Pid, 5000) of
            State when is_tuple(State); is_map(State) ->
                % Calculate flat size (includes all referenced data)
                Words = erts_debug:flat_size(State),
                Bytes = Words * erlang:system_info(wordsize),
                {ok, Bytes};
            State ->
                % For other data types, still calculate size
                Words = erts_debug:flat_size(State),
                Bytes = Words * erlang:system_info(wordsize),
                {ok, Bytes}
        end
    catch
        _:Reason ->
            ?LOG_WARNING("Failed to get state for pid ~p: ~p", [Pid, Reason]),
            {error, {state_access_failed, Reason}}
    end.

%% @doc Measure per-node RSS from OS perspective
%% Returns total resident memory for this Erlang node
-spec measure_per_node_rss() -> {ok, bytes()}.
measure_per_node_rss() ->
    % erlang:memory(total) gives the total amount of memory currently allocated
    % This includes all processes, ETS tables, atoms, code, etc.
    TotalBytes = erlang:memory(total),
    {ok, TotalBytes}.

%% @doc Measure per-node base overhead (supervisors, registry, ETS, etc.)
%% This is the memory used by infrastructure components, not connection processes
-spec measure_per_node_base_overhead(#{
    server_pid => pid() | undefined,
    registry_pid => pid() | undefined,
    supervisor_pids => [pid()]
}) -> {ok, bytes()}.
measure_per_node_base_overhead(Components) ->
    ServerPid = maps:get(server_pid, Components, undefined),
    RegistryPid = maps:get(registry_pid, Components, undefined),
    SupervisorPids = maps:get(supervisor_pids, Components, []),
    
    % Collect memory for infrastructure components
    Measurements = [
        measure_component(ServerPid, <<"server">>),
        measure_component(RegistryPid, <<"registry">>)
    ] ++ [measure_component(P, <<"supervisor">>) || P <- SupervisorPids],
    
    % Add ETS table overhead
    EtsMemory = erlang:memory(ets),
    
    % Add atom table overhead
    AtomMemory = erlang:memory(atom),
    
    % Add code memory
    CodeMemory = erlang:memory(code),
    
    % Sum all infrastructure memory
    ProcessOverhead = lists:sum([M || {ok, M} <- Measurements]),
    TotalOverhead = ProcessOverhead + EtsMemory + AtomMemory + CodeMemory,
    
    {ok, TotalOverhead}.

%% @doc Measure cluster total memory across all nodes
-spec measure_cluster_total([node()]) -> {ok, bytes()}.
measure_cluster_total(Nodes) when is_list(Nodes) ->
    % Collect RSS from all nodes
    Results = lists:map(
        fun(Node) ->
            case rpc:call(Node, erlmcp_memory_accounting, measure_per_node_rss, [], 5000) of
                {ok, Bytes} -> Bytes;
                {badrpc, Reason} ->
                    ?LOG_ERROR("Failed to get memory from node ~p: ~p", [Node, Reason]),
                    0
            end
        end,
        Nodes
    ),
    
    TotalBytes = lists:sum(Results),
    {ok, TotalBytes}.

%%====================================================================
%% Decomposition and Validation
%%====================================================================

%% @doc Decompose memory into all components with proper labeling
%% This is the main function that should be used for all memory reports.
-spec decompose(#{
    connection_pids := [pid()],
    server_pid => pid(),
    registry_pid => pid(),
    supervisor_pids => [pid()],
    cluster_nodes => [node()]
}) -> memory_decomposition().
decompose(Options) ->
    ConnectionPids = maps:get(connection_pids, Options),
    ServerPid = maps:get(server_pid, Options, undefined),
    RegistryPid = maps:get(registry_pid, Options, undefined),
    SupervisorPids = maps:get(supervisor_pids, Options, []),
    ClusterNodes = maps:get(cluster_nodes, Options, [node()]),
    
    ConnectionCount = length(ConnectionPids),
    
    % Measure per-connection metrics (average across all connections)
    {AvgHeapBytes, AvgStateBytes} = case ConnectionPids of
        [] -> {0, 0};
        _ -> measure_average_connection_memory(ConnectionPids)
    end,
    
    % Measure per-node metrics
    {ok, NodeRssBytes} = measure_per_node_rss(),
    {ok, BaseOverheadBytes} = measure_per_node_base_overhead(#{
        server_pid => ServerPid,
        registry_pid => RegistryPid,
        supervisor_pids => SupervisorPids
    }),
    
    % Measure cluster metrics
    {ok, ClusterTotalBytes} = measure_cluster_total(ClusterNodes),
    
    #memory_decomposition{
        per_connection_heap_mib = bytes_to_mib(AvgHeapBytes),
        per_connection_state_mib = bytes_to_mib(AvgStateBytes),
        per_connection_total_mib = bytes_to_mib(AvgHeapBytes + AvgStateBytes),
        
        per_node_base_overhead_mib = bytes_to_mib(BaseOverheadBytes),
        per_node_total_rss_mib = bytes_to_mib(NodeRssBytes),
        per_node_process_count = erlang:system_info(process_count),
        
        connections = ConnectionCount,
        cluster_nodes = length(ClusterNodes),
        cluster_total_rss_mib = bytes_to_mib(ClusterTotalBytes),
        
        scope_per_node = true,
        scope_per_cluster = length(ClusterNodes) > 1,
        
        timestamp = erlang:system_time(millisecond),
        node = node(),
        measured_pids = ConnectionPids
    }.

%% @doc Validate that memory components sum correctly
%% Returns ok if decomposition is valid, {error, Reason} otherwise
-spec validate_decomposition(memory_decomposition()) -> ok | {error, term()}.
validate_decomposition(#memory_decomposition{
    per_connection_heap_mib = HeapMiB,
    per_connection_state_mib = StateMiB,
    per_connection_total_mib = TotalMiB,
    per_node_base_overhead_mib = OverheadMiB,
    per_node_total_rss_mib = RssMiB,
    connections = ConnCount,
    cluster_nodes = NodeCount,
    cluster_total_rss_mib = ClusterMiB
}) ->
    % Check 1: Per-connection total should equal heap + state
    PerConnCheck = abs(TotalMiB - (HeapMiB + StateMiB)) < 0.001,
    
    % Check 2: Per-node RSS should be >= base overhead + (connections * per_conn_total)
    ExpectedMinRss = OverheadMiB + (ConnCount * TotalMiB),
    RssCheck = RssMiB >= (ExpectedMinRss * 0.9),  % Allow 10% variance
    
    % Check 3: Cluster total should be >= per-node RSS * node count (for single node)
    ClusterCheck = case NodeCount of
        1 -> abs(ClusterMiB - RssMiB) < (RssMiB * 0.1);  % 10% tolerance
        _ -> ClusterMiB >= RssMiB  % Multi-node should be at least node RSS
    end,
    
    % Check 4: All values should be non-negative
    NonNegativeCheck = HeapMiB >= 0 andalso StateMiB >= 0 andalso 
                       TotalMiB >= 0 andalso OverheadMiB >= 0 andalso
                       RssMiB >= 0 andalso ClusterMiB >= 0,
    
    case {PerConnCheck, RssCheck, ClusterCheck, NonNegativeCheck} of
        {true, true, true, true} ->
            ok;
        {false, _, _, _} ->
            {error, {invalid_per_connection_sum, #{
                heap => HeapMiB,
                state => StateMiB,
                total => TotalMiB,
                expected => HeapMiB + StateMiB
            }}};
        {_, false, _, _} ->
            {error, {invalid_node_rss, #{
                rss => RssMiB,
                expected_min => ExpectedMinRss,
                overhead => OverheadMiB,
                connections => ConnCount,
                per_conn => TotalMiB
            }}};
        {_, _, false, _} ->
            {error, {invalid_cluster_total, #{
                cluster_total => ClusterMiB,
                node_rss => RssMiB,
                node_count => NodeCount
            }}};
        {_, _, _, false} ->
            {error, negative_values_detected}
    end.

%%====================================================================
%% Reporting Functions
%%====================================================================

%% @doc Format decomposition as human-readable report
-spec format_report(memory_decomposition()) -> binary().
format_report(#memory_decomposition{
    per_connection_heap_mib = HeapMiB,
    per_connection_state_mib = StateMiB,
    per_connection_total_mib = TotalMiB,
    per_node_base_overhead_mib = OverheadMiB,
    per_node_total_rss_mib = RssMiB,
    per_node_process_count = ProcCount,
    connections = ConnCount,
    cluster_nodes = NodeCount,
    cluster_total_rss_mib = ClusterMiB,
    scope_per_node = PerNode,
    scope_per_cluster = PerCluster,
    timestamp = Timestamp,
    node = Node
}) ->
    Lines = [
        <<"=== MEMORY DECOMPOSITION REPORT ===\n">>,
        io_lib:format("Timestamp: ~p~n", [Timestamp]),
        io_lib:format("Node: ~p~n", [Node]),
        <<"\n">>,
        <<"--- Per-Connection Memory (Average) ---\n">>,
        io_lib:format("  Heap (process memory):     ~.3f MiB~n", [HeapMiB]),
        io_lib:format("  State (data structures):   ~.3f MiB~n", [StateMiB]),
        io_lib:format("  Total per connection:      ~.3f MiB~n", [TotalMiB]),
        <<"\n">>,
        <<"--- Per-Node Memory ---\n">>,
        io_lib:format("  Base overhead (infra):     ~.3f MiB~n", [OverheadMiB]),
        io_lib:format("  Total RSS (OS view):       ~.3f MiB~n", [RssMiB]),
        io_lib:format("  Process count:             ~B~n", [ProcCount]),
        io_lib:format("  Active connections:        ~B~n", [ConnCount]),
        <<"\n">>,
        <<"--- Cluster Memory ---\n">>,
        io_lib:format("  Cluster nodes:             ~B~n", [NodeCount]),
        io_lib:format("  Cluster total RSS:         ~.3f MiB~n", [ClusterMiB]),
        <<"\n">>,
        <<"--- Scope Indicators ---\n">>,
        io_lib:format("  Per-node scope:            ~p~n", [PerNode]),
        io_lib:format("  Per-cluster scope:         ~p~n", [PerCluster]),
        <<"\n">>,
        <<"--- Calculations ---\n">>,
        io_lib:format("  Expected node memory:      ~.3f MiB (overhead + conn*total)~n",
                     [OverheadMiB + (ConnCount * TotalMiB)]),
        io_lib:format("  Actual node RSS:           ~.3f MiB~n", [RssMiB]),
        io_lib:format("  Variance:                  ~.3f MiB~n",
                     [RssMiB - (OverheadMiB + (ConnCount * TotalMiB))]),
        <<"=== END REPORT ===\n">>
    ],
    iolist_to_binary(Lines).

%% @doc Format decomposition as compact map for benchmarks
-spec format_compact(memory_decomposition()) -> map().
format_compact(#memory_decomposition{
    per_connection_heap_mib = HeapMiB,
    per_connection_state_mib = StateMiB,
    per_connection_total_mib = TotalMiB,
    per_node_base_overhead_mib = OverheadMiB,
    per_node_total_rss_mib = RssMiB,
    connections = ConnCount,
    cluster_nodes = NodeCount,
    cluster_total_rss_mib = ClusterMiB,
    scope_per_node = PerNode,
    scope_per_cluster = PerCluster
}) ->
    #{
        per_connection_heap_mib => HeapMiB,
        per_connection_state_mib => StateMiB,
        per_connection_total_mib => TotalMiB,
        per_node_base_overhead_mib => OverheadMiB,
        per_node_total_rss_mib => RssMiB,
        connections => ConnCount,
        cluster_nodes => NodeCount,
        cluster_total_rss_mib => ClusterMiB,
        scope_per_node => PerNode,
        scope_per_cluster => PerCluster
    }.

%%====================================================================
%% Utility Functions
%%====================================================================

%% @doc Get detailed process info for a connection
-spec get_connection_process_info(pid()) -> map().
get_connection_process_info(Pid) when is_pid(Pid) ->
    Info = [
        {memory, erlang:process_info(Pid, memory)},
        {heap_size, erlang:process_info(Pid, heap_size)},
        {total_heap_size, erlang:process_info(Pid, total_heap_size)},
        {stack_size, erlang:process_info(Pid, stack_size)},
        {message_queue_len, erlang:process_info(Pid, message_queue_len)},
        {reductions, erlang:process_info(Pid, reductions)}
    ],
    
    maps:from_list([{K, V} || {K, {K, V}} <- Info]).

%% @doc Estimate state size for any term
-spec estimate_state_size(term()) -> bytes().
estimate_state_size(State) ->
    Words = erts_debug:flat_size(State),
    Words * erlang:system_info(wordsize).

%% @doc Aggregate multiple measurements into summary statistics
-spec aggregate_measurements([memory_decomposition()]) -> map().
aggregate_measurements([]) ->
    #{error => no_measurements};
aggregate_measurements(Decompositions) ->
    HeapMiBs = [D#memory_decomposition.per_connection_heap_mib || D <- Decompositions],
    StateMiBs = [D#memory_decomposition.per_connection_state_mib || D <- Decompositions],
    TotalMiBs = [D#memory_decomposition.per_connection_total_mib || D <- Decompositions],
    
    #{
        count => length(Decompositions),
        heap_mib => #{
            min => lists:min(HeapMiBs),
            max => lists:max(HeapMiBs),
            avg => lists:sum(HeapMiBs) / length(HeapMiBs)
        },
        state_mib => #{
            min => lists:min(StateMiBs),
            max => lists:max(StateMiBs),
            avg => lists:sum(StateMiBs) / length(StateMiBs)
        },
        total_mib => #{
            min => lists:min(TotalMiBs),
            max => lists:max(TotalMiBs),
            avg => lists:sum(TotalMiBs) / length(TotalMiBs)
        }
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Convert bytes to mebibytes (MiB)
-spec bytes_to_mib(bytes()) -> mib().
bytes_to_mib(Bytes) ->
    Bytes / (1024 * 1024).

%% @private
%% Measure average connection memory across multiple pids
-spec measure_average_connection_memory([pid()]) -> {bytes(), bytes()}.
measure_average_connection_memory([]) ->
    {0, 0};
measure_average_connection_memory(Pids) ->
    Measurements = lists:filtermap(
        fun(Pid) ->
            case {measure_per_connection_heap(Pid), measure_per_connection_state(Pid)} of
                {{ok, Heap}, {ok, State}} ->
                    {true, {Heap, State}};
                _ ->
                    false
            end
        end,
        Pids
    ),
    
    case Measurements of
        [] ->
            {0, 0};
        _ ->
            TotalHeap = lists:sum([H || {H, _} <- Measurements]),
            TotalState = lists:sum([S || {_, S} <- Measurements]),
            Count = length(Measurements),
            {TotalHeap div Count, TotalState div Count}
    end.

%% @private
%% Measure memory for a single component
-spec measure_component(pid() | undefined, binary()) -> {ok, bytes()} | {error, term()}.
measure_component(undefined, _Label) ->
    {ok, 0};
measure_component(Pid, Label) when is_pid(Pid) ->
    case erlang:process_info(Pid, memory) of
        {memory, Bytes} ->
            {ok, Bytes};
        undefined ->
            ?LOG_WARNING("Component ~s (pid ~p) not alive", [Label, Pid]),
            {ok, 0};
        Error ->
            ?LOG_ERROR("Failed to measure component ~s: ~p", [Label, Error]),
            {error, Error}
    end.
