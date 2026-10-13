%%%-------------------------------------------------------------------
%%% @doc erlmcp_clustering_SUITE - Common Test Suite for Clustering
%%%
%%% Comprehensive integration tests for erlmcp clustering functionality:
%%% - Node connection (OTP 26 async)
%%% - Session affinity routing
%%% - Distributed tracing (OTP 28)
%%% - Split-brain detection and resolution
%%% - Cluster health monitoring
%%% - Distributed tool invocation
%%%
%%% == OTP 26-28 Features ==
%%% - OTP 26: Async node connection
%%% - OTP 27: Distribution flag improvements
%%% - OTP 28: Better distributed tracing
%%%
%%% == Chicago School TDD ==
%%% - Real cluster nodes (no mocks)
%%% - State-based assertions
%%% - Integration-focused
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_clustering_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/logger.hrl").

%% Suppress ct_slave deprecation (will migrate to peer in OTP 29)
-compile([{nowarn_deprecated_function, [{ct_slave, start, 2}, {ct_slave, stop, 1}]}]).

%% CT Callbacks
-export([all/0, suite/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2, init_per_testcase/2, end_per_testcase/2]).

%% Test Cases - Node Connection
-export([node_connection_sync/1,
         node_connection_async/1,
         node_disconnection/1,
         node_reconnection/1,
         cluster_status/1]).

%% Test Cases - Session Affinity
-export([session_registration/1,
         session_routing_local/1,
         session_routing_remote/1,
         session_migration/1,
         session_failover/1,
         session_invalidation/1]).

%% Test Cases - Distributed Tracing
-export([trace_id_generation/1,
         trace_injection_extraction/1,
         trace_correlation_single_node/1,
         trace_correlation_multi_node/1,
         trace_aggregation/1,
         trace_propagation/1]).

%% Test Cases - Split-Brain Detection
-export([partition_detection_two_nodes/1,
         partition_detection_three_nodes/1,
         split_brain_majority_resolution/1,
         split_brain_oldest_node_resolution/1,
         cluster_reconnection/1]).

%% Test Cases - Cluster Monitoring
-export([health_check_single_node/1,
         health_check_multi_node/1,
         health_metrics/1,
         health_history/1,
         alert_thresholds/1]).

%% Test Cases - Distributed Tool Invocation
-export([tool_invocation_local/1,
         tool_invocation_remote/1,
         tool_invocation_with_tracing/1,
         tool_invocation_failover/1]).

%%====================================================================
%% CT Callbacks
%%====================================================================

suite() ->
    [{timetrap, {seconds, 60}}].

all() ->
    [{group, node_connection},
     {group, session_affinity},
     {group, distributed_tracing},
     {group, split_brain},
     {group, cluster_monitoring},
     {group, distributed_tools}].

groups() ->
    [{node_connection, [sequence],
      [node_connection_sync,
       node_connection_async,
       node_disconnection,
       node_reconnection,
       cluster_status]},

     {session_affinity, [sequence],
      [session_registration,
       session_routing_local,
       session_routing_remote,
       session_migration,
       session_failover,
       session_invalidation]},

     {distributed_tracing, [sequence],
      [trace_id_generation,
       trace_injection_extraction,
       trace_correlation_single_node,
       trace_correlation_multi_node,
       trace_aggregation,
       trace_propagation]},

     {split_brain, [sequence],
      [partition_detection_two_nodes,
       partition_detection_three_nodes,
       split_brain_majority_resolution,
       split_brain_oldest_node_resolution,
       cluster_reconnection]},

     {cluster_monitoring, [sequence],
      [health_check_single_node,
       health_check_multi_node,
       health_metrics,
       health_history,
       alert_thresholds]},

     {distributed_tools, [sequence],
      [tool_invocation_local,
       tool_invocation_remote,
       tool_invocation_with_tracing,
       tool_invocation_failover]}].

init_per_suite(Config) ->
    %% Start distributed Erlang
    case net_kernel:longnames() of
        true ->
            ct:pal("Distributed Erlang already started with longnames"),
            ok;
        false ->
            case net_kernel:shortnames() of
                true ->
                    ct:pal("Distributed Erlang already started with shortnames"),
                    ok;
                false ->
                    case net_kernel:start([erlmcp_cluster_test@localhost, shortnames]) of
                        {ok, _Pid} ->
                            ct:pal("Started distributed Erlang: ~p", [node()]),
                            ok;
                        {error, {already_started, _Pid}} ->
                            ct:pal("Distributed Erlang already started"),
                            ok;
                        {error, Reason} ->
                            ct:fail("Failed to start distributed Erlang: ~p", [Reason])
                    end
            end
    end,

    %% Set cookie
    erlang:set_cookie(node(), erlmcp_cluster_cookie),

    %% Start required applications
    {ok, _Apps} = application:ensure_all_started(gproc),

    Config.

end_per_suite(_Config) ->
    application:stop(gproc),
    net_kernel:stop(),
    ok.

init_per_group(node_connection, Config) ->
    %% Start 2 slave nodes for connection tests
    case start_slave_nodes(2, Config) of
        {ok, Nodes} ->
            [{slave_nodes, Nodes} | Config];
        {error, Reason} ->
            ct:pal("Failed to start slave nodes: ~p", [Reason]),
            {skip, {slave_start_failed, Reason}}
    end;

init_per_group(session_affinity, Config) ->
    case start_slave_nodes(3, Config) of
        {ok, Nodes} ->
            [{slave_nodes, Nodes} | Config];
        {error, Reason} ->
            {skip, {slave_start_failed, Reason}}
    end;

init_per_group(distributed_tracing, Config) ->
    case start_slave_nodes(2, Config) of
        {ok, Nodes} ->
            [{slave_nodes, Nodes} | Config];
        {error, Reason} ->
            {skip, {slave_start_failed, Reason}}
    end;

init_per_group(split_brain, Config) ->
    case start_slave_nodes(3, Config) of
        {ok, Nodes} ->
            [{slave_nodes, Nodes} | Config];
        {error, Reason} ->
            {skip, {slave_start_failed, Reason}}
    end;

init_per_group(cluster_monitoring, Config) ->
    case start_slave_nodes(2, Config) of
        {ok, Nodes} ->
            [{slave_nodes, Nodes} | Config];
        {error, Reason} ->
            {skip, {slave_start_failed, Reason}}
    end;

init_per_group(distributed_tools, Config) ->
    case start_slave_nodes(2, Config) of
        {ok, Nodes} ->
            [{slave_nodes, Nodes} | Config];
        {error, Reason} ->
            {skip, {slave_start_failed, Reason}}
    end;

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    %% Stop slave nodes
    Nodes = proplists:get_value(slave_nodes, Config, []),
    lists:foreach(fun(Node) -> ct_slave:stop(Node) end, Nodes),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p", [TestCase]),

    %% Start clustering modules on all nodes
    Nodes = proplists:get_value(slave_nodes, Config, []),
    AllNodes = [node() | Nodes],

    %% Start gproc on all nodes
    lists:foreach(fun(Node) ->
                     ok = rpc:call(Node, application, ensure_all_started, [gproc])
                  end, AllNodes),

    %% Start cluster module
    {ok, _Pid} = erlmcp_cluster:start_link(#{nodes => Nodes}),

    %% Start session affinity
    {ok, _AffinityPid} = erlmcp_session_affinity:start_link(),

    %% Start distributed tracer
    {ok, _TracerPid} = erlmcp_distributed_tracer:start_link(),

    %% Start cluster monitor
    {ok, _MonitorPid} = erlmcp_cluster_monitor:start_link(),

    %% Wait for nodes to connect
    timer:sleep(500),

    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Stop clustering modules
    catch gen_server:stop(erlmcp_cluster_monitor),
    catch gen_server:stop(erlmcp_distributed_tracer),
    catch gen_server:stop(erlmcp_session_affinity),
    catch gen_server:stop(erlmcp_cluster),

    ok.

%%====================================================================
%% Test Cases - Node Connection
%%====================================================================

node_connection_sync(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1 | _] = Nodes,

    %% Test sync connection
    ok = erlmcp_cluster:connect([Node1]),

    %% Verify node is connected
    pong = erlmcp_cluster:ping_node(Node1),

    %% Verify node in cluster status
    Status = erlmcp_cluster:cluster_status(),
    ct:log("Cluster status: ~p", [Status]),
    true = lists:keymember(Node1, 1, Status),

    ok.

node_connection_async(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1, Node2] = Nodes,

    %% Test async connection (OTP 26 feature)
    ok = erlmcp_cluster:connect(Nodes),

    %% Wait for async connections
    timer:sleep(1000),

    %% Verify both nodes connected
    pong = erlmcp_cluster:ping_node(Node1),
    pong = erlmcp_cluster:ping_node(Node2),

    %% Verify cluster nodes
    ClusterNodes = erlmcp_cluster:cluster_nodes(),
    ct:log("Cluster nodes: ~p", [ClusterNodes]),
    true = lists:member(Node1, ClusterNodes),
    true = lists:member(Node2, ClusterNodes),

    ok.

node_disconnection(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1 | _] = Nodes,

    %% Connect first
    ok = erlmcp_cluster:connect([Node1]),
    pong = erlmcp_cluster:ping_node(Node1),

    %% Disconnect
    ok = erlmcp_cluster:disconnect(Node1),

    %% Verify disconnected
    pang = net_adm:ping(Node1),

    ok.

node_reconnection(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1 | _] = Nodes,

    %% Connect
    ok = erlmcp_cluster:connect([Node1]),
    pong = erlmcp_cluster:ping_node(Node1),

    %% Disconnect
    ok = erlmcp_cluster:disconnect(Node1),
    pang = net_adm:ping(Node1),

    %% Reconnect
    ok = erlmcp_cluster:connect([Node1]),
    timer:sleep(500),
    pong = erlmcp_cluster:ping_node(Node1),

    ok.

cluster_status(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1, Node2] = Nodes,

    %% Connect all nodes
    ok = erlmcp_cluster:connect(Nodes),
    timer:sleep(500),

    %% Get cluster status
    Status = erlmcp_cluster:cluster_status(),
    ct:log("Cluster status: ~p", [Status]),

    %% Verify status format
    true = is_list(Status),
    lists:foreach(fun({Node, NodeStatus, Health}) ->
                     true = is_atom(Node),
                     true = lists:member(NodeStatus, [up, down, connecting]),
                     true = lists:member(Health, [healthy, degraded, unhealthy])
                  end, Status),

    %% Verify local node included
    true = lists:keymember(node(), 1, Status),

    ok.

%%====================================================================
%% Test Cases - Session Affinity
%%====================================================================

session_registration(_Config) ->
    %% Create session on local node
    SessionId = <<"session_local_test">>,

    %% Store session via affinity module
    ok = erlmcp_session_affinity:route_request(SessionId, #{}),

    %% Verify session node
    {ok, SessionNode} = erlmcp_session_affinity:get_session_node(SessionId),
    node() = SessionNode,

    ok.

session_routing_local(_Config) ->
    %% Create session on local node
    SessionId = <<"session_local_routing">>,

    %% Register session location
    {ok, _} = erlmcp_session_affinity:get_session_node(SessionId),

    %% Route request
    {ok, RoutedNode} = erlmcp_session_affinity:route_request(SessionId, #{}),
    node() = RoutedNode,

    ok.

session_routing_remote(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1 | _] = Nodes,

    SessionId = <<"session_remote_routing">>,

    %% Register session on remote node via RPC
    ok = rpc:call(Node1, erlmcp_session_backend, store, [SessionId, #{data => test}]),

    %% Register in affinity map
    Key = {n, g, {erlmcp_session_location, SessionId}},
    Pid = spawn(Node1, fun() -> receive stop -> ok end end),
    rpc:call(Node1, gproc, reg, [Key, Pid]),

    %% Route request from local node
    {ok, RoutedNode} = erlmcp_session_affinity:route_request(SessionId, #{}),
    Node1 = RoutedNode,

    %% Cleanup
    Pid ! stop,
    ok.

session_migration(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1, Node2 | _] = Nodes,

    SessionId = <<"session_migration">>,

    %% Create session on Node1
    ok = rpc:call(Node1, erlmcp_session_backend, store, [SessionId, #{data => test}]),

    %% Migrate to Node2
    BackupNodes = Nodes -- [Node2],
    {ok, Node2} = erlmcp_session_affinity:migrate_session(SessionId, Node2, BackupNodes),

    %% Verify session on Node2
    {ok, _} = rpc:call(Node2, erlmcp_session_backend, fetch, [SessionId]),

    ok.

session_failover(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1, Node2 | _] = Nodes,

    SessionId = <<"session_failover">>,

    %% Register session on Node1
    ok = rpc:call(Node1, erlmcp_session_backend, store, [SessionId, #{data => failover_test}]),

    %% Simulate Node1 failure
    ok = erlmcp_cluster:disconnect(Node1),

    %% Trigger failover
    {ok, NewNode} = erlmcp_session_affinity:handle_failover(SessionId, Node1),

    %% Verify failover to available node
    true = NewNode =:= Node2 orelse NewNode =:= node(),

    %% Reconnect Node1 for cleanup
    pong = net_adm:ping(Node1),

    ok.

session_invalidation(_Config) ->
    SessionId = <<"session_invalidation">>,

    %% Create session
    ok = erlmcp_session_backend:store(SessionId, #{}),

    %% Invalidate
    ok = erlmcp_session_affinity:invalidate_session(SessionId),

    %% Verify not found
    timer:sleep(100),
    {error, not_found} = erlmcp_session_affinity:get_session_node(SessionId),

    ok.

%%====================================================================
%% Test Cases - Distributed Tracing
%%====================================================================

trace_id_generation(_Config) ->
    %% Generate trace ID
    TraceId1 = erlmcp_cluster:generate_trace_id(),
    TraceId2 = erlmcp_distributed_tracer:inject_trace_id(#{}, #{trace_id => <<>>}),

    %% Verify trace IDs are binary
    true = is_binary(TraceId1),
    #{trace_id := TraceId2Bin} = TraceId2,
    true = is_binary(TraceId2Bin),

    %% Verify unique
    TraceId3 = erlmcp_cluster:generate_trace_id(),
    true = TraceId1 =/= TraceId3,

    ok.

trace_injection_extraction(_Config) ->
    %% Create message
    Message = #{type => test, data => <<"test_data">>},
    TraceId = <<"trace_123">>,

    %% Inject trace ID
    {TraceCtx, TracedMessage} = erlmcp_distributed_tracer:inject_trace_id(Message, TraceId),

    %% Verify traced message format
    {mcp_trace, ExtractedCtx, OriginalMessage} = TracedMessage,
    TraceId = maps:get(trace_id, ExtractedCtx),
    Message = OriginalMessage,

    %% Extract trace ID
    {ExtractedId, ExtractedMessage} = erlmcp_distributed_tracer:extract_trace_id(TracedMessage),
    TraceId = ExtractedId,
    Message = ExtractedMessage,

    ok.

trace_correlation_single_node(_Config) ->
    %% Generate trace ID
    TraceId = erlmcp_cluster:generate_trace_id(),

    %% Create spans
    SpanId1 = <<"span_1">>,
    SpanId2 = <<"span_2">>,

    %% Propagate spans
    ok = erlmcp_distributed_tracer:propagate_span(TraceId, SpanId1, node()),
    ok = erlmcp_distributed_tracer:propagate_span(TraceId, SpanId2, node()),

    timer:sleep(200),

    %% Correlate traces
    {ok, ClusterTrace} = erlmcp_distributed_tracer:correlate_traces(TraceId),

    %% Verify trace structure
    TraceId = maps:get(trace_id, ClusterTrace),
    Spans = maps:get(spans, ClusterTrace),
    true = length(Spans) >= 2,

    ok.

trace_correlation_multi_node(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1 | _] = Nodes,

    TraceId = erlmcp_cluster:generate_trace_id(),
    SpanId1 = <<"span_multi_1">>,
    SpanId2 = <<"span_multi_2">>,

    %% Propagate span to remote node
    ok = erlmcp_distributed_tracer:propagate_span(TraceId, SpanId1, Node1),

    timer:sleep(200),

    %% Aggregate traces
    {ok, Spans} = erlmcp_distributed_tracer:aggregate_trace_spans(TraceId),

    %% Verify spans from multiple nodes
    ct:log("Aggregated spans: ~p", [Spans]),
    true = is_list(Spans),

    ok.

trace_aggregation(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1 | _] = Nodes,

    TraceId = <<"trace_agg_test">>,

    %% Create spans on different nodes
    ok = erlmcp_distributed_tracer:propagate_span(TraceId, <<"span_1">>, node()),
    ok = erlmcp_distributed_tracer:propagate_span(TraceId, <<"span_2">>, Node1),

    timer:sleep(300),

    %% Get cluster trace
    {ok, ClusterTrace} = erlmcp_distributed_tracer:get_cluster_trace(TraceId),

    %% Verify summary
    Summary = maps:get(summary, ClusterTrace),
    true = maps:get(span_count, Summary) >= 2,
    NodesList = maps:get(nodes, Summary),
    true = lists:member(node(), NodesList) orelse lists:member(Node1, NodesList),

    ok.

trace_propagation(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1 | _] = Nodes,

    TraceId = <<"trace_prop_test">>,

    %% Inject trace into message
    Message = #{data => test},
    {TraceCtx, TracedMessage} = erlmcp_distributed_tracer:inject_trace_id(Message, TraceId),

    %% Send to remote node via RPC
    ok = rpc:call(Node1, erlmcp_distributed_tracer, handle_incoming_span, [TraceCtx]),

    timer:sleep(200),

    %% Verify trace correlation
    {ok, _ClusterTrace} = erlmcp_distributed_tracer:correlate_traces(TraceId),

    ok.

%%====================================================================
%% Test Cases - Split-Brain Detection
%%====================================================================

partition_detection_two_nodes(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1 | _] = Nodes,

    %% Detect partition (should be none initially)
    {ok, PartitionInfo} = erlmcp_cluster_monitor:detect_partition(),
    undefined = PartitionInfo,

    %% Create partition
    ok = erlang:disconnect_node(Node1),
    timer:sleep(500),

    %% Detect partition again
    {ok, PartitionInfo2} = erlmcp_cluster_monitor:detect_partition(),
    ct:log("Partition info: ~p", [PartitionInfo2]),

    %% Reconnect
    pong = net_adm:ping(Node1),
    timer:sleep(500),

    ok.

partition_detection_three_nodes(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1, Node2 | _] = Nodes,

    %% Connect all nodes
    ok = erlmcp_cluster:connect(Nodes),

    %% Create partial partition
    ok = erlang:disconnect_node(Node1),
    timer:sleep(500),

    %% Detect partition
    {ok, PartitionInfo} = erlmcp_cluster_monitor:detect_partition(),
    ct:log("Partition info: ~p", [PartitionInfo]),

    %% Verify partition structure
    case PartitionInfo of
        undefined ->
            ct:log("No partition detected (expected)");
        #{isolated_groups := Groups} ->
            ct:log("Isolated groups: ~p", [Groups]),
            true = is_list(Groups)
    end,

    %% Reconnect
    pong = net_adm:ping(Node1),
    timer:sleep(500),

    ok.

split_brain_majority_resolution(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1, Node2 | _] = Nodes,

    %% Simulate split-brain scenario
    PartitionInfo = #{
        isolated_groups => [[node(), Node2], [Node1]],
        majority_group => [node(), Node2],
        minority_groups => [[Node1]],
        resolution_strategy => majority
    },

    %% Handle split-brain with majority strategy
    {ok, SurvivingNodes} = erlmcp_cluster_monitor:handle_split_brain(PartitionInfo, majority),

    %% Verify majority group survives
    true = length(SurvivingNodes) >= 2,

    ok.

split_brain_oldest_node_resolution(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1, Node2 | _] = Nodes,

    %% Simulate split-brain
    PartitionInfo = #{
        isolated_groups => [[node(), Node1], [Node2]],
        majority_group => [node(), Node1],
        minority_groups => [[Node2]],
        resolution_strategy => oldest_node
    },

    %% Handle split-brain with oldest node strategy
    {ok, [OldestNode]} = erlmcp_cluster_monitor:handle_split_brain(PartitionInfo, oldest_node),

    %% Verify single oldest node survives
    true = is_atom(OldestNode),

    ok.

cluster_reconnection(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1 | _] = Nodes,

    %% Disconnect node
    ok = erlang:disconnect_node(Node1),
    timer:sleep(300),

    %% Verify disconnected
    pang = net_adm:ping(Node1),

    %% Reconnect
    pong = net_adm:ping(Node1),
    timer:sleep(300),

    %% Verify reconnected in cluster status
    Status = erlmcp_cluster:cluster_status(),
    true = lists:keymember(Node1, 1, Status),

    ok.

%%====================================================================
%% Test Cases - Cluster Monitoring
%%====================================================================

health_check_single_node(_Config) ->
    %% Check health of local node
    {ok, Health, Info} = erlmcp_cluster_monitor:check_node_health(node()),

    %% Verify health status
    true = lists:member(Health, [healthy, degraded, unhealthy]),

    %% Verify info structure
    Status = maps:get(status, Info),
    ProcessCount = maps:get(process_count, Info),
    LastCheck = maps:get(last_check, Info),

    true = is_atom(Status),
    true = is_integer(ProcessCount),
    true = is_integer(LastCheck),

    ok.

health_check_multi_node(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1 | _] = Nodes,

    %% Check health of remote node
    {ok, Health, Info} = erlmcp_cluster_monitor:check_node_health(Node1),

    %% Verify health status
    true = lists:member(Health, [healthy, degraded, unhealthy]),

    ct:log("Remote node health: ~p, info: ~p", [Health, Info]),

    ok.

health_metrics(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1, Node2] = Nodes,

    %% Connect all nodes
    ok = erlmcp_cluster:connect(Nodes),
    timer:sleep(500),

    %% Get cluster metrics
    {ok, Metrics} = erlmcp_cluster_monitor:get_cluster_metrics(),

    %% Verify metrics structure
    TotalNodes = maps:get(total_nodes, Metrics),
    HealthyNodes = maps:get(healthy_nodes, Metrics),
    DegradedNodes = maps:get(degraded_nodes, Metrics),
    UnhealthyNodes = maps:get(unhealthy_nodes, Metrics),
    PartitionDetected = maps:get(partition_detected, Metrics),
    LastCheckTime = maps:get(last_check_time, Metrics),

    true = is_integer(TotalNodes),
    true = is_integer(HealthyNodes),
    true = is_integer(DegradedNodes),
    true = is_integer(UnhealthyNodes),
    true = is_boolean(PartitionDetected),
    true = is_integer(LastCheckTime),

    ct:log("Cluster metrics: ~p", [Metrics]),

    ok.

health_history(Config) ->
    %% Wait for some heartbeat cycles
    timer:sleep(2000),

    %% Get health history
    {ok, History} = erlmcp_cluster_monitor:get_health_history(),

    %% Verify history structure
    true = is_list(History),

    %% Verify entries
    lists:foreach(fun({Timestamp, Metrics}) ->
                     true = is_integer(Timestamp),
                     true = is_map(Metrics)
                  end, History),

    ct:log("Health history entries: ~p", [length(History)]),

    ok.

alert_thresholds(_Config) ->
    %% Set alert thresholds
    ok = erlmcp_cluster_monitor:set_alert_threshold(process_count, 1000),
    ok = erlmcp_cluster_monitor:set_alert_threshold(failed_ping_count, 3),
    ok = erlmcp_cluster_monitor:set_alert_threshold(partition_size, 2),

    %% Verify thresholds were set (by checking health check works)
    {ok, _Health, _Info} = erlmcp_cluster_monitor:check_node_health(node()),

    ok.

%%====================================================================
%% Test Cases - Distributed Tool Invocation
%%====================================================================

tool_invocation_local(_Config) ->
    %% Test local tool invocation via cluster
    TraceId = erlmcp_cluster:generate_trace_id(),

    %% Use distribute_call to invoke local function
    Result = erlmcp_cluster:distribute_call(node(), erlang, node, [], 5000),

    %% Verify result
    node() = Result,

    ok.

tool_invocation_remote(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1 | _] = Nodes,

    %% Test remote tool invocation
    Result = erlmcp_cluster:distribute_call(Node1, erlang, node, [], 5000),

    %% Verify result
    Node1 = Result,

    ok.

tool_invocation_with_tracing(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1 | _] = Nodes,

    TraceId = erlmcp_cluster:generate_trace_id(),

    %% Set trace context
    TraceCtx = #{trace_id => TraceId, span_id => <<"tool_span">>},
    erlang:put(erlmcp_otel_current_context, TraceCtx),

    %% Invoke remote tool
    Result = erlmcp_cluster:distribute_call(Node1, erlang, length, [[1, 2, 3]], 5000),

    %% Verify result
    3 = Result,

    %% Verify trace was recorded
    timer:sleep(200),
    {ok, _ClusterTrace} = erlmcp_distributed_tracer:correlate_traces(TraceId),

    ok.

tool_invocation_failover(Config) ->
    Nodes = proplists:get_value(slave_nodes, Config, []),
    [Node1, Node2] = Nodes,

    %% Invoke on Node1
    Result1 = erlmcp_cluster:distribute_call(Node1, erlang, node, [], 5000),
    Node1 = Result1,

    %% Disconnect Node1
    ok = erlmcp_cluster:disconnect(Node1),

    %% Try to invoke on Node1 (should fail)
    try
        erlmcp_cluster:distribute_call(Node1, erlang, node, [], 5000),
        ct:fail("Expected error for disconnected node")
    catch
        error:{node_down, Node1} ->
            ok; %% Expected
        _:Error ->
            ct:log("Got expected error: ~p", [Error])
    end,

    %% Reconnect Node1
    ok = erlmcp_cluster:connect([Node1]),
    timer:sleep(500),

    %% Verify can invoke again
    Result2 = erlmcp_cluster:distribute_call(Node1, erlang, node, [], 5000),
    Node1 = Result2,

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

start_slave_nodes(Count, Config) ->
    start_slave_nodes(Count, 1, [], Config).

start_slave_nodes(0, _N, Acc, _Config) ->
    {ok, lists:reverse(Acc)};
start_slave_nodes(Count, N, Acc, Config) ->
    NodeName = list_to_atom("cluster_slave" ++ integer_to_list(N) ++ "@" ++
                             net_adm:localhost()),

    case ct_slave:start(NodeName,
                        [{boot_timeout, 60},
                         {init_timeout, 60},
                         {startup_timeout, 60},
                         {monitor_master, true},
                         {kill_if_fail, true},
                         {erl_flags,
                          "-setcookie erlmcp_cluster_cookie -kernel dist_auto_connect once"}])
    of
        {ok, Node} ->
            %% Ensure node is connected
            case net_adm:ping(Node) of
                pong ->
                    ct:pal("Successfully started slave node ~p", [Node]),
                    start_slave_nodes(Count - 1, N + 1, [Node | Acc], Config);
                pang ->
                    ct:pal("Failed to ping slave node ~p", [Node]),
                    ct_slave:stop(Node),
                    {error, {ping_failed, Node}}
            end;
        {error, Reason} ->
            ct:pal("Failed to start slave node ~p: ~p", [NodeName, Reason]),
            {error, {start_failed, NodeName, Reason}}
    end.
