%%%-------------------------------------------------------------------
%%% @doc erlmcp_cluster_monitor - Cluster Health Monitoring and Split-Brain Detection
%%%
%%% Implements comprehensive cluster health monitoring with split-brain
%%% detection, partition recovery, and health tracking for MCP clustering.
%%%
%%% == Features ==
%%% - Node health monitoring: Ping + process count tracking
%%% - Partition detection: Identify network splits
%%% - Split-brain resolution: Majority-based conflict resolution
%%% - Health statistics: Cluster metrics and alerts
%%%
%%% == Architecture ==
%%% - Heartbeat monitoring: Regular ping to all nodes
%%% - Process count tracking: Detect node degradation
%%% - Partition detection: Identify isolated node groups
%%% - Split-brain resolution: Use majority voting or configured master
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cluster_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         check_node_health/1,
         detect_partition/0,
         handle_split_brain/2,
         get_cluster_metrics/0,
         set_alert_threshold/2,
         get_health_history/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_HEARTBEAT_INTERVAL, 10000).
-define(DEFAULT_PROCESS_COUNT_THRESHOLD, 1000).
-define(DEFAULT_PARTITION_THRESHOLD, 3).
-define(MAX_HEALTH_HISTORY, 100).

%%====================================================================
%% Types
%%====================================================================

-type node_health() :: healthy | degraded | unhealthy | unknown.
-type partition_info() ::
    #{isolated_groups => [[node()]],
      majority_group => [node()],
      minority_groups => [[node()]],
      resolution_strategy => majority | oldest_node | configured_master}.
-type cluster_metrics() ::
    #{total_nodes => non_neg_integer(),
      healthy_nodes => non_neg_integer(),
      degraded_nodes => non_neg_integer(),
      unhealthy_nodes => non_neg_integer(),
      partition_detected => boolean(),
      last_check_time => integer()}.
-type health_history() :: [{integer(), cluster_metrics()}].

-record(node_health_state,
        {node :: node(),
         status :: node_health(),
         process_count :: non_neg_integer(),
         last_check :: integer(),
         failed_pings = 0 :: non_neg_integer(),
         last_error :: term() | undefined}).

-record(alert_threshold,
        {process_count :: pos_integer(),
         failed_ping_count :: pos_integer(),
         partition_size :: pos_integer()}).

-record(state,
        {nodes = #{} :: #{node() => #node_health_state{}},
         heartbeat_interval :: pos_integer(),
         heartbeat_ref :: reference() | undefined,
         alert_threshold :: #alert_threshold{},
         health_history :: health_history(),
         partition_state :: undefined | partition_info(),
         split_brain_strategy :: winner_takes_all | oldest_node | configured_master}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Check health of a specific node
-spec check_node_health(node()) -> {ok, node_health(), map()} | {error, term()}.
check_node_health(Node) ->
    gen_server:call(?SERVER, {check_node_health, Node}, 5000).

%% @doc Detect if cluster is partitioned
-spec detect_partition() -> {ok, partition_info() | undefined}.
detect_partition() ->
    gen_server:call(?SERVER, detect_partition, 5000).

%% @doc Handle split-brain resolution
-spec handle_split_brain(partition_info(), majority | oldest_node | configured_master) ->
    {ok, [node()]} | {error, term()}.
handle_split_brain(PartitionInfo, Strategy) ->
    gen_server:call(?SERVER, {handle_split_brain, PartitionInfo, Strategy}, 30000).

%% @doc Get cluster health metrics
-spec get_cluster_metrics() -> {ok, cluster_metrics()}.
get_cluster_metrics() ->
    gen_server:call(?SERVER, get_cluster_metrics, 5000).

%% @doc Set alert threshold
-spec set_alert_threshold(process_count | failed_ping_count | partition_size, pos_integer()) -> ok.
set_alert_threshold(Type, Value) ->
    gen_server:call(?SERVER, {set_alert_threshold, Type, Value}, 5000).

%% @doc Get health history
-spec get_health_history() -> {ok, health_history()}.
get_health_history() ->
    gen_server:call(?SERVER, get_health_history, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map() | []) -> {ok, state()}.
init(Options) ->
    process_flag(trap_exit, true),

    %% Extract options
    HeartbeatInterval = maps:get(heartbeat_interval, Options, ?DEFAULT_HEARTBEAT_INTERVAL),
    ProcessCountThreshold = maps:get(process_count_threshold, Options, ?DEFAULT_PROCESS_COUNT_THRESHOLD),
    FailedPingThreshold = maps:get(failed_ping_threshold, Options, 3),
    PartitionThreshold = maps:get(partition_threshold, Options, ?DEFAULT_PARTITION_THRESHOLD),
    SplitBrainStrategy = maps:get(split_brain_strategy, Options, winner_takes_all),

    %% Setup alert threshold
    AlertThreshold = #alert_threshold{
        process_count = ProcessCountThreshold,
        failed_ping_count = FailedPingThreshold,
        partition_size = PartitionThreshold
    },

    %% Start node monitoring
    ok = net_kernel:monitor_nodes(true, [nodedown_reason]),

    %% Start heartbeat timer
    HeartbeatRef = erlang:send_after(HeartbeatInterval, self(), heartbeat),

    ?LOG_INFO("Starting erlmcp_cluster_monitor: interval=~p, strategy=~p",
              [HeartbeatInterval, SplitBrainStrategy]),

    {ok, #state{heartbeat_interval = HeartbeatInterval,
                heartbeat_ref = HeartbeatRef,
                alert_threshold = AlertThreshold,
                health_history = [],
                split_brain_strategy = SplitBrainStrategy}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call({check_node_health, Node}, _From, State) ->
    case maps:get(Node, State#state.nodes, undefined) of
        undefined ->
            %% Perform health check
            case do_check_node_health(Node, State) of
                {ok, HealthState} ->
                    NewNodes = maps:put(Node, HealthState, State#state.nodes),
                    HealthInfo = #{status => HealthState#node_health_state.status,
                                  process_count => HealthState#node_health_state.process_count,
                                  last_check => HealthState#node_health_state.last_check},
                    {reply, {ok, HealthState#node_health_state.status, HealthInfo},
                     State#state{nodes = NewNodes}, hibernate};
                {error, Reason} ->
                    {reply, {error, Reason}, State, hibernate}
            end;
        HealthState ->
            %% Return cached health state
            HealthInfo = #{status => HealthState#node_health_state.status,
                          process_count => HealthState#node_health_state.process_count,
                          last_check => HealthState#node_health_state.last_check},
            {reply, {ok, HealthState#node_health_state.status, HealthInfo}, State, hibernate}
    end;

handle_call(detect_partition, _From, State) ->
    PartitionInfo = do_detect_partition(State),
    {reply, {ok, PartitionInfo}, State, hibernate};

handle_call({handle_split_brain, PartitionInfo, Strategy}, _From, State) ->
    case do_handle_split_brain(PartitionInfo, Strategy, State) of
        {ok, SurvivingNodes} ->
            ?LOG_WARNING("Split-brain resolved with strategy ~p: ~p surviving nodes",
                        [Strategy, length(SurvivingNodes)]),
            {reply, {ok, SurvivingNodes}, State#state{partition_state = undefined}, hibernate};
        {error, Reason} ->
            {reply, {error, Reason}, State, hibernate}
    end;

handle_call(get_cluster_metrics, _From, State) ->
    Metrics = calculate_cluster_metrics(State),
    {reply, {ok, Metrics}, State, hibernate};

handle_call({set_alert_threshold, Type, Value}, _From, State) ->
    NewThreshold = case Type of
                       process_count ->
                           State#state.alert_threshold#alert_threshold{process_count = Value};
                       failed_ping_count ->
                           State#state.alert_threshold#alert_threshold{failed_ping_count = Value};
                       partition_size ->
                           State#state.alert_threshold#alert_threshold{partition_size = Value}
                   end,
    {reply, ok, State#state{alert_threshold = NewThreshold}, hibernate};

handle_call(get_health_history, _From, State) ->
    {reply, {ok, State#state.health_history}, State, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_info(heartbeat, State) ->
    %% Perform health check on all nodes
    NewNodes = perform_cluster_health_check(State),

    %% Calculate metrics and update history
    Metrics = calculate_cluster_metrics(State#state{nodes = NewNodes}),
    NewHistory = update_health_history(Metrics, State#state.health_history),

    %% Check for partition
    PartitionInfo = do_detect_partition(State#state{nodes = NewNodes}),
    NewState = case PartitionInfo of
                   undefined ->
                       State#state{nodes = NewNodes, health_history = NewHistory};
                   _ ->
                       ?LOG_ERROR("Cluster partition detected: ~p", [PartitionInfo]),
                       State#state{nodes = NewNodes,
                                   health_history = NewHistory,
                                   partition_state = PartitionInfo}
               end,

    %% Schedule next heartbeat
    HeartbeatRef = erlang:send_after(State#state.heartbeat_interval, self(), heartbeat),
    {noreply, NewState#state{heartbeat_ref = HeartbeatRef}, hibernate};

handle_info({nodeup, Node, _InfoList}, State) ->
    ?LOG_INFO("Node up: ~p (cluster monitor)", [Node]),
    {noreply, State, hibernate};

handle_info({nodedown, Node, InfoList}, State) ->
    Reason = proplists:get_value(reason, InfoList, unknown),
    ?LOG_WARNING("Node down: ~p (reason: ~p)", [Node, Reason]),

    %% Update node state to unhealthy
    NewNodes = case maps:get(Node, State#state.nodes, undefined) of
                   undefined ->
                       State#state.nodes;
                   HealthState ->
                       UpdatedState = HealthState#node_health_state{
                           status = unhealthy,
                           last_error = Reason
                       },
                       maps:put(Node, UpdatedState, State#state.nodes)
               end,

    {noreply, State#state{nodes = NewNodes}, hibernate};

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    net_kernel:monitor_nodes(false),
    case State#state.heartbeat_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ?LOG_INFO("erlmcp_cluster_monitor terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Perform health check on a node
-spec do_check_node_health(node(), state()) -> {ok, #node_health_state{}} | {error, term()}.
do_check_node_health(Node, _State) ->
    case net_adm:ping(Node) of
        pong ->
            %% Node is reachable, check process count
            case rpc:call(Node, erlang, system_info, [process_count], 2000) of
                {ok, ProcessCount} when is_integer(ProcessCount) ->
                    HealthState = #node_health_state{
                        node = Node,
                        status = healthy,
                        process_count = ProcessCount,
                        last_check = erlang:system_time(millisecond)
                    },
                    {ok, HealthState};
                {badrpc, Reason} ->
                    {error, {rpc_failed, Reason}};
                ProcessCount when is_integer(ProcessCount) ->
                    HealthState = #node_health_state{
                        node = Node,
                        status = healthy,
                        process_count = ProcessCount,
                        last_check = erlang:system_time(millisecond)
                    },
                    {ok, HealthState}
            end;
        pang ->
            {error, node_unreachable}
    end.

%% @doc Perform health check on all nodes in cluster
-spec perform_cluster_health_check(state()) -> #{node() => #node_health_state{}}.
perform_cluster_health_check(State) ->
    AllNodes = [node() | nodes()],
    Threshold = State#state.alert_threshold,

    lists:foldl(fun(Node, Acc) ->
                   case do_check_node_health(Node, State) of
                       {ok, HealthState} ->
                           maps:put(Node, HealthState, Acc);
                       {error, _Reason} ->
                           %% Mark as unhealthy
                           HealthState = #node_health_state{
                               node = Node,
                               status = unhealthy,
                               process_count = 0,
                               last_check = erlang:system_time(millisecond),
                               last_error = unreachable
                           },
                           maps:put(Node, HealthState, Acc)
                   end
               end, State#state.nodes, AllNodes).

%% @doc Detect cluster partition
-spec do_detect_partition(state()) -> partition_info() | undefined.
do_detect_partition(State) ->
    AllNodes = maps:keys(State#state.nodes),
    HealthyNodes = [Node || Node <- AllNodes,
                           case maps:get(Node, State#state.nodes, undefined) of
                               undefined -> false;
                               HS -> HS#node_health_state.status =:= healthy
                           end],

    %% Build connectivity graph
    ConnectivityGraph = build_connectivity_graph(HealthyNodes),

    %% Find isolated groups using connected components
    Groups = find_connected_components(ConnectivityGraph),

    case length(Groups) > 1 of
        true ->
            %% Partition detected
            GroupSizes = [{length(G), G} || G <- Groups],
            {MajoritySize, MajorityGroup} = lists:max(GroupSizes),
            MinorityGroups = [G || {Size, G} <- GroupSizes, Size < MajoritySize],

            #{isolated_groups => Groups,
              majority_group => MajorityGroup,
              minority_groups => MinorityGroups,
              resolution_strategy => State#state.split_brain_strategy};
        false ->
            undefined
    end.

%% @doc Handle split-brain resolution
-spec do_handle_split_brain(partition_info(), atom(), state()) -> {ok, [node()]} | {error, term()}.
do_handle_split_brain(PartitionInfo, Strategy, _State) ->
    MajorityGroup = maps:get(majority_group, PartitionInfo),

    case Strategy of
        majority ->
            %% Use majority group as surviving cluster
            {ok, MajorityGroup};
        oldest_node ->
            %% Find oldest node in each group and use that group
            OldestNode = find_oldest_node(MajorityGroup),
            {ok, [OldestNode]};
        configured_master ->
            %% Use configured master node (from application env)
            case application:get_env(erlmcp_core, master_node) of
                {ok, MasterNode} ->
                    case lists:member(MasterNode, MajorityGroup) of
                        true ->
                            {ok, MajorityGroup};
                        false ->
                            {error, master_not_in_majority}
                    end;
                undefined ->
                    {error, master_not_configured}
            end
    end.

%% @doc Calculate cluster metrics
-spec calculate_cluster_metrics(state()) -> cluster_metrics().
calculate_cluster_metrics(State) ->
    Nodes = maps:values(State#state.nodes),

    TotalCount = length(Nodes),
    HealthyCount = length([N || N <- Nodes, N#node_health_state.status =:= healthy]),
    DegradedCount = length([N || N <- Nodes, N#node_health_state.status =:= degraded]),
    UnhealthyCount = length([N || N <- Nodes, N#node_health_state.status =:= unhealthy]),

    #{total_nodes => TotalCount,
      healthy_nodes => HealthyCount,
      degraded_nodes => DegradedCount,
      unhealthy_nodes => UnhealthyCount,
      partition_detected => State#state.partition_state =/= undefined,
      last_check_time => erlang:system_time(millisecond)}.

%% @doc Update health history
-spec update_health_history(cluster_metrics(), health_history()) -> health_history().
update_health_history(Metrics, History) ->
    Entry = {erlang:system_time(millisecond), Metrics},
    NewHistory = [Entry | History],
    lists:sublist(NewHistory, ?MAX_HEALTH_HISTORY).

%% @doc Build connectivity graph
-spec build_connectivity_graph([node()]) -> #{node() => [node()]}.
build_connectivity_graph(Nodes) ->
    lists:foldl(fun(Node, Graph) ->
                   %% Find all nodes reachable from this node
                   Reachable = case net_adm:ping(Node) of
                                  pong ->
                                      [N || N <- Nodes, N =/= Node,
                                           case rpc:call(Node, net_adm, ping, [N], 1000) of
                                               pong -> true;
                                               _ -> false
                                           end];
                                  pang ->
                                      []
                              end,
                   maps:put(Node, Reachable, Graph)
               end, #{}, Nodes).

%% @doc Find connected components in graph
-spec find_connected_components(#{node() => [node()]}) -> [[node()]].
find_connected_components(Graph) ->
    Nodes = maps:keys(Graph),
    find_components(Nodes, Graph, [], []).

find_components([], _Graph, [], Acc) ->
    lists:reverse(Acc);
find_components([], _Graph, Current, Acc) ->
    lists:reverse([lists:reverse(Current) | Acc]);
find_components([Node | Rest], Graph, Current, Acc) ->
    case lists:member(Node, Current) orelse
         lists:any(fun(Group) -> lists:member(Node, Group) end, Acc) of
        true ->
            find_components(Rest, Graph, Current, Acc);
        false ->
            %% Start new component
            Component = dfs(Node, Graph, []),
            find_components(Rest, Graph, [], [Component | Acc])
    end.

dfs(Node, Graph, Visited) ->
    case lists:member(Node, Visited) of
        true ->
            Visited;
        false ->
            Neighbors = maps:get(Node, Graph, []),
            NewVisited = [Node | Visited],
            lists:foldl(fun(N, Acc) -> dfs(N, Graph, Acc) end, NewVisited, Neighbors)
    end.

%% @doc Find oldest node in a list
-spec find_oldest_node([node()]) -> node().
find_oldest_node(Nodes) ->
    %% Get start times from all nodes
    StartTimes = [{Node, get_node_start_time(Node)} || Node <- Nodes],
    %% Find node with earliest start time
    {_Time, OldestNode} = lists:min(fun({_, A}, {_, B}) -> A < B end, StartTimes),
    OldestNode.

%% @doc Get node start time
-spec get_node_start_time(node()) -> integer().
get_node_start_time(Node) ->
    case rpc:call(Node, erlang, statistics, [wall_clock], 1000) of
        {Time, _} when is_integer(Time) -> Time;
        _ -> 0
    end.
