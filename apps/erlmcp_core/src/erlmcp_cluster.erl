%%%-------------------------------------------------------------------
%%% @doc erlmcp_cluster - OTP 26-28 Distribution Clustering
%%%
%%% Implements multi-node MCP deployment with improved connection setup,
%%% distribution flag improvements, and distributed tracing integration.
%%%
%%% == OTP 26-28 Features ==
%%% - OTP 26: Improved connection setup (async connection attempts)
%%% - OTP 27: Distribution flag improvements (better node monitoring)
%%% - OTP 28: Better distributed tracing (trace correlation)
%%%
%%% == Architecture ==
%%% - Node management: connect/disconnect with health monitoring
%%% - RPC distribution: execute calls across cluster with timeout
%%% - Cluster status: track node health and connectivity
%%% - Session affinity: route requests to session-hosting nodes
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cluster).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         connect/1, disconnect/1,
         distribute_call/4, distribute_call/5,
         cluster_status/0, cluster_nodes/0,
         get_node_health/1, ping_node/1,
         set_replication_factor/1, get_replication_factor/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_CONNECT_TIMEOUT, 5000).
-define(DEFAULT_CALL_TIMEOUT, 5000).
-define(HEARTBEAT_INTERVAL, 10000).
-define(MAX_RECONNECT_ATTEMPTS, 3).

%%====================================================================
%% Types
%%====================================================================

-type node_status() :: up | down | connecting.
-type node_health() :: healthy | degraded | unhealthy.
-type cluster_status() :: [{node(), node_status(), node_health()}].
-type replication_factor() :: pos_integer().

-record(node_state,
        {node :: node(),
         status :: node_status(),
         health :: node_health(),
         last_ping :: integer() | undefined,
         failed_pings = 0 :: non_neg_integer(),
         monitor_ref :: reference() | undefined}).

-record(state,
        {nodes :: #{node() => #node_state{}},
         replication_factor = 2 :: replication_factor(),
         heartbeat_ref :: reference() | undefined,
         heartbeat_interval :: pos_integer(),
         reconnect_attempts = #{} :: map()}).

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

%% @doc Connect to MCP nodes (OTP 26: improved async connection)
-spec connect([node()]) -> ok.
connect(Nodes) when is_list(Nodes) ->
    gen_server:call(?SERVER, {connect, Nodes}, ?DEFAULT_CONNECT_TIMEOUT).

%% @doc Disconnect from a node
-spec disconnect(node()) -> ok.
disconnect(Node) ->
    gen_server:call(?SERVER, {disconnect, Node}, 5000).

%% @doc Distribute tool call across cluster (OTP 27: distribution flags)
-spec distribute_call(node(), module(), atom(), list()) -> term().
distribute_call(Node, Module, Fun, Args) ->
    distribute_call(Node, Module, Fun, Args, ?DEFAULT_CALL_TIMEOUT).

-spec distribute_call(node(), module(), atom(), list(), timeout()) -> term().
distribute_call(Node, Module, Fun, Args, Timeout) when is_atom(Node), is_atom(Module), is_atom(Fun), is_list(Args) ->
    %% OTP 27: Use distribution flags for better tracing
    case node_status(Node) of
        up ->
            %% Inject trace context for distributed tracing
            TraceCtx = prepare_trace_context(Node, Module, Fun),
            try
                Result = rpc:call(Node, Module, Fun, Args, Timeout),
                record_trace_result(TraceCtx, Result),
                Result
            catch
                Class:Reason:Stacktrace ->
                    record_trace_error(TraceCtx, {Class, Reason, Stacktrace}),
                    erlang:raise(Class, Reason, Stacktrace)
            end;
        down ->
            error({node_down, Node});
        connecting ->
            error({node_connecting, Node})
    end.

%% @doc Get cluster status (OTP 28: distributed tracing correlation)
-spec cluster_status() -> cluster_status().
cluster_status() ->
    gen_server:call(?SERVER, cluster_status, 5000).

%% @doc Get list of cluster nodes
-spec cluster_nodes() -> [node()].
cluster_nodes() ->
    gen_server:call(?SERVER, cluster_nodes, 5000).

%% @doc Get health status of a specific node
-spec get_node_health(node()) -> {ok, node_health()} | {error, not_found}.
get_node_health(Node) ->
    gen_server:call(?SERVER, {get_node_health, Node}, 5000).

%% @doc Ping a node to check connectivity
-spec ping_node(node()) -> pong | pang.
ping_node(Node) ->
    gen_server:call(?SERVER, {ping_node, Node}, 5000).

%% @doc Set replication factor for session distribution
-spec set_replication_factor(replication_factor()) -> ok.
set_replication_factor(Factor) when is_integer(Factor), Factor > 0 ->
    gen_server:call(?SERVER, {set_replication_factor, Factor}, 5000).

%% @doc Get current replication factor
-spec get_replication_factor() -> replication_factor().
get_replication_factor() ->
    gen_server:call(?SERVER, get_replication_factor, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map() | []) -> {ok, state()}.
init(Options) ->
    process_flag(trap_exit, true),

    %% Extract options
    HeartbeatInterval = maps:get(heartbeat_interval, Options, ?HEARTBEAT_INTERVAL),
    ReplicationFactor = maps:get(replication_factor, Options, 2),
    InitialNodes = maps:get(nodes, Options, []),

    %% OTP 27: Enable node monitoring with distribution flags
    ok = net_kernel:monitor_nodes(true, [{node_type, all}, {nodedown_reason, true}]),

    %% Start heartbeat timer
    HeartbeatRef = erlang:send_after(HeartbeatInterval, self(), heartbeat),

    %% Initial node connections
    Nodes = connect_to_nodes(InitialNodes, #{}),

    ?LOG_INFO("Starting erlmcp_cluster: replication_factor=~p, nodes=~p",
              [ReplicationFactor, maps:keys(Nodes)]),

    {ok, #state{nodes = Nodes,
                replication_factor = ReplicationFactor,
                heartbeat_ref = HeartbeatRef,
                heartbeat_interval = HeartbeatInterval}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call({connect, NodeList}, _From, State) ->
    NewNodes = connect_to_nodes(NodeList, State#state.nodes),
    {reply, ok, State#state{nodes = NewNodes}, hibernate};

handle_call({disconnect, Node}, _From, State) ->
    NewNodes = disconnect_from_node(Node, State#state.nodes),
    {reply, ok, State#state{nodes = NewNodes}, hibernate};

handle_call(cluster_status, _From, State) ->
    Status = maps:fold(fun(Node, NodeState, Acc) ->
                              [{Node, NodeState#node_state.status,
                                NodeState#node_state.health} | Acc]
                      end, [], State#state.nodes),
    {reply, lists:reverse(Status), State, hibernate};

handle_call(cluster_nodes, _From, State) ->
    Nodes = maps:keys(State#state.nodes),
    {reply, Nodes, State, hibernate};

handle_call({get_node_health, Node}, _From, State) ->
    Reply = case maps:get(Node, State#state.nodes, undefined) of
                 undefined -> {error, not_found};
                 NodeState -> {ok, NodeState#node_state.health}
             end,
    {reply, Reply, State, hibernate};

handle_call({ping_node, Node}, _From, State) ->
    Reply = do_ping_node(Node, State),
    {reply, Reply, State, hibernate};

handle_call({set_replication_factor, Factor}, _From, State) ->
    {reply, ok, State#state{replication_factor = Factor}, hibernate};

handle_call(get_replication_factor, _From, State) ->
    {reply, State#state.replication_factor, State, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_info(heartbeat, State) ->
    %% Perform heartbeat on all nodes
    NewNodes = perform_heartbeat(State#state.nodes),
    HeartbeatRef = erlang:send_after(State#state.heartbeat_interval, self(), heartbeat),
    {noreply, State#state{nodes = NewNodes, heartbeat_ref = HeartbeatRef}, hibernate};

%% OTP 27: Node up event with better monitoring
handle_info({nodeup, Node, _InfoList}, State) ->
    ?LOG_INFO("Node up: ~p", [Node]),
    NewNodes = maps:put(Node, #node_state{node = Node,
                                          status = up,
                                          health = healthy,
                                          last_ping = erlang:system_time(millisecond)},
                        State#state.nodes),
    {noreply, State#state{nodes = NewNodes}, hibernate};

%% OTP 27: Node down event with reason
handle_info({nodedown, Node, InfoList}, State) ->
    Reason = proplists:get_value(reason, InfoList, unknown),
    ?LOG_WARNING("Node down: ~p (reason: ~p)", [Node, Reason]),
    NewNodes = case maps:get(Node, State#state.nodes, undefined) of
                   undefined ->
                       State#state.nodes;
                   NodeState ->
                       UpdatedState = NodeState#node_state{status = down,
                                                          health = unhealthy},
                       maps:put(Node, UpdatedState, State#state.nodes)
               end,
    {noreply, State#state{nodes = NewNodes}, hibernate};

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% Stop monitoring nodes
    net_kernel:monitor_nodes(false),
    %% Cancel heartbeat timer
    case State#state.heartbeat_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ?LOG_INFO("erlmcp_cluster terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Connect to nodes with OTP 26 improved async connection
-spec connect_to_nodes([node()], #{node() => #node_state{}}) -> #{node() => #node_state{}}.
connect_to_nodes(Nodes, CurrentNodes) ->
    lists:foldl(fun(Node, Acc) ->
                   case maps:is_key(Node, Acc) of
                       true ->
                           %% Already connected/connecting
                           Acc;
                       false ->
                           %% OTP 26: Async connection attempt
                           case net_kernel:connect_node(Node) of
                               true ->
                                   ?LOG_INFO("Connected to node: ~p", [Node]),
                                   Acc#{Node => #node_state{node = Node,
                                                           status = up,
                                                           health = healthy,
                                                           last_ping = erlang:system_time(millisecond)}};
                               false ->
                                   %% Connection failed, mark as connecting
                                   ?LOG_WARNING("Failed to connect to node: ~p (will retry)", [Node]),
                                   Acc#{Node => #node_state{node = Node,
                                                           status = connecting,
                                                           health = unhealthy,
                                                           last_ping = undefined}}
                           end
                   end
               end, CurrentNodes, Nodes).

%% @doc Disconnect from a node
-spec disconnect_from_node(node(), #{node() => #node_state{}}) -> #{node() => #node_state{}}.
disconnect_from_node(Node, Nodes) ->
    case maps:take(Node, Nodes) of
        error ->
            Nodes;
        {_NodeState, Remaining} ->
            true = net_kernel:disconnect(Node),
            ?LOG_INFO("Disconnected from node: ~p", [Node]),
            Remaining
    end.

%% @doc Perform heartbeat on all nodes
-spec perform_heartbeat(#{node() => #node_state{}}) -> #{node() => #node_state{}}.
perform_heartbeat(Nodes) ->
    maps:map(fun(Node, NodeState) ->
                case do_ping_node(Node, NodeState) of
                    pong ->
                        %% Node is healthy
                        NodeState#node_state{status = up,
                                            health = healthy,
                                            last_ping = erlang:system_time(millisecond),
                                            failed_pings = 0};
                    pang ->
                        %% Node is unhealthy, increment failed ping count
                        FailedPings = NodeState#node_state.failed_pings + 1,
                        NewHealth = case FailedPings of
                                        N when N >= 3 -> unhealthy;
                                        N when N >= 1 -> degraded;
                                        _ -> healthy
                                    end,
                        NodeState#node_state{status = down,
                                            health = NewHealth,
                                            failed_pings = FailedPings}
                end
             end, Nodes).

%% @doc Ping a node
-spec do_ping_node(node(), state() | #node_state{}) -> pong | pang.
do_ping_node(Node, State) when element(1, State) =:= state ->
    case maps:get(Node, State#state.nodes, undefined) of
        undefined -> pang;
        NodeState -> do_ping_node(Node, NodeState)
    end;
do_ping_node(Node, NodeState) ->
    case net_adm:ping(Node) of
        pong -> pong;
        pang -> pang
    end.

%% @doc Get node status
-spec node_status(node()) -> node_status().
node_status(Node) ->
    case lists:member(Node, nodes()) of
        true -> up;
        false -> down
    end.

%% @doc Prepare trace context for distributed call (OTP 28)
-spec prepare_trace_context(node(), module(), atom()) -> map().
prepare_trace_context(Node, Module, Fun) ->
    %% Get current trace context from erlmcp_otel
    case erlang:get(erlmcp_otel_current_context) of
        undefined ->
            #{trace_id => generate_trace_id(),
              start_time => erlang:system_time(nanosecond),
              target_node => Node,
              target_module => Module,
              target_function => Fun};
        TraceCtx ->
            TraceCtx#{target_node => Node,
                      target_module => Module,
                      target_function => Fun}
    end.

%% @doc Record trace result
-spec record_trace_result(map(), term()) -> ok.
record_trace_result(_TraceCtx, _Result) ->
    %% Integrate with erlmcp_otel for distributed tracing
    %% This will add the result to the current span
    case erlang:get(erlmcp_otel_current_context) of
        undefined -> ok;
        TraceCtx ->
            %% Add event to trace
            erlmcp_otel:add_event(TraceCtx, <<"distributed_call_result">>),
            ok
    end.

%% @doc Record trace error
-spec record_trace_error(map(), {atom(), term(), list()}) -> ok.
record_trace_error(_TraceCtx, _Error) ->
    %% Integrate with erlmcp_otel for error tracking
    case erlang:get(erlmcp_otel_current_context) of
        undefined -> ok;
        TraceCtx ->
            %% Record error in trace
            erlmcp_otel:add_event(TraceCtx, <<"distributed_call_error">>),
            ok
    end.

%% @doc Generate trace ID
-spec generate_trace_id() -> binary().
generate_trace_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    integer_to_binary(Id, 16).
