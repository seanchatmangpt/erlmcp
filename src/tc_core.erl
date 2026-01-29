%%%-------------------------------------------------------------------
%% @doc Testcontainers Core - Container Orchestration for Erlang
%%
%% 80/20 Principle: 8 combinatorial features that cover 80% of use cases:
%%
%% 1. Erlang Cluster + Governance    - Distributed contract enforcement
%% 2. Containers + Chaos             - Network partition/node failure
%% 3. Multi-Node + Receipt Chain     - Cross-node audit trails
%% 4. GCP Sim + Distributed          - Cloud simulators in containers
%% 5. Database + State Sync          - Persistence across restarts
%% 6. Kill Switch + Cluster          - Global emergency stop
%% 7. Audit Trail + Multi-Node       - Distributed evidence collection
%% 8. Policy Engine + Federation     - Federated access control
%%
%% @end
%%%-------------------------------------------------------------------
-module(tc_core).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/0
]).

%% Container Management
-export([
    create_network/1,
    remove_network/1,
    run_container/1,
    stop_container/1,
    exec_in_container/2,
    get_container_ip/1,
    wait_for_port/3
]).

%% Erlang Cluster Operations
-export([
    start_erlang_node/2,
    start_erlang_cluster/2,
    connect_to_node/1,
    disconnect_node/1,
    get_cluster_nodes/0
]).

%% Combinatorial Features (80/20)
-export([
    %% Feature 1: Governance Cluster
    start_governed_cluster/2,

    %% Feature 2: Chaos Injection
    inject_network_partition/2,
    inject_node_failure/1,
    heal_partition/2,

    %% Feature 3: Receipt Chain Verification
    verify_receipt_chain_across_nodes/1,

    %% Feature 4: Distributed GCP
    start_gcp_simulator_cluster/1,

    %% Feature 5: Stateful Testing
    start_with_persistence/2,
    checkpoint_state/1,
    restore_state/1,

    %% Feature 6: Kill Switch Propagation
    activate_global_kill_switch/0,
    deactivate_global_kill_switch/0,
    verify_kill_switch_propagation/0,

    %% Feature 7: Distributed Audit
    collect_distributed_audit_trail/1,

    %% Feature 8: Federated Policy
    deploy_federated_policy/2,
    evaluate_federated_policy/3
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    networks = #{} :: #{binary() => network_info()},
    containers = #{} :: #{binary() => container_info()},
    erlang_nodes = #{} :: #{node() => node_info()},
    cookie :: atom(),
    base_port = 9100 :: pos_integer()
}).

-record(network_info, {
    id :: binary(),
    name :: binary(),
    created_at :: pos_integer()
}).

-record(container_info, {
    id :: binary(),
    name :: binary(),
    image :: binary(),
    network :: binary(),
    ip :: binary() | undefined,
    ports = #{} :: #{pos_integer() => pos_integer()},
    status :: running | stopped | paused
}).

-record(node_info, {
    name :: node(),
    container_id :: binary(),
    host :: binary(),
    connected = false :: boolean()
}).

-type network_info() :: #network_info{}.
-type container_info() :: #container_info{}.
-type node_info() :: #node_info{}.

%%====================================================================
%% API
%%====================================================================

start_link() ->
    start_link(#{}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% Container Management
%%====================================================================

-spec create_network(binary()) -> {ok, binary()} | {error, term()}.
create_network(Name) ->
    gen_server:call(?MODULE, {create_network, Name}).

-spec remove_network(binary()) -> ok | {error, term()}.
remove_network(Name) ->
    gen_server:call(?MODULE, {remove_network, Name}).

-spec run_container(map()) -> {ok, binary()} | {error, term()}.
run_container(Opts) ->
    gen_server:call(?MODULE, {run_container, Opts}, 60000).

-spec stop_container(binary()) -> ok | {error, term()}.
stop_container(ContainerId) ->
    gen_server:call(?MODULE, {stop_container, ContainerId}).

-spec exec_in_container(binary(), binary()) -> {ok, binary()} | {error, term()}.
exec_in_container(ContainerId, Command) ->
    gen_server:call(?MODULE, {exec_in_container, ContainerId, Command}, 30000).

-spec get_container_ip(binary()) -> {ok, binary()} | {error, term()}.
get_container_ip(ContainerId) ->
    gen_server:call(?MODULE, {get_container_ip, ContainerId}).

-spec wait_for_port(binary(), pos_integer(), pos_integer()) -> ok | {error, timeout}.
wait_for_port(Host, Port, TimeoutMs) ->
    wait_for_port_loop(Host, Port, TimeoutMs, erlang:monotonic_time(millisecond)).

%%====================================================================
%% Erlang Cluster Operations
%%====================================================================

-spec start_erlang_node(atom(), map()) -> {ok, node()} | {error, term()}.
start_erlang_node(NodeName, Opts) ->
    gen_server:call(?MODULE, {start_erlang_node, NodeName, Opts}, 60000).

-spec start_erlang_cluster(pos_integer(), map()) -> {ok, [node()]} | {error, term()}.
start_erlang_cluster(NumNodes, Opts) ->
    gen_server:call(?MODULE, {start_erlang_cluster, NumNodes, Opts}, 120000).

-spec connect_to_node(node()) -> boolean().
connect_to_node(Node) ->
    net_kernel:connect_node(Node).

-spec disconnect_node(node()) -> boolean().
disconnect_node(Node) ->
    erlang:disconnect_node(Node).

-spec get_cluster_nodes() -> [node()].
get_cluster_nodes() ->
    gen_server:call(?MODULE, get_cluster_nodes).

%%====================================================================
%% Feature 1: Governance Cluster
%%====================================================================

-spec start_governed_cluster(pos_integer(), map()) -> {ok, map()} | {error, term()}.
start_governed_cluster(NumNodes, Opts) ->
    gen_server:call(?MODULE, {start_governed_cluster, NumNodes, Opts}, 180000).

%%====================================================================
%% Feature 2: Chaos Injection
%%====================================================================

-spec inject_network_partition([node()], [node()]) -> ok | {error, term()}.
inject_network_partition(GroupA, GroupB) ->
    gen_server:call(?MODULE, {inject_partition, GroupA, GroupB}).

-spec inject_node_failure(node()) -> ok | {error, term()}.
inject_node_failure(Node) ->
    gen_server:call(?MODULE, {inject_node_failure, Node}).

-spec heal_partition([node()], [node()]) -> ok | {error, term()}.
heal_partition(GroupA, GroupB) ->
    gen_server:call(?MODULE, {heal_partition, GroupA, GroupB}).

%%====================================================================
%% Feature 3: Receipt Chain Verification
%%====================================================================

-spec verify_receipt_chain_across_nodes(binary()) -> {ok, map()} | {error, term()}.
verify_receipt_chain_across_nodes(ContractId) ->
    gen_server:call(?MODULE, {verify_receipt_chain, ContractId}, 30000).

%%====================================================================
%% Feature 4: Distributed GCP
%%====================================================================

-spec start_gcp_simulator_cluster(map()) -> {ok, map()} | {error, term()}.
start_gcp_simulator_cluster(Opts) ->
    gen_server:call(?MODULE, {start_gcp_cluster, Opts}, 120000).

%%====================================================================
%% Feature 5: Stateful Testing
%%====================================================================

-spec start_with_persistence(atom(), map()) -> {ok, node()} | {error, term()}.
start_with_persistence(NodeName, Opts) ->
    gen_server:call(?MODULE, {start_with_persistence, NodeName, Opts}, 60000).

-spec checkpoint_state(node()) -> {ok, binary()} | {error, term()}.
checkpoint_state(Node) ->
    gen_server:call(?MODULE, {checkpoint_state, Node}).

-spec restore_state(binary()) -> {ok, node()} | {error, term()}.
restore_state(CheckpointId) ->
    gen_server:call(?MODULE, {restore_state, CheckpointId}, 60000).

%%====================================================================
%% Feature 6: Kill Switch Propagation
%%====================================================================

-spec activate_global_kill_switch() -> ok | {error, term()}.
activate_global_kill_switch() ->
    gen_server:call(?MODULE, activate_global_kill_switch).

-spec deactivate_global_kill_switch() -> ok | {error, term()}.
deactivate_global_kill_switch() ->
    gen_server:call(?MODULE, deactivate_global_kill_switch).

-spec verify_kill_switch_propagation() -> {ok, map()} | {error, term()}.
verify_kill_switch_propagation() ->
    gen_server:call(?MODULE, verify_kill_switch_propagation, 10000).

%%====================================================================
%% Feature 7: Distributed Audit
%%====================================================================

-spec collect_distributed_audit_trail(binary()) -> {ok, map()} | {error, term()}.
collect_distributed_audit_trail(ContractId) ->
    gen_server:call(?MODULE, {collect_audit_trail, ContractId}, 30000).

%%====================================================================
%% Feature 8: Federated Policy
%%====================================================================

-spec deploy_federated_policy(binary(), map()) -> ok | {error, term()}.
deploy_federated_policy(PolicyId, Policy) ->
    gen_server:call(?MODULE, {deploy_policy, PolicyId, Policy}).

-spec evaluate_federated_policy(binary(), atom(), map()) -> allow | {deny, term()}.
evaluate_federated_policy(PolicyId, Operation, Context) ->
    gen_server:call(?MODULE, {evaluate_policy, PolicyId, Operation, Context}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    Cookie = maps:get(cookie, Opts, erlang:get_cookie()),
    BasePort = maps:get(base_port, Opts, 9100),
    {ok, #state{cookie = Cookie, base_port = BasePort}}.

handle_call({create_network, Name}, _From, State) ->
    Cmd = io_lib:format("docker network create ~s 2>&1", [Name]),
    case run_docker_cmd(Cmd) of
        {ok, NetworkId} ->
            Info = #network_info{
                id = NetworkId,
                name = Name,
                created_at = erlang:system_time(millisecond)
            },
            Networks = maps:put(Name, Info, State#state.networks),
            {reply, {ok, NetworkId}, State#state{networks = Networks}};
        {error, _} = Err ->
            %% Network might already exist
            case run_docker_cmd(io_lib:format("docker network inspect ~s --format '{{.Id}}'", [Name])) of
                {ok, ExistingId} ->
                    Info = #network_info{id = ExistingId, name = Name, created_at = 0},
                    Networks = maps:put(Name, Info, State#state.networks),
                    {reply, {ok, ExistingId}, State#state{networks = Networks}};
                _ ->
                    {reply, Err, State}
            end
    end;

handle_call({remove_network, Name}, _From, State) ->
    Cmd = io_lib:format("docker network rm ~s 2>&1", [Name]),
    case run_docker_cmd(Cmd) of
        {ok, _} ->
            Networks = maps:remove(Name, State#state.networks),
            {reply, ok, State#state{networks = Networks}};
        Err ->
            {reply, Err, State}
    end;

handle_call({run_container, Opts}, _From, State) ->
    Image = maps:get(image, Opts, <<"erlang:26">>),
    Name = maps:get(name, Opts, generate_container_name()),
    Network = maps:get(network, Opts, <<"bridge">>),
    Env = maps:get(env, Opts, #{}),
    Ports = maps:get(ports, Opts, #{}),
    Cmd = maps:get(cmd, Opts, <<>>),

    EnvArgs = maps:fold(fun(K, V, Acc) ->
        [io_lib:format(" -e ~s=~s", [K, V]) | Acc]
    end, [], Env),

    PortArgs = maps:fold(fun(Host, Container, Acc) ->
        [io_lib:format(" -p ~p:~p", [Host, Container]) | Acc]
    end, [], Ports),

    DockerCmd = io_lib:format(
        "docker run -d --rm --network ~s --hostname ~s --name ~s~s~s ~s ~s 2>&1",
        [Network, Name, Name, lists:flatten(EnvArgs), lists:flatten(PortArgs), Image, Cmd]),

    case run_docker_cmd(DockerCmd) of
        {ok, ContainerId} ->
            Info = #container_info{
                id = ContainerId,
                name = Name,
                image = Image,
                network = Network,
                ports = Ports,
                status = running
            },
            Containers = maps:put(ContainerId, Info, State#state.containers),
            {reply, {ok, ContainerId}, State#state{containers = Containers}};
        Err ->
            {reply, Err, State}
    end;

handle_call({stop_container, ContainerId}, _From, State) ->
    Cmd = io_lib:format("docker stop ~s 2>&1", [ContainerId]),
    case run_docker_cmd(Cmd) of
        {ok, _} ->
            Containers = maps:remove(ContainerId, State#state.containers),
            {reply, ok, State#state{containers = Containers}};
        Err ->
            {reply, Err, State}
    end;

handle_call({exec_in_container, ContainerId, Command}, _From, State) ->
    Cmd = io_lib:format("docker exec ~s ~s 2>&1", [ContainerId, Command]),
    {reply, run_docker_cmd(Cmd), State};

handle_call({get_container_ip, ContainerId}, _From, State) ->
    Cmd = io_lib:format(
        "docker inspect -f '{{range.NetworkSettings.Networks}}{{.IPAddress}}{{end}}' ~s 2>&1",
        [ContainerId]),
    {reply, run_docker_cmd(Cmd), State};

handle_call({start_erlang_node, NodeName, Opts}, _From, State) ->
    Network = maps:get(network, Opts, <<"erlmcp-test">>),
    Cookie = maps:get(cookie, Opts, State#state.cookie),

    %% Ensure network exists
    ensure_network(Network),

    NameStr = atom_to_list(NodeName),
    ErlCmd = io_lib:format(
        "erl -name ~s@~s -setcookie ~s -noshell -eval 'timer:sleep(infinity).'",
        [NameStr, NameStr, Cookie]),

    ContainerOpts = #{
        image => <<"erlang:26">>,
        name => list_to_binary(NameStr),
        network => Network,
        env => #{<<"ERLANG_COOKIE">> => atom_to_binary(Cookie)},
        cmd => list_to_binary(ErlCmd)
    },

    case do_run_container(ContainerOpts, State) of
        {ok, ContainerId, State2} ->
            FullNodeName = list_to_atom(NameStr ++ "@" ++ NameStr),
            NodeInfo = #node_info{
                name = FullNodeName,
                container_id = ContainerId,
                host = list_to_binary(NameStr)
            },
            Nodes = maps:put(FullNodeName, NodeInfo, State2#state.erlang_nodes),

            %% Wait for node to be ready
            timer:sleep(2000),

            {reply, {ok, FullNodeName}, State2#state{erlang_nodes = Nodes}};
        {error, _} = Err ->
            {reply, Err, State}
    end;

handle_call({start_erlang_cluster, NumNodes, Opts}, _From, State) ->
    Network = maps:get(network, Opts, <<"erlmcp-cluster">>),
    Prefix = maps:get(prefix, Opts, "node"),

    ensure_network(Network),

    Results = lists:map(fun(N) ->
        NodeName = list_to_atom(Prefix ++ integer_to_list(N)),
        NodeOpts = Opts#{network => Network},
        case gen_server:call(?MODULE, {start_erlang_node, NodeName, NodeOpts}, 60000) of
            {ok, Node} -> {ok, Node};
            Err -> Err
        end
    end, lists:seq(1, NumNodes)),

    Nodes = [N || {ok, N} <- Results],
    case length(Nodes) of
        NumNodes ->
            {reply, {ok, Nodes}, State};
        _ ->
            {reply, {error, {partial_cluster, Nodes, Results}}, State}
    end;

handle_call(get_cluster_nodes, _From, State) ->
    Nodes = maps:keys(State#state.erlang_nodes),
    {reply, Nodes, State};

%% Feature 1: Governed Cluster
handle_call({start_governed_cluster, NumNodes, Opts}, _From, State) ->
    Network = <<"governed-cluster">>,
    ensure_network(Network),

    %% Start nodes with governance modules
    ClusterOpts = Opts#{
        network => Network,
        prefix => "governed"
    },

    case gen_server:call(?MODULE, {start_erlang_cluster, NumNodes, ClusterOpts}, 120000) of
        {ok, Nodes} ->
            %% Initialize governance on each node
            Results = lists:map(fun(Node) ->
                init_governance_on_node(Node)
            end, Nodes),

            ClusterInfo = #{
                nodes => Nodes,
                network => Network,
                governance_status => Results
            },
            {reply, {ok, ClusterInfo}, State};
        Err ->
            {reply, Err, State}
    end;

%% Feature 2: Chaos Injection
handle_call({inject_partition, GroupA, GroupB}, _From, State) ->
    %% Use iptables to block traffic between groups
    Results = lists:flatmap(fun(NodeA) ->
        lists:map(fun(NodeB) ->
            block_node_traffic(NodeA, NodeB, State)
        end, GroupB)
    end, GroupA),

    case lists:all(fun(R) -> R =:= ok end, Results) of
        true -> {reply, ok, State};
        false -> {reply, {error, {partial_partition, Results}}, State}
    end;

handle_call({inject_node_failure, Node}, _From, State) ->
    case maps:get(Node, State#state.erlang_nodes, undefined) of
        undefined ->
            {reply, {error, node_not_found}, State};
        #node_info{container_id = ContainerId} ->
            Cmd = io_lib:format("docker pause ~s", [ContainerId]),
            case run_docker_cmd(Cmd) of
                {ok, _} ->
                    Nodes = maps:update_with(Node, fun(Info) ->
                        Info#node_info{connected = false}
                    end, State#state.erlang_nodes),
                    {reply, ok, State#state{erlang_nodes = Nodes}};
                Err ->
                    {reply, Err, State}
            end
    end;

handle_call({heal_partition, GroupA, GroupB}, _From, State) ->
    Results = lists:flatmap(fun(NodeA) ->
        lists:map(fun(NodeB) ->
            unblock_node_traffic(NodeA, NodeB, State)
        end, GroupB)
    end, GroupA),

    case lists:all(fun(R) -> R =:= ok end, Results) of
        true -> {reply, ok, State};
        false -> {reply, {error, {partial_heal, Results}}, State}
    end;

%% Feature 3: Receipt Chain Verification
handle_call({verify_receipt_chain, ContractId}, _From, State) ->
    Nodes = maps:keys(State#state.erlang_nodes),

    %% Collect receipts from all nodes
    AllReceipts = lists:flatmap(fun(Node) ->
        case rpc:call(Node, gcp_governed, get_receipts, [ContractId], 5000) of
            {ok, Receipts} -> Receipts;
            _ -> []
        end
    end, Nodes),

    %% Verify chain integrity
    Verification = verify_receipt_chain_integrity(AllReceipts),
    {reply, {ok, Verification}, State};

%% Feature 4: GCP Simulator Cluster
handle_call({start_gcp_cluster, Opts}, _From, State) ->
    NumNodes = maps:get(num_nodes, Opts, 3),
    Network = <<"gcp-sim-cluster">>,
    ensure_network(Network),

    ClusterOpts = Opts#{network => Network, prefix => "gcpsim"},

    case gen_server:call(?MODULE, {start_erlang_cluster, NumNodes, ClusterOpts}, 120000) of
        {ok, Nodes} ->
            %% Start GCP simulators on each node
            Results = lists:map(fun(Node) ->
                start_gcp_simulators_on_node(Node)
            end, Nodes),

            ClusterInfo = #{
                nodes => Nodes,
                network => Network,
                gcp_status => Results
            },
            {reply, {ok, ClusterInfo}, State};
        Err ->
            {reply, Err, State}
    end;

%% Feature 5: Stateful Testing
handle_call({start_with_persistence, NodeName, Opts}, _From, State) ->
    %% Create volume for persistence
    VolumeName = io_lib:format("erlmcp-state-~s", [NodeName]),
    run_docker_cmd(io_lib:format("docker volume create ~s", [VolumeName])),

    NodeOpts = Opts#{
        volumes => #{VolumeName => <<"/var/lib/erlmcp">>}
    },

    gen_server:call(?MODULE, {start_erlang_node, NodeName, NodeOpts}, 60000);

handle_call({checkpoint_state, Node}, _From, State) ->
    case maps:get(Node, State#state.erlang_nodes, undefined) of
        undefined ->
            {reply, {error, node_not_found}, State};
        #node_info{container_id = ContainerId} ->
            CheckpointId = generate_checkpoint_id(),
            Cmd = io_lib:format(
                "docker commit ~s erlmcp-checkpoint:~s",
                [ContainerId, CheckpointId]),
            case run_docker_cmd(Cmd) of
                {ok, _} -> {reply, {ok, CheckpointId}, State};
                Err -> {reply, Err, State}
            end
    end;

handle_call({restore_state, CheckpointId}, _From, State) ->
    NodeName = list_to_atom("restored-" ++ binary_to_list(CheckpointId)),
    Opts = #{image => <<"erlmcp-checkpoint:", CheckpointId/binary>>},
    gen_server:call(?MODULE, {start_erlang_node, NodeName, Opts}, 60000);

%% Feature 6: Kill Switch Propagation
handle_call(activate_global_kill_switch, _From, State) ->
    Nodes = maps:keys(State#state.erlang_nodes),
    Results = lists:map(fun(Node) ->
        rpc:call(Node, gcp_governed, activate_kill_switch, [], 5000)
    end, Nodes),

    case lists:all(fun(R) -> R =:= ok end, Results) of
        true -> {reply, ok, State};
        false -> {reply, {error, {partial_activation, Results}}, State}
    end;

handle_call(deactivate_global_kill_switch, _From, State) ->
    Nodes = maps:keys(State#state.erlang_nodes),
    Results = lists:map(fun(Node) ->
        rpc:call(Node, gcp_governed, deactivate_kill_switch, [], 5000)
    end, Nodes),

    case lists:all(fun(R) -> R =:= ok end, Results) of
        true -> {reply, ok, State};
        false -> {reply, {error, {partial_deactivation, Results}}, State}
    end;

handle_call(verify_kill_switch_propagation, _From, State) ->
    Nodes = maps:keys(State#state.erlang_nodes),

    Statuses = lists:map(fun(Node) ->
        Status = rpc:call(Node, gcp_governed, is_kill_switch_active, [], 5000),
        {Node, Status}
    end, Nodes),

    AllActive = lists:all(fun({_, S}) -> S =:= true end, Statuses),
    AllInactive = lists:all(fun({_, S}) -> S =:= false end, Statuses),

    Result = #{
        statuses => maps:from_list(Statuses),
        consistent => AllActive orelse AllInactive,
        all_active => AllActive,
        all_inactive => AllInactive
    },
    {reply, {ok, Result}, State};

%% Feature 7: Distributed Audit
handle_call({collect_audit_trail, ContractId}, _From, State) ->
    Nodes = maps:keys(State#state.erlang_nodes),

    %% Collect audit trails from all nodes
    Trails = lists:map(fun(Node) ->
        case rpc:call(Node, gcp_governed, export_audit_trail, [ContractId], 10000) of
            {ok, Trail} -> {Node, Trail};
            Err -> {Node, {error, Err}}
        end
    end, Nodes),

    %% Merge and deduplicate
    MergedTrail = merge_audit_trails(Trails),
    {reply, {ok, MergedTrail}, State};

%% Feature 8: Federated Policy
handle_call({deploy_policy, PolicyId, Policy}, _From, State) ->
    Nodes = maps:keys(State#state.erlang_nodes),

    Results = lists:map(fun(Node) ->
        rpc:call(Node, gcp_policy_engine, set_policy, [PolicyId, Policy], 5000)
    end, Nodes),

    case lists:all(fun(R) -> R =:= ok end, Results) of
        true -> {reply, ok, State};
        false -> {reply, {error, {partial_deployment, Results}}, State}
    end;

handle_call({evaluate_policy, PolicyId, Operation, Context}, _From, State) ->
    Nodes = maps:keys(State#state.erlang_nodes),

    %% Majority vote on policy evaluation
    Results = lists:map(fun(Node) ->
        rpc:call(Node, gcp_policy_engine, evaluate_by_id, [PolicyId, Operation, Context], 5000)
    end, Nodes),

    %% Count votes
    AllowCount = length([R || R <- Results, R =:= allow]),
    DenyResults = [R || R <- Results, is_tuple(R), element(1, R) =:= deny],

    case AllowCount > length(DenyResults) of
        true -> {reply, allow, State};
        false -> {reply, hd(DenyResults), State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup all containers
    lists:foreach(fun({_, #container_info{id = Id}}) ->
        run_docker_cmd(io_lib:format("docker stop ~s", [Id]))
    end, maps:to_list(State#state.containers)),

    %% Cleanup networks
    lists:foreach(fun({Name, _}) ->
        run_docker_cmd(io_lib:format("docker network rm ~s", [Name]))
    end, maps:to_list(State#state.networks)),

    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

run_docker_cmd(Cmd) ->
    CmdStr = lists:flatten(Cmd),
    Result = os:cmd(CmdStr),
    Trimmed = string:trim(Result),
    case Trimmed of
        "" -> {ok, <<>>};
        _ ->
            case string:find(Trimmed, "Error") of
                nomatch -> {ok, list_to_binary(Trimmed)};
                _ -> {error, list_to_binary(Trimmed)}
            end
    end.

generate_container_name() ->
    list_to_binary("erlmcp-" ++ integer_to_list(erlang:unique_integer([positive]))).

generate_checkpoint_id() ->
    list_to_binary(integer_to_list(erlang:unique_integer([positive]))).

ensure_network(Name) ->
    run_docker_cmd(io_lib:format("docker network create ~s 2>/dev/null || true", [Name])).

do_run_container(Opts, State) ->
    Image = maps:get(image, Opts, <<"erlang:26">>),
    Name = maps:get(name, Opts, generate_container_name()),
    Network = maps:get(network, Opts, <<"bridge">>),
    Env = maps:get(env, Opts, #{}),
    Cmd = maps:get(cmd, Opts, <<>>),

    EnvArgs = maps:fold(fun(K, V, Acc) ->
        [io_lib:format(" -e ~s=~s", [K, V]) | Acc]
    end, [], Env),

    DockerCmd = io_lib:format(
        "docker run -d --rm --network ~s --hostname ~s --name ~s~s ~s ~s 2>&1",
        [Network, Name, Name, lists:flatten(EnvArgs), Image, Cmd]),

    case run_docker_cmd(DockerCmd) of
        {ok, ContainerId} ->
            Info = #container_info{
                id = ContainerId,
                name = Name,
                image = Image,
                network = Network,
                status = running
            },
            Containers = maps:put(ContainerId, Info, State#state.containers),
            {ok, ContainerId, State#state{containers = Containers}};
        Err ->
            Err
    end.

wait_for_port_loop(Host, Port, TimeoutMs, StartTime) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    case Elapsed > TimeoutMs of
        true ->
            {error, timeout};
        false ->
            case gen_tcp:connect(binary_to_list(Host), Port, [], 1000) of
                {ok, Socket} ->
                    gen_tcp:close(Socket),
                    ok;
                {error, _} ->
                    timer:sleep(100),
                    wait_for_port_loop(Host, Port, TimeoutMs, StartTime)
            end
    end.

init_governance_on_node(Node) ->
    %% Start governance services on remote node
    case rpc:call(Node, application, ensure_all_started, [erlmcp], 10000) of
        {ok, _} ->
            rpc:call(Node, gcp_governed, start_link, [], 5000);
        Err ->
            Err
    end.

start_gcp_simulators_on_node(Node) ->
    rpc:call(Node, gcp_storage_sim, start_link, [], 5000),
    rpc:call(Node, gcp_pubsub_sim, start_link, [], 5000),
    rpc:call(Node, gcp_compute_sim, start_link, [], 5000),
    rpc:call(Node, gcp_iam_sim, start_link, [], 5000).

block_node_traffic(NodeA, NodeB, State) ->
    case {maps:get(NodeA, State#state.erlang_nodes, undefined),
          maps:get(NodeB, State#state.erlang_nodes, undefined)} of
        {#node_info{container_id = IdA}, #node_info{host = HostB}} ->
            Cmd = io_lib:format(
                "docker exec ~s iptables -A OUTPUT -d ~s -j DROP 2>&1",
                [IdA, HostB]),
            case run_docker_cmd(Cmd) of
                {ok, _} -> ok;
                _ -> error
            end;
        _ ->
            error
    end.

unblock_node_traffic(NodeA, NodeB, State) ->
    case {maps:get(NodeA, State#state.erlang_nodes, undefined),
          maps:get(NodeB, State#state.erlang_nodes, undefined)} of
        {#node_info{container_id = IdA}, #node_info{host = HostB}} ->
            Cmd = io_lib:format(
                "docker exec ~s iptables -D OUTPUT -d ~s -j DROP 2>&1",
                [IdA, HostB]),
            case run_docker_cmd(Cmd) of
                {ok, _} -> ok;
                _ -> error
            end;
        _ ->
            error
    end.

verify_receipt_chain_integrity(Receipts) ->
    %% Sort by sequence number
    Sorted = lists:sort(fun(A, B) ->
        maps:get(sequence, A, 0) < maps:get(sequence, B, 0)
    end, Receipts),

    %% Verify hash chain
    verify_chain(Sorted, undefined, []).

verify_chain([], _PrevHash, Verified) ->
    #{
        valid => true,
        receipts_verified => length(Verified),
        chain => lists:reverse(Verified)
    };
verify_chain([Receipt | Rest], PrevHash, Verified) ->
    CurrentPrevHash = maps:get(previous_hash, Receipt, undefined),
    case PrevHash =:= undefined orelse CurrentPrevHash =:= PrevHash of
        true ->
            NewPrevHash = maps:get(request_hash, Receipt),
            verify_chain(Rest, NewPrevHash, [Receipt | Verified]);
        false ->
            #{
                valid => false,
                break_at => maps:get(sequence, Receipt),
                expected_hash => PrevHash,
                actual_hash => CurrentPrevHash
            }
    end.

merge_audit_trails(Trails) ->
    %% Collect all receipts
    AllReceipts = lists:flatmap(fun({_Node, Trail}) ->
        case Trail of
            {error, _} -> [];
            Json when is_binary(Json) ->
                case jsx:decode(Json, [return_maps]) of
                    #{<<"receipts">> := Rs} -> Rs;
                    _ -> []
                end;
            _ -> []
        end
    end, Trails),

    %% Deduplicate by receipt ID
    Unique = lists:usort(fun(A, B) ->
        maps:get(<<"id">>, A, <<>>) =< maps:get(<<"id">>, B, <<>>)
    end, AllReceipts),

    #{
        source_nodes => length(Trails),
        total_receipts => length(AllReceipts),
        unique_receipts => length(Unique),
        receipts => Unique
    }.
