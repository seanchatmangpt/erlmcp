%%%-------------------------------------------------------------------
%% @doc Testcontainers Composable Fixtures
%%
%% Pre-built test scenarios combining the 8 core features.
%% Each fixture is a complete, reusable test environment.
%%
%% Fixture Matrix (80/20 combinations):
%%
%% | Fixture                | Features Combined          | Use Case                    |
%% |------------------------|----------------------------|-----------------------------
%% | minimal_cluster/1      | 1                          | Basic distributed tests     |
%% | chaos_cluster/1        | 1+2                        | Fault tolerance testing     |
%% | audited_cluster/1      | 1+3+7                      | Compliance testing          |
%% | gcp_sandbox/1          | 4+1                        | Cloud integration tests     |
%% | persistent_cluster/1   | 1+5                        | State recovery testing      |
%% | killswitch_cluster/1   | 1+6                        | Emergency stop testing      |
%% | policy_federation/1    | 1+8                        | Access control testing      |
%% | full_governance/1      | 1+2+3+6+7+8                | Complete governance tests   |
%%
%% @end
%%%-------------------------------------------------------------------
-module(tc_fixtures).

%% Fixture API
-export([
    %% Core fixtures
    minimal_cluster/1,
    chaos_cluster/1,
    audited_cluster/1,
    gcp_sandbox/1,
    persistent_cluster/1,
    killswitch_cluster/1,
    policy_federation/1,
    full_governance/1,

    %% Fixture lifecycle
    setup/1,
    teardown/1,
    with_fixture/2
]).

%% Fixture info
-export([
    list_fixtures/0,
    fixture_info/1
]).

-record(fixture, {
    name :: atom(),
    features :: [atom()],
    nodes :: [node()],
    network :: binary(),
    extra :: map()
}).

-type fixture() :: #fixture{}.
-type fixture_opts() :: #{
    num_nodes => pos_integer(),
    cookie => atom(),
    timeout => pos_integer()
}.

%%====================================================================
%% Fixture Definitions
%%====================================================================

-spec minimal_cluster(fixture_opts()) -> {ok, fixture()} | {error, term()}.
%% @doc Basic Erlang cluster for distributed testing.
%% Features: [governance_cluster]
minimal_cluster(Opts) ->
    NumNodes = maps:get(num_nodes, Opts, 3),
    ensure_tc_started(),

    case tc_core:start_governed_cluster(NumNodes, Opts) of
        {ok, ClusterInfo} ->
            Fixture = #fixture{
                name = minimal_cluster,
                features = [governance_cluster],
                nodes = maps:get(nodes, ClusterInfo),
                network = maps:get(network, ClusterInfo),
                extra = ClusterInfo
            },
            {ok, Fixture};
        Err ->
            Err
    end.

-spec chaos_cluster(fixture_opts()) -> {ok, fixture()} | {error, term()}.
%% @doc Cluster with chaos injection capabilities.
%% Features: [governance_cluster, chaos_injection]
chaos_cluster(Opts) ->
    case minimal_cluster(Opts) of
        {ok, Fixture} ->
            %% Add chaos capabilities
            OldExtra = Fixture#fixture.extra,
            Extra = maps:merge(OldExtra, #{
                chaos_enabled => true,
                partition_fn => fun(GroupA, GroupB) ->
                    tc_core:inject_network_partition(GroupA, GroupB)
                end,
                heal_fn => fun(GroupA, GroupB) ->
                    tc_core:heal_partition(GroupA, GroupB)
                end,
                fail_node_fn => fun(Node) ->
                    tc_core:inject_node_failure(Node)
                end
            }),
            {ok, Fixture#fixture{
                name = chaos_cluster,
                features = [governance_cluster, chaos_injection],
                extra = Extra
            }};
        Err ->
            Err
    end.

-spec audited_cluster(fixture_opts()) -> {ok, fixture()} | {error, term()}.
%% @doc Cluster with full audit trail support.
%% Features: [governance_cluster, receipt_chain, distributed_audit]
audited_cluster(Opts) ->
    case minimal_cluster(Opts) of
        {ok, Fixture} ->
            %% Enable receipt chain verification
            OldExtra = Fixture#fixture.extra,
            Extra = maps:merge(OldExtra, #{
                audit_enabled => true,
                verify_chain_fn => fun(ContractId) ->
                    tc_core:verify_receipt_chain_across_nodes(ContractId)
                end,
                collect_trail_fn => fun(ContractId) ->
                    tc_core:collect_distributed_audit_trail(ContractId)
                end
            }),
            {ok, Fixture#fixture{
                name = audited_cluster,
                features = [governance_cluster, receipt_chain, distributed_audit],
                extra = Extra
            }};
        Err ->
            Err
    end.

-spec gcp_sandbox(fixture_opts()) -> {ok, fixture()} | {error, term()}.
%% @doc GCP simulator sandbox for cloud testing.
%% Features: [gcp_distributed, governance_cluster]
gcp_sandbox(Opts) ->
    NumNodes = maps:get(num_nodes, Opts, 2),
    ensure_tc_started(),

    case tc_core:start_gcp_simulator_cluster(Opts#{num_nodes => NumNodes}) of
        {ok, ClusterInfo} ->
            Nodes = maps:get(nodes, ClusterInfo),

            %% Create helpers for GCP operations
            Extra = maps:merge(ClusterInfo, #{
                storage_fn => fun(Node, Op, Args) ->
                    apply_gcp_op(Node, gcp_storage_sim, Op, Args)
                end,
                pubsub_fn => fun(Node, Op, Args) ->
                    apply_gcp_op(Node, gcp_pubsub_sim, Op, Args)
                end,
                compute_fn => fun(Node, Op, Args) ->
                    apply_gcp_op(Node, gcp_compute_sim, Op, Args)
                end,
                governed_fn => fun(Node, Op, Args) ->
                    apply_gcp_op(Node, gcp_governed, Op, Args)
                end
            }),

            Fixture = #fixture{
                name = gcp_sandbox,
                features = [gcp_distributed, governance_cluster],
                nodes = Nodes,
                network = maps:get(network, ClusterInfo),
                extra = Extra
            },
            {ok, Fixture};
        Err ->
            Err
    end.

-spec persistent_cluster(fixture_opts()) -> {ok, fixture()} | {error, term()}.
%% @doc Cluster with state persistence for recovery testing.
%% Features: [governance_cluster, stateful_testing]
persistent_cluster(Opts) ->
    NumNodes = maps:get(num_nodes, Opts, 2),
    ensure_tc_started(),

    %% Start nodes with persistence
    Results = lists:map(fun(N) ->
        NodeName = list_to_atom("persistent" ++ integer_to_list(N)),
        tc_core:start_with_persistence(NodeName, Opts)
    end, lists:seq(1, NumNodes)),

    Nodes = [N || {ok, N} <- Results],

    case length(Nodes) of
        NumNodes ->
            Extra = #{
                checkpoint_fn => fun(Node) ->
                    tc_core:checkpoint_state(Node)
                end,
                restore_fn => fun(CheckpointId) ->
                    tc_core:restore_state(CheckpointId)
                end,
                checkpoints => []
            },

            Fixture = #fixture{
                name = persistent_cluster,
                features = [governance_cluster, stateful_testing],
                nodes = Nodes,
                network = <<"persistent-cluster">>,
                extra = Extra
            },
            {ok, Fixture};
        _ ->
            {error, {partial_cluster, Results}}
    end.

-spec killswitch_cluster(fixture_opts()) -> {ok, fixture()} | {error, term()}.
%% @doc Cluster with global kill switch testing.
%% Features: [governance_cluster, kill_switch]
killswitch_cluster(Opts) ->
    case minimal_cluster(Opts) of
        {ok, Fixture} ->
            OldExtra = Fixture#fixture.extra,
            Extra = maps:merge(OldExtra, #{
                killswitch_enabled => true,
                activate_fn => fun() ->
                    tc_core:activate_global_kill_switch()
                end,
                deactivate_fn => fun() ->
                    tc_core:deactivate_global_kill_switch()
                end,
                verify_fn => fun() ->
                    tc_core:verify_kill_switch_propagation()
                end
            }),
            {ok, Fixture#fixture{
                name = killswitch_cluster,
                features = [governance_cluster, kill_switch],
                extra = Extra
            }};
        Err ->
            Err
    end.

-spec policy_federation(fixture_opts()) -> {ok, fixture()} | {error, term()}.
%% @doc Cluster with federated policy evaluation.
%% Features: [governance_cluster, federated_policy]
policy_federation(Opts) ->
    case minimal_cluster(Opts) of
        {ok, Fixture} ->
            OldExtra = Fixture#fixture.extra,
            Extra = maps:merge(OldExtra, #{
                policy_enabled => true,
                deploy_policy_fn => fun(PolicyId, Policy) ->
                    tc_core:deploy_federated_policy(PolicyId, Policy)
                end,
                evaluate_fn => fun(PolicyId, Op, Ctx) ->
                    tc_core:evaluate_federated_policy(PolicyId, Op, Ctx)
                end
            }),
            {ok, Fixture#fixture{
                name = policy_federation,
                features = [governance_cluster, federated_policy],
                extra = Extra
            }};
        Err ->
            Err
    end.

-spec full_governance(fixture_opts()) -> {ok, fixture()} | {error, term()}.
%% @doc Complete governance test environment with all features.
%% Features: [governance_cluster, chaos_injection, receipt_chain,
%%            kill_switch, distributed_audit, federated_policy]
full_governance(Opts) ->
    NumNodes = maps:get(num_nodes, Opts, 5),

    case chaos_cluster(Opts#{num_nodes => NumNodes}) of
        {ok, ChaosFixture} ->
            %% Combine all features
            OldExtra = ChaosFixture#fixture.extra,
            Extra = maps:merge(OldExtra, #{
                %% Audit features
                audit_enabled => true,
                verify_chain_fn => fun(ContractId) ->
                    tc_core:verify_receipt_chain_across_nodes(ContractId)
                end,
                collect_trail_fn => fun(ContractId) ->
                    tc_core:collect_distributed_audit_trail(ContractId)
                end,

                %% Kill switch features
                killswitch_enabled => true,
                activate_killswitch_fn => fun() ->
                    tc_core:activate_global_kill_switch()
                end,
                deactivate_killswitch_fn => fun() ->
                    tc_core:deactivate_global_kill_switch()
                end,
                verify_killswitch_fn => fun() ->
                    tc_core:verify_kill_switch_propagation()
                end,

                %% Policy features
                policy_enabled => true,
                deploy_policy_fn => fun(PolicyId, Policy) ->
                    tc_core:deploy_federated_policy(PolicyId, Policy)
                end,
                evaluate_policy_fn => fun(PolicyId, Op, Ctx) ->
                    tc_core:evaluate_federated_policy(PolicyId, Op, Ctx)
                end
            }),

            {ok, ChaosFixture#fixture{
                name = full_governance,
                features = [
                    governance_cluster,
                    chaos_injection,
                    receipt_chain,
                    kill_switch,
                    distributed_audit,
                    federated_policy
                ],
                extra = Extra
            }};
        Err ->
            Err
    end.

%%====================================================================
%% Fixture Lifecycle
%%====================================================================

-spec setup(atom() | {atom(), fixture_opts()}) -> {ok, fixture()} | {error, term()}.
%% @doc Setup a fixture by name.
setup(FixtureName) when is_atom(FixtureName) ->
    setup({FixtureName, #{}});
setup({FixtureName, Opts}) ->
    case FixtureName of
        minimal_cluster -> minimal_cluster(Opts);
        chaos_cluster -> chaos_cluster(Opts);
        audited_cluster -> audited_cluster(Opts);
        gcp_sandbox -> gcp_sandbox(Opts);
        persistent_cluster -> persistent_cluster(Opts);
        killswitch_cluster -> killswitch_cluster(Opts);
        policy_federation -> policy_federation(Opts);
        full_governance -> full_governance(Opts);
        _ -> {error, {unknown_fixture, FixtureName}}
    end.

-spec teardown(fixture()) -> ok.
%% @doc Teardown a fixture, cleaning up all resources.
teardown(#fixture{nodes = _Nodes, network = _Network}) ->
    %% Stop tc_core (cleans up containers and networks)
    catch tc_core:stop(),
    ok.

-spec with_fixture(atom() | {atom(), fixture_opts()}, fun((fixture()) -> term())) -> term().
%% @doc Run a function with a fixture, ensuring cleanup.
with_fixture(FixtureSpec, Fun) ->
    case setup(FixtureSpec) of
        {ok, Fixture} ->
            try
                Fun(Fixture)
            after
                teardown(Fixture)
            end;
        {error, Reason} ->
            error({fixture_setup_failed, Reason})
    end.

%%====================================================================
%% Fixture Info
%%====================================================================

-spec list_fixtures() -> [atom()].
%% @doc List all available fixtures.
list_fixtures() ->
    [
        minimal_cluster,
        chaos_cluster,
        audited_cluster,
        gcp_sandbox,
        persistent_cluster,
        killswitch_cluster,
        policy_federation,
        full_governance
    ].

-spec fixture_info(atom()) -> map().
%% @doc Get information about a fixture.
fixture_info(minimal_cluster) ->
    #{
        name => minimal_cluster,
        description => <<"Basic Erlang cluster for distributed testing">>,
        features => [governance_cluster],
        default_nodes => 3,
        use_cases => [
            <<"Basic distributed governance tests">>,
            <<"Contract enforcement across nodes">>,
            <<"Simple RPC testing">>
        ]
    };
fixture_info(chaos_cluster) ->
    #{
        name => chaos_cluster,
        description => <<"Cluster with chaos injection for fault tolerance testing">>,
        features => [governance_cluster, chaos_injection],
        default_nodes => 3,
        use_cases => [
            <<"Network partition testing">>,
            <<"Node failure recovery">>,
            <<"Split-brain scenarios">>,
            <<"Resilience validation">>
        ]
    };
fixture_info(audited_cluster) ->
    #{
        name => audited_cluster,
        description => <<"Cluster with full audit trail support">>,
        features => [governance_cluster, receipt_chain, distributed_audit],
        default_nodes => 3,
        use_cases => [
            <<"Compliance testing">>,
            <<"Audit trail verification">>,
            <<"Receipt chain integrity">>,
            <<"Evidence bundle collection">>
        ]
    };
fixture_info(gcp_sandbox) ->
    #{
        name => gcp_sandbox,
        description => <<"GCP simulator sandbox for cloud integration testing">>,
        features => [gcp_distributed, governance_cluster],
        default_nodes => 2,
        use_cases => [
            <<"Cloud Storage integration">>,
            <<"Pub/Sub messaging tests">>,
            <<"Compute Engine simulation">>,
            <<"Multi-region testing">>
        ]
    };
fixture_info(persistent_cluster) ->
    #{
        name => persistent_cluster,
        description => <<"Cluster with state persistence for recovery testing">>,
        features => [governance_cluster, stateful_testing],
        default_nodes => 2,
        use_cases => [
            <<"State recovery testing">>,
            <<"Checkpoint/restore validation">>,
            <<"Data persistence verification">>,
            <<"Restart scenarios">>
        ]
    };
fixture_info(killswitch_cluster) ->
    #{
        name => killswitch_cluster,
        description => <<"Cluster with global kill switch testing">>,
        features => [governance_cluster, kill_switch],
        default_nodes => 3,
        use_cases => [
            <<"Emergency stop testing">>,
            <<"Kill switch propagation">>,
            <<"Cluster-wide coordination">>,
            <<"Safety mechanism validation">>
        ]
    };
fixture_info(policy_federation) ->
    #{
        name => policy_federation,
        description => <<"Cluster with federated policy evaluation">>,
        features => [governance_cluster, federated_policy],
        default_nodes => 3,
        use_cases => [
            <<"Distributed access control">>,
            <<"Policy consistency testing">>,
            <<"Consensus-based authorization">>,
            <<"Multi-node policy sync">>
        ]
    };
fixture_info(full_governance) ->
    #{
        name => full_governance,
        description => <<"Complete governance test environment">>,
        features => [
            governance_cluster, chaos_injection, receipt_chain,
            kill_switch, distributed_audit, federated_policy
        ],
        default_nodes => 5,
        use_cases => [
            <<"End-to-end governance testing">>,
            <<"Full compliance validation">>,
            <<"Production-like scenarios">>,
            <<"Comprehensive integration tests">>
        ]
    };
fixture_info(_) ->
    #{error => unknown_fixture}.

%%====================================================================
%% Internal Functions
%%====================================================================

ensure_tc_started() ->
    case whereis(tc_core) of
        undefined -> tc_core:start_link();
        _ -> ok
    end.

apply_gcp_op(Node, Module, Op, Args) ->
    rpc:call(Node, Module, Op, Args, 10000).
