%%%-------------------------------------------------------------------
%% @doc Testcontainers Pre-built Test Scenarios
%%
%% Ready-to-run test scenarios that exercise the 8 combinatorial features.
%% Each scenario is self-contained and produces detailed results.
%%
%% Scenario Categories:
%% 1. Resilience Scenarios    - Fault tolerance, recovery
%% 2. Governance Scenarios    - Contracts, receipts, audit
%% 3. Coordination Scenarios  - Kill switch, policy sync
%% 4. Integration Scenarios   - GCP, persistence
%%
%% @end
%%%-------------------------------------------------------------------
-module(tc_scenarios).

%% Include fixture record definition
-record(fixture, {
    name :: atom(),
    features :: [atom()],
    nodes :: [node()],
    network :: binary(),
    extra :: map()
}).

%% Resilience Scenarios
-export([
    scenario_network_partition_recovery/0,
    scenario_node_failure_recovery/0,
    scenario_split_brain_resolution/0
]).

%% Governance Scenarios
-export([
    scenario_receipt_chain_integrity/0,
    scenario_distributed_audit_collection/0,
    scenario_contract_enforcement_across_nodes/0
]).

%% Coordination Scenarios
-export([
    scenario_global_kill_switch/0,
    scenario_federated_policy_consensus/0,
    scenario_kill_switch_during_partition/0
]).

%% Integration Scenarios
-export([
    scenario_gcp_multi_region/0,
    scenario_state_checkpoint_restore/0,
    scenario_full_governance_flow/0
]).

%% Runner
-export([
    run_scenario/1,
    run_all_scenarios/0,
    run_category/1
]).

-record(scenario_result, {
    name :: atom(),
    status :: passed | failed | skipped,
    duration_ms :: non_neg_integer(),
    details :: map(),
    errors :: [term()]
}).

%%====================================================================
%% Resilience Scenarios
%%====================================================================

-spec scenario_network_partition_recovery() -> #scenario_result{}.
%% @doc Test recovery from network partition.
%% 1. Start 5-node cluster
%% 2. Create network partition (2 nodes vs 3 nodes)
%% 3. Verify both partitions operate independently
%% 4. Heal partition
%% 5. Verify cluster reunification
scenario_network_partition_recovery() ->
    run_with_timing(network_partition_recovery, fun() ->
        tc_fixtures:with_fixture({chaos_cluster, #{num_nodes => 5}}, fun(Fixture) ->
            Nodes = Fixture#fixture.nodes,
            Extra = Fixture#fixture.extra,

            %% Split into two groups
            {GroupA, GroupB} = lists:split(2, Nodes),

            %% Inject partition
            PartitionFn = maps:get(partition_fn, Extra),
            ok = PartitionFn(GroupA, GroupB),

            %% Verify partition - nodes in different groups can't communicate
            PartitionVerified = lists:all(fun(NodeA) ->
                lists:all(fun(NodeB) ->
                    rpc:call(NodeA, net_adm, ping, [NodeB], 2000) =:= pang
                end, GroupB)
            end, GroupA),

            %% Heal partition
            HealFn = maps:get(heal_fn, Extra),
            ok = HealFn(GroupA, GroupB),
            timer:sleep(2000), %% Allow reconnection

            %% Verify healed - all nodes can communicate
            HealVerified = lists:all(fun(NodeA) ->
                lists:all(fun(NodeB) ->
                    rpc:call(NodeA, net_adm, ping, [NodeB], 2000) =:= pong
                end, GroupB)
            end, GroupA),

            #{
                partition_verified => PartitionVerified,
                heal_verified => HealVerified,
                group_a => GroupA,
                group_b => GroupB
            }
        end)
    end).

-spec scenario_node_failure_recovery() -> #scenario_result{}.
%% @doc Test recovery when a node fails.
%% 1. Start 3-node cluster with operations in progress
%% 2. Fail one node
%% 3. Verify remaining nodes continue operating
%% 4. Verify no data loss
scenario_node_failure_recovery() ->
    run_with_timing(node_failure_recovery, fun() ->
        tc_fixtures:with_fixture({chaos_cluster, #{num_nodes => 3}}, fun(Fixture) ->
            [Node1, Node2, Node3] = Fixture#fixture.nodes,
            Extra = Fixture#fixture.extra,
            FailNodeFn = maps:get(fail_node_fn, Extra),

            %% Perform operations on all nodes
            ContractId = <<"fail-test-contract">>,
            {ok, _, _} = rpc:call(Node1, gcp_governed, storage_upload,
                [ContractId, <<"test-bucket">>, <<"file1.txt">>,
                 <<"text/plain">>, <<"data1">>], 5000),

            %% Fail Node2
            ok = FailNodeFn(Node2),
            timer:sleep(1000),

            %% Verify Node1 and Node3 still work
            Node1Works = case rpc:call(Node1, gcp_governed, storage_upload,
                [ContractId, <<"test-bucket">>, <<"file2.txt">>,
                 <<"text/plain">>, <<"data2">>], 5000) of
                {ok, _, _} -> true;
                _ -> false
            end,

            Node3Works = case rpc:call(Node3, gcp_governed, storage_upload,
                [ContractId, <<"test-bucket">>, <<"file3.txt">>,
                 <<"text/plain">>, <<"data3">>], 5000) of
                {ok, _, _} -> true;
                _ -> false
            end,

            #{
                failed_node => Node2,
                remaining_nodes_operational => Node1Works andalso Node3Works,
                node1_status => Node1Works,
                node3_status => Node3Works
            }
        end)
    end).

-spec scenario_split_brain_resolution() -> #scenario_result{}.
%% @doc Test split-brain scenario with kill switch.
%% 1. Create partition
%% 2. Activate kill switch on one partition
%% 3. Verify kill switch prevents operations
%% 4. Heal and verify consistent state
scenario_split_brain_resolution() ->
    run_with_timing(split_brain_resolution, fun() ->
        tc_fixtures:with_fixture({full_governance, #{num_nodes => 4}}, fun(Fixture) ->
            Nodes = Fixture#fixture.nodes,
            Extra = Fixture#fixture.extra,

            {GroupA, GroupB} = lists:split(2, Nodes),
            PartitionFn = maps:get(partition_fn, Extra),
            HealFn = maps:get(heal_fn, Extra),

            %% Create partition
            ok = PartitionFn(GroupA, GroupB),

            %% Activate kill switch on GroupA only
            lists:foreach(fun(Node) ->
                rpc:call(Node, gcp_governed, activate_kill_switch, [], 5000)
            end, GroupA),

            %% GroupA should refuse operations
            GroupABlocked = lists:all(fun(Node) ->
                case rpc:call(Node, gcp_governed, storage_upload,
                    [<<"test">>, <<"bucket">>, <<"file">>, <<"text/plain">>, <<"data">>], 5000) of
                    {error, _} -> true;
                    _ -> false
                end
            end, GroupA),

            %% GroupB should still work
            GroupBWorks = lists:all(fun(Node) ->
                case rpc:call(Node, gcp_governed, storage_upload,
                    [<<"test">>, <<"bucket">>, <<"file">>, <<"text/plain">>, <<"data">>], 5000) of
                    {ok, _, _} -> true;
                    _ -> false
                end
            end, GroupB),

            %% Heal partition
            ok = HealFn(GroupA, GroupB),

            %% Verify kill switch state after heal
            VerifyFn = maps:get(verify_killswitch_fn, Extra),
            {ok, FinalState} = VerifyFn(),

            #{
                group_a_blocked => GroupABlocked,
                group_b_works => GroupBWorks,
                final_state => FinalState
            }
        end)
    end).

%%====================================================================
%% Governance Scenarios
%%====================================================================

-spec scenario_receipt_chain_integrity() -> #scenario_result{}.
%% @doc Verify receipt chain integrity across distributed operations.
scenario_receipt_chain_integrity() ->
    run_with_timing(receipt_chain_integrity, fun() ->
        tc_fixtures:with_fixture({audited_cluster, #{num_nodes => 3}}, fun(Fixture) ->
            Nodes = Fixture#fixture.nodes,
            Extra = Fixture#fixture.extra,
            VerifyChainFn = maps:get(verify_chain_fn, Extra),

            ContractId = <<"chain-test-contract">>,

            %% Perform operations on different nodes
            lists:foreach(fun({Node, N}) ->
                FileName = list_to_binary("chain-file-" ++ integer_to_list(N) ++ ".txt"),
                rpc:call(Node, gcp_governed, storage_upload,
                    [ContractId, <<"test-bucket">>, FileName,
                     <<"text/plain">>, <<"data">>], 5000)
            end, lists:zip(Nodes, lists:seq(1, length(Nodes)))),

            %% Verify chain integrity
            {ok, Verification} = VerifyChainFn(ContractId),

            #{
                chain_valid => maps:get(valid, Verification, false),
                receipts_verified => maps:get(receipts_verified, Verification, 0),
                verification_details => Verification
            }
        end)
    end).

-spec scenario_distributed_audit_collection() -> #scenario_result{}.
%% @doc Collect and merge audit trails from all nodes.
scenario_distributed_audit_collection() ->
    run_with_timing(distributed_audit_collection, fun() ->
        tc_fixtures:with_fixture({audited_cluster, #{num_nodes => 3}}, fun(Fixture) ->
            Nodes = Fixture#fixture.nodes,
            Extra = Fixture#fixture.extra,
            CollectTrailFn = maps:get(collect_trail_fn, Extra),

            ContractId = <<"audit-test-contract">>,

            %% Generate operations on each node
            NumOpsPerNode = 5,
            lists:foreach(fun(Node) ->
                lists:foreach(fun(N) ->
                    FileName = list_to_binary(io_lib:format("audit-~s-~p.txt",
                        [Node, N])),
                    rpc:call(Node, gcp_governed, storage_upload,
                        [ContractId, <<"test-bucket">>, FileName,
                         <<"text/plain">>, <<"data">>], 5000)
                end, lists:seq(1, NumOpsPerNode))
            end, Nodes),

            %% Collect distributed audit trail
            {ok, MergedTrail} = CollectTrailFn(ContractId),

            ExpectedReceipts = length(Nodes) * NumOpsPerNode,

            #{
                source_nodes => maps:get(source_nodes, MergedTrail, 0),
                total_receipts => maps:get(total_receipts, MergedTrail, 0),
                unique_receipts => maps:get(unique_receipts, MergedTrail, 0),
                expected_receipts => ExpectedReceipts,
                complete => maps:get(unique_receipts, MergedTrail, 0) >= ExpectedReceipts
            }
        end)
    end).

-spec scenario_contract_enforcement_across_nodes() -> #scenario_result{}.
%% @doc Verify contract enforcement works across all nodes.
scenario_contract_enforcement_across_nodes() ->
    run_with_timing(contract_enforcement_across_nodes, fun() ->
        tc_fixtures:with_fixture({minimal_cluster, #{num_nodes => 3}}, fun(Fixture) ->
            Nodes = Fixture#fixture.nodes,

            ContractId = <<"enforce-test-contract">>,

            %% Test that all nodes enforce contracts
            Results = lists:map(fun(Node) ->
                %% Operation should succeed with valid contract
                Result = rpc:call(Node, gcp_governed, storage_upload,
                    [ContractId, <<"test-bucket">>, <<"test.txt">>,
                     <<"text/plain">>, <<"data">>], 5000),

                case Result of
                    {ok, _, Receipt} ->
                        %% Verify receipt was generated
                        HasReceipt = is_tuple(Receipt),
                        {Node, {success, HasReceipt}};
                    {error, Reason} ->
                        {Node, {error, Reason}}
                end
            end, Nodes),

            AllEnforced = lists:all(fun({_, {success, true}}) -> true; (_) -> false end, Results),

            #{
                all_nodes_enforced => AllEnforced,
                node_results => maps:from_list(Results)
            }
        end)
    end).

%%====================================================================
%% Coordination Scenarios
%%====================================================================

-spec scenario_global_kill_switch() -> #scenario_result{}.
%% @doc Test global kill switch activation and propagation.
scenario_global_kill_switch() ->
    run_with_timing(global_kill_switch, fun() ->
        tc_fixtures:with_fixture({killswitch_cluster, #{num_nodes => 4}}, fun(Fixture) ->
            Nodes = Fixture#fixture.nodes,
            Extra = Fixture#fixture.extra,
            ActivateFn = maps:get(activate_fn, Extra),
            DeactivateFn = maps:get(deactivate_fn, Extra),
            VerifyFn = maps:get(verify_fn, Extra),

            %% Verify initially inactive
            {ok, InitialState} = VerifyFn(),
            InitiallyInactive = maps:get(all_inactive, InitialState, false),

            %% Activate global kill switch
            ok = ActivateFn(),
            timer:sleep(1000),

            %% Verify all nodes activated
            {ok, ActiveState} = VerifyFn(),
            AllActive = maps:get(all_active, ActiveState, false),

            %% Verify operations are blocked
            Blocked = lists:all(fun(Node) ->
                case rpc:call(Node, gcp_governed, storage_upload,
                    [<<"test">>, <<"bucket">>, <<"file">>, <<"text/plain">>, <<"data">>], 5000) of
                    {error, _} -> true;
                    _ -> false
                end
            end, Nodes),

            %% Deactivate
            ok = DeactivateFn(),
            timer:sleep(1000),

            %% Verify all nodes deactivated
            {ok, FinalState} = VerifyFn(),
            AllDeactivated = maps:get(all_inactive, FinalState, false),

            #{
                initially_inactive => InitiallyInactive,
                all_activated => AllActive,
                operations_blocked => Blocked,
                all_deactivated => AllDeactivated,
                propagation_consistent => AllActive andalso AllDeactivated
            }
        end)
    end).

-spec scenario_federated_policy_consensus() -> #scenario_result{}.
%% @doc Test federated policy deployment and evaluation.
scenario_federated_policy_consensus() ->
    run_with_timing(federated_policy_consensus, fun() ->
        tc_fixtures:with_fixture({policy_federation, #{num_nodes => 3}}, fun(Fixture) ->
            Extra = Fixture#fixture.extra,
            DeployFn = maps:get(deploy_policy_fn, Extra),
            EvaluateFn = maps:get(evaluate_fn, Extra),

            %% Create and deploy policy
            PolicyId = <<"test-policy">>,
            Policy = #{
                name => PolicyId,
                rules => [
                    #{type => tier_rule, allowed => [tier_a_automate, tier_b_assist]},
                    #{type => operation_blacklist, ops => [<<"dangerous.operation">>]}
                ]
            },

            ok = DeployFn(PolicyId, Policy),
            timer:sleep(500),

            %% Test allowed operation
            AllowedResult = EvaluateFn(PolicyId, <<"safe.operation">>,
                #{tier => tier_a_automate}),

            %% Test denied operation
            DeniedResult = EvaluateFn(PolicyId, <<"dangerous.operation">>,
                #{tier => tier_a_automate}),

            #{
                policy_deployed => true,
                allowed_result => AllowedResult,
                denied_result => DeniedResult,
                policy_correct => AllowedResult =:= allow andalso
                                  element(1, DeniedResult) =:= deny
            }
        end)
    end).

-spec scenario_kill_switch_during_partition() -> #scenario_result{}.
%% @doc Test kill switch behavior during network partition.
scenario_kill_switch_during_partition() ->
    run_with_timing(kill_switch_during_partition, fun() ->
        tc_fixtures:with_fixture({full_governance, #{num_nodes => 4}}, fun(Fixture) ->
            Nodes = Fixture#fixture.nodes,
            Extra = Fixture#fixture.extra,
            PartitionFn = maps:get(partition_fn, Extra),
            HealFn = maps:get(heal_fn, Extra),
            ActivateFn = maps:get(activate_killswitch_fn, Extra),

            {GroupA, GroupB} = lists:split(2, Nodes),

            %% Create partition
            ok = PartitionFn(GroupA, GroupB),
            timer:sleep(1000),

            %% Try to activate global kill switch (should only reach reachable nodes)
            ActivateFn(),
            timer:sleep(500),

            %% Check states during partition
            GroupAStates = lists:map(fun(Node) ->
                rpc:call(Node, gcp_governed, is_kill_switch_active, [], 2000)
            end, GroupA),

            GroupBStates = lists:map(fun(Node) ->
                rpc:call(Node, gcp_governed, is_kill_switch_active, [], 2000)
            end, GroupB),

            %% Heal partition
            ok = HealFn(GroupA, GroupB),
            timer:sleep(2000),

            %% Re-sync kill switch state
            ActivateFn(),
            timer:sleep(500),

            %% Verify consistent after heal
            {ok, FinalState} = (maps:get(verify_killswitch_fn, Extra))(),

            #{
                group_a_states_during_partition => GroupAStates,
                group_b_states_during_partition => GroupBStates,
                consistent_after_heal => maps:get(consistent, FinalState, false)
            }
        end)
    end).

%%====================================================================
%% Integration Scenarios
%%====================================================================

-spec scenario_gcp_multi_region() -> #scenario_result{}.
%% @doc Test GCP operations across multiple simulated regions.
scenario_gcp_multi_region() ->
    run_with_timing(gcp_multi_region, fun() ->
        tc_fixtures:with_fixture({gcp_sandbox, #{num_nodes => 3}}, fun(Fixture) ->
            Nodes = Fixture#fixture.nodes,
            Extra = Fixture#fixture.extra,
            StorageFn = maps:get(storage_fn, Extra),
            GovernedFn = maps:get(governed_fn, Extra),

            %% Simulate different regions
            Regions = [<<"us-east1">>, <<"eu-west1">>, <<"asia-east1">>],
            NodeRegions = lists:zip(Nodes, Regions),

            %% Create buckets in each "region"
            BucketResults = lists:map(fun({Node, Region}) ->
                BucketName = <<"bucket-", Region/binary>>,
                Result = StorageFn(Node, create_bucket,
                    [<<"test-project">>, BucketName, #{region => Region}]),
                {Region, Result}
            end, NodeRegions),

            %% Cross-region operations via governance
            ContractId = <<"multi-region-contract">>,
            [Node1 | _] = Nodes,

            CrossRegionOps = lists:map(fun({_Node, Region}) ->
                BucketName = <<"bucket-", Region/binary>>,
                FileName = <<"cross-region-", Region/binary, ".txt">>,
                Result = GovernedFn(Node1, storage_upload,
                    [ContractId, BucketName, FileName, <<"text/plain">>, <<"data">>]),
                {Region, Result}
            end, NodeRegions),

            #{
                regions => Regions,
                bucket_results => maps:from_list(BucketResults),
                cross_region_operations => maps:from_list(CrossRegionOps),
                all_regions_accessible => lists:all(fun({_, {ok, _, _}}) -> true; (_) -> false end, CrossRegionOps)
            }
        end)
    end).

-spec scenario_state_checkpoint_restore() -> #scenario_result{}.
%% @doc Test state checkpoint and restore functionality.
scenario_state_checkpoint_restore() ->
    run_with_timing(state_checkpoint_restore, fun() ->
        tc_fixtures:with_fixture({persistent_cluster, #{num_nodes => 2}}, fun(Fixture) ->
            [Node1, _Node2] = Fixture#fixture.nodes,
            Extra = Fixture#fixture.extra,
            CheckpointFn = maps:get(checkpoint_fn, Extra),
            RestoreFn = maps:get(restore_fn, Extra),

            %% Perform some operations
            ContractId = <<"checkpoint-test">>,
            rpc:call(Node1, gcp_governed, storage_upload,
                [ContractId, <<"bucket">>, <<"before-checkpoint.txt">>,
                 <<"text/plain">>, <<"original data">>], 5000),

            %% Create checkpoint
            {ok, CheckpointId} = CheckpointFn(Node1),

            %% Perform more operations after checkpoint
            rpc:call(Node1, gcp_governed, storage_upload,
                [ContractId, <<"bucket">>, <<"after-checkpoint.txt">>,
                 <<"text/plain">>, <<"new data">>], 5000),

            %% Restore from checkpoint
            {ok, RestoredNode} = RestoreFn(CheckpointId),

            %% Verify restored state
            %% The "after-checkpoint.txt" should not exist in restored node
            BeforeExists = case rpc:call(RestoredNode, gcp_governed, storage_download,
                [ContractId, <<"bucket">>, <<"before-checkpoint.txt">>], 5000) of
                {ok, _, _} -> true;
                _ -> false
            end,

            AfterExists = case rpc:call(RestoredNode, gcp_governed, storage_download,
                [ContractId, <<"bucket">>, <<"after-checkpoint.txt">>], 5000) of
                {ok, _, _} -> true;
                _ -> false
            end,

            #{
                checkpoint_id => CheckpointId,
                restored_node => RestoredNode,
                before_checkpoint_exists => BeforeExists,
                after_checkpoint_missing => not AfterExists,
                restore_successful => BeforeExists andalso not AfterExists
            }
        end)
    end).

-spec scenario_full_governance_flow() -> #scenario_result{}.
%% @doc Complete end-to-end governance test with all features.
scenario_full_governance_flow() ->
    run_with_timing(full_governance_flow, fun() ->
        tc_fixtures:with_fixture({full_governance, #{num_nodes => 5}}, fun(Fixture) ->
            Nodes = Fixture#fixture.nodes,
            Extra = Fixture#fixture.extra,

            ContractId = <<"full-governance-test">>,
            Results = #{},

            %% Step 1: Deploy federated policy
            PolicyId = <<"governance-policy">>,
            DeployFn = maps:get(deploy_policy_fn, Extra),
            ok = DeployFn(PolicyId, #{
                name => PolicyId,
                rules => [#{type => tier_rule, allowed => [tier_a_automate]}]
            }),
            Results1 = Results#{policy_deployed => true},

            %% Step 2: Perform governed operations across nodes
            OpResults = lists:map(fun(Node) ->
                rpc:call(Node, gcp_governed, storage_upload,
                    [ContractId, <<"bucket">>, <<"file.txt">>,
                     <<"text/plain">>, <<"data">>], 5000)
            end, Nodes),
            Results2 = Results1#{operations_completed => length([R || {ok, _, _} = R <- OpResults])},

            %% Step 3: Verify receipt chain
            VerifyChainFn = maps:get(verify_chain_fn, Extra),
            {ok, ChainVerification} = VerifyChainFn(ContractId),
            Results3 = Results2#{chain_valid => maps:get(valid, ChainVerification, false)},

            %% Step 4: Inject chaos (partition)
            PartitionFn = maps:get(partition_fn, Extra),
            {GroupA, GroupB} = lists:split(2, Nodes),
            ok = PartitionFn(GroupA, GroupB),
            Results4 = Results3#{partition_created => true},

            %% Step 5: Activate kill switch
            ActivateFn = maps:get(activate_killswitch_fn, Extra),
            ActivateFn(),
            Results5 = Results4#{kill_switch_activated => true},

            %% Step 6: Heal partition
            HealFn = maps:get(heal_fn, Extra),
            ok = HealFn(GroupA, GroupB),
            timer:sleep(2000),
            Results6 = Results5#{partition_healed => true},

            %% Step 7: Verify consistent state
            VerifyKillFn = maps:get(verify_killswitch_fn, Extra),
            {ok, KillState} = VerifyKillFn(),
            Results7 = Results6#{kill_switch_consistent => maps:get(consistent, KillState, false)},

            %% Step 8: Collect distributed audit
            CollectFn = maps:get(collect_trail_fn, Extra),
            {ok, AuditTrail} = CollectFn(ContractId),
            Results8 = Results7#{audit_collected => maps:get(unique_receipts, AuditTrail, 0) > 0},

            %% Final: Deactivate kill switch
            DeactivateFn = maps:get(deactivate_killswitch_fn, Extra),
            DeactivateFn(),

            Results8#{
                full_flow_completed => true,
                nodes_tested => length(Nodes)
            }
        end)
    end).

%%====================================================================
%% Runner Functions
%%====================================================================

-spec run_scenario(atom()) -> #scenario_result{}.
run_scenario(ScenarioName) ->
    case ScenarioName of
        network_partition_recovery -> scenario_network_partition_recovery();
        node_failure_recovery -> scenario_node_failure_recovery();
        split_brain_resolution -> scenario_split_brain_resolution();
        receipt_chain_integrity -> scenario_receipt_chain_integrity();
        distributed_audit_collection -> scenario_distributed_audit_collection();
        contract_enforcement_across_nodes -> scenario_contract_enforcement_across_nodes();
        global_kill_switch -> scenario_global_kill_switch();
        federated_policy_consensus -> scenario_federated_policy_consensus();
        kill_switch_during_partition -> scenario_kill_switch_during_partition();
        gcp_multi_region -> scenario_gcp_multi_region();
        state_checkpoint_restore -> scenario_state_checkpoint_restore();
        full_governance_flow -> scenario_full_governance_flow();
        _ -> #scenario_result{name = ScenarioName, status = skipped,
                             duration_ms = 0, details = #{}, errors = [{unknown_scenario, ScenarioName}]}
    end.

-spec run_all_scenarios() -> [#scenario_result{}].
run_all_scenarios() ->
    Scenarios = [
        network_partition_recovery,
        node_failure_recovery,
        split_brain_resolution,
        receipt_chain_integrity,
        distributed_audit_collection,
        contract_enforcement_across_nodes,
        global_kill_switch,
        federated_policy_consensus,
        kill_switch_during_partition,
        gcp_multi_region,
        state_checkpoint_restore,
        full_governance_flow
    ],
    [run_scenario(S) || S <- Scenarios].

-spec run_category(atom()) -> [#scenario_result{}].
run_category(resilience) ->
    [run_scenario(S) || S <- [network_partition_recovery, node_failure_recovery, split_brain_resolution]];
run_category(governance) ->
    [run_scenario(S) || S <- [receipt_chain_integrity, distributed_audit_collection, contract_enforcement_across_nodes]];
run_category(coordination) ->
    [run_scenario(S) || S <- [global_kill_switch, federated_policy_consensus, kill_switch_during_partition]];
run_category(integration) ->
    [run_scenario(S) || S <- [gcp_multi_region, state_checkpoint_restore, full_governance_flow]];
run_category(_) ->
    [].

%%====================================================================
%% Internal Functions
%%====================================================================

run_with_timing(Name, Fun) ->
    StartTime = erlang:monotonic_time(millisecond),
    try
        Details = Fun(),
        #scenario_result{
            name = Name,
            status = passed,
            duration_ms = erlang:monotonic_time(millisecond) - StartTime,
            details = Details,
            errors = []
        }
    catch
        Class:Reason:Stack ->
            #scenario_result{
                name = Name,
                status = failed,
                duration_ms = erlang:monotonic_time(millisecond) - StartTime,
                details = #{},
                errors = [{Class, Reason, Stack}]
            }
    end.
