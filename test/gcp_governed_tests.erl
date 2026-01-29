%%%-------------------------------------------------------------------
%% @doc Governed GCP Client Tests
%%
%% Integration tests for governance-wrapped GCP operations.
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_governed_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp_governance.hrl").
-include("gcp_simulator.hrl").

%% Only export test generators to prevent running tests twice
-export([
    governed_test_/0,
    pubsub_governed_test_/0,
    compute_governed_test_/0
]).

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    %% Start all required services
    {ok, _} = gcp_storage_sim:start_link(),
    {ok, _} = gcp_pubsub_sim:start_link(),
    {ok, _} = gcp_compute_sim:start_link(),
    {ok, _} = gcp_governed:start_link(),

    %% Create test bucket and topic
    gcp_storage_sim:create_bucket(<<"test-project">>, <<"test-bucket">>),
    gcp_pubsub_sim:create_topic(<<"test-project">>, <<"test-topic">>),
    gcp_pubsub_sim:create_subscription(<<"test-project">>, <<"test-sub">>,
        <<"projects/test-project/topics/test-topic">>),
    ok.

cleanup(_) ->
    gcp_governed:stop(),
    gcp_storage_sim:stop(),
    gcp_pubsub_sim:stop(),
    gcp_compute_sim:stop(),
    ok.

%%====================================================================
%% Storage Tests
%%====================================================================

governed_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Storage upload with receipt", fun storage_upload_receipt_/0},
        {"Storage download with receipt", fun storage_download_receipt_/0},
        {"Multiple operations create chain", fun receipt_chain_/0},
        {"Kill switch blocks operations", fun kill_switch_/0},
        {"Audit trail export", fun audit_trail_/0},
        {"Policy evaluation", fun policy_evaluation_/0}
    ]}.

storage_upload_receipt_() ->
    ContractId = <<"test-contract-1">>,
    Data = <<"Hello, Governed GCP!">>,

    {ok, Obj, Receipt} = gcp_governed:storage_upload(
        ContractId, <<"test-bucket">>, <<"governed.txt">>,
        <<"text/plain">>, Data),

    %% Verify object created
    ?assertEqual(<<"governed.txt">>, Obj#gcp_object.name),

    %% Verify receipt
    ?assertEqual(ContractId, Receipt#mcp_receipt.contract_id),
    ?assertEqual(32, byte_size(Receipt#mcp_receipt.request_hash)),
    ?assertEqual(32, byte_size(Receipt#mcp_receipt.response_hash)),
    ?assert(Receipt#mcp_receipt.timestamp > 0),
    ok.

storage_download_receipt_() ->
    ContractId = <<"test-contract-2">>,

    %% Upload first
    {ok, _, _} = gcp_governed:storage_upload(
        ContractId, <<"test-bucket">>, <<"download-test.txt">>,
        <<"text/plain">>, <<"download me">>),

    %% Download with governance
    {ok, Data, Receipt} = gcp_governed:storage_download(
        ContractId, <<"test-bucket">>, <<"download-test.txt">>),

    ?assertEqual(<<"download me">>, Data),
    ?assertEqual(ContractId, Receipt#mcp_receipt.contract_id),
    ok.

receipt_chain_() ->
    ContractId = <<"chain-contract">>,

    %% Perform multiple operations
    {ok, _, R1} = gcp_governed:storage_upload(
        ContractId, <<"test-bucket">>, <<"chain1.txt">>,
        <<"text/plain">>, <<"data1">>),
    {ok, _, R2} = gcp_governed:storage_upload(
        ContractId, <<"test-bucket">>, <<"chain2.txt">>,
        <<"text/plain">>, <<"data2">>),
    {ok, _, R3} = gcp_governed:storage_upload(
        ContractId, <<"test-bucket">>, <<"chain3.txt">>,
        <<"text/plain">>, <<"data3">>),

    %% Verify sequence numbers increase
    ?assert(R2#mcp_receipt.sequence > R1#mcp_receipt.sequence),
    ?assert(R3#mcp_receipt.sequence > R2#mcp_receipt.sequence),

    %% Verify chaining (each receipt references previous)
    ?assertEqual(32, byte_size(R2#mcp_receipt.previous_hash)),
    ?assertEqual(32, byte_size(R3#mcp_receipt.previous_hash)),
    ok.

kill_switch_() ->
    ContractId = <<"kill-switch-contract">>,

    %% Operation should work
    {ok, _, _} = gcp_governed:storage_upload(
        ContractId, <<"test-bucket">>, <<"before-kill.txt">>,
        <<"text/plain">>, <<"before">>),

    %% Activate kill switch
    gen_server:call(gcp_governed, {activate_kill_switch}),

    %% Operation should be refused
    Result = gcp_governed:storage_upload(
        ContractId, <<"test-bucket">>, <<"after-kill.txt">>,
        <<"text/plain">>, <<"after">>),

    ?assertMatch({error, #mcp_refusal{}}, Result),
    {error, Refusal} = Result,
    ?assertEqual(?REFUSAL_KILL_SWITCH_ACTIVE, Refusal#mcp_refusal.code),

    %% Deactivate kill switch
    gen_server:call(gcp_governed, {deactivate_kill_switch}),

    %% Operation should work again
    {ok, _, _} = gcp_governed:storage_upload(
        ContractId, <<"test-bucket">>, <<"after-deactivate.txt">>,
        <<"text/plain">>, <<"after deactivate">>),
    ok.

audit_trail_() ->
    ContractId = <<"audit-contract">>,

    %% Perform operations
    {ok, _, _} = gcp_governed:storage_upload(
        ContractId, <<"test-bucket">>, <<"audit1.txt">>,
        <<"text/plain">>, <<"audit data 1">>),
    {ok, _, _} = gcp_governed:storage_upload(
        ContractId, <<"test-bucket">>, <<"audit2.txt">>,
        <<"text/plain">>, <<"audit data 2">>),

    %% Export audit trail
    {ok, TrailJson} = gcp_governed:export_audit_trail(ContractId),

    %% Verify it's valid JSON
    Trail = jsx:decode(TrailJson, [return_maps]),
    ?assertEqual(ContractId, maps:get(<<"contract_id">>, Trail)),
    ?assertEqual(2, maps:get(<<"receipt_count">>, Trail)),

    Receipts = maps:get(<<"receipts">>, Trail),
    ?assertEqual(2, length(Receipts)),
    ok.

%%====================================================================
%% Policy Engine Tests
%%====================================================================

policy_evaluation_() ->
    %% Create a restrictive policy
    Policy = gcp_policy_engine:create_policy(#{
        name => <<"test-policy">>,
        rules => [
            gcp_policy_engine:tier_rule([tier_a_automate, tier_b_assist]),
            gcp_policy_engine:operation_blacklist([<<"compute.instances.delete">>]),
            gcp_policy_engine:max_instances(5)
        ]
    }),

    %% Test allowed operation
    Context1 = #{tier => tier_b_assist, instance_count => 2},
    ?assertEqual(allow, gcp_policy_engine:evaluate(Policy, <<"storage.objects.create">>, Context1)),

    %% Test denied by tier
    Context2 = #{tier => tier_c_do_not_automate},
    ?assertMatch({deny, _}, gcp_policy_engine:evaluate(Policy, <<"storage.objects.create">>, Context2)),

    %% Test denied by blacklist
    Context3 = #{tier => tier_a_automate},
    ?assertMatch({deny, _}, gcp_policy_engine:evaluate(Policy, <<"compute.instances.delete">>, Context3)),

    %% Test denied by instance limit
    Context4 = #{tier => tier_a_automate, instance_count => 5},
    ?assertMatch({deny, _}, gcp_policy_engine:evaluate(Policy, <<"compute.instances.create">>, Context4)),
    ok.

%%====================================================================
%% Pub/Sub Tests
%%====================================================================

pubsub_governed_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Pubsub publish with receipt", fun pubsub_publish_/0},
        {"Pubsub pull with receipt", fun pubsub_pull_/0}
    ]}.

pubsub_publish_() ->
    ContractId = <<"pubsub-contract">>,

    {ok, MsgIds, Receipt} = gcp_governed:pubsub_publish(
        ContractId, <<"test-project">>, <<"test-topic">>,
        [#{data => <<"message 1">>}, #{data => <<"message 2">>}]),

    ?assertEqual(2, length(MsgIds)),
    ?assertEqual(ContractId, Receipt#mcp_receipt.contract_id),
    ok.

pubsub_pull_() ->
    ContractId = <<"pubsub-pull-contract">>,

    %% Publish first
    {ok, _, _} = gcp_governed:pubsub_publish(
        ContractId, <<"test-project">>, <<"test-topic">>,
        [#{data => <<"pull test">>}]),

    %% Pull with governance
    {ok, Messages, Receipt} = gcp_governed:pubsub_pull(
        ContractId, <<"test-project">>, <<"test-sub">>, 10),

    ?assert(length(Messages) >= 1),
    ?assertEqual(ContractId, Receipt#mcp_receipt.contract_id),
    ok.

%%====================================================================
%% Compute Tests
%%====================================================================

compute_governed_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Compute create with receipt", fun compute_create_/0}
    ]}.

compute_create_() ->
    ContractId = <<"compute-contract">>,

    {ok, Instance, Receipt} = gcp_governed:compute_create(
        ContractId, <<"test-project">>, <<"us-central1-a">>, <<"governed-vm">>,
        #{machine_type => <<"e2-micro">>}),

    ?assertEqual(<<"governed-vm">>, Instance#gcp_instance.name),
    ?assertEqual(ContractId, Receipt#mcp_receipt.contract_id),
    ok.
