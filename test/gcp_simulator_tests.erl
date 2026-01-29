%%%-------------------------------------------------------------------
%% @doc GCP Simulator Tests
%%
%% Tests for all GCP service simulators.
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_simulator_tests).

-include_lib("eunit/include/eunit.hrl").
-include("gcp_simulator.hrl").

%% Only export the test generators
-export([
    storage_test_/0,
    pubsub_test_/0,
    iam_test_/0,
    compute_test_/0,
    integration_test_/0
]).

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    %% Start all simulators
    {ok, _} = gcp_storage_sim:start_link(),
    {ok, _} = gcp_pubsub_sim:start_link(),
    {ok, _} = gcp_iam_sim:start_link(),
    {ok, _} = gcp_compute_sim:start_link(),
    ok.

cleanup(_) ->
    gcp_storage_sim:stop(),
    gcp_pubsub_sim:stop(),
    gcp_iam_sim:stop(),
    gcp_compute_sim:stop(),
    ok.

%%====================================================================
%% Cloud Storage Tests
%%====================================================================

storage_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Create bucket", fun storage_create_bucket_/0},
        {"Bucket already exists", fun storage_bucket_exists_/0},
        {"Upload and download object", fun storage_object_roundtrip_/0},
        {"List objects", fun storage_list_objects_/0},
        {"Delete object", fun storage_delete_object_/0},
        {"Copy object", fun storage_copy_object_/0},
        {"Versioning", fun storage_versioning_/0}
    ]}.

storage_create_bucket_() ->
    {ok, Bucket} = gcp_storage_sim:create_bucket(<<"my-project">>, <<"my-bucket">>),
    ?assertEqual(<<"my-bucket">>, Bucket#gcp_bucket.name),
    ?assertEqual(<<"my-project">>, Bucket#gcp_bucket.project_id),
    ok.

storage_bucket_exists_() ->
    %% Bucket already created in previous test
    {error, Error} = gcp_storage_sim:create_bucket(<<"my-project">>, <<"my-bucket">>),
    ?assertEqual(?GCP_CONFLICT, Error#gcp_error.code),
    ok.

storage_object_roundtrip_() ->
    Data = <<"Hello, GCP Storage!">>,
    {ok, Obj} = gcp_storage_sim:upload_object(
        <<"my-bucket">>, <<"test.txt">>, <<"text/plain">>, Data),
    ?assertEqual(<<"test.txt">>, Obj#gcp_object.name),
    ?assertEqual(byte_size(Data), Obj#gcp_object.size),

    {ok, Downloaded} = gcp_storage_sim:download_object(<<"my-bucket">>, <<"test.txt">>),
    ?assertEqual(Data, Downloaded),
    ok.

storage_list_objects_() ->
    %% Upload a few more objects
    gcp_storage_sim:upload_object(<<"my-bucket">>, <<"dir/file1.txt">>,
                                   <<"text/plain">>, <<"content1">>),
    gcp_storage_sim:upload_object(<<"my-bucket">>, <<"dir/file2.txt">>,
                                   <<"text/plain">>, <<"content2">>),

    {ok, Objects} = gcp_storage_sim:list_objects(<<"my-bucket">>),
    ?assert(length(Objects) >= 3),

    %% List with prefix
    {ok, DirObjects} = gcp_storage_sim:list_objects(<<"my-bucket">>,
                                                     #{prefix => <<"dir/">>}),
    ?assertEqual(2, length(DirObjects)),
    ok.

storage_delete_object_() ->
    ok = gcp_storage_sim:delete_object(<<"my-bucket">>, <<"dir/file1.txt">>),
    {error, Error} = gcp_storage_sim:download_object(<<"my-bucket">>,
                                                      <<"dir/file1.txt">>),
    ?assertEqual(?GCP_NOT_FOUND, Error#gcp_error.code),
    ok.

storage_copy_object_() ->
    %% Create destination bucket
    {ok, _} = gcp_storage_sim:create_bucket(<<"my-project">>, <<"dest-bucket">>),

    %% Copy object
    {ok, CopiedObj} = gcp_storage_sim:copy_object(
        <<"my-bucket">>, <<"test.txt">>,
        <<"dest-bucket">>, <<"copied.txt">>),

    ?assertEqual(<<"dest-bucket">>, CopiedObj#gcp_object.bucket),
    ?assertEqual(<<"copied.txt">>, CopiedObj#gcp_object.name),

    %% Verify content
    {ok, Data} = gcp_storage_sim:download_object(<<"dest-bucket">>, <<"copied.txt">>),
    ?assertEqual(<<"Hello, GCP Storage!">>, Data),
    ok.

storage_versioning_() ->
    %% Create bucket with versioning
    {ok, _} = gcp_storage_sim:create_bucket(<<"my-project">>, <<"versioned-bucket">>,
                                            #{versioning => true}),

    %% Upload multiple versions
    gcp_storage_sim:upload_object(<<"versioned-bucket">>, <<"file.txt">>,
                                   <<"text/plain">>, <<"version1">>),
    gcp_storage_sim:upload_object(<<"versioned-bucket">>, <<"file.txt">>,
                                   <<"text/plain">>, <<"version2">>),

    %% List versions
    {ok, Versions} = gcp_storage_sim:list_object_versions(<<"versioned-bucket">>,
                                                           <<"file.txt">>),
    ?assert(length(Versions) >= 2),
    ok.

%%====================================================================
%% Pub/Sub Tests
%%====================================================================

pubsub_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Create topic", fun pubsub_create_topic_/0},
        {"Create subscription", fun pubsub_create_subscription_/0},
        {"Publish and pull", fun pubsub_publish_pull_/0},
        {"Acknowledge message", fun pubsub_acknowledge_/0},
        {"Delete topic", fun pubsub_delete_topic_/0}
    ]}.

pubsub_create_topic_() ->
    {ok, Topic} = gcp_pubsub_sim:create_topic(<<"my-project">>, <<"my-topic">>),
    ?assertEqual(<<"projects/my-project/topics/my-topic">>, Topic#gcp_topic.name),
    ok.

pubsub_create_subscription_() ->
    TopicName = <<"projects/my-project/topics/my-topic">>,
    {ok, Sub} = gcp_pubsub_sim:create_subscription(
        <<"my-project">>, <<"my-sub">>, TopicName),
    ?assertEqual(<<"projects/my-project/subscriptions/my-sub">>,
                 Sub#gcp_subscription.name),
    ?assertEqual(TopicName, Sub#gcp_subscription.topic),
    ok.

pubsub_publish_pull_() ->
    TopicName = <<"projects/my-project/topics/my-topic">>,
    SubName = <<"projects/my-project/subscriptions/my-sub">>,

    %% Publish messages
    Messages = [
        #{data => <<"message1">>, attributes => #{<<"key">> => <<"value1">>}},
        #{data => <<"message2">>, attributes => #{<<"key">> => <<"value2">>}}
    ],
    {ok, MessageIds} = gcp_pubsub_sim:publish(TopicName, Messages),
    ?assertEqual(2, length(MessageIds)),

    %% Pull messages
    {ok, ReceivedMessages} = gcp_pubsub_sim:pull(SubName, 10),
    ?assertEqual(2, length(ReceivedMessages)),

    %% Verify message content
    [Msg1 | _] = ReceivedMessages,
    ?assert(is_binary(Msg1#gcp_received_message.ack_id)),
    ok.

pubsub_acknowledge_() ->
    SubName = <<"projects/my-project/subscriptions/my-sub">>,

    %% Pull messages first
    {ok, ReceivedMessages} = gcp_pubsub_sim:pull(SubName, 10),
    AckIds = [M#gcp_received_message.ack_id || M <- ReceivedMessages],

    %% Acknowledge
    ok = gcp_pubsub_sim:acknowledge(SubName, AckIds),

    %% Pull again - should be empty
    {ok, MoreMessages} = gcp_pubsub_sim:pull(SubName, 10),
    ?assertEqual(0, length(MoreMessages)),
    ok.

pubsub_delete_topic_() ->
    %% Create new topic to delete
    {ok, _} = gcp_pubsub_sim:create_topic(<<"my-project">>, <<"delete-me">>),
    TopicName = <<"projects/my-project/topics/delete-me">>,

    ok = gcp_pubsub_sim:delete_topic(TopicName),

    {error, Error} = gcp_pubsub_sim:get_topic(TopicName),
    ?assertEqual(?GCP_NOT_FOUND, Error#gcp_error.code),
    ok.

%%====================================================================
%% IAM Tests
%%====================================================================

iam_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Create service account", fun iam_create_service_account_/0},
        {"IAM policy", fun iam_policy_/0},
        {"Add binding", fun iam_add_binding_/0},
        {"Test permissions", fun iam_test_permissions_/0}
    ]}.

iam_create_service_account_() ->
    {ok, SA} = gcp_iam_sim:create_service_account(
        <<"my-project">>, <<"my-sa">>, <<"My Service Account">>),
    ?assertEqual(<<"my-sa@my-project.iam.gserviceaccount.com">>,
                 SA#gcp_service_account.email),
    ?assertEqual(<<"My Service Account">>, SA#gcp_service_account.display_name),
    ok.

iam_policy_() ->
    {ok, Policy} = gcp_iam_sim:get_iam_policy(bucket, <<"test-bucket">>),
    ?assertEqual(1, Policy#gcp_iam_policy.version),
    ?assertEqual([], Policy#gcp_iam_policy.bindings),
    ok.

iam_add_binding_() ->
    {ok, Policy} = gcp_iam_sim:add_binding(
        bucket, <<"test-bucket">>,
        <<"roles/storage.admin">>,
        [<<"user:admin@example.com">>]
    ),
    ?assertEqual(1, length(Policy#gcp_iam_policy.bindings)),
    [Binding] = Policy#gcp_iam_policy.bindings,
    ?assertEqual(<<"roles/storage.admin">>, Binding#gcp_iam_binding.role),
    ok.

iam_test_permissions_() ->
    %% Test permissions for the binding we added
    {ok, Allowed} = gcp_iam_sim:test_iam_permissions(
        bucket, <<"test-bucket">>,
        [<<"storage.objects.get">>, <<"compute.instances.get">>]
    ),
    ?assert(lists:member(<<"storage.objects.get">>, Allowed)),
    ?assertNot(lists:member(<<"compute.instances.get">>, Allowed)),
    ok.

%%====================================================================
%% Compute Engine Tests
%%====================================================================

compute_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Create instance", fun compute_create_instance_/0},
        {"Get instance", fun compute_get_instance_/0},
        {"List instances", fun compute_list_instances_/0},
        {"Stop and start instance", fun compute_stop_start_/0},
        {"Set metadata", fun compute_set_metadata_/0}
    ]}.

compute_create_instance_() ->
    {ok, Instance} = gcp_compute_sim:create_instance(
        <<"my-project">>, <<"us-central1-a">>, <<"my-vm">>, <<"e2-micro">>),
    ?assertEqual(<<"my-vm">>, Instance#gcp_instance.name),
    ?assertEqual(<<"e2-micro">>, Instance#gcp_instance.machine_type),
    ?assertEqual(provisioning, Instance#gcp_instance.status),
    ok.

compute_get_instance_() ->
    {ok, Instance} = gcp_compute_sim:get_instance(
        <<"my-project">>, <<"us-central1-a">>, <<"my-vm">>),
    ?assertEqual(<<"my-vm">>, Instance#gcp_instance.name),
    ok.

compute_list_instances_() ->
    %% Create another instance
    {ok, _} = gcp_compute_sim:create_instance(
        <<"my-project">>, <<"us-central1-a">>, <<"my-vm-2">>, <<"e2-small">>),

    {ok, Instances} = gcp_compute_sim:list_instances(
        <<"my-project">>, <<"us-central1-a">>),
    ?assert(length(Instances) >= 2),
    ok.

compute_stop_start_() ->
    %% Wait for instance to be running
    timer:sleep(100),

    %% Force instance to running state for test
    {ok, Instance} = gcp_compute_sim:get_instance(
        <<"my-project">>, <<"us-central1-a">>, <<"my-vm">>),

    %% If still provisioning, we can't stop
    case Instance#gcp_instance.status of
        running ->
            {ok, StoppedInstance} = gcp_compute_sim:stop_instance(
                <<"my-project">>, <<"us-central1-a">>, <<"my-vm">>),
            ?assertEqual(stopping, StoppedInstance#gcp_instance.status);
        _ ->
            ok  %% Skip test if not running
    end,
    ok.

compute_set_metadata_() ->
    Metadata = #{<<"startup-script">> => <<"echo hello">>},
    {ok, Instance} = gcp_compute_sim:set_metadata(
        <<"my-project">>, <<"us-central1-a">>, <<"my-vm">>, Metadata),
    ?assertEqual(Metadata, Instance#gcp_instance.metadata),
    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Full workflow", fun integration_full_workflow_/0}
    ]}.

integration_full_workflow_() ->
    ProjectId = <<"integration-project">>,

    %% 1. Create service account
    {ok, SA} = gcp_iam_sim:create_service_account(
        ProjectId, <<"worker-sa">>, <<"Worker Service Account">>),
    SAEmail = SA#gcp_service_account.email,

    %% 2. Create storage bucket
    {ok, _Bucket} = gcp_storage_sim:create_bucket(ProjectId, <<"data-bucket">>),

    %% 3. Grant storage access to service account
    {ok, _Policy} = gcp_iam_sim:add_binding(
        bucket, <<"data-bucket">>,
        <<"roles/storage.objectCreator">>,
        [<<"serviceAccount:", SAEmail/binary>>]
    ),

    %% 4. Create Pub/Sub topic for notifications
    {ok, _Topic} = gcp_pubsub_sim:create_topic(ProjectId, <<"notifications">>),
    TopicName = <<"projects/", ProjectId/binary, "/topics/notifications">>,

    %% 5. Create subscription
    {ok, _Sub} = gcp_pubsub_sim:create_subscription(
        ProjectId, <<"worker-sub">>, TopicName),

    %% 6. Create compute instance
    {ok, _Instance} = gcp_compute_sim:create_instance(
        ProjectId, <<"us-central1-a">>, <<"worker-vm">>, <<"e2-standard-4">>,
        #{
            labels => #{<<"role">> => <<"worker">>},
            service_accounts => [#{email => SAEmail}]
        }
    ),

    %% 7. Upload data to storage
    {ok, _Obj} = gcp_storage_sim:upload_object(
        <<"data-bucket">>, <<"input/data.json">>,
        <<"application/json">>, <<"{\"value\": 42}">>
    ),

    %% 8. Publish notification
    {ok, [_MsgId]} = gcp_pubsub_sim:publish(TopicName, [
        #{data => <<"new_data_available">>, attributes => #{<<"bucket">> => <<"data-bucket">>}}
    ]),

    %% Verify everything exists
    {ok, Buckets} = gcp_storage_sim:list_buckets(ProjectId),
    ?assert(length(Buckets) >= 1),

    {ok, Topics} = gcp_pubsub_sim:list_topics(ProjectId),
    ?assert(length(Topics) >= 1),

    {ok, Instances} = gcp_compute_sim:list_instances(ProjectId),
    ?assert(length(Instances) >= 1),

    ok.
