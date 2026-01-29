-module(gcp_simulator_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup/Teardown
%%====================================================================

setup() ->
    %% Initialize state for each test
    gcp_simulator_server:init_state(),
    ok.

cleanup(_) ->
    %% Clean up ETS tables
    lists:foreach(fun(Table) ->
        catch ets:delete(Table)
    end, [gcp_compute_instances, gcp_storage_buckets, gcp_storage_objects,
          gcp_cloud_functions, gcp_cloud_sql, gcp_pubsub_topics,
          gcp_pubsub_subscriptions, gcp_iam_service_accounts]),
    ok.

%%====================================================================
%% Compute Engine Tests
%%====================================================================

compute_create_instance_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Create an instance
        Result = gcp_simulator_server:create_compute_instance(
            <<"test-vm">>, <<"e2-micro">>, <<"us-central1-a">>),

        %% Verify result contains expected fields
        ?assert(is_binary(Result)),
        ?assertMatch(true, binary:match(Result, <<"test-vm">>) =/= nomatch),
        ?assertMatch(true, binary:match(Result, <<"e2-micro">>) =/= nomatch),
        ?assertMatch(true, binary:match(Result, <<"us-central1-a">>) =/= nomatch),

        %% Verify instance is in ETS
        ?assertMatch([{<<"test-vm">>, _}], ets:lookup(gcp_compute_instances, <<"test-vm">>))
     end}.

compute_list_instances_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Empty list
        EmptyResult = gcp_simulator_server:list_compute_instances(),
        ?assertMatch(true, binary:match(EmptyResult, <<"No instances">>) =/= nomatch),

        %% Create instances
        gcp_simulator_server:create_compute_instance(
            <<"vm-1">>, <<"n1-standard-1">>, <<"us-east1-b">>),
        gcp_simulator_server:create_compute_instance(
            <<"vm-2">>, <<"e2-medium">>, <<"us-west1-a">>),

        %% List should contain both
        ListResult = gcp_simulator_server:list_compute_instances(),
        ?assertMatch(true, binary:match(ListResult, <<"vm-1">>) =/= nomatch),
        ?assertMatch(true, binary:match(ListResult, <<"vm-2">>) =/= nomatch)
     end}.

compute_get_instance_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Try to get non-existent instance
        NotFoundResult = gcp_simulator_server:get_compute_instance(<<"no-such-vm">>),
        ?assertMatch(true, binary:match(NotFoundResult, <<"not found">>) =/= nomatch),

        %% Create and get instance
        gcp_simulator_server:create_compute_instance(
            <<"my-vm">>, <<"e2-micro">>, <<"us-central1-a">>),
        GetResult = gcp_simulator_server:get_compute_instance(<<"my-vm">>),
        ?assertMatch(true, binary:match(GetResult, <<"my-vm">>) =/= nomatch),
        ?assertMatch(true, binary:match(GetResult, <<"RUNNING">>) =/= nomatch)
     end}.

compute_start_stop_instance_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Create instance
        gcp_simulator_server:create_compute_instance(
            <<"test-vm">>, <<"e2-micro">>, <<"us-central1-a">>),

        %% Stop instance
        StopResult = gcp_simulator_server:stop_compute_instance(<<"test-vm">>),
        ?assertMatch(true, binary:match(StopResult, <<"stopped successfully">>) =/= nomatch),

        %% Verify status changed
        GetResult = gcp_simulator_server:get_compute_instance(<<"test-vm">>),
        ?assertMatch(true, binary:match(GetResult, <<"TERMINATED">>) =/= nomatch),

        %% Start instance
        StartResult = gcp_simulator_server:start_compute_instance(<<"test-vm">>),
        ?assertMatch(true, binary:match(StartResult, <<"started successfully">>) =/= nomatch),

        %% Verify status changed back
        GetResult2 = gcp_simulator_server:get_compute_instance(<<"test-vm">>),
        ?assertMatch(true, binary:match(GetResult2, <<"RUNNING">>) =/= nomatch)
     end}.

compute_delete_instance_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Create instance
        gcp_simulator_server:create_compute_instance(
            <<"test-vm">>, <<"e2-micro">>, <<"us-central1-a">>),

        %% Delete instance
        DeleteResult = gcp_simulator_server:delete_compute_instance(<<"test-vm">>),
        ?assertMatch(true, binary:match(DeleteResult, <<"deleted successfully">>) =/= nomatch),

        %% Verify instance is gone
        ?assertEqual([], ets:lookup(gcp_compute_instances, <<"test-vm">>))
     end}.

%%====================================================================
%% Cloud Storage Tests
%%====================================================================

storage_create_bucket_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        Result = gcp_simulator_server:create_storage_bucket(
            <<"my-bucket">>, <<"US">>),

        ?assert(is_binary(Result)),
        ?assertMatch(true, binary:match(Result, <<"my-bucket">>) =/= nomatch),
        ?assertMatch(true, binary:match(Result, <<"US">>) =/= nomatch),

        %% Verify bucket in ETS
        ?assertMatch([{<<"my-bucket">>, _}], ets:lookup(gcp_storage_buckets, <<"my-bucket">>))
     end}.

storage_upload_download_object_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Create bucket first
        gcp_simulator_server:create_storage_bucket(<<"test-bucket">>, <<"US">>),

        %% Upload object
        Content = <<"Hello, GCP!">>,
        UploadResult = gcp_simulator_server:upload_storage_object(
            <<"test-bucket">>, <<"test.txt">>, Content),
        ?assertMatch(true, binary:match(UploadResult, <<"uploaded">>) =/= nomatch),

        %% Download object
        DownloadResult = gcp_simulator_server:download_storage_object(
            <<"test-bucket">>, <<"test.txt">>),
        ?assertMatch(true, binary:match(DownloadResult, <<"Hello, GCP!">>) =/= nomatch)
     end}.

storage_list_objects_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Create bucket
        gcp_simulator_server:create_storage_bucket(<<"test-bucket">>, <<"US">>),

        %% Upload multiple objects
        gcp_simulator_server:upload_storage_object(
            <<"test-bucket">>, <<"file1.txt">>, <<"content1">>),
        gcp_simulator_server:upload_storage_object(
            <<"test-bucket">>, <<"file2.txt">>, <<"content2">>),

        %% List objects
        ListResult = gcp_simulator_server:list_storage_objects(<<"test-bucket">>),
        ?assertMatch(true, binary:match(ListResult, <<"file1.txt">>) =/= nomatch),
        ?assertMatch(true, binary:match(ListResult, <<"file2.txt">>) =/= nomatch)
     end}.

storage_delete_object_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Create bucket and upload object
        gcp_simulator_server:create_storage_bucket(<<"test-bucket">>, <<"US">>),
        gcp_simulator_server:upload_storage_object(
            <<"test-bucket">>, <<"test.txt">>, <<"content">>),

        %% Delete object
        DeleteResult = gcp_simulator_server:delete_storage_object(
            <<"test-bucket">>, <<"test.txt">>),
        ?assertMatch(true, binary:match(DeleteResult, <<"deleted successfully">>) =/= nomatch),

        %% Verify object is gone
        ?assertEqual([], ets:lookup(gcp_storage_objects, {<<"test-bucket">>, <<"test.txt">>}))
     end}.

%%====================================================================
%% Cloud Functions Tests
%%====================================================================

functions_deploy_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        Result = gcp_simulator_server:deploy_cloud_function(
            <<"my-function">>, <<"python39">>, <<"main">>),

        ?assert(is_binary(Result)),
        ?assertMatch(true, binary:match(Result, <<"deployed successfully">>) =/= nomatch),
        ?assertMatch(true, binary:match(Result, <<"python39">>) =/= nomatch),

        %% Verify function in ETS
        ?assertMatch([{<<"my-function">>, _}], ets:lookup(gcp_cloud_functions, <<"my-function">>))
     end}.

functions_list_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Deploy functions
        gcp_simulator_server:deploy_cloud_function(
            <<"func1">>, <<"nodejs18">>, <<"handler">>),
        gcp_simulator_server:deploy_cloud_function(
            <<"func2">>, <<"python39">>, <<"main">>),

        %% List functions
        ListResult = gcp_simulator_server:list_cloud_functions(),
        ?assertMatch(true, binary:match(ListResult, <<"func1">>) =/= nomatch),
        ?assertMatch(true, binary:match(ListResult, <<"func2">>) =/= nomatch)
     end}.

functions_invoke_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Deploy function
        gcp_simulator_server:deploy_cloud_function(
            <<"test-func">>, <<"python39">>, <<"main">>),

        %% Invoke function
        InvokeResult = gcp_simulator_server:invoke_cloud_function(
            <<"test-func">>, <<"{\"key\": \"value\"}">>),
        ?assertMatch(true, binary:match(InvokeResult, <<"invoked successfully">>) =/= nomatch),
        ?assertMatch(true, binary:match(InvokeResult, <<"success">>) =/= nomatch)
     end}.

functions_delete_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Deploy function
        gcp_simulator_server:deploy_cloud_function(
            <<"test-func">>, <<"python39">>, <<"main">>),

        %% Delete function
        DeleteResult = gcp_simulator_server:delete_cloud_function(<<"test-func">>),
        ?assertMatch(true, binary:match(DeleteResult, <<"deleted successfully">>) =/= nomatch),

        %% Verify function is gone
        ?assertEqual([], ets:lookup(gcp_cloud_functions, <<"test-func">>))
     end}.

%%====================================================================
%% Cloud SQL Tests
%%====================================================================

sql_create_instance_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        Result = gcp_simulator_server:create_sql_instance(
            <<"my-db">>, <<"POSTGRES_14">>, <<"db-f1-micro">>),

        ?assert(is_binary(Result)),
        ?assertMatch(true, binary:match(Result, <<"my-db">>) =/= nomatch),
        ?assertMatch(true, binary:match(Result, <<"POSTGRES_14">>) =/= nomatch),

        %% Verify instance in ETS
        ?assertMatch([{<<"my-db">>, _}], ets:lookup(gcp_cloud_sql, <<"my-db">>))
     end}.

sql_list_instances_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Create instances
        gcp_simulator_server:create_sql_instance(
            <<"db1">>, <<"POSTGRES_14">>, <<"db-f1-micro">>),
        gcp_simulator_server:create_sql_instance(
            <<"db2">>, <<"MYSQL_8_0">>, <<"db-n1-standard-1">>),

        %% List instances
        ListResult = gcp_simulator_server:list_sql_instances(),
        ?assertMatch(true, binary:match(ListResult, <<"db1">>) =/= nomatch),
        ?assertMatch(true, binary:match(ListResult, <<"db2">>) =/= nomatch)
     end}.

sql_delete_instance_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Create instance
        gcp_simulator_server:create_sql_instance(
            <<"test-db">>, <<"POSTGRES_14">>, <<"db-f1-micro">>),

        %% Delete instance
        DeleteResult = gcp_simulator_server:delete_sql_instance(<<"test-db">>),
        ?assertMatch(true, binary:match(DeleteResult, <<"deleted successfully">>) =/= nomatch),

        %% Verify instance is gone
        ?assertEqual([], ets:lookup(gcp_cloud_sql, <<"test-db">>))
     end}.

%%====================================================================
%% Pub/Sub Tests
%%====================================================================

pubsub_create_topic_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        Result = gcp_simulator_server:create_pubsub_topic(<<"my-topic">>),

        ?assert(is_binary(Result)),
        ?assertMatch(true, binary:match(Result, <<"created">>) =/= nomatch),

        %% Verify topic in ETS
        ?assertMatch([{<<"my-topic">>, _}], ets:lookup(gcp_pubsub_topics, <<"my-topic">>))
     end}.

pubsub_publish_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Create topic
        gcp_simulator_server:create_pubsub_topic(<<"test-topic">>),

        %% Publish message
        PublishResult = gcp_simulator_server:publish_pubsub_message(
            <<"test-topic">>, <<"Hello, Pub/Sub!">>),
        ?assertMatch(true, binary:match(PublishResult, <<"published">>) =/= nomatch),
        ?assertMatch(true, binary:match(PublishResult, <<"Message ID:">>) =/= nomatch)
     end}.

pubsub_create_subscription_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Create topic
        gcp_simulator_server:create_pubsub_topic(<<"test-topic">>),

        %% Create subscription
        SubResult = gcp_simulator_server:create_pubsub_subscription(
            <<"my-sub">>, <<"test-topic">>),
        ?assertMatch(true, binary:match(SubResult, <<"created">>) =/= nomatch),

        %% Verify subscription in ETS
        ?assertMatch([{<<"my-sub">>, _}], ets:lookup(gcp_pubsub_subscriptions, <<"my-sub">>))
     end}.

%%====================================================================
%% IAM Tests
%%====================================================================

iam_create_service_account_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        Result = gcp_simulator_server:create_service_account(
            <<"my-sa">>, <<"My Service Account">>),

        ?assert(is_binary(Result)),
        ?assertMatch(true, binary:match(Result, <<"my-sa">>) =/= nomatch),
        ?assertMatch(true, binary:match(Result, <<"Email:">>) =/= nomatch),

        %% Verify account in ETS
        ?assertMatch([{<<"my-sa">>, _}], ets:lookup(gcp_iam_service_accounts, <<"my-sa">>))
     end}.

iam_list_service_accounts_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Create accounts
        gcp_simulator_server:create_service_account(
            <<"sa1">>, <<"Service Account 1">>),
        gcp_simulator_server:create_service_account(
            <<"sa2">>, <<"Service Account 2">>),

        %% List accounts
        ListResult = gcp_simulator_server:list_service_accounts(),
        ?assertMatch(true, binary:match(ListResult, <<"sa1">>) =/= nomatch),
        ?assertMatch(true, binary:match(ListResult, <<"sa2">>) =/= nomatch)
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

full_workflow_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Create infrastructure for a web application

        %% 1. Create compute instance
        gcp_simulator_server:create_compute_instance(
            <<"web-server">>, <<"n1-standard-1">>, <<"us-central1-a">>),

        %% 2. Create storage bucket for static assets
        gcp_simulator_server:create_storage_bucket(
            <<"static-assets">>, <<"US">>),
        gcp_simulator_server:upload_storage_object(
            <<"static-assets">>, <<"index.html">>, <<"<html>Hello</html>">>),

        %% 3. Create database
        gcp_simulator_server:create_sql_instance(
            <<"app-db">>, <<"POSTGRES_14">>, <<"db-n1-standard-1">>),

        %% 4. Deploy cloud function
        gcp_simulator_server:deploy_cloud_function(
            <<"api-handler">>, <<"nodejs18">>, <<"handler">>),

        %% 5. Create pub/sub topic for events
        gcp_simulator_server:create_pubsub_topic(<<"app-events">>),

        %% 6. Create service account
        gcp_simulator_server:create_service_account(
            <<"app-sa">>, <<"Application Service Account">>),

        %% Verify all resources exist
        ?assertMatch([_], ets:lookup(gcp_compute_instances, <<"web-server">>)),
        ?assertMatch([_], ets:lookup(gcp_storage_buckets, <<"static-assets">>)),
        ?assertMatch([_], ets:lookup(gcp_cloud_sql, <<"app-db">>)),
        ?assertMatch([_], ets:lookup(gcp_cloud_functions, <<"api-handler">>)),
        ?assertMatch([_], ets:lookup(gcp_pubsub_topics, <<"app-events">>)),
        ?assertMatch([_], ets:lookup(gcp_iam_service_accounts, <<"app-sa">>))
     end}.

%%====================================================================
%% Error Path Tests - Chicago School TDD
%% Real ETS operations, state-based verification, no mocks
%%====================================================================

%%--------------------------------------------------------------------
%% Duplicate Creation Tests (verify idempotency/already_exists behavior)
%%--------------------------------------------------------------------

compute_duplicate_instance_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Create instance twice (duplicate creation)
        gcp_simulator_server:create_compute_instance(
            <<"dup-vm">>, <<"e2-micro">>, <<"us-central1-a">>),
        Result2 = gcp_simulator_server:create_compute_instance(
            <<"dup-vm">>, <<"n1-standard-1">>, <<"us-east1-b">>),

        %% Verify: Second creation succeeds (ETS insert overwrites)
        %% Note: Current implementation allows overwrites, not returning error
        ?assert(is_binary(Result2)),

        %% Verify: Instance exists with LATEST data (observable state)
        [{_, Instance}] = ets:lookup(gcp_compute_instances, <<"dup-vm">>),
        ?assertEqual(<<"n1-standard-1">>, maps:get(machine_type, Instance)),
        ?assertEqual(<<"us-east1-b">>, maps:get(zone, Instance))
     end}.

storage_duplicate_bucket_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Create bucket twice with same name
        gcp_simulator_server:create_storage_bucket(
            <<"dup-bucket">>, <<"US">>),
        Result2 = gcp_simulator_server:create_storage_bucket(
            <<"dup-bucket">>, <<"EU">>),

        %% Verify: Second creation succeeds (overwrites)
        ?assert(is_binary(Result2)),

        %% Verify: Bucket exists with LATEST location (state verification)
        [{_, Bucket}] = ets:lookup(gcp_storage_buckets, <<"dup-bucket">>),
        ?assertEqual(<<"EU">>, maps:get(location, Bucket))
     end}.

functions_duplicate_deploy_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Deploy function twice with same name
        gcp_simulator_server:deploy_cloud_function(
            <<"dup-func">>, <<"python39">>, <<"main">>),
        Result2 = gcp_simulator_server:deploy_cloud_function(
            <<"dup-func">>, <<"nodejs18">>, <<"handler">>),

        %% Verify: Second deploy succeeds (overwrites)
        ?assert(is_binary(Result2)),

        %% Verify: Function exists with LATEST runtime (state verification)
        [{_, Function}] = ets:lookup(gcp_cloud_functions, <<"dup-func">>),
        ?assertEqual(<<"nodejs18">>, maps:get(runtime, Function)),
        ?assertEqual(<<"handler">>, maps:get(entry_point, Function))
     end}.

%%--------------------------------------------------------------------
%% Invalid Input Tests (empty names, special chars, edge cases)
%%--------------------------------------------------------------------

compute_empty_name_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Create instance with empty name
        Result = gcp_simulator_server:create_compute_instance(
            <<>>, <<"e2-micro">>, <<"us-central1-a">>),

        %% Verify: Operation completes (current impl doesn't validate)
        ?assert(is_binary(Result)),

        %% Verify: Instance created even with empty name (state verification)
        ?assertMatch([{<<>>, _}], ets:lookup(gcp_compute_instances, <<>>))
     end}.

compute_special_chars_name_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Create instance with special characters
        SpecialName = <<"vm@test#$%">>,
        Result = gcp_simulator_server:create_compute_instance(
            SpecialName, <<"e2-micro">>, <<"us-central1-a">>),

        %% Verify: Operation accepts special chars (current impl)
        ?assert(is_binary(Result)),

        %% Verify: Instance created (state verification)
        ?assertMatch([{_, _}], ets:lookup(gcp_compute_instances, SpecialName))
     end}.

storage_special_chars_bucket_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Create bucket with uppercase and special chars
        BucketName = <<"My-Bucket_123!@#">>,
        Result = gcp_simulator_server:create_storage_bucket(
            BucketName, <<"US">>),

        %% Verify: Operation accepts special chars
        ?assert(is_binary(Result)),

        %% Verify: Bucket created (state verification)
        ?assertMatch([{_, _}], ets:lookup(gcp_storage_buckets, BucketName))
     end}.

functions_empty_runtime_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Deploy function with empty runtime
        Result = gcp_simulator_server:deploy_cloud_function(
            <<"test-func">>, <<>>, <<"main">>),

        %% Verify: Operation completes (no validation)
        ?assert(is_binary(Result)),

        %% Verify: Function created with empty runtime (state verification)
        [{_, Function}] = ets:lookup(gcp_cloud_functions, <<"test-func">>),
        ?assertEqual(<<>>, maps:get(runtime, Function))
     end}.

%%--------------------------------------------------------------------
%% Non-existent Resource Get/Delete Tests
%%--------------------------------------------------------------------

compute_get_nonexistent_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Get non-existent instance
        Result = gcp_simulator_server:get_compute_instance(<<"no-such-vm">>),

        %% Verify: Returns error message (observable behavior)
        ?assertMatch(true, binary:match(Result, <<"not found">>) =/= nomatch)
     end}.

compute_delete_nonexistent_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Delete non-existent instance
        Result = gcp_simulator_server:delete_compute_instance(<<"ghost-vm">>),

        %% Verify: Returns success message (ETS delete returns true even if key doesn't exist)
        ?assertMatch(true, binary:match(Result, <<"deleted successfully">>) =/= nomatch),

        %% Verify: ETS unchanged (no such key)
        ?assertEqual([], ets:lookup(gcp_compute_instances, <<"ghost-vm">>))
     end}.

compute_start_nonexistent_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Start non-existent instance
        Result = gcp_simulator_server:start_compute_instance(<<"phantom-vm">>),

        %% Verify: Returns error message
        ?assertMatch(true, binary:match(Result, <<"not found">>) =/= nomatch)
     end}.

compute_stop_nonexistent_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Stop non-existent instance
        Result = gcp_simulator_server:stop_compute_instance(<<"imaginary-vm">>),

        %% Verify: Returns error message
        ?assertMatch(true, binary:match(Result, <<"not found">>) =/= nomatch)
     end}.

storage_upload_to_nonexistent_bucket_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Upload to non-existent bucket
        Result = gcp_simulator_server:upload_storage_object(
            <<"ghost-bucket">>, <<"test.txt">>, <<"content">>),

        %% Verify: Returns error message
        ?assertMatch(true, binary:match(Result, <<"not found">>) =/= nomatch),

        %% Verify: No object created (state verification)
        ?assertEqual([], ets:lookup(gcp_storage_objects, {<<"ghost-bucket">>, <<"test.txt">>}))
     end}.

storage_download_nonexistent_object_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Setup: Create bucket
        gcp_simulator_server:create_storage_bucket(<<"test-bucket">>, <<"US">>),

        %% Exercise: Download non-existent object
        Result = gcp_simulator_server:download_storage_object(
            <<"test-bucket">>, <<"ghost.txt">>),

        %% Verify: Returns error message
        ?assertMatch(true, binary:match(Result, <<"not found">>) =/= nomatch)
     end}.

storage_list_nonexistent_bucket_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: List objects in non-existent bucket
        Result = gcp_simulator_server:list_storage_objects(<<"ghost-bucket">>),

        %% Verify: Returns empty list message (not error, per implementation)
        ?assert(is_binary(Result))
     end}.

functions_invoke_nonexistent_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Invoke non-existent function
        Result = gcp_simulator_server:invoke_cloud_function(
            <<"ghost-func">>, <<"{\"data\": \"test\"}">>),

        %% Verify: Returns error message
        ?assertMatch(true, binary:match(Result, <<"not found">>) =/= nomatch)
     end}.

sql_delete_nonexistent_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Delete non-existent SQL instance
        Result = gcp_simulator_server:delete_sql_instance(<<"ghost-db">>),

        %% Verify: Returns success message (ETS delete returns true even if key doesn't exist)
        ?assertMatch(true, binary:match(Result, <<"deleted successfully">>) =/= nomatch),

        %% Verify: ETS unchanged
        ?assertEqual([], ets:lookup(gcp_cloud_sql, <<"ghost-db">>))
     end}.

pubsub_publish_nonexistent_topic_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Publish to non-existent topic
        Result = gcp_simulator_server:publish_pubsub_message(
            <<"ghost-topic">>, <<"Test message">>),

        %% Verify: Returns error message
        ?assertMatch(true, binary:match(Result, <<"not found">>) =/= nomatch)
     end}.

pubsub_subscription_nonexistent_topic_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Create subscription for non-existent topic
        Result = gcp_simulator_server:create_pubsub_subscription(
            <<"my-sub">>, <<"ghost-topic">>),

        %% Verify: Returns error message
        ?assertMatch(true, binary:match(Result, <<"not found">>) =/= nomatch),

        %% Verify: No subscription created (state verification)
        ?assertEqual([], ets:lookup(gcp_pubsub_subscriptions, <<"my-sub">>))
     end}.

%%--------------------------------------------------------------------
%% Concurrent Operations Tests (race conditions, real concurrency)
%%--------------------------------------------------------------------

compute_concurrent_create_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: 50 processes create instances concurrently (real concurrency)
        Pids = lists:map(fun(N) ->
            spawn(fun() ->
                Name = list_to_binary("concurrent-vm-" ++ integer_to_list(N)),
                gcp_simulator_server:create_compute_instance(
                    Name, <<"e2-micro">>, <<"us-central1-a">>),
                receive stop -> ok end
            end)
        end, lists:seq(1, 50)),

        timer:sleep(300), %% Let all creations complete

        %% Verify: All processes completed (no crashes)
        AlivePids = lists:filter(fun erlang:is_process_alive/1, Pids),
        ?assertEqual(50, length(AlivePids)),

        %% Verify: All instances created (state verification)
        InstanceCount = length(ets:tab2list(gcp_compute_instances)),
        ?assertEqual(50, InstanceCount),

        %% Cleanup
        lists:foreach(fun(Pid) -> Pid ! stop end, Pids)
     end}.

compute_concurrent_delete_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Setup: Create instances
        lists:foreach(fun(N) ->
            Name = list_to_binary("delete-vm-" ++ integer_to_list(N)),
            gcp_simulator_server:create_compute_instance(
                Name, <<"e2-micro">>, <<"us-central1-a">>)
        end, lists:seq(1, 20)),

        %% Exercise: 20 processes delete instances concurrently
        Pids = lists:map(fun(N) ->
            spawn(fun() ->
                Name = list_to_binary("delete-vm-" ++ integer_to_list(N)),
                gcp_simulator_server:delete_compute_instance(Name),
                receive stop -> ok end
            end)
        end, lists:seq(1, 20)),

        timer:sleep(300), %% Let all deletes complete

        %% Verify: All processes completed
        AlivePids = lists:filter(fun erlang:is_process_alive/1, Pids),
        ?assertEqual(20, length(AlivePids)),

        %% Verify: All instances deleted (state verification)
        RemainingCount = length(ets:tab2list(gcp_compute_instances)),
        ?assertEqual(0, RemainingCount),

        %% Cleanup
        lists:foreach(fun(Pid) -> Pid ! stop end, Pids)
     end}.

storage_concurrent_upload_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Setup: Create bucket
        gcp_simulator_server:create_storage_bucket(<<"test-bucket">>, <<"US">>),

        %% Exercise: 30 processes upload objects concurrently
        Pids = lists:map(fun(N) ->
            spawn(fun() ->
                ObjectName = list_to_binary("file" ++ integer_to_list(N) ++ ".txt"),
                Content = list_to_binary("content-" ++ integer_to_list(N)),
                gcp_simulator_server:upload_storage_object(
                    <<"test-bucket">>, ObjectName, Content),
                receive stop -> ok end
            end)
        end, lists:seq(1, 30)),

        timer:sleep(300), %% Let all uploads complete

        %% Verify: All processes completed
        AlivePids = lists:filter(fun erlang:is_process_alive/1, Pids),
        ?assertEqual(30, length(AlivePids)),

        %% Verify: All objects uploaded (state verification)
        Objects = ets:match_object(gcp_storage_objects, {{<<"test-bucket">>, '_'}, '_'}),
        ?assertEqual(30, length(Objects)),

        %% Cleanup
        lists:foreach(fun(Pid) -> Pid ! stop end, Pids)
     end}.

functions_concurrent_deploy_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: 25 processes deploy functions concurrently
        Pids = lists:map(fun(N) ->
            spawn(fun() ->
                Name = list_to_binary("concurrent-func-" ++ integer_to_list(N)),
                Runtime = case N rem 2 of
                    0 -> <<"python39">>;
                    1 -> <<"nodejs18">>
                end,
                gcp_simulator_server:deploy_cloud_function(
                    Name, Runtime, <<"handler">>),
                receive stop -> ok end
            end)
        end, lists:seq(1, 25)),

        timer:sleep(300), %% Let all deploys complete

        %% Verify: All processes completed
        AlivePids = lists:filter(fun erlang:is_process_alive/1, Pids),
        ?assertEqual(25, length(AlivePids)),

        %% Verify: All functions deployed (state verification)
        FunctionCount = length(ets:tab2list(gcp_cloud_functions)),
        ?assertEqual(25, FunctionCount),

        %% Cleanup
        lists:foreach(fun(Pid) -> Pid ! stop end, Pids)
     end}.

%%--------------------------------------------------------------------
%% Edge Cases and Boundary Conditions
%%--------------------------------------------------------------------

compute_very_long_name_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Create instance with very long name (1000 chars)
        LongName = list_to_binary(lists:duplicate(1000, $a)),
        Result = gcp_simulator_server:create_compute_instance(
            LongName, <<"e2-micro">>, <<"us-central1-a">>),

        %% Verify: Operation accepts long names
        ?assert(is_binary(Result)),

        %% Verify: Instance created (state verification)
        ?assertMatch([{_, _}], ets:lookup(gcp_compute_instances, LongName))
     end}.

storage_binary_content_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Setup: Create bucket
        gcp_simulator_server:create_storage_bucket(<<"test-bucket">>, <<"US">>),

        %% Exercise: Upload binary content (UTF-8, emoji, null bytes)
        BinaryContent = <<0, 1, 2, 255, 254, 253, "Hello🚀", 0>>,
        Result = gcp_simulator_server:upload_storage_object(
            <<"test-bucket">>, <<"binary.bin">>, BinaryContent),

        %% Verify: Upload succeeds
        ?assertMatch(true, binary:match(Result, <<"uploaded">>) =/= nomatch),

        %% Verify: Content preserved (state verification)
        [{_, Object}] = ets:lookup(gcp_storage_objects, {<<"test-bucket">>, <<"binary.bin">>}),
        ?assertEqual(BinaryContent, maps:get(content, Object))
     end}.

functions_large_data_invoke_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Setup: Deploy function
        gcp_simulator_server:deploy_cloud_function(
            <<"test-func">>, <<"python39">>, <<"main">>),

        %% Exercise: Invoke with large data payload (10KB)
        LargeData = list_to_binary(lists:duplicate(10000, $x)),
        Result = gcp_simulator_server:invoke_cloud_function(
            <<"test-func">>, LargeData),

        %% Verify: Invoke succeeds
        ?assertMatch(true, binary:match(Result, <<"invoked successfully">>) =/= nomatch)
     end}.

iam_duplicate_service_account_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Exercise: Create service account twice with same name
        gcp_simulator_server:create_service_account(
            <<"dup-sa">>, <<"First Account">>),
        Result2 = gcp_simulator_server:create_service_account(
            <<"dup-sa">>, <<"Second Account">>),

        %% Verify: Second creation succeeds (overwrites)
        ?assert(is_binary(Result2)),

        %% Verify: Account exists with LATEST display name (state verification)
        [{_, Account}] = ets:lookup(gcp_iam_service_accounts, <<"dup-sa">>),
        ?assertEqual(<<"Second Account">>, maps:get(display_name, Account))
     end}.

pubsub_duplicate_subscription_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Setup: Create topic
        gcp_simulator_server:create_pubsub_topic(<<"test-topic">>),

        %% Exercise: Create subscription twice with same name
        gcp_simulator_server:create_pubsub_subscription(
            <<"dup-sub">>, <<"test-topic">>),
        Result2 = gcp_simulator_server:create_pubsub_subscription(
            <<"dup-sub">>, <<"test-topic">>),

        %% Verify: Second creation succeeds (overwrites)
        ?assert(is_binary(Result2))
     end}.

%%--------------------------------------------------------------------
%% State Transition Tests (verify status changes)
%%--------------------------------------------------------------------

compute_stop_already_stopped_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Setup: Create and stop instance
        gcp_simulator_server:create_compute_instance(
            <<"test-vm">>, <<"e2-micro">>, <<"us-central1-a">>),
        gcp_simulator_server:stop_compute_instance(<<"test-vm">>),

        %% Exercise: Stop already stopped instance
        Result = gcp_simulator_server:stop_compute_instance(<<"test-vm">>),

        %% Verify: Operation succeeds (idempotent stop)
        ?assertMatch(true, binary:match(Result, <<"stopped successfully">>) =/= nomatch),

        %% Verify: Status remains TERMINATED (state verification)
        [{_, Instance}] = ets:lookup(gcp_compute_instances, <<"test-vm">>),
        ?assertEqual(<<"TERMINATED">>, maps:get(status, Instance))
     end}.

compute_start_already_running_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Setup: Create instance (already RUNNING)
        gcp_simulator_server:create_compute_instance(
            <<"test-vm">>, <<"e2-micro">>, <<"us-central1-a">>),

        %% Exercise: Start already running instance
        Result = gcp_simulator_server:start_compute_instance(<<"test-vm">>),

        %% Verify: Operation succeeds (idempotent start)
        ?assertMatch(true, binary:match(Result, <<"started successfully">>) =/= nomatch),

        %% Verify: Status remains RUNNING (state verification)
        [{_, Instance}] = ets:lookup(gcp_compute_instances, <<"test-vm">>),
        ?assertEqual(<<"RUNNING">>, maps:get(status, Instance))
     end}.

compute_stop_start_cycle_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun() ->
        %% Setup: Create instance
        gcp_simulator_server:create_compute_instance(
            <<"test-vm">>, <<"e2-micro">>, <<"us-central1-a">>),

        %% Exercise: Stop -> Start -> Stop -> Start cycle (multiple transitions)
        ?assertMatch(true, binary:match(
            gcp_simulator_server:stop_compute_instance(<<"test-vm">>),
            <<"stopped successfully">>) =/= nomatch),
        ?assertMatch(true, binary:match(
            gcp_simulator_server:start_compute_instance(<<"test-vm">>),
            <<"started successfully">>) =/= nomatch),
        ?assertMatch(true, binary:match(
            gcp_simulator_server:stop_compute_instance(<<"test-vm">>),
            <<"stopped successfully">>) =/= nomatch),
        ?assertMatch(true, binary:match(
            gcp_simulator_server:start_compute_instance(<<"test-vm">>),
            <<"started successfully">>) =/= nomatch),

        %% Verify: Final state is RUNNING (state verification)
        [{_, Instance}] = ets:lookup(gcp_compute_instances, <<"test-vm">>),
        ?assertEqual(<<"RUNNING">>, maps:get(status, Instance))
     end}.
