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
