-module(gcp_demo).
-export([run/0, run_web_app_deployment/0, run_data_pipeline/0]).

%% Demo script showing how to use the GCP simulator
%% This demonstrates common GCP workflows

run() ->
    io:format("~n=== GCP Simulator Demo ===~n~n"),

    %% Initialize the simulator
    gcp_simulator_server:init_state(),

    %% Run different scenarios
    run_web_app_deployment(),
    io:format("~n"),
    run_data_pipeline(),
    io:format("~n"),
    run_microservices_deployment(),

    io:format("~n=== Demo Complete ===~n").

%% Scenario 1: Deploy a web application
run_web_app_deployment() ->
    io:format("--- Scenario 1: Web Application Deployment ---~n~n"),

    %% Step 1: Create a compute instance for the web server
    io:format("1. Creating web server instance...~n"),
    Result1 = gcp_simulator_server:create_compute_instance(
        <<"web-server">>, <<"n1-standard-2">>, <<"us-central1-a">>),
    io:format("~s~n", [Result1]),

    %% Step 2: Create a storage bucket for static assets
    io:format("2. Creating storage bucket for static assets...~n"),
    Result2 = gcp_simulator_server:create_storage_bucket(
        <<"my-app-static">>, <<"US">>),
    io:format("~s~n", [Result2]),

    %% Step 3: Upload static files
    io:format("3. Uploading static files...~n"),
    Result3a = gcp_simulator_server:upload_storage_object(
        <<"my-app-static">>, <<"index.html">>,
        <<"<html><head><title>My App</title></head><body>Welcome!</body></html>">>),
    io:format("~s", [Result3a]),
    Result3b = gcp_simulator_server:upload_storage_object(
        <<"my-app-static">>, <<"styles.css">>,
        <<"body { font-family: Arial; }">>),
    io:format("~s~n", [Result3b]),

    %% Step 4: Create a Cloud SQL database
    io:format("4. Creating Cloud SQL database...~n"),
    Result4 = gcp_simulator_server:create_sql_instance(
        <<"app-database">>, <<"POSTGRES_14">>, <<"db-n1-standard-1">>),
    io:format("~s~n", [Result4]),

    %% Step 5: Deploy a Cloud Function for API endpoints
    io:format("5. Deploying API Cloud Function...~n"),
    Result5 = gcp_simulator_server:deploy_cloud_function(
        <<"api-handler">>, <<"nodejs18">>, <<"handleRequest">>),
    io:format("~s~n", [Result5]),

    %% Step 6: Create a service account
    io:format("6. Creating service account...~n"),
    Result6 = gcp_simulator_server:create_service_account(
        <<"web-app-sa">>, <<"Web Application Service Account">>),
    io:format("~s~n", [Result6]),

    %% Step 7: List all resources
    io:format("7. Listing all compute instances...~n"),
    Result7 = gcp_simulator_server:list_compute_instances(),
    io:format("~s~n", [Result7]),

    io:format("Web application deployment complete!~n").

%% Scenario 2: Data processing pipeline
run_data_pipeline() ->
    io:format("--- Scenario 2: Data Processing Pipeline ---~n~n"),

    %% Step 1: Create a storage bucket for raw data
    io:format("1. Creating data ingestion bucket...~n"),
    Result1 = gcp_simulator_server:create_storage_bucket(
        <<"data-ingestion">>, <<"US">>),
    io:format("~s~n", [Result1]),

    %% Step 2: Create Pub/Sub topic for data events
    io:format("2. Creating Pub/Sub topic for data events...~n"),
    Result2 = gcp_simulator_server:create_pubsub_topic(<<"data-events">>),
    io:format("~s~n", [Result2]),

    %% Step 3: Create subscription for processing
    io:format("3. Creating subscription for data processor...~n"),
    Result3 = gcp_simulator_server:create_pubsub_subscription(
        <<"data-processor-sub">>, <<"data-events">>),
    io:format("~s~n", [Result3]),

    %% Step 4: Deploy Cloud Function for data processing
    io:format("4. Deploying data processing function...~n"),
    Result4 = gcp_simulator_server:deploy_cloud_function(
        <<"process-data">>, <<"python39">>, <<"process_file">>),
    io:format("~s~n", [Result4]),

    %% Step 5: Upload sample data
    io:format("5. Uploading sample data file...~n"),
    SampleData = <<"user_id,action,timestamp
1,login,2024-01-15T10:30:00Z
2,purchase,2024-01-15T10:31:00Z
3,logout,2024-01-15T10:32:00Z">>,
    Result5 = gcp_simulator_server:upload_storage_object(
        <<"data-ingestion">>, <<"users_2024_01_15.csv">>, SampleData),
    io:format("~s~n", [Result5]),

    %% Step 6: Publish event about new data
    io:format("6. Publishing data arrival event...~n"),
    Result6 = gcp_simulator_server:publish_pubsub_message(
        <<"data-events">>,
        <<"{\"bucket\": \"data-ingestion\", \"file\": \"users_2024_01_15.csv\"}">>),
    io:format("~s~n", [Result6]),

    %% Step 7: Create BigQuery dataset (simulated via storage bucket)
    io:format("7. Creating processed data bucket...~n"),
    Result7 = gcp_simulator_server:create_storage_bucket(
        <<"data-processed">>, <<"US">>),
    io:format("~s~n", [Result7]),

    %% Step 8: Invoke processing function
    io:format("8. Invoking data processing function...~n"),
    Result8 = gcp_simulator_server:invoke_cloud_function(
        <<"process-data">>,
        <<"{\"input_bucket\": \"data-ingestion\", \"output_bucket\": \"data-processed\"}">>),
    io:format("~s~n", [Result8]),

    io:format("Data processing pipeline setup complete!~n").

%% Scenario 3: Microservices deployment
run_microservices_deployment() ->
    io:format("--- Scenario 3: Microservices Architecture ---~n~n"),

    %% Create multiple services
    Services = [
        {<<"auth-service">>, <<"nodejs18">>, <<"authenticate">>},
        {<<"user-service">>, <<"python39">>, <<"handle_user">>},
        {<<"order-service">>, <<"nodejs18">>, <<"handle_order">>},
        {<<"notification-service">>, <<"python39">>, <<"send_notification">>}
    ],

    io:format("1. Deploying microservices...~n"),
    lists:foreach(fun({Name, Runtime, EntryPoint}) ->
        Result = gcp_simulator_server:deploy_cloud_function(Name, Runtime, EntryPoint),
        io:format("   ~s", [Result])
    end, Services),

    %% Create Pub/Sub topics for inter-service communication
    io:format("~n2. Creating Pub/Sub topics for event-driven architecture...~n"),
    Topics = [<<"user-events">>, <<"order-events">>, <<"notification-events">>],
    lists:foreach(fun(Topic) ->
        Result = gcp_simulator_server:create_pubsub_topic(Topic),
        io:format("   ~s", [Result])
    end, Topics),

    %% Create databases for each service
    io:format("~n3. Creating dedicated databases...~n"),
    Databases = [
        {<<"auth-db">>, <<"POSTGRES_14">>, <<"db-f1-micro">>},
        {<<"user-db">>, <<"POSTGRES_14">>, <<"db-f1-micro">>},
        {<<"order-db">>, <<"MYSQL_8_0">>, <<"db-n1-standard-1">>}
    ],
    lists:foreach(fun({Name, Version, Tier}) ->
        Result = gcp_simulator_server:create_sql_instance(Name, Version, Tier),
        io:format("   ~s", [Result])
    end, Databases),

    %% Create API Gateway instance
    io:format("~n4. Creating API gateway instance...~n"),
    Result4 = gcp_simulator_server:create_compute_instance(
        <<"api-gateway">>, <<"e2-medium">>, <<"us-central1-a">>),
    io:format("~s~n", [Result4]),

    %% Create service accounts for each service
    io:format("5. Creating service accounts...~n"),
    ServiceAccounts = [
        {<<"auth-sa">>, <<"Auth Service Account">>},
        {<<"user-sa">>, <<"User Service Account">>},
        {<<"order-sa">>, <<"Order Service Account">>},
        {<<"notification-sa">>, <<"Notification Service Account">>}
    ],
    lists:foreach(fun({Name, DisplayName}) ->
        Result = gcp_simulator_server:create_service_account(Name, DisplayName),
        io:format("   ~s", [Result])
    end, ServiceAccounts),

    %% Show final status
    io:format("~n6. Final infrastructure status:~n"),
    Status = gcp_simulator_server:get_simulator_status(),
    io:format("~s~n", [Status]),

    io:format("Microservices architecture deployment complete!~n").
