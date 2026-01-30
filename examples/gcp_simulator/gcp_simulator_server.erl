-module(gcp_simulator_server).
-behaviour(gen_server).

%% API exports
-export([start_link/0, stop/0, setup_gcp_simulator/0, start/0, main/1,
         %% Compute Engine
         create_compute_instance/3, list_compute_instances/0, get_compute_instance/1,
         start_compute_instance/1, stop_compute_instance/1, delete_compute_instance/1,
         format_instance/1,
         %% Cloud Storage
         create_storage_bucket/2, list_storage_buckets/0, upload_storage_object/3,
         list_storage_objects/1, download_storage_object/2, delete_storage_object/2,
         format_bucket/1, format_object/1,
         %% Cloud Functions
         deploy_cloud_function/3, list_cloud_functions/0, invoke_cloud_function/2,
         delete_cloud_function/1, format_function/1,
         %% Cloud SQL
         create_sql_instance/3, list_sql_instances/0, delete_sql_instance/1,
         format_sql_instance/1,
         %% Pub/Sub
         create_pubsub_topic/1, list_pubsub_topics/0, publish_pubsub_message/2,
         create_pubsub_subscription/2,
         %% IAM
         create_service_account/2, list_service_accounts/0, format_service_account/1,
         %% Helpers
         get_simulator_status/0, get_help_text/0,
         generate_ip/0, generate_message_id/0, get_timestamp/0,
         wait_for_shutdown/0,
         %% Validation
         validate_resource_name/1, validate_machine_type/1, validate_zone/1,
         validate_location/1, validate_runtime/1, validate_entry_point/1,
         validate_database_version/1, validate_tier/1,
         validate_email_safe_name/1, validate_no_control_chars/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% GCP Simulator MCP Server
%% Simulates common GCP services for testing and development
%% Supports: Compute Engine, Cloud Storage, Cloud Functions, Cloud SQL, Pub/Sub, IAM

%%====================================================================
%% TYPE DEFINITIONS
%%====================================================================

%% GCP resource types
-type gcp_compute_instance() :: #{
    name := binary(),
    machine_type := binary(),
    zone := binary(),
    status := binary(), %% <<"RUNNING">> | <<"TERMINATED">>
    internal_ip := binary(),
    external_ip := binary(),
    created := binary()
}.

-type gcp_storage_bucket() :: #{
    name := binary(),
    location := binary(),
    created := binary(),
    storage_class := binary()
}.

-type gcp_storage_object() :: #{
    bucket := binary(),
    name := binary(),
    content := binary(),
    size := non_neg_integer(),
    uploaded := binary()
}.

-type gcp_cloud_function() :: #{
    name := binary(),
    runtime := binary(),
    entry_point := binary(),
    status := binary(), %% <<"ACTIVE">>
    deployed := binary(),
    url := binary()
}.

-type gcp_sql_instance() :: #{
    name := binary(),
    database_version := binary(),
    tier := binary(),
    state := binary(), %% <<"RUNNABLE">>
    connection_name := binary(),
    ip_address := binary(),
    created := binary()
}.

-type gcp_pubsub_topic() :: #{
    name := binary(),
    created := binary()
}.

-type gcp_pubsub_subscription() :: #{
    name := binary(),
    topic := binary(),
    created := binary()
}.

-type gcp_service_account() :: #{
    name := binary(),
    display_name := binary(),
    email := binary(),
    created := binary()
}.

%% Unified error type
-type gcp_error() :: {error, not_found | invalid_state | invalid_input}.

%% Success response type
-type gcp_response() :: binary().

%% Generic result type
-type gcp_result() :: {ok, gcp_response()} | gcp_error().

%% State record
-record(state, {
    compute_instances :: ets:tid(),
    storage_buckets :: ets:tid(),
    storage_objects :: ets:tid(),
    cloud_functions :: ets:tid(),
    cloud_sql :: ets:tid(),
    pubsub_topics :: ets:tid(),
    pubsub_subscriptions :: ets:tid(),
    service_accounts :: ets:tid()
}).

%%====================================================================
%% FUNCTION SPECIFICATIONS
%%====================================================================

%% Module entry points (backward compatibility)
-spec start() -> ok | no_return().
-spec main([string()]) -> no_return().

%% gen_server API
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec stop() -> ok.
-spec setup_gcp_simulator() -> ok.

%% Compute Engine functions
-spec create_compute_instance(binary(), binary(), binary()) -> gcp_response().
-spec list_compute_instances() -> gcp_response().
-spec get_compute_instance(binary()) -> {ok, gcp_response()} | gcp_error().
-spec start_compute_instance(binary()) -> {ok, gcp_response()} | gcp_error().
-spec stop_compute_instance(binary()) -> {ok, gcp_response()} | gcp_error().
-spec delete_compute_instance(binary()) -> {ok, gcp_response()} | gcp_error().
-spec format_instance(gcp_compute_instance()) -> binary().

%% Cloud Storage functions
-spec create_storage_bucket(binary(), binary()) -> gcp_response().
-spec list_storage_buckets() -> gcp_response().
-spec upload_storage_object(binary(), binary(), binary()) -> {ok, gcp_response()} | gcp_error().
-spec list_storage_objects(binary()) -> gcp_response().
-spec download_storage_object(binary(), binary()) -> {ok, gcp_response()} | gcp_error().
-spec delete_storage_object(binary(), binary()) -> {ok, gcp_response()} | gcp_error().
-spec format_bucket(gcp_storage_bucket()) -> binary().
-spec format_object(gcp_storage_object()) -> binary().

%% Cloud Functions
-spec deploy_cloud_function(binary(), binary(), binary()) -> gcp_response().
-spec list_cloud_functions() -> gcp_response().
-spec invoke_cloud_function(binary(), binary()) -> {ok, gcp_response()} | gcp_error().
-spec delete_cloud_function(binary()) -> {ok, gcp_response()} | gcp_error().
-spec format_function(gcp_cloud_function()) -> binary().

%% Cloud SQL functions
-spec create_sql_instance(binary(), binary(), binary()) -> gcp_response().
-spec list_sql_instances() -> gcp_response().
-spec delete_sql_instance(binary()) -> {ok, gcp_response()} | gcp_error().
-spec format_sql_instance(gcp_sql_instance()) -> binary().

%% Pub/Sub functions
-spec create_pubsub_topic(binary()) -> gcp_response().
-spec list_pubsub_topics() -> gcp_response().
-spec publish_pubsub_message(binary(), binary()) -> {ok, gcp_response()} | gcp_error().
-spec create_pubsub_subscription(binary(), binary()) -> {ok, gcp_response()} | gcp_error().

%% IAM functions
-spec create_service_account(binary(), binary()) -> gcp_response().
-spec list_service_accounts() -> gcp_response().
-spec format_service_account(gcp_service_account()) -> binary().

%% Helper functions
-spec get_simulator_status() -> gcp_response().
-spec get_help_text() -> gcp_response().
-spec generate_ip() -> binary().
-spec generate_message_id() -> binary().
-spec get_timestamp() -> binary().
-spec wait_for_shutdown() -> no_return().

%% Validation functions
-spec validate_resource_name(binary()) -> ok | {error, invalid_input}.
-spec validate_machine_type(binary()) -> ok | {error, invalid_input}.
-spec validate_zone(binary()) -> ok | {error, invalid_input}.
-spec validate_location(binary()) -> ok | {error, invalid_input}.
-spec validate_runtime(binary()) -> ok | {error, invalid_input}.
-spec validate_entry_point(binary()) -> ok | {error, invalid_input}.
-spec validate_database_version(binary()) -> ok | {error, invalid_input}.
-spec validate_tier(binary()) -> ok | {error, invalid_input}.
-spec validate_email_safe_name(binary()) -> ok | {error, invalid_input}.
-spec validate_no_control_chars(binary()) -> ok | {error, invalid_input}.

%%====================================================================
%% API FUNCTIONS
%%====================================================================

start() ->
    main([]).

main(_Args) ->
    %% Start the erlmcp applications (v2.0 umbrella structure)
    case application:ensure_all_started(erlmcp_core) of
        {ok, _StartedCore} ->
            logger:info("Successfully started erlmcp_core application~n");
        {error, Reason} ->
            logger:error("Failed to start erlmcp_core application: ~p~n", [Reason]),
            halt(1)
    end,
    case application:ensure_all_started(erlmcp_transports) of
        {ok, _StartedTransports} ->
            logger:info("Successfully started erlmcp_transports application~n");
        {error, TransportReason} ->
            logger:error("Failed to start erlmcp_transports application: ~p~n", [TransportReason]),
            halt(1)
    end,

    %% Configure logging
    logger:remove_handler(default),
    logger:add_handler(stderr_handler, logger_std_h, #{
        level => info,
        config => #{type => standard_error}
    }),
    logger:set_primary_config(level, info),

    logger:info("Starting GCP Simulator MCP server...~n"),

    %% Start the gen_server
    case start_link() of
        {ok, _Pid} ->
            logger:info("Successfully started gcp_simulator_server~n"),
            ok;
        {error, StartErrReason} ->
            logger:error("Failed to start gcp_simulator_server: ~p", [StartErrReason]),
            halt(1)
    end,

    %% Start the stdio MCP server
    case erlmcp_stdio:start() of
        ok ->
            logger:info("Successfully started stdio server~n"),
            setup_gcp_simulator(),
            logger:info("GCP Simulator setup complete, waiting for shutdown...~n"),
            wait_for_shutdown();
        {error, StdioReason} ->
            logger:error("Failed to start stdio server: ~p", [StdioReason]),
            halt(1)
    end.

%% @doc Start the gen_server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the gen_server
stop() ->
    gen_server:stop(?MODULE).

%% @doc Setup GCP simulator tools and resources
setup_gcp_simulator() ->
    %% ===============================================
    %% COMPUTE ENGINE TOOLS
    %% ===============================================

    ok = erlmcp_stdio:add_tool(<<"gcp_compute_create_instance">>,
        <<"Create a new Compute Engine VM instance">>,
        fun(#{<<"name">> := Name, <<"machine_type">> := MachineType, <<"zone">> := Zone}) ->
            create_compute_instance(Name, MachineType, Zone)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Instance name">>},
                <<"machine_type">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Machine type (e.g., e2-micro, n1-standard-1)">>},
                <<"zone">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Zone (e.g., us-central1-a)">>}
            },
            <<"required">> => [<<"name">>, <<"machine_type">>, <<"zone">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_compute_list_instances">>,
        <<"List all Compute Engine instances">>,
        fun(_Args) -> list_compute_instances() end,
        #{<<"type">> => <<"object">>, <<"properties">> => #{}}),

    ok = erlmcp_stdio:add_tool(<<"gcp_compute_get_instance">>,
        <<"Get details of a specific instance">>,
        fun(#{<<"name">> := Name}) -> get_compute_instance(Name) end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Instance name">>}
            },
            <<"required">> => [<<"name">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_compute_start_instance">>,
        <<"Start a stopped instance">>,
        fun(#{<<"name">> := Name}) -> start_compute_instance(Name) end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Instance name">>}
            },
            <<"required">> => [<<"name">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_compute_stop_instance">>,
        <<"Stop a running instance">>,
        fun(#{<<"name">> := Name}) -> stop_compute_instance(Name) end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Instance name">>}
            },
            <<"required">> => [<<"name">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_compute_delete_instance">>,
        <<"Delete an instance">>,
        fun(#{<<"name">> := Name}) -> delete_compute_instance(Name) end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Instance name">>}
            },
            <<"required">> => [<<"name">>]
        }),

    %% ===============================================
    %% CLOUD STORAGE TOOLS
    %% ===============================================

    ok = erlmcp_stdio:add_tool(<<"gcp_storage_create_bucket">>,
        <<"Create a new Cloud Storage bucket">>,
        fun(#{<<"name">> := Name, <<"location">> := Location}) ->
            create_storage_bucket(Name, Location)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Bucket name (globally unique)">>},
                <<"location">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Bucket location (e.g., US, EU, ASIA)">>}
            },
            <<"required">> => [<<"name">>, <<"location">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_storage_list_buckets">>,
        <<"List all Cloud Storage buckets">>,
        fun(_Args) -> list_storage_buckets() end,
        #{<<"type">> => <<"object">>, <<"properties">> => #{}}),

    ok = erlmcp_stdio:add_tool(<<"gcp_storage_upload_object">>,
        <<"Upload an object to a bucket">>,
        fun(#{<<"bucket">> := Bucket, <<"object_name">> := ObjectName, <<"content">> := Content}) ->
            upload_storage_object(Bucket, ObjectName, Content)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"bucket">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Bucket name">>},
                <<"object_name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Object name/path">>},
                <<"content">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Object content">>}
            },
            <<"required">> => [<<"bucket">>, <<"object_name">>, <<"content">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_storage_list_objects">>,
        <<"List objects in a bucket">>,
        fun(#{<<"bucket">> := Bucket}) -> list_storage_objects(Bucket) end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"bucket">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Bucket name">>}
            },
            <<"required">> => [<<"bucket">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_storage_download_object">>,
        <<"Download an object from a bucket">>,
        fun(#{<<"bucket">> := Bucket, <<"object_name">> := ObjectName}) ->
            download_storage_object(Bucket, ObjectName)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"bucket">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Bucket name">>},
                <<"object_name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Object name/path">>}
            },
            <<"required">> => [<<"bucket">>, <<"object_name">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_storage_delete_object">>,
        <<"Delete an object from a bucket">>,
        fun(#{<<"bucket">> := Bucket, <<"object_name">> := ObjectName}) ->
            delete_storage_object(Bucket, ObjectName)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"bucket">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Bucket name">>},
                <<"object_name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Object name/path">>}
            },
            <<"required">> => [<<"bucket">>, <<"object_name">>]
        }),

    %% ===============================================
    %% CLOUD FUNCTIONS TOOLS
    %% ===============================================

    ok = erlmcp_stdio:add_tool(<<"gcp_functions_deploy">>,
        <<"Deploy a Cloud Function">>,
        fun(#{<<"name">> := Name, <<"runtime">> := Runtime, <<"entry_point">> := EntryPoint}) ->
            deploy_cloud_function(Name, Runtime, EntryPoint)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Function name">>},
                <<"runtime">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Runtime (e.g., python39, nodejs18)">>},
                <<"entry_point">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Entry point function name">>}
            },
            <<"required">> => [<<"name">>, <<"runtime">>, <<"entry_point">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_functions_list">>,
        <<"List all Cloud Functions">>,
        fun(_Args) -> list_cloud_functions() end,
        #{<<"type">> => <<"object">>, <<"properties">> => #{}}),

    ok = erlmcp_stdio:add_tool(<<"gcp_functions_invoke">>,
        <<"Invoke a Cloud Function">>,
        fun(#{<<"name">> := Name, <<"data">> := Data}) ->
            invoke_cloud_function(Name, Data)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Function name">>},
                <<"data">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Input data (JSON string)">>}
            },
            <<"required">> => [<<"name">>, <<"data">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_functions_delete">>,
        <<"Delete a Cloud Function">>,
        fun(#{<<"name">> := Name}) -> delete_cloud_function(Name) end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Function name">>}
            },
            <<"required">> => [<<"name">>]
        }),

    %% ===============================================
    %% CLOUD SQL TOOLS
    %% ===============================================

    ok = erlmcp_stdio:add_tool(<<"gcp_sql_create_instance">>,
        <<"Create a Cloud SQL instance">>,
        fun(#{<<"name">> := Name, <<"database_version">> := DbVersion, <<"tier">> := Tier}) ->
            create_sql_instance(Name, DbVersion, Tier)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Instance name">>},
                <<"database_version">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Database version (e.g., POSTGRES_14, MYSQL_8_0)">>},
                <<"tier">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Machine tier (e.g., db-f1-micro, db-n1-standard-1)">>}
            },
            <<"required">> => [<<"name">>, <<"database_version">>, <<"tier">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_sql_list_instances">>,
        <<"List all Cloud SQL instances">>,
        fun(_Args) -> list_sql_instances() end,
        #{<<"type">> => <<"object">>, <<"properties">> => #{}}),

    ok = erlmcp_stdio:add_tool(<<"gcp_sql_delete_instance">>,
        <<"Delete a Cloud SQL instance">>,
        fun(#{<<"name">> := Name}) -> delete_sql_instance(Name) end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Instance name">>}
            },
            <<"required">> => [<<"name">>]
        }),

    %% ===============================================
    %% CLOUD PUB/SUB TOOLS
    %% ===============================================

    ok = erlmcp_stdio:add_tool(<<"gcp_pubsub_create_topic">>,
        <<"Create a Pub/Sub topic">>,
        fun(#{<<"name">> := Name}) -> create_pubsub_topic(Name) end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Topic name">>}
            },
            <<"required">> => [<<"name">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_pubsub_list_topics">>,
        <<"List all Pub/Sub topics">>,
        fun(_Args) -> list_pubsub_topics() end,
        #{<<"type">> => <<"object">>, <<"properties">> => #{}}),

    ok = erlmcp_stdio:add_tool(<<"gcp_pubsub_publish">>,
        <<"Publish a message to a topic">>,
        fun(#{<<"topic">> := Topic, <<"message">> := Message}) ->
            publish_pubsub_message(Topic, Message)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"topic">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Topic name">>},
                <<"message">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Message content">>}
            },
            <<"required">> => [<<"topic">>, <<"message">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_pubsub_create_subscription">>,
        <<"Create a subscription to a topic">>,
        fun(#{<<"name">> := Name, <<"topic">> := Topic}) ->
            create_pubsub_subscription(Name, Topic)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Subscription name">>},
                <<"topic">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Topic name">>}
            },
            <<"required">> => [<<"name">>, <<"topic">>]
        }),

    %% ===============================================
    %% IAM TOOLS
    %% ===============================================

    ok = erlmcp_stdio:add_tool(<<"gcp_iam_create_service_account">>,
        <<"Create an IAM service account">>,
        fun(#{<<"name">> := Name, <<"display_name">> := DisplayName}) ->
            create_service_account(Name, DisplayName)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Service account ID">>},
                <<"display_name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Display name">>}
            },
            <<"required">> => [<<"name">>, <<"display_name">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"gcp_iam_list_service_accounts">>,
        <<"List all service accounts">>,
        fun(_Args) -> list_service_accounts() end,
        #{<<"type">> => <<"object">>, <<"properties">> => #{}}),

    %% ===============================================
    %% RESOURCES
    %% ===============================================

    ok = erlmcp_stdio:add_resource(<<"gcp://status">>, <<"GCP Simulator Status">>,
        fun(_Uri) -> get_simulator_status() end, <<"text/plain">>),

    ok = erlmcp_stdio:add_resource(<<"gcp://help">>, <<"GCP Simulator Help">>,
        fun(_Uri) -> get_help_text() end, <<"text/plain">>),

    %% ===============================================
    %% PROMPTS
    %% ===============================================

    ok = erlmcp_stdio:add_prompt(<<"gcp_deploy_web_app">>,
        <<"Generate GCP deployment plan for a web application">>,
        fun(Args) ->
            AppName = maps:get(<<"app_name">>, Args, <<"my-app">>),
            [#{
                <<"role">> => <<"user">>,
                <<"content">> => #{
                    <<"type">> => <<"text">>,
                    <<"text">> => <<"Create a deployment plan for '", AppName/binary,
                                   "' web application on GCP. Include: ",
                                   "1) Compute Engine instance configuration, ",
                                   "2) Cloud Storage bucket for static assets, ",
                                   "3) Cloud SQL database setup, ",
                                   "4) Load balancer configuration.">>
                }
            }]
        end,
        [#{<<"name">> => <<"app_name">>, <<"description">> => <<"Application name">>, <<"required">> => false}]),

    logger:info("GCP Simulator configured with all tools, resources, and prompts~n").

%%====================================================================
%% COMPUTE ENGINE FUNCTIONS
%%====================================================================

create_compute_instance(Name, MachineType, Zone) ->
    gen_server:call(?MODULE, {create_compute_instance, Name, MachineType, Zone}, 5000).

list_compute_instances() ->
    gen_server:call(?MODULE, list_compute_instances, 5000).

get_compute_instance(Name) ->
    gen_server:call(?MODULE, {get_compute_instance, Name}, 5000).

start_compute_instance(Name) ->
    gen_server:call(?MODULE, {start_compute_instance, Name}, 5000).

stop_compute_instance(Name) ->
    gen_server:call(?MODULE, {stop_compute_instance, Name}, 5000).

delete_compute_instance(Name) ->
    gen_server:call(?MODULE, {delete_compute_instance, Name}, 5000).

format_instance(#{name := Name, machine_type := Type, zone := Zone,
                  status := Status, internal_ip := IntIP, external_ip := ExtIP}) ->
    iolist_to_binary(io_lib:format(
        "Instance: ~s~n"
        "  Machine Type: ~s~n"
        "  Zone: ~s~n"
        "  Status: ~s~n"
        "  Internal IP: ~s~n"
        "  External IP: ~s~n~n",
        [Name, Type, Zone, Status, IntIP, ExtIP]
    )).

%%====================================================================
%% CLOUD STORAGE FUNCTIONS
%%====================================================================

create_storage_bucket(Name, Location) ->
    gen_server:call(?MODULE, {create_storage_bucket, Name, Location}, 5000).

list_storage_buckets() ->
    gen_server:call(?MODULE, list_storage_buckets, 5000).

upload_storage_object(Bucket, ObjectName, Content) ->
    gen_server:call(?MODULE, {upload_storage_object, Bucket, ObjectName, Content}, 5000).

list_storage_objects(Bucket) ->
    gen_server:call(?MODULE, {list_storage_objects, Bucket}, 5000).

download_storage_object(Bucket, ObjectName) ->
    gen_server:call(?MODULE, {download_storage_object, Bucket, ObjectName}, 5000).

delete_storage_object(Bucket, ObjectName) ->
    gen_server:call(?MODULE, {delete_storage_object, Bucket, ObjectName}, 5000).

format_bucket(#{name := Name, location := Location, storage_class := Class}) ->
    iolist_to_binary(io_lib:format("  ~s (~s, ~s)~n", [Name, Location, Class])).

format_object(#{name := Name, size := Size, uploaded := Time}) ->
    iolist_to_binary(io_lib:format("  ~s (~p bytes, uploaded: ~s)~n", [Name, Size, Time])).

%%====================================================================
%% CLOUD FUNCTIONS
%%====================================================================

deploy_cloud_function(Name, Runtime, EntryPoint) ->
    gen_server:call(?MODULE, {deploy_cloud_function, Name, Runtime, EntryPoint}, 5000).

list_cloud_functions() ->
    gen_server:call(?MODULE, list_cloud_functions, 5000).

invoke_cloud_function(Name, Data) ->
    gen_server:call(?MODULE, {invoke_cloud_function, Name, Data}, 5000).

delete_cloud_function(Name) ->
    gen_server:call(?MODULE, {delete_cloud_function, Name}, 5000).

format_function(#{name := Name, runtime := Runtime, status := Status, url := Url}) ->
    iolist_to_binary(io_lib:format("  ~s (~s, ~s)~n    URL: ~s~n", [Name, Runtime, Status, Url])).

%%====================================================================
%% CLOUD SQL
%%====================================================================

create_sql_instance(Name, DbVersion, Tier) ->
    gen_server:call(?MODULE, {create_sql_instance, Name, DbVersion, Tier}, 5000).

list_sql_instances() ->
    gen_server:call(?MODULE, list_sql_instances, 5000).

delete_sql_instance(Name) ->
    gen_server:call(?MODULE, {delete_sql_instance, Name}, 5000).

format_sql_instance(#{name := Name, database_version := Version, state := State, ip_address := IP}) ->
    iolist_to_binary(io_lib:format("  ~s (~s, ~s, IP: ~s)~n", [Name, Version, State, IP])).

%%====================================================================
%% CLOUD PUB/SUB
%%====================================================================

create_pubsub_topic(Name) ->
    gen_server:call(?MODULE, {create_pubsub_topic, Name}, 5000).

list_pubsub_topics() ->
    gen_server:call(?MODULE, list_pubsub_topics, 5000).

publish_pubsub_message(Topic, Message) ->
    gen_server:call(?MODULE, {publish_pubsub_message, Topic, Message}, 5000).

create_pubsub_subscription(Name, Topic) ->
    gen_server:call(?MODULE, {create_pubsub_subscription, Name, Topic}, 5000).

%%====================================================================
%% IAM
%%====================================================================

create_service_account(Name, DisplayName) ->
    gen_server:call(?MODULE, {create_service_account, Name, DisplayName}, 5000).

list_service_accounts() ->
    gen_server:call(?MODULE, list_service_accounts, 5000).

format_service_account(#{name := Name, display_name := DisplayName, email := Email}) ->
    iolist_to_binary(io_lib:format("  ~s (~s) - ~s~n", [Name, DisplayName, Email])).

%%====================================================================
%% HELPER FUNCTIONS
%%====================================================================

get_simulator_status() ->
    gen_server:call(?MODULE, get_simulator_status, 5000).

get_help_text() ->
    <<"GCP SIMULATOR HELP\n\n"
      "COMPUTE ENGINE:\n"
      "  gcp_compute_create_instance - Create VM instance\n"
      "  gcp_compute_list_instances - List all instances\n"
      "  gcp_compute_get_instance - Get instance details\n"
      "  gcp_compute_start_instance - Start instance\n"
      "  gcp_compute_stop_instance - Stop instance\n"
      "  gcp_compute_delete_instance - Delete instance\n\n"
      "CLOUD STORAGE:\n"
      "  gcp_storage_create_bucket - Create bucket\n"
      "  gcp_storage_list_buckets - List buckets\n"
      "  gcp_storage_upload_object - Upload object\n"
      "  gcp_storage_list_objects - List objects in bucket\n"
      "  gcp_storage_download_object - Download object\n"
      "  gcp_storage_delete_object - Delete object\n\n"
      "CLOUD FUNCTIONS:\n"
      "  gcp_functions_deploy - Deploy function\n"
      "  gcp_functions_list - List functions\n"
      "  gcp_functions_invoke - Invoke function\n"
      "  gcp_functions_delete - Delete function\n\n"
      "CLOUD SQL:\n"
      "  gcp_sql_create_instance - Create SQL instance\n"
      "  gcp_sql_list_instances - List SQL instances\n"
      "  gcp_sql_delete_instance - Delete SQL instance\n\n"
      "PUB/SUB:\n"
      "  gcp_pubsub_create_topic - Create topic\n"
      "  gcp_pubsub_list_topics - List topics\n"
      "  gcp_pubsub_publish - Publish message\n"
      "  gcp_pubsub_create_subscription - Create subscription\n\n"
      "IAM:\n"
      "  gcp_iam_create_service_account - Create service account\n"
      "  gcp_iam_list_service_accounts - List service accounts\n\n"
      "RESOURCES:\n"
      "  gcp://status - Simulator status\n"
      "  gcp://help - This help text\n\n"
      "PROMPTS:\n"
      "  gcp_deploy_web_app - Web application deployment plan\n">>.

generate_ip() ->
    iolist_to_binary(io_lib:format("~p.~p.~p.~p",
        [rand:uniform(255), rand:uniform(255), rand:uniform(255), rand:uniform(255)])).

generate_message_id() ->
    iolist_to_binary(io_lib:format("~p", [rand:uniform(999999999)])).

get_timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0wZ",
        [Year, Month, Day, Hour, Minute, Second])).

wait_for_shutdown() ->
    case whereis(erlmcp_stdio_server) of
        undefined ->
            logger:warn("Stdio server not found, exiting~n");
        Pid ->
            monitor(process, Pid),
            receive
                {'DOWN', _Ref, process, Pid, _Reason} ->
                    logger:info("Stdio server terminated, exiting~n")
            end
    end.

%%====================================================================
%% VALIDATION FUNCTIONS
%%====================================================================

%% Validate resource name (not empty, reasonable length, no control chars)
validate_resource_name(Name) when is_binary(Name) ->
    Size = byte_size(Name),
    if
        Size =:= 0 -> {error, invalid_input};
        Size > 100 -> {error, invalid_input};
        true -> validate_no_control_chars(Name)
    end;
validate_resource_name(_) -> {error, invalid_input}.

%% Validate machine type format (e.g., e2-micro, n1-standard-1, n2-highmem-16)
validate_machine_type(Type) when is_binary(Type) ->
    case re:run(Type, <<"^[a-z]\\d+-[a-z]+-?\\d*$">>, [{capture, none}]) of
        match -> ok;
        nomatch -> {error, invalid_input}
    end;
validate_machine_type(_) -> {error, invalid_input}.

%% Validate zone format (e.g., us-central1-a, europe-west1-b)
validate_zone(Zone) when is_binary(Zone) ->
    case re:run(Zone, <<"^[a-z]+-[a-z]+\\d+-[a-z]$">>, [{capture, none}]) of
        match -> ok;
        nomatch -> {error, invalid_input}
    end;
validate_zone(_) -> {error, invalid_input}.

%% Validate location format (e.g., US, EU, ASIA, us-central1)
validate_location(Location) when is_binary(Location) ->
    case re:run(Location, <<"^[A-Z]{2}$|^[a-z]+-[a-z]+\\d+$">>, [{capture, none}]) of
        match -> ok;
        nomatch -> {error, invalid_input}
    end;
validate_location(_) -> {error, invalid_input}.

%% Validate runtime format (e.g., python39, nodejs18, go116)
validate_runtime(Runtime) when is_binary(Runtime) ->
    case re:run(Runtime, <<"^[a-z]+\\d+$">>, [{capture, none}]) of
        match -> ok;
        nomatch -> {error, invalid_input}
    end;
validate_runtime(_) -> {error, invalid_input}.

%% Validate entry point (function name, alphanumeric with underscores)
validate_entry_point(EntryPoint) when is_binary(EntryPoint) ->
    Size = byte_size(EntryPoint),
    if
        Size =:= 0 -> {error, invalid_input};
        Size > 100 -> {error, invalid_input};
        true ->
            case re:run(EntryPoint, <<"^[a-zA-Z_][a-zA-Z0-9_]*$">>, [{capture, none}]) of
                match -> ok;
                nomatch -> {error, invalid_input}
            end
    end;
validate_entry_point(_) -> {error, invalid_input}.

%% Validate database version (e.g., POSTGRES_14, MYSQL_8_0)
validate_database_version(DbVersion) when is_binary(DbVersion) ->
    case re:run(DbVersion, <<"^[A-Z]+_\\d+(_\\d+)?$">>, [{capture, none}]) of
        match -> ok;
        nomatch -> {error, invalid_input}
    end;
validate_database_version(_) -> {error, invalid_input}.

%% Validate tier format (e.g., db-f1-micro, db-n1-standard-1)
validate_tier(Tier) when is_binary(Tier) ->
    case re:run(Tier, <<"^db-[a-z]\\d+-[a-z]+-?\\d*$">>, [{capture, none}]) of
        match -> ok;
        nomatch -> {error, invalid_input}
    end;
validate_tier(_) -> {error, invalid_input}.

%% Validate email-safe name for service accounts (DNS-safe, no special chars)
validate_email_safe_name(Name) when is_binary(Name) ->
    Size = byte_size(Name),
    if
        Size =:= 0 -> {error, invalid_input};
        Size > 64 -> {error, invalid_input};
        true ->
            case re:run(Name, <<"^[a-z0-9-]+$">>, [{capture, none}]) of
                match -> ok;
                nomatch -> {error, invalid_input}
            end
    end;
validate_email_safe_name(_) -> {error, invalid_input}.

%% Validate no control characters that could break JSON/protocols
validate_no_control_chars(Binary) when is_binary(Binary) ->
    case re:run(Binary, <<"[\\x00-\\x1F\\x7F]">>, [{capture, none}]) of
        match -> {error, invalid_input};
        nomatch -> ok
    end;
validate_no_control_chars(_) -> {error, invalid_input}.

%%====================================================================
%% GEN_SERVER CALLBACKS
%%====================================================================

%% @private
init([]) ->
    %% Create ETS tables for each GCP service
    ComputeInstances = ets:new(gcp_compute_instances, [set, private]),
    StorageBuckets = ets:new(gcp_storage_buckets, [set, private]),
    StorageObjects = ets:new(gcp_storage_objects, [bag, private]),
    CloudFunctions = ets:new(gcp_cloud_functions, [set, private]),
    CloudSql = ets:new(gcp_cloud_sql, [set, private]),
    PubsubTopics = ets:new(gcp_pubsub_topics, [set, private]),
    PubsubSubscriptions = ets:new(gcp_pubsub_subscriptions, [set, private]),
    ServiceAccounts = ets:new(gcp_iam_service_accounts, [set, private]),

    logger:info("Initialized GCP state storage~n"),

    State = #state{
        compute_instances = ComputeInstances,
        storage_buckets = StorageBuckets,
        storage_objects = StorageObjects,
        cloud_functions = CloudFunctions,
        cloud_sql = CloudSql,
        pubsub_topics = PubsubTopics,
        pubsub_subscriptions = PubsubSubscriptions,
        service_accounts = ServiceAccounts
    },

    {ok, State}.

%% @private
handle_call({create_compute_instance, Name, MachineType, Zone}, _From, State) ->
    case validate_resource_name(Name) of
        {error, invalid_input} -> {reply, {error, invalid_input}, State};
        ok ->
            case validate_machine_type(MachineType) of
                {error, invalid_input} -> {reply, {error, invalid_input}, State};
                ok ->
                    case validate_zone(Zone) of
                        {error, invalid_input} -> {reply, {error, invalid_input}, State};
                        ok ->
                            Instance = #{
                                name => Name,
                                machine_type => MachineType,
                                zone => Zone,
                                status => <<"RUNNING">>,
                                internal_ip => generate_ip(),
                                external_ip => generate_ip(),
                                created => get_timestamp()
                            },
                            ets:insert(State#state.compute_instances, {Name, Instance}),
                            {reply, format_instance(Instance), State}
                    end
            end
    end;

handle_call(list_compute_instances, _From, State) ->
    Instances = ets:tab2list(State#state.compute_instances),
    Response = case Instances of
        [] -> <<"No instances found\n">>;
        _ ->
            Header = <<"COMPUTE ENGINE INSTANCES:\n\n">>,
            Body = [format_instance(I) || {_K, I} <- Instances],
            iolist_to_binary([Header, Body])
    end,
    {reply, Response, State};

handle_call({get_compute_instance, Name}, _From, State) ->
    Response = case ets:lookup(State#state.compute_instances, Name) of
        [{_, Instance}] -> {ok, format_instance(Instance)};
        [] -> {error, not_found}
    end,
    {reply, Response, State};

handle_call({start_compute_instance, Name}, _From, State) ->
    Response = case ets:lookup(State#state.compute_instances, Name) of
        [{_, Instance}] ->
            Updated = Instance#{status => <<"RUNNING">>},
            ets:insert(State#state.compute_instances, {Name, Updated}),
            {ok, <<"Instance started successfully\n">>};
        [] -> {error, not_found}
    end,
    {reply, Response, State};

handle_call({stop_compute_instance, Name}, _From, State) ->
    Response = case ets:lookup(State#state.compute_instances, Name) of
        [{_, Instance}] ->
            Updated = Instance#{status => <<"TERMINATED">>},
            ets:insert(State#state.compute_instances, {Name, Updated}),
            {ok, <<"Instance stopped successfully\n">>};
        [] -> {error, not_found}
    end,
    {reply, Response, State};

handle_call({delete_compute_instance, Name}, _From, State) ->
    Response = case ets:delete(State#state.compute_instances, Name) of
        true -> {ok, <<"Instance deleted successfully\n">>};
        false -> {error, not_found}
    end,
    {reply, Response, State};

handle_call({create_storage_bucket, Name, Location}, _From, State) ->
    case validate_resource_name(Name) of
        {error, invalid_input} -> {reply, {error, invalid_input}, State};
        ok ->
            case validate_location(Location) of
                {error, invalid_input} -> {reply, {error, invalid_input}, State};
                ok ->
                    Bucket = #{
                        name => Name,
                        location => Location,
                        created => get_timestamp(),
                        storage_class => <<"STANDARD">>
                    },
                    ets:insert(State#state.storage_buckets, {Name, Bucket}),
                    Response = iolist_to_binary(io_lib:format("Bucket '~s' created in ~s~n", [Name, Location])),
                    {reply, Response, State}
            end
    end;

handle_call(list_storage_buckets, _From, State) ->
    Buckets = ets:tab2list(State#state.storage_buckets),
    Response = case Buckets of
        [] -> <<"No buckets found\n">>;
        _ ->
            Header = <<"CLOUD STORAGE BUCKETS:\n\n">>,
            Body = [format_bucket(B) || {_K, B} <- Buckets],
            iolist_to_binary([Header, Body])
    end,
    {reply, Response, State};

handle_call({upload_storage_object, Bucket, ObjectName, Content}, _From, State) ->
    Response = case ets:lookup(State#state.storage_buckets, Bucket) of
        [] -> {error, not_found};
        _ ->
            Object = #{
                bucket => Bucket,
                name => ObjectName,
                content => Content,
                size => byte_size(Content),
                uploaded => get_timestamp()
            },
            ets:insert(State#state.storage_objects, {{Bucket, ObjectName}, Object}),
            {ok, iolist_to_binary(io_lib:format("Object '~s' uploaded to bucket '~s' (~p bytes)~n",
                                          [ObjectName, Bucket, byte_size(Content)]))}
    end,
    {reply, Response, State};

handle_call({list_storage_objects, Bucket}, _From, State) ->
    Objects = ets:match_object(State#state.storage_objects, {{Bucket, '_'}, '_'}),
    Response = case Objects of
        [] -> iolist_to_binary(io_lib:format("No objects in bucket '~s'~n", [Bucket]));
        _ ->
            Header = iolist_to_binary(io_lib:format("OBJECTS IN BUCKET '~s':~n~n", [Bucket])),
            Body = [format_object(O) || {_K, O} <- Objects],
            iolist_to_binary([Header, Body])
    end,
    {reply, Response, State};

handle_call({download_storage_object, Bucket, ObjectName}, _From, State) ->
    Response = case ets:lookup(State#state.storage_objects, {Bucket, ObjectName}) of
        [{_, #{content := Content}}] ->
            {ok, iolist_to_binary(io_lib:format("Object content:~n~s~n", [Content]))};
        [] -> {error, not_found}
    end,
    {reply, Response, State};

handle_call({delete_storage_object, Bucket, ObjectName}, _From, State) ->
    Response = case ets:delete(State#state.storage_objects, {Bucket, ObjectName}) of
        true -> {ok, <<"Object deleted successfully\n">>};
        false -> {error, not_found}
    end,
    {reply, Response, State};

handle_call({deploy_cloud_function, Name, Runtime, EntryPoint}, _From, State) ->
    case validate_resource_name(Name) of
        {error, invalid_input} -> {reply, {error, invalid_input}, State};
        ok ->
            case validate_runtime(Runtime) of
                {error, invalid_input} -> {reply, {error, invalid_input}, State};
                ok ->
                    case validate_entry_point(EntryPoint) of
                        {error, invalid_input} -> {reply, {error, invalid_input}, State};
                        ok ->
                            Function = #{
                                name => Name,
                                runtime => Runtime,
                                entry_point => EntryPoint,
                                status => <<"ACTIVE">>,
                                deployed => get_timestamp(),
                                url => iolist_to_binary(io_lib:format("https://~s-abcd123.cloudfunctions.net", [Name]))
                            },
                            ets:insert(State#state.cloud_functions, {Name, Function}),
                            Response = iolist_to_binary(io_lib:format(
                                "Function '~s' deployed successfully~n"
                                "Runtime: ~s~n"
                                "Entry Point: ~s~n"
                                "URL: ~s~n",
                                [Name, Runtime, EntryPoint, maps:get(url, Function)]
                            )),
                            {reply, Response, State}
                    end
            end
    end;

handle_call(list_cloud_functions, _From, State) ->
    Functions = ets:tab2list(State#state.cloud_functions),
    Response = case Functions of
        [] -> <<"No functions found\n">>;
        _ ->
            Header = <<"CLOUD FUNCTIONS:\n\n">>,
            Body = [format_function(F) || {_K, F} <- Functions],
            iolist_to_binary([Header, Body])
    end,
    {reply, Response, State};

handle_call({invoke_cloud_function, Name, Data}, _From, State) ->
    Response = case ets:lookup(State#state.cloud_functions, Name) of
        [{_, #{status := <<"ACTIVE">>}}] ->
            {ok, iolist_to_binary(io_lib:format(
                "Function '~s' invoked successfully~n"
                "Input: ~s~n"
                "Output: {\"result\": \"processed\", \"status\": \"success\"}~n",
                [Name, Data]
            ))};
        [{_, _}] -> {error, invalid_state};
        [] -> {error, not_found}
    end,
    {reply, Response, State};

handle_call({delete_cloud_function, Name}, _From, State) ->
    Response = case ets:delete(State#state.cloud_functions, Name) of
        true -> {ok, <<"Function deleted successfully\n">>};
        false -> {error, not_found}
    end,
    {reply, Response, State};

handle_call({create_sql_instance, Name, DbVersion, Tier}, _From, State) ->
    case validate_resource_name(Name) of
        {error, invalid_input} -> {reply, {error, invalid_input}, State};
        ok ->
            case validate_database_version(DbVersion) of
                {error, invalid_input} -> {reply, {error, invalid_input}, State};
                ok ->
                    case validate_tier(Tier) of
                        {error, invalid_input} -> {reply, {error, invalid_input}, State};
                        ok ->
                            Instance = #{
                                name => Name,
                                database_version => DbVersion,
                                tier => Tier,
                                state => <<"RUNNABLE">>,
                                connection_name => iolist_to_binary(io_lib:format("project:region:~s", [Name])),
                                ip_address => generate_ip(),
                                created => get_timestamp()
                            },
                            ets:insert(State#state.cloud_sql, {Name, Instance}),
                            Response = iolist_to_binary(io_lib:format(
                                "Cloud SQL instance '~s' created~n"
                                "Database Version: ~s~n"
                                "Tier: ~s~n"
                                "IP Address: ~s~n",
                                [Name, DbVersion, Tier, maps:get(ip_address, Instance)]
                            )),
                            {reply, Response, State}
                    end
            end
    end;

handle_call(list_sql_instances, _From, State) ->
    Instances = ets:tab2list(State#state.cloud_sql),
    Response = case Instances of
        [] -> <<"No SQL instances found\n">>;
        _ ->
            Header = <<"CLOUD SQL INSTANCES:\n\n">>,
            Body = [format_sql_instance(I) || {_K, I} <- Instances],
            iolist_to_binary([Header, Body])
    end,
    {reply, Response, State};

handle_call({delete_sql_instance, Name}, _From, State) ->
    Response = case ets:delete(State#state.cloud_sql, Name) of
        true -> {ok, <<"SQL instance deleted successfully\n">>};
        false -> {error, not_found}
    end,
    {reply, Response, State};

handle_call({create_pubsub_topic, Name}, _From, State) ->
    case validate_resource_name(Name) of
        {error, invalid_input} -> {reply, {error, invalid_input}, State};
        ok ->
            Topic = #{
                name => Name,
                created => get_timestamp()
            },
            ets:insert(State#state.pubsub_topics, {Name, Topic}),
            Response = iolist_to_binary(io_lib:format("Topic '~s' created~n", [Name])),
            {reply, Response, State}
    end;

handle_call(list_pubsub_topics, _From, State) ->
    Topics = ets:tab2list(State#state.pubsub_topics),
    Response = case Topics of
        [] -> <<"No topics found\n">>;
        _ ->
            Header = <<"PUB/SUB TOPICS:\n\n">>,
            Body = [iolist_to_binary(io_lib:format("  ~s~n", [N])) || {N, _} <- Topics],
            iolist_to_binary([Header, Body])
    end,
    {reply, Response, State};

handle_call({publish_pubsub_message, Topic, Message}, _From, State) ->
    Response = case ets:lookup(State#state.pubsub_topics, Topic) of
        [] -> {error, not_found};
        _ ->
            MessageId = generate_message_id(),
            {ok, iolist_to_binary(io_lib:format(
                "Message published to topic '~s'~n"
                "Message ID: ~s~n"
                "Message: ~s~n",
                [Topic, MessageId, Message]
            ))}
    end,
    {reply, Response, State};

handle_call({create_pubsub_subscription, Name, Topic}, _From, State) ->
    Response = case ets:lookup(State#state.pubsub_topics, Topic) of
        [] -> {error, not_found};
        _ ->
            Subscription = #{
                name => Name,
                topic => Topic,
                created => get_timestamp()
            },
            ets:insert(State#state.pubsub_subscriptions, {Name, Subscription}),
            {ok, iolist_to_binary(io_lib:format("Subscription '~s' created for topic '~s'~n", [Name, Topic]))}
    end,
    {reply, Response, State};

handle_call({create_service_account, Name, DisplayName}, _From, State) ->
    case validate_email_safe_name(Name) of
        {error, invalid_input} -> {reply, {error, invalid_input}, State};
        ok ->
            case validate_no_control_chars(DisplayName) of
                {error, invalid_input} -> {reply, {error, invalid_input}, State};
                ok ->
                    ServiceAccount = #{
                        name => Name,
                        display_name => DisplayName,
                        email => iolist_to_binary(io_lib:format("~s@project.iam.gserviceaccount.com", [Name])),
                        created => get_timestamp()
                    },
                    ets:insert(State#state.service_accounts, {Name, ServiceAccount}),
                    Response = iolist_to_binary(io_lib:format(
                        "Service account created~n"
                        "Name: ~s~n"
                        "Display Name: ~s~n"
                        "Email: ~s~n",
                        [Name, DisplayName, maps:get(email, ServiceAccount)]
                    )),
                    {reply, Response, State}
            end
    end;

handle_call(list_service_accounts, _From, State) ->
    Accounts = ets:tab2list(State#state.service_accounts),
    Response = case Accounts of
        [] -> <<"No service accounts found\n">>;
        _ ->
            Header = <<"SERVICE ACCOUNTS:\n\n">>,
            Body = [format_service_account(A) || {_K, A} <- Accounts],
            iolist_to_binary([Header, Body])
    end,
    {reply, Response, State};

handle_call(get_simulator_status, _From, State) ->
    ComputeCount = ets:info(State#state.compute_instances, size),
    BucketCount = ets:info(State#state.storage_buckets, size),
    FunctionCount = ets:info(State#state.cloud_functions, size),
    SqlCount = ets:info(State#state.cloud_sql, size),
    TopicCount = ets:info(State#state.pubsub_topics, size),
    AccountCount = ets:info(State#state.service_accounts, size),

    Response = iolist_to_binary(io_lib:format(
        "GCP SIMULATOR STATUS~n~n"
        "Compute Engine Instances: ~p~n"
        "Cloud Storage Buckets: ~p~n"
        "Cloud Functions: ~p~n"
        "Cloud SQL Instances: ~p~n"
        "Pub/Sub Topics: ~p~n"
        "Service Accounts: ~p~n~n"
        "Simulator Version: 1.0.0~n"
        "Status: Active~n",
        [ComputeCount, BucketCount, FunctionCount, SqlCount, TopicCount, AccountCount]
    )),
    {reply, Response, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    %% Clean up ETS tables
    ets:delete(State#state.compute_instances),
    ets:delete(State#state.storage_buckets),
    ets:delete(State#state.storage_objects),
    ets:delete(State#state.cloud_functions),
    ets:delete(State#state.cloud_sql),
    ets:delete(State#state.pubsub_topics),
    ets:delete(State#state.pubsub_subscriptions),
    ets:delete(State#state.service_accounts),
    logger:info("GCP Simulator terminated: ~p~n", [_Reason]),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
