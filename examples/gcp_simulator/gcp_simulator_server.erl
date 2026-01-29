-module(gcp_simulator_server).
-export([start/0, main/1, init_state/0]).

%% GCP Simulator MCP Server
%% Simulates common GCP services for testing and development
%% Supports: Compute Engine, Cloud Storage, Cloud Functions, Cloud SQL, Pub/Sub, IAM

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

    %% Initialize state storage
    init_state(),

    %% Start the stdio MCP server
    case erlmcp_stdio:start() of
        ok ->
            logger:info("Successfully started stdio server~n"),
            setup_gcp_simulator(),
            logger:info("GCP Simulator setup complete, waiting for shutdown...~n"),
            wait_for_shutdown();
        {error, StartErrReason} ->
            logger:error("Failed to start stdio server: ~p", [StartErrReason]),
            halt(1)
    end.

%% Initialize ETS tables for state storage
init_state() ->
    %% Create ETS tables for each GCP service
    ets:new(gcp_compute_instances, [named_table, public, set]),
    ets:new(gcp_storage_buckets, [named_table, public, set]),
    ets:new(gcp_storage_objects, [named_table, public, bag]),
    ets:new(gcp_cloud_functions, [named_table, public, set]),
    ets:new(gcp_cloud_sql, [named_table, public, set]),
    ets:new(gcp_pubsub_topics, [named_table, public, set]),
    ets:new(gcp_pubsub_subscriptions, [named_table, public, set]),
    ets:new(gcp_iam_service_accounts, [named_table, public, set]),
    logger:info("Initialized GCP state storage~n"),
    ok.

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
    Instance = #{
        name => Name,
        machine_type => MachineType,
        zone => Zone,
        status => <<"RUNNING">>,
        internal_ip => generate_ip(),
        external_ip => generate_ip(),
        created => get_timestamp()
    },
    ets:insert(gcp_compute_instances, {Name, Instance}),
    format_instance(Instance).

list_compute_instances() ->
    Instances = ets:tab2list(gcp_compute_instances),
    case Instances of
        [] -> <<"No instances found\n">>;
        _ ->
            Header = <<"COMPUTE ENGINE INSTANCES:\n\n">>,
            Body = [format_instance(I) || {_K, I} <- Instances],
            iolist_to_binary([Header, Body])
    end.

get_compute_instance(Name) ->
    case ets:lookup(gcp_compute_instances, Name) of
        [{_, Instance}] -> format_instance(Instance);
        [] -> <<"Error: Instance not found\n">>
    end.

start_compute_instance(Name) ->
    case ets:lookup(gcp_compute_instances, Name) of
        [{_, Instance}] ->
            Updated = Instance#{status => <<"RUNNING">>},
            ets:insert(gcp_compute_instances, {Name, Updated}),
            <<"Instance started successfully\n">>;
        [] -> <<"Error: Instance not found\n">>
    end.

stop_compute_instance(Name) ->
    case ets:lookup(gcp_compute_instances, Name) of
        [{_, Instance}] ->
            Updated = Instance#{status => <<"TERMINATED">>},
            ets:insert(gcp_compute_instances, {Name, Updated}),
            <<"Instance stopped successfully\n">>;
        [] -> <<"Error: Instance not found\n">>
    end.

delete_compute_instance(Name) ->
    case ets:delete(gcp_compute_instances, Name) of
        true -> <<"Instance deleted successfully\n">>;
        false -> <<"Error: Instance not found\n">>
    end.

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
    Bucket = #{
        name => Name,
        location => Location,
        created => get_timestamp(),
        storage_class => <<"STANDARD">>
    },
    ets:insert(gcp_storage_buckets, {Name, Bucket}),
    iolist_to_binary(io_lib:format("Bucket '~s' created in ~s~n", [Name, Location])).

list_storage_buckets() ->
    Buckets = ets:tab2list(gcp_storage_buckets),
    case Buckets of
        [] -> <<"No buckets found\n">>;
        _ ->
            Header = <<"CLOUD STORAGE BUCKETS:\n\n">>,
            Body = [format_bucket(B) || {_K, B} <- Buckets],
            iolist_to_binary([Header, Body])
    end.

upload_storage_object(Bucket, ObjectName, Content) ->
    case ets:lookup(gcp_storage_buckets, Bucket) of
        [] -> <<"Error: Bucket not found\n">>;
        _ ->
            Object = #{
                bucket => Bucket,
                name => ObjectName,
                content => Content,
                size => byte_size(Content),
                uploaded => get_timestamp()
            },
            ets:insert(gcp_storage_objects, {{Bucket, ObjectName}, Object}),
            iolist_to_binary(io_lib:format("Object '~s' uploaded to bucket '~s' (~p bytes)~n",
                                          [ObjectName, Bucket, byte_size(Content)]))
    end.

list_storage_objects(Bucket) ->
    Objects = ets:match_object(gcp_storage_objects, {{Bucket, '_'}, '_'}),
    case Objects of
        [] -> iolist_to_binary(io_lib:format("No objects in bucket '~s'~n", [Bucket]));
        _ ->
            Header = iolist_to_binary(io_lib:format("OBJECTS IN BUCKET '~s':~n~n", [Bucket])),
            Body = [format_object(O) || {_K, O} <- Objects],
            iolist_to_binary([Header, Body])
    end.

download_storage_object(Bucket, ObjectName) ->
    case ets:lookup(gcp_storage_objects, {Bucket, ObjectName}) of
        [{_, #{content := Content}}] ->
            iolist_to_binary(io_lib:format("Object content:~n~s~n", [Content]));
        [] -> <<"Error: Object not found\n">>
    end.

delete_storage_object(Bucket, ObjectName) ->
    case ets:delete(gcp_storage_objects, {Bucket, ObjectName}) of
        true -> <<"Object deleted successfully\n">>;
        false -> <<"Error: Object not found\n">>
    end.

format_bucket(#{name := Name, location := Location, storage_class := Class}) ->
    iolist_to_binary(io_lib:format("  ~s (~s, ~s)~n", [Name, Location, Class])).

format_object(#{name := Name, size := Size, uploaded := Time}) ->
    iolist_to_binary(io_lib:format("  ~s (~p bytes, uploaded: ~s)~n", [Name, Size, Time])).

%%====================================================================
%% CLOUD FUNCTIONS
%%====================================================================

deploy_cloud_function(Name, Runtime, EntryPoint) ->
    Function = #{
        name => Name,
        runtime => Runtime,
        entry_point => EntryPoint,
        status => <<"ACTIVE">>,
        deployed => get_timestamp(),
        url => iolist_to_binary(io_lib:format("https://~s-abcd123.cloudfunctions.net", [Name]))
    },
    ets:insert(gcp_cloud_functions, {Name, Function}),
    iolist_to_binary(io_lib:format(
        "Function '~s' deployed successfully~n"
        "Runtime: ~s~n"
        "Entry Point: ~s~n"
        "URL: ~s~n",
        [Name, Runtime, EntryPoint, maps:get(url, Function)]
    )).

list_cloud_functions() ->
    Functions = ets:tab2list(gcp_cloud_functions),
    case Functions of
        [] -> <<"No functions found\n">>;
        _ ->
            Header = <<"CLOUD FUNCTIONS:\n\n">>,
            Body = [format_function(F) || {_K, F} <- Functions],
            iolist_to_binary([Header, Body])
    end.

invoke_cloud_function(Name, Data) ->
    case ets:lookup(gcp_cloud_functions, Name) of
        [{_, #{status := <<"ACTIVE">>}}] ->
            iolist_to_binary(io_lib:format(
                "Function '~s' invoked successfully~n"
                "Input: ~s~n"
                "Output: {\"result\": \"processed\", \"status\": \"success\"}~n",
                [Name, Data]
            ));
        [{_, _}] -> <<"Error: Function is not active\n">>;
        [] -> <<"Error: Function not found\n">>
    end.

delete_cloud_function(Name) ->
    case ets:delete(gcp_cloud_functions, Name) of
        true -> <<"Function deleted successfully\n">>;
        false -> <<"Error: Function not found\n">>
    end.

format_function(#{name := Name, runtime := Runtime, status := Status, url := Url}) ->
    iolist_to_binary(io_lib:format("  ~s (~s, ~s)~n    URL: ~s~n", [Name, Runtime, Status, Url])).

%%====================================================================
%% CLOUD SQL
%%====================================================================

create_sql_instance(Name, DbVersion, Tier) ->
    Instance = #{
        name => Name,
        database_version => DbVersion,
        tier => Tier,
        state => <<"RUNNABLE">>,
        connection_name => iolist_to_binary(io_lib:format("project:region:~s", [Name])),
        ip_address => generate_ip(),
        created => get_timestamp()
    },
    ets:insert(gcp_cloud_sql, {Name, Instance}),
    iolist_to_binary(io_lib:format(
        "Cloud SQL instance '~s' created~n"
        "Database Version: ~s~n"
        "Tier: ~s~n"
        "IP Address: ~s~n",
        [Name, DbVersion, Tier, maps:get(ip_address, Instance)]
    )).

list_sql_instances() ->
    Instances = ets:tab2list(gcp_cloud_sql),
    case Instances of
        [] -> <<"No SQL instances found\n">>;
        _ ->
            Header = <<"CLOUD SQL INSTANCES:\n\n">>,
            Body = [format_sql_instance(I) || {_K, I} <- Instances],
            iolist_to_binary([Header, Body])
    end.

delete_sql_instance(Name) ->
    case ets:delete(gcp_cloud_sql, Name) of
        true -> <<"SQL instance deleted successfully\n">>;
        false -> <<"Error: SQL instance not found\n">>
    end.

format_sql_instance(#{name := Name, database_version := Version, state := State, ip_address := IP}) ->
    iolist_to_binary(io_lib:format("  ~s (~s, ~s, IP: ~s)~n", [Name, Version, State, IP])).

%%====================================================================
%% CLOUD PUB/SUB
%%====================================================================

create_pubsub_topic(Name) ->
    Topic = #{
        name => Name,
        created => get_timestamp()
    },
    ets:insert(gcp_pubsub_topics, {Name, Topic}),
    iolist_to_binary(io_lib:format("Topic '~s' created~n", [Name])).

list_pubsub_topics() ->
    Topics = ets:tab2list(gcp_pubsub_topics),
    case Topics of
        [] -> <<"No topics found\n">>;
        _ ->
            Header = <<"PUB/SUB TOPICS:\n\n">>,
            Body = [iolist_to_binary(io_lib:format("  ~s~n", [N])) || {N, _} <- Topics],
            iolist_to_binary([Header, Body])
    end.

publish_pubsub_message(Topic, Message) ->
    case ets:lookup(gcp_pubsub_topics, Topic) of
        [] -> <<"Error: Topic not found\n">>;
        _ ->
            MessageId = generate_message_id(),
            iolist_to_binary(io_lib:format(
                "Message published to topic '~s'~n"
                "Message ID: ~s~n"
                "Message: ~s~n",
                [Topic, MessageId, Message]
            ))
    end.

create_pubsub_subscription(Name, Topic) ->
    case ets:lookup(gcp_pubsub_topics, Topic) of
        [] -> <<"Error: Topic not found\n">>;
        _ ->
            Subscription = #{
                name => Name,
                topic => Topic,
                created => get_timestamp()
            },
            ets:insert(gcp_pubsub_subscriptions, {Name, Subscription}),
            iolist_to_binary(io_lib:format("Subscription '~s' created for topic '~s'~n", [Name, Topic]))
    end.

%%====================================================================
%% IAM
%%====================================================================

create_service_account(Name, DisplayName) ->
    ServiceAccount = #{
        name => Name,
        display_name => DisplayName,
        email => iolist_to_binary(io_lib:format("~s@project.iam.gserviceaccount.com", [Name])),
        created => get_timestamp()
    },
    ets:insert(gcp_iam_service_accounts, {Name, ServiceAccount}),
    iolist_to_binary(io_lib:format(
        "Service account created~n"
        "Name: ~s~n"
        "Display Name: ~s~n"
        "Email: ~s~n",
        [Name, DisplayName, maps:get(email, ServiceAccount)]
    )).

list_service_accounts() ->
    Accounts = ets:tab2list(gcp_iam_service_accounts),
    case Accounts of
        [] -> <<"No service accounts found\n">>;
        _ ->
            Header = <<"SERVICE ACCOUNTS:\n\n">>,
            Body = [format_service_account(A) || {_K, A} <- Accounts],
            iolist_to_binary([Header, Body])
    end.

format_service_account(#{name := Name, display_name := DisplayName, email := Email}) ->
    iolist_to_binary(io_lib:format("  ~s (~s) - ~s~n", [Name, DisplayName, Email])).

%%====================================================================
%% HELPER FUNCTIONS
%%====================================================================

get_simulator_status() ->
    ComputeCount = ets:info(gcp_compute_instances, size),
    BucketCount = ets:info(gcp_storage_buckets, size),
    FunctionCount = ets:info(gcp_cloud_functions, size),
    SqlCount = ets:info(gcp_cloud_sql, size),
    TopicCount = ets:info(gcp_pubsub_topics, size),
    AccountCount = ets:info(gcp_iam_service_accounts, size),

    iolist_to_binary(io_lib:format(
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
    )).

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
