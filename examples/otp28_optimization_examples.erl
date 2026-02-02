%% @doc OTP 28 Optimization Examples for erlmcp
%% Demonstrates practical implementation of OTP 28 features in erlmcp codebase
-module(otp28_optimization_examples).

-include("erlmcp.hrl").

%% API exports
-export([
    priority_message_example/0,
    strict_generator_example/0,
    zip_generator_example/0,
    hibernate_example/0,
    tls13_optimization_example/0,
    zstd_compression_example/0,
    process_iterator_example/0,
    nominal_types_example/0,
    pcre2_regex_example/0
]).

%%====================================================================
%% OTP 28 Feature Examples
%%====================================================================

%% @doc Priority Messages Example for High-Priority MCP Events
priority_message_example() ->
    %% Create priority alias for high-priority MCP messages
    PrioAlias = alias([priority]),

    %% Register with process that needs priority handling
    erlmcp_connection_manager:start_link([{priority_alias, PrioAlias}]),

    %% Send high-priority message (skips regular queue)
    erlang:send(PrioAlias, #mcp_system_event{
        type = 'connection_error',
        server_id = <<"server1">>,
        details = #{reason => tcp_timeout, timestamp => erlang:system_time(millisecond)}
    }, [priority]),

    %% Send priority exit signal
    exit(PrioAlias, shutdown, [priority]),

    %% Monitor with priority
    erlang:monitor(PrioAlias, erlmcp_client, [priority]),
    ok.

%% @doc Strict Generators Example for MCP Resource Validation
strict_generator_example() ->
    %% Simulate raw MCP resources from external source
    RawResources = [
        #mcp_resource{name = "user", uri = "mcp://users/123"},
        #mcp_resource{name = "invalid", uri = undefined},  %% Will cause error
        #mcp_resource{name = "project", uri = "mcp://projects/456"}
    ],

    try
        %% Strict validation - will crash on invalid resource
        ValidResources = [Resource ||
            #mcp_resource{name = Name, uri = URI} <- RawResources,
            URI =/= undefined
        ],
        io:format("Valid resources: ~p~n", [ValidResources])
    catch
        error:no_match ->
            %% Handle strict generator error
            io:format("Invalid resource data detected~n"),
            handle_invalid_resources(RawResources)
    end,

    %% Alternative with proper error handling
    ValidResources = [Resource ||
        #mcp_resource{name = Name, uri = URI} <- RawResources,
        case URI of
            undefined -> false;
            _ -> true
        end
    ],
    ValidResources.

%% @doc Zip Generators Example for Parallel Processing
zip_generator_example() ->
    %% Multiple handlers and resources for parallel processing
    Handlers = [json_handler, xml_handler, binary_handler],
    Resources = [user_data, project_data, system_data],

    %% Process resources with all handlers in parallel
    Results = [Result ||
        Resource <- Resources && Handler <- Handlers,
        Result = process_resource_handler(Resource, Handler)
    ],

    %% Tool processing with multiple argument sets
    Tools = [file_reader, data_transformer, validator],
    ToolArgs = [#{path => "/data"}, #{format => json}, #{strict => true}],

    ToolResults = [ToolResult ||
        Tool <- Tools && Args <- ToolArgs,
        ToolResult = call_tool(Tool, Args)
    ],

    %% Subscription matching for active clients
    ActiveSubscriptions = [Sub ||
        Resource <- ?ACTIVE_RESOURCES && Client <- ?ACTIVE_CLIENTS,
        Sub = get_subscription(Resource, Client)
    ],

    Results.

%% @doc Hibernate Example for Memory Optimization
hibernate_example() ->
    %% Start connection manager with hibernation capability
    {ok, ConnMgr} = erlmcp_connection_manager:start_link([
        {hibernate_timeout, 30000},
        {max_connections, 1000}
    ]),

    %% Process messages with hibernation for long idle times
    connection_loop(ConnMgr, #{connections => #{}, stats => #{}).

    %% Session management with memory optimization
    {ok, SessionPid} = erlmcp_session:start_link(SessionId),
    session_loop(SessionPid, SessionId, #{}),

    ok.

%% @doc TLS 1.3 Optimization Example
tls13_optimization_example() ->
    %% TLS 1.3 optimized configuration for MCP connections
    TransportOpts = #{
        transport => ssl,
        ssl_opts => #{
            verify => verify_peer,
            versions => [tlsv1_3],
            ciphers => ssl:cipher_suites(tls13, 'all', 'strong'),
            server_name_indication => disable,
            secure_renegotiate => true,
            %% OTP 28 specific optimizations
            client_preferred_next_protocols => [<<"h2">>],
            honor_cipher_order => true,
            signature_algs => [rsa_pkcs1_sha256, ecdsa_secp256r1_sha256],
            key_update => at_user_request
        }
    },

    %% Gun HTTP/2 client with TLS 1.3
    GunOpts = #{
        transport => ssl,
        transport_opts => TransportOpts,
        proto_opts => #{
            %% OTP 28 improvements
            connection_window_size => 10 * 1024 * 1024,  %% 10MB
            stream_window_size => 1 * 1024 * 1024,       %% 1MB
            idle_timeout => 30000,
            request_timeout => 15000,
            %% Enable HTTP/2 priority for MCP messages
            priority => true
        }
    },

    %% Start optimized transport
    {ok, _} = erlmcp_transport:start_link(http, GunOpts),
    ok.

%% @doc Zstandard Compression Example
zstd_compression_example() ->
    %% Example with large MCP resource data
    LargeResourceData = generate_large_resource_data(),

    %% Compress resource data for efficient transfer
    Compressed = zstd:compress(LargeResourceData, 3),  %% Level 3 compression
    CompressionRatio = byte_size(LargeResourceData) / byte_size(Compressed),

    %% Create compressed MCP response
    Response = #{
        compressed => true,
        data => Compressed,
        original_size => byte_size(LargeResourceData),
        compressed_size => byte_size(Compressed),
        ratio => CompressionRatio,
        encoding => zstd
    },

    %% Batch request compression
    BatchRequests = [
        #mcp_request{method = read_resource, params = #{uri => "uri1"}},
        #mcp_request{method = read_resource, params = #{uri => "uri2"}},
        #mcp_request{method = read_resource, params = #{uri => "uri3"}}
    ],

    BatchData = jsx:encode(BatchRequests),
    CompressedBatch = zstd:compress(BatchData),

    %% Batch response handling
    handle_compressed_batch(CompressedBatch, Response),

    ok.

%% @doc Process Iterator Example for Large Scale Operations
process_iterator_example() ->
    %% Efficient process monitoring for large deployments
    monitor_all_processes(),

    %% Connection management with many processes
    find_active_connections(),

    %% Resource cleanup for terminated processes
    cleanup_orphaned_resources(),

    %% Memory optimization for large process trees
    optimize_process_memory(),

    ok.

%% @doc Nominal Types Example for MCP Type Safety
nominal_types_example() ->
    %% Define MCP-specific nominal types
    -nominal mcp_resource_id() :: binary().
    -nominal mcp_tool_id() :: binary().
    -nominal mcp_session_id() :: binary().
    -nominal mcp_request_id() :: binary().

    %% Type-safe resource registration
    ResourceId = <<"res_123">>,
    ResourceConfig = #{type => user, data => #{name => "john"}},

    try
        RegisteredId = register_resource(ResourceId, ResourceConfig),
        io:format("Registered resource: ~p~n", [RegisteredId])
    catch
        error:{invalid_type, Expected, Actual} ->
            io:format("Type error: expected ~p, got ~p~n", [Expected, Actual])
    end,

    %% Type-safe tool calls
    ToolId = <<"tool_456">>,
    ToolArgs = #{input => "test data"},

    try
        ToolResult = call_tool(ToolId, ToolArgs),
        io:format("Tool result: ~p~n", [ToolResult])
    catch
        error:{type_mismatch, Parameter, ExpectedType} ->
            io:format("Type error in parameter ~p: expected ~p~n",
                     [Parameter, ExpectedType])
    end,

    ok.

%% @doc PCRE2 Regex Example for Modern Pattern Matching
pcre2_regex_example() ->
    %% Modern regex patterns for MCP validation
    Patterns = #{
        resource_uri => re:compile("^[a-z]+://[\\w\\-.]+(:\\d+)?/.*$", [unicode]),
        tool_name => re:compile("^[a-z][a-z0-9_]*$", [unicode]),
        session_id => re:compile("^[a-fA-F0-9]{8}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{4}-[a-fA-F0-9]{12}$", [unicode]),
        json_key => re:compile("^[a-zA-Z_][a-zA-Z0-9_]*$", [unicode])
    },

    %% Validate MCP resource URIs
    ValidUris = [
        "mcp://users/123",
        "mcp://projects/456/files",
        "http://example.com/data"
    ],
    InvalidUris = [
        "invalid_uri",
        "ftp://example.com",
        "mcp://"
    ],

    UriValidation = lists:map(fun(Uri) ->
        case re:run(Uri, maps:get(resource_uri, Patterns)) of
            nomatch -> {invalid, Uri};
            _ -> {valid, Uri}
        end
    end, ValidUris ++ InvalidUris),

    io:format("URI Validation: ~p~n", [UriValidation]),

    %% Tool name validation
    ToolNames = ["file_reader", "data_transformer", "1invalid_tool"],
    ToolValidation = lists:map(fun(Name) ->
        case re:run(Name, maps:get(tool_name, Patterns)) of
            nomatch -> {invalid, Name};
            _ -> {valid, Name}
        end
    end, ToolNames),

    ToolValidation.

%%====================================================================
%% Helper Functions
%%====================================================================

connection_loop(ConnMgr, State) ->
    receive
        {connect, ClientId} ->
            NewState = handle_connect(ConnMgr, ClientId, State),
            connection_loop(ConnMgr, NewState);
        {disconnect, ClientId} ->
            NewState = handle_disconnect(ConnMgr, ClientId, State),
            connection_loop(ConnMgr, NewState);
        {system_event, Event} ->
            NewState = handle_system_event(ConnMgr, Event, State),
            connection_loop(ConnMgr, NewState);
        _ ->
            connection_loop(ConnMgr, State)
    after 30000 ->
        %% Hibernate when idle for 30 seconds
        erlang:hibernate()
    end.

session_loop(SessionPid, SessionId, State) ->
    receive
        {message, Data} ->
            NewState = handle_message(SessionPid, SessionId, Data, State),
            session_loop(SessionPid, SessionId, NewState);
        {ping, _} ->
            pong = erlmcp_session:ping(SessionPid),
            session_loop(SessionPid, SessionId, State);
        _ ->
            session_loop(SessionPid, SessionId, State)
    after 60000 ->
        %% Hibernate idle sessions to save memory
        erlang:hibernate()
    end.

process_resource_handler(Resource, Handler) ->
    %% Simulate parallel processing with OTP 28 zip generators
    case {Resource, Handler} of
        {user_data, json_handler} ->
            process_user_json(Resource);
        {user_data, xml_handler} ->
            process_user_xml(Resource);
        {user_data, binary_handler} ->
            process_user_binary(Resource);
        {project_data, json_handler} ->
            process_project_json(Resource);
        {project_data, xml_handler} ->
            process_project_xml(Resource);
        {project_data, binary_handler} ->
            process_project_binary(Resource);
        {system_data, json_handler} ->
            process_system_json(Resource);
        {system_data, xml_handler} ->
            process_system_xml(Resource);
        {system_data, binary_handler} ->
            process_system_binary(Resource)
    end.

%% Simulated MCP resource processing functions
process_user_json(Resource) -> {ok, user_json, Resource}.
process_user_xml(Resource) -> {ok, user_xml, Resource}.
process_user_binary(Resource) -> {ok, user_binary, Resource}.
process_project_json(Resource) -> {ok, project_json, Resource}.
process_project_xml(Resource) -> {ok, project_xml, Resource}.
process_project_binary(Resource) -> {ok, project_binary, Resource}.
process_system_json(Resource) -> {ok, system_json, Resource}.
process_system_xml(Resource) -> {ok, system_xml, Resource}.
process_system_binary(Resource) -> {ok, system_binary, Resource}.

%% Type-safe resource registration
register_resource(ResourceId, Config) when is_binary(ResourceId) ->
    validate_resource_config(ResourceId, Config),
    erlmcp_registry:register_resource(ResourceId, Config).

%% Type-safe tool call
call_tool(ToolId, Args) when is_binary(ToolId) ->
    validate_tool_args(ToolId, Args),
    erlmcp_tool_handler:execute(ToolId, Args).

%% Error handling functions
handle_invalid_resources(RawResources) ->
    %% Log and handle invalid resources
    InvalidResources = [R || R <- RawResources, R#mcp_resource.uri =:= undefined],
    io:format("Found ~p invalid resources~n", [length(InvalidResources)]),
    ok.

handle_compressed_batch(CompressedBatch, Response) ->
    %% Handle compressed MCP batch responses
    case Response of
        #{compressed := true} ->
            DecodedBatch = zstd:decompress(CompressedBatch),
            BatchRequests = jsx:decode(DecodedBatch, [{labels, atom}]),
            process_batch_requests(BatchRequests);
        _ ->
            process_regular_response(Response)
    end.

%% Utility functions
generate_large_resource_data() ->
    %% Generate large test data
    lists:duplicate(10000, #{
        id => <<"test_id">>,
        data => <<"large_test_data">>,
        timestamp => erlang:system_time(millisecond)
    }).

monitor_all_processes() ->
    %% OTP 28 process iterator for efficient monitoring
    Iterator = erlang:processes_iterator(),
    monitor_processes(Iterator, []).

monitor_processes(Iterator, Acc) ->
    case erlang:process_next(Iterator) of
        '$end_of_table' -> Acc;
        Pid when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true ->
                    Info = get_process_info(Pid),
                    monitor_processes(Iterator, [Info | Acc]);
                false ->
                    monitor_processes(Iterator, Acc)
            end
    end.

find_active_connections() ->
    %% Find active MCP connections using process iterator
    Iterator = erlang:processes_iterator(),
    find_connections(Iterator, []).

find_connections(Iterator, Acc) ->
    case erlang:process_next(Iterator) of
        '$end_of_table' -> Acc;
        Pid when is_pid(Pid) ->
            case erlmcp_registry:is_connection_process(Pid) of
                true ->
                    find_connections(Iterator, [Pid | Acc]);
                false ->
                    find_connections(Iterator, Acc)
            end
    end.

cleanup_orphaned_resources() ->
    %% Clean up resources from terminated processes
    Iterator = erlang:processes_iterator(),
    cleanup_resources(Iterator, #{}).

cleanup_resources(Iterator, State) ->
    case erlang:process_next(Iterator) of
        '$end_of_table' -> State;
        Pid when is_pid(Pid) ->
            case erlmcp_registry:get_resources_for_pid(Pid) of
                {ok, Resources} when not is_process_alive(Pid) ->
                    %% Clean up orphaned resources
                    lists:foreach(fun erlmcp_registry:unregister_resource/1, Resources),
                    cleanup_resources(Iterator, State#{orphaned => maps:get(orphaned, State, 0) + 1});
                _ ->
                    cleanup_resources(Iterator, State)
            end
    end.

optimize_process_memory() ->
    %% Optimize memory for large process trees
    Iterator = erlang:processes_iterator(),
    optimize_memory(Iterator, #{}).

optimize_memory(Iterator, State) ->
    case erlang:process_next(Iterator) of
        '$end_of_table' -> State;
        Pid when is_pid(Pid) ->
            case should_hibernate(Pid) of
                true ->
                    Pid ! {hibernate, self()},
                    optimize_memory(Iterator, State#{hibernated => maps:get(hibernated, State, 0) + 1});
                false ->
                    optimize_memory(Iterator, State)
            end
    end.

should_hibernate(Pid) ->
    %% Check if process should hibernate based on activity
    case process_info(Pid, message_queue_len) of
        {message_queue_len, 0} ->
            true;  %% No messages, can hibernate
        _ ->
            false
    end.

%% Type validation functions
validate_resource_config(ResourceId, Config) ->
    %% Validate MCP resource configuration
    case maps:get(type, Config, undefined) of
        undefined -> throw({error, missing_resource_type});
        _ -> ok
    end.

validate_tool_args(ToolId, Args) ->
    %% Validate MCP tool arguments
    case maps:get(input, Args, undefined) of
        undefined -> throw({error, missing_tool_input});
        _ -> ok
    end.

%% Process info utilities
get_process_info(Pid) ->
    %% Get process information for monitoring
    case process_info(Pid, [
        current_function,
        message_queue_len,
        memory,
        links
    ]) of
        undefined -> {error, no_process_info};
        Info -> {pid, Pid, Info}
    end.

%% Batch processing
process_batch_requests(BatchRequests) ->
    %% Process MCP batch requests efficiently
    lists:map(fun process_batch_request/1, BatchRequests).

process_batch_request(Request) ->
    %% Handle individual batch request
    case Request of
        #{method := read_resource, params := Params} ->
            erlmcp_resource:read(Params);
        #{method := call_tool, params := Params} ->
            erlmcp_tool:call(Params);
        _ ->
            {error, unknown_method}
    end.

process_regular_response(Response) ->
    %% Process regular MCP response
    Response.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

priority_message_test() ->
    %% Test priority message functionality
    ok = priority_message_example(),
    ok.

strict_generator_test() ->
    %% Test strict generator functionality
    ok = strict_generator_example(),
    ok.

zip_generator_test() ->
    %% Test zip generator functionality
    Results = zip_generator_example(),
    ?assert(is_list(Results)),
    ok.

hibernate_test() ->
    %% Test hibernate functionality
    ok = hibernate_example(),
    ok.

-endif.