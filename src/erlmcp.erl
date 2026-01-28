-module(erlmcp).

-include("erlmcp.hrl").

%% Re-export types from erlmcp_transport_behavior
-export_type([server_id/0]).

%% Application management API
-export([
    start_server/1, start_server/2, stop_server/1, list_servers/0,
    start_transport/2, start_transport/3, stop_transport/1, list_transports/0,
    bind_transport_to_server/2, unbind_transport/1
]).

%% Server operations API
-export([
    add_resource/3, add_resource/4,
    add_tool/3, add_tool/4,
    add_prompt/3, add_prompt/4
]).

%% Configuration API
-export([
    get_server_config/1, update_server_config/2,
    get_transport_config/1, update_transport_config/2,
    validate_transport_config/1
]).

%% Legacy compatibility for stdio server
-export([
    start_stdio_server/0, start_stdio_server/1, stop_stdio_server/0
]).

%% Convenience functions
-export([
    start_stdio_setup/2, start_tcp_setup/3,
    setup_server_components/2, quick_stdio_server/3
]).

%% Connection Pool Management API (poolboy integration)
-export([
    setup_connection_pool/2, setup_connection_pool/3,
    stop_connection_pool/1,
    with_pool_worker/2, with_pool_worker/3,
    pool_transaction/2, pool_transaction/3,
    pool_status/1, pool_name/1, worker_module/1
]).


%%====================================================================
%% Application Management API - Updated for Phase 2
%%====================================================================

-spec start_server(server_id()) -> {ok, pid()} | {error, term()}.
start_server(ServerId) ->
    start_server(ServerId, #{}).

-spec start_server(server_id(), map()) -> {ok, pid()} | {error, term()}.
start_server(ServerId, Config) ->
    % Ensure default capabilities if not provided
    DefaultCaps = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    },
    
    % Merge with provided config
    FinalConfig = case maps:get(capabilities, Config, undefined) of
        undefined ->
            Config#{capabilities => DefaultCaps};
        _ ->
            Config
    end,
    
    % Start server using the refactored erlmcp_server
    case start_server_process(ServerId, FinalConfig) of
        {ok, ServerPid} ->
            % Register with registry if available
            case register_server_with_registry(ServerId, ServerPid, FinalConfig) of
                ok -> 
                    logger:info("Started and registered server ~p", [ServerId]),
                    {ok, ServerPid};
                {error, Reason} -> 
                    % Registration failed, but server started - log warning and continue
                    logger:warning("Server ~p started but registry registration failed: ~p", [ServerId, Reason]),
                    {ok, ServerPid}
            end;
        {error, _} = Error ->
            Error
    end.

-spec stop_server(server_id()) -> ok | {error, term()}.
stop_server(ServerId) ->
    % Try registry approach first
    case whereis(erlmcp_registry) of
        undefined ->
            % No registry, try to find server process by registered name or other means
            logger:warning("Registry not available for server ~p cleanup", [ServerId]),
            {error, registry_not_available};
        _ ->
            case erlmcp_registry:find_server(ServerId) of
                {ok, {ServerPid, _Config}} ->
                    erlmcp_registry:unregister_server(ServerId),
                    case is_process_alive(ServerPid) of
                        true -> 
                            erlmcp_server:stop(ServerPid),
                            logger:info("Stopped server ~p", [ServerId]),
                            ok;
                        false -> 
                            logger:info("Server ~p already stopped", [ServerId]),
                            ok
                    end;
                {error, not_found} ->
                    logger:warning("Server ~p not found in registry", [ServerId]),
                    ok
            end
    end.

-spec list_servers() -> [{server_id(), {pid(), map()}}].
list_servers() ->
    case whereis(erlmcp_registry) of
        undefined -> 
            logger:warning("Registry not available for listing servers"),
            [];
        _ -> 
            erlmcp_registry:list_servers()
    end.

-spec start_transport(transport_id(), transport_type()) -> {ok, pid()} | {error, term()}.
start_transport(TransportId, Type) ->
    start_transport(TransportId, Type, #{}).

-spec start_transport(transport_id(), transport_type(), map()) -> {ok, pid()} | {error, term()}.
start_transport(TransportId, Type, Config) ->
    % Add type to config and validate
    ConfigWithType = Config#{type => Type},
    case validate_transport_config(ConfigWithType) of
        ok ->
            start_transport_internal(TransportId, Type, ConfigWithType);
        {error, _} = ValidationError ->
            ValidationError
    end.

%% @doc Internal function to start transport after validation
-spec start_transport_internal(transport_id(), transport_type(), map()) -> {ok, pid()} | {error, term()}.
start_transport_internal(TransportId, stdio, Config) ->
    case erlmcp_transport_stdio_new:start_link(TransportId, Config) of
        {ok, TransportPid} ->
            case register_transport_with_registry(TransportId, TransportPid, Config) of
                ok ->
                    logger:info("Started and registered transport ~p", [TransportId]),
                    {ok, TransportPid};
                {error, Reason} ->
                    logger:warning("Transport ~p started but registry registration failed: ~p", [TransportId, Reason]),
                    {ok, TransportPid}
            end;
        Error -> Error
    end;
start_transport_internal(_TransportId, tcp, _Config) ->
    {error, {transport_not_implemented, tcp}};
start_transport_internal(_TransportId, http, _Config) ->
    {error, {transport_not_implemented, http}};
start_transport_internal(_TransportId, Type, _Config) ->
    {error, {unknown_transport_type, Type}}.

-spec stop_transport(transport_id()) -> ok | {error, term()}.
stop_transport(TransportId) ->
    case whereis(erlmcp_registry) of
        undefined ->
            {error, registry_not_available};
        _ ->
            case erlmcp_registry:find_transport(TransportId) of
                {ok, {TransportPid, _Config}} ->
                    erlmcp_registry:unregister_transport(TransportId),
                    case is_process_alive(TransportPid) of
                        true -> 
                            erlmcp_transport_stdio_new:close(TransportPid),
                            logger:info("Stopped transport ~p", [TransportId]),
                            ok;
                        false -> ok
                    end;
                {error, not_found} ->
                    ok
            end
    end.

-spec list_transports() -> [{transport_id(), {pid(), map()}}].
list_transports() ->
    case whereis(erlmcp_registry) of
        undefined -> [];
        _ -> erlmcp_registry:list_transports()
    end.

-spec bind_transport_to_server(transport_id(), server_id()) -> ok | {error, term()}.
bind_transport_to_server(TransportId, ServerId) ->
    case whereis(erlmcp_registry) of
        undefined ->
            {error, registry_not_available};
        _ ->
            erlmcp_registry:bind_transport_to_server(TransportId, ServerId)
    end.

-spec unbind_transport(transport_id()) -> ok.
unbind_transport(TransportId) ->
    case whereis(erlmcp_registry) of
        undefined ->
            {error, registry_not_available};
        _ ->
            erlmcp_registry:unbind_transport(TransportId)
    end.

%%====================================================================
%% Server Operations API - Updated for Registry-based Architecture
%%====================================================================

-spec add_resource(server_id(), binary(), fun()) -> ok | {error, term()}.
add_resource(ServerId, Uri, Handler) ->
    add_resource(ServerId, Uri, Handler, #{}).

-spec add_resource(server_id(), binary(), fun(), map()) -> ok | {error, term()}.
add_resource(ServerId, Uri, Handler, Options) ->
    case find_server_process(ServerId) of
        {ok, ServerPid} ->
            case maps:get(template, Options, false) of
                true ->
                    Name = maps:get(name, Options, Uri),
                    erlmcp_server:add_resource_template(ServerPid, Uri, Name, Handler);
                false ->
                    erlmcp_server:add_resource(ServerPid, Uri, Handler)
            end;
        {error, not_found} ->
            {error, server_not_found}
    end.

-spec add_tool(server_id(), binary(), fun()) -> ok | {error, term()}.
add_tool(ServerId, Name, Handler) ->
    add_tool(ServerId, Name, Handler, #{}).

-spec add_tool(server_id(), binary(), fun(), map()) -> ok | {error, term()}.
add_tool(ServerId, Name, Handler, Options) ->
    case find_server_process(ServerId) of
        {ok, ServerPid} ->
            case maps:get(schema, Options, undefined) of
                undefined ->
                    erlmcp_server:add_tool(ServerPid, Name, Handler);
                Schema ->
                    erlmcp_server:add_tool_with_schema(ServerPid, Name, Handler, Schema)
            end;
        {error, not_found} ->
            {error, server_not_found}
    end.

-spec add_prompt(server_id(), binary(), fun()) -> ok | {error, term()}.
add_prompt(ServerId, Name, Handler) ->
    add_prompt(ServerId, Name, Handler, #{}).

-spec add_prompt(server_id(), binary(), fun(), map()) -> ok | {error, term()}.
add_prompt(ServerId, Name, Handler, Options) ->
    case find_server_process(ServerId) of
        {ok, ServerPid} ->
            case maps:get(arguments, Options, undefined) of
                undefined ->
                    erlmcp_server:add_prompt(ServerPid, Name, Handler);
                Arguments ->
                    erlmcp_server:add_prompt_with_args(ServerPid, Name, Handler, Arguments)
            end;
        {error, not_found} ->
            {error, server_not_found}
    end.

%%====================================================================
%% Configuration API
%%====================================================================

-spec get_server_config(server_id()) -> {ok, map()} | {error, term()}.
get_server_config(ServerId) ->
    case whereis(erlmcp_registry) of
        undefined ->
            {error, registry_not_available};
        _ ->
            case erlmcp_registry:find_server(ServerId) of
                {ok, {_ServerPid, Config}} ->
                    {ok, Config};
                {error, not_found} ->
                    {error, server_not_found}
            end
    end.

-spec update_server_config(server_id(), map()) -> ok | {error, term()}.
update_server_config(ServerId, NewConfig) ->
    case whereis(erlmcp_registry) of
        undefined ->
            {error, registry_not_available};
        _ ->
            case erlmcp_registry:find_server(ServerId) of
                {ok, {ServerPid, OldConfig}} ->
                    UpdatedConfig = maps:merge(OldConfig, NewConfig),
                    erlmcp_registry:register_server(ServerId, ServerPid, UpdatedConfig);
                {error, not_found} ->
                    {error, server_not_found}
            end
    end.

-spec get_transport_config(transport_id()) -> {ok, map()} | {error, term()}.
get_transport_config(TransportId) ->
    case whereis(erlmcp_registry) of
        undefined ->
            {error, registry_not_available};
        _ ->
            case erlmcp_registry:find_transport(TransportId) of
                {ok, {_TransportPid, Config}} ->
                    {ok, Config};
                {error, not_found} ->
                    {error, transport_not_found}
            end
    end.

-spec update_transport_config(transport_id(), map()) -> ok | {error, term()}.
update_transport_config(TransportId, NewConfig) ->
    case whereis(erlmcp_registry) of
        undefined ->
            {error, registry_not_available};
        _ ->
            case erlmcp_registry:find_transport(TransportId) of
                {ok, {TransportPid, OldConfig}} ->
                    UpdatedConfig = maps:merge(OldConfig, NewConfig),
                    erlmcp_registry:register_transport(TransportId, TransportPid, UpdatedConfig);
                {error, not_found} ->
                    {error, transport_not_found}
            end
    end.

%%====================================================================
%% Configuration Validation API
%%====================================================================

%% @doc Validate transport configuration based on transport type
%% Ensures all required fields are present and have valid values
-spec validate_transport_config(map()) -> ok | {error, {validation_error, term()}}.
validate_transport_config(#{type := stdio} = Config) ->
    RequiredFields = [type],
    OptionalFields = [server_id, buffer_size, read_timeout],
    case validate_fields(Config, RequiredFields, OptionalFields) of
        ok -> validate_stdio_specific(Config);
        {error, _} = Error -> Error
    end;
validate_transport_config(#{type := tcp} = Config) ->
    RequiredFields = [type, host, port],
    OptionalFields = [server_id, keepalive, connect_timeout, recv_timeout,
                     send_timeout, nodelay, buffer_size, max_connections],
    case validate_fields(Config, RequiredFields, OptionalFields) of
        ok -> validate_tcp_specific(Config);
        {error, _} = Error -> Error
    end;
validate_transport_config(#{type := http} = Config) ->
    RequiredFields = [type, url],
    OptionalFields = [server_id, method, headers, timeout, max_redirects,
                     verify_ssl, auth, retry_policy, compression],
    case validate_fields(Config, RequiredFields, OptionalFields) of
        ok -> validate_http_specific(Config);
        {error, _} = Error -> Error
    end;
validate_transport_config(#{} = Config) ->
    case maps:is_key(type, Config) of
        true ->
            {error, {validation_error, {unsupported_transport_type, maps:get(type, Config)}}};
        false ->
            {error, {validation_error, missing_required_field_type}}
    end;
validate_transport_config(_) ->
    {error, {validation_error, config_must_be_map}}.

%%====================================================================
%% Transport-Specific Validation Functions
%%====================================================================

%% @doc Validate stdio-specific configuration parameters
-spec validate_stdio_specific(map()) -> ok | {error, {validation_error, term()}}.
validate_stdio_specific(Config) ->
    Validations = [
        fun() -> validate_buffer_size(maps:get(buffer_size, Config, undefined)) end,
        fun() -> validate_timeout(maps:get(read_timeout, Config, undefined), read_timeout) end
    ],
    run_validations(Validations).

%% @doc Validate TCP-specific configuration parameters
-spec validate_tcp_specific(map()) -> ok | {error, {validation_error, term()}}.
validate_tcp_specific(Config) ->
    Validations = [
        fun() -> validate_host(maps:get(host, Config)) end,
        fun() -> validate_port(maps:get(port, Config)) end,
        fun() -> validate_boolean(maps:get(keepalive, Config, undefined), keepalive) end,
        fun() -> validate_timeout(maps:get(connect_timeout, Config, undefined), connect_timeout) end,
        fun() -> validate_timeout(maps:get(recv_timeout, Config, undefined), recv_timeout) end,
        fun() -> validate_timeout(maps:get(send_timeout, Config, undefined), send_timeout) end,
        fun() -> validate_boolean(maps:get(nodelay, Config, undefined), nodelay) end,
        fun() -> validate_buffer_size(maps:get(buffer_size, Config, undefined)) end,
        fun() -> validate_max_connections(maps:get(max_connections, Config, undefined)) end
    ],
    run_validations(Validations).

%% @doc Validate HTTP-specific configuration parameters
-spec validate_http_specific(map()) -> ok | {error, {validation_error, term()}}.
validate_http_specific(Config) ->
    Validations = [
        fun() -> validate_url(maps:get(url, Config)) end,
        fun() -> validate_http_method(maps:get(method, Config, undefined)) end,
        fun() -> validate_headers(maps:get(headers, Config, undefined)) end,
        fun() -> validate_timeout(maps:get(timeout, Config, undefined), timeout) end,
        fun() -> validate_max_redirects(maps:get(max_redirects, Config, undefined)) end,
        fun() -> validate_boolean(maps:get(verify_ssl, Config, undefined), verify_ssl) end,
        fun() -> validate_boolean(maps:get(compression, Config, undefined), compression) end
    ],
    run_validations(Validations).

%%====================================================================
%% Field Validation Helpers
%%====================================================================

%% @doc Validate that all required fields are present and no unknown fields exist
-spec validate_fields(map(), [atom()], [atom()]) -> ok | {error, {validation_error, term()}}.
validate_fields(Config, RequiredFields, OptionalFields) ->
    AllowedFields = RequiredFields ++ OptionalFields,
    ConfigKeys = maps:keys(Config),

    % Check for missing required fields
    case find_missing_fields(RequiredFields, ConfigKeys) of
        [] ->
            % Check for unknown fields
            case find_unknown_fields(ConfigKeys, AllowedFields) of
                [] -> ok;
                UnknownFields ->
                    {error, {validation_error, {unknown_fields, UnknownFields}}}
            end;
        MissingFields ->
            {error, {validation_error, {missing_required_fields, MissingFields}}}
    end.

%% @doc Find fields that are required but missing from config
-spec find_missing_fields([atom()], [atom()]) -> [atom()].
find_missing_fields(RequiredFields, ConfigKeys) ->
    [Field || Field <- RequiredFields, not lists:member(Field, ConfigKeys)].

%% @doc Find fields in config that are not in the allowed list
-spec find_unknown_fields([atom()], [atom()]) -> [atom()].
find_unknown_fields(ConfigKeys, AllowedFields) ->
    [Key || Key <- ConfigKeys, not lists:member(Key, AllowedFields)].

%% @doc Run a list of validation functions and return first error or ok
-spec run_validations([fun(() -> ok | {error, term()})]) -> ok | {error, term()}.
run_validations([]) -> ok;
run_validations([Validation | Rest]) ->
    case Validation() of
        ok -> run_validations(Rest);
        {error, _} = Error -> Error
    end.

%%====================================================================
%% Value Validation Helpers
%%====================================================================

%% @doc Validate host is a non-empty string or binary
-spec validate_host(term()) -> ok | {error, {validation_error, term()}}.
validate_host(Host) when is_binary(Host), byte_size(Host) > 0 -> ok;
validate_host(Host) when is_list(Host), length(Host) > 0 -> ok;
validate_host(undefined) -> {error, {validation_error, {invalid_host, undefined}}};
validate_host(Host) -> {error, {validation_error, {invalid_host, Host}}}.

%% @doc Validate port is an integer between 1 and 65535
-spec validate_port(term()) -> ok | {error, {validation_error, term()}}.
validate_port(Port) when is_integer(Port), Port >= 1, Port =< 65535 -> ok;
validate_port(Port) -> {error, {validation_error, {invalid_port, Port}}}.

%% @doc Validate URL format
-spec validate_url(term()) -> ok | {error, {validation_error, term()}}.
validate_url(Url) when is_binary(Url) ->
    case binary:match(Url, [<<"http://">>, <<"https://">>]) of
        {0, _} -> ok;
        _ -> {error, {validation_error, {invalid_url, Url}}}
    end;
validate_url(Url) when is_list(Url) ->
    case string:prefix(Url, "http://") =/= nomatch orelse
         string:prefix(Url, "https://") =/= nomatch of
        true -> ok;
        false -> {error, {validation_error, {invalid_url, Url}}}
    end;
validate_url(Url) -> {error, {validation_error, {invalid_url, Url}}}.

%% @doc Validate HTTP method
-spec validate_http_method(term()) -> ok | {error, {validation_error, term()}}.
validate_http_method(undefined) -> ok; % Optional field
validate_http_method(Method) when is_binary(Method) ->
    case lists:member(Method, [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>,
                               <<"PATCH">>, <<"HEAD">>, <<"OPTIONS">>]) of
        true -> ok;
        false -> {error, {validation_error, {invalid_http_method, Method}}}
    end;
validate_http_method(Method) when is_atom(Method) ->
    case lists:member(Method, [get, post, put, delete, patch, head, options]) of
        true -> ok;
        false -> {error, {validation_error, {invalid_http_method, Method}}}
    end;
validate_http_method(Method) -> {error, {validation_error, {invalid_http_method, Method}}}.

%% @doc Validate headers is a map or list of tuples
-spec validate_headers(term()) -> ok | {error, {validation_error, term()}}.
validate_headers(undefined) -> ok; % Optional field
validate_headers(Headers) when is_map(Headers) -> ok;
validate_headers(Headers) when is_list(Headers) ->
    case lists:all(fun
        ({K, V}) when (is_binary(K) orelse is_list(K)) andalso
                     (is_binary(V) orelse is_list(V)) -> true;
        (_) -> false
    end, Headers) of
        true -> ok;
        false -> {error, {validation_error, {invalid_headers, Headers}}}
    end;
validate_headers(Headers) -> {error, {validation_error, {invalid_headers, Headers}}}.

%% @doc Validate timeout is a positive integer or infinity
-spec validate_timeout(term(), atom()) -> ok | {error, {validation_error, term()}}.
validate_timeout(undefined, _Field) -> ok; % Optional field
validate_timeout(infinity, _Field) -> ok;
validate_timeout(Timeout, _Field) when is_integer(Timeout), Timeout > 0 -> ok;
validate_timeout(Timeout, Field) ->
    {error, {validation_error, {invalid_timeout, Field, Timeout}}}.

%% @doc Validate buffer size is a positive integer
-spec validate_buffer_size(term()) -> ok | {error, {validation_error, term()}}.
validate_buffer_size(undefined) -> ok; % Optional field
validate_buffer_size(Size) when is_integer(Size), Size > 0 -> ok;
validate_buffer_size(Size) -> {error, {validation_error, {invalid_buffer_size, Size}}}.

%% @doc Validate max connections is a positive integer
-spec validate_max_connections(term()) -> ok | {error, {validation_error, term()}}.
validate_max_connections(undefined) -> ok; % Optional field
validate_max_connections(Max) when is_integer(Max), Max > 0 -> ok;
validate_max_connections(Max) -> {error, {validation_error, {invalid_max_connections, Max}}}.

%% @doc Validate max redirects is a non-negative integer
-spec validate_max_redirects(term()) -> ok | {error, {validation_error, term()}}.
validate_max_redirects(undefined) -> ok; % Optional field
validate_max_redirects(Max) when is_integer(Max), Max >= 0 -> ok;
validate_max_redirects(Max) -> {error, {validation_error, {invalid_max_redirects, Max}}}.

%% @doc Validate boolean value
-spec validate_boolean(term(), atom()) -> ok | {error, {validation_error, term()}}.
validate_boolean(undefined, _Field) -> ok; % Optional field
validate_boolean(Value, _Field) when is_boolean(Value) -> ok;
validate_boolean(Value, Field) ->
    {error, {validation_error, {invalid_boolean, Field, Value}}}.

%%====================================================================
%% Poolboy Integration Helpers (Stubs for Future Implementation)
%%====================================================================

%% @doc Convert transport config to poolboy worker args (stub)
%% This will be implemented when poolboy integration is added
-spec transport_config_to_poolboy_args(map()) -> {ok, list()} | {error, term()}.
transport_config_to_poolboy_args(#{type := stdio} = Config) ->
    {ok, [
        {transport_type, stdio},
        {server_id, maps:get(server_id, Config, undefined)},
        {buffer_size, maps:get(buffer_size, Config, 4096)},
        {read_timeout, maps:get(read_timeout, Config, 5000)}
    ]};
transport_config_to_poolboy_args(#{type := tcp} = Config) ->
    {ok, [
        {transport_type, tcp},
        {server_id, maps:get(server_id, Config, undefined)},
        {host, maps:get(host, Config)},
        {port, maps:get(port, Config)},
        {keepalive, maps:get(keepalive, Config, true)},
        {connect_timeout, maps:get(connect_timeout, Config, 5000)},
        {recv_timeout, maps:get(recv_timeout, Config, 30000)},
        {send_timeout, maps:get(send_timeout, Config, 5000)},
        {nodelay, maps:get(nodelay, Config, true)},
        {buffer_size, maps:get(buffer_size, Config, 4096)}
    ]};
transport_config_to_poolboy_args(#{type := http} = Config) ->
    {ok, [
        {transport_type, http},
        {server_id, maps:get(server_id, Config, undefined)},
        {url, maps:get(url, Config)},
        {method, maps:get(method, Config, get)},
        {headers, maps:get(headers, Config, #{})},
        {timeout, maps:get(timeout, Config, 30000)},
        {max_redirects, maps:get(max_redirects, Config, 5)},
        {verify_ssl, maps:get(verify_ssl, Config, true)},
        {compression, maps:get(compression, Config, true)}
    ]};
transport_config_to_poolboy_args(_Config) ->
    {error, {unsupported_transport_type, unknown}}.

%% @doc Generate default poolboy pool configuration (stub)
%% This will be implemented when poolboy integration is added
-spec default_pool_config(transport_type()) -> list().
default_pool_config(stdio) ->
    [
        {name, {local, erlmcp_stdio_pool}},
        {worker_module, erlmcp_transport_stdio_worker},
        {size, 5},
        {max_overflow, 10}
    ];
default_pool_config(tcp) ->
    [
        {name, {local, erlmcp_tcp_pool}},
        {worker_module, erlmcp_transport_tcp_worker},
        {size, 10},
        {max_overflow, 20}
    ];
default_pool_config(http) ->
    [
        {name, {local, erlmcp_http_pool}},
        {worker_module, erlmcp_transport_http_worker},
        {size, 20},
        {max_overflow, 40}
    ].

%%====================================================================
%% Legacy Compatibility API - Enhanced for Phase 2
%%====================================================================

-spec start_stdio_server() -> {ok, pid()} | {error, term()}.
start_stdio_server() ->
    start_stdio_server(#{}).

-spec start_stdio_server(map()) -> {ok, pid()} | {error, term()}.
start_stdio_server(Options) ->
    % Use the new architecture by default, fall back to legacy if needed
    case is_new_architecture_available() of
        true ->
            % Use new registry-based approach
            case start_stdio_setup(default_stdio_server, Options) of
                {ok, #{server := ServerPid}} ->
                    {ok, ServerPid};
                {error, _} = Error ->
                    % Fall back to legacy approach
                    logger:warning("New stdio setup failed, falling back to legacy: ~p", [Error]),
                    start_legacy_stdio_server(Options)
            end;
        false ->
            % Registry not available, use legacy approach
            start_legacy_stdio_server(Options)
    end.

-spec stop_stdio_server() -> ok.
stop_stdio_server() ->
    % Try new architecture first
    case is_new_architecture_available() of
        true ->
            _ = stop_transport(default_stdio_transport),
            _ = stop_server(default_stdio_server),
            % Also stop legacy server if it exists
            stop_legacy_stdio_server(),
            ok;
        false ->
            % Fall back to legacy approach
            stop_legacy_stdio_server(),
            ok
    end.

%%====================================================================
%% Convenience Functions - Updated for Phase 2
%%====================================================================

%% Create a complete MCP server setup with stdio transport
-spec start_stdio_setup(server_id(), map()) -> {ok, #{server => pid(), transport => pid()}} | {error, term()}.
start_stdio_setup(ServerId, Config) ->
    case start_server(ServerId, Config) of
        {ok, ServerPid} ->
            TransportId = create_transport_id(ServerId, <<"stdio">>),
            TransportConfig = #{server_id => ServerId},
            case start_transport(TransportId, stdio, TransportConfig) of
                {ok, TransportPid} ->
                    % Ensure they're bound together
                    case bind_transport_to_server(TransportId, ServerId) of
                        ok ->
                            {ok, #{server => ServerPid, transport => TransportPid}};
                        {error, BindError} ->
                            logger:warning("Server and transport started but binding failed: ~p", [BindError]),
                            {ok, #{server => ServerPid, transport => TransportPid}}
                    end;
                {error, TransportError} ->
                    _ = stop_server(ServerId),
                    {error, TransportError}
            end;
        {error, _} = ServerError ->
            ServerError
    end.

%% Create a complete MCP server setup with TCP transport
-spec start_tcp_setup(server_id(), map(), map()) -> {ok, #{server => pid(), transport => pid()}} | {error, term()}.
start_tcp_setup(ServerId, ServerConfig, TcpConfig) ->
    case start_server(ServerId, ServerConfig) of
        {ok, ServerPid} ->
            TransportId = create_transport_id(ServerId, <<"tcp">>),
            TransportConfig = maps:merge(#{server_id => ServerId}, TcpConfig),
            case start_transport(TransportId, tcp, TransportConfig) of
                {ok, TransportPid} ->
                    case bind_transport_to_server(TransportId, ServerId) of
                        ok ->
                            {ok, #{server => ServerPid, transport => TransportPid}};
                        {error, BindError} ->
                            logger:warning("Server and transport started but binding failed: ~p", [BindError]),
                            {ok, #{server => ServerPid, transport => TransportPid}}
                    end;
                {error, TransportError} ->
                    _ = stop_server(ServerId),
                    {error, TransportError}
            end;
        {error, _} = ServerError ->
            ServerError
    end.

%% Batch add multiple resources, tools, and prompts
-spec setup_server_components(server_id(), map()) -> ok | {error, term()}.
setup_server_components(ServerId, Components) ->
    Resources = maps:get(resources, Components, []),
    Tools = maps:get(tools, Components, []),
    Prompts = maps:get(prompts, Components, []),
    
    try
        % Add resources
        lists:foreach(fun
            ({Uri, Handler}) ->
                ok = add_resource(ServerId, Uri, Handler);
            ({Uri, Handler, Options}) ->
                ok = add_resource(ServerId, Uri, Handler, Options)
        end, Resources),
        
        % Add tools
        lists:foreach(fun
            ({Name, Handler}) ->
                ok = add_tool(ServerId, Name, Handler);
            ({Name, Handler, Options}) ->
                ok = add_tool(ServerId, Name, Handler, Options)
        end, Tools),
        
        % Add prompts
        lists:foreach(fun
            ({Name, Handler}) ->
                ok = add_prompt(ServerId, Name, Handler);
            ({Name, Handler, Options}) ->
                ok = add_prompt(ServerId, Name, Handler, Options)
        end, Prompts),
        
        ok
    catch
        error:{badmatch, {error, Reason}} ->
            {error, Reason};
        Class:Exception ->
            {error, {Class, Exception}}
    end.

%% Quick way to create a complete stdio MCP server with components
-spec quick_stdio_server(server_id(), map(), map()) -> {ok, #{server => pid(), transport => pid()}} | {error, term()}.
quick_stdio_server(ServerId, ServerConfig, Components) ->
    case start_stdio_setup(ServerId, ServerConfig) of
        {ok, Result} ->
            case setup_server_components(ServerId, Components) of
                ok ->
                    {ok, Result};
                {error, SetupError} ->
                    TransportId = create_transport_id(ServerId, <<"stdio">>),
                    _ = stop_server(ServerId),
                    _ = stop_transport(TransportId),
                    {error, SetupError}
            end;
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Internal Helper Functions - Phase 2 Specific
%%====================================================================

-spec start_server_process(server_id(), map()) -> {ok, pid()} | {error, term()}.
start_server_process(ServerId, Config) ->
    % Try supervisor approach first, fall back to direct start
    case whereis(erlmcp_server_sup) of
        undefined ->
            % Supervisor not available, create server directly
            Capabilities = maps:get(capabilities, Config, #mcp_server_capabilities{}),
            erlmcp_server:start_link(ServerId, Capabilities);
        _Pid ->
            % Use supervisor
            erlmcp_server_sup:start_child(ServerId, Config)
    end.

-spec register_server_with_registry(server_id(), pid(), map()) -> ok | {error, term()}.
register_server_with_registry(ServerId, ServerPid, Config) ->
    case whereis(erlmcp_registry) of
        undefined -> 
            % Registry not available - this is OK for some deployments
            ok;
        _ -> 
            erlmcp_registry:register_server(ServerId, ServerPid, Config)
    end.

-spec register_transport_with_registry(transport_id(), pid(), map()) -> ok | {error, term()}.
register_transport_with_registry(TransportId, TransportPid, Config) ->
    case whereis(erlmcp_registry) of
        undefined -> 
            ok; % Registry not available - this is OK
        _ -> 
            erlmcp_registry:register_transport(TransportId, TransportPid, Config)
    end.

-spec find_server_process(server_id()) -> {ok, pid()} | {error, not_found}.
find_server_process(ServerId) ->
    case whereis(erlmcp_registry) of
        undefined ->
            % Registry not available, try other approaches
            {error, registry_not_available};
        _ ->
            case erlmcp_registry:find_server(ServerId) of
                {ok, {ServerPid, _Config}} ->
                    {ok, ServerPid};
                {error, not_found} ->
                    {error, not_found}
            end
    end.

-spec is_new_architecture_available() -> boolean().
is_new_architecture_available() ->
    whereis(erlmcp_registry) =/= undefined andalso
    whereis(erlmcp_server_sup) =/= undefined andalso
    whereis(erlmcp_transport_sup) =/= undefined.

-spec create_transport_id(server_id(), binary()) -> transport_id().
create_transport_id(ServerId, Type) ->
    binary_to_atom(<<(atom_to_binary(ServerId))/binary, "_", Type/binary>>, utf8).

%%====================================================================
%% Legacy Support Functions
%%====================================================================

-spec start_legacy_stdio_server(map()) -> {ok, pid()} | {error, term()}.
start_legacy_stdio_server(Options) ->
    case erlmcp_stdio_server:start_link(Options) of
        {ok, ServerPid} ->
            % Register as default stdio server if registry is available
            case whereis(erlmcp_registry) of
                undefined -> 
                    {ok, ServerPid};
                _ ->
                    ServerConfig = #{
                        capabilities => #mcp_server_capabilities{
                            resources = #mcp_capability{enabled = true},
                            tools = #mcp_capability{enabled = true},
                            prompts = #mcp_capability{enabled = true}
                        },
                        options => Options,
                        legacy => true
                    },
                    case erlmcp_registry:register_server(default_stdio_server, ServerPid, ServerConfig) of
                        ok -> {ok, ServerPid};
                        {error, already_registered} -> {ok, ServerPid};
                        {error, Reason} -> 
                            logger:warning("Legacy server started but registration failed: ~p", [Reason]),
                            {ok, ServerPid}
                    end
            end;
        Error -> Error
    end.

-spec stop_legacy_stdio_server() -> ok.
stop_legacy_stdio_server() ->
    case whereis(erlmcp_stdio_server) of
        undefined -> ok;
        _Pid ->
            erlmcp_stdio_server:stop(),
            ok
    end.

%%====================================================================
%% Connection Pool Management API - Poolboy Integration
%%====================================================================

%% @doc Setup a connection pool for a specific transport type with custom configuration
%% Creates a poolboy worker pool that can be used to manage multiple connections
%% to MCP servers or clients.
%%
%% Example:
%% ```
%% PoolConfig = #{
%%     pool_size => 10,
%%     max_overflow => 5
%% },
%% TransportConfig = #{
%%     host => <<"localhost">>,
%%     port => 8080
%% },
%% {ok, PoolPid} = erlmcp:setup_connection_pool(tcp, PoolConfig, TransportConfig).
%% '''
-spec setup_connection_pool(transport_type(), map(), map()) -> {ok, pid()} | {error, term()}.
setup_connection_pool(Type, PoolConfig, TransportConfig) ->
    PoolName = pool_name(Type),
    WorkerModule = worker_module(Type),

    % Validate transport config first
    ConfigWithType = TransportConfig#{type => Type},
    case validate_transport_config(ConfigWithType) of
        ok ->
            % Build poolboy configuration
            PoolArgs = [
                {name, {local, PoolName}},
                {worker_module, WorkerModule},
                {size, maps:get(pool_size, PoolConfig, 10)},
                {max_overflow, maps:get(max_overflow, PoolConfig, 5)}
            ],

            % Convert transport config to worker args
            case transport_config_to_poolboy_args(ConfigWithType) of
                {ok, WorkerArgs} ->
                    % Start the poolboy pool
                    case poolboy:start_link(PoolArgs, WorkerArgs) of
                        {ok, PoolPid} ->
                            logger:info("Started connection pool ~p for transport ~p", [PoolName, Type]),
                            {ok, PoolPid};
                        {error, {already_started, PoolPid}} ->
                            logger:warning("Connection pool ~p already started", [PoolName]),
                            {ok, PoolPid};
                        {error, Reason} ->
                            logger:error("Failed to start connection pool ~p: ~p", [PoolName, Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, {invalid_transport_config, Reason}}
            end;
        {error, _} = ValidationError ->
            ValidationError
    end.

%% @doc Setup a connection pool with default transport configuration
%% Uses default transport settings appropriate for the transport type.
-spec setup_connection_pool(transport_type(), map()) -> {ok, pid()} | {error, term()}.
setup_connection_pool(Type, PoolConfig) ->
    DefaultTransportConfig = default_transport_config(Type),
    setup_connection_pool(Type, PoolConfig, DefaultTransportConfig).

%% @doc Stop a connection pool for a specific transport type
%% Gracefully shuts down all workers and the pool supervisor.
-spec stop_connection_pool(transport_type()) -> ok.
stop_connection_pool(Type) ->
    PoolName = pool_name(Type),
    case whereis(PoolName) of
        undefined ->
            logger:warning("Connection pool ~p not found", [PoolName]),
            ok;
        PoolPid ->
            try
                poolboy:stop(PoolPid),
                logger:info("Stopped connection pool ~p", [PoolName]),
                ok
            catch
                Class:Reason ->
                    logger:error("Error stopping pool ~p: ~p:~p", [PoolName, Class, Reason]),
                    ok
            end
    end.

%% @doc Execute a function with a checked-out pool worker with timeout
%% The worker is automatically checked back in when the function completes or errors.
%%
%% Example:
%% ```
%% Result = erlmcp:with_pool_worker(tcp, fun(Worker) ->
%%     gen_server:call(Worker, {send_message, Message})
%% end, 5000).
%% '''
-spec with_pool_worker(transport_type(), fun((pid()) -> term()), timeout()) -> term() | {error, term()}.
with_pool_worker(Type, Fun, Timeout) when is_function(Fun, 1) ->
    PoolName = pool_name(Type),
    case whereis(PoolName) of
        undefined ->
            {error, {pool_not_found, PoolName}};
        _Pid ->
            Worker = poolboy:checkout(PoolName, true, Timeout),
            try
                Fun(Worker)
            after
                poolboy:checkin(PoolName, Worker)
            end
    end.

%% @doc Execute a function with a checked-out pool worker (default timeout: 5 seconds)
-spec with_pool_worker(transport_type(), fun((pid()) -> term())) -> term() | {error, term()}.
with_pool_worker(Type, Fun) ->
    with_pool_worker(Type, Fun, 5000).

%% @doc Execute a poolboy transaction with automatic checkout/checkin and timeout
%% This is a safer alternative to with_pool_worker/2-3 as poolboy handles errors internally.
%%
%% Example:
%% ```
%% Result = erlmcp:pool_transaction(tcp, fun(Worker) ->
%%     gen_server:call(Worker, {read_resource, Uri})
%% end, 10000).
%% '''
-spec pool_transaction(transport_type(), fun((pid()) -> term()), timeout()) -> term().
pool_transaction(Type, Fun, Timeout) when is_function(Fun, 1) ->
    PoolName = pool_name(Type),
    poolboy:transaction(PoolName, Fun, Timeout).

%% @doc Execute a poolboy transaction (default timeout: 5 seconds)
-spec pool_transaction(transport_type(), fun((pid()) -> term())) -> term().
pool_transaction(Type, Fun) ->
    pool_transaction(Type, Fun, 5000).

%% @doc Get the status of a connection pool
%% Returns information about the pool including size, overflow, and available workers.
-spec pool_status(transport_type()) -> {ok, map()} | {error, term()}.
pool_status(Type) ->
    PoolName = pool_name(Type),
    case whereis(PoolName) of
        undefined ->
            {error, {pool_not_found, PoolName}};
        _Pid ->
            try
                Status = poolboy:status(PoolName),
                {ok, #{
                    pool_name => PoolName,
                    transport_type => Type,
                    status => Status
                }}
            catch
                Class:Reason ->
                    {error, {status_error, Class, Reason}}
            end
    end.

%% @doc Get the pool name for a specific transport type
%% Pool names follow the convention: erlmcp_{type}_pool
-spec pool_name(transport_type()) -> atom().
pool_name(stdio) -> erlmcp_stdio_pool;
pool_name(tcp) -> erlmcp_tcp_pool;
pool_name(http) -> erlmcp_http_pool;
pool_name(Type) -> binary_to_atom(<<"erlmcp_", (atom_to_binary(Type))/binary, "_pool">>, utf8).

%% @doc Get the worker module for a specific transport type
%% Worker modules implement the poolboy_worker behavior and handle connections.
-spec worker_module(transport_type()) -> module().
worker_module(stdio) -> erlmcp_transport_stdio_worker;
worker_module(tcp) -> erlmcp_transport_tcp_worker;
worker_module(http) -> erlmcp_transport_http_worker;
worker_module(_Type) -> erlmcp_transport_worker.

%%====================================================================
%% Pool Configuration Helpers
%%====================================================================

%% @doc Get default transport configuration for a specific type
%% These are sensible defaults that can be overridden by the caller.
-spec default_transport_config(transport_type()) -> map().
default_transport_config(stdio) ->
    #{
        type => stdio,
        buffer_size => 4096,
        read_timeout => 5000
    };
default_transport_config(tcp) ->
    #{
        type => tcp,
        host => <<"localhost">>,
        port => 8080,
        keepalive => true,
        connect_timeout => 5000,
        recv_timeout => 30000,
        send_timeout => 5000,
        nodelay => true,
        buffer_size => 4096
    };
default_transport_config(http) ->
    #{
        type => http,
        url => <<"http://localhost:8080">>,
        method => get,
        headers => #{},
        timeout => 30000,
        max_redirects => 5,
        verify_ssl => true,
        compression => true
    }.
