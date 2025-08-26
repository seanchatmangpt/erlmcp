-module(erlmcp).

-include("erlmcp.hrl").

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
    start_stdio_setup/2, start_tcp_setup/3, start_http_setup/3,
    setup_server_components/2, quick_stdio_server/3,
    validate_transport_config/2, get_transport_bindings/0,
    cleanup_transport_bindings/1
]).

%% Types
-type server_id() :: atom().
-type transport_id() :: atom().
-type transport_type() :: stdio | tcp | http.

-export_type([server_id/0, transport_id/0, transport_type/0]).

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
    logger:info("Starting transport ~p of type ~p", [TransportId, Type]),
    
    % Enhanced validation with type-specific checks
    case validate_transport_config(Type, Config) of
        ok ->
            case start_transport_impl(TransportId, Type, Config) of
                {ok, TransportPid} ->
                    TransportConfig = Config#{type => Type, started_at => erlang:timestamp()},
                    case register_transport_with_registry(TransportId, TransportPid, TransportConfig) of
                        ok -> 
                            logger:info("Successfully started and registered transport ~p (~p)", [TransportId, Type]),
                            {ok, TransportPid};
                        {error, Reason} ->
                            logger:warning("Transport ~p started but registry registration failed: ~p", [TransportId, Reason]),
                            {ok, TransportPid}
                    end;
                {error, Reason} = Error ->
                    logger:error("Failed to start transport ~p (~p): ~p", [TransportId, Type, Reason]),
                    Error
            end;
        {error, ValidationError} ->
            logger:error("Transport config validation failed for ~p (~p): ~p", [TransportId, Type, ValidationError]),
            {error, {invalid_config, ValidationError}}
    end.

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
                    % Validate the updated configuration
                    case validate_transport_config(UpdatedConfig) of
                        ok ->
                            erlmcp_registry:register_transport(TransportId, TransportPid, UpdatedConfig);
                        {error, ValidationError} ->
                            {error, ValidationError}
                    end;
                {error, not_found} ->
                    {error, transport_not_found}
            end
    end.

%%====================================================================
%% Configuration Validation API
%%====================================================================

%% @doc Validate transport configuration based on type
-spec validate_transport_config(map()) -> ok | {error, term()}.
validate_transport_config(Config) ->
    case maps:get(type, Config, undefined) of
        undefined ->
            {error, {validation_error, missing_required_field, type}};
        Type ->
            validate_transport_config_by_type(Type, Config)
    end.

%% @doc Validate configuration for specific transport type
-spec validate_transport_config_by_type(atom(), map()) -> ok | {error, term()}.
validate_transport_config_by_type(stdio, Config) ->
    RequiredFields = [type],
    OptionalFields = [server_id, test_mode],
    case validate_fields(Config, RequiredFields, OptionalFields) of
        ok ->
            % Additional stdio-specific validation
            case maps:get(test_mode, Config, false) of
                Boolean when is_boolean(Boolean) ->
                    ok;
                _ ->
                    {error, {validation_error, invalid_field_type, test_mode, "must be boolean"}}
            end;
        Error ->
            Error
    end;

validate_transport_config_by_type(tcp, Config) ->
    RequiredFields = [type, host, port],
    OptionalFields = [keepalive, connect_timeout, max_reconnect_attempts, server_id],
    case validate_fields(Config, RequiredFields, OptionalFields) of
        ok ->
            % Additional tcp-specific validation
            case validate_tcp_fields(Config) of
                ok -> ok;
                Error -> Error
            end;
        Error ->
            Error
    end;

validate_transport_config_by_type(http, Config) ->
    RequiredFields = [type, url],
    OptionalFields = [method, headers, timeout, server_id],
    case validate_fields(Config, RequiredFields, OptionalFields) of
        ok ->
            % Additional http-specific validation
            case validate_http_fields(Config) of
                ok -> ok;
                Error -> Error
            end;
        Error ->
            Error
    end;

validate_transport_config_by_type(Type, _Config) ->
    {error, {validation_error, unknown_transport_type, Type}}.

%% @doc Helper function to validate required and optional fields
-spec validate_fields(map(), [atom()], [atom()]) -> ok | {error, term()}.
validate_fields(Config, RequiredFields, OptionalFields) ->
    case validate_required_fields(Config, RequiredFields) of
        ok ->
            validate_allowed_fields(Config, RequiredFields ++ OptionalFields);
        Error ->
            Error
    end.

%% @doc Validate that all required fields are present
-spec validate_required_fields(map(), [atom()]) -> ok | {error, term()}.
validate_required_fields(Config, RequiredFields) ->
    Missing = [Field || Field <- RequiredFields, not maps:is_key(Field, Config)],
    case Missing of
        [] ->
            ok;
        [Field] ->
            {error, {validation_error, missing_required_field, Field}};
        Fields ->
            {error, {validation_error, missing_required_fields, Fields}}
    end.

%% @doc Validate that only allowed fields are present
-spec validate_allowed_fields(map(), [atom()]) -> ok | {error, term()}.
validate_allowed_fields(Config, AllowedFields) ->
    ConfigFields = maps:keys(Config),
    Unknown = [Field || Field <- ConfigFields, not lists:member(Field, AllowedFields)],
    case Unknown of
        [] ->
            ok;
        [Field] ->
            {error, {validation_error, unknown_field, Field, 
                     io_lib:format("allowed fields: ~p", [AllowedFields])}};
        Fields ->
            {error, {validation_error, unknown_fields, Fields,
                     io_lib:format("allowed fields: ~p", [AllowedFields])}}
    end.

%% @doc Validate TCP-specific fields
-spec validate_tcp_fields(map()) -> ok | {error, term()}.
validate_tcp_fields(Config) ->
    % Validate host
    case validate_host_field(maps:get(host, Config)) of
        ok ->
            % Validate port
            case validate_port_field(maps:get(port, Config)) of
                ok ->
                    % Validate optional fields
                    validate_tcp_optional_fields(Config);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc Validate TCP optional fields
-spec validate_tcp_optional_fields(map()) -> ok | {error, term()}.
validate_tcp_optional_fields(Config) ->
    % Validate keepalive
    case maps:get(keepalive, Config, undefined) of
        undefined -> 
            % Continue to next validation
            validate_tcp_connect_timeout(Config);
        Boolean when is_boolean(Boolean) -> 
            % Continue to next validation
            validate_tcp_connect_timeout(Config);
        _ -> {error, {validation_error, invalid_field_type, keepalive, "must be boolean"}}
    end.

%% @doc Validate TCP connect_timeout field
-spec validate_tcp_connect_timeout(map()) -> ok | {error, term()}.
validate_tcp_connect_timeout(Config) ->
    case maps:get(connect_timeout, Config, undefined) of
        undefined -> 
            validate_tcp_max_reconnect(Config);
        Timeout when is_integer(Timeout), Timeout > 0 -> 
            validate_tcp_max_reconnect(Config);
        _ -> {error, {validation_error, invalid_field_type, connect_timeout, "must be positive integer"}}
    end.

%% @doc Validate TCP max_reconnect_attempts field
-spec validate_tcp_max_reconnect(map()) -> ok | {error, term()}.
validate_tcp_max_reconnect(Config) ->
    case maps:get(max_reconnect_attempts, Config, undefined) of
        undefined -> ok;
        Attempts when is_integer(Attempts), Attempts >= 0 -> ok;
        _ -> {error, {validation_error, invalid_field_type, max_reconnect_attempts, "must be non-negative integer"}}
    end.

%% @doc Validate HTTP-specific fields
-spec validate_http_fields(map()) -> ok | {error, term()}.
validate_http_fields(Config) ->
    % Validate URL
    case validate_url_field(maps:get(url, Config)) of
        ok ->
            validate_http_optional_fields(Config);
        Error ->
            Error
    end.

%% @doc Validate HTTP optional fields
-spec validate_http_optional_fields(map()) -> ok | {error, term()}.
validate_http_optional_fields(Config) ->
    % Validate method
    case maps:get(method, Config, get) of
        Method when Method =:= get; Method =:= post; Method =:= put; Method =:= delete; 
                   Method =:= patch; Method =:= head; Method =:= options -> 
            validate_http_headers_field(Config);
        _ -> {error, {validation_error, invalid_field_value, method, "must be valid HTTP method"}}
    end.

%% @doc Validate HTTP headers field
-spec validate_http_headers_field(map()) -> ok | {error, term()}.
validate_http_headers_field(Config) ->
    case maps:get(headers, Config, undefined) of
        undefined -> 
            validate_http_timeout_field(Config);
        Headers when is_map(Headers) -> 
            case validate_http_headers(Headers) of
                ok -> validate_http_timeout_field(Config);
                Error -> Error
            end;
        _ -> {error, {validation_error, invalid_field_type, headers, "must be a map"}}
    end.

%% @doc Validate HTTP timeout field
-spec validate_http_timeout_field(map()) -> ok | {error, term()}.
validate_http_timeout_field(Config) ->
    case maps:get(timeout, Config, undefined) of
        undefined -> ok;
        Timeout when is_integer(Timeout), Timeout > 0 -> ok;
        _ -> {error, {validation_error, invalid_field_type, timeout, "must be positive integer"}}
    end.

%% @doc Validate HTTP headers map
-spec validate_http_headers(map()) -> ok | {error, term()}.
validate_http_headers(Headers) ->
    try
        maps:fold(fun(Key, Value, _Acc) ->
            case {is_binary(Key) orelse is_list(Key), is_binary(Value) orelse is_list(Value)} of
                {true, true} -> ok;
                {false, _} -> throw({error, {validation_error, invalid_header_key, Key, "must be binary or string"}});
                {_, false} -> throw({error, {validation_error, invalid_header_value, Value, "must be binary or string"}})
            end
        end, ok, Headers),
        ok
    catch
        throw:Error -> Error
    end.

%% @doc Validate host field
-spec validate_host_field(term()) -> ok | {error, term()}.
validate_host_field(Host) when is_binary(Host) ->
    case byte_size(Host) > 0 of
        true -> ok;
        false -> {error, {validation_error, invalid_field_value, host, "cannot be empty"}}
    end;
validate_host_field(Host) when is_list(Host) ->
    case length(Host) > 0 of
        true -> ok;
        false -> {error, {validation_error, invalid_field_value, host, "cannot be empty"}}
    end;
validate_host_field(_) ->
    {error, {validation_error, invalid_field_type, host, "must be binary or string"}}.

%% @doc Validate port field
-spec validate_port_field(term()) -> ok | {error, term()}.
validate_port_field(Port) when is_integer(Port), Port > 0, Port =< 65535 ->
    ok;
validate_port_field(Port) when is_integer(Port) ->
    {error, {validation_error, invalid_field_value, port, "must be between 1 and 65535"}};
validate_port_field(_) ->
    {error, {validation_error, invalid_field_type, port, "must be integer"}}.

%% @doc Validate URL field
-spec validate_url_field(term()) -> ok | {error, term()}.
validate_url_field(Url) when is_binary(Url) ->
    case validate_url_format(Url) of
        true -> ok;
        false -> {error, {validation_error, invalid_field_value, url, "must be valid HTTP/HTTPS URL"}}
    end;
validate_url_field(Url) when is_list(Url) ->
    case validate_url_format(list_to_binary(Url)) of
        true -> ok;
        false -> {error, {validation_error, invalid_field_value, url, "must be valid HTTP/HTTPS URL"}}
    end;
validate_url_field(_) ->
    {error, {validation_error, invalid_field_type, url, "must be binary or string"}}.

%% @doc Simple URL format validation
-spec validate_url_format(binary()) -> boolean().
validate_url_format(Url) ->
    case binary:split(Url, <<"://">>) of
        [Scheme, _Rest] when Scheme =:= <<"http">>; Scheme =:= <<"https">> ->
            byte_size(Url) > 10;  % Basic length check
        _ ->
            false
    end.

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
    logger:info("Setting up TCP server ~p with config", [ServerId]),
    case validate_transport_config(tcp, TcpConfig) of
        ok ->
            case start_server(ServerId, ServerConfig) of
                {ok, ServerPid} ->
                    TransportId = create_transport_id(ServerId, <<"tcp">>),
                    TransportConfig = maps:merge(#{server_id => ServerId}, TcpConfig),
                    case start_transport(TransportId, tcp, TransportConfig) of
                        {ok, TransportPid} ->
                            case bind_transport_to_server(TransportId, ServerId) of
                                ok ->
                                    logger:info("TCP setup completed successfully for ~p", [ServerId]),
                                    {ok, #{server => ServerPid, transport => TransportPid}};
                                {error, BindError} ->
                                    logger:warning("Server and transport started but binding failed: ~p", [BindError]),
                                    {ok, #{server => ServerPid, transport => TransportPid}}
                            end;
                        {error, TransportError} ->
                            logger:error("TCP transport setup failed for ~p: ~p", [ServerId, TransportError]),
                            _ = stop_server(ServerId),
                            {error, TransportError}
                    end;
                {error, _} = ServerError ->
                    logger:error("Server setup failed for TCP ~p: ~p", [ServerId, ServerError]),
                    ServerError
            end;
        {error, ValidationError} ->
            logger:error("TCP config validation failed for ~p: ~p", [ServerId, ValidationError]),
            {error, {invalid_tcp_config, ValidationError}}
    end.

%% Create a complete MCP server setup with HTTP transport
-spec start_http_setup(server_id(), map(), map()) -> {ok, #{server => pid(), transport => pid()}} | {error, term()}.
start_http_setup(ServerId, ServerConfig, HttpConfig) ->
    logger:info("Setting up HTTP server ~p with config", [ServerId]),
    case validate_transport_config(http, HttpConfig) of
        ok ->
            case start_server(ServerId, ServerConfig) of
                {ok, ServerPid} ->
                    TransportId = create_transport_id(ServerId, <<"http">>),
                    TransportConfig = maps:merge(#{server_id => ServerId}, HttpConfig),
                    case start_transport(TransportId, http, TransportConfig) of
                        {ok, TransportPid} ->
                            case bind_transport_to_server(TransportId, ServerId) of
                                ok ->
                                    logger:info("HTTP setup completed successfully for ~p", [ServerId]),
                                    {ok, #{server => ServerPid, transport => TransportPid}};
                                {error, BindError} ->
                                    logger:warning("Server and transport started but binding failed: ~p", [BindError]),
                                    {ok, #{server => ServerPid, transport => TransportPid}}
                            end;
                        {error, TransportError} ->
                            logger:error("HTTP transport setup failed for ~p: ~p", [ServerId, TransportError]),
                            _ = stop_server(ServerId),
                            {error, TransportError}
                    end;
                {error, _} = ServerError ->
                    logger:error("Server setup failed for HTTP ~p: ~p", [ServerId, ServerError]),
                    ServerError
            end;
        {error, ValidationError} ->
            logger:error("HTTP config validation failed for ~p: ~p", [ServerId, ValidationError]),
            {error, {invalid_http_config, ValidationError}}
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
%% Enhanced API Functions - Phase 3
%%====================================================================

%% Transport implementation helper
-spec start_transport_impl(transport_id(), transport_type(), map()) -> {ok, pid()} | {error, term()}.
start_transport_impl(TransportId, stdio, Config) ->
    erlmcp_transport_stdio_new:start_link(TransportId, Config);
start_transport_impl(_TransportId, tcp, Config) ->
    % Placeholder for TCP implementation
    Port = maps:get(port, Config, 8080),
    Host = maps:get(host, Config, "localhost"),
    logger:info("TCP transport would start on ~s:~p", [Host, Port]),
    {error, {transport_not_implemented, tcp}};
start_transport_impl(_TransportId, http, Config) ->
    % Placeholder for HTTP implementation
    Port = maps:get(port, Config, 8000),
    Path = maps:get(path, Config, "/mcp"),
    logger:info("HTTP transport would start on port ~p path ~s", [Port, Path]),
    {error, {transport_not_implemented, http}};
start_transport_impl(_TransportId, Type, _Config) ->
    {error, {unknown_transport_type, Type}}.

%% Enhanced configuration validation
-spec validate_transport_config(transport_type(), map()) -> ok | {error, term()}.
validate_transport_config(stdio, Config) ->
    % STDIO transport validation
    case maps:get(server_id, Config, undefined) of
        undefined -> ok; % server_id is optional for stdio
        ServerId when is_atom(ServerId) -> ok;
        _ -> {error, {invalid_server_id, "must be an atom"}}
    end;
validate_transport_config(tcp, Config) ->
    % TCP transport validation
    try
        Port = maps:get(port, Config, 8080),
        Host = maps:get(host, Config, "localhost"),
        
        % Validate port
        if 
            not is_integer(Port) -> throw({invalid_port, "must be integer"});
            Port < 1024 -> throw({invalid_port, "port must be >= 1024"});
            Port > 65535 -> throw({invalid_port, "port must be <= 65535"});
            true -> ok
        end,
        
        % Validate host
        case Host of
            H when is_list(H) -> ok;
            H when is_binary(H) -> ok;
            _ -> throw({invalid_host, "must be string or binary"})
        end,
        
        % Validate optional SSL config
        case maps:get(ssl, Config, false) of
            false -> ok;
            true -> 
                case {maps:get(certfile, Config, undefined), maps:get(keyfile, Config, undefined)} of
                    {undefined, _} -> throw({missing_ssl_config, "certfile required for SSL"});
                    {_, undefined} -> throw({missing_ssl_config, "keyfile required for SSL"});
                    {CertFile, KeyFile} when is_list(CertFile), is_list(KeyFile) -> ok;
                    _ -> throw({invalid_ssl_config, "certfile and keyfile must be strings"})
                end;
            _ -> throw({invalid_ssl_config, "ssl must be boolean"})
        end,
        
        ok
    catch
        throw:Error -> {error, Error}
    end;
validate_transport_config(http, Config) ->
    % HTTP transport validation
    try
        Port = maps:get(port, Config, 8000),
        Path = maps:get(path, Config, "/mcp"),
        
        % Validate port (same as TCP)
        if 
            not is_integer(Port) -> throw({invalid_port, "must be integer"});
            Port < 1024 -> throw({invalid_port, "port must be >= 1024"});
            Port > 65535 -> throw({invalid_port, "port must be <= 65535"});
            true -> ok
        end,
        
        % Validate path
        case Path of
            P when is_list(P) ->
                case string:prefix(P, "/") of
                    nomatch -> throw({invalid_path, "must start with /"});
                    _ -> ok
                end;
            P when is_binary(P) ->
                case binary:match(P, <<"/">>) of
                    {0, 1} -> ok;
                    _ -> throw({invalid_path, "must start with /"})
                end;
            _ -> throw({invalid_path, "must be string or binary"})
        end,
        
        % Validate optional CORS config
        case maps:get(cors, Config, false) of
            false -> ok;
            true -> ok; % Basic CORS enabled
            Origins when is_list(Origins) -> 
                % Validate origins list
                lists:foreach(fun
                    (Origin) when is_list(Origin); is_binary(Origin) -> ok;
                    (_) -> throw({invalid_cors, "origins must be strings"})
                end, Origins);
            _ -> throw({invalid_cors, "cors must be boolean or list of origins"})
        end,
        
        ok
    catch
        throw:Error -> {error, Error}
    end;
validate_transport_config(Type, _Config) ->
    {error, {unknown_transport_type, Type}}.

%% Get all transport bindings
-spec get_transport_bindings() -> [{transport_id(), server_id()}] | {error, term()}.
get_transport_bindings() ->
    case whereis(erlmcp_registry) of
        undefined ->
            {error, registry_not_available};
        _ ->
            try
                Transports = erlmcp_registry:list_transports(),
                Bindings = lists:filtermap(fun({TransportId, {_Pid, Config}}) ->
                    case maps:get(server_id, Config, undefined) of
                        undefined -> false;
                        ServerId -> {true, {TransportId, ServerId}}
                    end
                end, Transports),
                Bindings
            catch
                _:Error -> {error, Error}
            end
    end.

%% Cleanup orphaned transport bindings
-spec cleanup_transport_bindings(server_id()) -> ok | {error, term()}.
cleanup_transport_bindings(ServerId) ->
    case get_transport_bindings() of
        {error, _} = Error -> Error;
        Bindings ->
            OrphanedTransports = [TId || {TId, SId} <- Bindings, SId =:= ServerId],
            lists:foreach(fun(TransportId) ->
                case unbind_transport(TransportId) of
                    ok -> 
                        logger:info("Cleaned up orphaned transport binding: ~p", [TransportId]);
                    {error, Reason} -> 
                        logger:warning("Failed to cleanup transport binding ~p: ~p", [TransportId, Reason])
                end
            end, OrphanedTransports),
            ok
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
