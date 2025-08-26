-module(erlmcp).

-include("erlmcp.hrl").

%% Application management API
-export([start_server/1, start_server/2, stop_server/1, list_servers/0, start_transport/2,
         start_transport/3, stop_transport/1, list_transports/0, bind_transport_to_server/2,
         unbind_transport/1]).
%% Server operations API
-export([add_resource/3, add_resource/4, add_tool/3, add_tool/4, add_prompt/3,
         add_prompt/4]).
%% Configuration API
-export([get_server_config/1, update_server_config/2, get_transport_config/1,
         update_transport_config/2, validate_transport_config/1, get_config_schema/1,
         validate_config_field/3]).
%% Legacy compatibility for stdio server
-export([start_stdio_server/0, start_stdio_server/1, stop_stdio_server/0]).
%% Convenience functions
-export([start_stdio_setup/2, start_tcp_setup/3, start_http_setup/3,
         setup_server_components/2, quick_stdio_server/3, validate_transport_config/2,
         get_transport_bindings/0, cleanup_transport_bindings/1, list_supported_transport_types/0,
         get_config_examples/0]).
%% Enhanced API functions - Phase 3 Step 7
-export([format_validation_error/3, format_transport_error/4, format_setup_error/4,
         get_transport_binding_info/1, list_transport_bindings/0, rebind_transport/2,
         validate_transport_binding/2, audit_transport_bindings/0, cleanup_failed_setup/3]).

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
    DefaultCaps =
        #mcp_server_capabilities{resources = #mcp_capability{enabled = true},
                                 tools = #mcp_capability{enabled = true},
                                 prompts = #mcp_capability{enabled = true}},

    % Merge with provided config
    FinalConfig =
        case maps:get(capabilities, Config, undefined) of
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
                    logger:warning("Server ~p started but registry registration failed: ~p",
                                   [ServerId, Reason]),
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

-spec start_transport(transport_id(), transport_type(), map()) ->
                         {ok, pid()} | {error, term()}.
start_transport(TransportId, Type, Config) ->
    logger:info("Starting transport ~p of type ~p with config validation",
                [TransportId, Type]),

    % Enhanced validation using dedicated validation module
    ConfigWithType = Config#{type => Type},
    case erlmcp_transport_validation:validate_transport_config(Type, ConfigWithType) of
        ok ->
            case start_transport_impl(TransportId, Type, ConfigWithType) of
                {ok, TransportPid} ->
                    TransportConfig =
                        ConfigWithType#{started_at => erlang:timestamp(),
                                        validation_passed => true},
                    case register_transport_with_registry(TransportId,
                                                          TransportPid,
                                                          TransportConfig)
                    of
                        ok ->
                            logger:info("Successfully started and registered transport ~p (~p) with "
                                        "validation",
                                        [TransportId, Type]),
                            {ok, TransportPid};
                        {error, Reason} ->
                            logger:warning("Transport ~p started but registry registration failed: ~p",
                                           [TransportId, Reason]),
                            {ok, TransportPid}
                    end;
                {error, Reason} ->
                    logger:error("Failed to start transport ~p (~p): ~p",
                                 [TransportId, Type, Reason]),
                    erlmcp_transport_behavior:format_transport_error(TransportId,
                                                                     start_failed,
                                                                     Reason)
            end;
        {error, ValidationError} ->
            logger:error("Transport config validation failed for ~p (~p): ~p",
                         [TransportId, Type, ValidationError]),
            format_validation_error(TransportId, Type, ValidationError)
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
                        false ->
                            ok
                    end;
                {error, not_found} ->
                    ok
            end
    end.

-spec list_transports() -> [{transport_id(), {pid(), map()}}].
list_transports() ->
    case whereis(erlmcp_registry) of
        undefined ->
            [];
        _ ->
            erlmcp_registry:list_transports()
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
                            erlmcp_registry:register_transport(TransportId,
                                                               TransportPid,
                                                               UpdatedConfig);
                        {error, ValidationError} ->
                            {error, ValidationError}
                    end;
                {error, not_found} ->
                    {error, transport_not_found}
            end
    end.

%%====================================================================
%% Configuration Validation API - Enhanced with Schema Support
%%====================================================================

%% @doc Configuration schema definitions for all transport types
-define(STDIO_CONFIG_SCHEMA,
        #{required_fields => [type],
          optional_fields => [server_id, test_mode, buffer_size],
          field_validators =>
              #{type =>
                    fun (stdio) ->
                            ok;
                        (_) ->
                            {error, "must be 'stdio'"}
                    end,
                server_id =>
                    fun (Id) when is_atom(Id) ->
                            ok;
                        (_) ->
                            {error, "must be an atom"}
                    end,
                test_mode =>
                    fun (Bool) when is_boolean(Bool) ->
                            ok;
                        (_) ->
                            {error, "must be boolean"}
                    end,
                buffer_size =>
                    fun (Size) when is_integer(Size), Size > 0 ->
                            ok;
                        (_) ->
                            {error, "must be positive integer"}
                    end},
          description => "STDIO transport configuration for direct process communication"}).
-define(TCP_CONFIG_SCHEMA,
        #{required_fields => [type, host, port],
          optional_fields =>
              [keepalive,
               connect_timeout,
               max_reconnect_attempts,
               server_id,
               ssl,
               certfile,
               keyfile],
          field_validators =>
              #{type =>
                    fun (tcp) ->
                            ok;
                        (_) ->
                            {error, "must be 'tcp'"}
                    end,
                host =>
                    fun (H) when is_binary(H); is_list(H) ->
                            case iolist_size([H]) > 0 of
                                true ->
                                    ok;
                                false ->
                                    {error, "cannot be empty"}
                            end;
                        (_) ->
                            {error, "must be binary or string"}
                    end,
                port =>
                    fun (P) when is_integer(P), P >= 1, P =< 65535 ->
                            ok;
                        (_) ->
                            {error, "must be integer between 1 and 65535"}
                    end,
                keepalive =>
                    fun (Bool) when is_boolean(Bool) ->
                            ok;
                        (_) ->
                            {error, "must be boolean"}
                    end,
                connect_timeout =>
                    fun (T) when is_integer(T), T > 0 ->
                            ok;
                        (_) ->
                            {error, "must be positive integer (milliseconds)"}
                    end,
                max_reconnect_attempts =>
                    fun (A) when is_integer(A), A >= 0 ->
                            ok;
                        (_) ->
                            {error, "must be non-negative integer"}
                    end,
                ssl =>
                    fun (Bool) when is_boolean(Bool) ->
                            ok;
                        (_) ->
                            {error, "must be boolean"}
                    end,
                certfile =>
                    fun (F) when is_list(F) ->
                            case filelib:is_file(F) of
                                true ->
                                    ok;
                                false ->
                                    {error, "SSL certificate file does not exist"}
                            end;
                        (_) ->
                            {error, "must be valid file path"}
                    end,
                keyfile =>
                    fun (F) when is_list(F) ->
                            case filelib:is_file(F) of
                                true ->
                                    ok;
                                false ->
                                    {error, "SSL key file does not exist"}
                            end;
                        (_) ->
                            {error, "must be valid file path"}
                    end},
          description => "TCP transport configuration for network communication"}).
-define(HTTP_CONFIG_SCHEMA,
        #{required_fields => [type, url],
          optional_fields => [method, headers, timeout, server_id, cors, max_body_size],
          field_validators =>
              #{type =>
                    fun (http) ->
                            ok;
                        (_) ->
                            {error, "must be 'http'"}
                    end,
                url =>
                    fun (U) when is_binary(U); is_list(U) ->
                            case validate_url_format(iolist_to_binary([U])) of
                                true ->
                                    ok;
                                false ->
                                    {error, "must be valid HTTP/HTTPS URL"}
                            end;
                        (_) ->
                            {error, "must be binary or string"}
                    end,
                method =>
                    fun (M)
                            when M =:= get;
                                 M =:= post;
                                 M =:= put;
                                 M =:= delete;
                                 M =:= patch;
                                 M =:= head;
                                 M =:= options ->
                            ok;
                        (_) ->
                            {error,
                             "must be valid HTTP method (get|post|put|delete|patch|head|options)"}
                    end,
                headers =>
                    fun (H) when is_map(H) ->
                            validate_http_headers(H);
                        (_) ->
                            {error, "must be a map of header name/value pairs"}
                    end,
                timeout =>
                    fun (T) when is_integer(T), T > 0 ->
                            ok;
                        (_) ->
                            {error, "must be positive integer (milliseconds)"}
                    end,
                cors =>
                    fun (false) ->
                            ok;
                        (true) ->
                            ok;
                        (Origins) when is_list(Origins) ->
                            try
                                lists:foreach(fun (Origin)
                                                      when is_binary(Origin); is_list(Origin) ->
                                                      ok;
                                                  (_) ->
                                                      throw({error, "CORS origins must be strings"})
                                              end,
                                              Origins),
                                ok
                            catch
                                Error ->
                                    Error
                            end;
                        (_) ->
                            {error, "must be boolean or list of origin URLs"}
                    end,
                max_body_size =>
                    fun (Size) when is_integer(Size), Size > 0 ->
                            ok;
                        (_) ->
                            {error, "must be positive integer (bytes)"}
                    end},
          description => "HTTP transport configuration for web-based communication"}).

%% @doc Validate transport configuration using schema-based approach
-spec validate_transport_config(map()) -> ok | {error, term()}.
validate_transport_config(Config) ->
    case maps:get(type, Config, undefined) of
        undefined ->
            format_validation_error(missing_required_field,
                                    type,
                                    "Configuration must specify transport type. Valid types: stdio, "
                                    "tcp, http");
        Type ->
            validate_transport_config_with_schema(Type, Config)
    end.

%% @doc Enhanced schema-based validation
-spec validate_transport_config_with_schema(atom(), map()) -> ok | {error, term()}.
validate_transport_config_with_schema(stdio, Config) ->
    validate_config_against_schema(Config, ?STDIO_CONFIG_SCHEMA);
validate_transport_config_with_schema(tcp, Config) ->
    validate_config_against_schema(Config, ?TCP_CONFIG_SCHEMA);
validate_transport_config_with_schema(http, Config) ->
    validate_config_against_schema(Config, ?HTTP_CONFIG_SCHEMA);
validate_transport_config_with_schema(Type, _Config) ->
    format_validation_error(unknown_transport_type,
                            Type,
                            "Unknown transport type. Supported types: stdio, tcp, http").

%% @doc Generic schema validation
-spec validate_config_against_schema(map(), map()) -> ok | {error, term()}.
validate_config_against_schema(Config, Schema) ->
    RequiredFields = maps:get(required_fields, Schema),
    OptionalFields = maps:get(optional_fields, Schema),
    FieldValidators = maps:get(field_validators, Schema),
    Description = maps:get(description, Schema),

    case validate_schema_fields(Config, RequiredFields, OptionalFields) of
        ok ->
            validate_schema_field_values(Config, FieldValidators);
        {error, {validation_error, ErrorType, Field}} ->
            case ErrorType of
                missing_required_field ->
                    format_validation_error(ErrorType,
                                            Field,
                                            io_lib:format("Required field missing for ~s. Required fields: ~p",
                                                          [Description, RequiredFields]));
                unknown_field ->
                    AllowedFields = RequiredFields ++ OptionalFields,
                    format_validation_error(ErrorType,
                                            Field,
                                            io_lib:format("Unknown field for ~s. Allowed fields: ~p",
                                                          [Description, AllowedFields]));
                _ ->
                    {error, {validation_error, ErrorType, Field}}
            end;
        Error ->
            Error
    end.

%% @doc Validate schema field presence and allowed fields
-spec validate_schema_fields(map(), [atom()], [atom()]) -> ok | {error, term()}.
validate_schema_fields(Config, RequiredFields, OptionalFields) ->
    case validate_required_fields(Config, RequiredFields) of
        ok ->
            AllowedFields = RequiredFields ++ OptionalFields,
            validate_allowed_fields(Config, AllowedFields);
        Error ->
            Error
    end.

%% @doc Validate field values using validators
-spec validate_schema_field_values(map(), map()) -> ok | {error, term()}.
validate_schema_field_values(Config, FieldValidators) ->
    try
        maps:fold(fun(Field, Value, _Acc) ->
                     case maps:get(Field, FieldValidators, undefined) of
                         undefined -> ok; % Field not in schema validators, skip
                         Validator when is_function(Validator, 1) ->
                             case Validator(Value) of
                                 ok -> ok;
                                 {error, Reason} ->
                                     throw({validation_error, invalid_field_value, Field, Reason})
                             end
                     end
                  end,
                  ok,
                  Config),
        ok
    catch
        {validation_error, ErrorType, Field, Reason} ->
            format_validation_error(ErrorType, Field, Reason)
    end.

%% @doc Format validation errors with helpful messages
-spec format_validation_error(atom(), term(), string()) -> {error, term()}.
format_validation_error(ErrorType, Field, Reason) ->
    FormattedReason =
        case io_lib:char_list(Reason) of
            true ->
                Reason;
            false ->
                io_lib:format("~p", [Reason])
        end,
    {error, {validation_error, ErrorType, Field, lists:flatten(FormattedReason)}}.

%% @doc Legacy validation function - now delegates to schema-based approach
-spec validate_transport_config_by_type(atom(), map()) -> ok | {error, term()}.
validate_transport_config_by_type(Type, Config) ->
    validate_transport_config_with_schema(Type, Config).

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
            {error,
             {validation_error,
              unknown_field,
              Field,
              io_lib:format("allowed fields: ~p", [AllowedFields])}};
        Fields ->
            {error,
             {validation_error,
              unknown_fields,
              Fields,
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
        _ ->
            {error, {validation_error, invalid_field_type, keepalive, "must be boolean"}}
    end.

%% @doc Validate TCP connect_timeout field
-spec validate_tcp_connect_timeout(map()) -> ok | {error, term()}.
validate_tcp_connect_timeout(Config) ->
    case maps:get(connect_timeout, Config, undefined) of
        undefined ->
            validate_tcp_max_reconnect(Config);
        Timeout when is_integer(Timeout), Timeout > 0 ->
            validate_tcp_max_reconnect(Config);
        _ ->
            {error,
             {validation_error, invalid_field_type, connect_timeout, "must be positive integer"}}
    end.

%% @doc Validate TCP max_reconnect_attempts field
-spec validate_tcp_max_reconnect(map()) -> ok | {error, term()}.
validate_tcp_max_reconnect(Config) ->
    case maps:get(max_reconnect_attempts, Config, undefined) of
        undefined ->
            ok;
        Attempts when is_integer(Attempts), Attempts >= 0 ->
            ok;
        _ ->
            {error,
             {validation_error,
              invalid_field_type,
              max_reconnect_attempts,
              "must be non-negative integer"}}
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
        Method
            when Method =:= get;
                 Method =:= post;
                 Method =:= put;
                 Method =:= delete;
                 Method =:= patch;
                 Method =:= head;
                 Method =:= options ->
            validate_http_headers_field(Config);
        _ ->
            {error, {validation_error, invalid_field_value, method, "must be valid HTTP method"}}
    end.

%% @doc Validate HTTP headers field
-spec validate_http_headers_field(map()) -> ok | {error, term()}.
validate_http_headers_field(Config) ->
    case maps:get(headers, Config, undefined) of
        undefined ->
            validate_http_timeout_field(Config);
        Headers when is_map(Headers) ->
            case validate_http_headers(Headers) of
                ok ->
                    validate_http_timeout_field(Config);
                Error ->
                    Error
            end;
        _ ->
            {error, {validation_error, invalid_field_type, headers, "must be a map"}}
    end.

%% @doc Validate HTTP timeout field
-spec validate_http_timeout_field(map()) -> ok | {error, term()}.
validate_http_timeout_field(Config) ->
    case maps:get(timeout, Config, undefined) of
        undefined ->
            ok;
        Timeout when is_integer(Timeout), Timeout > 0 ->
            ok;
        _ ->
            {error, {validation_error, invalid_field_type, timeout, "must be positive integer"}}
    end.

%% @doc Validate HTTP headers map
-spec validate_http_headers(map()) -> ok | {error, term()}.
validate_http_headers(Headers) ->
    try
        maps:fold(fun(Key, Value, _Acc) ->
                     case {is_binary(Key) orelse is_list(Key),
                           is_binary(Value) orelse is_list(Value)}
                     of
                         {true, true} -> ok;
                         {false, _} ->
                             throw({error,
                                    {validation_error,
                                     invalid_header_key,
                                     Key,
                                     "must be binary or string"}});
                         {_, false} ->
                             throw({error,
                                    {validation_error,
                                     invalid_header_value,
                                     Value,
                                     "must be binary or string"}})
                     end
                  end,
                  ok,
                  Headers),
        ok
    catch
        Error ->
            Error
    end.

%% @doc Validate host field
-spec validate_host_field(term()) -> ok | {error, term()}.
validate_host_field(Host) when is_binary(Host) ->
    case byte_size(Host) > 0 of
        true ->
            ok;
        false ->
            {error, {validation_error, invalid_field_value, host, "cannot be empty"}}
    end;
validate_host_field(Host) when is_list(Host) ->
    case length(Host) > 0 of
        true ->
            ok;
        false ->
            {error, {validation_error, invalid_field_value, host, "cannot be empty"}}
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
        true ->
            ok;
        false ->
            {error, {validation_error, invalid_field_value, url, "must be valid HTTP/HTTPS URL"}}
    end;
validate_url_field(Url) when is_list(Url) ->
    case validate_url_format(list_to_binary(Url)) of
        true ->
            ok;
        false ->
            {error, {validation_error, invalid_field_value, url, "must be valid HTTP/HTTPS URL"}}
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
-spec start_stdio_setup(server_id(), map()) ->
                           {ok, #{server => pid(), transport => pid()}} | {error, term()}.
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
                            logger:warning("Server and transport started but binding failed: ~p",
                                           [BindError]),
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
-spec start_tcp_setup(server_id(), map(), map()) ->
                         {ok, #{server => pid(), transport => pid()}} | {error, term()}.
start_tcp_setup(ServerId, ServerConfig, TcpConfig) ->
    logger:info("Setting up TCP server ~p with enhanced validation", [ServerId]),

    % Enhanced validation with detailed error reporting
    case erlmcp_transport_validation:validate_transport_config(tcp, TcpConfig) of
        ok ->
            case start_server(ServerId, ServerConfig) of
                {ok, ServerPid} ->
                    TransportId = create_transport_id(ServerId, <<"tcp">>),
                    % Enhanced config with validation metadata
                    TransportConfig =
                        maps:merge(#{server_id => ServerId,
                                     transport_type => tcp,
                                     setup_type => convenience_function},
                                   TcpConfig),

                    case start_transport(TransportId, tcp, TransportConfig) of
                        {ok, TransportPid} ->
                            case bind_transport_to_server(TransportId, ServerId) of
                                ok ->
                                    logger:info("TCP setup completed successfully for ~p (~p:~p)",
                                                [ServerId,
                                                 maps:get(host, TcpConfig, "localhost"),
                                                 maps:get(port, TcpConfig, 8080)]),
                                    {ok,
                                     #{server => ServerPid,
                                       transport => TransportPid,
                                       transport_id => TransportId,
                                       config => TransportConfig}};
                                {error, BindError} ->
                                    logger:warning("TCP server ~p started but binding failed: ~p",
                                                   [ServerId, BindError]),
                                    {ok,
                                     #{server => ServerPid,
                                       transport => TransportPid,
                                       transport_id => TransportId,
                                       binding_warning => BindError}}
                            end;
                        {error, TransportError} ->
                            logger:error("TCP transport setup failed for ~p: ~p",
                                         [ServerId, TransportError]),
                            cleanup_failed_setup(ServerId, tcp, TransportError)
                    end;
                {error, ServerError} ->
                    logger:error("Server setup failed for TCP ~p: ~p", [ServerId, ServerError]),
                    format_setup_error(ServerId, tcp, server_start_failed, ServerError)
            end;
        {error, ValidationError} ->
            logger:error("TCP config validation failed for ~p: ~p", [ServerId, ValidationError]),
            format_validation_error(ServerId, tcp, ValidationError)
    end.

%% Create a complete MCP server setup with HTTP transport
-spec start_http_setup(server_id(), map(), map()) ->
                          {ok, #{server => pid(), transport => pid()}} | {error, term()}.
start_http_setup(ServerId, ServerConfig, HttpConfig) ->
    logger:info("Setting up HTTP server ~p with enhanced validation", [ServerId]),

    % Enhanced validation with detailed error reporting
    case erlmcp_transport_validation:validate_transport_config(http, HttpConfig) of
        ok ->
            case start_server(ServerId, ServerConfig) of
                {ok, ServerPid} ->
                    TransportId = create_transport_id(ServerId, <<"http">>),
                    % Enhanced config with validation metadata
                    TransportConfig =
                        maps:merge(#{server_id => ServerId,
                                     transport_type => http,
                                     setup_type => convenience_function},
                                   HttpConfig),

                    case start_transport(TransportId, http, TransportConfig) of
                        {ok, TransportPid} ->
                            case bind_transport_to_server(TransportId, ServerId) of
                                ok ->
                                    logger:info("HTTP setup completed successfully for ~p (~s)",
                                                [ServerId,
                                                 maps:get(url,
                                                          HttpConfig,
                                                          "http://localhost:8000/mcp")]),
                                    {ok,
                                     #{server => ServerPid,
                                       transport => TransportPid,
                                       transport_id => TransportId,
                                       config => TransportConfig}};
                                {error, BindError} ->
                                    logger:warning("HTTP server ~p started but binding failed: ~p",
                                                   [ServerId, BindError]),
                                    {ok,
                                     #{server => ServerPid,
                                       transport => TransportPid,
                                       transport_id => TransportId,
                                       binding_warning => BindError}}
                            end;
                        {error, TransportError} ->
                            logger:error("HTTP transport setup failed for ~p: ~p",
                                         [ServerId, TransportError]),
                            cleanup_failed_setup(ServerId, http, TransportError)
                    end;
                {error, ServerError} ->
                    logger:error("Server setup failed for HTTP ~p: ~p", [ServerId, ServerError]),
                    format_setup_error(ServerId, http, server_start_failed, ServerError)
            end;
        {error, ValidationError} ->
            logger:error("HTTP config validation failed for ~p: ~p", [ServerId, ValidationError]),
            format_validation_error(ServerId, http, ValidationError)
    end.

%% Batch add multiple resources, tools, and prompts
-spec setup_server_components(server_id(), map()) -> ok | {error, term()}.
setup_server_components(ServerId, Components) ->
    Resources = maps:get(resources, Components, []),
    Tools = maps:get(tools, Components, []),
    Prompts = maps:get(prompts, Components, []),

    try
        % Add resources
        lists:foreach(fun ({Uri, Handler}) ->
                              ok = add_resource(ServerId, Uri, Handler);
                          ({Uri, Handler, Options}) ->
                              ok = add_resource(ServerId, Uri, Handler, Options)
                      end,
                      Resources),

        % Add tools
        lists:foreach(fun ({Name, Handler}) ->
                              ok = add_tool(ServerId, Name, Handler);
                          ({Name, Handler, Options}) ->
                              ok = add_tool(ServerId, Name, Handler, Options)
                      end,
                      Tools),

        % Add prompts
        lists:foreach(fun ({Name, Handler}) ->
                              ok = add_prompt(ServerId, Name, Handler);
                          ({Name, Handler, Options}) ->
                              ok = add_prompt(ServerId, Name, Handler, Options)
                      end,
                      Prompts),

        ok
    catch
        error:{badmatch, {error, Reason}} ->
            {error, Reason};
        Class:Exception ->
            {error, {Class, Exception}}
    end.

%% Quick way to create a complete stdio MCP server with components
-spec quick_stdio_server(server_id(), map(), map()) ->
                            {ok, #{server => pid(), transport => pid()}} | {error, term()}.
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
%% Configuration Schema and Documentation API
%%====================================================================

%% @doc Get configuration schema for a transport type
-spec get_config_schema(atom()) -> {ok, map()} | {error, term()}.
get_config_schema(stdio) ->
    {ok, ?STDIO_CONFIG_SCHEMA};
get_config_schema(tcp) ->
    {ok, ?TCP_CONFIG_SCHEMA};
get_config_schema(http) ->
    {ok, ?HTTP_CONFIG_SCHEMA};
get_config_schema(Type) ->
    {error, {unknown_transport_type, Type}}.

%% @doc Validate a single configuration field
-spec validate_config_field(atom(), atom(), term()) -> ok | {error, term()}.
validate_config_field(TransportType, Field, Value) ->
    case get_config_schema(TransportType) of
        {ok, Schema} ->
            FieldValidators = maps:get(field_validators, Schema),
            case maps:get(Field, FieldValidators, undefined) of
                undefined ->
                    {error, {unknown_field, Field, TransportType}};
                Validator when is_function(Validator, 1) ->
                    case Validator(Value) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            format_validation_error(invalid_field_value, Field, Reason)
                    end
            end;
        Error ->
            Error
    end.

%% @doc List all supported transport types
-spec list_supported_transport_types() -> [atom()].
list_supported_transport_types() ->
    [stdio, tcp, http].

%% @doc Get example configurations for all transport types
-spec get_config_examples() -> map().
get_config_examples() ->
    #{stdio =>
          #{type => stdio,
            server_id => my_server,
            test_mode => false,
            buffer_size => 8192},
      tcp =>
          #{type => tcp,
            host => "localhost",
            port => 8080,
            keepalive => true,
            connect_timeout => 5000,
            max_reconnect_attempts => 3,
            ssl => false},
      http =>
          #{type => http,
            url => "https://api.example.com/mcp",
            method => post,
            headers => #{<<"Content-Type">> => <<"application/json">>},
            timeout => 30000,
            cors => true,
            max_body_size => 1048576}}.

%%====================================================================
%% Enhanced API Functions - Phase 3
%%====================================================================

%% Transport implementation helper
-spec start_transport_impl(transport_id(), transport_type(), map()) ->
                              {ok, pid()} | {error, term()}.
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

%% Enhanced configuration validation (delegated to validation module)
-spec validate_transport_config(transport_type(), map()) -> ok | {error, term()}.
validate_transport_config(Type, Config) ->
    % Delegate to validation module for comprehensive validation
    erlmcp_transport_validation:validate_transport_config(Type, Config).

%% Get all transport bindings
-spec get_transport_bindings() -> [{transport_id(), server_id()}] | {error, term()}.
get_transport_bindings() ->
    case whereis(erlmcp_registry) of
        undefined ->
            {error, registry_not_available};
        _ ->
            try
                Transports = erlmcp_registry:list_transports(),
                Bindings =
                    lists:filtermap(fun({TransportId, {_Pid, Config}}) ->
                                       case maps:get(server_id, Config, undefined) of
                                           undefined -> false;
                                           ServerId -> {true, {TransportId, ServerId}}
                                       end
                                    end,
                                    Transports),
                Bindings
            catch
                _:Error ->
                    {error, Error}
            end
    end.

%% Cleanup orphaned transport bindings
-spec cleanup_transport_bindings(server_id()) -> ok | {error, term()}.
cleanup_transport_bindings(ServerId) ->
    case get_transport_bindings() of
        {error, _} = Error ->
            Error;
        Bindings ->
            OrphanedTransports = [TId || {TId, SId} <- Bindings, SId =:= ServerId],
            lists:foreach(fun(TransportId) ->
                             case unbind_transport(TransportId) of
                                 ok ->
                                     logger:info("Cleaned up orphaned transport binding: ~p",
                                                 [TransportId]);
                                 {error, Reason} ->
                                     logger:warning("Failed to cleanup transport binding ~p: ~p",
                                                    [TransportId, Reason])
                             end
                          end,
                          OrphanedTransports),
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

-spec register_transport_with_registry(transport_id(), pid(), map()) ->
                                          ok | {error, term()}.
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
    whereis(erlmcp_registry) =/= undefined
    andalso whereis(erlmcp_server_sup) =/= undefined
    andalso whereis(erlmcp_transport_sup) =/= undefined.

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
                    ServerConfig =
                        #{capabilities =>
                              #mcp_server_capabilities{resources = #mcp_capability{enabled = true},
                                                       tools = #mcp_capability{enabled = true},
                                                       prompts = #mcp_capability{enabled = true}},
                          options => Options,
                          legacy => true},
                    case erlmcp_registry:register_server(default_stdio_server,
                                                         ServerPid,
                                                         ServerConfig)
                    of
                        ok ->
                            {ok, ServerPid};
                        {error, already_registered} ->
                            {ok, ServerPid};
                        {error, Reason} ->
                            logger:warning("Legacy server started but registration failed: ~p",
                                           [Reason]),
                            {ok, ServerPid}
                    end
            end;
        Error ->
            Error
    end.

-spec stop_legacy_stdio_server() -> ok.
stop_legacy_stdio_server() ->
    case whereis(erlmcp_stdio_server) of
        undefined ->
            ok;
        _Pid ->
            erlmcp_stdio_server:stop(),
            ok
    end.

%%====================================================================
%% Enhanced Error Handling Functions - Phase 3 Step 7
%%====================================================================

%% @doc Format transport error with context
-spec format_transport_error(transport_id(), transport_type(), atom(), term()) ->
                                {error, term()}.
format_transport_error(TransportId, Type, ErrorType, Reason) ->
    {error,
     #{error_type => ErrorType,
       transport_id => TransportId,
       transport_type => Type,
       reason => Reason,
       suggestion => get_transport_suggestion(Type, ErrorType, Reason),
       timestamp => erlang:timestamp()}}.

%% @doc Format setup error with context
-spec format_setup_error(server_id(), transport_type(), atom(), term()) ->
                            {error, term()}.
format_setup_error(ServerId, Type, ErrorType, Reason) ->
    {error,
     #{error_type => ErrorType,
       server_id => ServerId,
       transport_type => Type,
       reason => Reason,
       suggestion => get_setup_suggestion(Type, ErrorType),
       timestamp => erlang:timestamp()}}.

%% @doc Format validation error details
-spec format_validation_details(term()) -> map().
format_validation_details({validation_error, ErrorType, Field, Message}) ->
    #{type => ErrorType,
      field => Field,
      message => Message,
      severity => error};
format_validation_details(Errors) when is_list(Errors) ->
    #{type => multiple_validation_errors,
      errors => [format_validation_details(E) || E <- Errors],
      severity => error};
format_validation_details(Error) ->
    #{type => unknown_validation_error,
      details => Error,
      severity => error}.

%% @doc Get validation suggestion based on error
-spec get_validation_suggestion(transport_type(), term()) -> string().
get_validation_suggestion(tcp, {validation_error, missing_field, host, _}) ->
    "TCP transport requires 'host' field. Example: #{host => \"localhost\"}";
get_validation_suggestion(tcp, {validation_error, missing_field, port, _}) ->
    "TCP transport requires 'port' field. Example: #{port => 8080}";
get_validation_suggestion(tcp, {validation_error, invalid_value, port, _}) ->
    "TCP port must be between 1 and 65535. Example: #{port => 8080}";
get_validation_suggestion(http, {validation_error, missing_field, url, _}) ->
    "HTTP transport requires 'url' field. Example: #{url => \"http://loca"
    "lhost:8000/mcp\"}";
get_validation_suggestion(http, {validation_error, invalid_value, url, _}) ->
    "HTTP URL must be valid HTTP/HTTPS URL. Example: #{url => \"https://a"
    "pi.example.com/mcp\"}";
get_validation_suggestion(stdio, {validation_error, invalid_type, test_mode, _}) ->
    "STDIO test_mode must be boolean. Example: #{test_mode => true}";
get_validation_suggestion(_Type, Error) ->
    io_lib:format("Check transport configuration documentation. Error: ~p", [Error]).

%% @doc Get transport suggestion based on error
-spec get_transport_suggestion(transport_type(), atom(), term()) -> string().
get_transport_suggestion(tcp, start_failed, {port_in_use, Port}) ->
    io_lib:format("Port ~p is already in use. Try a different port number", [Port]);
get_transport_suggestion(tcp, start_failed, connection_refused) ->
    "Connection refused. Check if the target host is reachable and "
    "accepting connections";
get_transport_suggestion(http, start_failed, {port_in_use, Port}) ->
    io_lib:format("HTTP port ~p is already in use. Try a different port", [Port]);
get_transport_suggestion(http, start_failed, invalid_url) ->
    "Invalid HTTP URL format. Use format: http://host:port/path "
    "or https://host:port/path";
get_transport_suggestion(stdio, start_failed, _Reason) ->
    "STDIO transport failed. Ensure standard input/output streams "
    "are available";
get_transport_suggestion(Type, ErrorType, Reason) ->
    io_lib:format("Transport ~p failed with ~p: ~p. Check transport logs for details",
                  [Type, ErrorType, Reason]).

%% @doc Get setup suggestion based on error
-spec get_setup_suggestion(transport_type(), atom()) -> string().
get_setup_suggestion(_Type, server_start_failed) ->
    "Server startup failed. Check server configuration and ensure "
    "required dependencies are available";
get_setup_suggestion(tcp, transport_start_failed) ->
    "TCP transport setup failed. Verify host/port availability and "
    "network configuration";
get_setup_suggestion(http, transport_start_failed) ->
    "HTTP transport setup failed. Check URL accessibility and server "
    "configuration";
get_setup_suggestion(Type, ErrorType) ->
    io_lib:format("Setup failed for ~p transport with error ~p. Check system logs "
                  "and configuration",
                  [Type, ErrorType]).

%% @doc Clean up after failed setup
-spec cleanup_failed_setup(server_id(), transport_type(), term()) -> {error, term()}.
cleanup_failed_setup(ServerId, Type, TransportError) ->
    logger:info("Cleaning up after failed ~p setup for server ~p", [Type, ServerId]),
    _ = stop_server(ServerId),
    format_setup_error(ServerId, Type, transport_start_failed, TransportError).

%%====================================================================
%% Enhanced Transport Binding Management - Phase 3 Step 7
%%====================================================================

%% @doc Get detailed transport binding information
-spec get_transport_binding_info(transport_id()) -> {ok, map()} | {error, term()}.
get_transport_binding_info(TransportId) ->
    case whereis(erlmcp_registry) of
        undefined ->
            {error, registry_not_available};
        _ ->
            case erlmcp_registry:find_transport(TransportId) of
                {ok, {TransportPid, Config}} ->
                    ServerId = maps:get(server_id, Config, undefined),
                    {ok,
                     #{transport_id => TransportId,
                       transport_pid => TransportPid,
                       server_id => ServerId,
                       transport_type => maps:get(type, Config, unknown),
                       bound => ServerId =/= undefined,
                       config => Config,
                       status => get_process_status(TransportPid)}};
                {error, not_found} ->
                    {error, transport_not_found}
            end
    end.

%% @doc List all transport bindings with detailed information
-spec list_transport_bindings() -> [{transport_id(), map()}] | {error, term()}.
list_transport_bindings() ->
    case get_transport_bindings() of
        {error, _} = Error ->
            Error;
        Bindings ->
            DetailedBindings =
                lists:map(fun({TransportId, ServerId}) ->
                             case get_transport_binding_info(TransportId) of
                                 {ok, Info} -> {TransportId, Info#{server_id => ServerId}};
                                 {error, _} ->
                                     {TransportId, #{server_id => ServerId, status => unknown}}
                             end
                          end,
                          Bindings),
            DetailedBindings
    end.

%% @doc Force rebind transport to different server
-spec rebind_transport(transport_id(), server_id()) -> ok | {error, term()}.
rebind_transport(TransportId, NewServerId) ->
    case unbind_transport(TransportId) of
        ok ->
            case bind_transport_to_server(TransportId, NewServerId) of
                ok ->
                    logger:info("Successfully rebound transport ~p to server ~p",
                                [TransportId, NewServerId]),
                    ok;
                {error, Reason} = Error ->
                    logger:error("Failed to rebind transport ~p to server ~p: ~p",
                                 [TransportId, NewServerId, Reason]),
                    Error
            end;
        {error, Reason} = Error ->
            logger:error("Failed to unbind transport ~p before rebinding: ~p",
                         [TransportId, Reason]),
            Error
    end.

%% @doc Validate transport binding compatibility
-spec validate_transport_binding(transport_id(), server_id()) -> ok | {error, term()}.
validate_transport_binding(TransportId, ServerId) ->
    case {get_transport_config(TransportId), get_server_config(ServerId)} of
        {{ok, TransportConfig}, {ok, ServerConfig}} ->
            TransportType = maps:get(type, TransportConfig, unknown),
            ServerCapabilities = maps:get(capabilities, ServerConfig, #{}),

            case is_compatible_binding(TransportType, ServerCapabilities) of
                true ->
                    ok;
                false ->
                    {error,
                     {incompatible_binding,
                      io_lib:format("Transport ~p not compatible with server ~p",
                                    [TransportType, ServerId])}}
            end;
        {{error, transport_not_found}, _} ->
            {error, transport_not_found};
        {_, {error, server_not_found}} ->
            {error, server_not_found};
        {Error, _} ->
            Error
    end.

%% @doc Check if binding is compatible
-spec is_compatible_binding(atom(), map()) -> boolean().
is_compatible_binding(stdio, _ServerCapabilities) ->
    true; % STDIO is compatible with all servers
is_compatible_binding(tcp, ServerCapabilities) ->
    % TCP requires network-capable server
    maps:get(network_enabled, ServerCapabilities, true);
is_compatible_binding(http, ServerCapabilities) ->
    % HTTP requires web-capable server
    maps:get(web_enabled, ServerCapabilities, true)
    andalso maps:get(http_enabled, ServerCapabilities, true);
is_compatible_binding(_Type, _Capabilities) ->
    false.

%% @doc Get process status
-spec get_process_status(pid()) -> atom().
get_process_status(Pid) ->
    case is_process_alive(Pid) of
        true ->
            case process_info(Pid, status) of
                {status, Status} ->
                    Status;
                undefined ->
                    dead
            end;
        false ->
            dead
    end.

%% @doc Audit transport bindings for consistency
-spec audit_transport_bindings() -> {ok, map()} | {error, term()}.
audit_transport_bindings() ->
    case list_transport_bindings() of
        {error, _} = Error ->
            Error;
        Bindings ->
            Issues =
                lists:foldl(fun({TransportId, Info}, Acc) ->
                               case audit_single_binding(TransportId, Info) of
                                   [] -> Acc;
                                   BindingIssues -> [{TransportId, BindingIssues} | Acc]
                               end
                            end,
                            [],
                            Bindings),

            {ok,
             #{total_bindings => length(Bindings),
               healthy_bindings => length(Bindings) - length(Issues),
               issues => Issues,
               audit_time => erlang:timestamp()}}
    end.

%% @doc Audit single binding
-spec audit_single_binding(transport_id(), map()) -> [atom()].
audit_single_binding(TransportId, Info) ->
    Issues = [],

    % Check if transport process is alive
    Issues1 =
        case maps:get(status, Info, unknown) of
            dead ->
                [dead_transport | Issues];
            _ ->
                Issues
        end,

    % Check if server binding is valid
    Issues2 =
        case maps:get(server_id, Info, undefined) of
            undefined ->
                [unbound_transport | Issues1];
            SrvId ->
                case get_server_config(SrvId) of
                    {ok, _} ->
                        Issues1;
                    {error, server_not_found} ->
                        [orphaned_binding | Issues1];
                    {error, _} ->
                        [server_unreachable | Issues1]
                end
        end,

    % Check transport type compatibility
    Issues3 =
        case maps:get(server_id, Info, undefined) of
            undefined ->
                Issues2;
            ServerIdForBinding ->
                case validate_transport_binding(TransportId, ServerIdForBinding) of
                    ok ->
                        Issues2;
                    {error, {incompatible_binding, _}} ->
                        [incompatible_binding | Issues2];
                    {error, _} ->
                        [binding_validation_failed | Issues2]
                end
        end,

    Issues3.
