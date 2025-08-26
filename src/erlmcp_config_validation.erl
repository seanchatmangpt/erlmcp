%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive Configuration Validation for Erlang MCP
%%%
%%% This module provides comprehensive configuration validation for all
%%% transport types and system components. It implements:
%%%
%%% - Transport-specific configuration schemas
%%% - Field-level validation with helpful error messages
%%% - Configuration documentation and examples
%%% - Integration with existing validation patterns
%%% - Extensible validation framework
%%%
%%% == Features ==
%%%
%%% * Schema-based validation with detailed error messages
%%% * Transport-specific validation (stdio, tcp, http, websocket)
%%% * Field validation with type checking and constraints
%%% * Configuration documentation with examples
%%% * Validation result formatting for debugging
%%% * Extensible framework for new transport types
%%%
%%% == Usage ==
%%%
%%% ```erlang
%%% % Validate complete transport configuration
%%% Config = #{transport_type => stdio, owner => self(), test_mode => true},
%%% case erlmcp_config_validation:validate_transport_config(my_transport, Config) of
%%%     ok -> start_transport(Config);
%%%     {error, Errors} -> handle_validation_errors(Errors)
%%% end.
%%%
%%% % Validate specific field
%%% case erlmcp_config_validation:validate_field(tcp, host, "localhost") of
%%%     ok -> proceed();
%%%     {error, Reason} -> handle_field_error(Reason)
%%% end.
%%%
%%% % Get schema documentation
%%% Schema = erlmcp_config_validation:get_transport_schema(http),
%%% Documentation = erlmcp_config_validation:get_schema_documentation(http).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_config_validation).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

%% API exports
-export([validate_transport_config/2, validate_transport_config/3,
         validate_field/3, validate_config_map/2,
         get_transport_schema/1, get_schema_documentation/1,
         format_validation_errors/1, get_supported_transports/0,
         create_example_config/1, validate_with_schema/2]).

%% Schema definitions for each transport type
-export([stdio_schema/0, tcp_schema/0, http_schema/0, websocket_schema/0]).

%% Type definitions
-type validation_result() :: ok | {error, validation_errors()}.
-type validation_errors() :: [validation_error()].
-type validation_error() :: 
    #{field => atom(),
      value => term(),
      error => atom(),
      message => binary(),
      constraint => term()}.
-type field_schema() ::
    #{type := atom(),
      required => boolean(),
      default => term(),
      constraints => [constraint()],
      description => binary(),
      examples => [term()]}.
-type constraint() ::
    {min, integer()} | {max, integer()} |
    {min_length, integer()} | {max_length, integer()} |
    {one_of, [term()]} | {format, atom()} |
    {custom, fun((term()) -> boolean())}.
-type transport_schema() :: #{atom() => field_schema()}.

%% Export types
-export_type([validation_result/0, validation_errors/0, validation_error/0,
              field_schema/0, constraint/0, transport_schema/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate complete transport configuration
-spec validate_transport_config(atom(), map()) -> validation_result().
validate_transport_config(TransportId, Config) when is_atom(TransportId), is_map(Config) ->
    case maps:get(transport_type, Config, undefined) of
        undefined ->
            {error, [#{field => transport_type,
                      value => undefined,
                      error => missing_required_field,
                      message => <<"Transport type must be specified">>,
                      constraint => required}]};
        TransportType ->
            validate_transport_config(TransportId, TransportType, Config)
    end.

%% @doc Validate transport configuration with explicit type
-spec validate_transport_config(atom(), atom(), map()) -> validation_result().
validate_transport_config(TransportId, TransportType, Config) 
    when is_atom(TransportId), is_atom(TransportType), is_map(Config) ->
    ?LOG_DEBUG("Validating ~p transport configuration for ~p", [TransportType, TransportId]),
    
    case get_transport_schema(TransportType) of
        {error, Reason} ->
            {error, [#{field => transport_type,
                      value => TransportType,
                      error => unsupported_transport,
                      message => iolist_to_binary(io_lib:format("Unsupported transport type: ~p", [TransportType])),
                      constraint => Reason}]};
        Schema ->
            ConfigWithDefaults = apply_schema_defaults(Config, Schema),
            validate_with_schema(ConfigWithDefaults, Schema)
    end.

%% @doc Validate individual field value against schema
-spec validate_field(atom(), atom(), term()) -> validation_result().
validate_field(TransportType, FieldName, Value) ->
    case get_transport_schema(TransportType) of
        {error, Reason} ->
            {error, [#{field => FieldName,
                      value => Value,
                      error => unsupported_transport,
                      message => iolist_to_binary(io_lib:format("Transport type ~p not supported", [TransportType])),
                      constraint => Reason}]};
        Schema ->
            case maps:get(FieldName, Schema, undefined) of
                undefined ->
                    {error, [#{field => FieldName,
                              value => Value,
                              error => unknown_field,
                              message => iolist_to_binary(io_lib:format("Field ~p not defined for transport ~p", [FieldName, TransportType])),
                              constraint => undefined}]};
                FieldSchema ->
                    validate_field_value(FieldName, Value, FieldSchema)
            end
    end.

%% @doc Validate configuration map against schema
-spec validate_config_map(map(), transport_schema()) -> validation_result().
validate_config_map(Config, Schema) ->
    validate_with_schema(Config, Schema).

%% @doc Get transport schema definition
-spec get_transport_schema(atom()) -> transport_schema() | {error, term()}.
get_transport_schema(stdio) -> stdio_schema();
get_transport_schema(tcp) -> tcp_schema();
get_transport_schema(http) -> http_schema();
get_transport_schema(websocket) -> websocket_schema();
get_transport_schema(Type) -> {error, {unsupported_transport_type, Type}}.

%% @doc Get schema documentation for transport type
-spec get_schema_documentation(atom()) -> map() | {error, term()}.
get_schema_documentation(TransportType) ->
    case get_transport_schema(TransportType) of
        {error, Reason} ->
            {error, Reason};
        Schema ->
            #{transport_type => TransportType,
              description => get_transport_description(TransportType),
              fields => maps:map(fun(_Field, FieldSchema) ->
                                    maps:with([description, examples, type, required, default], FieldSchema)
                                end, Schema),
              examples => [create_example_config(TransportType)]
            }
    end.

%% @doc Format validation errors for human-readable output
-spec format_validation_errors(validation_errors()) -> binary().
format_validation_errors(Errors) when is_list(Errors) ->
    FormattedErrors = lists:map(fun format_single_error/1, Errors),
    ErrorList = string:join([binary_to_list(E) || E <- FormattedErrors], "\n  - "),
    iolist_to_binary(["Configuration validation failed:\n  - ", ErrorList]).

%% @doc Get list of supported transport types
-spec get_supported_transports() -> [atom()].
get_supported_transports() ->
    [stdio, tcp, http, websocket].

%% @doc Create example configuration for transport type
-spec create_example_config(atom()) -> map().
create_example_config(stdio) ->
    #{transport_type => stdio,
      owner => self(),
      test_mode => false,
      server_id => my_server};
create_example_config(tcp) ->
    #{transport_type => tcp,
      host => "localhost",
      port => 8080,
      owner => self(),
      connect_timeout => 5000,
      keepalive => true,
      nodelay => true,
      buffer_size => 65536,
      max_reconnect_attempts => 5};
create_example_config(http) ->
    #{transport_type => http,
      url => "https://api.example.com/mcp",
      owner => self(),
      method => post,
      headers => [{"Content-Type", "application/json"}],
      timeout => 30000,
      connect_timeout => 10000,
      max_retries => 3,
      retry_delay => 1000};
create_example_config(websocket) ->
    #{transport_type => websocket,
      url => "wss://api.example.com/mcp/ws",
      owner => self(),
      protocols => [<<"mcp">>],
      headers => [],
      connect_timeout => 10000,
      ping_interval => 30000};
create_example_config(_) ->
    #{transport_type => custom,
      owner => self()}.

%%====================================================================
%% Schema Definitions
%%====================================================================

%% @doc STDIO transport schema definition
-spec stdio_schema() -> transport_schema().
stdio_schema() ->
    #{owner => #{type => pid,
                 required => true,
                 description => <<"Process ID that owns this transport">>,
                 examples => [self()]},
      server_id => #{type => atom,
                     required => false,
                     default => undefined,
                     description => <<"Optional server identifier for registry">>,
                     examples => [my_server, mcp_server, undefined]},
      test_mode => #{type => boolean,
                     required => false,
                     default => false,
                     description => <<"Enable test mode for unit testing">>,
                     examples => [true, false]},
      transport_id => #{type => atom,
                        required => false,
                        description => <<"Unique transport identifier">>,
                        examples => [stdio_transport, my_stdio]}}.

%% @doc TCP transport schema definition  
-spec tcp_schema() -> transport_schema().
tcp_schema() ->
    #{host => #{type => {one_of, [string, binary, ip_address]},
                required => true,
                description => <<"Hostname or IP address to connect to">>,
                examples => ["localhost", <<"127.0.0.1">>, {127,0,0,1}]},
      port => #{type => integer,
                required => true,
                constraints => [{min, 1}, {max, 65535}],
                description => <<"TCP port number (1-65535)">>,
                examples => [8080, 3000, 8443]},
      owner => #{type => pid,
                 required => true,
                 description => <<"Process ID that owns this transport">>,
                 examples => [self()]},
      connect_timeout => #{type => integer,
                           required => false,
                           default => 5000,
                           constraints => [{min, 1000}],
                           description => <<"Connection timeout in milliseconds">>,
                           examples => [5000, 10000, 30000]},
      keepalive => #{type => boolean,
                     required => false,
                     default => true,
                     description => <<"Enable TCP keepalive">>,
                     examples => [true, false]},
      nodelay => #{type => boolean,
                   required => false,
                   default => true,
                   description => <<"Disable Nagle algorithm for low latency">>,
                   examples => [true, false]},
      buffer_size => #{type => integer,
                       required => false,
                       default => 65536,
                       constraints => [{min, 1024}, {max, 1048576}],
                       description => <<"Socket buffer size in bytes">>,
                       examples => [32768, 65536, 131072]},
      max_reconnect_attempts => #{type => {one_of, [integer, infinity]},
                                  required => false,
                                  default => infinity,
                                  constraints => [{min, 0}],
                                  description => <<"Maximum reconnection attempts">>,
                                  examples => [3, 10, infinity]},
      server_id => #{type => atom,
                     required => false,
                     default => undefined,
                     description => <<"Optional server identifier">>,
                     examples => [my_server, tcp_server]},
      transport_id => #{type => atom,
                        required => false,
                        description => <<"Unique transport identifier">>,
                        examples => [tcp_transport, my_tcp]}}.

%% @doc HTTP transport schema definition
-spec http_schema() -> transport_schema().
http_schema() ->
    #{url => #{type => {one_of, [string, binary]},
               required => true,
               constraints => [{format, http_url}],
               description => <<"HTTP/HTTPS URL endpoint">>,
               examples => ["https://api.example.com/mcp", <<"http://localhost:8080/api">>]},
      owner => #{type => pid,
                 required => true,
                 description => <<"Process ID that owns this transport">>,
                 examples => [self()]},
      method => #{type => atom,
                  required => false,
                  default => post,
                  constraints => [{one_of, [get, post, put, patch]}],
                  description => <<"HTTP method for requests">>,
                  examples => [post, get, put]},
      headers => #{type => list,
                   required => false,
                   default => [],
                   description => <<"HTTP headers as {Key, Value} tuples">>,
                   examples => [[{"Authorization", "Bearer token123"}]]},
      timeout => #{type => integer,
                   required => false,
                   default => 30000,
                   constraints => [{min, 1000}],
                   description => <<"Request timeout in milliseconds">>,
                   examples => [30000, 60000, 120000]},
      connect_timeout => #{type => integer,
                           required => false,
                           default => 10000,
                           constraints => [{min, 1000}],
                           description => <<"Connection timeout in milliseconds">>,
                           examples => [5000, 10000, 15000]},
      max_retries => #{type => integer,
                       required => false,
                       default => 3,
                       constraints => [{min, 0}, {max, 10}],
                       description => <<"Maximum retry attempts">>,
                       examples => [0, 3, 5]},
      retry_delay => #{type => integer,
                       required => false,
                       default => 1000,
                       constraints => [{min, 100}],
                       description => <<"Delay between retries in milliseconds">>,
                       examples => [500, 1000, 2000]},
      ssl_options => #{type => list,
                       required => false,
                       default => [],
                       description => <<"SSL/TLS options for HTTPS">>,
                       examples => [[{verify, verify_peer}]]},
      pool_size => #{type => integer,
                     required => false,
                     default => 5,
                     constraints => [{min, 1}, {max, 100}],
                     description => <<"HTTP connection pool size">>,
                     examples => [1, 5, 10]},
      server_id => #{type => atom,
                     required => false,
                     default => undefined,
                     description => <<"Optional server identifier">>,
                     examples => [my_server, http_server]},
      transport_id => #{type => atom,
                        required => false,
                        description => <<"Unique transport identifier">>,
                        examples => [http_transport, my_http]}}.

%% @doc WebSocket transport schema definition
-spec websocket_schema() -> transport_schema().
websocket_schema() ->
    #{url => #{type => {one_of, [string, binary]},
               required => true,
               constraints => [{format, websocket_url}],
               description => <<"WebSocket URL (ws:// or wss://)">>,
               examples => ["wss://api.example.com/ws", <<"ws://localhost:8080/mcp">>]},
      owner => #{type => pid,
                 required => true,
                 description => <<"Process ID that owns this transport">>,
                 examples => [self()]},
      protocols => #{type => list,
                     required => false,
                     default => [],
                     description => <<"WebSocket subprotocols">>,
                     examples => [[<<"mcp">>, <<"json-rpc">>]]},
      headers => #{type => list,
                   required => false,
                   default => [],
                   description => <<"Additional WebSocket headers">>,
                   examples => [[{"Authorization", "Bearer token123"}]]},
      connect_timeout => #{type => integer,
                           required => false,
                           default => 10000,
                           constraints => [{min, 1000}],
                           description => <<"Connection timeout in milliseconds">>,
                           examples => [5000, 10000, 15000]},
      ping_interval => #{type => integer,
                         required => false,
                         default => 30000,
                         constraints => [{min, 5000}],
                         description => <<"WebSocket ping interval in milliseconds">>,
                         examples => [15000, 30000, 60000]},
      ssl_options => #{type => list,
                       required => false,
                       default => [],
                       description => <<"SSL/TLS options for wss:// URLs">>,
                       examples => [[{verify, verify_peer}]]},
      server_id => #{type => atom,
                     required => false,
                     default => undefined,
                     description => <<"Optional server identifier">>,
                     examples => [my_server, ws_server]},
      transport_id => #{type => atom,
                        required => false,
                        description => <<"Unique transport identifier">>,
                        examples => [ws_transport, my_websocket]}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Validate configuration against schema
-spec validate_with_schema(map(), transport_schema()) -> validation_result().
validate_with_schema(Config, Schema) ->
    % First validate required fields are present
    case check_required_fields(Config, Schema) of
        ok ->
            % Then validate each field in config
            ValidationResults = maps:fold(
                fun(Field, Value, Acc) ->
                    case maps:get(Field, Schema, undefined) of
                        undefined ->
                            % Unknown field warning but not error
                            [#{field => Field,
                               value => Value,
                               error => unknown_field,
                               message => iolist_to_binary(io_lib:format("Unknown field ~p", [Field])),
                               constraint => undefined} | Acc];
                        FieldSchema ->
                            case validate_field_value(Field, Value, FieldSchema) of
                                ok -> Acc;
                                {error, Errors} -> Errors ++ Acc
                            end
                    end
                end,
                [],
                Config
            ),
            case ValidationResults of
                [] -> ok;
                Errors -> {error, lists:reverse(Errors)}
            end;
        {error, RequiredErrors} ->
            {error, RequiredErrors}
    end.

%% @private
%% Check if all required fields are present
-spec check_required_fields(map(), transport_schema()) -> validation_result().
check_required_fields(Config, Schema) ->
    RequiredFields = maps:fold(
        fun(Field, FieldSchema, Acc) ->
            case maps:get(required, FieldSchema, false) of
                true -> [Field | Acc];
                false -> Acc
            end
        end,
        [],
        Schema
    ),
    
    MissingFields = [Field || Field <- RequiredFields, not maps:is_key(Field, Config)],
    
    case MissingFields of
        [] -> ok;
        Missing ->
            Errors = [#{field => Field,
                       value => undefined,
                       error => missing_required_field,
                       message => iolist_to_binary(io_lib:format("Required field ~p is missing", [Field])),
                       constraint => required} || Field <- Missing],
            {error, Errors}
    end.

%% @private
%% Validate individual field value
-spec validate_field_value(atom(), term(), field_schema()) -> validation_result().
validate_field_value(Field, Value, FieldSchema) ->
    % Check type first
    case validate_field_type(Value, maps:get(type, FieldSchema)) of
        ok ->
            % Then check constraints
            validate_field_constraints(Field, Value, maps:get(constraints, FieldSchema, []));
        {error, TypeError} ->
            {error, [#{field => Field,
                      value => Value,
                      error => type_mismatch,
                      message => iolist_to_binary(io_lib:format("Field ~p has invalid type: ~s", [Field, TypeError])),
                      constraint => maps:get(type, FieldSchema)}]}
    end.

%% @private  
%% Validate field type
-spec validate_field_type(term(), atom() | tuple()) -> ok | {error, binary()}.
validate_field_type(Value, atom) when is_atom(Value) -> ok;
validate_field_type(Value, integer) when is_integer(Value) -> ok;
validate_field_type(Value, boolean) when is_boolean(Value) -> ok;
validate_field_type(Value, pid) when is_pid(Value) -> ok;
validate_field_type(Value, binary) when is_binary(Value) -> ok;
validate_field_type(Value, string) when is_list(Value) -> ok;
validate_field_type(Value, list) when is_list(Value) -> ok;
validate_field_type(Value, map) when is_map(Value) -> ok;
validate_field_type(Value, {one_of, Types}) ->
    case lists:any(fun(Type) -> validate_field_type(Value, Type) =:= ok end, Types) of
        true -> ok;
        false -> {error, iolist_to_binary(io_lib:format("Expected one of ~p", [Types]))}
    end;
validate_field_type({A,B,C,D}, ip_address) 
    when is_integer(A), A >= 0, A =< 255,
         is_integer(B), B >= 0, B =< 255,
         is_integer(C), C >= 0, C =< 255,
         is_integer(D), D >= 0, D =< 255 -> ok;
validate_field_type(Value, Type) ->
    {error, iolist_to_binary(io_lib:format("Expected ~p, got ~p", [Type, typeof(Value)]))}.

%% @private
%% Validate field constraints
-spec validate_field_constraints(atom(), term(), [constraint()]) -> validation_result().
validate_field_constraints(_Field, _Value, []) -> ok;
validate_field_constraints(Field, Value, [{min, Min} | Rest]) when is_integer(Value) ->
    case Value >= Min of
        true -> validate_field_constraints(Field, Value, Rest);
        false ->
            {error, [#{field => Field,
                      value => Value,
                      error => constraint_violation,
                      message => iolist_to_binary(io_lib:format("Value ~p is less than minimum ~p", [Value, Min])),
                      constraint => {min, Min}}]}
    end;
validate_field_constraints(Field, Value, [{max, Max} | Rest]) when is_integer(Value) ->
    case Value =< Max of
        true -> validate_field_constraints(Field, Value, Rest);
        false ->
            {error, [#{field => Field,
                      value => Value,
                      error => constraint_violation,
                      message => iolist_to_binary(io_lib:format("Value ~p exceeds maximum ~p", [Value, Max])),
                      constraint => {max, Max}}]}
    end;
validate_field_constraints(Field, Value, [{one_of, ValidValues} | Rest]) ->
    case lists:member(Value, ValidValues) of
        true -> validate_field_constraints(Field, Value, Rest);
        false ->
            {error, [#{field => Field,
                      value => Value,
                      error => constraint_violation,
                      message => iolist_to_binary(io_lib:format("Value ~p not in allowed values: ~p", [Value, ValidValues])),
                      constraint => {one_of, ValidValues}}]}
    end;
validate_field_constraints(Field, Value, [{format, http_url} | Rest]) ->
    case validate_http_url(Value) of
        ok -> validate_field_constraints(Field, Value, Rest);
        {error, Reason} ->
            {error, [#{field => Field,
                      value => Value,
                      error => format_violation,
                      message => iolist_to_binary(io_lib:format("Invalid HTTP URL: ~s", [Reason])),
                      constraint => {format, http_url}}]}
    end;
validate_field_constraints(Field, Value, [{format, websocket_url} | Rest]) ->
    case validate_websocket_url(Value) of
        ok -> validate_field_constraints(Field, Value, Rest);
        {error, Reason} ->
            {error, [#{field => Field,
                      value => Value,
                      error => format_violation,
                      message => iolist_to_binary(io_lib:format("Invalid WebSocket URL: ~s", [Reason])),
                      constraint => {format, websocket_url}}]}
    end;
validate_field_constraints(Field, Value, [{custom, ValidationFun} | Rest]) when is_function(ValidationFun, 1) ->
    case ValidationFun(Value) of
        true -> validate_field_constraints(Field, Value, Rest);
        false ->
            {error, [#{field => Field,
                      value => Value,
                      error => custom_validation_failed,
                      message => iolist_to_binary(io_lib:format("Custom validation failed for field ~p", [Field])),
                      constraint => custom}]}
    end;
validate_field_constraints(Field, Value, [_UnknownConstraint | Rest]) ->
    % Skip unknown constraints but continue validation
    validate_field_constraints(Field, Value, Rest).

%% @private
%% Apply schema defaults to configuration
-spec apply_schema_defaults(map(), transport_schema()) -> map().
apply_schema_defaults(Config, Schema) ->
    maps:fold(
        fun(Field, FieldSchema, AccConfig) ->
            case maps:is_key(Field, AccConfig) of
                true -> AccConfig; % Field already present
                false ->
                    case maps:get(default, FieldSchema, undefined) of
                        undefined -> AccConfig; % No default value
                        DefaultValue -> AccConfig#{Field => DefaultValue}
                    end
            end
        end,
        Config,
        Schema
    ).

%% @private
%% Get human-readable transport description
-spec get_transport_description(atom()) -> binary().
get_transport_description(stdio) ->
    <<"Standard input/output transport for process communication">>;
get_transport_description(tcp) ->
    <<"TCP socket transport for network communication">>;
get_transport_description(http) ->
    <<"HTTP/HTTPS client transport for web API communication">>;
get_transport_description(websocket) ->
    <<"WebSocket transport for real-time bidirectional communication">>;
get_transport_description(_) ->
    <<"Custom transport implementation">>.

%% @private
%% Format single validation error
-spec format_single_error(validation_error()) -> binary().
format_single_error(#{field := Field, message := Message}) ->
    iolist_to_binary(io_lib:format("~p: ~s", [Field, Message])).

%% @private
%% Get type of a value
-spec typeof(term()) -> atom().
typeof(Value) when is_atom(Value) -> atom;
typeof(Value) when is_integer(Value) -> integer;
typeof(Value) when is_float(Value) -> float;
typeof(Value) when is_boolean(Value) -> boolean;
typeof(Value) when is_binary(Value) -> binary;
typeof(Value) when is_list(Value) -> list;
typeof(Value) when is_map(Value) -> map;
typeof(Value) when is_pid(Value) -> pid;
typeof(Value) when is_port(Value) -> port;
typeof(Value) when is_reference(Value) -> reference;
typeof(Value) when is_tuple(Value) -> tuple;
typeof(_) -> unknown.

%% @private
%% Validate HTTP URL format
-spec validate_http_url(string() | binary()) -> ok | {error, binary()}.
validate_http_url(Url) when is_binary(Url) ->
    validate_http_url(binary_to_list(Url));
validate_http_url(Url) when is_list(Url) ->
    case string:prefix(Url, "http://") orelse string:prefix(Url, "https://") of
        nomatch ->
            {error, <<"URL must start with http:// or https://">>};
        _ ->
            case uri_string:parse(Url) of
                #{scheme := _, host := _} -> ok;
                _ -> {error, <<"Invalid URL format">>}
            end
    end;
validate_http_url(_) ->
    {error, <<"URL must be a string or binary">>}.

%% @private
%% Validate WebSocket URL format
-spec validate_websocket_url(string() | binary()) -> ok | {error, binary()}.
validate_websocket_url(Url) when is_binary(Url) ->
    validate_websocket_url(binary_to_list(Url));
validate_websocket_url(Url) when is_list(Url) ->
    case string:prefix(Url, "ws://") orelse string:prefix(Url, "wss://") of
        nomatch ->
            {error, <<"URL must start with ws:// or wss://">>};
        _ ->
            case uri_string:parse(Url) of
                #{scheme := _, host := _} -> ok;
                _ -> {error, <<"Invalid WebSocket URL format">>}
            end
    end;
validate_websocket_url(_) ->
    {error, <<"URL must be a string or binary">>}.