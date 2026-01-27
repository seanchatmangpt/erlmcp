%% @doc Transport Validation Module
%% Provides comprehensive validation for transport configurations
%% with detailed error reporting and type-specific checks.
%%
%% Features:
%% - Type-specific validation for stdio, tcp, and http transports
%% - Field presence and format validation
%% - Enhanced error messages with context
%% - Integration with main API validation flow
-module(erlmcp_transport_validation).

%% API
-export([validate_transport_config/2, validate_transport_config/1, validate_field/3,
         get_validation_errors/1, is_valid_transport_type/1, get_required_fields/1,
         get_optional_fields/1]).

-include("erlmcp.hrl").

%% Types
-type transport_type() :: stdio | tcp | http | websocket | custom.
-type validation_error() :: {validation_error, atom(), term(), string()}.
-type validation_result() :: ok | {error, validation_error() | [validation_error()]}.

-export_type([transport_type/0, validation_error/0, validation_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate transport configuration with type
-spec validate_transport_config(atom(), map()) -> validation_result().
validate_transport_config(Type, Config) ->
    validate_transport_config(Config#{type => Type}).

%% @doc Validate transport configuration (expects type in config)
-spec validate_transport_config(map()) -> validation_result().
validate_transport_config(Config) ->
    case maps:get(type, Config, undefined) of
        undefined ->
            {error, {validation_error, missing_field, type, "Transport type is required"}};
        Type ->
            case is_valid_transport_type(Type) of
                true ->
                    validate_transport_by_type(Type, Config);
                false ->
                    ValidTypes = [stdio, tcp, http],
                    {error,
                     {validation_error,
                      invalid_type,
                      Type,
                      io_lib:format("Unknown transport type. Valid types: ~p", [ValidTypes])}}
            end
    end.

%% @doc Validate a specific field with context
-spec validate_field(atom(), term(), map()) -> ok | {error, validation_error()}.
validate_field(type, Value, _Context) ->
    case is_valid_transport_type(Value) of
        true ->
            ok;
        false ->
            {error, {validation_error, invalid_type, Value, "Must be one of: stdio, tcp, http"}}
    end;
validate_field(host, Value, _Context) ->
    validate_host_field(Value);
validate_field(port, Value, _Context) ->
    validate_port_field(Value);
validate_field(url, Value, _Context) ->
    validate_url_field(Value);
validate_field(server_id, Value, _Context) ->
    case is_atom(Value) of
        true ->
            ok;
        false ->
            {error, {validation_error, invalid_type, server_id, "Server ID must be an atom"}}
    end;
validate_field(enabled, Value, _Context) ->
    case is_boolean(Value) of
        true ->
            ok;
        false ->
            {error, {validation_error, invalid_type, enabled, "Enabled flag must be boolean"}}
    end;
validate_field(timeout, Value, _Context) ->
    case is_integer(Value) andalso Value > 0 of
        true ->
            ok;
        false ->
            {error, {validation_error, invalid_value, timeout, "Timeout must be positive integer"}}
    end;
validate_field(Field, _Value, _Context) ->
    {error, {validation_error, unknown_field, Field, "Unknown field"}}.

%% @doc Get accumulated validation errors from config
-spec get_validation_errors(map()) -> [validation_error()].
get_validation_errors(Config) ->
    case validate_transport_config(Config) of
        ok ->
            [];
        {error, Error} when is_tuple(Error) ->
            [Error];
        {error, Errors} when is_list(Errors) ->
            Errors
    end.

%% @doc Check if transport type is valid
-spec is_valid_transport_type(term()) -> boolean().
is_valid_transport_type(stdio) ->
    true;
is_valid_transport_type(tcp) ->
    true;
is_valid_transport_type(http) ->
    true;
is_valid_transport_type(_) ->
    false.

%% @doc Get required fields for transport type
-spec get_required_fields(atom()) -> [atom()].
get_required_fields(stdio) ->
    [type];
get_required_fields(tcp) ->
    [type, host, port];
get_required_fields(http) ->
    [type, url];
get_required_fields(_) ->
    [].

%% @doc Get optional fields for transport type
-spec get_optional_fields(atom()) -> [atom()].
get_optional_fields(stdio) ->
    [server_id, buffer_size, timeout, test_mode];
get_optional_fields(tcp) ->
    [server_id, keepalive, connect_timeout, max_reconnect_attempts, ssl, certfile, keyfile];
get_optional_fields(http) ->
    [server_id, method, headers, timeout, cors, max_content_length];
get_optional_fields(_) ->
    [].

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Validate transport configuration by type
-spec validate_transport_by_type(atom(), map()) -> validation_result().
validate_transport_by_type(Type, Config) ->
    RequiredFields = get_required_fields(Type),
    OptionalFields = get_optional_fields(Type),
    AllowedFields = RequiredFields ++ OptionalFields,

    case validate_field_presence(Config, RequiredFields) of
        ok ->
            case validate_field_allowance(Config, AllowedFields) of
                ok ->
                    validate_field_values(Type, Config);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc Validate required fields are present
-spec validate_field_presence(map(), [atom()]) -> validation_result().
validate_field_presence(Config, RequiredFields) ->
    MissingFields = [Field || Field <- RequiredFields, not maps:is_key(Field, Config)],
    case MissingFields of
        [] ->
            ok;
        [Field] ->
            {error, {validation_error, missing_field, Field, "Required field is missing"}};
        Fields ->
            {error,
             {validation_error,
              missing_fields,
              Fields,
              io_lib:format("Required fields are missing: ~p", [Fields])}}
    end.

%% @doc Validate only allowed fields are present
-spec validate_field_allowance(map(), [atom()]) -> validation_result().
validate_field_allowance(Config, AllowedFields) ->
    ConfigFields = maps:keys(Config),
    UnknownFields = [Field || Field <- ConfigFields, not lists:member(Field, AllowedFields)],
    case UnknownFields of
        [] ->
            ok;
        [Field] ->
            {error,
             {validation_error,
              unknown_field,
              Field,
              io_lib:format("Unknown field. Allowed: ~p", [AllowedFields])}};
        Fields ->
            {error,
             {validation_error,
              unknown_fields,
              Fields,
              io_lib:format("Unknown fields: ~p. Allowed: ~p", [Fields, AllowedFields])}}
    end.

%% @doc Validate field values based on transport type
-spec validate_field_values(atom(), map()) -> validation_result().
validate_field_values(stdio, Config) ->
    validate_stdio_fields(Config);
validate_field_values(tcp, Config) ->
    validate_tcp_fields(Config);
validate_field_values(http, Config) ->
    validate_http_fields(Config).

%% @doc Validate STDIO-specific fields
-spec validate_stdio_fields(map()) -> validation_result().
validate_stdio_fields(Config) ->
    Validations =
        [validate_optional_field(buffer_size, Config, fun validate_buffer_size/1),
         validate_optional_field(timeout, Config, fun validate_timeout/1),
         validate_optional_field(test_mode, Config, fun validate_boolean_field/1),
         validate_optional_field(server_id, Config, fun validate_server_id/1)],
    collect_validation_results(Validations).

%% @doc Validate TCP-specific fields
-spec validate_tcp_fields(map()) -> validation_result().
validate_tcp_fields(Config) ->
    Validations =
        [validate_field(host, maps:get(host, Config), Config),
         validate_field(port, maps:get(port, Config), Config),
         validate_optional_field(keepalive, Config, fun validate_boolean_field/1),
         validate_optional_field(connect_timeout, Config, fun validate_timeout/1),
         validate_optional_field(max_reconnect_attempts, Config, fun validate_non_negative_int/1),
         validate_optional_field(ssl, Config, fun validate_ssl_config/1),
         validate_optional_field(server_id, Config, fun validate_server_id/1)],
    collect_validation_results(Validations).

%% @doc Validate HTTP-specific fields
-spec validate_http_fields(map()) -> validation_result().
validate_http_fields(Config) ->
    Validations =
        [validate_field(url, maps:get(url, Config), Config),
         validate_optional_field(method, Config, fun validate_http_method/1),
         validate_optional_field(headers, Config, fun validate_http_headers/1),
         validate_optional_field(timeout, Config, fun validate_timeout/1),
         validate_optional_field(cors, Config, fun validate_cors_config/1),
         validate_optional_field(max_content_length, Config, fun validate_positive_int/1),
         validate_optional_field(server_id, Config, fun validate_server_id/1)],
    collect_validation_results(Validations).

%% @doc Validate optional field if present
-spec validate_optional_field(atom(), map(), fun((term()) -> validation_result())) ->
                                 validation_result().
validate_optional_field(Field, Config, ValidatorFun) ->
    case maps:get(Field, Config, undefined) of
        undefined ->
            ok;
        Value ->
            case ValidatorFun(Value) of
                ok ->
                    ok;
                {error, {validation_error, ErrorType, _, Message}} ->
                    {error, {validation_error, ErrorType, Field, Message}};
                {error, Error} ->
                    {error, {validation_error, invalid_value, Field, io_lib:format("~p", [Error])}}
            end
    end.

%% @doc Collect and combine validation results
-spec collect_validation_results([validation_result()]) -> validation_result().
collect_validation_results(Results) ->
    Errors = [Error || {error, Error} <- Results],
    case Errors of
        [] ->
            ok;
        [SingleError] ->
            {error, SingleError};
        MultipleErrors ->
            {error, MultipleErrors}
    end.

%%====================================================================
%% Field Validation Functions
%%====================================================================

%% @doc Validate host field
-spec validate_host_field(term()) -> validation_result().
validate_host_field(Host) when is_binary(Host) ->
    case byte_size(Host) > 0 of
        true ->
            ok;
        false ->
            {error, {validation_error, invalid_value, host, "Host cannot be empty"}}
    end;
validate_host_field(Host) when is_list(Host) ->
    case length(Host) > 0 of
        true ->
            ok;
        false ->
            {error, {validation_error, invalid_value, host, "Host cannot be empty"}}
    end;
validate_host_field(_) ->
    {error, {validation_error, invalid_type, host, "Host must be string or binary"}}.

%% @doc Validate port field
-spec validate_port_field(term()) -> validation_result().
validate_port_field(Port) when is_integer(Port), Port > 0, Port =< 65535 ->
    ok;
validate_port_field(Port) when is_integer(Port) ->
    {error, {validation_error, invalid_value, port, "Port must be between 1 and 65535"}};
validate_port_field(_) ->
    {error, {validation_error, invalid_type, port, "Port must be integer"}}.

%% @doc Validate URL field
-spec validate_url_field(term()) -> validation_result().
validate_url_field(Url) when is_binary(Url) ->
    case validate_url_format(Url) of
        true ->
            ok;
        false ->
            {error, {validation_error, invalid_value, url, "Must be valid HTTP/HTTPS URL"}}
    end;
validate_url_field(Url) when is_list(Url) ->
    case validate_url_format(list_to_binary(Url)) of
        true ->
            ok;
        false ->
            {error, {validation_error, invalid_value, url, "Must be valid HTTP/HTTPS URL"}}
    end;
validate_url_field(_) ->
    {error, {validation_error, invalid_type, url, "URL must be string or binary"}}.

%% @doc Simple URL format validation
-spec validate_url_format(binary()) -> boolean().
validate_url_format(Url) ->
    case binary:split(Url, <<"://">>) of
        [Scheme, Rest] when Scheme =:= <<"http">>; Scheme =:= <<"https">> ->
            byte_size(Rest) > 0;
        _ ->
            false
    end.

%% @doc Validate buffer size
-spec validate_buffer_size(term()) -> validation_result().
validate_buffer_size(Size) when is_integer(Size), Size > 0 ->
    ok;
validate_buffer_size(_) ->
    {error, {validation_error, invalid_type, buffer_size, "Must be positive integer"}}.

%% @doc Validate timeout value
-spec validate_timeout(term()) -> validation_result().
validate_timeout(Timeout) when is_integer(Timeout), Timeout > 0 ->
    ok;
validate_timeout(infinity) ->
    ok;
validate_timeout(_) ->
    {error,
     {validation_error, invalid_value, timeout, "Must be positive integer or 'infinity'"}}.

%% @doc Validate boolean field
-spec validate_boolean_field(term()) -> validation_result().
validate_boolean_field(Value) when is_boolean(Value) ->
    ok;
validate_boolean_field(_) ->
    {error, {validation_error, invalid_type, boolean, "Must be boolean"}}.

%% @doc Validate server ID
-spec validate_server_id(term()) -> validation_result().
validate_server_id(ServerId) when is_atom(ServerId) ->
    ok;
validate_server_id(_) ->
    {error, {validation_error, invalid_type, server_id, "Must be atom"}}.

%% @doc Validate non-negative integer
-spec validate_non_negative_int(term()) -> validation_result().
validate_non_negative_int(Value) when is_integer(Value), Value >= 0 ->
    ok;
validate_non_negative_int(_) ->
    {error,
     {validation_error, invalid_value, non_negative_int, "Must be non-negative integer"}}.

%% @doc Validate positive integer
-spec validate_positive_int(term()) -> validation_result().
validate_positive_int(Value) when is_integer(Value), Value > 0 ->
    ok;
validate_positive_int(_) ->
    {error, {validation_error, invalid_value, positive_int, "Must be positive integer"}}.

%% @doc Validate SSL configuration
-spec validate_ssl_config(term()) -> validation_result().
validate_ssl_config(false) ->
    ok;
validate_ssl_config(true) ->
    ok;
validate_ssl_config(Config) when is_map(Config) ->
    RequiredSslFields = [certfile, keyfile],
    case validate_field_presence(Config, RequiredSslFields) of
        ok ->
            validate_ssl_file_fields(Config);
        Error ->
            Error
    end;
validate_ssl_config(_) ->
    {error, {validation_error, invalid_type, ssl, "Must be boolean or SSL config map"}}.

%% @doc Validate SSL file fields
-spec validate_ssl_file_fields(map()) -> validation_result().
validate_ssl_file_fields(Config) ->
    Validations =
        [validate_file_field(certfile, maps:get(certfile, Config)),
         validate_file_field(keyfile, maps:get(keyfile, Config))],
    collect_validation_results(Validations).

%% @doc Validate file field
-spec validate_file_field(atom(), term()) -> validation_result().
validate_file_field(Field, Path) when is_list(Path); is_binary(Path) ->
    case filelib:is_file(Path) of
        true ->
            ok;
        false ->
            {error,
             {validation_error, file_not_found, Field, io_lib:format("File not found: ~s", [Path])}}
    end;
validate_file_field(Field, _) ->
    {error, {validation_error, invalid_type, Field, "Must be valid file path"}}.

%% @doc Validate HTTP method
-spec validate_http_method(term()) -> validation_result().
validate_http_method(Method)
    when Method =:= get;
         Method =:= post;
         Method =:= put;
         Method =:= delete;
         Method =:= patch;
         Method =:= head;
         Method =:= options ->
    ok;
validate_http_method(_) ->
    ValidMethods = [get, post, put, delete, patch, head, options],
    {error,
     {validation_error,
      invalid_value,
      http_method,
      io_lib:format("Must be one of: ~p", [ValidMethods])}}.

%% @doc Validate HTTP headers
-spec validate_http_headers(term()) -> validation_result().
validate_http_headers(Headers) when is_map(Headers) ->
    try
        maps:fold(fun(Key, Value, _Acc) ->
                     case {is_valid_header_key(Key), is_valid_header_value(Value)} of
                         {true, true} -> ok;
                         {false, _} -> throw({invalid_header_key, Key});
                         {_, false} -> throw({invalid_header_value, Value})
                     end
                  end,
                  ok,
                  Headers),
        ok
    catch
        {invalid_header_key, Key} ->
            {error,
             {validation_error, invalid_header_key, Key, "Header key must be string or binary"}};
        {invalid_header_value, Value} ->
            {error,
             {validation_error,
              invalid_header_value,
              Value,
              "Header value must be string or binary"}}
    end;
validate_http_headers(_) ->
    {error, {validation_error, invalid_type, headers, "Must be map"}}.

%% @doc Check if header key is valid
-spec is_valid_header_key(term()) -> boolean().
is_valid_header_key(Key) when is_binary(Key); is_list(Key) ->
    true;
is_valid_header_key(_) ->
    false.

%% @doc Check if header value is valid
-spec is_valid_header_value(term()) -> boolean().
is_valid_header_value(Value) when is_binary(Value); is_list(Value) ->
    true;
is_valid_header_value(_) ->
    false.

%% @doc Validate CORS configuration
-spec validate_cors_config(term()) -> validation_result().
validate_cors_config(false) ->
    ok;
validate_cors_config(true) ->
    ok;
validate_cors_config(Origins) when is_list(Origins) ->
    try
        lists:foreach(fun(Origin) ->
                         case is_list(Origin) orelse is_binary(Origin) of
                             true -> ok;
                             false -> throw({invalid_origin, Origin})
                         end
                      end,
                      Origins),
        ok
    catch
        {invalid_origin, Origin} ->
            {error,
             {validation_error, invalid_origin, Origin, "CORS origin must be string or binary"}}
    end;
validate_cors_config(_) ->
    {error,
     {validation_error, invalid_type, cors, "Must be boolean or list of origin strings"}}.
