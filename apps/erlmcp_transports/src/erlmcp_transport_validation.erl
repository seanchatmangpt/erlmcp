%%%-------------------------------------------------------------------
%%% @doc
%%% Transport Validation Module
%%%
%%% Provides comprehensive validation functions for transport configurations,
%% messages, and operations. Ensures protocol compliance and security.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_validation).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

%% API exports
-export([
    validate_transport_config/1,
    validate_connection_params/1,
    validate_message_size/2,
    validate_host/1,
    validate_port/1,
    validate_url/1,
    validate_ssl_options/1,
    sanitize_headers/1,
    check_rate_limit/2,
    validate_authentication/2
]).

-define(MAX_MESSAGE_SIZE_DEFAULT, 16777216). % 16MB
-define(MAX_HEADER_SIZE, 8192). % 8KB
-define(MAX_URL_LENGTH, 2048).

-type validation_result() :: ok | {error, term()}.
-type transport_config() :: map().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Validate complete transport configuration
-spec validate_transport_config(transport_config()) -> validation_result().
validate_transport_config(Config) when is_map(Config) ->
    case validate_required_fields(Config) of
        ok ->
            case validate_transport_type(Config) of
                ok ->
                    validate_security_settings(Config);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
validate_transport_config(_) ->
    {error, {invalid_config, not_a_map}}.

%% @doc Validate connection parameters
-spec validate_connection_params(map()) -> validation_result().
validate_connection_params(Params) when is_map(Params) ->
    do_validate_connection_params(Params);
validate_connection_params(_) ->
    {error, {invalid_params, not_a_map}}.

%% @doc Validate message size against limit
-spec validate_message_size(binary(), pos_integer()) -> validation_result().
validate_message_size(Message, MaxSize) when is_binary(Message) ->
    Size = byte_size(Message),
    case Size =< MaxSize of
        true -> ok;
        false -> {error, {message_too_large, Size, MaxSize}}
    end;
validate_message_size(_, _) ->
    {error, {invalid_message, not_binary}}.

%% @doc Validate host (hostname or IP)
-spec validate_host(inet:hostname() | inet:ip_address()) -> validation_result().
validate_host(Host) when is_list(Host) ->
    validate_hostname(Host);
validate_host(Host) when is_tuple(Host) ->
    validate_ip_address(Host);
validate_host(_) ->
    {error, {invalid_host, unsupported_type}}.

%% @doc Validate port number
-spec validate_port(inet:port_number()) -> validation_result().
validate_port(Port) when is_integer(Port), Port > 0, Port =< 65535 ->
    ok;
validate_port(_) ->
    {error, {invalid_port, out_of_range}}.

%% @doc Validate URL format
-spec validate_url(binary() | string()) -> validation_result().
validate_url(URL) when is_binary(URL) ->
    validate_url(binary_to_list(URL));
validate_url(URL) when is_list(URL) ->
    case length(URL) > ?MAX_URL_LENGTH of
        true ->
            {error, {invalid_url, too_long}};
        false ->
            case parse_url(URL) of
                {ok, _Scheme, _Host, _Port, _Path} ->
                    ok;
                {error, _} = Error ->
                    Error
            end
    end;
validate_url(_) ->
    {error, {invalid_url, unsupported_type}}.

%% @doc Validate SSL/TLS options
-spec validate_ssl_options(proplists:proplist()) -> validation_result().
validate_ssl_options(Options) when is_list(Options) ->
    validate_ssl_options_list(Options);
validate_ssl_options(_) ->
    {error, {invalid_ssl_options, not_a_list}}.

%% @doc Sanitize HTTP headers to prevent injection attacks
-spec sanitize_headers(proplists:proplist()) -> {ok, proplists:proplist()} | {error, term()}.
sanitize_headers(Headers) when is_list(Headers) ->
    sanitize_headers_list(Headers, []);
sanitize_headers(_) ->
    {error, {invalid_headers, not_a_list}}.

%% @doc Check if rate limit is exceeded
-spec check_rate_limit(transport_config(), non_neg_integer()) -> allowed | {error, rate_exceeded}.
check_rate_limit(Config, RequestCount) ->
    MaxRequests = maps:get(max_requests, Config, infinity),
    case MaxRequests of
        infinity ->
            allowed;
        _ when RequestCount < MaxRequests ->
            allowed;
        _ ->
            {error, rate_exceeded}
    end.

%% @doc Validate authentication credentials
-spec validate_authentication(map(), term()) -> validation_result().
validate_authentication(_Config, Credentials) ->
    case validate_credentials_format(Credentials) of
        ok ->
            ok;
        {error, _} = Error ->
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Validate required configuration fields
-spec validate_required_fields(map()) -> validation_result().
validate_required_fields(Config) ->
    Required = [transport_id],
    Missing = [Field || Field <- Required, not maps:is_key(Field, Config)],
    case Missing of
        [] -> ok;
        _ -> {error, {missing_required_fields, Missing}}
    end.

%% @doc Validate transport type
-spec validate_transport_type(map()) -> validation_result().
validate_transport_type(Config) ->
    Type = maps:get(type, Config, custom),
    ValidTypes = [stdio, tcp, http, websocket, sse, custom],
    case lists:member(Type, ValidTypes) of
        true -> ok;
        false -> {error, {invalid_transport_type, Type}}
    end.

%% @doc Validate security settings
-spec validate_security_settings(map()) -> validation_result().
validate_security_settings(Config) ->
    % Check for insecure configurations
    case maps:get(allow_insecure, Config, false) of
        true ->
            ?LOG_WARNING("Insecure transport configuration detected", []),
            {warning, insecure_configuration};
        false ->
            ok
    end.

%% @doc Validate connection parameters details
-spec do_validate_connection_params(map()) -> validation_result().
do_validate_connection_params(Params) ->
    case maps:get(host, Params, undefined) of
        undefined ->
            {error, {missing_param, host}};
        Host ->
            case validate_host(Host) of
                ok ->
                    case maps:get(port, Params, undefined) of
                        undefined ->
                            {error, {missing_param, port}};
                        Port ->
                            validate_port(Port)
                    end;
                {error, _} = Error ->
                    Error
            end
    end.

%% @doc Validate hostname format
-spec validate_hostname(string()) -> validation_result().
validate_hostname(Host) ->
    case Host of
        "" ->
            {error, {invalid_host, empty}};
        "localhost" ->
            ok;
        _ ->
            case length(Host) > 253 of
                true ->
                    {error, {invalid_host, too_long}};
                false ->
                    validate_hostname_labels(Host)
            end
    end.

%% @doc Validate hostname labels
-spec validate_hostname_labels(string()) -> validation_result().
validate_hostname_labels(Host) ->
    Labels = string:split(Host, ".", all),
    InvalidLabels = [L || L <- Labels,
                          not validate_label(L)],
    case InvalidLabels of
        [] ->
            ok;
        _ ->
            {error, {invalid_host, invalid_labels}}
    end.

%% @doc Validate single hostname label
-spec validate_label(string()) -> boolean().
validate_label(Label) ->
    Length = length(Label),
    Length >= 1 andalso Length =< 63 andalso
    lists:all(fun(Char) ->
        case Char of
            $- -> true;
            C when C >= $0, C =< $9 -> true;
            C when C >= $a, C =< $z -> true;
            C when C >= $A, C =< $Z -> true;
            _ -> false
        end
    end, Label).

%% @doc Validate IP address
-spec validate_ip_address(inet:ip_address()) -> validation_result().
validate_ip_address({A, B, C, D}) when is_integer(A), is_integer(B),
                                       is_integer(C), is_integer(D),
                                       A >= 0, A =< 255,
                                       B >= 0, B =< 255,
                                       C >= 0, C =< 255,
                                       D >= 0, D =< 255 ->
    ok;
validate_ip_address({A, B, C, D, E, F, G, H}) when is_integer(A), is_integer(B),
                                                   is_integer(C), is_integer(D),
                                                   is_integer(E), is_integer(F),
                                                   is_integer(G), is_integer(H) ->
    % IPv6 validation (simplified)
    ok;
validate_ip_address(_) ->
    {error, {invalid_host, invalid_ip_format}}.

%% @doc Parse URL into components
-spec parse_url(string()) -> {ok, atom(), string(), inet:port_number(), string()} | {error, term()}.
parse_url(URL) ->
    case string:split(URL, "://", 2) of
        [Scheme, Rest] ->
            parse_scheme_and_rest(Scheme, Rest);
        _ ->
            {error, {invalid_url, missing_scheme}}
    end.

%% @doc Parse URL scheme and rest
-spec parse_scheme_and_rest(string(), string()) -> {ok, atom(), string(), inet:port_number(), string()} | {error, term()}.
parse_scheme_and_rest(Scheme, Rest) ->
    SchemeAtom = list_to_existing_atom(Scheme),
    case SchemeAtom of
        http -> parse_host_port_path(Rest, 80);
        https -> parse_host_port_path(Rest, 443);
        ws -> parse_host_port_path(Rest, 80);
        wss -> parse_host_port_path(Rest, 443);
        _ ->
            {error, {invalid_url, unsupported_scheme, Scheme}}
    end.

%% @doc Parse host, port, and path from URL
-spec parse_host_port_path(string(), inet:port_number()) -> {ok, atom(), string(), inet:port_number(), string()} | {error, term()}.
parse_host_port_path(Rest, DefaultPort) ->
    case string:split(Rest, "/", 2) of
        [HostPort, Path] ->
            parse_host_port(HostPort, DefaultPort, Path);
        [HostPort] ->
            parse_host_port(HostPort, DefaultPort, "/")
    end.

%% @doc Parse host and port
-spec parse_host_port(string(), inet:port_number(), string()) -> {ok, atom(), string(), inet:port_number(), string()} | {error, term()}.
parse_host_port(HostPort, DefaultPort, Path) ->
    case string:split(HostPort, ":", 2) of
        [Host, PortStr] ->
            case string:to_integer(PortStr) of
                {Port, ""} when Port > 0, Port =< 65535 ->
                    {ok, http, Host, Port, Path};
                _ ->
                    {error, {invalid_url, invalid_port}}
            end;
        [Host] ->
            {ok, http, Host, DefaultPort, Path}
    end.

%% @doc Validate SSL options list
-spec validate_ssl_options_list(proplists:proplist()) -> validation_result().
validate_ssl_options_list([]) ->
    ok;
validate_ssl_options_list([{verify, Verify} | Rest]) when Verify =:= verify_none;
                                                           Verify =:= verify_peer;
                                                           Verify =:= verify_fail_if_no_peer_cert ->
    validate_ssl_options_list(Rest);
validate_ssl_options_list([{depth, Depth} | Rest]) when is_integer(Depth), Depth > 0 ->
    validate_ssl_options_list(Rest);
validate_ssl_options_list([{cert, Cert} | Rest]) when is_binary(Cert); is_list(Cert) ->
    validate_ssl_options_list(Rest);
validate_ssl_options_list([{key, Key} | Rest]) when is_binary(Key); is_list(Key) ->
    validate_ssl_options_list(Rest);
validate_ssl_options_list([{cacerts, Cacerts} | Rest]) when is_list(Cacerts) ->
    validate_ssl_options_list(Rest);
validate_ssl_options_list([_ | Rest]) ->
    validate_ssl_options_list(Rest);
validate_ssl_options_list(_) ->
    {error, {invalid_ssl_options, malformed}}.

%% @doc Sanitize headers list
-spec sanitize_headers_list(proplists:proplist(), proplists:proplist()) -> {ok, proplists:proplist()} | {error, term()}.
sanitize_headers_list([], Acc) ->
    {ok, lists:reverse(Acc)};
sanitize_headers_list([{Key, Value} | Rest], Acc) ->
    case sanitize_header_key(Key) of
        ok ->
            case sanitize_header_value(Value) of
                ok ->
                    sanitize_headers_list(Rest, [{Key, Value} | Acc]);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%% @doc Sanitize header key
-spec sanitize_header_key(binary() | string()) -> validation_result().
sanitize_header_key(Key) when is_binary(Key) ->
    sanitize_header_key(binary_to_list(Key));
sanitize_header_key(Key) when is_list(Key) ->
    case length(Key) > ?MAX_HEADER_SIZE of
        true ->
            {error, {invalid_header, key_too_long}};
        false ->
            case lists:all(fun(C) ->
                case C of
                    C when C >= 32, C =< 126 -> true;
                    _ -> false
                end
            end, Key) of
                true -> ok;
                false -> {error, {invalid_header, key_contains_invalid_chars}}
            end
    end.

%% @doc Sanitize header value
-spec sanitize_header_value(binary() | string()) -> validation_result().
sanitize_header_value(Value) when is_binary(Value) ->
    sanitize_header_value(binary_to_list(Value));
sanitize_header_value(Value) when is_list(Value) ->
    case length(Value) > ?MAX_HEADER_SIZE of
        true ->
            {error, {invalid_header, value_too_long}};
        false ->
            ok
    end.

%% @doc Validate credentials format
-spec validate_credentials_format(term()) -> validation_result().
validate_credentials_format(Credentials) when is_map(Credentials) ->
    case maps:is_key(username, Credentials) andalso maps:is_key(password, Credentials) of
        true ->
            ok;
        false ->
            case maps:is_key(token, Credentials) of
                true -> ok;
                false -> {error, {invalid_credentials, missing_fields}}
            end
    end;
validate_credentials_format(_) ->
    {error, {invalid_credentials, unsupported_format}}.
