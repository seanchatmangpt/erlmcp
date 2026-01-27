%%%-------------------------------------------------------------------
%% @doc MCP HTTP Header Validation Module (Gap #10)
%%
%% Implements comprehensive HTTP header validation per MCP 2025-11-25 spec:
%% 1. MCP-Protocol-Version header validation (required on all requests)
%% 2. Accept header validation for content negotiation
%% 3. Content-Type header validation on request body
%% 4. MCP-Session-Id header validation for session tracking
%% 5. Authorization header handling (informational)
%% 6. User-Agent header extraction (informational)
%%
%% Error Responses:
%% - HTTP 400: Bad Request (missing/invalid required headers)
%% - HTTP 415: Unsupported Media Type (invalid Content-Type)
%% - HTTP 406: Not Acceptable (unsupported Accept types)
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_http_header_validator).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    validate_request_headers/2,
    validate_protocol_version/1,
    validate_content_type/2,
    validate_accept/1,
    validate_session_id/1,
    validate_authorization/1,
    extract_headers_map/1,
    format_error_response/3
]).

%% Type definitions
-type http_method() :: get | post | put | patch | head | options | delete.
-type http_header() :: {binary() | string(), binary() | string()}.
-type headers_list() :: [http_header()].
-type headers_map() :: map().
-type headers() :: headers_list() | headers_map().

%% Error format: {StatusCode, ErrorMessage, ErrorData}
-type validation_error() :: {non_neg_integer(), binary(), map() | undefined}.

-type validation_result() :: {ok, map()} | {error, validation_error()}.

-export_type([
    http_method/0,
    http_header/0,
    headers_list/0,
    headers_map/0,
    headers/0,
    validation_error/0,
    validation_result/0
]).

%%====================================================================
%% Constants
%%====================================================================

%% Supported MCP protocol versions
-define(SUPPORTED_VERSIONS, [
    <<"2025-11-25">>,
    <<"2024-11-05">>,
    <<"2024-10-01">>,
    <<"2024-09-01">>
]).

%% Default version
-define(DEFAULT_VERSION, <<"2025-11-25">>).

%% Valid content types for requests
-define(VALID_CONTENT_TYPES, [
    <<"application/json">>,
    <<"text/plain">>,
    <<"application/octet-stream">>
]).

%% Valid accept types for responses
-define(VALID_ACCEPT_TYPES, [
    <<"application/json">>,
    <<"text/event-stream">>
]).

%% Session ID format: minimum 32 bytes hex string
-define(MIN_SESSION_ID_LENGTH, 32).

%% HTTP status codes
-define(HTTP_STATUS_OK, 200).
-define(HTTP_STATUS_ACCEPTED, 202).
-define(HTTP_STATUS_BAD_REQUEST, 400).
-define(HTTP_STATUS_UNAUTHORIZED, 401).
-define(HTTP_STATUS_NOT_ACCEPTABLE, 406).
-define(HTTP_STATUS_UNSUPPORTED_MEDIA_TYPE, 415).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate all request headers
%% Validates protocol version, content-type, accept, and session-id headers
%% @param Headers HTTP headers as list or map
%% @param Method HTTP method (get, post, put, etc.)
%% @return {ok, ValidatedHeaders} | {error, ValidationError}
-spec validate_request_headers(headers(), http_method()) -> validation_result().
validate_request_headers(Headers, Method) when is_list(Headers); is_map(Headers) ->
    %% Convert headers to normalized map
    HeadersMap = extract_headers_map(Headers),

    %% Validate protocol version (always required)
    case validate_protocol_version(HeadersMap) of
        {error, Error} ->
            {error, Error};
        {ok, Version} ->
            %% Validate content-type for methods with body
            case validate_content_type_for_method(HeadersMap, Method) of
                {error, Error} ->
                    {error, Error};
                {ok, ContentType} ->
                    %% Validate accept header
                    case validate_accept(HeadersMap) of
                        {error, Error} ->
                            {error, Error};
                        {ok, AcceptFormat} ->
                            %% Validate session ID (if present)
                            case validate_session_id(HeadersMap) of
                                {error, Error} ->
                                    {error, Error};
                                {ok, SessionId} ->
                                    %% Validate authorization (optional)
                                    case validate_authorization(HeadersMap) of
                                        {error, Error} ->
                                            {error, Error};
                                        {ok, Auth} ->
                                            {ok, #{
                                                protocol_version => Version,
                                                content_type => ContentType,
                                                accept_format => AcceptFormat,
                                                session_id => SessionId,
                                                authorization => Auth,
                                                user_agent => extract_user_agent(HeadersMap),
                                                original_headers => HeadersMap
                                            }}
                                    end
                            end
                    end
            end
    end;
validate_request_headers(_Headers, _Method) ->
    {error, {?HTTP_STATUS_BAD_REQUEST, <<"Invalid headers format">>, undefined}}.

%% @doc Validate MCP-Protocol-Version header
%% @param Headers HTTP headers as list or map
%% @return {ok, Version} | {error, {StatusCode, Message, Data}}
-spec validate_protocol_version(headers_map()) -> {ok, binary()} | {error, validation_error()}.
validate_protocol_version(Headers) when is_map(Headers) ->
    HeadersLower = maps:fold(fun(K, V, Acc) ->
        KeyLower = normalize_header_name(K),
        maps:put(KeyLower, V, Acc)
    end, #{}, Headers),

    case maps:get(<<"mcp-protocol-version">>, HeadersLower, undefined) of
        undefined ->
            %% Missing header - use default
            {ok, ?DEFAULT_VERSION};
        Version ->
            validate_version_value(Version)
    end;
validate_protocol_version(Headers) ->
    validate_protocol_version(extract_headers_map(Headers)).

%% @doc Validate Content-Type header for request with body
%% POST/PUT/PATCH require Content-Type: application/json
%% @param Headers HTTP headers map
%% @param Method HTTP method
%% @return {ok, ContentType} | {error, {StatusCode, Message, Data}}
-spec validate_content_type(headers_map(), http_method()) ->
    {ok, binary()} | {error, validation_error()}.
validate_content_type(Headers, Method) ->
    validate_content_type_for_method(Headers, Method).

%% @doc Validate Accept header for content negotiation
%% @param Headers HTTP headers map
%% @return {ok, Format} where Format is json | sse | mixed
-spec validate_accept(headers_map()) -> {ok, atom()} | {error, validation_error()}.
validate_accept(Headers) when is_map(Headers) ->
    HeadersLower = maps:fold(fun(K, V, Acc) ->
        KeyLower = normalize_header_name(K),
        maps:put(KeyLower, V, Acc)
    end, #{}, Headers),

    case maps:get(<<"accept">>, HeadersLower, undefined) of
        undefined ->
            %% Default to JSON if not specified
            {ok, json};
        AcceptValue ->
            validate_accept_value(AcceptValue)
    end;
validate_accept(Headers) ->
    validate_accept(extract_headers_map(Headers)).

%% @doc Validate MCP-Session-Id header
%% Format: 32+ byte hex string or base64 string
%% @param Headers HTTP headers map
%% @return {ok, SessionId} | {error, {StatusCode, Message, Data}} | {ok, undefined}
-spec validate_session_id(headers_map()) ->
    {ok, binary() | undefined} | {error, validation_error()}.
validate_session_id(Headers) when is_map(Headers) ->
    HeadersLower = maps:fold(fun(K, V, Acc) ->
        KeyLower = normalize_header_name(K),
        maps:put(KeyLower, V, Acc)
    end, #{}, Headers),

    case maps:get(<<"mcp-session-id">>, HeadersLower, undefined) of
        undefined ->
            %% Session ID is optional on initial request
            {ok, undefined};
        SessionId ->
            validate_session_id_format(SessionId)
    end;
validate_session_id(Headers) ->
    validate_session_id(extract_headers_map(Headers)).

%% @doc Validate Authorization header (Bearer token)
%% @param Headers HTTP headers map
%% @return {ok, Token} | {ok, undefined} | {error, ValidationError}
-spec validate_authorization(headers_map()) ->
    {ok, binary() | undefined} | {error, validation_error()}.
validate_authorization(Headers) when is_map(Headers) ->
    HeadersLower = maps:fold(fun(K, V, Acc) ->
        KeyLower = normalize_header_name(K),
        maps:put(KeyLower, V, Acc)
    end, #{}, Headers),

    case maps:get(<<"authorization">>, HeadersLower, undefined) of
        undefined ->
            {ok, undefined};
        AuthValue ->
            validate_auth_format(AuthValue)
    end;
validate_authorization(Headers) ->
    validate_authorization(extract_headers_map(Headers)).

%% @doc Extract headers from list format to normalized map
%% Normalizes all header names to lowercase binaries
%% @param Headers HTTP headers as list or map
%% @return Normalized headers map
-spec extract_headers_map(headers()) -> headers_map().
extract_headers_map(Headers) when is_list(Headers) ->
    lists:foldl(fun({Name, Value}, Acc) ->
        NameBin = ensure_binary(Name),
        NameLower = normalize_header_name(NameBin),
        ValueBin = ensure_binary(Value),
        maps:put(NameLower, ValueBin, Acc)
    end, #{}, Headers);
extract_headers_map(Headers) when is_map(Headers) ->
    maps:fold(fun(K, V, Acc) ->
        KeyBin = ensure_binary(K),
        KeyLower = normalize_header_name(KeyBin),
        ValueBin = ensure_binary(V),
        maps:put(KeyLower, ValueBin, Acc)
    end, #{}, Headers);
extract_headers_map(_) ->
    #{}.

%% @doc Format error response for HTTP
%% Creates JSON-RPC error response with proper HTTP status
%% @param StatusCode HTTP status code
%% @param Message Error message
%% @param Data Additional error context
%% @return {StatusCode, Headers, Body}
-spec format_error_response(non_neg_integer(), binary(), map() | undefined) ->
    {non_neg_integer(), [{binary(), binary()}], binary()}.
format_error_response(StatusCode, Message, Data) ->
    Headers = [{<<"content-type">>, <<"application/json">>}],

    ErrorObject = maps:from_list([
        {<<"code">>, error_code_for_status(StatusCode)},
        {<<"message">>, Message}
    ] ++ case Data of
        undefined -> [];
        _ -> [{<<"data">>, Data}]
    end),

    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"error">> => ErrorObject
    },

    Body = jsx:encode(Response),
    {StatusCode, Headers, Body}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Validate content-type based on HTTP method
-spec validate_content_type_for_method(headers_map(), http_method()) ->
    {ok, binary()} | {error, validation_error()}.
validate_content_type_for_method(Headers, Method) ->
    %% Methods that require Content-Type header with body
    MethodsWithBody = [post, put, patch],

    case lists:member(Method, MethodsWithBody) of
        true ->
            %% Must have Content-Type
            HeadersLower = maps:fold(fun(K, V, Acc) ->
                KeyLower = normalize_header_name(K),
                maps:put(KeyLower, V, Acc)
            end, #{}, Headers),

            case maps:get(<<"content-type">>, HeadersLower, undefined) of
                undefined ->
                    %% Missing Content-Type on POST/PUT/PATCH
                    Data = #{
                        <<"method">> => atom_to_binary(Method),
                        <<"error_type">> => <<"missing_content_type">>,
                        <<"expected">> => <<"application/json">>
                    },
                    {error, {?HTTP_STATUS_BAD_REQUEST, <<"Missing Content-Type header">>, Data}};
                ContentType ->
                    validate_content_type_value(ContentType)
            end;
        false ->
            %% GET/HEAD/DELETE/OPTIONS don't require Content-Type
            {ok, undefined}
    end.

%% @doc Validate content-type value
-spec validate_content_type_value(term()) -> {ok, binary()} | {error, validation_error()}.
validate_content_type_value(ContentType) when is_binary(ContentType) ->
    %% Extract media type (before semicolon)
    MediaType = case binary:split(ContentType, <<";">>) of
        [MT | _] -> string:trim(MT);
        [MT] -> string:trim(MT)
    end,

    case lists:member(MediaType, ?VALID_CONTENT_TYPES) of
        true ->
            {ok, MediaType};
        false ->
            Data = #{
                <<"content_type">> => ContentType,
                <<"supported">> => ?VALID_CONTENT_TYPES,
                <<"error_type">> => <<"unsupported_content_type">>
            },
            {error, {?HTTP_STATUS_UNSUPPORTED_MEDIA_TYPE, <<"Invalid Content-Type">>, Data}}
    end;
validate_content_type_value(ContentType) when is_list(ContentType) ->
    validate_content_type_value(erlang:list_to_binary(ContentType));
validate_content_type_value(_) ->
    Data = #{<<"error_type">> => <<"invalid_content_type">>},
    {error, {?HTTP_STATUS_BAD_REQUEST, <<"Invalid Content-Type format">>, Data}}.

%% @doc Validate protocol version value
-spec validate_version_value(term()) -> {ok, binary()} | {error, validation_error()}.
validate_version_value(Version) when is_binary(Version) ->
    case lists:member(Version, ?SUPPORTED_VERSIONS) of
        true ->
            {ok, Version};
        false ->
            Data = #{
                <<"requested">> => Version,
                <<"supported">> => ?SUPPORTED_VERSIONS,
                <<"error_type">> => <<"unsupported_protocol_version">>
            },
            {error, {?HTTP_STATUS_BAD_REQUEST, <<"Unsupported protocol version">>, Data}}
    end;
validate_version_value(Version) when is_list(Version) ->
    validate_version_value(erlang:list_to_binary(Version));
validate_version_value(Version) ->
    Data = #{
        <<"received">> => ensure_binary(Version),
        <<"error_type">> => <<"invalid_protocol_version">>
    },
    {error, {?HTTP_STATUS_BAD_REQUEST, <<"Invalid protocol version format">>, Data}}.

%% @doc Validate Accept header value
-spec validate_accept_value(term()) -> {ok, atom()} | {error, validation_error()}.
validate_accept_value(AcceptValue) when is_binary(AcceptValue) ->
    %% Parse comma-separated accept header
    Parts = binary:split(AcceptValue, <<",">>, [global]),
    check_accept_types(Parts);
validate_accept_value(AcceptValue) when is_list(AcceptValue) ->
    validate_accept_value(erlang:list_to_binary(AcceptValue));
validate_accept_value(_) ->
    {error, {?HTTP_STATUS_BAD_REQUEST, <<"Invalid Accept header format">>, undefined}}.

%% @doc Check if any accept type is supported
-spec check_accept_types([binary()]) -> {ok, atom()} | {error, validation_error()}.
check_accept_types([]) ->
    Data = #{<<"error_type">> => <<"no_acceptable_types">>},
    {error, {?HTTP_STATUS_NOT_ACCEPTABLE, <<"No acceptable content types">>, Data}};
check_accept_types([Type | Rest]) ->
    %% Trim and extract media type (before semicolon for quality factor)
    TypeTrimmed = string:trim(binary_to_list(Type)),
    MediaType = case string:split(TypeTrimmed, ";") of
        [MT | _] -> string:trim(MT);
        [MT] -> MT
    end,

    case MediaType of
        "*/*" ->
            {ok, mixed};
        "application/json" ->
            {ok, json};
        "text/event-stream" ->
            {ok, sse};
        "application/*" ->
            {ok, json};
        _ ->
            check_accept_types(Rest)
    end.

%% @doc Validate session ID format
-spec validate_session_id_format(term()) ->
    {ok, binary()} | {error, validation_error()}.
validate_session_id_format(SessionId) when is_binary(SessionId) ->
    %% Session ID should be at least 32 bytes (hex-encoded or base64)
    case byte_size(SessionId) >= ?MIN_SESSION_ID_LENGTH of
        true ->
            {ok, SessionId};
        false ->
            Data = #{
                <<"session_id">> => SessionId,
                <<"min_length">> => ?MIN_SESSION_ID_LENGTH,
                <<"error_type">> => <<"invalid_session_id_length">>
            },
            {error, {?HTTP_STATUS_BAD_REQUEST, <<"Invalid session ID format">>, Data}}
    end;
validate_session_id_format(SessionId) when is_list(SessionId) ->
    validate_session_id_format(erlang:list_to_binary(SessionId));
validate_session_id_format(_) ->
    Data = #{<<"error_type">> => <<"invalid_session_id_format">>},
    {error, {?HTTP_STATUS_BAD_REQUEST, <<"Invalid session ID format">>, Data}}.

%% @doc Validate authorization header format
-spec validate_auth_format(term()) -> {ok, binary() | undefined} | {error, validation_error()}.
validate_auth_format(AuthValue) when is_binary(AuthValue) ->
    %% Check for Bearer token format
    case binary:split(AuthValue, <<" ">>) of
        [<<"Bearer">>, Token] when byte_size(Token) > 0 ->
            {ok, Token};
        [Scheme, _Token] when is_binary(Scheme) ->
            %% Other schemes accepted, just extract token part
            {ok, AuthValue};
        _ ->
            Data = #{
                <<"scheme">> => AuthValue,
                <<"error_type">> => <<"invalid_auth_format">>
            },
            {error, {?HTTP_STATUS_UNAUTHORIZED, <<"Invalid authorization header">>, Data}}
    end;
validate_auth_format(AuthValue) when is_list(AuthValue) ->
    validate_auth_format(erlang:list_to_binary(AuthValue));
validate_auth_format(_) ->
    {error, {?HTTP_STATUS_BAD_REQUEST, <<"Invalid authorization format">>, undefined}}.

%% @doc Extract User-Agent header
-spec extract_user_agent(headers_map()) -> binary() | undefined.
extract_user_agent(Headers) when is_map(Headers) ->
    HeadersLower = maps:fold(fun(K, V, Acc) ->
        KeyLower = normalize_header_name(K),
        maps:put(KeyLower, V, Acc)
    end, #{}, Headers),
    maps:get(<<"user-agent">>, HeadersLower, undefined);
extract_user_agent(_) ->
    undefined.

%% @doc Normalize header name to lowercase binary
-spec normalize_header_name(term()) -> binary().
normalize_header_name(Name) when is_binary(Name) ->
    string:lowercase(Name);
normalize_header_name(Name) when is_list(Name) ->
    normalize_header_name(erlang:list_to_binary(Name));
normalize_header_name(Name) when is_atom(Name) ->
    normalize_header_name(erlang:atom_to_binary(Name));
normalize_header_name(Name) ->
    normalize_header_name(erlang:term_to_binary(Name)).

%% @doc Ensure value is binary
-spec ensure_binary(term()) -> binary().
ensure_binary(Value) when is_binary(Value) ->
    Value;
ensure_binary(Value) when is_list(Value) ->
    erlang:list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) ->
    erlang:atom_to_binary(Value);
ensure_binary(Value) when is_integer(Value) ->
    erlang:integer_to_binary(Value);
ensure_binary(Value) ->
    erlang:term_to_binary(Value).

%% @doc Get JSON-RPC error code for HTTP status
-spec error_code_for_status(non_neg_integer()) -> integer().
error_code_for_status(?HTTP_STATUS_BAD_REQUEST) ->
    -32600;  % Invalid Request
error_code_for_status(?HTTP_STATUS_UNAUTHORIZED) ->
    -32000;  % Server error
error_code_for_status(?HTTP_STATUS_NOT_ACCEPTABLE) ->
    -32600;  % Invalid Request
error_code_for_status(?HTTP_STATUS_UNSUPPORTED_MEDIA_TYPE) ->
    -32600;  % Invalid Request
error_code_for_status(_) ->
    -32603.  % Internal error
