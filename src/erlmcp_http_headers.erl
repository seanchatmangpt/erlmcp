%%%-------------------------------------------------------------------
%% @doc MCP HTTP Header Validation - Protocol Version & Content Negotiation
%%
%% Implements HTTP header validation per MCP specification:
%% - MCP-Protocol-Version validation
%% - Accept header validation for content negotiation (JSON vs SSE)
%% - Content-Type response header setting
%% - HTTP status code mapping
%% - Method validation (DELETE not supported)
%%
%% Supported MCP versions:
%% - 2025-11-25 (latest)
%% - 2024-11-05 (stable)
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_http_headers).

-include_lib("kernel/include/logger.hrl").

%% API exports
-export([
    validate_protocol_version/1,
    validate_accept_header/1,
    get_response_content_type/1,
    get_http_status_code/1,
    validate_method/1,
    extract_header/2,
    format_response_headers/2
]).

%% Type definitions
-type header_name() :: binary() | string().
-type header_value() :: binary() | string().
-type headers() :: [{header_name(), header_value()}] | map().
-type validation_result() :: {ok, term()} | {error, term()}.
-type response_format() :: json | sse | error.
-type http_status() :: 200 | 202 | 400 | 403 | 404 | 405 | 500.

-export_type([
    header_name/0,
    header_value/0,
    headers/0,
    validation_result/0,
    response_format/0,
    http_status/0
]).

%%====================================================================
%% Constants
%%====================================================================

%% Supported MCP protocol versions
-define(SUPPORTED_VERSIONS, [
    <<"2025-11-25">>,
    <<"2024-11-05">>
]).

%% Default version if not specified
-define(DEFAULT_VERSION, <<"2024-11-05">>).

%% Valid HTTP methods for MCP
-define(VALID_METHODS, [
    <<"GET">>,
    <<"POST">>,
    <<"PUT">>,
    <<"PATCH">>,
    <<"HEAD">>,
    <<"OPTIONS">>,
    get,
    post,
    put,
    patch,
    head,
    options
]).

%% Valid content types for Accept header
-define(VALID_ACCEPT_TYPES, [
    <<"application/json">>,
    <<"text/event-stream">>
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate MCP-Protocol-Version header
%% @param Headers List of header tuples or map
%% @return {ok, Version} if valid, {error, Reason} otherwise
-spec validate_protocol_version(headers()) -> {ok, binary()} | {error, term()}.
validate_protocol_version(Headers) when is_list(Headers) ->
    case extract_header(<<"mcp-protocol-version">>, Headers) of
        {ok, Version} ->
            validate_version_value(Version);
        {error, not_found} ->
            %% Use default version if not specified
            {ok, ?DEFAULT_VERSION};
        {error, Reason} ->
            {error, Reason}
    end;
validate_protocol_version(Headers) when is_map(Headers) ->
    case maps:get(<<"mcp-protocol-version">>, Headers, undefined) of
        undefined ->
            {ok, ?DEFAULT_VERSION};
        Version ->
            validate_version_value(Version)
    end;
validate_protocol_version(_Headers) ->
    {error, invalid_headers_format}.

%% @doc Validate Accept header for content negotiation
%% @param Headers List of header tuples or map
%% @return {ok, Format} where Format is json | sse, {error, Reason} otherwise
-spec validate_accept_header(headers()) -> {ok, response_format()} | {error, term()}.
validate_accept_header(Headers) when is_list(Headers) ->
    case extract_header(<<"accept">>, Headers) of
        {ok, AcceptValue} ->
            validate_accept_value(AcceptValue);
        {error, not_found} ->
            %% Default to JSON if Accept header not specified
            {ok, json};
        {error, Reason} ->
            {error, Reason}
    end;
validate_accept_header(Headers) when is_map(Headers) ->
    case maps:get(<<"accept">>, Headers, undefined) of
        undefined ->
            {ok, json};
        AcceptValue ->
            validate_accept_value(AcceptValue)
    end;
validate_accept_header(_Headers) ->
    {error, invalid_headers_format}.

%% @doc Get response Content-Type header based on format
%% @param Format Response format (json | sse | error)
%% @return Binary content type
-spec get_response_content_type(response_format()) -> binary().
get_response_content_type(json) ->
    <<"application/json">>;
get_response_content_type(sse) ->
    <<"text/event-stream">>;
get_response_content_type(error) ->
    <<"text/plain">>;
get_response_content_type(_) ->
    <<"application/json">>.

%% @doc Get HTTP status code for response type
%% @param Type Response type (ok | accepted | bad_request | forbidden | not_found | method_not_allowed | error)
%% @return HTTP status code
-spec get_http_status_code(atom()) -> http_status().
get_http_status_code(ok) ->
    200;
get_http_status_code(accepted) ->
    202;
get_http_status_code(bad_request) ->
    400;
get_http_status_code(forbidden) ->
    403;
get_http_status_code(not_found) ->
    404;
get_http_status_code(method_not_allowed) ->
    405;
get_http_status_code(error) ->
    500;
get_http_status_code(_) ->
    400.

%% @doc Validate HTTP method
%% @param Method HTTP method as binary, string, or atom
%% @return {ok, Method} if valid, {error, not_allowed} if DELETE, {error, invalid_method} otherwise
-spec validate_method(term()) -> {ok, binary()} | {error, term()}.
validate_method(Method) when is_binary(Method) ->
    MethodUpper = string:uppercase(Method),
    case MethodUpper of
        <<"DELETE">> ->
            {error, method_not_allowed};
        _ ->
            case lists:member(MethodUpper, [M || M <- ?VALID_METHODS, is_binary(M)]) of
                true ->
                    {ok, MethodUpper};
                false ->
                    {error, invalid_method}
            end
    end;
validate_method(Method) when is_list(Method) ->
    validate_method(erlang:list_to_binary(Method));
validate_method(Method) when is_atom(Method) ->
    case atom_to_binary(Method) of
        <<"delete">> ->
            {error, method_not_allowed};
        MethodBin ->
            MethodUpper = string:uppercase(MethodBin),
            case lists:member(MethodUpper, [M || M <- ?VALID_METHODS, is_binary(M)]) of
                true ->
                    {ok, MethodUpper};
                false ->
                    {error, invalid_method}
            end
    end;
validate_method(_) ->
    {error, invalid_method}.

%% @doc Extract header value from list of headers
%% Case-insensitive header name matching
%% @param HeaderName Header name to extract
%% @param Headers List of header tuples
%% @return {ok, Value} if found, {error, not_found} otherwise
-spec extract_header(header_name(), headers()) ->
    {ok, header_value()} | {error, not_found | invalid_headers_format}.
extract_header(Name, Headers) when is_list(Headers) ->
    NameLower = normalize_header_name(Name),
    case lists:search(
        fun({H, _V}) ->
            normalize_header_name(H) =:= NameLower
        end,
        Headers
    ) of
        {value, {_H, Value}} ->
            {ok, ensure_binary(Value)};
        false ->
            {error, not_found}
    end;
extract_header(_Name, _Headers) ->
    {error, invalid_headers_format}.

%% @doc Format response headers with Content-Type
%% @param Format Response format (json | sse | error)
%% @param ExtraHeaders Additional headers to include
%% @return List of header tuples
-spec format_response_headers(response_format(), headers()) -> [{binary(), binary()}].
format_response_headers(Format, ExtraHeaders) when is_list(ExtraHeaders) ->
    ContentType = get_response_content_type(Format),
    BaseHeaders = [{<<"content-type">>, ContentType}],
    merge_headers(BaseHeaders, ExtraHeaders);
format_response_headers(Format, ExtraHeaders) when is_map(ExtraHeaders) ->
    ContentType = get_response_content_type(Format),
    BaseHeaders = [{<<"content-type">>, ContentType}],
    merge_headers(BaseHeaders, maps:to_list(ExtraHeaders));
format_response_headers(Format, _) ->
    [{<<"content-type">>, get_response_content_type(Format)}].

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Validate protocol version value
-spec validate_version_value(term()) -> {ok, binary()} | {error, term()}.
validate_version_value(Version) when is_binary(Version) ->
    case lists:member(Version, ?SUPPORTED_VERSIONS) of
        true ->
            {ok, Version};
        false ->
            {error, {unsupported_protocol_version, Version}}
    end;
validate_version_value(Version) when is_list(Version) ->
    validate_version_value(erlang:list_to_binary(Version));
validate_version_value(Version) ->
    {error, {invalid_protocol_version, Version}}.

%% @doc Validate Accept header value
-spec validate_accept_value(term()) -> {ok, response_format()} | {error, term()}.
validate_accept_value(AcceptValue) when is_binary(AcceptValue) ->
    normalize_accept(AcceptValue);
validate_accept_value(AcceptValue) when is_list(AcceptValue) ->
    normalize_accept(erlang:list_to_binary(AcceptValue));
validate_accept_value(AcceptValue) ->
    {error, {invalid_accept_header, AcceptValue}}.

%% @doc Normalize and validate Accept header
%% Handles wildcards and quality factors
-spec normalize_accept(binary()) -> {ok, response_format()} | {error, term()}.
normalize_accept(AcceptValue) ->
    %% Parse accept header and check for supported types
    %% Format: "type/subtype;q=0.9, type2/subtype2, ..."
    %% Split by comma to handle multiple types
    Types = binary:split(AcceptValue, <<",">>),
    check_accept_types(Types).

%% @doc Check if any of the accept types are supported
-spec check_accept_types([binary()]) -> {ok, response_format()} | {error, term()}.
check_accept_types([]) ->
    %% No supported types found
    {error, {unsupported_content_type, empty}};
check_accept_types([Type | Rest]) ->
    %% Extract media type before semicolon (quality factor)
    TypeTrimmed = string:trim(erlang:binary_to_list(Type)),
    MediaTypeStr = case string:split(TypeTrimmed, ";") of
        [MT | _] -> string:trim(MT)
    end,

    case MediaTypeStr of
        "*/*" ->
            %% Accept any format, default to JSON
            {ok, json};
        "application/json" ->
            {ok, json};
        "text/event-stream" ->
            {ok, sse};
        "application/*" ->
            %% Default to JSON for application/* wildcard
            {ok, json};
        "text/*" ->
            %% Check if text/event-stream is preferred
            {ok, sse};
        _ ->
            %% Try next type
            check_accept_types(Rest)
    end.

%% @doc Normalize header name to lowercase binary
-spec normalize_header_name(term()) -> binary().
normalize_header_name(Name) when is_binary(Name) ->
    string:lowercase(Name);
normalize_header_name(Name) when is_list(Name) ->
    normalize_header_name(erlang:list_to_binary(Name));
normalize_header_name(Name) when is_atom(Name) ->
    normalize_header_name(erlang:atom_to_binary(Name));
normalize_header_name(Name) ->
    string:lowercase(erlang:term_to_binary(Name)).

%% @doc Ensure value is binary
-spec ensure_binary(term()) -> binary().
ensure_binary(Value) when is_binary(Value) ->
    Value;
ensure_binary(Value) when is_list(Value) ->
    erlang:list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) ->
    erlang:atom_to_binary(Value);
ensure_binary(Value) ->
    erlang:term_to_binary(Value).

%% @doc Merge response headers, avoiding duplicates
-spec merge_headers([{binary(), binary()}], [{binary(), binary()}]) ->
    [{binary(), binary()}].
merge_headers(Base, Extra) ->
    %% Remove base headers that are overridden by extra
    BaseKeys = [normalize_header_name(K) || {K, _} <- Base],
    FilteredExtra = [
        {K, V}
     || {K, V} <- Extra,
        not lists:member(normalize_header_name(K), BaseKeys)
    ],
    Base ++ FilteredExtra.
