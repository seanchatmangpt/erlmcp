%%%-------------------------------------------------------------------
%%% @doc erlmcp_http_header_validator - HTTP Header Validation for MCP
%%%
%%% Validates HTTP headers for MCP requests per the specification.
%%% Ensures required headers are present and have valid values.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_http_header_validator).

%% API exports
-export([
    validate_request_headers/2,
    format_error_response/3,
    validate_header_security/2
]).

%% Security constants
-define(MAX_HEADER_SIZE, 8192). %% 8KB max per header
-define(MAX_TOTAL_HEADERS_SIZE, 65536). %% 64KB total headers

%% Types
-type http_method() :: get | post | put | delete | head | options | patch.
-type headers() :: [{binary(), binary()}].
-type validation_result() :: {ok, map()} | {error, {integer(), binary(), term()}}.

-export_type([http_method/0, headers/0, validation_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate HTTP request headers per MCP specification
-spec validate_request_headers(headers(), http_method()) -> validation_result().
validate_request_headers(Headers, Method) when is_list(Headers), is_atom(Method) ->
    %% First, validate security constraints on all headers
    case validate_all_headers_security(Headers) of
        {error, _} = Error ->
            Error;
        ok ->
            %% Convert header list to map for easier lookup
            HeaderMap = maps:from_list([{string:lowercase(K), V} || {K, V} <- Headers]),

            %% Validate based on method
            case Method of
                get ->
                    validate_get_headers(HeaderMap);
                post ->
                    validate_post_headers(HeaderMap);
                delete ->
                    validate_delete_headers(HeaderMap);
                _ ->
                    %% For other methods, do basic validation
                    validate_common_headers(HeaderMap)
            end
    end.

%% @doc Format error response for HTTP header validation failures
-spec format_error_response(integer(), binary(), term()) -> {integer(), [{binary(), binary()}], binary()}.
format_error_response(StatusCode, Message, Data) ->
    Headers = [
        {<<"content-type">>, <<"application/json">>}
    ],
    Body = jsx:encode(#{
        <<"error">> => <<"header_validation_failed">>,
        <<"message">> => Message,
        <<"data">> => format_error_data(Data)
    }),
    {StatusCode, Headers, Body}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Validate GET request headers
validate_get_headers(HeaderMap) ->
    %% GET requests for SSE must have specific headers
    RequiredHeaders = [
        {<<"accept">>, fun validate_accept_header/1}
    ],

    validate_headers(HeaderMap, RequiredHeaders).

%% @private Validate POST request headers
validate_post_headers(HeaderMap) ->
    %% POST requests must have content-type
    RequiredHeaders = [
        {<<"content-type">>, fun validate_content_type_header/1}
    ],

    validate_headers(HeaderMap, RequiredHeaders).

%% @private Validate DELETE request headers
validate_delete_headers(HeaderMap) ->
    %% DELETE requests should have session ID header
    RequiredHeaders = [],
    validate_headers(HeaderMap, RequiredHeaders).

%% @private Validate common headers for all requests
validate_common_headers(HeaderMap) ->
    %% Basic validation
    RequiredHeaders = [],
    validate_headers(HeaderMap, RequiredHeaders).

%% @private Validate headers against required list
validate_headers(HeaderMap, RequiredHeaders) ->
    case validate_required_headers(HeaderMap, RequiredHeaders, []) of
        {ok, ValidatedHeaders} ->
            %% Extract protocol version if present
            ProtocolVersion = maps:get(<<"x-mcp-protocol-version">>, HeaderMap, <<"2025-01-07">>),
            {ok, ValidatedHeaders#{protocol_version => ProtocolVersion}};
        {error, _} = Error ->
            Error
    end.

%% @private Validate required headers
validate_required_headers(_HeaderMap, [], Acc) ->
    {ok, maps:from_list(Acc)};
validate_required_headers(HeaderMap, [{HeaderName, ValidatorFun} | Rest], Acc) ->
    LowerName = string:lowercase(HeaderName),
    case maps:get(LowerName, HeaderMap, undefined) of
        undefined ->
            {error, {400, <<"Missing required header">>, #{header => HeaderName}}};
        Value ->
            case ValidatorFun(Value) of
                {ok, ValidatedValue} ->
                    validate_required_headers(HeaderMap, Rest, [{HeaderName, ValidatedValue} | Acc]);
                {error, Reason} ->
                    {error, {400, <<"Invalid header value">>, #{header => HeaderName, reason => Reason}}}
            end
    end.

%% @private Validate Accept header
validate_accept_header(<<"text/event-stream">>) ->
    {ok, <<"text/event-stream">>};
validate_accept_header(Accept) when is_binary(Accept) ->
    case binary:match(Accept, <<"text/event-stream">>) of
        nomatch ->
            {error, <<"Must accept text/event-stream">>};
        _ ->
            {ok, <<"text/event-stream">>}
    end.

%% @private Validate Content-Type header
validate_content_type_header(ContentType) when is_binary(ContentType) ->
    %% Accept application/json with optional charset
    case ContentType of
        <<"application/json">> ->
            {ok, <<"application/json">>};
        <<"application/json; charset=utf-8">> ->
            {ok, <<"application/json">>};
        <<"application/json; charset=UTF-8">> ->
            {ok, <<"application/json">>};
        _ ->
            {error, <<"Content-Type must be application/json">>}
    end.

%% @private Format error data for JSON response
format_error_data(Data) when is_map(Data) ->
    Data;
format_error_data(Data) when is_binary(Data) ->
    #{message => Data};
format_error_data(Data) ->
    #{message => iolist_to_binary(io_lib:format("~p", [Data]))}.

%%====================================================================
%% Security Validation Functions
%%====================================================================

%% @doc Validate security constraints on a single header
%% Checks for CRLF injection and size limits
-spec validate_header_security(binary(), binary()) -> ok | {error, term()}.
validate_header_security(Name, Value) when is_binary(Name), is_binary(Value) ->
    %% Check header name size
    NameSize = byte_size(Name),
    if
        NameSize > ?MAX_HEADER_SIZE ->
            logger:warning("HTTP header name too large: ~p bytes (max ~p)",
                [NameSize, ?MAX_HEADER_SIZE]),
            {error, {header_name_too_large, #{
                header => Name,
                size => NameSize,
                max_size => ?MAX_HEADER_SIZE
            }}};
        true ->
            ok
    end,

    %% Check header value size
    ValueSize = byte_size(Value),
    if
        ValueSize > ?MAX_HEADER_SIZE ->
            logger:warning("HTTP header value too large for ~s: ~p bytes (max ~p)",
                [Name, ValueSize, ?MAX_HEADER_SIZE]),
            {error, {header_value_too_large, #{
                header => Name,
                size => ValueSize,
                max_size => ?MAX_HEADER_SIZE
            }}};
        true ->
            ok
    end,

    %% Check for CRLF injection in header name
    case binary:match(Name, [<<"\r">>, <<"\n">>]) of
        nomatch ->
            ok;
        _ ->
            logger:error("CRLF injection attempt in header name: ~p", [Name]),
            {error, {crlf_injection_in_name, #{
                header => Name,
                attack_type => <<"crlf_injection">>
            }}}
    end,

    %% Check for CRLF injection in header value
    case binary:match(Value, [<<"\r">>, <<"\n">>]) of
        nomatch ->
            ok;
        _ ->
            logger:error("CRLF injection attempt in header value for ~s: ~p", [Name, Value]),
            {error, {crlf_injection_in_value, #{
                header => Name,
                attack_type => <<"crlf_injection">>,
                value_preview => truncate_for_log(Value, 100)
            }}}
    end.

%% @private Validate security constraints on all headers
-spec validate_all_headers_security(headers()) -> ok | {error, {integer(), binary(), term()}}.
validate_all_headers_security(Headers) ->
    %% Calculate total headers size
    TotalSize = lists:foldl(
        fun({Name, Value}, Acc) ->
            Acc + byte_size(Name) + byte_size(Value)
        end,
        0,
        Headers
    ),

    %% Check total size
    if
        TotalSize > ?MAX_TOTAL_HEADERS_SIZE ->
            logger:warning("Total HTTP headers size too large: ~p bytes (max ~p)",
                [TotalSize, ?MAX_TOTAL_HEADERS_SIZE]),
            {error, {400, <<"Request headers too large">>, #{
                total_size => TotalSize,
                max_size => ?MAX_TOTAL_HEADERS_SIZE
            }}};
        true ->
            %% Validate each header individually
            validate_each_header_security(Headers)
    end.

%% @private Validate security on each header
-spec validate_each_header_security(headers()) -> ok | {error, {integer(), binary(), term()}}.
validate_each_header_security([]) ->
    ok;
validate_each_header_security([{Name, Value} | Rest]) ->
    case validate_header_security(Name, Value) of
        ok ->
            validate_each_header_security(Rest);
        {error, {crlf_injection_in_name, Data}} ->
            {error, {400, <<"CRLF injection detected in header name">>, Data}};
        {error, {crlf_injection_in_value, Data}} ->
            {error, {400, <<"CRLF injection detected in header value">>, Data}};
        {error, {header_name_too_large, Data}} ->
            {error, {431, <<"Request header name too large">>, Data}};
        {error, {header_value_too_large, Data}} ->
            {error, {431, <<"Request header value too large">>, Data}};
        {error, Reason} ->
            {error, {400, <<"Invalid header">>, Reason}}
    end.

%% @private Truncate binary for logging
-spec truncate_for_log(binary(), pos_integer()) -> binary().
truncate_for_log(Bin, MaxLen) when byte_size(Bin) =< MaxLen ->
    Bin;
truncate_for_log(Bin, MaxLen) ->
    <<Truncated:MaxLen/binary, _/binary>> = Bin,
    <<Truncated/binary, "...">>.
