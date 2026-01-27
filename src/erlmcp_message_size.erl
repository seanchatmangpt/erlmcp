%%%-------------------------------------------------------------------
%% @doc Message Size Validation for MCP 2025-11-25 Compliance
%%
%% This module implements message size limit validation as per MCP
%% 2025-11-25 specification (Gap #45).
%%
%% Features:
%% - Configurable per-transport message size limits
%% - Default 16 MB limit
%% - Configurable via sys.config
%% - Returns standardized error responses
%% - Support for HTTP 413 Payload Too Large responses
%%
%% Configuration (sys.config):
%% {erlmcp, [
%%     {message_size_limits, #{
%%         default => 16777216,    % 16 MB
%%         http_body => 16777216,  % HTTP POST body
%%         sse_event => 16777216,  % SSE events
%%         websocket => 16777216,  % WebSocket messages
%%         tcp => 16777216,        % TCP messages
%%         stdio => 16777216       % Stdio messages
%%     }}
%% ]}
%%
%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_message_size).

-include("erlmcp.hrl").

%% API
-export([
    get_limit/1,
    validate_message_size/2,
    validate_message_size/3,
    validate_http_body_size/1,
    validate_sse_event_size/1,
    validate_websocket_size/1,
    validate_tcp_size/1,
    validate_stdio_size/1,
    get_max_size_error/1,
    get_http_413_error/0,
    get_size_limit_config/0
]).

%% Types
-export_type([transport_type/0, validation_result/0]).

-type transport_type() :: http | sse | websocket | tcp | stdio | default.
-type validation_result() :: ok | {error, {atom(), binary()}}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Get the configured message size limit for a transport type
%% Returns the limit in bytes, or the default 16 MB if not configured
-spec get_limit(transport_type()) -> non_neg_integer().
get_limit(Transport) ->
    Config = get_size_limit_config(),
    case Transport of
        http -> maps:get(http_body, Config, ?MCP_DEFAULT_HTTP_BODY_SIZE_LIMIT);
        sse -> maps:get(sse_event, Config, ?MCP_DEFAULT_SSE_EVENT_SIZE_LIMIT);
        websocket -> maps:get(websocket, Config, ?MCP_DEFAULT_WS_MESSAGE_SIZE_LIMIT);
        tcp -> maps:get(tcp, Config, ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT);
        stdio -> maps:get(stdio, Config, ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT);
        default -> maps:get(default, Config, ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT)
    end.

%% @doc Validate message size with transport type
%% Returns ok if size is within limits, {error, Reason} otherwise
-spec validate_message_size(transport_type(), binary()) -> validation_result().
validate_message_size(Transport, Message) ->
    Size = byte_size(Message),
    Limit = get_limit(Transport),
    case Size > Limit of
        true ->
            {error, {message_too_large, get_max_size_error(Limit)}};
        false ->
            ok
    end.

%% @doc Validate message size with custom limit
-spec validate_message_size(binary(), non_neg_integer(), transport_type()) -> validation_result().
validate_message_size(Message, CustomLimit, Transport) when is_binary(Message), is_integer(CustomLimit), CustomLimit > 0 ->
    Size = byte_size(Message),
    case Size > CustomLimit of
        true ->
            {error, {message_too_large, get_max_size_error(CustomLimit)}};
        false ->
            ok
    end;
validate_message_size(Message, CustomLimit, Transport) ->
    validate_message_size(Transport, Message).

%% @doc Validate HTTP POST body size
-spec validate_http_body_size(binary()) -> validation_result().
validate_http_body_size(Body) when is_binary(Body) ->
    validate_message_size(http, Body).

%% @doc Validate SSE event size
-spec validate_sse_event_size(binary()) -> validation_result().
validate_sse_event_size(Event) when is_binary(Event) ->
    validate_message_size(sse, Event).

%% @doc Validate WebSocket message size
-spec validate_websocket_size(binary()) -> validation_result().
validate_websocket_size(Message) when is_binary(Message) ->
    validate_message_size(websocket, Message).

%% @doc Validate TCP message size
-spec validate_tcp_size(binary()) -> validation_result().
validate_tcp_size(Message) when is_binary(Message) ->
    validate_message_size(tcp, Message).

%% @doc Validate Stdio message size
-spec validate_stdio_size(binary()) -> validation_result().
validate_stdio_size(Message) when is_binary(Message) ->
    validate_message_size(stdio, Message).

%% @doc Generate JSON-RPC error response for oversized message
%% Returns JSON-encoded error response
-spec get_max_size_error(non_neg_integer()) -> binary().
get_max_size_error(MaxSize) ->
    Data = #{
        <<"maxSize">> => MaxSize,
        <<"unit">> => <<"bytes">>,
        <<"maxSizeReadable">> => format_size(MaxSize)
    },
    erlmcp_json_rpc:encode_error_response(
        null,
        ?MCP_ERROR_MESSAGE_TOO_LARGE,
        ?MCP_MSG_MESSAGE_TOO_LARGE,
        Data
    ).

%% @doc Generate HTTP 413 Payload Too Large error response
%% Returns formatted error message suitable for HTTP response
-spec get_http_413_error() -> {http, binary()}.
get_http_413_error() ->
    {http, <<"Payload Too Large - Message size exceeds maximum allowed">>}.

%% @doc Get the complete message size limit configuration
-spec get_size_limit_config() -> map().
get_size_limit_config() ->
    case application:get_env(erlmcp, message_size_limits, undefined) of
        undefined ->
            %% Return defaults if not configured
            #{
                default => ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT,
                http_body => ?MCP_DEFAULT_HTTP_BODY_SIZE_LIMIT,
                sse_event => ?MCP_DEFAULT_SSE_EVENT_SIZE_LIMIT,
                websocket => ?MCP_DEFAULT_WS_MESSAGE_SIZE_LIMIT,
                tcp => ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT,
                stdio => ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT
            };
        Config when is_map(Config) ->
            %% Merge with defaults for missing keys
            Defaults = #{
                default => ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT,
                http_body => ?MCP_DEFAULT_HTTP_BODY_SIZE_LIMIT,
                sse_event => ?MCP_DEFAULT_SSE_EVENT_SIZE_LIMIT,
                websocket => ?MCP_DEFAULT_WS_MESSAGE_SIZE_LIMIT,
                tcp => ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT,
                stdio => ?MCP_DEFAULT_MESSAGE_SIZE_LIMIT
            },
            maps:merge(Defaults, Config)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Format byte size in human-readable format
-spec format_size(non_neg_integer()) -> binary().
format_size(Bytes) when is_integer(Bytes), Bytes < 0 ->
    <<"0 B">>;
format_size(Bytes) when is_integer(Bytes), Bytes < 1024 ->
    <<(erlang:integer_to_binary(Bytes))/binary, " B">>;
format_size(Bytes) when is_integer(Bytes), Bytes < 1024 * 1024 ->
    KB = Bytes / 1024,
    <<(erlang:float_to_binary(KB, [{decimals, 2}]))/binary, " KB">>;
format_size(Bytes) when is_integer(Bytes), Bytes < 1024 * 1024 * 1024 ->
    MB = Bytes / (1024 * 1024),
    <<(erlang:float_to_binary(MB, [{decimals, 2}]))/binary, " MB">>;
format_size(Bytes) when is_integer(Bytes) ->
    GB = Bytes / (1024 * 1024 * 1024),
    <<(erlang:float_to_binary(GB, [{decimals, 2}]))/binary, " GB">>.
