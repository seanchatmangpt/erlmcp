%%%-------------------------------------------------------------------
%%% @doc
%%% Transport Behavior Interface for Erlang MCP (Model Context Protocol)
%%%
%%% This module defines the behavior interface that all transport modules
%%% must implement. It provides a standardized API for different transport
%%% mechanisms including stdio, TCP, HTTP, and WebSocket.
%%%
%%% == Core Responsibilities ==
%%% 
%%% 1. **Transport Abstraction**: Provides a uniform interface across
%%%    different transport mechanisms
%%% 2. **Message Framing**: Handles protocol-specific message framing
%%% 3. **Connection Management**: Manages connection lifecycle and state
%%% 4. **Error Handling**: Provides consistent error reporting
%%%
%%% == Transport Message Flow ==
%%%
%%% ```
%%% Client/Server -> Transport:send/2 -> Network -> Transport:receive
%%%                     |                            |
%%%                     v                            v
%%%               Message Encoding            Message Decoding
%%%                     |                            |
%%%                     v                            v
%%%                Network Protocol         {transport_message, Data}
%%% ```
%%%
%%% == Standard Message Format ==
%%%
%%% All transport messages follow the MCP JSON-RPC 2.0 format:
%%%
%%% ```erlang
%%% #{<<"jsonrpc">> => <<"2.0">>,
%%%   <<"id">> => Id,           % null | binary() | integer()
%%%   <<"method">> => Method,   % binary()
%%%   <<"params">> => Params}   % map() | list() | undefined
%%% ```
%%%
%%% == Implementation Guide ==
%%%
%%% Transport modules must implement the core callbacks:
%%%
%%% ```erlang
%%% -module(my_transport).
%%% -behaviour(erlmcp_transport).
%%%
%%% %% Required callbacks
%%% init(Opts) -> {ok, State} | {error, Reason}.
%%% send(State, Message) -> ok | {error, Reason}.
%%% close(State) -> ok.
%%%
%%% %% Optional callbacks
%%% get_info(State) -> #{atom() => term()}.
%%% handle_transport_call(Request, State) -> 
%%%     {reply, Reply, NewState} | {error, Reason}.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport).

-include("erlmcp.hrl").

%% =============================================================================
%% Type Definitions
%% =============================================================================

%% @doc Transport implementation state
%% This is an opaque term maintained by each transport implementation
-type transport_state() :: term().

%% @doc Transport configuration options
%% Each transport defines its own option schema
-type transport_opts() :: 
    stdio_opts() | tcp_opts() | http_opts() | websocket_opts() | 
    #{atom() => term()}.

%% @doc Standard transport message format
%% All messages sent through transports must follow this structure
-type transport_message() :: #{binary() => term()}.

%% @doc Transport information map
%% Contains metadata about transport capabilities and state
-type transport_info() :: #{
    type => stdio | tcp | http | websocket | custom,
    version => binary(),
    capabilities => [atom()],
    connection_state => connected | disconnected | connecting | error,
    statistics => #{atom() => non_neg_integer()}
}.

%% @doc Transport-specific option types
-type stdio_opts() :: #{
    owner := pid(),
    test_mode => boolean()
}.

-type tcp_opts() :: #{
    host := inet:hostname() | inet:ip_address(),
    port := inet:port_number(),
    owner := pid(),
    connect_timeout => timeout(),
    keepalive => boolean(),
    nodelay => boolean(),
    buffer_size => pos_integer(),
    max_reconnect_attempts => pos_integer() | infinity
}.

-type http_opts() :: #{
    url := binary() | string(),
    owner := pid(),
    method => get | post,
    headers => [{string() | binary(), string() | binary()}],
    timeout => timeout(),
    connect_timeout => timeout(),
    max_retries => non_neg_integer(),
    retry_delay => pos_integer(),
    ssl_options => [ssl:tls_client_option()],
    pool_size => pos_integer()
}.

-type websocket_opts() :: #{
    url := binary() | string(),
    owner := pid(),
    protocols => [binary()],
    headers => [{string() | binary(), string() | binary()}],
    connect_timeout => timeout(),
    ping_interval => timeout(),
    ssl_options => [ssl:tls_client_option()]
}.

%% =============================================================================
%% Callback Definitions
%% =============================================================================

%% @doc Initialize the transport with given options
%% 
%% This callback is called when the transport is started. It should:
%% - Validate the provided options
%% - Initialize the transport-specific state
%% - Establish initial connections if required
%% - Return the initial transport state
%%
%% @param Opts Transport-specific configuration options
%% @returns {ok, State} on success, {error, Reason} on failure
-callback init(Opts :: transport_opts()) -> 
    {ok, State :: transport_state()} | 
    {error, Reason :: term()}.

%% @doc Send a message through the transport
%%
%% This callback handles message transmission. It should:
%% - Encode the message according to transport protocol
%% - Handle transport-specific framing
%% - Manage connection state and errors
%% - Provide appropriate error reporting
%%
%% @param State Current transport state
%% @param Data Message data to send (JSON-encoded binary or iodata)
%% @returns ok on success, {error, Reason} on failure
-callback send(State :: transport_state(), Data :: iodata()) -> 
    ok | 
    {error, Reason :: term()}.

%% @doc Close the transport and clean up resources
%%
%% This callback is called during transport shutdown. It should:
%% - Close any open connections
%% - Clean up resources (sockets, processes, etc.)
%% - Ensure graceful termination
%%
%% @param State Current transport state
%% @returns ok (always succeeds)
-callback close(State :: transport_state()) -> 
    ok.

%% @doc Get transport information and statistics (Optional)
%%
%% This optional callback provides introspection into transport state.
%% Useful for monitoring, debugging, and health checks.
%%
%% @param State Current transport state
%% @returns Map containing transport information
-callback get_info(State :: transport_state()) -> 
    transport_info().

%% @doc Handle transport-specific calls (Optional)
%%
%% This optional callback allows transports to handle custom requests.
%% Useful for transport-specific operations like reconnection, 
%% configuration updates, or advanced features.
%%
%% @param Request Transport-specific request term
%% @param State Current transport state  
%% @returns {reply, Reply, NewState} or {error, Reason}
-callback handle_transport_call(Request :: term(), State :: transport_state()) -> 
    {reply, Reply :: term(), NewState :: transport_state()} |
    {error, Reason :: term()}.

%% Specify optional callbacks
-optional_callbacks([
    get_info/1,
    handle_transport_call/2
]).

%% =============================================================================
%% Type Exports
%% =============================================================================

-export([
    validate_message/1,
    validate_transport_opts/2,
    create_message/3,
    create_notification/2,
    create_response/2,
    create_error_response/4
]).

-export_type([
    transport_state/0,
    transport_opts/0,
    transport_message/0,
    transport_info/0,
    stdio_opts/0,
    tcp_opts/0,
    http_opts/0,
    websocket_opts/0
]).

%% =============================================================================
%% API Functions 
%% =============================================================================

%% @doc Validate a transport message format
%% @param Message The message to validate
%% @returns ok if valid, {error, Reason} if invalid
-spec validate_message(term()) -> ok | {error, term()}.
validate_message(Message) when is_map(Message) ->
    case validate_json_rpc_fields(Message) of
        ok -> validate_message_content(Message);
        Error -> Error
    end;
validate_message(_) ->
    {error, {invalid_message, not_a_map}}.

%% @doc Validate transport options
%% @param Type Transport type
%% @param Opts Options to validate
%% @returns ok if valid, {error, Reason} if invalid
-spec validate_transport_opts(atom(), transport_opts()) -> ok | {error, term()}.
validate_transport_opts(stdio, Opts) ->
    validate_stdio_opts(Opts);
validate_transport_opts(tcp, Opts) ->
    validate_tcp_opts(Opts);
validate_transport_opts(http, Opts) ->
    validate_http_opts(Opts);
validate_transport_opts(websocket, Opts) ->
    validate_websocket_opts(Opts);
validate_transport_opts(Type, _Opts) ->
    {error, {unsupported_transport_type, Type}}.

%% @doc Create a standard transport message
%% @param Method JSON-RPC method name
%% @param Params Method parameters
%% @param Id Request ID (optional)
%% @returns Standard transport message map
-spec create_message(binary(), json_rpc_params(), json_rpc_id()) -> 
    transport_message().
create_message(Method, Params, Id) when is_binary(Method) ->
    Base = #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_METHOD => Method
    },
    WithId = case Id of
        undefined -> Base;
        _ -> Base#{?JSONRPC_FIELD_ID => Id}
    end,
    case Params of
        undefined -> WithId;
        _ -> WithId#{?JSONRPC_FIELD_PARAMS => Params}
    end.

%% @doc Create a notification message (no ID)
%% @param Method JSON-RPC method name
%% @param Params Method parameters
%% @returns Standard notification message map
-spec create_notification(binary(), json_rpc_params()) -> transport_message().
create_notification(Method, Params) ->
    create_message(Method, Params, undefined).

%% @doc Create a response message
%% @param Id Request ID
%% @param Result Response result
%% @returns Standard response message map
-spec create_response(json_rpc_id(), term()) -> transport_message().
create_response(Id, Result) ->
    #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => Id,
        ?JSONRPC_FIELD_RESULT => Result
    }.

%% @doc Create an error response message
%% @param Id Request ID
%% @param Code Error code
%% @param Message Error message
%% @param Data Additional error data (optional)
%% @returns Standard error response message map
-spec create_error_response(json_rpc_id(), integer(), binary(), term()) ->
    transport_message().
create_error_response(Id, Code, Message, Data) ->
    Error = case Data of
        undefined ->
            #{
                ?JSONRPC_ERROR_FIELD_CODE => Code,
                ?JSONRPC_ERROR_FIELD_MESSAGE => Message
            };
        _ ->
            #{
                ?JSONRPC_ERROR_FIELD_CODE => Code,
                ?JSONRPC_ERROR_FIELD_MESSAGE => Message,
                ?JSONRPC_ERROR_FIELD_DATA => Data
            }
    end,
    #{
        ?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
        ?JSONRPC_FIELD_ID => Id,
        ?JSONRPC_FIELD_ERROR => Error
    }.

%% =============================================================================
%% Validation Helper Functions (Private)
%% =============================================================================

%% @private
%% Validate JSON-RPC message fields
-spec validate_json_rpc_fields(map()) -> ok | {error, term()}.
validate_json_rpc_fields(#{?JSONRPC_FIELD_JSONRPC := Version}) ->
    case Version of
        ?JSONRPC_VERSION -> ok;
        _ -> {error, {invalid_jsonrpc_version, Version}}
    end;
validate_json_rpc_fields(_) ->
    {error, missing_jsonrpc_version}.

%% @private  
%% Validate message content structure
-spec validate_message_content(map()) -> ok | {error, term()}.
validate_message_content(#{?JSONRPC_FIELD_METHOD := Method}) 
  when is_binary(Method) ->
    ok;
validate_message_content(#{?JSONRPC_FIELD_RESULT := _}) ->
    ok;
validate_message_content(#{?JSONRPC_FIELD_ERROR := Error}) 
  when is_map(Error) ->
    validate_error_structure(Error);
validate_message_content(_) ->
    {error, invalid_message_structure}.

%% @private
%% Validate error object structure
-spec validate_error_structure(map()) -> ok | {error, term()}.
validate_error_structure(#{?JSONRPC_ERROR_FIELD_CODE := Code,
                          ?JSONRPC_ERROR_FIELD_MESSAGE := Message})
  when is_integer(Code), is_binary(Message) ->
    ok;
validate_error_structure(_) ->
    {error, invalid_error_structure}.

%% @private
%% Validate stdio transport options
-spec validate_stdio_opts(map()) -> ok | {error, term()}.
validate_stdio_opts(#{owner := Owner}) when is_pid(Owner) ->
    ok;
validate_stdio_opts(#{}) ->
    {error, missing_owner_pid};
validate_stdio_opts(_) ->
    {error, invalid_stdio_opts}.

%% @private
%% Validate TCP transport options
-spec validate_tcp_opts(map()) -> ok | {error, term()}.
validate_tcp_opts(#{host := Host, port := Port, owner := Owner})
  when is_pid(Owner), is_integer(Port), Port > 0, Port =< 65535 ->
    case validate_host(Host) of
        ok -> ok;
        Error -> Error
    end;
validate_tcp_opts(#{}) ->
    {error, missing_required_tcp_opts};
validate_tcp_opts(_) ->
    {error, invalid_tcp_opts}.

%% @private
%% Validate HTTP transport options
-spec validate_http_opts(map()) -> ok | {error, term()}.
validate_http_opts(#{url := Url, owner := Owner}) 
  when is_pid(Owner) ->
    case validate_url(Url) of
        ok -> ok;
        Error -> Error
    end;
validate_http_opts(#{}) ->
    {error, missing_required_http_opts};
validate_http_opts(_) ->
    {error, invalid_http_opts}.

%% @private
%% Validate WebSocket transport options  
-spec validate_websocket_opts(map()) -> ok | {error, term()}.
validate_websocket_opts(#{url := Url, owner := Owner})
  when is_pid(Owner) ->
    case validate_ws_url(Url) of
        ok -> ok;
        Error -> Error
    end;
validate_websocket_opts(#{}) ->
    {error, missing_required_websocket_opts};
validate_websocket_opts(_) ->
    {error, invalid_websocket_opts}.

%% @private
%% Validate host specification
-spec validate_host(term()) -> ok | {error, term()}.
validate_host(Host) when is_list(Host) -> ok;
validate_host(Host) when is_binary(Host) -> ok;
validate_host({A, B, C, D}) when 
    is_integer(A), A >= 0, A =< 255,
    is_integer(B), B >= 0, B =< 255,
    is_integer(C), C >= 0, C =< 255,
    is_integer(D), D >= 0, D =< 255 -> ok;
validate_host({A, B, C, D, E, F, G, H}) when
    is_integer(A), A >= 0, A =< 65535,
    is_integer(B), B >= 0, B =< 65535,
    is_integer(C), C >= 0, C =< 65535,
    is_integer(D), D >= 0, D =< 65535,
    is_integer(E), E >= 0, E =< 65535,
    is_integer(F), F >= 0, F =< 65535,
    is_integer(G), G >= 0, G =< 65535,
    is_integer(H), H >= 0, H =< 65535 -> ok;
validate_host(_) -> {error, invalid_host}.

%% @private
%% Validate HTTP URL
-spec validate_url(term()) -> ok | {error, term()}.
validate_url(Url) when is_binary(Url) ->
    validate_url(binary_to_list(Url));
validate_url(Url) when is_list(Url) ->
    case string:prefix(Url, "http://") orelse string:prefix(Url, "https://") of
        nomatch -> {error, invalid_http_url};
        _ -> ok
    end;
validate_url(_) -> {error, invalid_url_format}.

%% @private
%% Validate WebSocket URL
-spec validate_ws_url(term()) -> ok | {error, term()}.
validate_ws_url(Url) when is_binary(Url) ->
    validate_ws_url(binary_to_list(Url));
validate_ws_url(Url) when is_list(Url) ->
    case string:prefix(Url, "ws://") orelse string:prefix(Url, "wss://") of
        nomatch -> {error, invalid_websocket_url};
        _ -> ok
    end;
validate_ws_url(_) -> {error, invalid_url_format}.