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
    stdio_opts() | tcp_opts() | http_opts() | websocket_opts() | #{atom() => term()}.
%% @doc Standard transport message format
%% All messages sent through transports must follow this structure.
%% This represents a JSON-RPC 2.0 message as a map with binary keys.
%%
%% Examples:
%% - Request: #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
%%              <<"method">> => <<"initialize">>, <<"params">> => #{}}
%% - Response: #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
%%               <<"result">> => #{<<"version">> => <<"1.0">>}}
%% - Error: #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1,
%%            <<"error">> => #{<<"code">> => -32602, <<"message">> => <<"Invalid params">>}}
%% - Notification: #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"initialized">>,
%%                   <<"params">> => #{}}
-type transport_message() :: #{binary() => term()}.
%% @doc Standard transport messages sent to the registry
%% These are the message types that transports send to notify about events.
%%
%% Examples:
%% - {transport_data, <<"{'jsonrpc':'2.0','method':'ping'}">>}
%% - {transport_connected, #{peer => "127.0.0.1:8080", protocol => "HTTP/1.1"}}
%% - {transport_disconnected, connection_closed}
%% - {transport_error, timeout, {connect_timeout, 5000}}
-type transport_event_message() ::
    {transport_data, Data :: binary()} |
    {transport_connected, Info :: map()} |
    {transport_disconnected, Reason :: term()} |
    {transport_error, Type :: atom(), Reason :: term()}.
%% @doc Transport information map
%% Contains metadata about transport capabilities and state
-type transport_info() ::
    #{type => stdio | tcp | http | websocket | custom,
      status => connected | disconnected | connecting | error,
      peer => term(),
      version => binary(),
      capabilities => [atom()],
      connection_state => connected | disconnected | connecting | error,
      statistics => #{atom() => non_neg_integer()},
      transport_id => atom(),
      started_at => non_neg_integer(),
      last_activity => non_neg_integer()}.
%% @doc Transport-specific option types
-type stdio_opts() :: #{owner := pid(), test_mode => boolean()}.
-type tcp_opts() ::
    #{host := inet:hostname() | inet:ip_address(),
      port := inet:port_number(),
      owner := pid(),
      connect_timeout => timeout(),
      keepalive => boolean(),
      nodelay => boolean(),
      buffer_size => pos_integer(),
      max_reconnect_attempts => pos_integer() | infinity}.
-type http_opts() ::
    #{url := binary() | string(),
      owner := pid(),
      method => get | post,
      headers => [{string() | binary(), string() | binary()}],
      timeout => timeout(),
      connect_timeout => timeout(),
      max_retries => non_neg_integer(),
      retry_delay => pos_integer(),
      ssl_options => [ssl:tls_client_option()],
      pool_size => pos_integer()}.
-type websocket_opts() ::
    #{url := binary() | string(),
      owner := pid(),
      protocols => [binary()],
      headers => [{string() | binary(), string() | binary()}],
      connect_timeout => timeout(),
      ping_interval => timeout(),
      ssl_options => [ssl:tls_client_option()]}.

%% =============================================================================
%% Callback Definitions
%% =============================================================================

%% @doc Initialize the transport with transport ID and configuration (recommended)
%%
%% This callback is called when the transport is started. It should:
%% - Validate the provided options
%% - Initialize the transport-specific state
%% - Establish initial connections if required
%% - Return the initial transport state
%%
%% @param TransportId Unique identifier for this transport instance
%% @param Config Transport-specific configuration options
%% @returns {ok, State} on success, {error, Reason} on failure
-callback init(TransportId :: atom(), Config :: transport_opts()) ->
                  {ok, State :: transport_state()} | {error, Reason :: term()}.
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
                  ok | {error, Reason :: term()}.
%% @doc Close the transport and clean up resources
%%
%% This callback is called during transport shutdown. It should:
%% - Close any open connections
%% - Clean up resources (sockets, processes, etc.)
%% - Ensure graceful termination
%%
%% @param State Current transport state
%% @returns ok (always succeeds)
-callback close(State :: transport_state()) -> ok.
%% @doc Get transport information and statistics (Optional)
%%
%% This optional callback provides introspection into transport state.
%% Useful for monitoring, debugging, and health checks.
%%
%% The returned information should include:
%% - type: Transport type (stdio, tcp, http, websocket, custom)
%% - status: Current connection status
%% - peer: Peer information (if applicable)
%% - version: Transport version/protocol information
%% - capabilities: List of supported features
%% - statistics: Performance metrics and counters
%% - transport_id: Unique transport identifier
%% - started_at: Timestamp when transport was initialized
%% - last_activity: Timestamp of last message activity
%%
%% @param State Current transport state
%% @returns Map containing transport information
-callback get_info(State :: transport_state()) -> transport_info().
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
%% New implementations should use init/2
-optional_callbacks([get_info/1, handle_transport_call/2]).

%% Note: init/2 callback already defined above

%% =============================================================================
%% Type Exports
%% =============================================================================

-export_type([transport_state/0, transport_opts/0, transport_message/0,
              transport_event_message/0, transport_info/0, stdio_opts/0, tcp_opts/0, http_opts/0,
              websocket_opts/0]).

%% =============================================================================
%% Function Exports
%% =============================================================================

-export([validate_message/1, validate_transport_opts/2, validate_transport_config/3,
         validate_transport_event_message/1, create_message/3, create_notification/2,
         create_response/2, create_error_response/4, create_transport_data_message/1,
         create_transport_connected_message/1, create_transport_disconnected_message/1,
         create_transport_error_message/2, create_transport_info/2, create_transport_info/3,
         update_transport_activity/1, validate_transport_info/1, update_transport_statistics/3,
         increment_transport_errors/1, update_transport_status/2, validate_transport_behavior/1,
         create_standard_transport_state/2, register_transport_with_registry/3,
         unregister_transport_from_registry/1, handle_registry_message/2,
         validate_json_rpc_request/1, validate_json_rpc_response/1,
         validate_json_rpc_notification/1, extract_message_type/1, is_request/1, is_response/1,
         is_notification/1, is_error_response/1]).

%% =============================================================================
%% API Functions
%% =============================================================================

%% @doc Validate a transport message format
%% @param Message The message to validate
%% @returns ok if valid, {error, Reason} if invalid
-spec validate_message(term()) -> ok | {error, term()}.
validate_message(Message) when is_map(Message) ->
    case validate_json_rpc_fields(Message) of
        ok ->
            validate_message_content(Message);
        Error ->
            Error
    end;
validate_message(_) ->
    {error, {invalid_message, not_a_map}}.

%% @doc Validate a transport event message format
%% @param Message The transport event message to validate
%% @returns ok if valid, {error, Reason} if invalid
-spec validate_transport_event_message(transport_event_message()) -> ok | {error, term()}.
validate_transport_event_message({transport_data, Data}) when is_binary(Data) ->
    ok;
validate_transport_event_message({transport_connected, Info}) when is_map(Info) ->
    ok;
validate_transport_event_message({transport_disconnected, _Reason}) ->
    ok;
validate_transport_event_message({transport_error, Type, _Reason}) when is_atom(Type) ->
    ok;
validate_transport_event_message(_) ->
    {error, invalid_transport_event_message}.

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

%% @doc Validate transport configuration for a specific transport ID
%% @param TransportId Unique transport identifier
%% @param Type Transport type
%% @param Config Configuration map
%% @returns ok if valid, {error, Reason} if invalid
-spec validate_transport_config(atom(), atom(), map()) -> ok | {error, term()}.
validate_transport_config(_TransportId, Type, Config) when is_map(Config) ->
    validate_transport_opts(Type, Config);
validate_transport_config(_TransportId, _Type, _Config) ->
    {error, invalid_config_format}.

%% @doc Create a standard transport message
%% @param Method JSON-RPC method name
%% @param Params Method parameters
%% @param Id Request ID (optional)
%% @returns Standard transport message map
-spec create_message(binary(), json_rpc_params(), json_rpc_id()) -> transport_message().
create_message(Method, Params, Id) when is_binary(Method) ->
    Base = #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION, ?JSONRPC_FIELD_METHOD => Method},
    WithId =
        case Id of
            undefined ->
                Base;
            _ ->
                Base#{?JSONRPC_FIELD_ID => Id}
        end,
    case Params of
        undefined ->
            WithId;
        _ ->
            WithId#{?JSONRPC_FIELD_PARAMS => Params}
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
    #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
      ?JSONRPC_FIELD_ID => Id,
      ?JSONRPC_FIELD_RESULT => Result}.

%% @doc Create an error response message
%% @param Id Request ID
%% @param Code Error code
%% @param Message Error message
%% @param Data Additional error data (optional)
%% @returns Standard error response message map
-spec create_error_response(json_rpc_id(), integer(), binary(), term()) ->
                               transport_message().
create_error_response(Id, Code, Message, Data) ->
    Error =
        case Data of
            undefined ->
                #{?JSONRPC_ERROR_FIELD_CODE => Code, ?JSONRPC_ERROR_FIELD_MESSAGE => Message};
            _ ->
                #{?JSONRPC_ERROR_FIELD_CODE => Code,
                  ?JSONRPC_ERROR_FIELD_MESSAGE => Message,
                  ?JSONRPC_ERROR_FIELD_DATA => Data}
        end,
    #{?JSONRPC_FIELD_JSONRPC => ?JSONRPC_VERSION,
      ?JSONRPC_FIELD_ID => Id,
      ?JSONRPC_FIELD_ERROR => Error}.

%% @doc Create a transport data event message
%% @param Data Binary data received from transport
%% @returns Transport data event message
-spec create_transport_data_message(binary()) -> transport_event_message().
create_transport_data_message(Data) when is_binary(Data) ->
    {transport_data, Data}.

%% @doc Create a transport connected event message
%% @param Info Connection information map
%% @returns Transport connected event message
-spec create_transport_connected_message(map()) -> transport_event_message().
create_transport_connected_message(Info) when is_map(Info) ->
    {transport_connected, Info}.

%% @doc Create a transport disconnected event message
%% @param Reason Disconnection reason
%% @returns Transport disconnected event message
-spec create_transport_disconnected_message(term()) -> transport_event_message().
create_transport_disconnected_message(Reason) ->
    {transport_disconnected, Reason}.

%% @doc Create a transport error event message
%% @param Type Error type atom
%% @param Reason Error reason
%% @returns Transport error event message
-spec create_transport_error_message(atom(), term()) -> transport_event_message().
create_transport_error_message(Type, Reason) when is_atom(Type) ->
    {transport_error, Type, Reason}.

%% @doc Create a basic transport info structure
%% @param Type Transport type
%% @param TransportId Unique transport identifier
%% @returns Basic transport info map
-spec create_transport_info(atom(), atom()) -> transport_info().
create_transport_info(Type, TransportId) ->
    Now = erlang:system_time(millisecond),
    #{type => Type,
      status => connecting,
      peer => undefined,
      version => <<"1.0">>,
      capabilities => [],
      connection_state => connecting,
      statistics =>
          #{messages_sent => 0,
            messages_received => 0,
            bytes_sent => 0,
            bytes_received => 0,
            errors => 0},
      transport_id => TransportId,
      started_at => Now,
      last_activity => Now}.

%% @doc Create a transport info structure with additional options
%% @param Type Transport type
%% @param TransportId Unique transport identifier
%% @param Opts Additional options map
%% @returns Enhanced transport info map
-spec create_transport_info(atom(), atom(), map()) -> transport_info().
create_transport_info(Type, TransportId, Opts) when is_map(Opts) ->
    BaseInfo = create_transport_info(Type, TransportId),
    maps:merge(BaseInfo, Opts).

%% @doc Update transport info with current activity timestamp
%% @param Info Current transport info map
%% @returns Updated transport info map with current timestamp
-spec update_transport_activity(transport_info()) -> transport_info().
update_transport_activity(Info) when is_map(Info) ->
    Info#{last_activity => erlang:system_time(millisecond)}.

%% @doc Increment transport message statistics
%% @param Info Current transport info map
%% @param Type Type of message ('sent' or 'received')
%% @param Size Message size in bytes
%% @returns Updated transport info map with incremented statistics
-spec update_transport_statistics(transport_info(), sent | received, non_neg_integer()) ->
                                     transport_info().
update_transport_statistics(Info, Type, Size) when is_map(Info) ->
    Stats = maps:get(statistics, Info, #{}),
    UpdatedStats =
        case Type of
            sent ->
                Stats#{messages_sent => maps:get(messages_sent, Stats, 0) + 1,
                       bytes_sent => maps:get(bytes_sent, Stats, 0) + Size};
            received ->
                Stats#{messages_received => maps:get(messages_received, Stats, 0) + 1,
                       bytes_received => maps:get(bytes_received, Stats, 0) + Size}
        end,
    UpdatedInfo = Info#{statistics => UpdatedStats},
    update_transport_activity(UpdatedInfo).

%% @doc Increment transport error count
%% @param Info Current transport info map
%% @returns Updated transport info map with incremented error count
-spec increment_transport_errors(transport_info()) -> transport_info().
increment_transport_errors(Info) when is_map(Info) ->
    Stats = maps:get(statistics, Info, #{}),
    UpdatedStats = Stats#{errors => maps:get(errors, Stats, 0) + 1},
    Info#{statistics => UpdatedStats}.

%% @doc Update transport connection status
%% @param Info Current transport info map
%% @param Status New connection status
%% @returns Updated transport info map with new status
-spec update_transport_status(transport_info(), atom()) -> transport_info().
update_transport_status(Info, Status) when is_map(Info) ->
    UpdatedInfo = Info#{status => Status, connection_state => Status},
    update_transport_activity(UpdatedInfo).

%% @doc Validate transport info structure
%% @param Info Transport info map to validate
%% @returns ok if valid, {error, Reason} if invalid
-spec validate_transport_info(transport_info()) -> ok | {error, term()}.
validate_transport_info(Info) when is_map(Info) ->
    RequiredFields = [type, status, transport_id, started_at, last_activity],
    case validate_required_info_fields(Info, RequiredFields) of
        ok ->
            validate_info_field_values(Info);
        Error ->
            Error
    end;
validate_transport_info(_) ->
    {error, invalid_transport_info_format}.

%% =============================================================================
%% Validation Helper Functions (Private)
%% =============================================================================

%% @private
%% Validate JSON-RPC message fields
-spec validate_json_rpc_fields(map()) -> ok | {error, term()}.
validate_json_rpc_fields(#{?JSONRPC_FIELD_JSONRPC := Version}) ->
    case Version of
        ?JSONRPC_VERSION ->
            ok;
        _ ->
            {error, {invalid_jsonrpc_version, Version}}
    end;
validate_json_rpc_fields(_) ->
    {error, missing_jsonrpc_version}.

%% @private
%% Validate message content structure
-spec validate_message_content(map()) -> ok | {error, term()}.
validate_message_content(#{?JSONRPC_FIELD_METHOD := Method}) when is_binary(Method) ->
    ok;
validate_message_content(#{?JSONRPC_FIELD_RESULT := _}) ->
    ok;
validate_message_content(#{?JSONRPC_FIELD_ERROR := Error}) when is_map(Error) ->
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
validate_tcp_opts(#{host := Host,
                    port := Port,
                    owner := Owner})
    when is_pid(Owner), is_integer(Port), Port > 0, Port =< 65535 ->
    case validate_host(Host) of
        ok ->
            ok;
        Error ->
            Error
    end;
validate_tcp_opts(#{}) ->
    {error, missing_required_tcp_opts};
validate_tcp_opts(_) ->
    {error, invalid_tcp_opts}.

%% @private
%% Validate HTTP transport options
-spec validate_http_opts(map()) -> ok | {error, term()}.
validate_http_opts(#{url := Url, owner := Owner}) when is_pid(Owner) ->
    case validate_url(Url) of
        ok ->
            ok;
        Error ->
            Error
    end;
validate_http_opts(#{}) ->
    {error, missing_required_http_opts};
validate_http_opts(_) ->
    {error, invalid_http_opts}.

%% @private
%% Validate WebSocket transport options
-spec validate_websocket_opts(map()) -> ok | {error, term()}.
validate_websocket_opts(#{url := Url, owner := Owner}) when is_pid(Owner) ->
    case validate_ws_url(Url) of
        ok ->
            ok;
        Error ->
            Error
    end;
validate_websocket_opts(#{}) ->
    {error, missing_required_websocket_opts};
validate_websocket_opts(_) ->
    {error, invalid_websocket_opts}.

%% @private
%% Validate host specification
-spec validate_host(term()) -> ok | {error, term()}.
validate_host(Host) when is_list(Host) ->
    ok;
validate_host(Host) when is_binary(Host) ->
    ok;
validate_host({A, B, C, D})
    when is_integer(A), A >= 0, A =< 255, is_integer(B), B >= 0, B =< 255, is_integer(C),
         C >= 0, C =< 255, is_integer(D), D >= 0, D =< 255 ->
    ok;
validate_host({A, B, C, D, E, F, G, H})
    when is_integer(A), A >= 0, A =< 65535, is_integer(B), B >= 0, B =< 65535, is_integer(C),
         C >= 0, C =< 65535, is_integer(D), D >= 0, D =< 65535, is_integer(E), E >= 0, E =< 65535,
         is_integer(F), F >= 0, F =< 65535, is_integer(G), G >= 0, G =< 65535, is_integer(H),
         H >= 0, H =< 65535 ->
    ok;
validate_host(_) ->
    {error, invalid_host}.

%% @private
%% Validate HTTP URL
-spec validate_url(term()) -> ok | {error, term()}.
validate_url(Url) when is_binary(Url) ->
    validate_url(binary_to_list(Url));
validate_url(Url) when is_list(Url) ->
    case string:prefix(Url, "http://") orelse string:prefix(Url, "https://") of
        nomatch ->
            {error, invalid_http_url};
        _ ->
            ok
    end;
validate_url(_) ->
    {error, invalid_url_format}.

%% @private
%% Validate WebSocket URL
-spec validate_ws_url(term()) -> ok | {error, term()}.
validate_ws_url(Url) when is_binary(Url) ->
    validate_ws_url(binary_to_list(Url));
validate_ws_url(Url) when is_list(Url) ->
    case string:prefix(Url, "ws://") orelse string:prefix(Url, "wss://") of
        nomatch ->
            {error, invalid_websocket_url};
        _ ->
            ok
    end;
validate_ws_url(_) ->
    {error, invalid_url_format}.

%% @private
%% Validate required transport info fields
-spec validate_required_info_fields(map(), [atom()]) -> ok | {error, term()}.
validate_required_info_fields(_Info, []) ->
    ok;
validate_required_info_fields(Info, [Field | Rest]) ->
    case maps:is_key(Field, Info) of
        true ->
            validate_required_info_fields(Info, Rest);
        false ->
            {error, {missing_transport_info_field, Field}}
    end.

%% @private
%% Validate transport info field values
-spec validate_info_field_values(map()) -> ok | {error, term()}.
validate_info_field_values(#{type := Type,
                             status := Status,
                             transport_id := Id,
                             started_at := Started,
                             last_activity := Activity}) ->
    case validate_transport_type(Type) of
        ok ->
            case validate_transport_status(Status) of
                ok ->
                    case is_atom(Id) of
                        true ->
                            case is_integer(Started)
                                 andalso is_integer(Activity)
                                 andalso Started > 0
                                 andalso Activity > 0
                            of
                                true ->
                                    ok;
                                false ->
                                    {error, invalid_timestamp}
                            end;
                        false ->
                            {error, invalid_transport_id}
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
validate_info_field_values(_) ->
    {error, incomplete_transport_info}.

%% @private
%% Validate transport type
-spec validate_transport_type(atom()) -> ok | {error, term()}.
validate_transport_type(Type)
    when Type =:= stdio; Type =:= tcp; Type =:= http; Type =:= websocket; Type =:= custom ->
    ok;
validate_transport_type(_) ->
    {error, invalid_transport_type}.

%% @private
%% Validate transport status
-spec validate_transport_status(atom()) -> ok | {error, term()}.
validate_transport_status(Status)
    when Status =:= connected;
         Status =:= disconnected;
         Status =:= connecting;
         Status =:= error ->
    ok;
validate_transport_status(_) ->
    {error, invalid_transport_status}.

%% =============================================================================
%% Standard Transport Behavior Utilities
%% =============================================================================

%% @doc Check if a module implements the transport behavior correctly
%% @param Module The module to check
%% @returns ok if valid implementation, {error, Reason} if issues found
-spec validate_transport_behavior(module()) -> ok | {error, term()}.
validate_transport_behavior(Module) when is_atom(Module) ->
    RequiredCallbacks = [{init, 2}, {send, 2}, {close, 1}],
    OptionalCallbacks = [{get_info, 1}, {handle_transport_call, 2}],

    %% Check required callbacks
    case check_callbacks_exported(Module, RequiredCallbacks) of
        ok ->
            %% Check if optional callbacks are declared properly
            check_optional_callbacks(Module, OptionalCallbacks);
        Error ->
            Error
    end;
validate_transport_behavior(_) ->
    {error, invalid_module}.

%% @doc Create a standard transport state record template
%% @param TransportId Unique transport identifier
%% @param Config Transport configuration
%% @returns Standard transport state map
-spec create_standard_transport_state(atom(), map()) -> map().
create_standard_transport_state(TransportId, Config) ->
    #{transport_id => TransportId,
      server_id => maps:get(server_id, Config, undefined),
      config => Config,
      connection_state => connecting,
      buffer => <<>>,
      statistics =>
          #{messages_sent => 0,
            messages_received => 0,
            bytes_sent => 0,
            bytes_received => 0,
            errors => 0},
      started_at => erlang:system_time(millisecond),
      last_activity => erlang:system_time(millisecond)}.

%% @doc Standard registry registration helper
%% @param TransportId Unique transport identifier
%% @param Pid Transport process PID
%% @param Config Transport configuration
%% @returns ok on success, {error, Reason} on failure
-spec register_transport_with_registry(atom(), pid(), map()) -> ok | {error, term()}.
register_transport_with_registry(TransportId, Pid, Config)
    when is_atom(TransportId), is_pid(Pid) ->
    TransportConfig = Config#{pid => Pid, registered_at => erlang:system_time(millisecond)},
    try
        erlmcp_registry:register_transport(TransportId, Pid, TransportConfig)
    catch
        error:undef ->
            {error, registry_not_available};
        Class:Reason:Stack ->
            {error, {registry_error, {Class, Reason, Stack}}}
    end;
register_transport_with_registry(_, _, _) ->
    {error, invalid_parameters}.

%% @doc Standard registry unregistration helper
%% @param TransportId Unique transport identifier
%% @returns ok (always succeeds)
-spec unregister_transport_from_registry(atom()) -> ok.
unregister_transport_from_registry(TransportId) when is_atom(TransportId) ->
    try
        erlmcp_registry:unregister_transport(TransportId),
        ok
    catch
        _:_ ->
            ok  % Always succeed - registry might not be available
    end.

%% @doc Handle standard transport messages from registry
%% @param Message The message received from registry
%% @param State Current transport state
%% @returns {ok, NewState} or {error, Reason, State}
-spec handle_registry_message(term(), map()) -> {ok, map()} | {error, term(), map()}.
handle_registry_message({mcp_response, ServerId, Data}, State) ->
    case maps:get(server_id, State, undefined) of
        ServerId ->
            %% Forward to transport's send implementation
            _TransportId = maps:get(transport_id, State),
            try
                %% This would call the transport's send/2 callback
                case apply_transport_callback(send, [State, Data]) of
                    ok ->
                        NewStats = update_send_statistics(maps:get(statistics, State, #{})),
                        {ok, State#{statistics => NewStats}};
                    {error, Reason} ->
                        NewStats = increment_error_statistics(maps:get(statistics, State, #{})),
                        {error, {send_failed, Reason}, State#{statistics => NewStats}}
                end
            catch
                Class:CReason:CStack ->
                    {error, {transport_callback_error, {Class, CReason, CStack}}, State}
            end;
        _ ->
            {error, {wrong_server_id, ServerId}, State}
    end;
handle_registry_message(UnknownMessage, State) ->
    {error, {unknown_registry_message, UnknownMessage}, State}.

%% =============================================================================
%% Private Helper Functions for Transport Behavior Validation
%% =============================================================================

%% @private
%% Check if required callbacks are exported
-spec check_callbacks_exported(module(), [{atom(), arity()}]) -> ok | {error, term()}.
check_callbacks_exported(Module, Callbacks) ->
    Exports =
        try
            Module:module_info(exports)
        catch
            _:_ ->
                []
        end,
    MissingCallbacks =
        [{Fun, Arity} || {Fun, Arity} <- Callbacks, not lists:member({Fun, Arity}, Exports)],
    case MissingCallbacks of
        [] ->
            ok;
        _ ->
            {error, {missing_callbacks, MissingCallbacks}}
    end.

%% @private
%% Check optional callbacks
-spec check_optional_callbacks(module(), [{atom(), arity()}]) -> ok | {error, term()}.
check_optional_callbacks(_Module, _OptionalCallbacks) ->
    %% For now, just return ok - could be enhanced to check proper behavior attributes
    ok.

%% @private
%% Apply transport callback safely
-spec apply_transport_callback(atom(), [term()]) -> term().
apply_transport_callback(_CallbackName, _Args) ->
    %% This is a placeholder - in real implementation, this would call
    %% the appropriate transport module's callback
    {error, not_implemented}.

%% @private
%% Update send statistics
-spec update_send_statistics(map()) -> map().
update_send_statistics(Stats) ->
    Stats#{messages_sent => maps:get(messages_sent, Stats, 0) + 1,
           last_updated => erlang:system_time(millisecond)}.

%% @private
%% Increment error statistics
-spec increment_error_statistics(map()) -> map().
increment_error_statistics(Stats) ->
    Stats#{errors => maps:get(errors, Stats, 0) + 1,
           last_updated => erlang:system_time(millisecond)}.

%%=============================================================================
%% JSON-RPC Message Validation Functions (Exported but Missing Implementation)
%%=============================================================================

%% @doc Validate JSON-RPC request message
-spec validate_json_rpc_request(map()) -> ok | {error, term()}.
validate_json_rpc_request(Message) when is_map(Message) ->
    RequiredFields = [<<"jsonrpc">>, <<"id">>, <<"method">>],
    case validate_required_fields(Message, RequiredFields) of
        ok ->
            case maps:get(<<"jsonrpc">>, Message) of
                <<"2.0">> ->
                    ok;
                _ ->
                    {error, invalid_jsonrpc_version}
            end;
        Error ->
            Error
    end;
validate_json_rpc_request(_) ->
    {error, invalid_message_format}.

%% @doc Validate JSON-RPC response message
-spec validate_json_rpc_response(map()) -> ok | {error, term()}.
validate_json_rpc_response(Message) when is_map(Message) ->
    RequiredFields = [<<"jsonrpc">>, <<"id">>],
    case validate_required_fields(Message, RequiredFields) of
        ok ->
            case maps:get(<<"jsonrpc">>, Message) of
                <<"2.0">> ->
                    HasResult = maps:is_key(<<"result">>, Message),
                    HasError = maps:is_key(<<"error">>, Message),
                    case {HasResult, HasError} of
                        {true, false} ->
                            ok;
                        {false, true} ->
                            ok;
                        {true, true} ->
                            {error, both_result_and_error};
                        {false, false} ->
                            {error, missing_result_or_error}
                    end;
                _ ->
                    {error, invalid_jsonrpc_version}
            end;
        Error ->
            Error
    end;
validate_json_rpc_response(_) ->
    {error, invalid_message_format}.

%% @doc Validate JSON-RPC notification message
-spec validate_json_rpc_notification(map()) -> ok | {error, term()}.
validate_json_rpc_notification(Message) when is_map(Message) ->
    RequiredFields = [<<"jsonrpc">>, <<"method">>],
    case validate_required_fields(Message, RequiredFields) of
        ok ->
            case maps:get(<<"jsonrpc">>, Message) of
                <<"2.0">> ->
                    case maps:is_key(<<"id">>, Message) of
                        false ->
                            ok;
                        true ->
                            {error, notification_with_id}
                    end;
                _ ->
                    {error, invalid_jsonrpc_version}
            end;
        Error ->
            Error
    end;
validate_json_rpc_notification(_) ->
    {error, invalid_message_format}.

%% @doc Extract message type from JSON-RPC message
-spec extract_message_type(map()) -> request | response | notification | error | unknown.
extract_message_type(Message) when is_map(Message) ->
    HasId = maps:is_key(<<"id">>, Message),
    HasMethod = maps:is_key(<<"method">>, Message),
    HasResult = maps:is_key(<<"result">>, Message),
    HasError = maps:is_key(<<"error">>, Message),

    case {HasId, HasMethod, HasResult, HasError} of
        {true, true, false, false} ->
            request;
        {false, true, false, false} ->
            notification;
        {true, false, true, false} ->
            response;
        {true, false, false, true} ->
            error;
        _ ->
            unknown
    end;
extract_message_type(_) ->
    unknown.

%% @doc Check if message is a request
-spec is_request(map()) -> boolean().
is_request(Message) ->
    extract_message_type(Message) =:= request.

%% @doc Check if message is a response
-spec is_response(map()) -> boolean().
is_response(Message) ->
    extract_message_type(Message) =:= response.

%% @doc Check if message is a notification
-spec is_notification(map()) -> boolean().
is_notification(Message) ->
    extract_message_type(Message) =:= notification.

%% @doc Check if message is an error response
-spec is_error_response(map()) -> boolean().
is_error_response(Message) ->
    extract_message_type(Message) =:= error.

%% @private Validate required fields in message
-spec validate_required_fields(map(), [binary()]) -> ok | {error, term()}.
validate_required_fields(Message, RequiredFields) ->
    MissingFields = [Field || Field <- RequiredFields, not maps:is_key(Field, Message)],
    case MissingFields of
        [] ->
            ok;
        _ ->
            {error, {missing_fields, MissingFields}}
    end.
