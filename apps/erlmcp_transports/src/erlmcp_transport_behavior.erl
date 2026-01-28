%%%-------------------------------------------------------------------
%%% @doc
%%% Standardized Transport Behavior for Erlang MCP
%%%
%%% This module defines the complete behavior specification that all MCP
%%% transport implementations must follow. It provides:
%%%
%%% 1. **Behavior Definition**: Callback specifications for required functions
%%% 2. **Common Module Functions**: Standard startup and utility functions  
%%% 3. **Registry Integration**: Automatic registration and message routing
%%% 4. **Message Format Conversion**: Standardized message handling
%%% 5. **Performance Requirements**: Guidelines for implementation
%%%
%%% == Transport Implementation Pattern ==
%%%
%%% All transport modules should follow this structure:
%%%
%%% ```erlang
%%% -module(my_transport).
%%% -behaviour(erlmcp_transport_behavior).
%%% -behaviour(gen_server).
%%%
%%% %% Behavior callbacks
%%% -export([init/1, send/2, close/1, get_info/1]).
%%%
%%% %% gen_server callbacks  
%%% -export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
%%%          terminate/2, code_change/3]).
%%%
%%% %% API
%%% -export([start_link/2]).
%%%
%%% init(Config) -> {ok, State} | {error, Reason}.
%%% send(State, Data) -> ok | {error, Reason}.
%%% close(State) -> ok.
%%% get_info(State) -> #{}.
%%% ```
%%%
%%% == Registry Integration Requirements ==
%%%
%%% All transports MUST:
%%% 1. Auto-register with erlmcp_registry on successful initialization
%%% 2. Route incoming messages to registry via handle_transport_message/2
%%% 3. Handle registry responses appropriately
%%% 4. Unregister from registry during termination
%%% 5. Include transport type in registration config
%%%
%%% == Performance Guidelines ==
%%%
%%% - **Initialization**: Complete within 5 seconds
%%% - **Message Sending**: Handle backpressure gracefully
%%% - **Memory Usage**: Buffer size limits to prevent memory leaks
%%% - **Error Recovery**: Implement reconnection strategies where applicable
%%% - **Connection Pooling**: For HTTP/TCP transports with high throughput
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_behavior).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

%% =============================================================================
%% Type Exports
%% =============================================================================

-export_type([
    transport_state/0,
    transport_config/0,
    transport_type/0,
    transport_info/0,
    transport_status/0,
    transport_statistics/0
]).

%% =============================================================================
%% API Exports
%% =============================================================================

-export([
    start_link/2,
    register_with_registry/3,
    unregister_from_registry/1,
    handle_transport_message/2,
    extract_message_lines/2,
    trim_message_line/1,
    format_transport_error/3,
    validate_transport_config/1,
    default_get_info/3,
    default_handle_transport_call/2
]).

%% =============================================================================
%% Behavior Callback Definitions
%% =============================================================================

%% @doc Initialize transport with configuration
%%
%% This callback is called when the transport process starts. It should:
%% - Validate the provided configuration
%% - Initialize transport-specific resources (sockets, ports, etc.)
%% - Set up connection state
%% - Return initialized state for use in other callbacks
%%
%% The Config parameter contains transport-specific options and MUST include:
%% - transport_id: atom() - Unique identifier for this transport instance
%% - All other options are transport-specific
%%
%% @param Config Transport configuration map
%% @returns {ok, State} on success, {error, Reason} on failure
-callback init(Config :: map()) -> 
    {ok, State :: term()} | 
    {error, Reason :: term()}.

%% @doc Send message through the transport
%%
%% This callback handles outgoing message transmission. It should:
%% - Encode/frame the message according to transport protocol
%% - Handle transport-specific transmission (write to socket, HTTP POST, etc.)
%% - Manage connection state and handle errors appropriately
%% - Return ok on success or descriptive error on failure
%%
%% The Data parameter will be JSON-encoded binary ready for transmission.
%% Transports should NOT re-encode the data but may add framing/headers.
%%
%% @param State Current transport state
%% @param Data Message data to send (pre-encoded JSON binary)
%% @returns ok on success, {error, Reason} on failure
-callback send(State :: term(), Data :: binary()) -> 
    ok | 
    {error, Reason :: term()}.

%% @doc Close transport and clean up resources
%%
%% This callback is called during graceful shutdown. It should:
%% - Close any open connections (sockets, ports, HTTP clients)
%% - Clean up resources (timers, processes, files)
%% - Ensure no resource leaks
%% - Always return ok (never fails)
%%
%% @param State Current transport state
%% @returns ok (always succeeds)
-callback close(State :: term()) -> ok.

%% @doc Get transport information and statistics (Optional)
%%
%% This optional callback provides introspection into transport state.
%% Useful for monitoring, debugging, health checks, and metrics.
%%
%% Should return a map containing:
%% - transport_id: atom() - The transport identifier
%% - type: atom() - Transport type (stdio, tcp, http, websocket, etc.)
%% - status: atom() - Current status (running, connecting, disconnected, error)
%% - config: map() - Current configuration (may be redacted for security)
%% - statistics: map() - Performance metrics (messages_sent, bytes_transferred, etc.)
%% - Additional transport-specific fields
%%
%% @param State Current transport state
%% @returns Map containing transport information
-callback get_info(State :: term()) -> 
    #{atom() => term()}.

%% @doc Handle transport-specific calls (Optional)
%%
%% This optional callback allows transports to handle custom operations.
%% Useful for:
%% - Reconnection requests
%% - Configuration updates
%% - Transport-specific diagnostics
%% - Feature toggles
%%
%% @param Request Custom request term
%% @param State Current transport state
%% @returns {reply, Reply, NewState} or {error, Reason}
-callback handle_transport_call(Request :: term(), State :: term()) -> 
    {reply, Reply :: term(), NewState :: term()} |
    {error, Reason :: term()}.

%% Specify optional callbacks
-optional_callbacks([
    get_info/1,
    handle_transport_call/2
]).

%% =============================================================================
%% Type Definitions
%% =============================================================================

%% @doc Transport state - opaque term maintained by transport implementation
-type transport_state() :: term().

%% @doc Transport configuration map
-type transport_config() :: #{
    transport_id := atom(),
    type => transport_type(),
    test_mode => boolean(),
    atom() => term()
}.

%% @doc Supported transport types
-type transport_type() :: stdio | tcp | http | websocket | custom.

%% @doc Transport information map returned by get_info/1
-type transport_info() :: #{
    transport_id := atom(),
    type := transport_type(),
    status := transport_status(),
    config => map(),
    statistics => transport_statistics(),
    atom() => term()
}.

%% @doc Transport status indicators
-type transport_status() :: 
    running | connecting | connected | disconnected | error | shutdown.

%% @doc Transport performance statistics
-type transport_statistics() :: #{
    messages_sent => non_neg_integer(),
    messages_received => non_neg_integer(),
    bytes_sent => non_neg_integer(),
    bytes_received => non_neg_integer(),
    errors => non_neg_integer(),
    connection_time => non_neg_integer(),
    last_message_time => non_neg_integer()
}.

%% =============================================================================
%% API Functions (Common Implementation Helpers)
%% =============================================================================

%% @doc Standard transport startup function
%%
%% This function provides a standardized way to start transport processes.
%% It should be used by all transport implementations for consistency.
%%
%% @param TransportId Unique identifier for this transport instance
%% @param Config Transport-specific configuration
%% @returns {ok, Pid} on success, {error, Reason} on failure
-spec start_link(atom(), map()) -> 
    {ok, pid()} | 
    {error, term()}.
start_link(TransportId, Config) ->
    ?LOG_INFO("Starting transport: ~p with config: ~p", [TransportId, maps:without([password, secret, token], Config)]),
    
    % Add transport_id to config if not present
    FinalConfig = Config#{transport_id => TransportId},
    
    % Validate basic configuration
    case validate_config(FinalConfig) of
        ok ->
            % Start as gen_server (each transport module implements gen_server)
            Module = maps:get(module, FinalConfig, ?MODULE),
            gen_server:start_link(Module, [TransportId, FinalConfig], []);
        {error, Reason} ->
            ?LOG_ERROR("Invalid transport configuration for ~p: ~p", [TransportId, Reason]),
            {error, {invalid_config, Reason}}
    end.

%% @doc Register transport with the registry
%%
%% This function handles automatic registration with erlmcp_registry.
%% Should be called during transport initialization after successful setup.
%%
%% @param TransportId Transport identifier
%% @param TransportPid Transport process PID
%% @param Config Transport configuration
%% @returns ok | {error, Reason}
-spec register_with_registry(atom(), pid(), map()) -> 
    ok | 
    {error, term()}.
register_with_registry(TransportId, TransportPid, Config) ->
    case whereis(erlmcp_registry) of
        undefined ->
            ?LOG_WARNING("Registry not available for transport ~p", [TransportId]),
            ok;
        _RegistryPid ->
            % Add transport type to config for registry
            TransportType = maps:get(type, Config, custom),
            TransportConfig = Config#{
                type => TransportType,
                pid => TransportPid,
                started_at => erlang:system_time(millisecond)
            },
            
            case erlmcp_registry:register_transport(TransportId, TransportPid, TransportConfig) of
                ok ->
                    ?LOG_DEBUG("Registered transport ~p with registry", [TransportId]),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("Failed to register transport ~p: ~p", [TransportId, Reason]),
                    {error, Reason}
            end
    end.

%% @doc Unregister transport from the registry
%%
%% This function handles cleanup during transport termination.
%% Should be called during transport shutdown.
%%
%% @param TransportId Transport identifier
%% @returns ok
-spec unregister_from_registry(atom()) -> ok.
unregister_from_registry(TransportId) ->
    case whereis(erlmcp_registry) of
        undefined ->
            ok;
        _RegistryPid ->
            case erlmcp_registry:unregister_transport(TransportId) of
                ok ->
                    ?LOG_DEBUG("Unregistered transport ~p from registry", [TransportId]),
                    ok;
                {error, Reason} ->
                    ?LOG_WARNING("Failed to unregister transport ~p: ~p", [TransportId, Reason]),
                    ok
            end
    end.

%% @doc Handle incoming transport messages
%%
%% This function provides standardized message handling for all transports.
%% It should be called when a transport receives data from the network.
%%
%% @param TransportId Transport identifier
%% @param RawData Raw message data received from transport
%% @returns ok | {error, Reason}
-spec handle_transport_message(atom(), binary()) -> 
    ok | 
    {error, term()}.
handle_transport_message(TransportId, RawData) ->
    case erlmcp_registry:route_message(TransportId, RawData) of
        ok ->
            ?LOG_DEBUG("Routed message from transport ~p", [TransportId]),
            ok;
        {error, Reason} ->
            ?LOG_WARNING("Failed to route message from transport ~p: ~p", [TransportId, Reason]),
            {error, Reason}
    end.

%% @doc Convert raw data to standardized message lines
%%
%% This utility function handles line-based message extraction common
%% to many transports. It maintains a buffer and extracts complete lines.
%%
%% @param Buffer Current buffer content
%% @param NewData Newly received data
%% @returns {Lines, RemainingBuffer} tuple
-spec extract_message_lines(binary(), binary()) -> 
    {[binary()], binary()}.
extract_message_lines(Buffer, NewData) ->
    UpdatedBuffer = <<Buffer/binary, NewData/binary>>,
    extract_lines_from_buffer(UpdatedBuffer, [], <<>>).

%% @doc Trim line endings from message data
%%
%% Removes common line endings (CRLF, LF, CR) from message data.
%%
%% @param Line Raw line data
%% @returns Trimmed line data
-spec trim_message_line(binary()) -> binary().
trim_message_line(Line) ->
    re:replace(Line, "[\r\n]+$", "", [global, {return, binary}]).

%% @doc Create standardized transport error response
%%
%% Provides consistent error formatting across all transports.
%%
%% @param TransportId Transport identifier
%% @param Operation Operation that failed
%% @param Reason Error reason
%% @returns Formatted error tuple
-spec format_transport_error(atom(), atom(), term()) -> 
    {error, term()}.
format_transport_error(TransportId, Operation, Reason) ->
    {error, {transport_error, #{
        transport_id => TransportId,
        operation => Operation,
        reason => Reason,
        timestamp => erlang:system_time(millisecond)
    }}}.

%% @doc Validate transport configuration
%%
%% Performs basic validation of transport configuration.
%% Each transport should extend this with specific validation.
%%
%% @param Config Configuration to validate
%% @returns ok | {error, Reason}
-spec validate_transport_config(map()) -> 
    ok | 
    {error, term()}.
validate_transport_config(Config) when is_map(Config) ->
    RequiredFields = [transport_id],
    case check_required_fields(Config, RequiredFields) of
        ok ->
            case maps:get(transport_id, Config) of
                Id when is_atom(Id) -> ok;
                _ -> {error, {invalid_transport_id, not_atom}}
            end;
        Error -> Error
    end;
validate_transport_config(_) ->
    {error, {invalid_config, not_a_map}}.

%% =============================================================================
%% Default Implementations (for common operations)
%% =============================================================================

%% @doc Default get_info implementation
%%
%% Provides a basic implementation of get_info/1 for transports
%% that don't need custom information.
%%
%% @param State Transport state (should be a record with transport_id field)
%% @param Type Transport type
%% @param Config Transport configuration
%% @returns Default transport info map
-spec default_get_info(term(), transport_type(), map()) -> 
    transport_info().
default_get_info(State, Type, Config) ->
    TransportId = extract_transport_id(State),
    #{
        transport_id => TransportId,
        type => Type,
        status => running,
        config => Config,
        statistics => #{
            messages_sent => 0,
            messages_received => 0,
            bytes_sent => 0,
            bytes_received => 0,
            errors => 0,
            connection_time => erlang:system_time(millisecond),
            last_message_time => erlang:system_time(millisecond)
        }
    }.

%% @doc Default handle_transport_call implementation
%%
%% Provides a basic implementation that returns unknown_request error.
%%
%% @param Request The request term
%% @param State Current state
%% @returns Error response
-spec default_handle_transport_call(term(), term()) -> 
    {error, term()}.
default_handle_transport_call(_Request, _State) ->
    {error, unknown_request}.


%% =============================================================================
%% Private Functions
%% =============================================================================

%% @private
%% Basic configuration validation
-spec validate_config(map()) -> ok | {error, term()}.
validate_config(Config) ->
    validate_transport_config(Config).

%% @private
%% Check if all required fields are present in config
-spec check_required_fields(map(), [atom()]) -> ok | {error, term()}.
check_required_fields(Config, RequiredFields) ->
    case [Field || Field <- RequiredFields, not maps:is_key(Field, Config)] of
        [] -> ok;
        MissingFields -> {error, {missing_required_fields, MissingFields}}
    end.

%% @private
%% Extract lines from buffer, handling various line endings
-spec extract_lines_from_buffer(binary(), [binary()], binary()) -> 
    {[binary()], binary()}.
extract_lines_from_buffer(<<>>, Lines, CurrentLine) ->
    case CurrentLine of
        <<>> -> {lists:reverse(Lines), <<>>};
        _ -> {lists:reverse(Lines), CurrentLine}
    end;
extract_lines_from_buffer(<<$\r, $\n, Rest/binary>>, Lines, CurrentLine) ->
    extract_lines_from_buffer(Rest, [CurrentLine | Lines], <<>>);
extract_lines_from_buffer(<<$\n, Rest/binary>>, Lines, CurrentLine) ->
    extract_lines_from_buffer(Rest, [CurrentLine | Lines], <<>>);
extract_lines_from_buffer(<<$\r, Rest/binary>>, Lines, CurrentLine) ->
    extract_lines_from_buffer(Rest, [CurrentLine | Lines], <<>>);
extract_lines_from_buffer(<<Char, Rest/binary>>, Lines, CurrentLine) ->
    extract_lines_from_buffer(Rest, Lines, <<CurrentLine/binary, Char>>).

%% @private
%% Extract transport_id from state (handles various state formats)
-spec extract_transport_id(term()) -> atom().
extract_transport_id(State) when is_tuple(State), tuple_size(State) > 1 ->
    % Try to extract from record - assume transport_id is second element
    case element(2, State) of
        Id when is_atom(Id) -> Id;
        _ -> unknown_transport
    end;
extract_transport_id(#{transport_id := Id}) when is_atom(Id) ->
    Id;
extract_transport_id(_) ->
    unknown_transport.