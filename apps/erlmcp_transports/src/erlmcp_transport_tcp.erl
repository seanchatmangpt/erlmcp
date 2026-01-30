-module(erlmcp_transport_tcp).
-behaviour(gen_server).
-behaviour(ranch_protocol).

-include("erlmcp.hrl").
-include("erlmcp_refusal.hrl").
-include_lib("erlmcp_transports/include/erlmcp_transport_tcp.hrl").

%% Note: We implement erlmcp_transport behavior but use different naming
%% to avoid conflicts with gen_server callbacks

%% Transport API (erlmcp_transport-like interface)
-export([send/2, close/1, transport_init/1, get_max_message_size/0]).

%% Public API
-export([start_link/1, start_server/1, start_client/1, connect/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ranch_protocol callback
-export([start_link/3]).

%% Types
-type mode() :: client | server.
-type transport_opts() :: #{
    mode := mode(),
    host => inet:hostname() | inet:ip_address(),
    port => inet:port_number(),
    owner => pid(),
    server_id => atom(),
    transport_id => atom(),
    connect_timeout => timeout(),
    keepalive => boolean(),
    nodelay => boolean(),
    buffer_size => pos_integer(),
    num_acceptors => pos_integer(),
    max_connections => pos_integer() | infinity,
    max_reconnect_attempts => pos_integer() | infinity,
    % Pool configuration
    use_pool => boolean(),
    pool_name => atom(),
    pool_min_size => pos_integer(),
    pool_max_size => pos_integer(),
    pool_strategy => round_robin | least_loaded | random
}.

%%% State record definition is now in include/erlmcp_transport_tcp.hrl

-type state() :: #state{}.

-export_type([transport_opts/0, mode/0]).

%% Default values
-define(DEFAULT_CONNECT_TIMEOUT, 5000).
-define(DEFAULT_BUFFER_SIZE, 65536).
-define(DEFAULT_NUM_ACCEPTORS, 10).
-define(DEFAULT_MAX_CONNECTIONS, 1024).
-define(INITIAL_RECONNECT_DELAY, 1000).
-define(MAX_RECONNECT_DELAY, 60000).
-define(DEFAULT_MAX_RECONNECT_ATTEMPTS, infinity).
-define(IDLE_TIMEOUT, 300000). %% 5 minutes idle timeout
-define(RESOURCE_MONITOR_INTERVAL, 60000). %% 1 minute
-define(DEFAULT_MAX_MESSAGE_SIZE, 16777216). %% 16 MB

%% Pool defaults
-define(DEFAULT_POOL_MIN_SIZE, 10).
-define(DEFAULT_POOL_MAX_SIZE, 1000).
-define(DEFAULT_POOL_STRATEGY, round_robin).

%% Connection lease timeout (prevents stuck connections)
-define(CONNECTION_LEASE_TIMEOUT, 30000). %% 30 seconds max for handler init

%%====================================================================
%% Transport API (erlmcp_transport-like interface)
%%====================================================================

%% @doc Initialize transport state (used when started via external transport interface)
%% This is separate from gen_server init/1 to avoid callback conflicts
-spec transport_init(transport_opts()) -> {ok, state()} | {error, term()}.
transport_init(Opts) when is_map(Opts) ->
    Mode = maps:get(mode, Opts, client),
    case Mode of
        server ->
            init_server(Opts);
        client ->
            init_client(Opts)
    end.

%% @doc Send data through the transport
%% Optimized for zero-copy using iolist-based writes
-spec send(state(), iodata()) -> ok | {error, term()}.
send(#state{socket = undefined}, _Data) ->
    {error, not_connected};
send(#state{socket = Socket, connected = true}, Data) ->
    %% Use iolist format [Data, Newline] to avoid binary rebuilding
    %% gen_tcp:send/2 efficiently handles iolist encoding
    case gen_tcp:send(Socket, [Data, <<"\n">>]) of
        ok -> ok;
        {error, Reason} -> {error, {tcp_send_failed, Reason}}
    end;
send(_State, _Data) ->
    {error, not_connected}.

%% @doc Close the transport
-spec close(state()) -> ok.
close(#state{mode = server, ranch_ref = RanchRef, server_id = ServerId})
  when RanchRef =/= undefined ->
    %% Release connection slot before stopping listener
    catch erlmcp_connection_limiter:release_connection(ServerId),
    ranch:stop_listener(RanchRef),
    ok;
close(#state{socket = Socket, server_id = ServerId}) when Socket =/= undefined ->
    %% Release connection slot on close
    catch erlmcp_connection_limiter:release_connection(ServerId),
    gen_tcp:close(Socket),
    ok;
close(_State) ->
    ok.

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start transport process with options
-spec start_link(transport_opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Start a TCP server using ranch
-spec start_server(transport_opts()) -> {ok, pid()} | {error, term()}.
start_server(Opts) when is_map(Opts) ->
    start_link(Opts#{mode => server}).

%% @doc Start a TCP client
-spec start_client(transport_opts()) -> {ok, pid()} | {error, term()}.
start_client(Opts) when is_map(Opts) ->
    start_link(Opts#{mode => client}).

%% @doc Connect to a remote server (client mode only)
-spec connect(pid(), transport_opts()) -> ok | {error, term()}.
connect(Pid, Opts) when is_pid(Pid), is_map(Opts) ->
    gen_server:call(Pid, {connect, Opts}).

%%====================================================================
%% ranch_protocol Callback
%%====================================================================

%% @doc Start a ranch protocol handler for an accepted connection
%% CRITICAL: Guaranteed cleanup of connection slot via try...catch/after
-spec start_link(ranch:ref(), module(), map()) -> {ok, pid()} | {error, term()}.
start_link(RanchRef, _Transport, ProtocolOpts) ->
    ServerId = maps:get(server_id, ProtocolOpts, undefined),

    %% Check connection limit BEFORE accepting connection
    case erlmcp_connection_limiter:accept_connection(ServerId) of
        accept ->
            %% CRITICAL FIX: Use try...catch/after to guarantee slot release
            %% If init fails for ANY reason, we MUST release the slot
            try
                case gen_server:start_link(?MODULE, #{
                    mode => server,
                    ranch_ref => RanchRef,
                    protocol_opts => ProtocolOpts,
                    server_id => ServerId
                }, []) of
                    {ok, Pid} = Result ->
                        %% Monitor the handler process to detect early crashes
                        erlang:monitor(process, Pid),
                        Result;
                    {error, Reason} = Error ->
                        %% Handler failed to start, release slot immediately
                        logger:warning("Handler init failed, releasing slot: ~p", [Reason]),
                        erlmcp_connection_limiter:release_connection(ServerId),
                        Error
                end
            catch
                Type:Error:Stacktrace ->
                    %% EXCEPTION during handler start - MUST release slot
                    logger:error("Handler start exception ~p:~p, releasing slot~n~p",
                               [Type, Error, Stacktrace]),
                    erlmcp_connection_limiter:release_connection(ServerId),
                    {error, {handler_start_exception, {Type, Error}}}
            end;
        {error, too_many_connections} ->
            logger:warning("Rejecting connection: too many connections for server ~p", [ServerId]),
            {error, too_many_connections}
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @doc Initialize gen_server for accepted ranch connection
init(#{mode := server, ranch_ref := RanchRef, protocol_opts := ProtocolOpts} = InitArgs) ->
    %% CRITICAL: Track that we own a connection slot
    %% We'll release it in terminate/2
    process_flag(trap_exit, true),

    ServerId = maps:get(server_id, ProtocolOpts, undefined),
    TransportId = maps:get(transport_id, ProtocolOpts, undefined),

    %% CRITICAL: Set up connection lease timeout
    %% If init takes too long, we'll kill ourselves to release the slot
    LeaseTimer = erlang:send_after(?CONNECTION_LEASE_TIMEOUT, self(), connection_lease_timeout),

    try
        Owner = maps:get(owner, ProtocolOpts, self()),

        %% Get the socket from ranch (can fail)
        {ok, Socket} = ranch:handshake(RanchRef),

        %% Set socket to active mode for message reception
        ok = inet:setopts(Socket, [{active, true}]),

        %% Cancel lease timeout - we initialized successfully
        erlang:cancel_timer(LeaseTimer),
        flush_message(connection_lease_timeout),

        %% Start idle timeout timer
        IdleTimer = erlang:send_after(?IDLE_TIMEOUT, self(), cleanup_idle),

        %% Start resource monitor
        ResourceMonitorTimer = erlang:send_after(?RESOURCE_MONITOR_INTERVAL, self(), check_resources),

        %% Monitor connection for leak detection
        ConnectionInfo = #{
            socket => Socket,
            server_id => ServerId,
            transport_id => TransportId,
            bytes_sent => 0,
            bytes_received => 0
        },
        catch erlmcp_connection_monitor:monitor_connection(self(), ConnectionInfo),

        %% Notify owner of connection
        Owner ! {transport_connected, self()},

        %% Get max message size (default 16MB)
        MaxMessageSize = get_max_message_size(),

        %% Mark that we successfully initialized (flag for terminate/2)
        {ok, #state{
            mode = server,
            transport_id = TransportId,
            server_id = ServerId,
            socket = Socket,
            ranch_ref = RanchRef,
            owner = Owner,
            connected = true,
            options = [],
            idle_timer = IdleTimer,
            resource_monitor_timer = ResourceMonitorTimer,
            last_activity = erlang:monotonic_time(millisecond),
            max_message_size = MaxMessageSize,
            initialized = true  %% FLAG: Successfully initialized
        }}
    catch
        Type:Error:Stacktrace ->
            %% INIT FAILED - Clean up and release slot in terminate/2
            logger:error("TCP handler init failed ~p:~p, slot will be released in terminate~n~p",
                       [Type, Error, Stacktrace]),
            erlang:cancel_timer(LeaseTimer),
            flush_message(connection_lease_timeout),
            {stop, {init_failed, {Type, Error}}}
    end;

init(#{mode := Mode} = Opts) ->
    process_flag(trap_exit, true),
    case Mode of
        server -> init_server_listener(Opts);
        client -> init_client_process(Opts)
    end.

%% @doc Handle synchronous calls
handle_call({send, Data}, _From, State) ->
    Result = send(State, Data),
    case Result of
        ok ->
            %% Update activity tracking and byte count
            DataSize = byte_size(Data),
            NewState = State#state{
                last_activity = erlang:monotonic_time(millisecond),
                bytes_sent = State#state.bytes_sent + DataSize
            },
            {reply, ok, NewState};
        {error, _} = Error ->
            %% Connection might be lost, handle in async if needed
            {reply, Error, State}
    end;

handle_call({connect, NewOpts}, _From, #state{mode = client} = State) ->
    %% Update connection parameters and reconnect
    NewState = update_client_opts(State, NewOpts),

    %% Disconnect if currently connected
    FinalState = case State#state.socket of
        undefined -> NewState;
        Socket ->
            gen_tcp:close(Socket),
            NewState#state{socket = undefined, connected = false}
    end,

    %% Trigger reconnection
    self() ! connect,
    {reply, ok, FinalState};

handle_call({connect, _NewOpts}, _From, State) ->
    {reply, {error, {invalid_mode, State#state.mode}}, State};

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous casts
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info(connect, #state{mode = client} = State) ->
    {noreply, attempt_connection(State)};

handle_info({tcp, Socket, Data}, #state{socket = Socket, buffer = Buffer, max_message_size = MaxMessageSize} = State) ->
    %% Step 1: Validate 16MB message size limit first (transport-level enforcement)
    DataSize = byte_size(Data),
    NewBufferSize = byte_size(Buffer) + DataSize,

    case NewBufferSize > MaxMessageSize of
        true ->
            logger:error("TCP message exceeds 16MB limit (~p bytes > ~p bytes)",
                [NewBufferSize, MaxMessageSize]),
            %% Send proper JSON-RPC error response to client before closing
            ErrorMsg = erlmcp_json_rpc:error_message_too_large(null, MaxMessageSize),
            catch gen_tcp:send(Socket, [ErrorMsg, <<"\n">>]),
            %% Close connection to prevent resource exhaustion
            gen_tcp:close(Socket),
            {stop, {message_too_large, NewBufferSize}, State};
        false ->
            %% Step 2: Check system memory guard (second line of defense)
            case erlmcp_memory_guard:check_allocation(DataSize) of
                ok ->
                    %% Accumulate data in buffer
                    NewBuffer = <<Buffer/binary, Data/binary>>,

                    %% Process complete messages
                    {Messages, RemainingBuffer} = extract_messages(NewBuffer),

                    %% Send messages to owner
                    Owner = State#state.owner,
                    lists:foreach(fun(Msg) ->
                        Owner ! {transport_message, Msg}
                    end, Messages),

                    %% Update activity tracking and byte count
                    {noreply, State#state{
                        buffer = RemainingBuffer,
                        last_activity = erlang:monotonic_time(millisecond),
                        bytes_received = State#state.bytes_received + DataSize
                    }};
                {error, payload_too_large} ->
                    logger:error("TCP message rejected by memory guard: ~p bytes", [DataSize]),
                    %% Send proper JSON-RPC error response to client
                    ErrorMsg = erlmcp_json_rpc:error_message_too_large(null, MaxMessageSize),
                    catch gen_tcp:send(Socket, [ErrorMsg, <<"\n">>]),
                    %% Close connection to prevent resource exhaustion
                    gen_tcp:close(Socket),
                    {stop, {message_too_large, DataSize}, State};
                {error, resource_exhausted} ->
                    logger:error("System memory exhausted, rejecting message"),
                    %% Send resource exhausted error
                    ErrorMsg = erlmcp_json_rpc:error_internal(<<"System memory exhausted">>),
                    catch gen_tcp:send(Socket, [ErrorMsg, <<"\n">>]),
                    gen_tcp:close(Socket),
                    {stop, resource_exhausted, State}
            end
    end;

handle_info({tcp_closed, Socket}, #state{socket = Socket, mode = server} = State) ->
    %% Server connection closed - stop the handler process
    logger:info("Server connection closed"),
    {stop, normal, State};

handle_info({tcp_closed, Socket}, #state{socket = Socket, mode = client} = State) ->
    %% Client connection closed - attempt reconnection
    logger:info("Client connection closed"),
    {noreply, handle_disconnect(State, normal)};

handle_info({tcp_error, Socket, Reason}, #state{socket = Socket, mode = server} = State) ->
    %% Server connection error - stop the handler process
    logger:error("Server connection error: ~p", [Reason]),
    {stop, {tcp_error, Reason}, State};

handle_info({tcp_error, Socket, Reason}, #state{socket = Socket, mode = client} = State) ->
    %% Client connection error - attempt reconnection
    logger:error("Client connection error: ~p", [Reason]),
    {noreply, handle_disconnect(State, Reason)};

handle_info(reconnect, #state{mode = client} = State) ->
    {noreply, attempt_connection(State#state{reconnect_timer = undefined})};

handle_info({'DOWN', _MonitorRef, process, Owner, Reason},
            #state{owner = Owner} = State) ->
    logger:info("Owner process ~p died: ~p", [Owner, Reason]),
    {stop, {owner_died, Reason}, State};

handle_info({'EXIT', Socket, Reason}, #state{socket = Socket} = State) ->
    logger:warning("Socket process died: ~p", [Reason]),
    case State#state.mode of
        server ->
            {stop, {socket_died, Reason}, State};
        client ->
            {noreply, handle_disconnect(State, Reason)}
    end;

handle_info(cleanup_idle, #state{socket = Socket} = State) when Socket =/= undefined ->
    %% Check if connection has been idle for too long
    Now = erlang:monotonic_time(millisecond),
    IdleTime = Now - State#state.last_activity,
    case IdleTime > ?IDLE_TIMEOUT of
        true ->
            logger:info("Closing idle connection after ~pms", [IdleTime]),
            gen_tcp:close(Socket),
            {stop, normal, State};
        false ->
            %% Reschedule check
            NewIdleTimer = erlang:send_after(?IDLE_TIMEOUT - IdleTime, self(), cleanup_idle),
            {noreply, State#state{idle_timer = NewIdleTimer}}
    end;

handle_info(cleanup_idle, State) ->
    %% No socket, ignore
    {noreply, State};

handle_info(check_resources, State) ->
    %% Monitor resource usage and alert if approaching limits
    check_resource_usage(State),
    %% Reschedule next check
    NewMonitorTimer = erlang:send_after(?RESOURCE_MONITOR_INTERVAL, self(), check_resources),
    {noreply, State#state{resource_monitor_timer = NewMonitorTimer}};

handle_info(connection_lease_timeout, State) ->
    %% Handler init took too long - kill ourselves to release slot
    logger:error("Connection lease timeout during init, terminating to release slot"),
    {stop, connection_lease_timeout, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Cleanup on termination
%% CRITICAL: Release connection slot on ALL termination paths
terminate(Reason, #state{mode = server, server_id = ServerId, initialized = true} = State) ->
    %% Successfully initialized handler - release slot
    logger:info("TCP handler terminating: ~p, releasing connection slot for server ~p",
               [Reason, ServerId]),
    erlmcp_connection_limiter:release_connection(ServerId),
    cleanup_common(State),
    ok;

terminate(Reason, #state{mode = server, server_id = ServerId} = State) ->
    %% Init failed - slot was already released in start_link/3 or init/1
    logger:warning("TCP handler terminating before init complete: ~p", [Reason]),
    cleanup_common(State),
    ok;

terminate(_Reason, State) ->
    %% Client mode or other - no slot to release
    cleanup_common(State),
    ok.

%% @doc Handle code upgrades
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Cleanup
%%====================================================================

%% @doc Common cleanup for all termination paths
cleanup_common(State) ->
    %% Unmonitor connection from leak detector
    catch erlmcp_connection_monitor:unmonitor_connection(self()),

    %% Cancel reconnect timer if active
    cancel_reconnect_timer(State),

    %% Cancel idle timer if active
    cancel_idle_timer(State),

    %% Cancel resource monitor timer if active
    cancel_resource_monitor_timer(State),

    %% Close socket if connected
    case State#state.socket of
        undefined -> ok;
        Socket ->
            catch gen_tcp:close(Socket),
            logger:info("Socket closed, transferred ~p bytes sent, ~p bytes received",
                       [State#state.bytes_sent, State#state.bytes_received])
    end,

    %% Note: ranch listener cleanup is handled separately via close/1
    ok.

%% @doc Flush any pending message of the given type
flush_message(MsgType) ->
    receive
        MsgType ->
            ok
    after 0 ->
        ok
    end.

%%====================================================================
%% Internal Functions - Initialization
%%====================================================================

%% @doc Initialize server listener using ranch
init_server_listener(Opts) ->
    Port = maps:get(port, Opts, 0),
    ServerId = maps:get(server_id, Opts, undefined),
    TransportId = maps:get(transport_id, Opts, undefined),
    Owner = maps:get(owner, Opts, self()),
    NumAcceptors = maps:get(num_acceptors, Opts, ?DEFAULT_NUM_ACCEPTORS),
    MaxConnections = maps:get(max_connections, Opts, ?DEFAULT_MAX_CONNECTIONS),

    %% Create unique ranch reference
    RanchRef = make_ranch_ref(TransportId, ServerId),

    %% Build socket options for ranch
    SocketOpts = build_ranch_socket_options(Opts, Port),

    %% Transport options for ranch
    TransportOpts = #{
        socket_opts => SocketOpts,
        num_acceptors => NumAcceptors,
        max_connections => MaxConnections
    },

    %% Protocol options passed to each connection handler
    ProtocolOpts = #{
        owner => Owner,
        server_id => ServerId,
        transport_id => TransportId
    },

    %% Start ranch listener
    case ranch:start_listener(RanchRef, ranch_tcp, TransportOpts,
                               ?MODULE, ProtocolOpts) of
        {ok, _ListenerPid} ->
            logger:info("TCP server started on port ~p with ranch ref ~p",
                       [Port, RanchRef]),

            %% Get the actual port if 0 was specified
            ActualPort = case Port of
                0 -> ranch:get_port(RanchRef);
                _ -> Port
            end,

            %% Get max message size (default 16MB)
            MaxMessageSize = get_max_message_size(),

            {ok, #state{
                mode = server,
                transport_id = TransportId,
                server_id = ServerId,
                ranch_ref = RanchRef,
                owner = Owner,
                port = ActualPort,
                connected = true,
                max_message_size = MaxMessageSize
            }};
        {error, Reason} ->
            {stop, {ranch_start_failed, Reason}}
    end.

%% @doc Initialize client process
init_client_process(Opts) ->
    Host = maps:get(host, Opts),
    Port = maps:get(port, Opts),
    Owner = maps:get(owner, Opts, self()),
    ServerId = maps:get(server_id, Opts, undefined),
    TransportId = maps:get(transport_id, Opts, undefined),
    MaxReconnect = maps:get(max_reconnect_attempts, Opts,
                           ?DEFAULT_MAX_RECONNECT_ATTEMPTS),

    %% Monitor the owner process
    monitor(process, Owner),

    %% Get max message size (default 16MB)
    MaxMessageSize = get_max_message_size(),

    State = #state{
        mode = client,
        transport_id = TransportId,
        server_id = ServerId,
        owner = Owner,
        host = Host,
        port = Port,
        options = build_socket_options(Opts),
        max_reconnect_attempts = MaxReconnect,
        last_activity = erlang:monotonic_time(millisecond),
        max_message_size = MaxMessageSize
    },

    %% Attempt initial connection
    self() ! connect,

    {ok, State}.

%% @doc Initialize server mode (called from erlmcp_transport:init/1)
init_server(Opts) ->
    init_server_listener(Opts).

%% @doc Initialize client mode (called from erlmcp_transport:init/1)
init_client(Opts) ->
    init_client_process(Opts).

%%====================================================================
%% Internal Functions - Socket Options
%%====================================================================

%% @doc Build socket options for client connections
build_socket_options(Opts) ->
    BaseOpts = [
        binary,
        {active, true},
        {packet, line},
        {reuseaddr, true},
        {send_timeout, 5000},
        {send_timeout_close, true}
    ],

    %% Add optional settings
    OptionalOpts = lists:foldl(fun({Key, OptKey}, Acc) ->
        case maps:get(Key, Opts, undefined) of
            undefined -> Acc;
            Value -> [{OptKey, Value} | Acc]
        end
    end, BaseOpts, [
        {keepalive, keepalive},
        {nodelay, nodelay},
        {buffer_size, buffer}
    ]),

    %% Ensure we have appropriate buffer sizes
    BufferSize = maps:get(buffer_size, Opts, ?DEFAULT_BUFFER_SIZE),
    [
        {recbuf, BufferSize},
        {sndbuf, BufferSize}
        | OptionalOpts
    ].

%% @doc Build socket options for ranch listener
build_ranch_socket_options(Opts, Port) ->
    BaseOpts = [
        binary,
        {active, false},  % Ranch manages this
        {packet, line},
        {reuseaddr, true},
        {port, Port}
    ],

    BufferSize = maps:get(buffer_size, Opts, ?DEFAULT_BUFFER_SIZE),

    OptionalOpts = lists:foldl(fun({Key, OptKey}, Acc) ->
        case maps:get(Key, Opts, undefined) of
            undefined -> Acc;
            Value -> [{OptKey, Value} | Acc]
        end
    end, BaseOpts, [
        {keepalive, keepalive},
        {nodelay, nodelay}
    ]),

    [
        {recbuf, BufferSize},
        {sndbuf, BufferSize}
        | OptionalOpts
    ].

%%====================================================================
%% Internal Functions - Connection Management
%%====================================================================

%% @doc Attempt to establish a client connection
attempt_connection(#state{reconnect_attempts = Attempts,
                          max_reconnect_attempts = MaxAttempts} = State)
  when is_integer(MaxAttempts), Attempts >= MaxAttempts ->
    logger:error("Maximum reconnection attempts (~p) reached", [MaxAttempts]),
    State#state{connected = false};

attempt_connection(#state{host = Host, port = Port, options = Options} = State) ->
    ConnectTimeout = ?DEFAULT_CONNECT_TIMEOUT,

    logger:info("Attempting TCP connection to ~s:~p", [Host, Port]),

    case gen_tcp:connect(Host, Port, Options, ConnectTimeout) of
        {ok, Socket} ->
            logger:info("TCP connection established"),
            %% Notify owner of successful connection
            State#state.owner ! {transport_connected, self()},

            State#state{
                socket = Socket,
                connected = true,
                reconnect_attempts = 0,
                buffer = <<>>
            };
        {error, Reason} ->
            logger:error("TCP connection failed: ~p", [Reason]),
            schedule_reconnect(State)
    end.

%% @doc Handle disconnection
handle_disconnect(#state{socket = undefined} = State, _Reason) ->
    State;
handle_disconnect(#state{socket = Socket, owner = Owner} = State, Reason) ->
    %% Close the socket
    catch gen_tcp:close(Socket),

    %% Notify owner
    Owner ! {transport_disconnected, self(), Reason},

    %% Schedule reconnection
    NewState = State#state{
        socket = undefined,
        connected = false,
        buffer = <<>>
    },

    schedule_reconnect(NewState).

%% @doc Schedule a reconnection attempt
schedule_reconnect(#state{reconnect_timer = Timer} = State)
  when Timer =/= undefined ->
    %% Already scheduled
    State;
schedule_reconnect(#state{reconnect_attempts = Attempts} = State) ->
    %% Calculate backoff delay
    Delay = calculate_backoff(Attempts),

    logger:info("Scheduling reconnection in ~p ms (attempt ~p)",
                [Delay, Attempts + 1]),

    Timer = erlang:send_after(Delay, self(), reconnect),

    State#state{
        reconnect_timer = Timer,
        reconnect_attempts = Attempts + 1
    }.

%% @doc Calculate exponential backoff with jitter
calculate_backoff(Attempts) ->
    BaseDelay = min(?INITIAL_RECONNECT_DELAY * (1 bsl Attempts),
                    ?MAX_RECONNECT_DELAY),
    Jitter = rand:uniform(BaseDelay div 4),
    BaseDelay + Jitter.

%% @doc Cancel reconnection timer
cancel_reconnect_timer(#state{reconnect_timer = undefined}) ->
    ok;
cancel_reconnect_timer(#state{reconnect_timer = Timer}) ->
    case erlang:cancel_timer(Timer) of
        false -> ok;  % Timer already fired
        _ -> ok       % Timer cancelled
    end.

%%====================================================================
%% Internal Functions - Message Processing
%%====================================================================

%% @doc Extract complete messages from buffer
%% Optimized using binary:split/3 with global flag to reduce allocations
extract_messages(Buffer) ->
    extract_messages_optimized(Buffer, []).

extract_messages_optimized(Buffer, Acc) ->
    case binary:split(Buffer, <<"\n">>, [global]) of
        [_SinglePart] ->
            %% No complete message, return what we have
            {lists:reverse(Acc), Buffer};
        Parts when is_list(Parts) ->
            %% Split returned multiple parts
            case lists:reverse(Parts) of
                [LastPart | RestParts] ->
                    %% Last part is incomplete (no newline after it)
                    CompleteParts = lists:reverse(RestParts),
                    %% If any parts are empty (consecutive newlines), filter them
                    ValidMessages = [M || M <- CompleteParts, M =/= <<>>],
                    {lists:reverse(Acc) ++ ValidMessages, LastPart}
            end
    end.

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

%% @doc Create a unique ranch reference
make_ranch_ref(undefined, undefined) ->
    list_to_atom("erlmcp_tcp_" ++ integer_to_list(erlang:unique_integer([positive])));
make_ranch_ref(TransportId, undefined) ->
    list_to_atom(atom_to_list(TransportId) ++ "_ranch");
make_ranch_ref(undefined, ServerId) ->
    list_to_atom(atom_to_list(ServerId) ++ "_ranch");
make_ranch_ref(TransportId, ServerId) ->
    list_to_atom(atom_to_list(TransportId) ++ "_" ++
                 atom_to_list(ServerId) ++ "_ranch").

%% @doc Update client options during reconnection
update_client_opts(State, NewOpts) ->
    State#state{
        host = maps:get(host, NewOpts, State#state.host),
        port = maps:get(port, NewOpts, State#state.port),
        options = build_socket_options(NewOpts)
    }.

%% @doc Check resource usage and alert if approaching limits
check_resource_usage(#state{bytes_received = BytesRecv, bytes_sent = BytesSent, mode = Mode}) ->
    TotalBytes = BytesRecv + BytesSent,
    %% Check if we're approaching memory limits
    case erlmcp_memory_guard:is_circuit_breaker_open() of
        true ->
            logger:warning("Circuit breaker open: system memory critical, total transferred: ~p bytes",
                          [TotalBytes]);
        false ->
            ok
    end,
    %% Log resource usage if significant
    case TotalBytes > 100 * 1024 * 1024 of  % 100MB threshold
        true ->
            logger:info("Transport (~p) transferred ~p bytes (sent: ~p, recv: ~p)",
                       [Mode, TotalBytes, BytesSent, BytesRecv]);
        false ->
            ok
    end,
    ok.

%% @doc Cancel idle timer
cancel_idle_timer(#state{idle_timer = undefined}) ->
    ok;
cancel_idle_timer(#state{idle_timer = Timer}) ->
    case erlang:cancel_timer(Timer) of
        false -> ok;
        _ -> ok
    end.

%% @doc Cancel resource monitor timer
cancel_resource_monitor_timer(#state{resource_monitor_timer = undefined}) ->
    ok;
cancel_resource_monitor_timer(#state{resource_monitor_timer = Timer}) ->
    case erlang:cancel_timer(Timer) of
        false -> ok;
        _ -> ok
    end.

%% @doc Get the maximum allowed message size from configuration.
%% Falls back to default 16MB if not configured.
-spec get_max_message_size() -> pos_integer().
get_max_message_size() ->
    case application:get_env(erlmcp, message_size_limits) of
        {ok, Limits} when is_map(Limits) ->
            maps:get(tcp, Limits, ?DEFAULT_MAX_MESSAGE_SIZE);
        _ ->
            ?DEFAULT_MAX_MESSAGE_SIZE
    end.
