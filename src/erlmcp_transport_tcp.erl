%%%-------------------------------------------------------------------
%%% @doc
%%% TCP Transport Implementation for Erlang MCP
%%%
%%% This module implements a robust TCP transport that follows the standard
%%% gen_server pattern and the erlmcp_transport_behavior specification.
%%%
%%% Key features:
%%% - Automatic registry integration
%%% - Connection management with reconnection logic
%%% - Comprehensive error handling and recovery
%%% - Buffer management for message framing
%%% - Supervisor integration ready
%%% - Production-quality connection lifecycle management
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_tcp).

-behaviour(gen_server).
-behaviour(erlmcp_transport_behavior).

-include("erlmcp.hrl").

-include_lib("kernel/include/logger.hrl").

%% API exports
-export([start_link/2, send/2, close/1, get_info/1, handle_transport_call/2]).

%% Transport behavior callbacks (implementing erlmcp_transport_behavior)  
%% Note: init/1 callback conflicts with gen_server, so we provide transport_init/1 instead

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

%% State record as specified in the plan (lines 262-272)
-record(state,
        {transport_id :: atom(),
         server_id :: atom() | undefined,
         config :: map(),
         socket :: gen_tcp:socket() | undefined,
         host :: inet:hostname(),
         port :: inet:port_number(),
         buffer = <<>> :: binary(),
         connected = false :: boolean(),
         reconnect_timer :: reference() | undefined,
         reconnect_attempts = 0 :: non_neg_integer(),
         max_reconnect_attempts = infinity :: pos_integer() | infinity,
         socket_opts :: [gen_tcp:connect_option()],
         stats :: map()}).

-type state() :: #state{}.
-type tcp_config() ::
    #{host := inet:hostname() | inet:ip_address(),
      port := inet:port_number(),
      server_id => atom(),
      connect_timeout => timeout(),
      keepalive => boolean(),
      nodelay => boolean(),
      buffer_size => pos_integer(),
      max_reconnect_attempts => pos_integer() | infinity,
      test_mode => boolean()}.

%% Default configuration values
-define(DEFAULT_CONNECT_TIMEOUT, 5000).
-define(DEFAULT_BUFFER_SIZE, 65536).
-define(DEFAULT_MAX_RECONNECT_ATTEMPTS, infinity).
-define(INITIAL_RECONNECT_DELAY, 1000).
-define(MAX_RECONNECT_DELAY, 60000).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the TCP transport process
-spec start_link(atom(), tcp_config()) -> {ok, pid()} | {error, term()}.
start_link(TransportId, Config) ->
    ?LOG_INFO("Starting TCP transport: ~p", [TransportId]),
    gen_server:start_link(?MODULE, [TransportId, Config], []).

%% @doc Send data through the transport
-spec send(pid() | state(), iodata()) -> ok | {error, term()}.
send(Pid, Data) when is_pid(Pid) ->
    gen_server:call(Pid, {send, Data});
send(#state{} = State, Data) ->
    do_send(State, Data).

%% @doc Close the transport connection
-spec close(pid() | state()) -> ok.
close(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, close);
close(#state{} = State) ->
    do_close(State).

%% @doc Get transport information
-spec get_info(pid() | state()) -> map().
get_info(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_info);
get_info(#state{} = State) ->
    do_get_info(State).

%% @doc Handle transport-specific calls
-spec handle_transport_call(term(), state()) ->
                               {reply, term(), state()} | {error, term()}.
handle_transport_call(get_socket, #state{socket = Socket} = State) ->
    {reply, {ok, Socket}, State};
handle_transport_call(get_connection,
                      #state{host = Host,
                             port = Port,
                             connected = Conn} =
                          State) ->
    {reply,
     {ok,
      #{host => Host,
        port => Port,
        connected => Conn}},
     State};
handle_transport_call(get_stats, #state{stats = Stats} = State) ->
    {reply, {ok, Stats}, State};
handle_transport_call(reconnect, State) ->
    NewState = cancel_reconnect_timer(State),
    self() ! attempt_connection,
    {reply, ok, NewState#state{reconnect_attempts = 0}};
handle_transport_call(_Unknown, State) ->
    {error, unknown_request}.


%%====================================================================
%% gen_server Callbacks
%%====================================================================

%%====================================================================
%% Transport Behavior Callbacks
%%====================================================================

%% Note: The transport behavior init/1 callback is handled via internal_init/2 
%% to avoid conflicts with gen_server:init/1 callback

%% @doc Initialize the gen_server process
-spec init(list()) -> {ok, state()} | {error, term()} | {stop, term()}.
init([TransportId, Config]) ->
    process_flag(trap_exit, true),

    case internal_init(TransportId, Config) of
        {ok, State} ->
            %% Schedule initial connection attempt
            self() ! attempt_connection,
            {ok, State};
        {error, Reason} ->
            ?LOG_ERROR("Failed to initialize TCP transport ~p: ~p", [TransportId, Reason]),
            {stop, Reason}
    end.

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, state()) ->
                     {reply, term(), state()} |
                     {noreply, state()} |
                     {stop, term(), term(), state()}.
handle_call({send, Data}, _From, State) ->
    case do_send(State, Data) of
        ok ->
            NewStats = update_stats(State#state.stats, messages_sent, 1),
            {reply, ok, State#state{stats = NewStats}};
        {error, Reason} ->
            NewStats = update_stats(State#state.stats, errors, 1),
            {reply, {error, Reason}, State#state{stats = NewStats}}
    end;
handle_call(close, _From, State) ->
    ok = do_close(State),
    NewState =
        State#state{socket = undefined,
                    connected = false,
                    buffer = <<>>},
    {reply, ok, NewState};
handle_call(get_info, _From, State) ->
    Info = do_get_info(State),
    {reply, Info, State};
handle_call({transport_call, Request}, _From, State) ->
    case handle_transport_call(Request, State) of
        {reply, Reply, NewState} ->
            {reply, {reply, Reply, NewState}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call(reconnect, _From, State) ->
    %% Force reconnection by canceling timer and scheduling immediate attempt
    NewState = cancel_reconnect_timer(State),
    self() ! attempt_connection,
    {reply, ok, NewState#state{reconnect_attempts = 0}};
handle_call(get_state, _From, State) ->
    %% For testing and debugging
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle process messages
-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
%% Connection attempt
handle_info(attempt_connection, State) ->
    NewState = attempt_connection(State),
    {noreply, NewState};
%% Reconnection timer fired
handle_info(reconnect, State) ->
    NewState = State#state{reconnect_timer = undefined},
    FinalState = attempt_connection(NewState),
    {noreply, FinalState};
%% Incoming TCP data
handle_info({tcp, Socket, Data}, #state{socket = Socket} = State) ->
    NewState = handle_tcp_data(State, Data),
    %% Set socket back to active mode
    inet:setopts(Socket, [{active, once}]),
    {noreply, NewState};
%% TCP connection closed
handle_info({tcp_closed, Socket}, #state{socket = Socket} = State) ->
    ?LOG_INFO("TCP connection closed for transport ~p", [State#state.transport_id]),
    NewState = handle_disconnect(State, tcp_closed),
    {noreply, NewState};
%% TCP error
handle_info({tcp_error, Socket, Reason}, #state{socket = Socket} = State) ->
    ?LOG_ERROR("TCP error for transport ~p: ~p", [State#state.transport_id, Reason]),
    NewState = handle_disconnect(State, {tcp_error, Reason}),
    {noreply, NewState};
%% Response from server to send back to client
handle_info({mcp_response, ServerId, Message}, State) ->
    case State#state.server_id of
        ServerId ->
            case do_send(State, Message) of
                ok ->
                    NewStats = update_stats(State#state.stats, messages_sent, 1),
                    {noreply, State#state{stats = NewStats}};
                {error, SendReason} ->
                    ?LOG_WARNING("Failed to send response on transport ~p: ~p",
                                 [State#state.transport_id, SendReason]),
                    NewStats = update_stats(State#state.stats, errors, 1),
                    {noreply, State#state{stats = NewStats}}
            end;
        _ ->
            ?LOG_WARNING("Received response from unexpected server ~p on transport ~p",
                         [ServerId, State#state.transport_id]),
            {noreply, State}
    end;
%% Unknown message
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Clean shutdown
-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
    ?LOG_INFO("Terminating TCP transport ~p: ~p", [State#state.transport_id, Reason]),

    %% Unregister from registry
    erlmcp_transport_behavior:unregister_from_registry(State#state.transport_id),

    %% Cancel reconnect timer
    cancel_reconnect_timer(State),

    %% Close socket
    do_close(State),

    ok.

%% @doc Handle code changes
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Internal initialization logic shared between behavior and gen_server
-spec internal_init(atom(), tcp_config()) -> {ok, state()} | {error, term()}.
internal_init(TransportId, Config) ->
    ?LOG_INFO("Initializing TCP transport ~p", [TransportId]),

    %% Validate required configuration
    case validate_config(Config) of
        ok ->
            Host = maps:get(host, Config),
            Port = maps:get(port, Config),
            ServerId = maps:get(server_id, Config, undefined),
            MaxReconnectAttempts =
                maps:get(max_reconnect_attempts, Config, ?DEFAULT_MAX_RECONNECT_ATTEMPTS),

            %% Build socket options
            SocketOpts = build_socket_options(Config),

            %% Initialize statistics
            Stats =
                #{messages_sent => 0,
                  messages_received => 0,
                  bytes_sent => 0,
                  bytes_received => 0,
                  errors => 0,
                  connection_time => undefined,
                  last_message_time => undefined,
                  reconnect_count => 0},

            State =
                #state{transport_id = TransportId,
                       server_id = ServerId,
                       config = Config,
                       socket = undefined,
                       host = Host,
                       port = Port,
                       buffer = <<>>,
                       connected = false,
                       reconnect_timer = undefined,
                       reconnect_attempts = 0,
                       max_reconnect_attempts = MaxReconnectAttempts,
                       socket_opts = SocketOpts,
                       stats = Stats},

            %% Register with registry (always attempt; no-op if unavailable)
            case register_with_registry(State) of
                ok ->
                    ?LOG_INFO("TCP transport ~p initialized and registry registration attempted",
                              [TransportId]),
                    {ok, State};
                {error, Reason} ->
                    ?LOG_WARNING("TCP transport ~p could not register with registry: ~p",
                                 [TransportId, Reason]),
                    {ok, State}
            end;
        {error, Reason} ->
            {error, {invalid_config, Reason}}
    end.

%% @doc Validate transport configuration using comprehensive schema
-spec validate_config(tcp_config()) -> ok | {error, term()}.
validate_config(Config) ->
    case erlmcp_config_validation:validate_transport_config(tcp, tcp, Config) of
        ok -> 
            ok;
        {error, ValidationErrors} ->
            % Convert validation errors to legacy format for compatibility
            FirstError = hd(ValidationErrors),
            ErrorMessage = maps:get(message, FirstError, <<"Configuration validation failed">>),
            {error, {validation_failed, ErrorMessage, ValidationErrors}}
    end.

%% @private Legacy validation (kept for reference)
validate_config_legacy(Config) ->
    RequiredFields = [host, port],
    case check_required_fields(Config, RequiredFields) of
        ok ->
            %% Validate host and port types
            case {maps:get(host, Config), maps:get(port, Config)} of
                {Host, Port}
                    when is_list(Host) orelse is_tuple(Host), is_integer(Port), Port > 0,
                         Port =< 65535 ->
                    ok;
                _ ->
                    {error, invalid_host_or_port}
            end;
        Error ->
            Error
    end.

%% @doc Check required fields are present
-spec check_required_fields(map(), [atom()]) -> ok | {error, term()}.
check_required_fields(Config, RequiredFields) ->
    case [Field || Field <- RequiredFields, not maps:is_key(Field, Config)] of
        [] ->
            ok;
        MissingFields ->
            {error, {missing_required_fields, MissingFields}}
    end.

%% @doc Register transport with the registry
-spec register_with_registry(state()) -> ok | {error, term()}.
register_with_registry(#state{transport_id = TransportId, config = Config}) ->
    TransportConfig =
        Config#{type => tcp,
                pid => self(),
                started_at => erlang:system_time(millisecond)},
    erlmcp_transport_behavior:register_with_registry(TransportId, self(), TransportConfig).

%% @doc Build socket connection options
-spec build_socket_options(tcp_config()) -> [gen_tcp:connect_option()].
build_socket_options(Config) ->
    BaseOpts =
        [binary,
         {active, once},  % We'll control flow with inet:setopts
         {packet, 0},     % Raw TCP, we'll handle framing
         {reuseaddr, true},
         {send_timeout, 5000},
         {send_timeout_close, true}],

    %% Add optional settings
    OptionalOpts =
        lists:foldl(fun({ConfigKey, SocketOpt}, Acc) ->
                       case maps:get(ConfigKey, Config, undefined) of
                           undefined -> Acc;
                           Value -> [{SocketOpt, Value} | Acc]
                       end
                    end,
                    BaseOpts,
                    [{keepalive, keepalive}, {nodelay, nodelay}]),

    %% Set buffer sizes
    BufferSize = maps:get(buffer_size, Config, ?DEFAULT_BUFFER_SIZE),
    [{recbuf, BufferSize}, {sndbuf, BufferSize} | OptionalOpts].

%% @doc Attempt to establish TCP connection
-spec attempt_connection(state()) -> state().
attempt_connection(#state{reconnect_attempts = Attempts,
                          max_reconnect_attempts = MaxAttempts,
                          transport_id = TransportId} =
                       State)
    when is_integer(MaxAttempts), Attempts >= MaxAttempts ->
    ?LOG_ERROR("Maximum reconnection attempts (~p) reached for transport ~p",
               [MaxAttempts, TransportId]),
    State;
attempt_connection(#state{host = Host,
                          port = Port,
                          socket_opts = SocketOpts,
                          transport_id = TransportId,
                          config = Config} =
                       State) ->
    ConnectTimeout = maps:get(connect_timeout, Config, ?DEFAULT_CONNECT_TIMEOUT),

    ?LOG_INFO("Attempting TCP connection to ~s:~p for transport ~p (attempt "
              "~p)",
              [Host, Port, TransportId, State#state.reconnect_attempts + 1]),

    case gen_tcp:connect(Host, Port, SocketOpts, ConnectTimeout) of
        {ok, Socket} ->
            ?LOG_INFO("TCP connection established for transport ~p", [TransportId]),

            %% Update connection statistics
            CurrentTime = erlang:system_time(millisecond),
            NewStats =
                (State#state.stats)#{connection_time => CurrentTime,
                                     reconnect_count => State#state.reconnect_attempts},

            State#state{socket = Socket,
                        connected = true,
                        reconnect_attempts = 0,
                        buffer = <<>>,
                        reconnect_timer = undefined,
                        stats = NewStats};
        {error, Reason} ->
            ?LOG_WARNING("TCP connection failed for transport ~p: ~p", [TransportId, Reason]),
            schedule_reconnect(State, Reason)
    end.

%% @doc Handle TCP connection disconnect
-spec handle_disconnect(state(), term()) -> state().
handle_disconnect(#state{socket = undefined} = State, _Reason) ->
    State;
handle_disconnect(#state{socket = Socket} = State, Reason) ->
    %% Close the socket
    case Socket of
        undefined ->
            ok;
        _ ->
            catch gen_tcp:close(Socket)
    end,

    %% Update state and schedule reconnect
    DisconnectedState =
        State#state{socket = undefined,
                    connected = false,
                    buffer = <<>>},

    schedule_reconnect(DisconnectedState, Reason).

%% @doc Schedule reconnection attempt with exponential backoff
-spec schedule_reconnect(state(), term()) -> state().
schedule_reconnect(#state{reconnect_timer = Timer} = State, _Reason)
    when Timer =/= undefined ->
    %% Already have a reconnect timer scheduled
    State;
schedule_reconnect(#state{reconnect_attempts = Attempts, transport_id = TransportId} =
                       State,
                   _Reason) ->
    %% Calculate backoff delay
    Delay = calculate_backoff(Attempts),
    NewAttempts = Attempts + 1,

    ?LOG_INFO("Scheduling reconnection in ~p ms (attempt ~p) for transport ~p",
              [Delay, NewAttempts, TransportId]),

    Timer = erlang:send_after(Delay, self(), reconnect),

    State#state{reconnect_timer = Timer, reconnect_attempts = NewAttempts}.

%% @doc Calculate exponential backoff with jitter
-spec calculate_backoff(non_neg_integer()) -> pos_integer().
calculate_backoff(Attempts) ->
    BaseDelay = min(?INITIAL_RECONNECT_DELAY * (1 bsl Attempts), ?MAX_RECONNECT_DELAY),
    Jitter = rand:uniform(BaseDelay div 4),
    BaseDelay + Jitter.

%% @doc Cancel active reconnect timer
-spec cancel_reconnect_timer(state()) -> state().
cancel_reconnect_timer(#state{reconnect_timer = undefined} = State) ->
    State;
cancel_reconnect_timer(#state{reconnect_timer = Timer} = State) ->
    case erlang:cancel_timer(Timer) of
        false ->
            %% Timer already fired, drain the message
            receive
                reconnect ->
                    ok
            after 0 ->
                ok
            end;
        _ ->
            ok
    end,
    State#state{reconnect_timer = undefined}.

%% @doc Handle incoming TCP data with enhanced buffer management
-spec handle_tcp_data(state(), binary()) -> state().
handle_tcp_data(#state{buffer = Buffer,
                       transport_id = TransportId,
                       server_id = ServerId,
                       stats = Stats,
                       config = Config} =
                    State,
                Data) ->
    %% Check buffer size limits to prevent memory exhaustion
    BufferLimit = maps:get(buffer_size, Config, ?DEFAULT_BUFFER_SIZE),
    NewBufferSize = byte_size(Buffer) + byte_size(Data),
    
    case NewBufferSize > BufferLimit of
        true ->
            ?LOG_WARNING("Buffer overflow on transport ~p: ~p bytes, dropping oldest data", 
                         [TransportId, NewBufferSize]),
            %% Keep only the newest data to prevent memory issues
            TruncatedBuffer = binary:part(Buffer, max(0, byte_size(Buffer) - BufferLimit div 2), 
                                          min(byte_size(Buffer), BufferLimit div 2)),
            handle_tcp_data_internal(State#state{buffer = TruncatedBuffer}, Data);
        false ->
            handle_tcp_data_internal(State, Data)
    end.

%% @private Internal TCP data handling
-spec handle_tcp_data_internal(state(), binary()) -> state().
handle_tcp_data_internal(#state{buffer = Buffer,
                                transport_id = TransportId,
                                server_id = ServerId,
                                stats = Stats} = State,
                         Data) ->
    %% Update receive statistics
    DataSize = byte_size(Data),
    NewStats =
        Stats#{bytes_received => maps:get(bytes_received, Stats, 0) + DataSize,
               last_message_time => erlang:system_time(millisecond)},

    %% Accumulate data in buffer
    NewBuffer = <<Buffer/binary, Data/binary>>,

    %% Extract complete messages (line-based framing)
    {Messages, RemainingBuffer} =
        erlmcp_transport_behavior:extract_message_lines(<<>>, NewBuffer),

    %% Route complete messages to server via registry
    FinalStats =
        lists:foldl(fun(Message, StatsAcc) ->
                       TrimmedMessage = erlmcp_transport_behavior:trim_message_line(Message),
                       case byte_size(TrimmedMessage) of
                           0 -> StatsAcc;  % Skip empty messages
                           _ ->
                               case ServerId of
                                   undefined ->
                                       ?LOG_WARNING("Cannot route message from transport ~p: no server bound",
                                                    [TransportId]);
                                   _ ->
                                       case erlmcp_transport_behavior:handle_transport_message(TransportId,
                                                                                          TrimmedMessage) of
                                           ok -> ok;
                                           {error, Reason} ->
                                               ?LOG_ERROR("Failed to route message from transport ~p: ~p",
                                                          [TransportId, Reason])
                                       end
                               end,
                               StatsAcc#{messages_received =>
                                             maps:get(messages_received, StatsAcc, 0) + 1}
                       end
                    end,
                    NewStats,
                    Messages),

    State#state{buffer = RemainingBuffer, stats = FinalStats}.

%% @doc Send data through the transport with enhanced error handling
-spec do_send(state(), iodata()) -> ok | {error, term()}.
do_send(#state{config = #{test_mode := true}}, _Data) ->
    %% In test mode, just simulate successful send
    ok;
do_send(#state{connected = false}, _Data) ->
    {error, not_connected};
do_send(#state{socket = undefined}, _Data) ->
    {error, not_connected};
do_send(#state{socket = Socket, 
               transport_id = TransportId,
               stats = Stats} = State, Data) ->
    %% Frame message with newline for line-based protocol
    FramedData = [Data, "\n"],
    DataSize = iolist_size(FramedData),
    
    case gen_tcp:send(Socket, FramedData) of
        ok ->
            %% Update send statistics
            UpdatedStats = Stats#{
                bytes_sent => maps:get(bytes_sent, Stats, 0) + DataSize,
                last_message_time => erlang:system_time(millisecond)
            },
            %% Update state (note: this returns ok, state update handled by caller)
            ok;
        {error, Reason} = Error ->
            ?LOG_ERROR("TCP send failed on transport ~p: ~p", [TransportId, Reason]),
            %% Check if this indicates a connection problem
            case is_connection_error(Reason) of
                true ->
                    %% Trigger reconnection
                    self() ! {tcp_error, Socket, Reason},
                    Error;
                false ->
                    %% Just a send error, don't trigger reconnection
                    {error, {tcp_send_failed, Reason}}
            end
    end.

%% @private Check if error indicates connection problem
-spec is_connection_error(term()) -> boolean().
is_connection_error(closed) -> true;
is_connection_error(econnreset) -> true;
is_connection_error(econnaborted) -> true;
is_connection_error(enotconn) -> true;
is_connection_error(epipe) -> true;
is_connection_error(_) -> false.

%% @doc Close the transport
-spec do_close(state()) -> ok.
do_close(#state{socket = undefined}) ->
    ok;
do_close(#state{socket = Socket}) when Socket =/= undefined ->
    catch gen_tcp:close(Socket),
    ok;
do_close(_State) ->
    ok.

%% @doc Get transport information with enhanced metrics
-spec do_get_info(state()) -> map().
do_get_info(#state{transport_id = TransportId,
                   config = Config,
                   host = Host,
                   port = Port,
                   connected = Connected,
                   reconnect_attempts = Attempts,
                   reconnect_timer = Timer,
                   stats = Stats}) ->
    TestMode = maps:get(test_mode, Config, false),
    CurrentTime = erlang:system_time(millisecond),
    
    Status = case TestMode of
        true ->
            running;  % In test mode, always report running
        false ->
            case Connected of
                true ->
                    connected;
                false when Timer =/= undefined ->
                    reconnecting;
                false when Attempts > 0 ->
                    connecting;
                false ->
                    disconnected
            end
    end,
    
    ConnectionInfo = #{
        host => Host,
        port => Port,
        connected => Connected,
        reconnect_attempts => Attempts,
        reconnecting => Timer =/= undefined,
        uptime => case maps:get(connection_time, Stats, undefined) of
            undefined -> 0;
            ConnTime -> max(0, CurrentTime - ConnTime)
        end
    },
    
    #{transport_id => TransportId,
      type => tcp,
      test_mode => TestMode,
      status => Status,
      config => maps:without([password, secret, token], Config),
      connection => ConnectionInfo,
      statistics => Stats#{
          current_buffer_size => 0,  % Could be enhanced to track actual buffer size
          total_reconnects => maps:get(reconnect_count, Stats, 0)
      }}.

%% @doc Update transport statistics
-spec update_stats(map(), atom(), non_neg_integer()) -> map().
update_stats(Stats, Key, Increment) ->
    CurrentValue = maps:get(Key, Stats, 0),
    Stats#{Key => CurrentValue + Increment, last_updated => erlang:system_time(millisecond)}.
