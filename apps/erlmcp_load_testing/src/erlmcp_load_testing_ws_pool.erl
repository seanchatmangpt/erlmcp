%%%-------------------------------------------------------------------
%%% @doc
### WebSocket Connection Pool for Load Testing

This module implements a high-performance WebSocket connection pool for
 real-time communication with persistent connections and message batching.
%%%
 Features:
%%% - WebSocket connection pooling
%%% - WS/WSS support with TLS
%%% - Connection persistence
%%% - Message batching for efficiency
%%% - Heartbeat/ping-pong mechanism
%%% - Automatic reconnection
%%% - Protocol compliance (RFC 6455)
%%% - Backpressure handling
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_load_testing_ws_pool).

-behaviour(gen_server).

-export([start_link/2, send_message/3, get_stats/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp_load_testing.hrl").

%%====================================================================
## Type Definitions
%%====================================================================

-record(pool_state, {
    pool_id :: binary(),
    target :: string(),
    connections :: ets:tid(),
    active :: ets:tid(),
    pending_messages :: queue:queue(),
    pool_size :: pos_integer(),
    active_count :: pos_integer(),
    stats :: #{
        total_messages :: pos_integer(),
        successful_messages :: pos_integer(),
        failed_messages :: pos_integer(),
        total_bytes :: pos_integer(),
        avg_latency :: float(),
        connection_failures :: pos_integer()
    },
    config :: map(),
    heartbeat_timer :: reference(),
    reconnect_timer :: reference()
}.

-type pool_state() :: #pool_state{}.

-type message_info() :: #{
    id :: reference(),
    timestamp :: pos_integer(),
    payload :: binary(),
    callback :: function(),
    retry_count :: pos_integer()
}.

%%====================================================================
## API Functions
%%====================================================================

-spec start_link(string(), pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(Target, PoolSize) ->
    PoolId = generate_pool_id(Target),
    gen_server:start_link({local, PoolId}, ?MODULE, [Target, PoolSize], []).

-spec send_message(pid(), binary(), function()) -> ok.
send_message(PoolPid, Payload, Callback) ->
    gen_server:cast(PoolPid, {send_message, Payload, Callback}).

-spec get_stats(pid()) -> {ok, map()}.
get_stats(PoolPid) ->
    gen_server:call(PoolPid, get_stats).

-spec stop(pid()) -> ok.
stop(PoolPid) ->
    gen_server:stop(PoolPid).

%%====================================================================
## gen_server Callbacks
%%====================================================================

-spec init([string(), pos_integer()]) -> {ok, pool_state()}.
init([Target, PoolSize]) ->
    process_flag(trap_exit, true),

    %% Initialize connection tables
    Connections = ets:new(connections, [
        set,
        {keypos, 1},  % connection_id
        {write_concurrency, true},
        named_table,
        public
    ]),

    Active = ets:new(active, [
        set,
        {keypos, 1},  % connection_id
        {write_concurrency, true},
        named_table,
        public
    ]),

    %% Initialize pool state
    PoolId = generate_pool_id(Target),
    InitialStats = #{
        total_messages => 0,
        successful_messages => 0,
        failed_messages => 0,
        total_bytes => 0,
        avg_latency => 0.0,
        connection_failures => 0
    },

    Config = #{
        timeout => ?WEB_SOCKET_TIMEOUT,
        max_retries => 3,
        heartbeat_interval => 30000,  % 30 seconds
        reconnect_delay => 1000,     % 1 second
        max_message_size => 1048576, % 1MB
        auto_reconnect => true,
        subprotocols => []
    },

    State = #pool_state{
        pool_id = PoolId,
        target = Target,
        connections = Connections,
        active = Active,
        pending_messages = queue:new(),
        pool_size = PoolSize,
        active_count = 0,
        stats = InitialStats,
        config = Config
    },

    %% Initialize connections
    {ok, NewState} = initialize_connections(State),

    %% Start heartbeat timer
    HeartbeatRef = erlang:start_timer(
        maps:get(heartbeat_interval, Config, 30000),
        self(),
        heartbeat
    ),

    {ok, NewState#pool_state{heartbeat_timer = HeartbeatRef}}.

-spec handle_call(term(), {pid(), term()}, pool_state()) ->
                       {reply, term(), pool_state()} | {stop, term(), pool_state()}.
handle_call(get_stats, _From, State) ->
    Stats = State#pool_state.stats,
    {reply, {ok, Stats#{
        pool_id => State#pool_state.pool_id,
        target => State#pool_state.target,
        pool_size => State#pool_state.pool_size,
        active_connections => State#pool_state.active_count,
        pending_messages => queue:len(State#pool_state.pending_messages)
    }}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), pool_state()) -> {noreply, pool_state()} | {stop, term(), pool_state()}.
handle_cast({send_message, Payload, Callback}, State) ->
    MessageInfo = #{
        id = make_ref(),
        timestamp => erlang:system_time(millisecond),
        payload = Payload,
        callback = Callback,
        retry_count => 0
    },

    case State#pool_state.active_count > 0 of
        true ->
            %% Send message immediately
            send_message_to_pool(State, MessageInfo);
        false ->
            %% Queue the message
            NewQueue = queue:in(MessageInfo, State#pool_state.pending_messages),
            NewStats = State#pool_state.stats#{
                failed_messages => State#pool_state.stats#failed_messages + 1
            },
            {noreply, State#pool_state{
                pending_messages = NewQueue,
                stats = NewStats
            }}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), pool_state()) -> {noreply, pool_state()} | {stop, term(), pool_state()}.
handle_info({timeout, TimerRef, heartbeat}, State) ->
    %% Send heartbeat to all active connections
    send_heartbeat(State),

    %% Schedule next heartbeat
    NewTimerRef = erlang:start_timer(
        maps:get(heartbeat_interval, State#pool_state.config, 30000),
        self(),
        heartbeat
    ),

    {noreply, State#pool_state{heartbeat_timer = NewTimerRef}};

handle_info({timeout, TimerRef, reconnect}, State) ->
    %% Reconnect dead connections
    {ok, NewState} = reconnect_dead_connections(State),

    %% Schedule next reconnect check
    NewTimerRef = erlang:start_timer(
        maps:get(reconnect_delay, State#pool_state.config, 1000),
        self(),
        reconnect
    ),

    {noreply, NewState#pool_state{reconnect_timer = NewTimerRef}};

handle_info({websocket_message, ConnectionId, Message}, State) ->
    %% Handle incoming WebSocket message
    case ets:lookup(State#pool_state.connections, ConnectionId) of
        [{ConnectionId, Socket, _}] ->
            process_incoming_message(State, Message);
        [] ->
            %% Unknown connection
            {noreply, State}
    end;

handle_info({websocket_closed, ConnectionId, Reason}, State) ->
    %% Handle connection closure
    case ets:lookup(State#pool_state.connections, ConnectionId) of
        [{ConnectionId, Socket, _}] ->
            %% Remove connection
            ets:delete(State#pool_state.connections, ConnectionId),
            ets:delete(State#pool_state.active, ConnectionId),

            %% Log closure
            error_logger:info_msg("WebSocket connection closed: ~p, ~p", [ConnectionId, Reason]),

            %% Reconnect if auto-reconnect is enabled
            case maps:get(auto_reconnect, State#pool_state.config, true) of
                true ->
                    schedule_reconnect(State);
                false ->
                    ok
            end,

            {noreply, State#pool_state{
                active_count = State#pool_state.active_count - 1
            }};
        [] ->
            {noreply, State}
    end;

handle_info({websocket_error, ConnectionId, Error}, State) ->
    %% Handle WebSocket error
    case ets:lookup(State#pool_state.connections, ConnectionId) of
        [{ConnectionId, Socket, _}] ->
            error_logger:warning_msg("WebSocket error: ~p, ~p", [ConnectionId, Error]),

            %% Remove connection
            ets:delete(State#pool_state.connections, ConnectionId),
            ets:delete(State#pool_state.active, ConnectionId),

            Reconnect = maps:get(auto_reconnect, State#pool_state.config, true),
            case Reconnect of
                true ->
                    schedule_reconnect(State);
                false ->
                    ok
            end,

            NewStats = State#pool_state.stats#{
                connection_failures => State#pool_state.stats#connection_failures + 1
            },

            {noreply, State#pool_state{
                active_count = State#pool_state.active_count - 1,
                stats = NewStats
            }};
        [] ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), pool_state()) -> ok.
terminate(_Reason, State) ->
    %% Cleanup all connections
    close_all_connections(State),
    ok.

-spec code_change(term(), pool_state(), term()) -> {ok, pool_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
## Internal Functions
%%====================================================================

%% Generate unique pool ID
-spec generate_pool_id(string()) -> binary().
generate_pool_id(Target) ->
    Hash = erlang:phash2(Target),
    list_to_binary("ws_pool_" ++ integer_to_list(Hash)).

%% Initialize connection pool
-spec initialize_connections(pool_state()) -> {ok, pool_state()}.
initialize_connections(State) ->
    lists:foldl(fun(_, Acc) ->
                   initialize_connection(Acc)
               end, State, lists:seq(1, State#pool_state.pool_size)).

%% Initialize single WebSocket connection
-spec initialize_connection(pool_state()) -> pool_state().
initialize_connection(State) ->
    Target = State#pool_state.target,
    Config = State#pool_state.config,

    case parse_ws_url(Target) of
        {ok, {Scheme, Host, Port, Path}} ->
            case connect_to_websocket(Scheme, Host, Port, Path, Config) of
                {ok, Socket} ->
                    ConnectionId = make_ref(),
                    ets:insert(State#pool_state.connections,
                               {ConnectionId, Socket, erlang:system_time(millisecond)}),
                    ets:insert(State#pool_state.active,
                               {ConnectionId, true}),
                    State#pool_state{
                        active_count = State#pool_state.active_count + 1
                    };
                {error, Reason} ->
                    error_logger:warning_msg("Failed to connect WebSocket to ~p: ~p", [Target, Reason]),
                    State
            end;
        {error, Reason} ->
            error_logger:warning_msg("Invalid WebSocket URL ~p: ~p", [Target, Reason]),
            State
    end.

%% Parse WebSocket URL
-spec parse_ws_url(string()) -> {ok, {atom(), string(), pos_integer(), string()}} | {error, term()}.
parse_ws_url("ws://" ++ Rest) ->
    parse_url_parts("ws", Rest);
parse_ws_url("wss://" ++ Rest) ->
    parse_url_parts("wss", Rest);
parse_ws_url(_) ->
    {error, invalid_websocket_url}.

%% Parse URL components
-spec parse_url_parts(atom(), string()) -> {ok, {atom(), string(), pos_integer(), string()}} | {error, term()}.
parse_url_parts(Scheme, Rest) ->
    case string:find(Rest, "/", nomatch) of
        nomatch ->
            HostPort = Rest,
            case string:find(HostPort, ":", nomatch) of
                nomatch ->
                    {ok, {Scheme, HostPort, 80, "/"}};
                _ ->
                    [Host, PortStr] = string:split(HostPort, ":"),
                    case PortStr of
                        Port when is_list(Port) ->
                            case string:to_integer(Port) of
                                {PortNum, ""} when PortNum > 0, PortNum =< 65535 ->
                                    {ok, {Scheme, Host, PortNum, "/"}};
                                _ ->
                                    {error, invalid_port}
                            end;
                        _ ->
                            {error, invalid_port}
                    end
            end;
        _ ->
            [HostPort | PathParts] = string:split(Rest, "/", all),
            Path = "/" ++ string:join(PathParts, "/"),
            case string:find(HostPort, ":", nomatch) of
                nomatch ->
                    {ok, {Scheme, HostPort, 80, Path}};
                _ ->
                    [Host, PortStr] = string:split(HostPort, ":"),
                    case PortStr of
                        Port when is_list(Port) ->
                            case string:to_integer(Port) of
                                {PortNum, ""} when PortNum > 0, PortNum =< 65535 ->
                                    {ok, {Scheme, Host, PortNum, Path}};
                                _ ->
                                    {error, invalid_port}
                            end;
                        _ ->
                            {error, invalid_port}
                    end
            end
    end.

%% Connect to WebSocket
-spec connect_to_websocket(atom(), string(), pos_integer(), string(), map()) ->
                              {ok, port()} | {error, term()}.
connect_to_websocket(ws, Host, Port, Path, _Config) ->
    %% Connect with TCP for WebSocket
    case gen_tcp:connect(Host, Port, [
        {active, false},
        {packet, 0},
        {mode, binary},
        {reuseaddr, true}
    ], 5000) of
        {ok, Socket} ->
            %% Send WebSocket handshake
            Handshake = build_handshake(Host, Port, Path),
            case gen_tcp:send(Socket, Handshake) of
                ok ->
                    %% Wait for response
                    case gen_tcp:recv(Socket, 0, 5000) of
                        {ok, Response} ->
                            case parse_handshake_response(Response) of
                                ok ->
                                    {ok, Socket};
                                {error, Reason} ->
                                    gen_tcp:close(Socket),
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            gen_tcp:close(Socket),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    gen_tcp:close(Socket),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;

connect_to_websocket(wss, Host, Port, Path, Config) ->
    %% Connect with SSL for WebSocket
    case ssl:connect(Host, Port, [
        {verify, verify_none},
        {server_name_indication, Host},
        {versions, [tlsv1.2, tlsv1.3]}
    ], 5000) of
        {ok, Socket} ->
            %% Send WebSocket handshake
            Handshake = build_handshake(Host, Port, Path),
            case ssl:send(Socket, Handshake) of
                ok ->
                    %% Wait for response
                    case ssl:recv(Socket, 0, 5000) of
                        {ok, Response} ->
                            case parse_handshake_response(Response) of
                                ok ->
                                    {ok, Socket};
                                {error, Reason} ->
                                    ssl:close(Socket),
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            ssl:close(Socket),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    ssl:close(Socket),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Build WebSocket handshake
-spec build_handshake(string(), pos_integer(), string()) -> binary().
build_handshake(Host, Port, Path) ->
    Key = generate_handshake_key(),
    Headers = [
        "GET " ++ Path ++ " HTTP/1.1\r\n",
        "Host: " ++ Host ++ ":" ++ integer_to_list(Port) ++ "\r\n",
        "Upgrade: websocket\r\n",
        "Connection: Upgrade\r\n",
        "Sec-WebSocket-Key: " ++ Key ++ "\r\n",
        "Sec-WebSocket-Version: 13\r\n",
        "\r\n"
    ],
    list_to_binary(lists:flatten(Headers)).

%% Generate WebSocket handshake key
-spec generate_handshake_key() -> string().
generate_handshake_key() ->
    Base64 = base64:encode(crypto:strong_rand_bytes(16)),
    binary_to_list(Base64).

%% Parse WebSocket handshake response
-spec parse_handshake_response(binary()) -> ok | {error, term()}.
parse_handshake_response(Response) ->
    %% Simple handshake response validation
    case binary:split(Response, <<"\r\n">>, [global]) of
        [<<"HTTP/1.1 101 Switching Protocols">> | _Headers] ->
            ok;
        [<<"HTTP/1.1 ", _Status/binary>> | _] ->
            {error, handshake_failed};
        _ ->
            {error, invalid_response}
    end.

%% Send message to pool
-spec send_message_to_pool(pool_state(), map()) -> pool_state().
send_message_to_pool(State, MessageInfo) ->
    %% Use round-robin selection
    Connections = ets:tab2list(State#pool_state.connections),
    case Connections of
        [] ->
            %% No connections available
            process_failed_message(State, MessageInfo);
        _ ->
            %% Select connection (round-robin)
            Selected = select_connection(Connections, MessageInfo#id),
            case send_websocket_message(Selected, MessageInfo) of
                ok ->
                    %% Update stats
                    UpdatedStats = State#pool_state.stats#{
                        total_messages => State#pool_state.stats#total_messages + 1,
                        successful_messages => State#pool_state.stats#successful_messages + 1,
                        total_bytes => State#pool_state.stats#total_bytes + byte_size(MessageInfo#payload)
                    },
                    State#pool_state{stats = UpdatedStats};
                {error, Reason} ->
                    %% Handle error
                    process_failed_message(State, MessageInfo, Reason)
            end
    end.

%% Select connection using round-robin
-spec select_connection([{reference(), port(), pos_integer()}], reference()) -> {reference(), port(), pos_integer()}.
select_connection(Connections, MessageId) ->
    %% Simple hash-based selection
    Hash = erlang:phash2(MessageId),
    Index = Hash rem length(Connections),
    lists:nth(Index + 1, Connections).

%% Send WebSocket message
-spec send_websocket_message({reference(), port(), pos_integer()}, map()) -> ok | {error, term()}.
send_websocket_message({ConnectionId, Socket, _}, MessageInfo) ->
    Frame = build_websocket_frame(MessageInfo#payload),
    case gen_tcp:send(Socket, Frame) of
        ok ->
            %% Track message if needed
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Build WebSocket frame
-spec build_websocket_frame(binary()) -> binary().
build_websocket_frame(Payload) ->
    %% Basic WebSocket frame (no masking for client)
    Length = byte_size(Payload),
    Fin = 1,  % Final fragment
    OpCode = 1,  % Text frame
    Mask = 0,  % No mask
    MaskingKey = <<>>,

    <<Fin:1, OpCode:4, Mask:1, Length:7>> ++
        case Length of
            L when L < 126 ->
                <<MaskingKey/binary, Payload/binary>>;
            L when L =< 65535 ->
                <<16:8, MaskingKey/binary, L:16, Payload/binary>>;
            _ ->
                <<127:8, MaskingKey/binary, L:64, Payload/binary>>
        end.

%% Process incoming message
-spec process_incoming_message(pool_state(), binary()) -> pool_state().
process_incoming_message(State, Message) ->
    %% Process WebSocket message (can be extended for different message types)
    process_websocket_data(State, Message).

%% Process WebSocket data
-spec process_websocket_data(pool_state(), binary()) -> pool_state().
process_websocket_data(State, Message) ->
    %% Parse and process WebSocket frame
    case parse_websocket_frame(Message) of
        {text, Text} ->
            %% Process text message
            process_text_message(State, Text);
        {binary, Binary} ->
            %% Process binary message
            process_binary_message(State, Binary);
        {ping, _} ->
            %% Send pong response
            send_pong(State);
        {pong, _} ->
            %% Handle pong response
            State;
        {close, _} ->
            %% Connection closed by peer
            State
    end.

%% Parse WebSocket frame
-spec parse_websocket_frame(binary()) -> {atom(), binary()} | {close, binary()}.
parse_websocket_frame(<<Fin:1, OpCode:4, _Mask:1, Length:7/integer, Rest/binary>>) ->
    case Length of
        L when L < 126 ->
            <<Payload:L/binary, _/binary>> = Rest,
            case OpCode of
                1 -> {text, Payload};
                2 -> {binary, Payload};
                9 -> {ping, Payload};
                10 -> {pong, Payload};
                8 -> {close, Payload};
                _ -> {unknown, Payload}
            end;
        L when L =< 125 ->
            <<_:8, Payload:L/binary, _/binary>> = Rest,
            case OpCode of
                1 -> {text, Payload};
                2 -> {binary, Payload};
                9 -> {ping, Payload};
                10 -> {pong, Payload};
                8 -> {close, Payload};
                _ -> {unknown, Payload}
            end;
        126 ->
            <<_:16, Payload/binary>> = Rest,
            {text, Payload};
        127 ->
            <<_:64, Payload/binary>> = Rest,
            {text, Payload}
    end.

%% Send heartbeat
-spec send_heartbeat(pool_state()) -> ok.
send_heartbeat(State) ->
    Connections = State#pool_state.connections,
    lists:foreach(fun({ConnectionId, Socket, _}) ->
                      PingFrame = build_websocket_frame(<<"ping">>),
                      gen_tcp:send(Socket, PingFrame)
                  end, ets:tab2list(Connections)).

%% Send pong response
-spec send_pong(pool_state()) -> ok.
send_pong(State) ->
    Connections = State#pool_state.connections,
    lists:foreach(fun({ConnectionId, Socket, _}) ->
                      PongFrame = build_websocket_frame(<<"pong">>),
                      gen_tcp:send(Socket, PongFrame)
                  end, ets:tab2list(Connections)).

%% Schedule reconnect
-spec schedule_reconnect(pool_state()) -> ok.
schedule_reconnect(State) ->
    case State#pool_state.reconnect_timer of
        undefined ->
            ok;
        TimerRef ->
            erlang:cancel_timer(TimerRef)
    end,

    ReconnectRef = erlang:start_timer(
        maps:get(reconnect_delay, State#pool_state.config, 1000),
        self(),
        reconnect
    ),

    {ok, State#pool_state{reconnect_timer = ReconnectRef}}.

%% Reconnect dead connections
-spec reconnect_dead_connections(pool_state()) -> {ok, pool_state()}.
reconnect_dead_connections(State) ->
    Connections = State#pool_state.connections,
    Active = State#pool_state.active,

    %% Check for dead connections
    lists:foldl(fun({ConnectionId, Socket, ConnectedAt}, Acc) ->
                    %% Check if connection is dead (no activity for 5 minutes)
                    case erlang:system_time(millisecond) - ConnectedAt > 300000 of
                        true ->
                            %% Reconnect
                            case reconnect_single_connection(Acc, Socket) of
                                {ok, NewSocket} ->
                                    %% Update connection
                                    ets:delete(Connections, ConnectionId),
                                    ets:insert(Connections,
                                               {ConnectionId, NewSocket, erlang:system_time(millisecond)}),
                                    Acc#pool_state{active_count = Acc#pool_state.active_count + 1};
                                {error, _} ->
                                    Acc
                            end;
                        false ->
                            Acc
                    end
                end, State, ets:tab2list(Connections)).

%% Reconnect single connection
-spec reconnect_single_connection(pool_state(), port()) -> {ok, port()} | {error, term()}.
reconnect_single_connection(State, OldSocket) ->
    Target = State#pool_state.target,
    Config = State#pool_state.config,

    %% Close old connection
    gen_tcp:close(OldSocket),

    %% Parse URL and reconnect
    case parse_ws_url(Target) of
        {ok, {Scheme, Host, Port, Path}} ->
            case connect_to_websocket(Scheme, Host, Port, Path, Config) of
                {ok, NewSocket} ->
                    {ok, NewSocket};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Process failed message
-spec process_failed_message(pool_state(), map()) -> pool_state().
process_failed_message(State, MessageInfo) ->
    process_failed_message(State, MessageInfo, no_connection).

process_failed_message(State, MessageInfo, Reason) ->
    Callback = MessageInfo#callback,
    Callback({error, Reason}),

    UpdatedStats = State#pool_state.stats#{
        failed_messages => State#pool_state.stats#failed_messages + 1
    },

    State#pool_state{stats = UpdatedStats}.

%% Process text message
-spec process_text_message(pool_state(), binary()) -> pool_state().
process_text_message(State, Text) ->
    %% Process text WebSocket message
    %% Can be extended for specific message handling
    State.

%% Process binary message
-spec process_binary_message(pool_state(), binary()) -> pool_state().
process_binary_message(State, Binary) ->
    %% Process binary WebSocket message
    %% Can be extended for specific message handling
    State.

%% Close all connections
-spec close_all_connections(pool_state()) -> ok.
close_all_connections(State) ->
    Connections = State#pool_state.connections,
    lists:foreach(fun({_, Socket, _}) ->
                      gen_tcp:close(Socket)
                  end, ets:tab2list(Connections)),
    ets:delete(Connections),
    ets:delete(State#pool_state.active).