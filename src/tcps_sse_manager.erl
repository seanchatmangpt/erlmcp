%%%-------------------------------------------------------------------
%%% @doc TCPS SSE Manager - Server-Sent Events connection pool and broadcasting
%%%
%%% Manages SSE connections and broadcasts real-time updates to connected clients.
%%% Provides connection pooling, automatic cleanup, and efficient broadcasting.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_sse_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([register_client/1, unregister_client/1]).
-export([broadcast_update/1, broadcast_to_client/2]).
-export([get_client_count/0, get_connection_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(HEARTBEAT_INTERVAL, 30000). %% 30 seconds
-define(CLIENT_TIMEOUT, 120000). %% 2 minutes

-record(client, {
    pid :: pid(),
    monitor_ref :: reference(),
    registered_at :: erlang:timestamp(),
    last_activity :: erlang:timestamp()
}).

-record(state, {
    clients = #{} :: #{pid() => #client{}},
    broadcast_count = 0 :: non_neg_integer(),
    heartbeat_timer :: reference() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Register SSE client
-spec register_client(pid()) -> ok.
register_client(Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {register_client, Pid}).

%% @doc Unregister SSE client
-spec unregister_client(pid()) -> ok.
unregister_client(Pid) when is_pid(Pid) ->
    gen_server:cast(?SERVER, {unregister_client, Pid}).

%% @doc Broadcast update to all connected clients
-spec broadcast_update(map()) -> ok.
broadcast_update(Update) when is_map(Update) ->
    gen_server:cast(?SERVER, {broadcast_update, Update}).

%% @doc Send update to specific client
-spec broadcast_to_client(pid(), map()) -> ok.
broadcast_to_client(Pid, Update) when is_pid(Pid), is_map(Update) ->
    gen_server:cast(?SERVER, {broadcast_to_client, Pid, Update}).

%% @doc Get count of connected clients
-spec get_client_count() -> non_neg_integer().
get_client_count() ->
    gen_server:call(?SERVER, get_client_count).

%% @doc Get connection statistics
-spec get_connection_stats() -> map().
get_connection_stats() ->
    gen_server:call(?SERVER, get_connection_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ?LOG_INFO("Starting TCPS SSE Manager"),

    %% Start heartbeat timer
    TimerRef = erlang:send_after(?HEARTBEAT_INTERVAL, self(), send_heartbeat),

    State = #state{
        heartbeat_timer = TimerRef
    },

    {ok, State}.

handle_call({register_client, Pid}, _From, State) ->
    %% Monitor client process
    MonitorRef = monitor(process, Pid),

    Client = #client{
        pid = Pid,
        monitor_ref = MonitorRef,
        registered_at = erlang:timestamp(),
        last_activity = erlang:timestamp()
    },

    NewClients = maps:put(Pid, Client, State#state.clients),

    ?LOG_INFO("SSE client registered: ~p (total: ~p)", [Pid, maps:size(NewClients)]),

    {reply, ok, State#state{clients = NewClients}};

handle_call(get_client_count, _From, State) ->
    Count = maps:size(State#state.clients),
    {reply, Count, State};

handle_call(get_connection_stats, _From, State) ->
    Stats = calculate_connection_stats(State),
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({unregister_client, Pid}, State) ->
    NewState = remove_client(Pid, State),
    {noreply, NewState};

handle_cast({broadcast_update, Update}, State) ->
    %% Broadcast to all clients
    Clients = maps:keys(State#state.clients),
    BroadcastCount = length(Clients),

    lists:foreach(fun(ClientPid) ->
        send_to_client(ClientPid, Update)
    end, Clients),

    ?LOG_DEBUG("Broadcast update to ~p clients", [BroadcastCount]),

    NewState = State#state{
        broadcast_count = State#state.broadcast_count + 1
    },

    {noreply, NewState};

handle_cast({broadcast_to_client, Pid, Update}, State) ->
    case maps:is_key(Pid, State#state.clients) of
        true ->
            send_to_client(Pid, Update),
            %% Update last activity
            Client = maps:get(Pid, State#state.clients),
            UpdatedClient = Client#client{last_activity = erlang:timestamp()},
            NewClients = maps:put(Pid, UpdatedClient, State#state.clients),
            {noreply, State#state{clients = NewClients}};
        false ->
            ?LOG_WARNING("Attempted to send to unregistered client: ~p", [Pid]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(send_heartbeat, State) ->
    %% Send heartbeat to all clients
    Clients = maps:keys(State#state.clients),

    Heartbeat = #{
        type => heartbeat,
        timestamp => erlang:system_time(millisecond),
        client_count => length(Clients)
    },

    lists:foreach(fun(ClientPid) ->
        send_to_client(ClientPid, Heartbeat)
    end, Clients),

    %% Cleanup stale clients
    NewState = cleanup_stale_clients(State),

    %% Schedule next heartbeat
    NewTimerRef = erlang:send_after(?HEARTBEAT_INTERVAL, self(), send_heartbeat),

    {noreply, NewState#state{heartbeat_timer = NewTimerRef}};

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    %% Client process died
    ?LOG_INFO("SSE client ~p disconnected: ~p", [Pid, Reason]),

    NewClients = maps:filter(fun(_ClientPid, Client) ->
        Client#client.monitor_ref =/= MonitorRef
    end, State#state.clients),

    {noreply, State#state{clients = NewClients}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel heartbeat timer
    case State#state.heartbeat_timer of
        undefined -> ok;
        TimerRef -> erlang:cancel_timer(TimerRef)
    end,

    %% Demonitor all clients
    maps:foreach(fun(_Pid, Client) ->
        demonitor(Client#client.monitor_ref, [flush])
    end, State#state.clients),

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Send update to client
send_to_client(Pid, Update) ->
    try
        Pid ! {sse_update, Update},
        ok
    catch
        _:_ ->
            ?LOG_WARNING("Failed to send to client ~p", [Pid]),
            error
    end.

%% @private Remove client from state
remove_client(Pid, State) ->
    case maps:find(Pid, State#state.clients) of
        {ok, Client} ->
            demonitor(Client#client.monitor_ref, [flush]),
            NewClients = maps:remove(Pid, State#state.clients),
            ?LOG_INFO("SSE client unregistered: ~p (remaining: ~p)", [Pid, maps:size(NewClients)]),
            State#state{clients = NewClients};
        error ->
            State
    end.

%% @private Cleanup stale clients (inactive for > CLIENT_TIMEOUT)
cleanup_stale_clients(State) ->
    Now = erlang:timestamp(),

    {StaleClients, ActiveClients} = maps:fold(fun(Pid, Client, {Stale, Active}) ->
        LastActivity = Client#client.last_activity,
        ElapsedMs = timer:now_diff(Now, LastActivity) div 1000,

        if
            ElapsedMs > ?CLIENT_TIMEOUT ->
                {[Pid | Stale], Active};
            true ->
                {Stale, maps:put(Pid, Client, Active)}
        end
    end, {[], #{}}, State#state.clients),

    %% Remove stale clients
    lists:foreach(fun(Pid) ->
        Client = maps:get(Pid, State#state.clients),
        demonitor(Client#client.monitor_ref, [flush]),
        ?LOG_INFO("Removed stale SSE client: ~p", [Pid])
    end, StaleClients),

    State#state{clients = ActiveClients}.

%% @private Calculate connection statistics
calculate_connection_stats(State) ->
    Now = erlang:timestamp(),

    ClientStats = maps:fold(fun(_Pid, Client, Acc) ->
        RegisteredAt = Client#client.registered_at,
        LastActivity = Client#client.last_activity,

        ConnectionTime = timer:now_diff(Now, RegisteredAt) div 1000000,
        IdleTime = timer:now_diff(Now, LastActivity) div 1000000,

        [#{
            connection_time_seconds => ConnectionTime,
            idle_time_seconds => IdleTime
        } | Acc]
    end, [], State#state.clients),

    %% Calculate averages
    TotalClients = length(ClientStats),
    AvgConnectionTime = case TotalClients of
        0 -> 0;
        _ -> lists:sum([maps:get(connection_time_seconds, S) || S <- ClientStats]) / TotalClients
    end,

    AvgIdleTime = case TotalClients of
        0 -> 0;
        _ -> lists:sum([maps:get(idle_time_seconds, S) || S <- ClientStats]) / TotalClients
    end,

    #{
        total_clients => TotalClients,
        broadcast_count => State#state.broadcast_count,
        avg_connection_time_seconds => AvgConnectionTime,
        avg_idle_time_seconds => AvgIdleTime,
        clients => ClientStats
    }.
