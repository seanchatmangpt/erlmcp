%%%-------------------------------------------------------------------
%%% @doc
%%% Connection Tracker
%%% Tracks connection creation, closure, unexpected terminations
%%% and recovery during chaos testing
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_connection_tracker).

-behaviour(gen_server).

%% API
-export([
    start/0,
    track_connection_open/2,
    track_connection_close/2,
    track_unexpected_closure/2,
    track_reconnect/2,
    get_created_count/0,
    get_closed_count/0,
    get_unexpected_closures/0,
    get_reconnected_count/0,
    reset/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    connections = #{},      % ConnectionId -> {status, open_time, close_time}
    created_count = 0,
    closed_count = 0,
    unexpected_closures = 0,
    reconnected_count = 0,
    closure_history = []
}).

%%====================================================================
%% API
%%====================================================================

start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

track_connection_open(Tracker, ConnectionId) ->
    gen_server:cast(Tracker, {open, ConnectionId, erlang:monotonic_time(millisecond)}).

track_connection_close(Tracker, ConnectionId) ->
    gen_server:cast(Tracker, {close, ConnectionId, erlang:monotonic_time(millisecond)}).

track_unexpected_closure(Tracker, ConnectionId) ->
    gen_server:cast(Tracker, {unexpected_close, ConnectionId, erlang:monotonic_time(millisecond)}).

track_reconnect(Tracker, ConnectionId) ->
    gen_server:cast(Tracker, {reconnect, ConnectionId, erlang:monotonic_time(millisecond)}).

get_created_count() ->
    gen_server:call(?MODULE, get_created).

get_closed_count() ->
    gen_server:call(?MODULE, get_closed).

get_unexpected_closures() ->
    gen_server:call(?MODULE, get_unexpected).

get_reconnected_count() ->
    gen_server:call(?MODULE, get_reconnected).

reset() ->
    gen_server:call(?MODULE, reset).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call(get_created, _From, State) ->
    {reply, State#state.created_count, State};

handle_call(get_closed, _From, State) ->
    {reply, State#state.closed_count, State};

handle_call(get_unexpected, _From, State) ->
    {reply, State#state.unexpected_closures, State};

handle_call(get_reconnected, _From, State) ->
    {reply, State#state.reconnected_count, State};

handle_call(reset, _From, _State) ->
    {reply, ok, #state{}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({open, ConnectionId, OpenTime}, State) ->
    Connections = State#state.connections,
    NewConnections = maps:put(ConnectionId, {open, OpenTime, undefined}, Connections),
    NewState = State#state{
        connections = NewConnections,
        created_count = State#state.created_count + 1
    },
    {noreply, NewState};

handle_cast({close, ConnectionId, CloseTime}, State) ->
    Connections = State#state.connections,
    case maps:get(ConnectionId, Connections, undefined) of
        {open, OpenTime, _} ->
            NewConnections = maps:put(ConnectionId, {closed, OpenTime, CloseTime}, Connections),
            NewState = State#state{
                connections = NewConnections,
                closed_count = State#state.closed_count + 1
            },
            {noreply, NewState};
        _ ->
            {noreply, State}
    end;

handle_cast({unexpected_close, ConnectionId, CloseTime}, State) ->
    Connections = State#state.connections,
    ClosureHistory = State#state.closure_history,

    NewClosureHistory = [
        {ConnectionId, unexpected, CloseTime} | ClosureHistory
    ],

    NewConnections = case maps:get(ConnectionId, Connections, undefined) of
        {open, OpenTime, _} ->
            maps:put(ConnectionId, {unexpected_closed, OpenTime, CloseTime}, Connections);
        undefined ->
            maps:put(ConnectionId, {unexpected_closed, CloseTime, CloseTime}, Connections);
        Other ->
            maps:put(ConnectionId, Other, Connections)
    end,

    NewState = State#state{
        connections = NewConnections,
        unexpected_closures = State#state.unexpected_closures + 1,
        closure_history = NewClosureHistory
    },
    {noreply, NewState};

handle_cast({reconnect, ConnectionId, ReconnectTime}, State) ->
    Connections = State#state.connections,
    NewConnections = case maps:get(ConnectionId, Connections, undefined) of
        {unexpected_closed, OpenTime, _} ->
            maps:put(ConnectionId, {open, ReconnectTime, undefined}, Connections);
        {closed, OpenTime, _} ->
            maps:put(ConnectionId, {open, ReconnectTime, undefined}, Connections);
        Other ->
            maps:put(ConnectionId, Other, Connections)
    end,

    NewState = State#state{
        connections = NewConnections,
        reconnected_count = State#state.reconnected_count + 1
    },
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
