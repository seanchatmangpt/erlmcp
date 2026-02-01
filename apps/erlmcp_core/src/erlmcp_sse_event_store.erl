%%%-------------------------------------------------------------------
%%% @doc SSE Event Store (ETS-based Implementation)
%%%
%%% In-memory event store for Server-Sent Events with replay capability.
%%% Enables SSE stream resumption after network disconnects.
%%%
%%% Architecture:
%%% - ETS table per session (named: {erlmcp_sse_events, SessionId})
%%% - Events stored with event_number as key for ordering
%%% - Automatic cleanup of expired sessions
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sse_event_store).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([add_event/3, get_events_since/2, parse_event_id/1]).
-export([clear_session/1, cleanup_expired/0, get_session_info/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(EVENT_TTL, 3600000). %% 1 hour TTL per event
-define(CLEANUP_INTERVAL, 300000). %% 5 minutes

-record(state, {cleanup_timer :: reference() | undefined}).
-record(event,
        {event_number :: pos_integer(),
         event_id :: binary(),
         data :: binary(),
         timestamp :: integer()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Add an event to the store for a session
-spec add_event(binary(), pos_integer(), binary()) -> {ok, binary()}.
add_event(SessionId, EventNumber, Data) ->
    gen_server:call(?SERVER, {add_event, SessionId, EventNumber, Data}).

%% @doc Get all events since a given event ID (undefined = all events)
-spec get_events_since(binary(), binary() | undefined) -> {ok, [binary()]} | {error, term()}.
get_events_since(SessionId, LastEventId) ->
    gen_server:call(?SERVER, {get_events_since, SessionId, LastEventId}).

%% @doc Parse event ID to extract event number
%% Format: "session_<suffix>_<number>" or just "<number>"
-spec parse_event_id(binary()) -> non_neg_integer().
parse_event_id(EventId) when is_binary(EventId) ->
    case binary:split(EventId, <<"_">>, [global]) of
        Parts when length(Parts) >= 2 ->
            LastPart = lists:last(Parts),
            try
                binary_to_integer(LastPart)
            catch
                error:_ ->
                    0
            end;
        _ ->
            try
                binary_to_integer(EventId)
            catch
                error:_ ->
                    0
            end
    end;
parse_event_id(_) ->
    0.

%% @doc Clear all events for a session
-spec clear_session(binary()) -> ok.
clear_session(SessionId) ->
    gen_server:call(?SERVER, {clear_session, SessionId}).

%% @doc Cleanup expired events (called periodically)
-spec cleanup_expired() -> ok.
cleanup_expired() ->
    gen_server:cast(?SERVER, cleanup_expired).

%% @doc Get session information (event count, last event number, etc.)
-spec get_session_info(binary()) -> {ok, map()} | {error, term()}.
get_session_info(SessionId) ->
    gen_server:call(?SERVER, {get_session_info, SessionId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Start cleanup timer
    {ok, Ref} = timer:send_interval(?CLEANUP_INTERVAL, cleanup_expired),
    {ok, #state{cleanup_timer = Ref}}.

handle_call({add_event, SessionId, EventNumber, Data}, _From, State) ->
    TableId = get_or_create_table(SessionId),
    EventId = generate_event_id(SessionId, EventNumber),
    Event =
        #event{event_number = EventNumber,
               event_id = EventId,
               data = Data,
               timestamp = erlang:system_time(millisecond)},
    ets:insert(TableId, Event),
    {reply, {ok, EventId}, State};
handle_call({get_events_since, SessionId, LastEventId}, _From, State) ->
    TableName = {?MODULE, SessionId},
    case ets:whereis(TableName) of
        undefined ->
            {reply, {ok, []}, State};
        _TableId ->
            StartEventNum =
                case LastEventId of
                    undefined ->
                        0;
                    _ ->
                        parse_event_id(LastEventId)
                end,
            AllEvents = ets:tab2list(TableName),
            FilteredEvents = [E#event.data || E <- AllEvents, E#event.event_number > StartEventNum],
            SortedEvents = lists:sort(FilteredEvents),
            {reply, {ok, SortedEvents}, State}
    end;
handle_call({clear_session, SessionId}, _From, State) ->
    TableName = {?MODULE, SessionId},
    case ets:whereis(TableName) of
        undefined ->
            {reply, ok, State};
        _TableId ->
            ets:delete(TableName),
            {reply, ok, State}
    end;
handle_call({get_session_info, SessionId}, _From, State) ->
    TableName = {?MODULE, SessionId},
    case ets:whereis(TableName) of
        undefined ->
            {reply, {error, session_not_found}, State};
        _TableId ->
            EventCount = ets:info(TableName, size),
            LastEventNum =
                case ets:last(TableName) of
                    '$end_of_table' ->
                        0;
                    Key ->
                        Key
                end,
            Info =
                #{session_id => SessionId,
                  event_count => EventCount,
                  last_event_number => LastEventNum},
            {reply, {ok, Info}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(cleanup_expired, State) ->
    cleanup_expired_sessions(),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_expired, State) ->
    cleanup_expired_sessions(),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{cleanup_timer = Ref}) ->
    timer:cancel(Ref),
    %% Clean up all session tables
    Tables = ets:all(),
    lists:foreach(fun(Table) ->
                     case ets:info(Table, name) of
                         {?MODULE, _SessionId} ->
                             ets:delete(Table);
                         _ ->
                             ok
                     end
                  end,
                  Tables),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
%% @doc Get or create ETS table for a session
-spec get_or_create_table(binary()) -> ets:tid().
get_or_create_table(SessionId) ->
    TableName = {?MODULE, SessionId},
    case ets:whereis(TableName) of
        undefined ->
            ets:new(TableName,
                    [named_table,
                     set,
                     public,
                     {keypos, #event.event_number},
                     {read_concurrency, true},
                     {write_concurrency, true}]);
        _TableId ->
            TableName
    end.

%% @private
%% @doc Generate event ID: session_<suffix>_<number>
-spec generate_event_id(binary(), pos_integer()) -> binary().
generate_event_id(SessionId, EventNumber) ->
    %% SessionId already has format: "session_<client>_<timestamp>_<random>"
    %% Event ID: "<session_id>_<event_number>"
    EventNumBin = integer_to_binary(EventNumber),
    <<SessionId/binary, "_", EventNumBin/binary>>.

%% @private
%% @doc Clean up expired sessions based on timestamp
-spec cleanup_expired_sessions() -> ok.
cleanup_expired_sessions() ->
    Now = erlang:system_time(millisecond),
    Tables = ets:all(),

    lists:foreach(fun(Table) ->
                     case ets:info(Table, name) of
                         {?MODULE, SessionId} ->
                             cleanup_table_if_expired(Table, SessionId, Now);
                         _ ->
                             ok
                     end
                  end,
                  Tables),

    ok.

%% @private
-spec cleanup_table_if_expired(ets:tid(), binary(), integer()) -> ok.
cleanup_table_if_expired(Table, _SessionId, Now) ->
    case ets:first(Table) of
        '$end_of_table' ->
            %% Empty table, delete it
            ets:delete(Table);
        FirstKey ->
            case ets:lookup(Table, FirstKey) of
                [#event{timestamp = Ts}] when Now - Ts > ?EVENT_TTL ->
                    %% Oldest event is expired, delete entire table
                    ets:delete(Table);
                _ ->
                    %% Table still has valid events
                    ok
            end
    end.
