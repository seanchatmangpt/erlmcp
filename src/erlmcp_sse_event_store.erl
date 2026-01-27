%%%=====================================================================
%%% @doc
%%% SSE Event Store - Maintains recent events for stream resumability.
%%%
%%% This module provides a persistent store of recent SSE events indexed
%%% by session ID. Events can be replayed to clients after disconnection,
%%% enabling stream resumability without losing data.
%%%
%%% Features:
%%% - Store up to 100 events per session
%%% - Automatic cleanup of expired sessions
%%% - Event ID tracking for resume capability
%%% - ETS-based storage for fast access
%%%
%%% @end
%%%=====================================================================

-module(erlmcp_sse_event_store).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% Public API
-export([
    start_link/0,
    add_event/3,
    get_events_since/2,
    cleanup_expired/0,
    clear_session/1,
    get_session_info/1,
    get_all_sessions/0,
    parse_event_id/1
]).

%% Supervisor/gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

%% Constants
-define(TABLE_NAME, erlmcp_sse_events).
-define(CLEANUP_INTERVAL, 300000). %% 5 minutes
-define(MAX_EVENTS_PER_SESSION, 100).
-define(EVENT_TTL, 3600000). %% 1 hour in milliseconds
-define(SERVER, ?MODULE).

%% Record for event storage
-record(sse_event, {
    event_id :: binary(),        %% Unique event ID: session_id_event_number
    session_id :: binary(),      %% Session identifier
    event_number :: pos_integer(), %% Sequential number for ordering
    data :: binary(),            %% Event data (JSON)
    timestamp :: integer()       %% Unix timestamp in milliseconds
}).

%% Record for session tracking
-record(sse_session, {
    session_id :: binary(),
    created_at :: integer(),
    last_event_number :: non_neg_integer(),
    event_count :: non_neg_integer()
}).

%% Server state
-record(state, {
    cleanup_timer :: reference() | undefined,
    span_ctx :: term()
}).

-type state() :: #state{}.
-type event_id() :: binary().
-type session_id() :: binary().

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start the SSE event store as a gen_server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Add an event to the store for a session.
%%
%% Returns the generated EventId that should be sent in SSE id field.
-spec add_event(session_id(), pos_integer(), binary()) ->
    {ok, event_id()} | {error, term()}.
add_event(SessionId, EventNumber, Data) when
    is_binary(SessionId), is_integer(EventNumber), is_binary(Data)
->
    SpanCtx = erlmcp_tracing:start_span(<<"sse_event_store.add_event">>),
    try
        EventId = generate_event_id(SessionId, EventNumber),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"session_id">> => SessionId,
            <<"event_number">> => EventNumber,
            <<"event_id">> => EventId,
            <<"data_size">> => byte_size(Data)
        }),

        %% Create event record
        Event = #sse_event{
            event_id = EventId,
            session_id = SessionId,
            event_number = EventNumber,
            data = Data,
            timestamp = erlang:system_time(millisecond)
        },

        %% Insert event (will replace if exists, maintaining order by event_number)
        ets:insert(?TABLE_NAME, Event),

        %% Update session tracking
        update_session_info(SessionId, EventNumber),

        erlmcp_tracing:set_status(SpanCtx, ok),
        {ok, EventId}
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

add_event(_SessionId, _EventNumber, _Data) ->
    {error, invalid_arguments}.

%% @doc Retrieve all events for a session after a specific event ID.
%%
%% If LastEventId is undefined, returns all events for the session.
%% Returns events in order by event_number for proper replay.
-spec get_events_since(session_id(), event_id() | undefined) ->
    {ok, [binary()]} | {error, term()}.
get_events_since(SessionId, LastEventId) when is_binary(SessionId) ->
    SpanCtx = erlmcp_tracing:start_span(<<"sse_event_store.get_events_since">>),
    try
        %% Parse last event ID to get event number
        StartNumber = case LastEventId of
            undefined -> 0;
            _ -> parse_event_id(LastEventId)
        end,

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"session_id">> => SessionId,
            <<"start_number">> => StartNumber
        }),

        %% Query events: session matches and event_number > start
        MatchSpec = ets:fun2ms(fun(#sse_event{
            session_id = SId,
            event_number = ENum,
            data = D
        }) when (SId =:= SessionId) and (ENum > StartNumber) ->
            D
        end),

        Events = ets:select(?TABLE_NAME, MatchSpec),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"events_returned">> => length(Events)
        }),
        erlmcp_tracing:set_status(SpanCtx, ok),

        {ok, Events}
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

get_events_since(_SessionId, _LastEventId) ->
    {error, invalid_arguments}.

%% @doc Clean up expired events and sessions.
%%
%% Called periodically to remove old events and inactive sessions.
-spec cleanup_expired() -> ok | {error, term()}.
cleanup_expired() ->
    SpanCtx = erlmcp_tracing:start_span(<<"sse_event_store.cleanup_expired">>),
    try
        CurrentTime = erlang:system_time(millisecond),
        ExpiryTime = CurrentTime - ?EVENT_TTL,

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"current_time">> => CurrentTime,
            <<"expiry_time">> => ExpiryTime
        }),

        %% Delete events older than TTL
        MatchSpec = ets:fun2ms(fun(#sse_event{timestamp = T}) when T < ExpiryTime ->
            true
        end),

        DeleteCount = ets:select_delete(?TABLE_NAME, MatchSpec),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"deleted_events">> => DeleteCount
        }),
        erlmcp_tracing:set_status(SpanCtx, ok),
        ok
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

%% @doc Clear all events for a session.
-spec clear_session(session_id()) -> ok | {error, term()}.
clear_session(SessionId) when is_binary(SessionId) ->
    SpanCtx = erlmcp_tracing:start_span(<<"sse_event_store.clear_session">>),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"session_id">> => SessionId
        }),

        %% Delete all events for this session
        MatchSpec = ets:fun2ms(fun(#sse_event{session_id = SId}) when SId =:= SessionId ->
            true
        end),
        ets:select_delete(?TABLE_NAME, MatchSpec),

        erlmcp_tracing:set_status(SpanCtx, ok),
        ok
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            {error, {Class, CaughtReason}}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;

clear_session(_SessionId) ->
    {error, invalid_arguments}.

%% @doc Get information about a session.
-spec get_session_info(session_id()) -> {ok, map()} | {error, not_found}.
get_session_info(SessionId) when is_binary(SessionId) ->
    case ets:lookup(?TABLE_NAME, SessionId) of
        [] ->
            {error, not_found};
        [#sse_session{
            created_at = CreatedAt,
            last_event_number = LastEventNum,
            event_count = Count
        }] ->
            {ok, #{
                session_id => SessionId,
                created_at => CreatedAt,
                last_event_number => LastEventNum,
                event_count => Count
            }};
        [#sse_event{event_number = EventNum} | _Rest] ->
            %% If only events exist (no session record), create info from events
            {ok, #{
                session_id => SessionId,
                last_event_number => EventNum,
                event_count => count_session_events(SessionId)
            }}
    end;

get_session_info(_SessionId) ->
    {error, invalid_arguments}.

%% @doc Get all active sessions.
-spec get_all_sessions() -> {ok, [binary()]} | {error, term()}.
get_all_sessions() ->
    try
        MatchSpec = ets:fun2ms(fun(#sse_session{session_id = SId}) -> SId end),
        Sessions = ets:select(?TABLE_NAME, MatchSpec),
        {ok, lists:usort(Sessions)}
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(
                erlmcp_tracing:start_span(<<"sse_event_store.get_all_sessions">>),
                Class,
                CaughtReason,
                Stacktrace
            ),
            {error, {Class, CaughtReason}}
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    SpanCtx = erlmcp_tracing:start_span(<<"sse_event_store.init">>),

    %% Create ETS table for event storage
    ets:new(?TABLE_NAME, [
        {keypos, 2},  %% Key on event_id field
        ordered_set,
        public,
        named_table
    ]),

    erlmcp_tracing:set_status(SpanCtx, ok),

    %% Schedule periodic cleanup
    {ok, Timer} = timer:send_interval(?CLEANUP_INTERVAL, cleanup_timer),

    {ok, #state{
        cleanup_timer = Timer,
        span_ctx = SpanCtx
    }}.

-spec handle_call(term(), {pid(), reference()}, state()) ->
    {reply, term(), state()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(cleanup_timer, State) ->
    cleanup_expired(),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{cleanup_timer = Timer}) ->
    timer:cancel(Timer),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Generate unique event ID: session_id_event_number.
-spec generate_event_id(session_id(), pos_integer()) -> binary().
generate_event_id(SessionId, EventNumber) ->
    EventStr = integer_to_binary(EventNumber),
    <<SessionId/binary, "_", EventStr/binary>>.

%% @doc Parse event ID to extract event number.
%% Returns event number if valid, 0 if invalid.
-spec parse_event_id(event_id()) -> non_neg_integer().
parse_event_id(EventId) when is_binary(EventId) ->
    %% Format: session_id_event_number
    case binary:split(EventId, <<"_">>, [global]) of
        Parts when length(Parts) >= 2 ->
            LastPart = lists:last(Parts),
            try
                binary_to_integer(LastPart)
            catch
                _:_ -> 0
            end;
        _ -> 0
    end;

parse_event_id(_) ->
    0.

%% @doc Update session tracking information.
-spec update_session_info(session_id(), pos_integer()) -> ok.
update_session_info(SessionId, EventNumber) ->
    case ets:lookup(?TABLE_NAME, SessionId) of
        [] ->
            %% New session
            Session = #sse_session{
                session_id = SessionId,
                created_at = erlang:system_time(millisecond),
                last_event_number = EventNumber,
                event_count = 1
            },
            ets:insert(?TABLE_NAME, Session);
        [Session] when is_record(Session, sse_session) ->
            %% Update existing session
            UpdatedSession = Session#sse_session{
                last_event_number = EventNumber,
                event_count = Session#sse_session.event_count + 1
            },
            ets:insert(?TABLE_NAME, UpdatedSession)
    end,

    %% Enforce max events per session limit
    enforce_max_events(SessionId).

%% @doc Enforce maximum events per session by removing oldest events.
-spec enforce_max_events(session_id()) -> ok.
enforce_max_events(SessionId) ->
    MatchSpec = ets:fun2ms(fun(#sse_event{
        session_id = SId,
        event_number = ENum
    }) when (SId =:= SessionId) ->
        ENum
    end),

    EventNumbers = ets:select(?TABLE_NAME, MatchSpec),
    SortedNumbers = lists:sort(EventNumbers),

    case length(SortedNumbers) > ?MAX_EVENTS_PER_SESSION of
        true ->
            %% Remove oldest events
            ToRemove = length(SortedNumbers) - ?MAX_EVENTS_PER_SESSION,
            OldestNumbers = lists:sublist(SortedNumbers, ToRemove),

            lists:foreach(
                fun(EventNum) ->
                    ets:delete_all_objects(?TABLE_NAME),
                    MatchSpecDel = ets:fun2ms(fun(#sse_event{
                        session_id = SId,
                        event_number = ENum
                    }) when (SId =:= SessionId) and (ENum =:= EventNum) ->
                        true
                    end),
                    ets:select_delete(?TABLE_NAME, MatchSpecDel)
                end,
                OldestNumbers
            );
        false ->
            ok
    end.

%% @doc Count total events for a session.
-spec count_session_events(session_id()) -> non_neg_integer().
count_session_events(SessionId) ->
    MatchSpec = ets:fun2ms(fun(#sse_event{session_id = SId}) when SId =:= SessionId ->
        true
    end),
    length(ets:select(?TABLE_NAME, MatchSpec)).
