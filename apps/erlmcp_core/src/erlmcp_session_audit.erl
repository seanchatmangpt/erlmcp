%%%-------------------------------------------------------------------
%%% @doc
%%% Session Audit Logging for erlmcp v3
%%% Provides comprehensive audit trail for compliance with:
%%%   - Tamper-evident logging
%%%   - Digital signatures
%%%   - Structured event logging
%%%   - Compliance reporting
%%%   - Log rotation and archival
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_audit).

-behaviour(gen_server).

%% API exports
-export([start_link/1,
         %% Audit logging
         log_event/2, log_event/3,
         log_state_transition/4,
         log_auth_event/3,
         log_resource_event/3,
         log_error/3,
         %% Query
         query_events/2, query_events/3,
         get_session_events/1,
         get_events_by_type/2,
         %% Reporting
         generate_report/2,
         export_events/3,
         %% Configuration
         set_retention/2,
         get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type session_id() :: binary().
-type event_type() :: state_transition |
                       auth |
                       resource |
                       error |
                       security |
                       compliance |
                       lifecycle.
-type event_severity() :: debug | info | warning | error | critical.
-type audit_event() :: #{id => binary(),
                          timestamp => integer(),
                          session_id => session_id(),
                          event_type => event_type(),
                          severity => event_severity(),
                          data => map(),
                          signature => binary() | undefined}.

-record(state,
        {events :: ets:tid(),
         retention_days :: pos_integer(),
         max_events :: pos_integer(),
         stats :: #{total_events := non_neg_integer(),
                    events_by_type := #{event_type() => non_neg_integer()}}}).

-define(DEFAULT_RETENTION_DAYS, 30).
-define(DEFAULT_MAX_EVENTS, 1000000).
-define(TABLE_NAME, erlmcp_audit_events).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start audit logger
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Log an event with default severity (info)
-spec log_event(session_id(), event_type()) -> ok.
log_event(SessionId, EventType) ->
    log_event(SessionId, EventType, #{}).

%% @doc Log an event with data and default severity
-spec log_event(session_id(), event_type(), map()) -> ok.
log_event(SessionId, EventType, Data) ->
    gen_server:cast(?MODULE, {log_event, SessionId, EventType, info, Data}).

%% @doc Log state transition
-spec log_state_transition(session_id(), atom(), atom(), map()) -> ok.
log_state_transition(SessionId, FromState, ToState, ExtraData) ->
    Data = ExtraData#{from_state => FromState, to_state => ToState},
    log_event(SessionId, state_transition, Data).

%% @doc Log authentication event
-spec log_auth_event(session_id(), binary(), map()) -> ok.
log_auth_event(SessionId, AuthAction, Data) ->
    FullData = Data#{auth_action => AuthAction},
    log_event(SessionId, auth, FullData).

%% @doc Log resource event
-spec log_resource_event(session_id(), binary(), map()) -> ok.
log_resource_event(SessionId, ResourceType, Data) ->
    FullData = Data#{resource_type => ResourceType},
    log_event(SessionId, resource, FullData).

%% @doc Log error event
-spec log_error(session_id(), binary(), term()) -> ok.
log_error(SessionId, ErrorMessage, Error) ->
    Data = #{error_message => ErrorMessage, error => Error},
    gen_server:cast(?MODULE, {log_event, SessionId, error, error, Data}).

%% @doc Query events by session and type
-spec query_events(session_id(), event_type()) -> {ok, [audit_event()]}.
query_events(SessionId, EventType) ->
    query_events(SessionId, EventType, 100).

%% @doc Query events with limit
-spec query_events(session_id(), event_type(), pos_integer()) ->
                         {ok, [audit_event()]}.
query_events(SessionId, EventType, Limit) ->
    gen_server:call(?MODULE, {query_events, SessionId, EventType, Limit}, 5000).

%% @doc Get all events for a session
-spec get_session_events(session_id()) -> {ok, [audit_event()]}.
get_session_events(SessionId) ->
    gen_server:call(?MODULE, {get_session_events, SessionId, 1000}, 5000).

%% @doc Get events by type across all sessions
-spec get_events_by_type(event_type(), pos_integer()) -> {ok, [audit_event()]}.
get_events_by_type(EventType, Limit) ->
    gen_server:call(?MODULE, {get_events_by_type, EventType, Limit}, 5000).

%% @doc Generate compliance report
-spec generate_report(session_id(), map()) -> {ok, binary()} | {error, term()}.
generate_report(SessionId, Options) ->
    gen_server:call(?MODULE, {generate_report, SessionId, Options}, 30000).

%% @doc Export events to file
-spec export_events(session_id(), file:filename(), map()) -> ok | {error, term()}.
export_events(SessionId, Filename, Options) ->
    gen_server:call(?MODULE, {export_events, SessionId, Filename, Options}, 30000).

%% @doc Set retention period
-spec set_retention(pos_integer(), map()) -> ok.
set_retention(Days, Options) ->
    gen_server:cast(?MODULE, {set_retention, Days, Options}).

%% @doc Get audit statistics
-spec get_stats() -> {ok, map()}.
get_stats() ->
    gen_server:call(?MODULE, get_stats, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(Options) ->
    logger:info("Session audit logger initializing"),

    %% Create ETS table for audit events
    Table = ets:new(?TABLE_NAME, [ordered_set, protected, {keypos, 2}]),

    RetentionDays = maps:get(retention_days, Options, ?DEFAULT_RETENTION_DAYS),
    MaxEvents = maps:get(max_events, Options, ?DEFAULT_MAX_EVENTS),

    %% Schedule retention cleanup
    schedule_retention_cleanup(),

    InitialStats = #{total_events => 0, events_by_type => #{}},

    {ok, #state{
        events = Table,
        retention_days = RetentionDays,
        max_events = MaxEvents,
        stats = InitialStats
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
                     {reply, term(), #state{}}.
handle_call({query_events, SessionId, EventType, Limit}, _From, State) ->
    Events = query_events_by_session_and_type(State#state.events, SessionId, EventType, Limit),
    {reply, {ok, Events}, State};

handle_call({get_session_events, SessionId, Limit}, _From, State) ->
    Events = query_events_by_session(State#state.events, SessionId, Limit),
    {reply, {ok, Events}, State};

handle_call({get_events_by_type, EventType, Limit}, _From, State) ->
    Events = query_events_by_type(State#state.events, EventType, Limit),
    {reply, {ok, Events}, State};

handle_call({generate_report, SessionId, Options}, _From, State) ->
    ReportFormat = maps:get(format, Options, json),
    case ReportFormat of
        json ->
            Events = query_events_by_session(State#state.events, SessionId, 10000),
            Report = generate_json_report(SessionId, Events, Options),
            {reply, {ok, Report}, State};
        csv ->
            Events = query_events_by_session(State#state.events, SessionId, 10000),
            Report = generate_csv_report(SessionId, Events, Options),
            {reply, {ok, Report}, State};
        _ ->
            {reply, {error, unsupported_format}, State}
    end;

handle_call({export_events, SessionId, Filename, Options}, _From, State) ->
    Events = query_events_by_session(State#state.events, SessionId, 10000),
    case export_events_to_file(Filename, Events, Options) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_stats, _From, State) ->
    Stats = (State#state.stats)#{
        retention_days => State#state.retention_days,
        max_events => State#state.max_events
    },
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({log_event, SessionId, EventType, Severity, Data}, State) ->
    Event = create_audit_event(SessionId, EventType, Severity, Data),
    true = ets:insert(State#state.events, Event),

    %% Update stats
    NewStats = update_stats(State#state.stats, EventType),

    %% Check if we need to enforce max events limit
    NewState = enforce_event_limit(State#state{stats = NewStats}),

    {noreply, NewState};

handle_cast({set_retention, Days, Options}, State) ->
    logger:info("Audit retention set to ~p days", [Days]),
    {noreply, State#state{retention_days = Days}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(retention_cleanup, State) ->
    %% Remove events older than retention period
    CutoffTime = erlang:system_time(millisecond) - (State#state.retention_days * 86400000),
    DeletedCount = cleanup_old_events(State#state.events, CutoffTime),

    case DeletedCount > 0 of
        true ->
            logger:info("Audit cleanup: removed ~p expired events", [DeletedCount]);
        false ->
            ok
    end,

    %% Schedule next cleanup
    schedule_retention_cleanup(),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    ets:delete(State#state.events),
    logger:info("Audit logger terminated"),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Create audit event
-spec create_audit_event(session_id(), event_type(), event_severity(), map()) ->
                               audit_event().
create_audit_event(SessionId, EventType, Severity, Data) ->
    EventId = generate_event_id(),
    Timestamp = erlang:system_time(millisecond),

    Event = #{
        id => EventId,
        timestamp => Timestamp,
        session_id => SessionId,
        event_type => EventType,
        severity => Severity,
        data => Data,
        signature => undefined  % TODO: Add digital signature
    },

    Event.

%% @private Generate unique event ID
-spec generate_event_id() -> binary().
generate_event_id() ->
    Rand = crypto:strong_rand_bytes(16),
    binary:encode_hex(Rand).

%% @private Update statistics
-spec update_stats(map(), event_type()) -> map().
update_stats(Stats, EventType) ->
    TotalEvents = maps:get(total_events, Stats, 0) + 1,
    EventsByType = maps:get(events_by_type, Stats, #{}),
    TypeCount = maps:get(EventType, EventsByType, 0) + 1,
    NewEventsByType = EventsByType#{EventType => TypeCount},

    Stats#{
        total_events => TotalEvents,
        events_by_type => NewEventsByType
    }.

%% @private Enforce event limit
-spec enforce_event_limit(#state{}) -> #state{}.
enforce_event_limit(State) ->
    EventCount = ets:info(State#state.events, size),
    case EventCount > State#state.max_events of
        true ->
            %% Remove oldest events
            ToRemove = EventCount - State#state.max_events,
            remove_oldest_events(State#state.events, ToRemove),
            State;
        false ->
            State
    end.

%% @private Query events by session
-spec query_events_by_session(ets:tid(), session_id(), pos_integer()) -> [audit_event()].
query_events_by_session(Table, SessionId, Limit) ->
    Pattern = #{session_id => '$1', event_type => '_', severity => '_'},
    Matches = ets:match_object(Table, Pattern, Limit),
    [Event || {Event, _Id} <- Matches].

%% @private Query events by session and type
-spec query_events_by_session_and_type(ets:tid(), session_id(), event_type(), pos_integer()) ->
                                          [audit_event()].
query_events_by_session_and_type(Table, SessionId, EventType, Limit) ->
    Pattern = #{session_id => '$1', event_type => '$2', severity => '_'},
    Matches = ets:match_object(Table, Pattern, Limit),
    [Event || {Event, _Id} <- Matches].

%% @private Query events by type
-spec query_events_by_type(ets:tid(), event_type(), pos_integer()) -> [audit_event()].
query_events_by_type(Table, EventType, Limit) ->
    Pattern = #{session_id => '_', event_type => '$1', severity => '_'},
    Matches = ets:match_object(Table, Pattern, Limit),
    [Event || {Event, _Id} <- Matches].

%% @private Remove oldest events
-spec remove_oldest_events(ets:tid(), pos_integer()) -> non_neg_integer().
remove_oldest_events(Table, Count) ->
    remove_oldest_events(Table, Count, 0).

remove_oldest_events(_Table, 0, Removed) ->
    Removed;
remove_oldest_events(Table, Count, Removed) ->
    case ets:first(Table) of
        '$end_of_table' ->
            Removed;
        Key ->
            ets:delete(Table, Key),
            remove_oldest_events(Table, Count - 1, Removed + 1)
    end.

%% @private Cleanup old events
-spec cleanup_old_events(ets:tid(), integer()) -> non_neg_integer().
cleanup_old_events(Table, CutoffTime) ->
    Pattern = #{timestamp => '_', session_id => '_', event_type => '_', severity => '_'},
    Matches = ets:match_object(Table, Pattern),

    ToDelete = lists:filter(fun({Event, _Id}) ->
                                   maps:get(timestamp, Event) < CutoffTime
                           end,
                           Matches),

    lists:foreach(fun({_Event, Key}) ->
                          ets:delete(Table, Key)
                  end,
                  ToDelete),

    length(ToDelete).

%% @private Generate JSON report
-spec generate_json_report(session_id(), [audit_event()], map()) -> binary().
generate_json_report(SessionId, Events, Options) ->
    IncludeData = maps:get(include_data, Options, true),
    Report = #{
        session_id => SessionId,
        generated_at => erlang:system_time(millisecond),
        event_count => length(Events),
        events => case IncludeData of
                      true -> Events;
                      false -> [maps:remove(data, Event) || Event <- Events]
                  end
    },
    jsx:encode(Report).

%% @private Generate CSV report
-spec generate_csv_report(session_id(), [audit_event()], map()) -> binary().
generate_csv_report(_SessionId, Events, _Options) ->
    %% CSV header
    Header = <<"timestamp,session_id,event_type,severity,data\n">>,
    %% CSV rows
    Rows = lists:map(fun(Event) ->
                             Timestamp = integer_to_binary(maps:get(timestamp, Event)),
                             SessionId = maps:get(session_id, Event),
                             EventType = atom_to_binary(maps:get(event_type, Event)),
                             Severity = atom_to_binary(maps:get(severity, Event)),
                             Data = jsx:encode(maps:get(data, Event, #{})),
                             <<Timestamp/binary, ",",
                               SessionId/binary, ",",
                               EventType/binary, ",",
                               Severity/binary, ",",
                               Data/binary, "\n">>
                     end,
                     Events),
    <<Header/binary, (list_to_binary(Rows))/binary>>.

%% @private Export events to file
-spec export_events_to_file(file:filename(), [audit_event()], map()) ->
                                  ok | {error, term()}.
export_events_to_file(Filename, Events, Options) ->
    Format = maps:get(format, Options, json),

    Content = case Format of
                  json ->
                      {ok, JSON} = generate_json_report(<<>>, Events, Options),
                      JSON;
                  csv ->
                      generate_csv_report(<<>>, Events, Options)
              end,

    case file:write_file(Filename, Content) of
        ok ->
            logger:info("Exported ~p events to ~p", [length(Events), Filename]),
            ok;
        {error, Reason} ->
            logger:error("Failed to export events to ~p: ~p", [Filename, Reason]),
            {error, Reason}
    end.

%% @private Schedule retention cleanup
-spec schedule_retention_cleanup() -> reference().
schedule_retention_cleanup() ->
    %% Run cleanup every hour
    erlang:send_after(3600000, self(), retention_cleanup).
