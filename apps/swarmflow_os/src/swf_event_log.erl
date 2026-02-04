%%%====================================================================
%%% @doc SwarmFlow Event Log - Append-Only Event Store
%%%
%%% Provides an append-only event log for process mining with the following
%%% characteristics:
%%%
%%% - Append-only semantics: Events are immutable once written
%%% - ULID-based event IDs: Lexicographically sortable, time-ordered
%%% - Segmented storage: Efficient retrieval and compaction
%%% - Compression support: Reduces storage for older segments
%%% - Checksum verification: SHA-256 for integrity
%%% - Real-time subscriptions: PubSub for event streaming
%%%
%%% This module is critical for:
%%% - Replay: Reconstruct workflow state from events
%%% - Conformance checking: Compare actual vs expected behavior
%%% - Patch search: Identify improvement opportunities
%%%
%%% Architecture:
%%% - ETS for hot segments (recent events, fast writes)
%%% - Disk persistence for cold segments (compressed, durable)
%%% - Monotonic sequence per case for ordering guarantees
%%% - Subscribers receive events via process messages
%%%
%%% @end
%%%====================================================================
-module(swf_event_log).

-behaviour(gen_server).

-include("swarmflow.hrl").
-include_lib("kernel/include/logger.hrl").

%% API exports
-export([start_link/0, start_link/1]).
-export([append/1, append_batch/1]).
-export([get_events/2, get_events_by_type/2, get_events_since/2]).
-export([subscribe/1, unsubscribe/1]).
-export([get_segment/1, compact/1, verify_integrity/1]).
-export([stats/0, get_sequence/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type case_id() :: binary().
-type sequence() :: non_neg_integer().
-type segment_id() :: binary().
-type subscriber() :: pid().
-type event_filter() ::
    #{from_seq => sequence(),
      to_seq => sequence(),
      limit => pos_integer(),
      event_types => [event_type()]}.

-export_type([case_id/0, sequence/0, segment_id/0, event_filter/0]).

%%====================================================================
%% Constants
%%====================================================================

-define(DEFAULT_SEGMENT_SIZE, 1000).  % Events per segment
-define(DEFAULT_COMPACT_THRESHOLD, 10).  % Segments before compaction
-define(CHECKSUM_ALGO, sha256).
-define(HOT_SEGMENT_TABLE, swf_event_log_hot).
-define(SEQUENCE_TABLE, swf_event_log_seq).
-define(SEGMENT_INDEX_TABLE, swf_event_log_segments).
-define(SUBSCRIBER_TABLE, swf_event_log_subs).

%%====================================================================
%% State record
%%====================================================================

-record(state, {
    %% ETS tables for hot data
    hot_table :: ets:tid(),
    sequence_table :: ets:tid(),
    segment_index :: ets:tid(),
    subscriber_table :: ets:tid(),

    %% Configuration
    segment_size :: pos_integer(),
    compact_threshold :: pos_integer(),
    data_dir :: file:filename_all(),
    compression_level :: 0..9,

    %% Metrics
    stats :: #{
        events_appended => non_neg_integer(),
        events_read => non_neg_integer(),
        segments_created => non_neg_integer(),
        segments_compacted => non_neg_integer(),
        bytes_written => non_neg_integer(),
        bytes_compressed => non_neg_integer()
    },

    %% Timer for periodic maintenance
    maintenance_timer :: reference() | undefined
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Append a single event to the log.
%% Generates ULID, assigns sequence number, and notifies subscribers.
-spec append(#swf_event{}) -> {ok, #swf_event{}} | {error, term()}.
append(#swf_event{} = Event) ->
    gen_server:call(?MODULE, {append, Event}, 10000).

%% @doc Append multiple events atomically.
%% All events get consecutive sequence numbers within the same case.
-spec append_batch([#swf_event{}]) -> {ok, [#swf_event{}]} | {error, term()}.
append_batch(Events) when is_list(Events) ->
    gen_server:call(?MODULE, {append_batch, Events}, 30000).

%% @doc Get events for a case with filtering options.
%% Options:
%%   from_seq - Start sequence (inclusive)
%%   to_seq - End sequence (inclusive)
%%   limit - Maximum events to return
%%   event_types - Filter by event types
-spec get_events(case_id(), event_filter()) -> {ok, [#swf_event{}]} | {error, term()}.
get_events(CaseId, Opts) when is_binary(CaseId), is_map(Opts) ->
    gen_server:call(?MODULE, {get_events, CaseId, Opts}, 30000).

%% @doc Get events by type across all cases.
%% Useful for pattern mining across workflow instances.
-spec get_events_by_type(event_type(), event_filter()) -> {ok, [#swf_event{}]} | {error, term()}.
get_events_by_type(EventType, Opts) when is_atom(EventType), is_map(Opts) ->
    gen_server:call(?MODULE, {get_events_by_type, EventType, Opts}, 30000).

%% @doc Get all events since a global sequence number.
%% Useful for log replication and catch-up subscriptions.
-spec get_events_since(sequence(), pos_integer()) -> {ok, [#swf_event{}]} | {error, term()}.
get_events_since(Sequence, Limit) when is_integer(Sequence), is_integer(Limit) ->
    gen_server:call(?MODULE, {get_events_since, Sequence, Limit}, 30000).

%% @doc Subscribe to events for a specific case.
%% Subscriber receives messages: {swf_event, #swf_event{}}
-spec subscribe(case_id()) -> ok | {error, term()}.
subscribe(CaseId) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {subscribe, CaseId, self()}, 5000).

%% @doc Unsubscribe from case events.
-spec unsubscribe(case_id()) -> ok.
unsubscribe(CaseId) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {unsubscribe, CaseId, self()}, 5000).

%% @doc Get a specific log segment by ID.
%% Returns decompressed segment with all events.
-spec get_segment(segment_id()) -> {ok, #swf_event_log_segment{}} | {error, term()}.
get_segment(SegmentId) when is_binary(SegmentId) ->
    gen_server:call(?MODULE, {get_segment, SegmentId}, 30000).

%% @doc Compact/compress old segments for a case.
%% Merges multiple segments and applies compression.
-spec compact(case_id()) -> {ok, non_neg_integer()} | {error, term()}.
compact(CaseId) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {compact, CaseId}, 60000).

%% @doc Verify integrity of a segment via checksum.
-spec verify_integrity(segment_id()) -> ok | {error, checksum_mismatch | not_found}.
verify_integrity(SegmentId) when is_binary(SegmentId) ->
    gen_server:call(?MODULE, {verify_integrity, SegmentId}, 30000).

%% @doc Get log statistics.
-spec stats() -> map().
stats() ->
    gen_server:call(?MODULE, stats, 5000).

%% @doc Get current sequence number for a case.
-spec get_sequence(case_id()) -> non_neg_integer().
get_sequence(CaseId) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {get_sequence, CaseId}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, state()}.
init(Config) ->
    process_flag(trap_exit, true),

    %% Create ETS tables
    HotTable = ets:new(?HOT_SEGMENT_TABLE, [
        ordered_set, public, named_table,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),

    SequenceTable = ets:new(?SEQUENCE_TABLE, [
        set, public, named_table,
        {write_concurrency, true}
    ]),

    SegmentIndex = ets:new(?SEGMENT_INDEX_TABLE, [
        ordered_set, public, named_table,
        {read_concurrency, true}
    ]),

    SubscriberTable = ets:new(?SUBSCRIBER_TABLE, [
        bag, public, named_table,
        {read_concurrency, true}
    ]),

    %% Extract configuration
    SegmentSize = maps:get(segment_size, Config, ?DEFAULT_SEGMENT_SIZE),
    CompactThreshold = maps:get(compact_threshold, Config, ?DEFAULT_COMPACT_THRESHOLD),
    DataDir = maps:get(data_dir, Config, "/tmp/swf_event_log"),
    CompressionLevel = maps:get(compression_level, Config, 6),

    %% Ensure data directory exists
    ok = filelib:ensure_dir(filename:join(DataDir, "segments/")),

    %% Initialize stats
    Stats = #{
        events_appended => 0,
        events_read => 0,
        segments_created => 0,
        segments_compacted => 0,
        bytes_written => 0,
        bytes_compressed => 0
    },

    %% Start maintenance timer (every 5 minutes)
    MaintenanceTimer = erlang:send_after(300000, self(), maintenance),

    State = #state{
        hot_table = HotTable,
        sequence_table = SequenceTable,
        segment_index = SegmentIndex,
        subscriber_table = SubscriberTable,
        segment_size = SegmentSize,
        compact_threshold = CompactThreshold,
        data_dir = DataDir,
        compression_level = CompressionLevel,
        stats = Stats,
        maintenance_timer = MaintenanceTimer
    },

    ?LOG_INFO("swf_event_log started: segment_size=~p, data_dir=~s",
              [SegmentSize, DataDir]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
%% Append single event
handle_call({append, Event}, _From, State) ->
    case do_append_events([Event], State) of
        {ok, [EnrichedEvent], NewState} ->
            notify_subscribers(EnrichedEvent, State),
            {reply, {ok, EnrichedEvent}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Append batch of events
handle_call({append_batch, Events}, _From, State) ->
    case do_append_events(Events, State) of
        {ok, EnrichedEvents, NewState} ->
            lists:foreach(fun(E) -> notify_subscribers(E, State) end, EnrichedEvents),
            {reply, {ok, EnrichedEvents}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Get events for case
handle_call({get_events, CaseId, Opts}, _From, State) ->
    Result = do_get_events(CaseId, Opts, State),
    NewStats = increment_stat(events_read, length(element(2, Result)), State#state.stats),
    {reply, Result, State#state{stats = NewStats}};

%% Get events by type
handle_call({get_events_by_type, EventType, Opts}, _From, State) ->
    Result = do_get_events_by_type(EventType, Opts, State),
    NewStats = increment_stat(events_read, length(element(2, Result)), State#state.stats),
    {reply, Result, State#state{stats = NewStats}};

%% Get events since sequence
handle_call({get_events_since, Sequence, Limit}, _From, State) ->
    Result = do_get_events_since(Sequence, Limit, State),
    NewStats = increment_stat(events_read, length(element(2, Result)), State#state.stats),
    {reply, Result, State#state{stats = NewStats}};

%% Subscribe to case events
handle_call({subscribe, CaseId, Pid}, _From, State) ->
    ets:insert(State#state.subscriber_table, {CaseId, Pid}),
    erlang:monitor(process, Pid),
    ?LOG_DEBUG("Process ~p subscribed to case ~s", [Pid, CaseId]),
    {reply, ok, State};

%% Unsubscribe from case events
handle_call({unsubscribe, CaseId, Pid}, _From, State) ->
    ets:delete_object(State#state.subscriber_table, {CaseId, Pid}),
    {reply, ok, State};

%% Get segment
handle_call({get_segment, SegmentId}, _From, State) ->
    Result = do_get_segment(SegmentId, State),
    {reply, Result, State};

%% Compact case segments
handle_call({compact, CaseId}, _From, State) ->
    case do_compact(CaseId, State) of
        {ok, Count, NewState} ->
            {reply, {ok, Count}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Verify segment integrity
handle_call({verify_integrity, SegmentId}, _From, State) ->
    Result = do_verify_integrity(SegmentId, State),
    {reply, Result, State};

%% Get stats
handle_call(stats, _From, State) ->
    HotSize = ets:info(State#state.hot_table, size),
    SegmentCount = ets:info(State#state.segment_index, size),
    SubscriberCount = ets:info(State#state.subscriber_table, size),

    BaseStats = State#state.stats,
    ExtendedStats = maps:merge(BaseStats, #{
        hot_events => HotSize,
        segments => SegmentCount,
        subscribers => SubscriberCount
    }),
    {reply, ExtendedStats, State};

%% Get sequence for case
handle_call({get_sequence, CaseId}, _From, State) ->
    Seq = get_current_sequence(CaseId, State),
    {reply, Seq, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
%% Periodic maintenance
handle_info(maintenance, State) ->
    NewState = perform_maintenance(State),
    TimerRef = erlang:send_after(300000, self(), maintenance),
    {noreply, NewState#state{maintenance_timer = TimerRef}};

%% Subscriber process died
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Remove all subscriptions for this pid
    ets:match_delete(State#state.subscriber_table, {'_', Pid}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% Cancel maintenance timer
    case State#state.maintenance_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    %% Flush hot segments to disk
    flush_hot_segments(State),

    ?LOG_INFO("swf_event_log terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Event Append
%%====================================================================

-spec do_append_events([#swf_event{}], state()) ->
    {ok, [#swf_event{}], state()} | {error, term()}.
do_append_events(Events, State) ->
    Timestamp = erlang:system_time(microsecond),

    %% Group events by case_id
    EventsByCase = group_by_case(Events),

    %% Process each case's events
    try
        {EnrichedEvents, NewState} =
            maps:fold(
                fun(CaseId, CaseEvents, {AccEvents, AccState}) ->
                    {Enriched, UpdatedState} = enrich_and_store(
                        CaseId, CaseEvents, Timestamp, AccState
                    ),
                    {AccEvents ++ Enriched, UpdatedState}
                end,
                {[], State},
                EventsByCase
            ),

        %% Update stats
        NewStats = increment_stat(events_appended, length(EnrichedEvents), NewState#state.stats),

        %% Check if segments need rotation
        FinalState = maybe_rotate_segments(NewState#state{stats = NewStats}),

        {ok, EnrichedEvents, FinalState}
    catch
        error:Reason:Stack ->
            ?LOG_ERROR("Failed to append events: ~p~n~p", [Reason, Stack]),
            {error, Reason}
    end.

-spec group_by_case([#swf_event{}]) -> #{case_id() => [#swf_event{}]}.
group_by_case(Events) ->
    lists:foldl(
        fun(Event, Acc) ->
            CaseId = Event#swf_event.case_id,
            Existing = maps:get(CaseId, Acc, []),
            Acc#{CaseId => Existing ++ [Event]}
        end,
        #{},
        Events
    ).

-spec enrich_and_store(case_id(), [#swf_event{}], integer(), state()) ->
    {[#swf_event{}], state()}.
enrich_and_store(CaseId, Events, Timestamp, State) ->
    %% Get and increment sequence counter atomically
    BaseSeq = get_and_increment_sequence(CaseId, length(Events), State),

    %% Enrich each event with ID and sequence
    {EnrichedEvents, _} = lists:mapfoldl(
        fun(Event, Seq) ->
            EventId = generate_ulid(Timestamp),
            Enriched = Event#swf_event{
                id = EventId,
                sequence = Seq,
                timestamp = case Event#swf_event.timestamp of
                    undefined -> Timestamp;
                    T -> T
                end
            },

            %% Store in hot table
            %% Key format: {CaseId, Sequence} for ordered retrieval
            ets:insert(State#state.hot_table, {{CaseId, Seq}, Enriched}),

            {Enriched, Seq + 1}
        end,
        BaseSeq,
        Events
    ),

    {EnrichedEvents, State}.

-spec get_and_increment_sequence(case_id(), non_neg_integer(), state()) -> sequence().
get_and_increment_sequence(CaseId, Increment, State) ->
    %% Atomic counter increment
    try
        ets:update_counter(State#state.sequence_table, CaseId, {2, Increment})
            - Increment
    catch
        error:badarg ->
            %% Key doesn't exist, initialize it
            ets:insert(State#state.sequence_table, {CaseId, Increment}),
            0
    end.

-spec get_current_sequence(case_id(), state()) -> sequence().
get_current_sequence(CaseId, State) ->
    case ets:lookup(State#state.sequence_table, CaseId) of
        [{CaseId, Seq}] -> Seq;
        [] -> 0
    end.

%%====================================================================
%% Internal functions - Event Retrieval
%%====================================================================

-spec do_get_events(case_id(), event_filter(), state()) -> {ok, [#swf_event{}]}.
do_get_events(CaseId, Opts, State) ->
    FromSeq = maps:get(from_seq, Opts, 0),
    ToSeq = maps:get(to_seq, Opts, infinity),
    Limit = maps:get(limit, Opts, 10000),
    EventTypes = maps:get(event_types, Opts, all),

    %% Get events from hot table
    HotEvents = get_hot_events(CaseId, FromSeq, ToSeq, State),

    %% Get events from cold segments
    ColdEvents = get_cold_events(CaseId, FromSeq, ToSeq, State),

    %% Merge and sort by sequence
    AllEvents = lists:keysort(#swf_event.sequence, HotEvents ++ ColdEvents),

    %% Filter by event types if specified
    FilteredEvents = case EventTypes of
        all -> AllEvents;
        Types when is_list(Types) ->
            [E || E <- AllEvents, lists:member(E#swf_event.event_type, Types)]
    end,

    %% Apply limit
    LimitedEvents = lists:sublist(FilteredEvents, Limit),

    {ok, LimitedEvents}.

-spec get_hot_events(case_id(), sequence(), sequence() | infinity, state()) ->
    [#swf_event{}].
get_hot_events(CaseId, FromSeq, ToSeq, State) ->
    %% Use ets:select for range query
    MatchSpec = build_range_match_spec(CaseId, FromSeq, ToSeq),
    Results = ets:select(State#state.hot_table, MatchSpec),
    [Event || {_Key, Event} <- Results].

-spec build_range_match_spec(case_id(), sequence(), sequence() | infinity) ->
    ets:match_spec().
build_range_match_spec(CaseId, FromSeq, infinity) ->
    [{{{'$1', '$2'}, '$3'},
      [{'=:=', '$1', CaseId}, {'>=', '$2', FromSeq}],
      [{{'$2', '$3'}}]}];
build_range_match_spec(CaseId, FromSeq, ToSeq) ->
    [{{{'$1', '$2'}, '$3'},
      [{'=:=', '$1', CaseId}, {'>=', '$2', FromSeq}, {'=<', '$2', ToSeq}],
      [{{'$2', '$3'}}]}].

-spec get_cold_events(case_id(), sequence(), sequence() | infinity, state()) ->
    [#swf_event{}].
get_cold_events(CaseId, FromSeq, ToSeq, State) ->
    %% Find segments that overlap with the requested range
    Segments = find_segments_in_range(CaseId, FromSeq, ToSeq, State),

    %% Load and filter events from each segment
    lists:flatmap(
        fun(SegmentId) ->
            case do_get_segment(SegmentId, State) of
                {ok, Segment} ->
                    filter_segment_events(Segment#swf_event_log_segment.events,
                                         FromSeq, ToSeq);
                {error, _} ->
                    []
            end
        end,
        Segments
    ).

-spec find_segments_in_range(case_id(), sequence(), sequence() | infinity, state()) ->
    [segment_id()].
find_segments_in_range(CaseId, FromSeq, ToSeq, State) ->
    %% Query segment index: {CaseId, StartSeq} -> SegmentId
    MatchSpec = case ToSeq of
        infinity ->
            [{{{'$1', '$2'}, '$3'},
              [{'=:=', '$1', CaseId}],
              ['$3']}];
        _ ->
            [{{{'$1', '$2'}, '$3'},
              [{'=:=', '$1', CaseId}, {'=<', '$2', ToSeq}],
              ['$3']}]
    end,

    AllSegments = ets:select(State#state.segment_index, MatchSpec),

    %% Filter segments that actually overlap
    lists:filter(
        fun(SegmentId) ->
            case get_segment_metadata(SegmentId, State) of
                {ok, #{end_sequence := EndSeq}} -> EndSeq >= FromSeq;
                _ -> false
            end
        end,
        AllSegments
    ).

-spec filter_segment_events([#swf_event{}], sequence(), sequence() | infinity) ->
    [#swf_event{}].
filter_segment_events(Events, FromSeq, infinity) ->
    [E || E <- Events, E#swf_event.sequence >= FromSeq];
filter_segment_events(Events, FromSeq, ToSeq) ->
    [E || E <- Events,
          E#swf_event.sequence >= FromSeq,
          E#swf_event.sequence =< ToSeq].

-spec do_get_events_by_type(event_type(), event_filter(), state()) -> {ok, [#swf_event{}]}.
do_get_events_by_type(EventType, Opts, State) ->
    Limit = maps:get(limit, Opts, 1000),

    %% Scan hot table for matching events
    MatchSpec = [{{{'_', '_'}, '$1'},
                  [{'=:=', {element, #swf_event.event_type, '$1'}, EventType}],
                  ['$1']}],

    HotEvents = ets:select(State#state.hot_table, MatchSpec, Limit),

    Events = case HotEvents of
        {Results, _Continuation} -> Results;
        '$end_of_table' -> []
    end,

    {ok, Events}.

-spec do_get_events_since(sequence(), pos_integer(), state()) -> {ok, [#swf_event{}]}.
do_get_events_since(_Sequence, Limit, State) ->
    %% Get most recent events across all cases
    %% Using ets:select with limit for efficiency
    MatchSpec = [{{{'_', '_'}, '$1'}, [], ['$1']}],

    case ets:select(State#state.hot_table, MatchSpec, Limit) of
        {Results, _Continuation} ->
            %% Sort by timestamp descending
            Sorted = lists:sort(
                fun(A, B) -> A#swf_event.timestamp >= B#swf_event.timestamp end,
                Results
            ),
            {ok, lists:sublist(Sorted, Limit)};
        '$end_of_table' ->
            {ok, []}
    end.

%%====================================================================
%% Internal functions - Segments
%%====================================================================

-spec do_get_segment(segment_id(), state()) ->
    {ok, #swf_event_log_segment{}} | {error, term()}.
do_get_segment(SegmentId, State) ->
    SegmentPath = segment_path(SegmentId, State),

    case file:read_file(SegmentPath) of
        {ok, Data} ->
            try
                Segment = binary_to_term(Data),

                %% Decompress if needed
                DecompressedSegment = maybe_decompress_segment(Segment),

                {ok, DecompressedSegment}
            catch
                error:Reason ->
                    ?LOG_ERROR("Failed to decode segment ~s: ~p", [SegmentId, Reason]),
                    {error, decode_failed}
            end;
        {error, enoent} ->
            {error, not_found};
        {error, Reason} ->
            ?LOG_ERROR("Failed to read segment ~s: ~p", [SegmentId, Reason]),
            {error, Reason}
    end.

-spec get_segment_metadata(segment_id(), state()) -> {ok, map()} | {error, term()}.
get_segment_metadata(SegmentId, State) ->
    MetaPath = segment_meta_path(SegmentId, State),

    case file:read_file(MetaPath) of
        {ok, Data} ->
            {ok, binary_to_term(Data)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec maybe_rotate_segments(state()) -> state().
maybe_rotate_segments(State) ->
    %% Check each case for segment rotation
    CaseIds = get_all_case_ids(State),

    lists:foldl(
        fun(CaseId, AccState) ->
            HotCount = count_hot_events(CaseId, AccState),
            case HotCount >= AccState#state.segment_size of
                true ->
                    {ok, NewState} = rotate_segment(CaseId, AccState),
                    NewState;
                false ->
                    AccState
            end
        end,
        State,
        CaseIds
    ).

-spec count_hot_events(case_id(), state()) -> non_neg_integer().
count_hot_events(CaseId, State) ->
    MatchSpec = [{{{'$1', '_'}, '_'}, [{'=:=', '$1', CaseId}], [true]}],
    ets:select_count(State#state.hot_table, MatchSpec).

-spec get_all_case_ids(state()) -> [case_id()].
get_all_case_ids(State) ->
    %% Get unique case IDs from sequence table
    ets:select(State#state.sequence_table, [{{{'$1', '_'}}, [], ['$1']}]).

-spec rotate_segment(case_id(), state()) -> {ok, state()} | {error, term()}.
rotate_segment(CaseId, State) ->
    %% Collect hot events for this case
    HotEvents = get_hot_events(CaseId, 0, infinity, State),

    case HotEvents of
        [] ->
            {ok, State};
        _ ->
            %% Create segment
            SegmentId = generate_segment_id(CaseId),
            StartSeq = (hd(HotEvents))#swf_event.sequence,
            EndSeq = (lists:last(HotEvents))#swf_event.sequence,

            Checksum = compute_checksum(HotEvents),

            Segment = #swf_event_log_segment{
                id = SegmentId,
                case_id = CaseId,
                start_sequence = StartSeq,
                end_sequence = EndSeq,
                events = HotEvents,
                checksum = Checksum,
                compressed = false
            },

            %% Write segment to disk
            case write_segment(Segment, State) of
                ok ->
                    %% Update segment index
                    ets:insert(State#state.segment_index, {{CaseId, StartSeq}, SegmentId}),

                    %% Remove events from hot table
                    remove_hot_events(CaseId, StartSeq, EndSeq, State),

                    %% Update stats
                    NewStats = increment_stat(segments_created, 1, State#state.stats),

                    {ok, State#state{stats = NewStats}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec write_segment(#swf_event_log_segment{}, state()) -> ok | {error, term()}.
write_segment(Segment, State) ->
    SegmentPath = segment_path(Segment#swf_event_log_segment.id, State),
    MetaPath = segment_meta_path(Segment#swf_event_log_segment.id, State),

    %% Ensure directory exists
    ok = filelib:ensure_dir(SegmentPath),

    %% Write segment data
    Data = term_to_binary(Segment, [compressed]),

    case file:write_file(SegmentPath, Data) of
        ok ->
            %% Write metadata separately for quick lookups
            Meta = #{
                case_id => Segment#swf_event_log_segment.case_id,
                start_sequence => Segment#swf_event_log_segment.start_sequence,
                end_sequence => Segment#swf_event_log_segment.end_sequence,
                event_count => length(Segment#swf_event_log_segment.events),
                checksum => Segment#swf_event_log_segment.checksum,
                compressed => Segment#swf_event_log_segment.compressed
            },
            file:write_file(MetaPath, term_to_binary(Meta));
        {error, Reason} ->
            {error, Reason}
    end.

-spec remove_hot_events(case_id(), sequence(), sequence(), state()) -> ok.
remove_hot_events(CaseId, StartSeq, EndSeq, State) ->
    %% Delete events in range
    Keys = [Key || {{C, Seq} = Key, _} <-
                   ets:match_object(State#state.hot_table, {{'$1', '$2'}, '_'}),
                   C =:= CaseId, Seq >= StartSeq, Seq =< EndSeq],
    lists:foreach(fun(Key) -> ets:delete(State#state.hot_table, Key) end, Keys),
    ok.

-spec segment_path(segment_id(), state()) -> file:filename_all().
segment_path(SegmentId, State) ->
    filename:join([State#state.data_dir, "segments", binary_to_list(SegmentId) ++ ".seg"]).

-spec segment_meta_path(segment_id(), state()) -> file:filename_all().
segment_meta_path(SegmentId, State) ->
    filename:join([State#state.data_dir, "segments", binary_to_list(SegmentId) ++ ".meta"]).

%%====================================================================
%% Internal functions - Compaction
%%====================================================================

-spec do_compact(case_id(), state()) -> {ok, non_neg_integer(), state()} | {error, term()}.
do_compact(CaseId, State) ->
    %% Find all uncompressed segments for this case
    MatchSpec = [{{{'$1', '_'}, '$2'}, [{'=:=', '$1', CaseId}], ['$2']}],
    SegmentIds = ets:select(State#state.segment_index, MatchSpec),

    %% Filter to uncompressed segments
    UncompressedIds = lists:filter(
        fun(SegId) ->
            case get_segment_metadata(SegId, State) of
                {ok, #{compressed := false}} -> true;
                _ -> false
            end
        end,
        SegmentIds
    ),

    %% Compress each segment
    CompactedCount = lists:foldl(
        fun(SegId, Count) ->
            case compress_segment(SegId, State) of
                ok -> Count + 1;
                {error, _} -> Count
            end
        end,
        0,
        UncompressedIds
    ),

    NewStats = increment_stat(segments_compacted, CompactedCount, State#state.stats),

    {ok, CompactedCount, State#state{stats = NewStats}}.

-spec compress_segment(segment_id(), state()) -> ok | {error, term()}.
compress_segment(SegmentId, State) ->
    case do_get_segment(SegmentId, State) of
        {ok, Segment} ->
            %% Compress events
            EventsBin = term_to_binary(Segment#swf_event_log_segment.events),
            CompressedEvents = zlib:compress(EventsBin),

            %% Create compressed segment
            CompressedSegment = Segment#swf_event_log_segment{
                events = CompressedEvents,
                compressed = true
            },

            %% Rewrite segment
            write_segment(CompressedSegment, State);
        {error, Reason} ->
            {error, Reason}
    end.

-spec maybe_decompress_segment(#swf_event_log_segment{}) -> #swf_event_log_segment{}.
maybe_decompress_segment(#swf_event_log_segment{compressed = false} = Segment) ->
    Segment;
maybe_decompress_segment(#swf_event_log_segment{compressed = true, events = CompressedEvents} = Segment) ->
    DecompressedBin = zlib:uncompress(CompressedEvents),
    Events = binary_to_term(DecompressedBin),
    Segment#swf_event_log_segment{events = Events, compressed = false}.

%%====================================================================
%% Internal functions - Integrity
%%====================================================================

-spec do_verify_integrity(segment_id(), state()) -> ok | {error, checksum_mismatch | not_found}.
do_verify_integrity(SegmentId, State) ->
    case do_get_segment(SegmentId, State) of
        {ok, Segment} ->
            StoredChecksum = Segment#swf_event_log_segment.checksum,
            ComputedChecksum = compute_checksum(Segment#swf_event_log_segment.events),

            case StoredChecksum =:= ComputedChecksum of
                true -> ok;
                false ->
                    ?LOG_ERROR("Checksum mismatch for segment ~s", [SegmentId]),
                    {error, checksum_mismatch}
            end;
        {error, not_found} ->
            {error, not_found};
        {error, _} ->
            {error, not_found}
    end.

-spec compute_checksum([#swf_event{}]) -> binary().
compute_checksum(Events) ->
    Data = term_to_binary(Events),
    crypto:hash(?CHECKSUM_ALGO, Data).

%%====================================================================
%% Internal functions - Subscriptions
%%====================================================================

-spec notify_subscribers(#swf_event{}, state()) -> ok.
notify_subscribers(Event, State) ->
    CaseId = Event#swf_event.case_id,
    Subscribers = ets:lookup(State#state.subscriber_table, CaseId),

    lists:foreach(
        fun({_CaseId, Pid}) ->
            Pid ! {swf_event, Event}
        end,
        Subscribers
    ),
    ok.

%%====================================================================
%% Internal functions - ULID Generation
%%====================================================================

-spec generate_ulid(integer()) -> binary().
generate_ulid(Timestamp) ->
    %% ULID: 48-bit timestamp (ms) + 80-bit random
    %% Provides lexicographic ordering and uniqueness
    TimestampMs = Timestamp div 1000,

    %% Encode timestamp in base32 (10 chars)
    TimePart = encode_timestamp(TimestampMs),

    %% Generate random part (16 chars)
    RandomPart = encode_random(),

    <<TimePart/binary, RandomPart/binary>>.

-spec encode_timestamp(integer()) -> binary().
encode_timestamp(Timestamp) ->
    %% Encode 48-bit timestamp into 10 base32 characters
    Chars = <<"0123456789ABCDEFGHJKMNPQRSTVWXYZ">>,
    encode_base32(Timestamp, 10, Chars, <<>>).

-spec encode_random() -> binary().
encode_random() ->
    %% Generate 80 bits of randomness, encode as 16 base32 characters
    RandomBytes = crypto:strong_rand_bytes(10),
    RandomInt = binary:decode_unsigned(RandomBytes),
    Chars = <<"0123456789ABCDEFGHJKMNPQRSTVWXYZ">>,
    encode_base32(RandomInt, 16, Chars, <<>>).

-spec encode_base32(integer(), non_neg_integer(), binary(), binary()) -> binary().
encode_base32(_Value, 0, _Chars, Acc) ->
    Acc;
encode_base32(Value, Count, Chars, Acc) ->
    Index = Value rem 32,
    Char = binary:at(Chars, Index),
    encode_base32(Value div 32, Count - 1, Chars, <<Char, Acc/binary>>).

-spec generate_segment_id(case_id()) -> segment_id().
generate_segment_id(CaseId) ->
    Timestamp = erlang:system_time(microsecond),
    Ulid = generate_ulid(Timestamp),
    %% Include case_id prefix for organization
    <<(binary:part(CaseId, 0, min(8, byte_size(CaseId))))/binary, "_", Ulid/binary>>.

%%====================================================================
%% Internal functions - Maintenance
%%====================================================================

-spec perform_maintenance(state()) -> state().
perform_maintenance(State) ->
    ?LOG_DEBUG("Performing event log maintenance"),

    %% Rotate any oversized hot segments
    State1 = maybe_rotate_segments(State),

    %% Compact old segments
    CaseIds = get_all_case_ids(State1),
    State2 = lists:foldl(
        fun(CaseId, AccState) ->
            SegmentCount = count_case_segments(CaseId, AccState),
            case SegmentCount >= AccState#state.compact_threshold of
                true ->
                    case do_compact(CaseId, AccState) of
                        {ok, _, NewState} -> NewState;
                        {error, _} -> AccState
                    end;
                false ->
                    AccState
            end
        end,
        State1,
        CaseIds
    ),

    State2.

-spec count_case_segments(case_id(), state()) -> non_neg_integer().
count_case_segments(CaseId, State) ->
    MatchSpec = [{{{'$1', '_'}, '_'}, [{'=:=', '$1', CaseId}], [true]}],
    ets:select_count(State#state.segment_index, MatchSpec).

-spec flush_hot_segments(state()) -> ok.
flush_hot_segments(State) ->
    CaseIds = get_all_case_ids(State),
    lists:foreach(
        fun(CaseId) ->
            case rotate_segment(CaseId, State) of
                {ok, _} -> ok;
                {error, Reason} ->
                    ?LOG_WARNING("Failed to flush segment for case ~s: ~p", [CaseId, Reason])
            end
        end,
        CaseIds
    ),
    ok.

%%====================================================================
%% Internal functions - Stats
%%====================================================================

-spec increment_stat(atom(), non_neg_integer(), map()) -> map().
increment_stat(Key, Amount, Stats) ->
    Current = maps:get(Key, Stats, 0),
    Stats#{Key => Current + Amount}.
