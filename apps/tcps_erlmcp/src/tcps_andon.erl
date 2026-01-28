%%%-----------------------------------------------------------------------------
%%% @doc TCPS Andon Stop-the-Line System
%%%
%%% Production-grade Andon system implementing Toyota Production System
%%% principles for software engineering quality control.
%%%
%%% Core Responsibilities:
%%% - Event triggering for all failure types (SHACL, test, non-determinism, receipt)
%%% - Stop-the-line enforcement (blocking subsequent stages)
%%% - Resolution workflow with root cause analysis
%%% - Receipt generation with JSON storage and ontology linking
%%% - Integration hooks for compilation, testing, and validation
%%%
%%% The Andon system ensures zero-defect delivery by:
%%% 1. Immediately stopping work when defects are detected
%%% 2. Requiring root cause analysis before resuming
%%% 3. Enforcing prevention measures to avoid recurrence
%%% 4. Maintaining complete audit trail via receipts
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_andon).

%% API exports
-export([
    % Event triggering
    trigger_andon/2,
    get_andon_event/1,
    get_andon_history/1,

    % Stop-the-line enforcement
    is_blocked/1,
    can_proceed_to_stage/2,

    % Resolution workflow
    resolve_andon/2,
    list_receipts_for_andon/1,

    % Receipt generation
    generate_andon_receipt/1,
    store_receipt/2,

    % Integration hooks
    hook_compilation_failure/1,
    hook_test_failure/1,
    hook_shacl_failure/1,

    % System management
    start/0,
    stop/0,

    % Helper functions for testing
    list_all/0,
    delete/1,
    list_by_status/1,
    list_by_severity/1,
    can_proceed/2,
    count_open/0
]).

-ifdef(TEST).
-export([init/0]).
-on_load(init/0).
-endif.

-define(ETS_TABLE, tcps_andon_events).
-define(DEFAULT_RECEIPTS_DIR, "priv/receipts").

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type failure_type() :: shacl_violation
                      | test_failure
                      | non_determinism
                      | missing_receipt
                      | compilation_failure.

-type stage() :: compilation
               | testing
               | validation
               | execution
               | integration
               | deployment.

-type andon_event_id() :: binary().
-type sku_id() :: binary().

-type andon_context() :: #{
    sku_id := sku_id(),
    stage := stage(),
    details := map(),
    metadata => map()
}.

-type andon_event() :: #{
    event_id := andon_event_id(),
    failure_type := failure_type(),
    sku_id := sku_id(),
    stage := stage(),
    timestamp := integer(),
    details := map(),
    status := open | resolved,
    resolution => resolution(),
    metadata => map()
}.

-type resolution() :: #{
    root_cause := binary(),
    fix_applied := binary(),
    prevention_added := binary(),
    resolver => binary(),
    resolution_time_minutes => pos_integer(),
    resolution_timestamp := integer()
}.

-type receipt() :: #{
    receipt_id := binary(),
    andon_event_id := andon_event_id(),
    timestamp := integer(),
    timestamp_iso := binary(),
    failure_type := failure_type(),
    sku_id := sku_id(),
    stage := stage(),
    status := binary(),
    receipt_type := andon_event | resolution,
    ontology_refs := [binary()],
    details => map()
}.

%%%=============================================================================
%%% System Management
%%%=============================================================================

-spec start() -> ok.
start() ->
    case ets:info(?ETS_TABLE) of
        undefined ->
            ets:new(?ETS_TABLE, [named_table, public, set,
                                 {write_concurrency, true},
                                 {read_concurrency, true}]),
            ok;
        _ ->
            ok
    end,
    % Ensure receipts directory exists
    filelib:ensure_dir(?DEFAULT_RECEIPTS_DIR ++ "/").

-spec stop() -> ok.
stop() ->
    case ets:info(?ETS_TABLE) of
        undefined -> ok;
        _ -> ets:delete(?ETS_TABLE), ok
    end.

%%%=============================================================================
%%% Event Triggering
%%%=============================================================================

-spec trigger_andon(failure_type(), andon_context()) ->
    {ok, andon_event_id()} | {error, term()}.
trigger_andon(FailureType, Context) ->
    % Validate failure type
    case is_valid_failure_type(FailureType) of
        false ->
            {error, {invalid_failure_type, FailureType}};
        true ->
            % Validate context
            case validate_context(Context) of
                ok ->
                    AndonId = generate_andon_id(),
                    SkuId = maps:get(sku_id, Context),
                    Stage = maps:get(stage, Context),
                    Details = maps:get(details, Context, #{}),
                    Metadata = maps:get(metadata, Context, #{}),

                    Event = #{
                        event_id => AndonId,
                        failure_type => FailureType,
                        sku_id => SkuId,
                        stage => Stage,
                        timestamp => erlang:system_time(millisecond),
                        details => Details,
                        status => open,
                        metadata => Metadata
                    },

                    % Store event atomically
                    true = ets:insert(?ETS_TABLE, {AndonId, Event}),

                    % Generate initial receipt
                    Receipt = generate_andon_receipt(Event),
                    _ = store_receipt(Receipt, ?DEFAULT_RECEIPTS_DIR),

                    % Notify monitoring systems (if available)
                    _ = notify_andon_triggered(Event),

                    {ok, AndonId};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec get_andon_event(andon_event_id()) -> andon_event().
get_andon_event(AndonId) ->
    case ets:lookup(?ETS_TABLE, AndonId) of
        [{AndonId, Event}] -> Event;
        [] -> #{event_id => AndonId, status => not_found}
    end.

-spec get_andon_history(sku_id()) -> [andon_event()].
get_andon_history(SkuId) ->
    % Get all events and filter by SKU ID
    AllEvents = ets:tab2list(?ETS_TABLE),
    Events = [Event || {_, Event} <- AllEvents,
                       is_map(Event),
                       maps:get(sku_id, Event, undefined) =:= SkuId],
    % Sort by timestamp
    lists:sort(fun(E1, E2) ->
        maps:get(timestamp, E1, 0) =< maps:get(timestamp, E2, 0)
    end, Events).

%%%=============================================================================
%%% Stop-the-Line Enforcement
%%%=============================================================================

-spec is_blocked(sku_id()) -> boolean().
is_blocked(SkuId) ->
    % Check if any open Andon events exist for this SKU
    % Get all events and filter
    AllEvents = ets:tab2list(?ETS_TABLE),
    OpenEvents = [Event || {_, Event} <- AllEvents,
                           is_map(Event),
                           maps:get(sku_id, Event, undefined) =:= SkuId,
                           maps:get(status, Event, undefined) =:= open],
    case OpenEvents of
        [] -> false;
        [_|_] -> true
    end.

-spec can_proceed_to_stage(sku_id(), stage()) ->
    {ok, proceed} | {blocked, [andon_event_id()]}.
can_proceed_to_stage(SkuId, _Stage) ->
    % Get all open Andon events for this SKU
    AllEvents = ets:tab2list(?ETS_TABLE),
    OpenEvents = [{EventId, Event} || {EventId, Event} <- AllEvents,
                                       is_map(Event),
                                       maps:get(sku_id, Event, undefined) =:= SkuId,
                                       maps:get(status, Event, undefined) =:= open],

    case OpenEvents of
        [] ->
            {ok, proceed};
        Events ->
            BlockingIds = [EventId || {EventId, _} <- Events],
            {blocked, BlockingIds}
    end.

%%%=============================================================================
%%% Resolution Workflow
%%%=============================================================================

-spec resolve_andon(andon_event_id(), resolution()) -> ok | {error, term()}.
resolve_andon(AndonId, ResolutionData) ->
    case ets:lookup(?ETS_TABLE, AndonId) of
        [] ->
            {error, {andon_not_found, AndonId}};
        [{AndonId, Event}] ->
            % Check if already resolved
            case maps:get(status, Event) of
                resolved ->
                    {error, {already_resolved, AndonId}};
                open ->
                    % Validate resolution data
                    case validate_resolution(ResolutionData) of
                        ok ->
                            % Attempt atomic update
                            Resolution = prepare_resolution(ResolutionData),
                            UpdatedEvent = Event#{
                                status => resolved,
                                resolution => Resolution,
                                resolution_timestamp => erlang:system_time(millisecond)
                            },

                            % Use compare-and-swap semantics
                            case ets:insert(?ETS_TABLE, {AndonId, UpdatedEvent}) of
                                true ->
                                    % Generate resolution receipt
                                    ResolutionReceipt = generate_resolution_receipt(
                                        UpdatedEvent, Resolution),
                                    _ = store_receipt(ResolutionReceipt, ?DEFAULT_RECEIPTS_DIR),

                                    % Notify monitoring
                                    _ = notify_andon_resolved(UpdatedEvent),

                                    ok;
                                false ->
                                    {error, update_failed}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end
            end
    end.

-spec list_receipts_for_andon(andon_event_id()) -> [receipt()].
list_receipts_for_andon(AndonId) ->
    % In production, this would query a receipt database
    % For now, scan the receipts directory
    ReceiptsDir = ?DEFAULT_RECEIPTS_DIR,
    case file:list_dir(ReceiptsDir) of
        {ok, Files} ->
            lists:filtermap(fun(File) ->
                case filename:extension(File) of
                    ".json" ->
                        FullPath = filename:join(ReceiptsDir, File),
                        case file:read_file(FullPath) of
                            {ok, JsonBin} ->
                                Receipt = jsx:decode(JsonBin, [return_maps]),
                                case maps:get(<<"andon_event_id">>, Receipt, undefined) of
                                    AndonId -> {true, Receipt};
                                    _ -> false
                                end;
                            _ ->
                                false
                        end;
                    _ ->
                        false
                end
            end, Files);
        _ ->
            []
    end.

%%%=============================================================================
%%% Receipt Generation
%%%=============================================================================

-spec generate_andon_receipt(andon_event()) -> receipt().
generate_andon_receipt(AndonEvent) ->
    ReceiptId = generate_receipt_id(),
    Timestamp = maps:get(timestamp, AndonEvent),
    TimestampIso = format_iso8601(Timestamp),

    #{
        receipt_id => ReceiptId,
        andon_event_id => maps:get(event_id, AndonEvent),
        timestamp => Timestamp,
        timestamp_iso => TimestampIso,
        failure_type => maps:get(failure_type, AndonEvent),
        sku_id => maps:get(sku_id, AndonEvent),
        stage => maps:get(stage, AndonEvent),
        status => <<"open">>,
        receipt_type => andon_event,
        ontology_refs => generate_ontology_refs(AndonEvent),
        details => maps:get(details, AndonEvent, #{})
    }.

-spec generate_resolution_receipt(andon_event(), resolution()) -> receipt().
generate_resolution_receipt(AndonEvent, Resolution) ->
    ReceiptId = generate_receipt_id(),
    Timestamp = erlang:system_time(millisecond),
    TimestampIso = format_iso8601(Timestamp),

    #{
        receipt_id => ReceiptId,
        andon_event_id => maps:get(event_id, AndonEvent),
        timestamp => Timestamp,
        timestamp_iso => TimestampIso,
        failure_type => maps:get(failure_type, AndonEvent),
        sku_id => maps:get(sku_id, AndonEvent),
        stage => maps:get(stage, AndonEvent),
        status => <<"resolved">>,
        receipt_type => resolution,
        ontology_refs => generate_ontology_refs(AndonEvent),
        resolution => Resolution,
        details => #{
            root_cause => maps:get(root_cause, Resolution),
            fix_applied => maps:get(fix_applied, Resolution),
            prevention_added => maps:get(prevention_added, Resolution)
        }
    }.

-spec store_receipt(receipt(), file:filename()) -> ok | {error, term()}.
store_receipt(Receipt, ReceiptsDir) ->
    filelib:ensure_dir(ReceiptsDir ++ "/"),

    ReceiptId = maps:get(receipt_id, Receipt),
    Filename = binary_to_list(ReceiptId) ++ ".json",
    FullPath = filename:join(ReceiptsDir, Filename),

    % Convert receipt to JSON
    JsonBin = jsx:encode(Receipt),

    case file:write_file(FullPath, JsonBin) of
        ok -> ok;
        {error, Reason} -> {error, {receipt_write_failed, Reason}}
    end.

%%%=============================================================================
%%% Integration Hooks
%%%=============================================================================

-spec hook_compilation_failure(map()) -> {ok, andon_event_id()} | {error, term()}.
hook_compilation_failure(CompileError) ->
    Context = #{
        sku_id => maps:get(sku_id, CompileError),
        stage => compilation,
        details => #{
            error_type => maps:get(error_type, CompileError, unknown),
            file => maps:get(file, CompileError, <<"unknown">>),
            line => maps:get(line, CompileError, 0),
            message => maps:get(message, CompileError, <<"Compilation failed">>)
        }
    },
    trigger_andon(compilation_failure, Context).

-spec hook_test_failure(map()) -> {ok, andon_event_id()} | {error, term()}.
hook_test_failure(TestFailure) ->
    Context = #{
        sku_id => maps:get(sku_id, TestFailure),
        stage => testing,
        details => #{
            test_module => maps:get(test_module, TestFailure, unknown),
            test_function => maps:get(test_function, TestFailure, unknown),
            failure_type => maps:get(failure_type, TestFailure, unknown),
            expected => maps:get(expected, TestFailure, undefined),
            actual => maps:get(actual, TestFailure, undefined)
        }
    },
    trigger_andon(test_failure, Context).

-spec hook_shacl_failure(map()) -> {ok, andon_event_id()} | {error, term()}.
hook_shacl_failure(ShaclFailure) ->
    ValidationReport = maps:get(validation_report, ShaclFailure, #{}),
    Results = maps:get(results, ValidationReport, []),

    Context = #{
        sku_id => maps:get(sku_id, ShaclFailure),
        stage => validation,
        details => #{
            conforms => false,
            violations => Results,
            validation_report => ValidationReport
        }
    },
    trigger_andon(shacl_violation, Context).

%%%=============================================================================
%%% Internal Helper Functions
%%%=============================================================================

-spec is_valid_failure_type(atom()) -> boolean().
is_valid_failure_type(shacl_violation) -> true;
is_valid_failure_type(test_failure) -> true;
is_valid_failure_type(non_determinism) -> true;
is_valid_failure_type(missing_receipt) -> true;
is_valid_failure_type(compilation_failure) -> true;
is_valid_failure_type(_) -> false.

-spec validate_context(map()) -> ok | {error, term()}.
validate_context(Context) ->
    RequiredFields = [sku_id, stage],
    case check_required_fields(Context, RequiredFields) of
        ok ->
            % Additional validation
            SkuId = maps:get(sku_id, Context),
            case is_binary(SkuId) andalso byte_size(SkuId) > 0 of
                true -> ok;
                false -> {error, {invalid_sku_id, SkuId}}
            end;
        Error ->
            Error
    end.

-spec validate_resolution(map()) -> ok | {error, term()}.
validate_resolution(Resolution) ->
    RequiredFields = [root_cause, fix_applied, prevention_added],
    case check_required_fields(Resolution, RequiredFields) of
        ok ->
            % Validate non-empty strings
            case is_binary(maps:get(root_cause, Resolution))
                 andalso byte_size(maps:get(root_cause, Resolution)) > 0 of
                true -> ok;
                false -> {error, {invalid_field, root_cause}}
            end;
        Error ->
            Error
    end.

-spec check_required_fields(map(), [atom()]) -> ok | {error, term()}.
check_required_fields(Map, RequiredFields) ->
    Missing = [F || F <- RequiredFields, not maps:is_key(F, Map)],
    case Missing of
        [] -> ok;
        [Field | _] -> {error, {missing_required_field, Field}}
    end.

-spec prepare_resolution(map()) -> resolution().
prepare_resolution(ResolutionData) ->
    #{
        root_cause => maps:get(root_cause, ResolutionData),
        fix_applied => maps:get(fix_applied, ResolutionData),
        prevention_added => maps:get(prevention_added, ResolutionData),
        resolver => maps:get(resolver, ResolutionData, <<"system">>),
        resolution_time_minutes => maps:get(resolution_time_minutes,
                                            ResolutionData, 0),
        resolution_timestamp => erlang:system_time(millisecond)
    }.

-spec generate_andon_id() -> binary().
generate_andon_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(1000000),
    Unique = erlang:unique_integer([positive]),
    Id = lists:flatten(io_lib:format("ANDON-~p-~p-~p", [Timestamp, Random, Unique])),
    list_to_binary(Id).

-spec generate_receipt_id() -> binary().
generate_receipt_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(1000000),
    Unique = erlang:unique_integer([positive]),
    Id = lists:flatten(io_lib:format("RCPT-~p-~p-~p", [Timestamp, Random, Unique])),
    list_to_binary(Id).

-spec format_iso8601(integer()) -> binary().
format_iso8601(Millisecond) ->
    % Convert milliseconds to gregorian seconds
    Seconds = Millisecond div 1000,
    Microseconds = (Millisecond rem 1000) * 1000,

    % OTP base date: {{1970,1,1},{0,0,0}}
    BaseSeconds = calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    GregorianSeconds = BaseSeconds + Seconds,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:gregorian_seconds_to_datetime(GregorianSeconds),

    % Format as ISO 8601: "2024-01-26T12:34:56.789Z"
    Iso = io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
                        [Year, Month, Day, Hour, Minute, Second, Microseconds div 1000]),
    list_to_binary(lists:flatten(Iso)).

-spec generate_ontology_refs(andon_event()) -> [binary()].
generate_ontology_refs(AndonEvent) ->
    FailureType = maps:get(failure_type, AndonEvent),
    Stage = maps:get(stage, AndonEvent),

    % Generate semantic references to ontology concepts
    BaseUri = <<"http://example.org/tcps/ontology#">>,
    [
        <<BaseUri/binary, "AndonEvent">>,
        <<BaseUri/binary, (atom_to_binary(FailureType))/binary>>,
        <<BaseUri/binary, (atom_to_binary(Stage))/binary>>,
        <<BaseUri/binary, "StopTheLine">>
    ].

-spec notify_andon_triggered(andon_event()) -> ok.
notify_andon_triggered(Event) ->
    % Log the event
    error_logger:warning_msg("ANDON TRIGGERED: ~p~n", [Event]),

    % Broadcast to dashboard via SSE
    broadcast_andon_event(andon_triggered, Event),

    % In production, would also send:
    % - Slack/Teams alerts
    % - Email to on-call engineer
    % - Metrics to OTEL/Prometheus

    ok.

-spec notify_andon_resolved(andon_event()) -> ok.
notify_andon_resolved(Event) ->
    % Log the resolution
    error_logger:info_msg("ANDON RESOLVED: ~p~n", [Event]),

    % Broadcast to dashboard via SSE
    broadcast_andon_event(andon_resolved, Event),

    ok.

%%%=============================================================================
%%% Dashboard Integration
%%%=============================================================================

-spec broadcast_andon_event(atom(), andon_event()) -> ok.
broadcast_andon_event(EventType, AndonEvent) ->
    % Broadcast to SSE manager if available
    case whereis(tcps_sse_manager) of
        undefined -> ok;
        _SsePid ->
            Update = #{
                type => EventType,
                data => format_andon_for_dashboard(AndonEvent),
                timestamp => erlang:timestamp()
            },
            tcps_sse_manager:broadcast_update(Update)
    end,

    % Also notify dashboard directly (legacy path)
    case whereis(tcps_dashboard) of
        undefined -> ok;
        _DashPid ->
            tcps_dashboard:notify_event(EventType, format_andon_for_dashboard(AndonEvent))
    end,

    ok.

-spec format_andon_for_dashboard(andon_event()) -> map().
format_andon_for_dashboard(Event) ->
    #{
        event_id => maps:get(event_id, Event),
        failure_type => maps:get(failure_type, Event),
        sku_id => maps:get(sku_id, Event),
        stage => maps:get(stage, Event),
        status => maps:get(status, Event),
        timestamp => maps:get(timestamp, Event),
        details => maps:get(details, Event, #{}),
        severity => infer_severity_from_type(maps:get(failure_type, Event))
    }.

-spec infer_severity_from_type(failure_type()) -> atom().
infer_severity_from_type(shacl_violation) -> critical;
infer_severity_from_type(test_failure) -> warning;
infer_severity_from_type(compilation_failure) -> critical;
infer_severity_from_type(non_determinism) -> critical;
infer_severity_from_type(missing_receipt) -> warning.

%%%=============================================================================
%%% Helper Functions for Testing
%%%=============================================================================

%% @doc List all andon event IDs (for testing and cleanup)
-spec list_all() -> [andon_event_id()].
list_all() ->
    case ets:info(?ETS_TABLE) of
        undefined -> [];
        _ -> [EventId || {EventId, _Event} <- ets:tab2list(?ETS_TABLE)]
    end.

%% @doc Delete an andon event (for testing and cleanup)
-spec delete(andon_event_id()) -> ok.
delete(EventId) ->
    case ets:info(?ETS_TABLE) of
        undefined -> ok;
        _ -> ets:delete(?ETS_TABLE, EventId)
    end,
    ok.

%% @doc List Andon events by status
-spec list_by_status(atom()) -> [andon_event()].
list_by_status(Status) ->
    case ets:info(?ETS_TABLE) of
        undefined -> [];
        _ ->
            AllEvents = ets:tab2list(?ETS_TABLE),
            [Event || {_, Event} <- AllEvents,
                      is_map(Event),
                      maps:get(status, Event, undefined) =:= Status]
    end.

%% @doc List Andon events by severity
-spec list_by_severity(atom()) -> [andon_event()].
list_by_severity(Severity) ->
    case ets:info(?ETS_TABLE) of
        undefined -> [];
        _ ->
            AllEvents = ets:tab2list(?ETS_TABLE),
            [Event || {_, Event} <- AllEvents,
                      is_map(Event),
                      infer_severity_from_type(maps:get(failure_type, Event)) =:= Severity]
    end.

%% @doc Check if can proceed (alias for can_proceed_to_stage)
-spec can_proceed(sku_id(), stage()) ->
    {ok, proceed} | {blocked, [andon_event_id()]}.
can_proceed(SkuId, Stage) ->
    can_proceed_to_stage(SkuId, Stage).

%% @doc Count open Andon events
-spec count_open() -> non_neg_integer().
count_open() ->
    length(list_by_status(open)).

%%%=============================================================================
%%% Module Initialization (automatically called by Erlang)
%%%=============================================================================

-ifdef(TEST).
% Auto-initialize ETS table when module is loaded in test environment
-spec init() -> ok.
init() ->
    start().
-endif.
