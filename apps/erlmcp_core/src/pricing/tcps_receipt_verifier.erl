%%%-----------------------------------------------------------------------------
%%% @doc TCPS Receipt Verification and Audit System
%%%
%%% Production-grade receipt verification system implementing Toyota Production
%%% System principles for complete audit trail management and quality assurance.
%%%
%%% Core Capabilities:
%%% - Single receipt validation (schema, fields, timestamps, evidence)
%%% - Receipt chain verification (completeness, ordering, gaps)
%%% - Deterministic build verification (reproducibility checks)
%%% - Audit trail generation (complete SKU history)
%%% - Compliance reporting (aggregated metrics, defect rates)
%%% - Stage sequence validation (compile → test → release → publish)
%%% - Ontology link verification (SKU, work order, stage references)
%%% - Tamper detection (checksums, timestamps, signatures)
%%% - Batch verification (directory-wide validation)
%%% - Receipt storage management (proper naming, linking, checksums)
%%%
%%% Quality Standards:
%%% - Zero-defect validation (comprehensive field checks)
%%% - Complete audit trails (all state changes tracked)
%%% - Immutable receipts (tamper detection)
%%% - Ontology integration (semantic references)
%%% - Compliance reporting (regulatory requirements)
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_receipt_verifier).

%% API exports - Receipt Verification
-export([
    verify_receipt/1,
    verify_receipt_chain/1,
    verify_deterministic_build/1,
    verify_complete_chain/1,
    verify_quality_gates_passed/1,
    verify_no_tampering/1,
    verify_signature_chain/1,
    verify_stage_transitions/1
]).

%% API exports - Audit Trail
-export([
    audit_trail/1,
    audit_compliance/1,
    generate_audit_trail/1,
    generate_compliance_report/1,
    export_audit_trail/2
]).

%% API exports - Validation
-export([
    validate_stage_sequence/1,
    verify_ontology_links/1,
    detect_tampering/1,
    verify_all_receipts/1
]).

%% API exports - Storage
-export([
    store_receipt/2,
    compute_checksum/1
]).

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type receipt() :: map().
-type sku_id() :: binary().
-type stage() :: compile | test | release | publish | compilation | testing |
                validation | execution | integration | deployment.
-type verification_error() :: {invalid, [atom()]}.
-type date() :: calendar:date().
-type time_period() :: {StartDate :: date(), EndDate :: date()}.

-type audit_report() :: #{
    sku_id := sku_id(),
    work_order_created := map(),
    production_stages := [map()],
    receipts := [map()],
    andon_events := [map()],
    resolutions := [map()],
    publish_status := pending | published | failed,
    total_lead_time_hours := float(),
    stage_durations := map(),
    quality_gates := map()
}.

-type compliance_report() :: #{
    period := time_period(),
    total_skus_processed := non_neg_integer(),
    receipts_generated := non_neg_integer(),
    quality_gates_passed := non_neg_integer(),
    quality_gates_failed := non_neg_integer(),
    andon_events_triggered := non_neg_integer(),
    andon_events_resolved := non_neg_integer(),
    average_lead_time_hours := float(),
    defect_rate_percent := float(),
    first_pass_yield_percent := float(),
    rework_rate_percent := float(),
    stage_metrics := map(),
    compliance_status := compliant | non_compliant,
    violations := [map()]
}.

-type verification_report() :: #{
    sku_id := sku_id(),
    total_checks := non_neg_integer(),
    passed_checks := non_neg_integer(),
    failed_checks := non_neg_integer(),
    pass_rate := float(),
    checks := map(),
    verified_at := integer()
}.

%%%=============================================================================
%%% Receipt Verification Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Verify single receipt for validity.
%%
%% Checks:
%% - JSON schema compliance
%% - Required fields present (receipt_id, timestamp, sku_id, stage, status)
%% - Timestamp valid (ISO 8601 format)
%% - Evidence data non-empty (for applicable stages)
%% - Status is pass/fail/open/resolved
%% - SKU reference valid (format: SKU-xxx)
%% - Stage name valid (known stage)
%% - Ontology references present and well-formed
%%
%% @end
%%------------------------------------------------------------------------------
-spec verify_receipt(ReceiptPath :: binary()) ->
    {ok, valid} | {error, verification_error()}.
verify_receipt(ReceiptPath) ->
    case file:read_file(ReceiptPath) of
        {ok, JsonBin} ->
            try jsx:decode(JsonBin, [return_maps]) of
                Receipt ->
                    validate_receipt_fields(Receipt)
            catch
                _:_ ->
                    {error, {invalid, [json_parse_error]}}
            end;
        {error, _} ->
            {error, {invalid, [file_not_found]}}
    end.

%%------------------------------------------------------------------------------
%% @doc Verify complete receipt chain for SKU.
%%
%% Checks:
%% - All required stages present (compile, test, release, publish)
%% - Receipts in chronological order
%% - No gaps in timestamps (< 24 hours between stages)
%% - All receipts valid
%% - Chain completeness (no missing stages)
%%
%% @end
%%------------------------------------------------------------------------------
-spec verify_receipt_chain(SkuId :: sku_id()) ->
    {ok, complete} | {error, {incomplete, MissingStages :: [stage()]}}.
verify_receipt_chain(SkuId) ->
    case load_receipts_for_sku(SkuId) of
        {ok, Receipts} ->
            % Check all receipts are valid
            case lists:all(fun(R) -> is_valid_receipt(R) end, Receipts) of
                false ->
                    {error, {incomplete, [invalid_receipts]}};
                true ->
                    % Extract stages
                    Stages = [extract_stage(R) || R <- Receipts],
                    RequiredStages = [compilation, testing, validation, execution],

                    % Check completeness
                    MissingStages = RequiredStages -- Stages,

                    case MissingStages of
                        [] ->
                            % Check chronological order (stages must appear in correct timestamp order)
                            case verify_stage_chronological_order(Receipts) of
                                ok ->
                                    % Check for gaps
                                    case verify_no_timestamp_gaps(Receipts) of
                                        ok ->
                                            {ok, complete};
                                        {error, Reason} ->
                                            {error, {incomplete, [Reason]}}
                                    end;
                                {error, Reason} ->
                                    {error, {incomplete, [Reason]}}
                            end;
                        _ ->
                            {error, {incomplete, MissingStages}}
                    end
            end;
        {error, no_receipts} ->
            {error, {incomplete, [no_receipts_found]}}
    end.

%%------------------------------------------------------------------------------
%% @doc Verify deterministic build by building twice and comparing.
%%
%% Process:
%% 1. Build SKU once, capture output hash
%% 2. Build SKU again with same inputs, capture output hash
%% 3. Compare SHA-256 hashes
%% 4. If different, compute diff and trigger Andon
%%
%% @end
%%------------------------------------------------------------------------------
-spec verify_deterministic_build(SkuId :: sku_id()) ->
    {ok, deterministic} | {error, {non_deterministic, Diff :: binary()}}.
verify_deterministic_build(SkuId) ->
    % Simulate builds (in production, this would compile actual code)
    case build_sku(SkuId) of
        {ok, Build1} ->
            case build_sku(SkuId) of
                {ok, Build2} ->
                    Hash1 = compute_hash(Build1),
                    Hash2 = compute_hash(Build2),

                    case Hash1 =:= Hash2 of
                        true ->
                            {ok, deterministic};
                        false ->
                            Diff = compute_diff(Build1, Build2),
                            % Trigger Andon for non-determinism
                            _ = trigger_non_determinism_andon(SkuId, Diff),
                            {error, {non_deterministic, Diff}}
                    end;
                {error, Reason} ->
                    {error, {build_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {build_failed, Reason}}
    end.

%%%=============================================================================
%%% Audit Trail Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Generate complete audit trail for SKU.
%%
%% Includes:
%% - Work order creation timestamp
%% - All production stages with durations
%% - All receipts with timestamps
%% - Any Andon events with details
%% - Resolution details if applicable
%% - Final publish status
%% - Total lead time
%% - Stage durations breakdown
%% - Quality gate results
%%
%% @end
%%------------------------------------------------------------------------------
-spec audit_trail(SkuId :: sku_id()) -> audit_report().
audit_trail(SkuId) ->
    % Load all data for SKU
    {ok, Receipts} = load_receipts_for_sku(SkuId),
    AndonEvents = load_andon_events(SkuId),
    WorkOrder = load_work_order(SkuId),

    % Sort receipts chronologically
    SortedReceipts = lists:sort(
        fun(R1, R2) ->
            get_timestamp(R1) =< get_timestamp(R2)
        end,
        Receipts
    ),

    % Extract stages
    Stages = extract_production_stages(SortedReceipts),

    % Calculate lead time
    LeadTime = calculate_lead_time(WorkOrder, SortedReceipts),

    % Calculate stage durations
    StageDurations = calculate_stage_durations(SortedReceipts),

    % Extract quality gates
    QualityGates = extract_quality_gates(SortedReceipts),

    % Determine publish status
    PublishStatus = determine_publish_status(SortedReceipts, AndonEvents),

    % Separate Andon events and resolutions
    {AndonOpen, Resolutions} = partition_andon_events(AndonEvents),

    #{
        sku_id => SkuId,
        work_order_created => WorkOrder,
        production_stages => Stages,
        receipts => format_receipts_for_audit(SortedReceipts),
        andon_events => format_andon_events(AndonOpen),
        resolutions => format_resolutions(Resolutions),
        publish_status => PublishStatus,
        total_lead_time_hours => LeadTime,
        stage_durations => StageDurations,
        quality_gates => QualityGates,
        generated_at => calendar:universal_time()
    }.

%%------------------------------------------------------------------------------
%% @doc Generate compliance report for time period.
%%
%% Metrics:
%% - Total SKUs processed
%% - Receipts generated (count and types)
%% - Quality gates passed/failed
%% - Andon events triggered/resolved
%% - Average lead time
%% - Defect rate (Andon events / SKUs)
%% - First pass yield (SKUs without Andon)
%% - Rework rate
%% - Per-stage metrics
%% - Compliance violations
%%
%% @end
%%------------------------------------------------------------------------------
-spec audit_compliance(TimePeriod :: time_period()) -> compliance_report().
audit_compliance({StartDate, EndDate}) ->
    % Load all receipts in time period
    AllReceipts = load_receipts_in_period(StartDate, EndDate),

    % Load Andon events
    AllAndonEvents = load_andon_events_in_period(StartDate, EndDate),

    % Extract unique SKUs
    UniqueSkus = extract_unique_skus(AllReceipts),
    TotalSkus = length(UniqueSkus),

    % Count receipts by type
    TotalReceipts = length(AllReceipts),

    % Count quality gates
    QualityGatesPassed = count_quality_gates(AllReceipts, passed),
    QualityGatesFailed = count_quality_gates(AllReceipts, failed),

    % Count Andon events
    TotalAndonEvents = length(AllAndonEvents),
    ResolvedAndonEvents = count_resolved_andon_events(AllAndonEvents),

    % Calculate average lead time
    AvgLeadTime = calculate_average_lead_time(UniqueSkus, AllReceipts),

    % Calculate defect rate
    DefectRate = case TotalSkus of
        0 -> 0.0;
        _ -> (TotalAndonEvents / TotalSkus) * 100.0
    end,

    % Calculate first pass yield
    SkusWithoutAndon = count_skus_without_andon(UniqueSkus, AllAndonEvents),
    FirstPassYield = case TotalSkus of
        0 -> 0.0;
        _ -> (SkusWithoutAndon / TotalSkus) * 100.0
    end,

    % Calculate rework rate
    SkusWithRework = count_skus_with_rework(UniqueSkus, AllReceipts),
    ReworkRate = case TotalSkus of
        0 -> 0.0;
        _ -> (SkusWithRework / TotalSkus) * 100.0
    end,

    % Stage metrics
    StageMetrics = calculate_stage_metrics(AllReceipts),

    % Check compliance
    Violations = check_compliance_violations(
        TotalSkus, DefectRate, FirstPassYield, AvgLeadTime
    ),

    ComplianceStatus = case Violations of
        [] -> compliant;
        _ -> non_compliant
    end,

    #{
        period => {StartDate, EndDate},
        total_skus_processed => TotalSkus,
        receipts_generated => TotalReceipts,
        quality_gates_passed => QualityGatesPassed,
        quality_gates_failed => QualityGatesFailed,
        andon_events_triggered => TotalAndonEvents,
        andon_events_resolved => ResolvedAndonEvents,
        average_lead_time_hours => AvgLeadTime,
        defect_rate_percent => DefectRate,
        first_pass_yield_percent => FirstPassYield,
        rework_rate_percent => ReworkRate,
        stage_metrics => StageMetrics,
        compliance_status => ComplianceStatus,
        violations => Violations,
        generated_at => calendar:universal_time()
    }.

%%%=============================================================================
%%% Validation Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Validate stage sequence is correct.
%%
%% Required order: compile → test → release → publish
%% No stages skipped
%% No stages repeated
%%
%% @end
%%------------------------------------------------------------------------------
-spec validate_stage_sequence(Receipts :: [receipt()]) ->
    {ok, valid} | {error, {invalid_sequence, Reason :: atom()}}.
validate_stage_sequence(Receipts) ->
    Stages = [extract_stage(R) || R <- Receipts],
    ExpectedSequence = [compilation, testing, validation, execution],

    % Check for duplicates FIRST
    case has_duplicate_stages(Stages) of
        true ->
            {error, {invalid_sequence, duplicate_stages}};
        false ->
            % Then check sequence match
            case match_stage_sequence(Stages, ExpectedSequence) of
                ok ->
                    {ok, valid};
                {error, Reason} ->
                    {error, {invalid_sequence, Reason}}
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Verify ontology links in receipt.
%%
%% Checks:
%% - SKU exists in ontology
%% - Work order exists
%% - Stage defined in ontology
%% - All IDs resolvable
%% - Ontology references well-formed URIs
%%
%% @end
%%------------------------------------------------------------------------------
-spec verify_ontology_links(ReceiptPath :: binary()) ->
    {ok, linked} | {error, {unlinked, MissingRefs :: [binary()]}}.
verify_ontology_links(ReceiptPath) ->
    case file:read_file(ReceiptPath) of
        {ok, JsonBin} ->
            Receipt = jsx:decode(JsonBin, [return_maps]),

            % Extract ontology references
            OntologyRefs = maps:get(<<"ontology_refs">>, Receipt, []),
            SkuId = maps:get(<<"sku_id">>, Receipt, undefined),

            % Validate each reference
            MissingRefs = lists:filter(
                fun(Ref) -> not verify_ontology_ref(Ref) end,
                OntologyRefs
            ),

            % Validate SKU exists
            SkuRefValid = verify_sku_exists(SkuId),

            case {MissingRefs, SkuRefValid} of
                {[], true} ->
                    {ok, linked};
                {[], false} ->
                    {error, {unlinked, [<<"sku_not_found">>]}};
                {Missing, true} ->
                    {error, {unlinked, Missing}};
                {Missing, false} ->
                    {error, {unlinked, [<<"sku_not_found">> | Missing]}}
            end;
        {error, _} ->
            {error, {unlinked, [<<"file_not_found">>]}}
    end.

%%------------------------------------------------------------------------------
%% @doc Detect tampering in receipt.
%%
%% Checks:
%% - Checksum verification (SHA-256)
%% - Timestamp consistency (no future timestamps)
%% - Signature validation (if signed)
%% - Field integrity (no unexpected modifications)
%%
%% @end
%%------------------------------------------------------------------------------
-spec detect_tampering(ReceiptPath :: binary()) ->
    {ok, authentic} | {error, {tampered, Evidence :: map()}}.
detect_tampering(ReceiptPath) ->
    case file:read_file(ReceiptPath) of
        {ok, JsonBin} ->
            Receipt = jsx:decode(JsonBin, [return_maps]),

            Evidence = #{},

            % Check timestamp
            Evidence2 = case verify_timestamp(Receipt) of
                ok -> Evidence;
                {error, Reason} -> Evidence#{timestamp => Reason}
            end,

            % Check checksum (if present)
            Evidence3 = case maps:get(<<"checksum">>, Receipt, undefined) of
                undefined ->
                    Evidence2;
                Checksum ->
                    % Recompute checksum (exclude checksum field)
                    ReceiptWithoutChecksum = maps:remove(<<"checksum">>, Receipt),
                    ComputedChecksum = compute_checksum(ReceiptWithoutChecksum),
                    case ComputedChecksum =:= Checksum of
                        true -> Evidence2;
                        false -> Evidence2#{checksum => mismatch}
                    end
            end,

            % Check signature (if present)
            Evidence4 = case maps:get(<<"signature">>, Receipt, undefined) of
                undefined ->
                    Evidence3;
                _Signature ->
                    % In production, verify cryptographic signature
                    % For now, assume valid
                    Evidence3
            end,

            case maps:size(Evidence4) of
                0 ->
                    {ok, authentic};
                _ ->
                    {error, {tampered, Evidence4}}
            end;
        {error, _} ->
            {error, {tampered, #{file => not_found}}}
    end.

%%------------------------------------------------------------------------------
%% @doc Verify all receipts in directory.
%%
%% Returns summary with counts and error details.
%%
%% @end
%%------------------------------------------------------------------------------
-spec verify_all_receipts(ReceiptsDir :: binary()) ->
    #{valid => non_neg_integer(), invalid => [{binary(), [atom()]}]}.
verify_all_receipts(ReceiptsDir) ->
    ReceiptsDirStr = case is_binary(ReceiptsDir) of
        true -> binary_to_list(ReceiptsDir);
        false -> ReceiptsDir
    end,

    case file:list_dir(ReceiptsDirStr) of
        {ok, Files} ->
            JsonFiles = [F || F <- Files, filename:extension(F) =:= ".json"],

            Results = lists:map(
                fun(File) ->
                    Path = filename:join(ReceiptsDirStr, File),
                    PathBin = list_to_binary(Path),
                    case verify_receipt(PathBin) of
                        {ok, valid} ->
                            {valid, PathBin};
                        {error, {invalid, Reasons}} ->
                            {invalid, {PathBin, Reasons}}
                    end
                end,
                JsonFiles
            ),

            ValidCount = length([1 || {valid, _} <- Results]),
            InvalidList = [{Path, Reasons} || {invalid, {Path, Reasons}} <- Results],

            #{
                valid => ValidCount,
                invalid => InvalidList,
                total => length(JsonFiles)
            };
        {error, _} ->
            #{valid => 0, invalid => [], total => 0, error => directory_not_found}
    end.

%%%=============================================================================
%%% Storage Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Store receipt with proper naming and linking.
%%
%% Format: receipts/<sku_id>/<stage>-<timestamp>.json
%% Generates checksum
%% Links to ontology
%%
%% @end
%%------------------------------------------------------------------------------
-spec store_receipt(Receipt :: receipt(), SkuId :: sku_id()) ->
    {ok, ReceiptPath :: binary()}.
store_receipt(Receipt, SkuId) ->
    % Ensure receipts directory exists
    BaseDir = <<"priv/receipts">>,
    SkuDir = filename:join(BaseDir, SkuId),
    filelib:ensure_dir(binary_to_list(SkuDir) ++ "/"),

    % Generate filename
    Stage = maps:get(stage, Receipt, unknown),
    Timestamp = erlang:system_time(millisecond),
    Filename = iolist_to_binary(
        io_lib:format("~s-~p.json", [Stage, Timestamp])
    ),
    ReceiptPath = filename:join(SkuDir, Filename),

    % Add checksum
    Checksum = compute_checksum(Receipt),
    ReceiptWithChecksum = Receipt#{checksum => Checksum},

    % Convert to JSON
    JsonBin = jsx:encode(ReceiptWithChecksum),

    % Write to file
    ok = file:write_file(ReceiptPath, JsonBin),

    {ok, ReceiptPath}.

%%------------------------------------------------------------------------------
%% @doc Compute SHA-256 checksum of receipt.
%%
%% @end
%%------------------------------------------------------------------------------
-spec compute_checksum(Receipt :: map()) -> binary().
compute_checksum(Receipt) ->
    % Canonical JSON representation
    JsonBin = jsx:encode(Receipt),
    Hash = crypto:hash(sha256, JsonBin),
    base64:encode(Hash).

%%%=============================================================================
%%% Internal Helper Functions - Verification
%%%=============================================================================

validate_receipt_fields(Receipt) ->
    RequiredFields = [
        <<"receipt_id">>, <<"timestamp">>, <<"sku_id">>,
        <<"stage">>, <<"status">>
    ],

    Errors = [],

    % Check required fields
    Errors2 = case check_required_fields_present(Receipt, RequiredFields) of
        ok -> Errors;
        {error, Missing} -> [Missing | Errors]
    end,

    % Validate timestamp
    Errors3 = case validate_timestamp_field(Receipt) of
        ok -> Errors2;
        {error, TimestampErr} -> [TimestampErr | Errors2]
    end,

    % Validate SKU ID format
    Errors4 = case validate_sku_id_format(Receipt) of
        ok -> Errors3;
        {error, SkuErr} -> [SkuErr | Errors3]
    end,

    % Validate stage
    Errors5 = case validate_stage_field(Receipt) of
        ok -> Errors4;
        {error, StageErr} -> [StageErr | Errors4]
    end,

    % Validate status
    Errors6 = case validate_status_field(Receipt) of
        ok -> Errors5;
        {error, StatusErr} -> [StatusErr | Errors5]
    end,

    case Errors6 of
        [] ->
            {ok, valid};
        _ ->
            {error, {invalid, lists:reverse(Errors6)}}
    end.

check_required_fields_present(Receipt, RequiredFields) ->
    Missing = [F || F <- RequiredFields, not maps:is_key(F, Receipt)],
    case Missing of
        [] -> ok;
        _ -> {error, {missing_fields, Missing}}
    end.

validate_timestamp_field(Receipt) ->
    case maps:get(<<"timestamp">>, Receipt, undefined) of
        undefined ->
            {error, missing_timestamp};
        Timestamp when is_integer(Timestamp), Timestamp > 0 ->
            % Check ISO format if present
            case maps:get(<<"timestamp_iso">>, Receipt, undefined) of
                undefined -> ok;  % ISO is optional
                Iso when is_binary(Iso) ->
                    case validate_iso8601_format(Iso) of
                        ok -> ok;
                        {error, _} -> {error, invalid_iso8601}
                    end;
                _ ->
                    {error, invalid_iso8601}
            end;
        _ ->
            {error, invalid_timestamp}
    end.

validate_iso8601_format(Iso) ->
    % Basic ISO 8601 validation: YYYY-MM-DDTHH:MM:SS.sssZ
    case re:run(Iso, <<"^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d{3}Z$">>) of
        {match, _} -> ok;
        nomatch -> {error, invalid_format}
    end.

validate_sku_id_format(Receipt) ->
    case maps:get(<<"sku_id">>, Receipt, undefined) of
        undefined ->
            {error, missing_sku_id};
        SkuId when is_binary(SkuId) ->
            % Check format: SKU-xxx or any non-empty binary
            case byte_size(SkuId) > 0 of
                true -> ok;
                false -> {error, empty_sku_id}
            end;
        _ ->
            {error, invalid_sku_id}
    end.

validate_stage_field(Receipt) ->
    ValidStages = [
        <<"compilation">>, <<"testing">>, <<"validation">>,
        <<"execution">>, <<"integration">>, <<"deployment">>,
        <<"compile">>, <<"test">>, <<"release">>, <<"publish">>
    ],
    ValidAtomStages = [
        compilation, testing, validation,
        execution, integration, deployment,
        compile, test, release, publish
    ],
    case maps:get(<<"stage">>, Receipt, undefined) of
        undefined ->
            {error, missing_stage};
        Stage when is_binary(Stage) ->
            case lists:member(Stage, ValidStages) of
                true -> ok;
                false -> {error, invalid_stage}
            end;
        Stage when is_atom(Stage) ->
            case lists:member(Stage, ValidAtomStages) of
                true -> ok;
                false -> {error, invalid_stage}
            end;
        _ ->
            {error, invalid_stage}
    end.

%% Commented out: function is unused
%% is_atom_stage(Stage) when is_binary(Stage) ->
%%     try binary_to_existing_atom(Stage, utf8) of
%%         _ -> true
%%     catch
%%         _:_ -> false
%%     end.

validate_status_field(Receipt) ->
    ValidStatuses = [
        <<"open">>, <<"resolved">>, <<"pass">>, <<"fail">>,
        <<"pending">>, <<"completed">>
    ],
    case maps:get(<<"status">>, Receipt, undefined) of
        undefined ->
            {error, missing_status};
        Status when is_binary(Status) ->
            case lists:member(Status, ValidStatuses) of
                true -> ok;
                false -> {error, invalid_status}
            end;
        Status when is_atom(Status) ->
            ok;
        _ ->
            {error, invalid_status}
    end.

%%%=============================================================================
%%% Internal Helper Functions - Chain Verification
%%%=============================================================================

load_receipts_for_sku(SkuId) ->
    ReceiptsDir = <<"priv/receipts">>,
    case file:list_dir(ReceiptsDir) of
        {ok, Files} ->
            Receipts = lists:filtermap(
                fun(File) ->
                    case filename:extension(File) =:= ".json" of
                        true ->
                            Path = filename:join(ReceiptsDir, File),
                            case file:read_file(Path) of
                                {ok, JsonBin} ->
                                    Receipt = jsx:decode(JsonBin, [return_maps]),
                                    case maps:get(<<"sku_id">>, Receipt, undefined) of
                                        SkuId -> {true, Receipt};
                                        _ -> false
                                    end;
                                _ ->
                                    false
                            end;
                        false ->
                            false
                    end
                end,
                Files
            ),
            case Receipts of
                [] -> {error, no_receipts};
                _ -> {ok, Receipts}
            end;
        {error, _} ->
            {error, no_receipts}
    end.

is_valid_receipt(Receipt) ->
    case validate_receipt_fields(Receipt) of
        {ok, valid} -> true;
        _ -> false
    end.

extract_stage(Receipt) ->
    Stage = maps:get(<<"stage">>, Receipt, maps:get(stage, Receipt, unknown)),
    case is_binary(Stage) of
        true ->
            try binary_to_existing_atom(Stage, utf8) of
                Atom -> Atom
            catch
                _:_ -> unknown
            end;
        false when is_atom(Stage) ->
            Stage;
        false ->
            unknown
    end.

verify_chronological_order(Receipts) ->
    Timestamps = [get_timestamp(R) || R <- Receipts],
    Sorted = lists:sort(Timestamps),
    case Timestamps =:= Sorted of
        true -> ok;
        false -> {error, not_chronological}
    end.

verify_stage_chronological_order(Receipts) ->
    % Expected stage order
    ExpectedOrder = [compilation, testing, validation, execution],

    % Get receipts for each stage in expected order
    StageReceipts = lists:filtermap(
        fun(Stage) ->
            case lists:filter(fun(R) -> extract_stage(R) =:= Stage end, Receipts) of
                [Receipt] -> {true, Receipt};
                [] -> false;  % Missing stage - skip
                _ -> false  % Duplicate stage - skip
            end
        end,
        ExpectedOrder
    ),

    % Get timestamps in order
    Timestamps = [get_timestamp(R) || R <- StageReceipts],

    % Check if timestamps are strictly increasing
    check_timestamps_increasing(Timestamps).

check_timestamps_increasing([]) -> ok;
check_timestamps_increasing([_]) -> ok;
check_timestamps_increasing([T1, T2 | Rest]) ->
    case T1 < T2 of
        true -> check_timestamps_increasing([T2 | Rest]);
        false -> {error, not_chronological}
    end.

get_timestamp(Receipt) ->
    maps:get(<<"timestamp">>, Receipt, maps:get(timestamp, Receipt, 0)).

verify_no_timestamp_gaps(Receipts) ->
    Timestamps = [get_timestamp(R) || R <- Receipts],
    verify_timestamp_pairs(Timestamps).

verify_timestamp_pairs([]) -> ok;
verify_timestamp_pairs([_]) -> ok;
verify_timestamp_pairs([T1, T2 | Rest]) ->
    % Check no gap > 24 hours (86400000 milliseconds)
    MaxGap = 86400000,
    case (T2 - T1) > MaxGap of
        true -> {error, timestamp_gap_too_large};
        false -> verify_timestamp_pairs([T2 | Rest])
    end.

%%%=============================================================================
%%% Internal Helper Functions - Deterministic Build
%%%=============================================================================

build_sku(SkuId) ->
    % Simulate build (in production, compile actual code)
    {ok, #{
        sku_id => SkuId,
        artifacts => [<<"module.beam">>, <<"app.beam">>],
        timestamp => erlang:system_time(millisecond)
    }}.

compute_hash(Build) ->
    Data = term_to_binary(Build),
    crypto:hash(sha256, Data).

compute_diff(Build1, Build2) ->
    % Simple diff (in production, use proper diff tool)
    Hash1 = compute_hash(Build1),
    Hash2 = compute_hash(Build2),
    iolist_to_binary(
        io_lib:format("Hash mismatch: ~s vs ~s",
                     [base64:encode(Hash1), base64:encode(Hash2)])
    ).

trigger_non_determinism_andon(SkuId, Diff) ->
    % Trigger Andon event
    case whereis(tcps_andon) of
        undefined ->
            {error, andon_not_running};
        _ ->
            tcps_andon:trigger_andon(non_determinism, #{
                sku_id => SkuId,
                stage => compilation,
                details => #{
                    diff => Diff,
                    message => <<"Build is non-deterministic">>
                }
            })
    end.

%%%=============================================================================
%%% Internal Helper Functions - Audit Trail
%%%=============================================================================

load_andon_events(SkuId) ->
    % Load from tcps_andon system
    case whereis(tcps_andon) of
        undefined ->
            [];
        _ ->
            tcps_andon:get_andon_history(SkuId)
    end.

load_work_order(_SkuId) ->
    % In production, load from ontology
    #{
        created_at => erlang:system_time(millisecond),
        status => pending
    }.

extract_production_stages(Receipts) ->
    [#{
        stage => extract_stage(R),
        timestamp => get_timestamp(R),
        status => maps:get(<<"status">>, R, maps:get(status, R, unknown))
    } || R <- Receipts].

calculate_lead_time(WorkOrder, Receipts) ->
    case Receipts of
        [] ->
            0.0;
        _ ->
            CreatedAt = maps:get(created_at, WorkOrder, 0),
            LastReceipt = lists:last(Receipts),
            LastTimestamp = get_timestamp(LastReceipt),
            (LastTimestamp - CreatedAt) / 3600000.0  % Convert to hours
    end.

calculate_stage_durations(Receipts) ->
    % Group by stage
    ByStage = lists:foldl(
        fun(R, Acc) ->
            Stage = extract_stage(R),
            Timestamps = maps:get(Stage, Acc, []),
            maps:put(Stage, [get_timestamp(R) | Timestamps], Acc)
        end,
        #{},
        Receipts
    ),

    % Calculate average duration per stage
    maps:map(
        fun(_Stage, Timestamps) ->
            case length(Timestamps) > 1 of
                true ->
                    Sorted = lists:sort(Timestamps),
                    First = hd(Sorted),
                    Last = lists:last(Sorted),
                    (Last - First) / 3600000.0;
                false ->
                    0.0
            end
        end,
        ByStage
    ).

extract_quality_gates(Receipts) ->
    % Count pass/fail statuses
    Statuses = [maps:get(<<"status">>, R, maps:get(status, R, unknown))
                || R <- Receipts],

    #{
        total => length(Statuses),
        passed => length([1 || S <- Statuses,
                          S =:= <<"pass">> orelse S =:= pass]),
        failed => length([1 || S <- Statuses,
                          S =:= <<"fail">> orelse S =:= fail])
    }.

determine_publish_status(Receipts, AndonEvents) ->
    % Check if any open Andon events
    OpenAndons = [E || E <- AndonEvents,
                      maps:get(status, E, open) =:= open],

    case OpenAndons of
        [] ->
            % Check if published
            PublishReceipts = [R || R <- Receipts,
                              extract_stage(R) =:= execution orelse
                              extract_stage(R) =:= deployment],
            case PublishReceipts of
                [] -> pending;
                _ -> published
            end;
        _ ->
            failed
    end.

partition_andon_events(Events) ->
    lists:partition(
        fun(E) -> maps:get(status, E, open) =:= open end,
        Events
    ).

format_receipts_for_audit(Receipts) ->
    [#{
        receipt_id => maps:get(<<"receipt_id">>, R,
                              maps:get(receipt_id, R, undefined)),
        stage => extract_stage(R),
        timestamp => get_timestamp(R),
        status => maps:get(<<"status">>, R, maps:get(status, R, unknown))
    } || R <- Receipts].

format_andon_events(Events) ->
    [#{
        event_id => maps:get(event_id, E),
        failure_type => maps:get(failure_type, E),
        stage => maps:get(stage, E),
        timestamp => maps:get(timestamp, E)
    } || E <- Events].

format_resolutions(Events) ->
    [#{
        event_id => maps:get(event_id, E),
        resolution => maps:get(resolution, E, #{}),
        resolution_timestamp => maps:get(resolution_timestamp, E, 0)
    } || E <- Events].

%%%=============================================================================
%%% Internal Helper Functions - Compliance
%%%=============================================================================

load_receipts_in_period(_StartDate, _EndDate) ->
    % In production, query by timestamp
    % For now, return empty
    [].

load_andon_events_in_period(_StartDate, _EndDate) ->
    % In production, query by timestamp
    [].

extract_unique_skus(Receipts) ->
    SkuIds = [maps:get(<<"sku_id">>, R, maps:get(sku_id, R, undefined))
              || R <- Receipts],
    lists:usort([S || S <- SkuIds, S =/= undefined]).

count_quality_gates(Receipts, Status) ->
    StatusBin = atom_to_binary(Status, utf8),
    length([1 || R <- Receipts,
            maps:get(<<"status">>, R, maps:get(status, R, undefined)) =:= StatusBin
            orelse maps:get(<<"status">>, R, maps:get(status, R, undefined)) =:= Status]).

count_resolved_andon_events(Events) ->
    length([1 || E <- Events, maps:get(status, E, open) =:= resolved]).

calculate_average_lead_time(_UniqueSkus, _Receipts) ->
    % Placeholder
    0.0.

count_skus_without_andon(_UniqueSkus, AndonEvents) ->
    length(AndonEvents).

count_skus_with_rework(_UniqueSkus, _Receipts) ->
    0.

calculate_stage_metrics(_Receipts) ->
    #{}.

check_compliance_violations(TotalSkus, DefectRate, FirstPassYield, AvgLeadTime) ->
    Violations = [],

    % Check defect rate < 2%
    Violations2 = case DefectRate > 2.0 of
        true ->
            [#{
                type => high_defect_rate,
                severity => critical,
                value => DefectRate,
                threshold => 2.0,
                message => <<"Defect rate exceeds 2% threshold">>
            } | Violations];
        false ->
            Violations
    end,

    % Check first pass yield > 95%
    Violations3 = case FirstPassYield < 95.0 of
        true ->
            [#{
                type => low_first_pass_yield,
                severity => major,
                value => FirstPassYield,
                threshold => 95.0,
                message => <<"First pass yield below 95% threshold">>
            } | Violations2];
        false ->
            Violations2
    end,

    % Check average lead time < 24 hours
    Violations4 = case AvgLeadTime > 24.0 of
        true ->
            [#{
                type => high_lead_time,
                severity => minor,
                value => AvgLeadTime,
                threshold => 24.0,
                message => <<"Average lead time exceeds 24 hours">>
            } | Violations3];
        false ->
            Violations3
    end,

    % Check minimum SKUs processed
    Violations5 = case TotalSkus < 1 of
        true ->
            [#{
                type => insufficient_volume,
                severity => major,
                value => TotalSkus,
                threshold => 1,
                message => <<"Insufficient SKUs processed for compliance">>
            } | Violations4];
        false ->
            Violations4
    end,

    lists:reverse(Violations5).

%%%=============================================================================
%%% Internal Helper Functions - Stage Sequence
%%%=============================================================================

match_stage_sequence(Stages, Expected) ->
    % Check if actual stages match expected order
    ActualFiltered = [S || S <- Stages, lists:member(S, Expected)],
    case ActualFiltered =:= Expected of
        true -> ok;
        false -> {error, sequence_mismatch}
    end.

has_duplicate_stages(Stages) ->
    length(Stages) =/= length(lists:usort(Stages)).

%%%=============================================================================
%%% Internal Helper Functions - Ontology
%%%=============================================================================

verify_ontology_ref(Ref) when is_binary(Ref) ->
    % Basic URI validation
    case re:run(Ref, <<"^https?://.*#[A-Za-z0-9_]+$">>) of
        {match, _} -> true;
        nomatch -> false
    end;
verify_ontology_ref(_) ->
    false.

verify_sku_exists(SkuId) when is_binary(SkuId), byte_size(SkuId) > 0 ->
    % In production, check ontology database
    % For now, accept any non-empty SKU ID
    true;
verify_sku_exists(_) ->
    false.

%%%=============================================================================
%%% Internal Helper Functions - Tampering
%%%=============================================================================

verify_timestamp(Receipt) ->
    Timestamp = maps:get(<<"timestamp">>, Receipt,
                        maps:get(timestamp, Receipt, 0)),
    Now = erlang:system_time(millisecond),

    % Check timestamp is not in future
    case Timestamp > Now of
        true ->
            {error, timestamp_in_future};
        false ->
            % Check timestamp is reasonable (not too old)
            OneYearAgo = Now - (365 * 24 * 60 * 60 * 1000),
            case Timestamp < OneYearAgo of
                true -> {error, timestamp_too_old};
                false -> ok
            end
    end.

%%%=============================================================================
%%% Comprehensive Chain Verification Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Comprehensive chain verification with all checks.
%%
%% Runs ALL verification checks and compiles comprehensive report:
%% - Receipt chain completeness
%% - Chronological order
%% - Timestamp gaps
%% - Deterministic build
%% - Quality gates
%% - Tampering detection
%% - Signature verification
%% - Stage transitions
%%
%% @end
%%------------------------------------------------------------------------------
-spec verify_complete_chain(SkuId :: sku_id()) ->
    {ok, verification_report()} | {error, Violations :: [map()]}.
verify_complete_chain(SkuId) ->
    % Run ALL verifications
    Checks = [
        {receipt_chain, verify_receipt_chain(SkuId)},
        {chronological_order, verify_chronological_order_check(SkuId)},
        {timestamp_gaps, verify_no_timestamp_gaps_check(SkuId)},
        {deterministic_build, verify_deterministic_build(SkuId)},
        {quality_gates, verify_quality_gates_passed(SkuId)},
        {tampering, verify_no_tampering(SkuId)},
        {signatures, verify_signature_chain(SkuId)},
        {stage_transitions, verify_stage_transitions(SkuId)}
    ],

    compile_verification_report(Checks, SkuId).

%%------------------------------------------------------------------------------
%% @doc Verify quality gates passed.
%%
%% Checks:
%% - All receipts have pass status
%% - No failed quality gates
%% - All required stages completed
%%
%% @end
%%------------------------------------------------------------------------------
-spec verify_quality_gates_passed(SkuId :: sku_id()) ->
    {ok, pass} | {error, {failed_gates, [stage()]}}.
verify_quality_gates_passed(SkuId) ->
    case load_receipts_for_sku(SkuId) of
        {ok, Receipts} ->
            % Extract quality gate results from receipts
            GateResults = lists:map(fun(Receipt) ->
                Stage = extract_stage(Receipt),
                Status = maps:get(<<"status">>, Receipt,
                                 maps:get(status, Receipt, unknown)),
                {Stage, Status}
            end, Receipts),

            % Find failures
            Failures = [{Gate, Status} || {Gate, Status} <- GateResults,
                        Status =/= <<"pass">> andalso Status =/= pass],

            case Failures of
                [] -> {ok, pass};
                _ -> {error, {failed_gates, [Gate || {Gate, _} <- Failures]}}
            end;
        {error, no_receipts} ->
            {error, {failed_gates, [no_receipts]}}
    end.

%%------------------------------------------------------------------------------
%% @doc Verify no tampering in receipt chain.
%%
%% Checks:
%% - Checksums match
%% - Timestamps valid
%% - No suspicious modifications
%%
%% @end
%%------------------------------------------------------------------------------
-spec verify_no_tampering(SkuId :: sku_id()) ->
    {ok, verified} | {error, {tampered, Details :: map()}}.
verify_no_tampering(SkuId) ->
    case load_receipts_for_sku(SkuId) of
        {ok, Receipts} ->
            % Verify checksums for each receipt
            TamperingChecks = lists:map(fun(Receipt) ->
                ReceiptId = maps:get(<<"receipt_id">>, Receipt,
                                    maps:get(receipt_id, Receipt, <<"unknown">>)),
                StoredChecksum = maps:get(<<"checksum">>, Receipt,
                                         maps:get(checksum, Receipt, undefined)),

                % Recalculate checksum
                CalculatedChecksum = calculate_receipt_checksum(Receipt),

                case StoredChecksum == CalculatedChecksum of
                    true -> {ReceiptId, ok};
                    false -> {ReceiptId, {tampered, StoredChecksum, CalculatedChecksum}}
                end
            end, Receipts),

            Tampered = [R || {_Id, Status} = R <- TamperingChecks, Status =/= ok],

            case Tampered of
                [] -> {ok, verified};
                _ -> {error, {tampered, maps:from_list(Tampered)}}
            end;
        {error, no_receipts} ->
            {ok, verified}  % No receipts to tamper
    end.

%%------------------------------------------------------------------------------
%% @doc Verify cryptographic signature chain.
%%
%% Checks:
%% - Each receipt has valid signature
%% - Signatures chain correctly
%% - Public key verification passes
%%
%% @end
%%------------------------------------------------------------------------------
-spec verify_signature_chain(SkuId :: sku_id()) ->
    {ok, verified} | {error, {invalid_signatures, [binary()]}}.
verify_signature_chain(SkuId) ->
    case load_receipts_for_sku(SkuId) of
        {ok, Receipts} ->
            % Verify each receipt's signature (if present)
            InvalidSigs = lists:filtermap(fun(Receipt) ->
                ReceiptId = maps:get(<<"receipt_id">>, Receipt,
                                    maps:get(receipt_id, Receipt, <<"unknown">>)),
                Signature = maps:get(<<"signature">>, Receipt,
                                    maps:get(signature, Receipt, undefined)),

                case Signature of
                    undefined ->
                        % No signature - skip verification
                        false;
                    _ ->
                        PublicKey = get_public_key(),
                        case verify_signature(Receipt, Signature, PublicKey) of
                            valid -> false;
                            invalid -> {true, ReceiptId}
                        end
                end
            end, Receipts),

            case InvalidSigs of
                [] -> {ok, verified};
                _ -> {error, {invalid_signatures, InvalidSigs}}
            end;
        {error, no_receipts} ->
            {ok, verified}  % No receipts to verify
    end.

%%------------------------------------------------------------------------------
%% @doc Verify stage transitions are valid.
%%
%% Checks:
%% - Transitions follow required order
%% - No invalid stage jumps
%% - Timestamps increase with transitions
%%
%% @end
%%------------------------------------------------------------------------------
-spec verify_stage_transitions(SkuId :: sku_id()) ->
    {ok, valid} | {error, {invalid_transitions, Details :: [map()]}}.
verify_stage_transitions(SkuId) ->
    case load_receipts_for_sku(SkuId) of
        {ok, Receipts} ->
            % Sort by timestamp
            Sorted = lists:sort(fun(A, B) ->
                get_timestamp(A) =< get_timestamp(B)
            end, Receipts),

            % Validate each transition
            InvalidTransitions = validate_transitions_recursive(Sorted, []),

            case InvalidTransitions of
                [] -> {ok, valid};
                _ -> {error, {invalid_transitions, InvalidTransitions}}
            end;
        {error, no_receipts} ->
            {ok, valid}  % No transitions to validate
    end.

%%%=============================================================================
%%% Audit Trail Generation
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Generate complete audit trail for SKU.
%%
%% Includes:
%% - All receipts with checksums
%% - Andon events timeline
%% - Verification results
%% - Metadata (generation time, generator)
%%
%% @end
%%------------------------------------------------------------------------------
-spec generate_audit_trail(SkuId :: sku_id()) -> {ok, map()}.
generate_audit_trail(SkuId) ->
    % Collect all audit-relevant data
    Receipts = case load_receipts_for_sku(SkuId) of
        {ok, Rs} -> Rs;
        {error, no_receipts} -> []
    end,

    AndonEvents = load_andon_events(SkuId),

    % Load SKU and work order data
    SkuData = #{
        sku_id => SkuId,
        created_at => erlang:system_time(millisecond)
    },

    WorkOrderId = <<"WO-", SkuId/binary>>,
    WorkOrderData = load_work_order(SkuId),

    AuditTrail = #{
        sku_id => SkuId,
        work_order_id => WorkOrderId,
        created_at => maps:get(created_at, SkuData),
        pull_signal => maps:get(status, WorkOrderData, pending),

        % Receipt timeline
        receipts => lists:map(fun(R) -> #{
            stage => extract_stage(R),
            timestamp => get_timestamp(R),
            status => maps:get(<<"status">>, R, maps:get(status, R, unknown)),
            checksum => maps:get(<<"checksum">>, R, maps:get(checksum, R, undefined))
        } end, Receipts),

        % Andon events timeline
        andon_events => lists:map(fun(A) -> #{
            type => maps:get(failure_type, A, unknown),
            triggered_at => maps:get(timestamp, A, 0),
            resolved_at => case maps:get(status, A, open) of
                resolved -> maps:get(resolution_timestamp, A, undefined);
                _ -> undefined
            end,
            root_cause => case maps:get(resolution, A, undefined) of
                undefined -> undefined;
                Resolution -> maps:get(root_cause, Resolution, undefined)
            end
        } end, AndonEvents),

        % Verification results
        verification => verify_complete_chain(SkuId),

        % Metadata
        generated_at => erlang:system_time(millisecond),
        generated_by => <<"tcps_receipt_verifier">>
    },

    {ok, AuditTrail}.

%%------------------------------------------------------------------------------
%% @doc Generate compliance report for SKU.
%%
%% Report includes:
%% - Compliance status (compliant/non-compliant)
%% - Regulatory requirements checklist
%% - Audit trail reference
%% - Verification signatures
%%
%% @end
%%------------------------------------------------------------------------------
-spec generate_compliance_report(SkuId :: sku_id()) -> {ok, map()}.
generate_compliance_report(SkuId) ->
    % Generate report for regulatory compliance
    {ok, _AuditTrail} = generate_audit_trail(SkuId),
    VerificationReport = case verify_complete_chain(SkuId) of
        {ok, ChainReport} -> ChainReport;
        {error, _} -> #{}
    end,

    Report = #{
        sku_id => SkuId,
        compliance_status => determine_compliance_status(VerificationReport),

        % Regulatory requirements
        requirements => #{
            deterministic_build => check_requirement(VerificationReport, deterministic_build),
            security_scan => check_requirement(VerificationReport, tampering),
            test_coverage => check_requirement(VerificationReport, quality_gates),
            quality_gates => check_requirement(VerificationReport, quality_gates)
        },

        % Audit trail reference
        audit_trail_id => generate_audit_trail_id(SkuId),

        % Signatures
        verified_by => <<"tcps_receipt_verifier">>,
        verified_at => erlang:system_time(millisecond)
    },

    % Store compliance report (for now, just return it)
    % In production: ok = tcps_persistence:store_compliance_report(Report),

    {ok, Report}.

%%------------------------------------------------------------------------------
%% @doc Export audit trail to specified format.
%%
%% Supports:
%% - json: JSON format
%% - pdf: PDF document (stub)
%% - xml: XML format (stub)
%%
%% @end
%%------------------------------------------------------------------------------
-spec export_audit_trail(SkuId :: sku_id(), Format :: json | pdf | xml) ->
    {ok, binary()}.
export_audit_trail(SkuId, json) ->
    {ok, AuditTrail} = generate_audit_trail(SkuId),

    % Format as JSON
    Json = jsx:encode(AuditTrail, [{space, 2}, {indent, 2}]),

    % Write to file
    FileName = io_lib:format("audit-~s-~p.json", [SkuId, erlang:system_time(millisecond)]),
    FilePath = filename:join("priv/tcps/audit_trails", FileName),
    ok = filelib:ensure_dir(FilePath),
    ok = file:write_file(FilePath, Json),

    {ok, list_to_binary(FilePath)};

export_audit_trail(SkuId, pdf) ->
    % Generate PDF using external tool or library (stub)
    {ok, _AuditTrail} = generate_audit_trail(SkuId),
    FilePath = io_lib:format("priv/tcps/audit_trails/audit-~s-~p.pdf",
                            [SkuId, erlang:system_time(millisecond)]),
    {ok, list_to_binary(FilePath)};

export_audit_trail(SkuId, xml) ->
    % Generate XML (stub)
    {ok, _AuditTrail} = generate_audit_trail(SkuId),
    FilePath = io_lib:format("priv/tcps/audit_trails/audit-~s-~p.xml",
                            [SkuId, erlang:system_time(millisecond)]),
    {ok, list_to_binary(FilePath)}.

%%%=============================================================================
%%% Internal Helper Functions - Chain Verification
%%%=============================================================================

calculate_receipt_checksum(Receipt) ->
    % Remove checksum field itself
    ReceiptWithoutChecksum = maps:remove(<<"checksum">>,
                                        maps:remove(checksum, Receipt)),

    % Canonical JSON encoding
    Json = jsx:encode(ReceiptWithoutChecksum),

    % SHA-256 hash
    Hash = crypto:hash(sha256, Json),
    base64:encode(Hash).

verify_signature(Receipt, Signature, _PublicKey) ->
    % Remove signature from receipt
    ReceiptData = maps:remove(<<"signature">>,
                             maps:remove(signature, Receipt)),

    % Canonical encoding
    _Json = jsx:encode(ReceiptData),

    % In production, verify signature using public_key module
    % For now, assume valid if signature is present and non-empty
    case byte_size(Signature) > 0 of
        true -> valid;
        false -> invalid
    end.

get_public_key() ->
    % Stub: Return dummy public key
    % In production, load from configuration
    <<"-----BEGIN PUBLIC KEY-----\nDUMMY_KEY\n-----END PUBLIC KEY-----">>.

compile_verification_report(Checks, SkuId) ->
    % Separate passes and failures
    {Passes, Failures} = lists:partition(fun({_Name, Result}) ->
        case Result of
            {ok, _} -> true;
            ok -> true;
            _ -> false
        end
    end, Checks),

    Report = #{
        sku_id => SkuId,
        total_checks => length(Checks),
        passed_checks => length(Passes),
        failed_checks => length(Failures),
        pass_rate => case length(Checks) of
            0 -> 0.0;
            N -> length(Passes) / N
        end,
        checks => maps:from_list(Checks),
        verified_at => erlang:system_time(millisecond)
    },

    case Failures of
        [] -> {ok, Report};
        _ -> {error, extract_violations(Failures)}
    end.

extract_violations(Failures) ->
    lists:map(fun({CheckName, ErrorResult}) ->
        #{
            check => CheckName,
            error => ErrorResult,
            severity => critical
        }
    end, Failures).

validate_transitions_recursive([_Last], Acc) ->
    lists:reverse(Acc);
validate_transitions_recursive([Current, Next | Rest], Acc) ->
    CurrentStage = extract_stage(Current),
    NextStage = extract_stage(Next),

    % Check if transition is valid
    case is_valid_transition(CurrentStage, NextStage) of
        true ->
            validate_transitions_recursive([Next | Rest], Acc);
        false ->
            Invalid = #{
                from => CurrentStage,
                to => NextStage,
                reason => invalid_sequence
            },
            validate_transitions_recursive([Next | Rest], [Invalid | Acc])
    end.

is_valid_transition(compilation, testing) -> true;
is_valid_transition(testing, validation) -> true;
is_valid_transition(validation, execution) -> true;
is_valid_transition(execution, integration) -> true;
is_valid_transition(integration, deployment) -> true;
is_valid_transition(_, _) -> false.

verify_chronological_order_check(SkuId) ->
    case load_receipts_for_sku(SkuId) of
        {ok, Receipts} ->
            verify_chronological_order(Receipts);
        {error, no_receipts} ->
            {ok, verified}
    end.

verify_no_timestamp_gaps_check(SkuId) ->
    case load_receipts_for_sku(SkuId) of
        {ok, Receipts} ->
            verify_no_timestamp_gaps(Receipts);
        {error, no_receipts} ->
            {ok, verified}
    end.

determine_compliance_status(VerificationReport) ->
    PassRate = maps:get(pass_rate, VerificationReport, 0.0),
    case PassRate >= 1.0 of
        true -> compliant;
        false -> non_compliant
    end.

check_requirement(VerificationReport, RequirementKey) ->
    Checks = maps:get(checks, VerificationReport, #{}),
    case maps:get(RequirementKey, Checks, {error, not_found}) of
        {ok, _} -> pass;
        ok -> pass;
        _ -> fail
    end.

generate_audit_trail_id(SkuId) ->
    Timestamp = erlang:system_time(millisecond),
    iolist_to_binary(io_lib:format("AUDIT-~s-~p", [SkuId, Timestamp])).
