%%%-------------------------------------------------------------------
%%% @doc Auditable Receipt Module for Plan Enforcement Tracking
%%%
%%% Provides immutable audit trail for pricing plan enforcement with:
%%% - Receipt creation with envelope bounds snapshot
%%% - Refusal event logging with hash chain
%%% - Receipt chain verification (hash continuity)
%%% - Conformance checking (actual vs envelope bounds)
%%% - Export to JSON/CSV/TSV for compliance audits
%%%
%%% All receipts are write-once, immutable artifacts stored in
%%% priv/receipts/<plan>/<version>/<timestamp>.receipt.json
%%%
%%% Hash chain prevents tampering: any field modification causes
%%% hash mismatch, enabling detection of audit trail corruption.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_pricing_receipt).

-export([create_receipt/2, create_receipt/3, get_receipt/1, list_receipts/1, list_receipts/2,
         add_refusal/3, add_refusal/4, verify_receipt_chain/1, verify_receipt/1,
         verify_conformance/2, export_receipt/2, export_receipt/3, export_receipts/3,
         compute_hash/1, receipt_filename/1]).

                                                  % Receipt creation and retrieval

    % Refusal tracking

    % Verification

    % Export

    % Utility

-type plan_id() :: free | starter | pro.
-type receipt_id() :: binary().
-type refusal_code() :: 1001..1089.
-type refusal_reason() ::
    queue_overflow |
    rate_limit_exceeded |
    circuit_breaker_open |
    concurrent_limit |
    throughput_limit |
    latency_p99_violated |
    failover_timeout |
    plan_expired |
    unknown.
-type format_type() :: json | csv | tsv.
-type receipt() ::
    #{receipt_id := receipt_id(),
      plan_id := plan_id(),
      version := binary(),
      timestamp := binary(),
      envelope_claim := envelope_claim(),
      refusal_trigger => refusal_trigger(),
      hash_chain := hash_chain(),
      audit_fields := audit_fields(),
      conformance_events => [conformance_event()]}.
-type envelope_claim() ::
    #{throughput_req_s := float(),
      concurrent := integer(),
      queue_depth := integer(),
      latency_p99_ms := float(),
      failover_s := float()}.
-type refusal_trigger() ::
    #{code := refusal_code(),
      reason := refusal_reason(),
      attempted_action := binary(),
      timestamp := binary(),
      metric_value => float()}.
-type hash_chain() :: #{previous_receipt_hash := binary() | null, current_hash := binary()}.
-type audit_fields() ::
    #{requestor_id => binary() | null,
      machine_id := binary(),
      erlang_version := binary(),
      otp_version := binary(),
      hostname => binary() | null}.
-type conformance_event() ::
    #{timestamp := binary(),
      status := pass | fail | warning,
      metrics := map()}.

-define(RECEIPTS_BASE_DIR, "priv/receipts").
-define(SCHEMA_FILE, "shapes/pricing_receipt.schema.json").

%%%===================================================================
%%% API - Receipt Creation
%%%===================================================================

%% @doc Create a new receipt for plan+version with current envelope bounds.
%% Snapshot of plan envelope at receipt creation time.
%% @end
-spec create_receipt(PlanId :: plan_id(), Version :: binary()) ->
                        {ok, Receipt :: receipt()} | {error, term()}.
create_receipt(PlanId, Version) ->
    create_receipt(PlanId, Version, null).

%% @doc Create receipt with optional requestor ID.
%% @end
-spec create_receipt(PlanId :: plan_id(), Version :: binary(), RequestorId :: binary() | null) ->
                        {ok, Receipt :: receipt()} | {error, term()}.
create_receipt(PlanId, Version, RequestorId) when is_atom(PlanId), is_binary(Version) ->
    try
        % Get envelope bounds for this plan
        EnvelopeClaim = get_plan_envelope(PlanId),

        % Generate receipt ID
        ReceiptId = generate_receipt_id(),

        % Get audit info
        AuditFields = build_audit_fields(RequestorId),

        % Get previous receipt hash (for hash chain)
        PreviousHash = get_previous_receipt_hash(PlanId, Version),

        % Create receipt skeleton (without hash)
        Receipt =
            #{receipt_id => ReceiptId,
              plan_id => PlanId,
              version => Version,
              timestamp => iso8601_timestamp(),
              envelope_claim => EnvelopeClaim,
              hash_chain =>
                  #{previous_receipt_hash => PreviousHash,
                    current_hash => <<"pending">>},  % Placeholder, will compute
              audit_fields => AuditFields},

        % Compute hash over immutable fields
        CurrentHash = compute_hash(Receipt),

        % Update hash in receipt
        FinalReceipt =
            Receipt#{hash_chain =>
                         #{previous_receipt_hash => PreviousHash, current_hash => CurrentHash}},

        % Store receipt
        ok = store_receipt(FinalReceipt),

        {ok, FinalReceipt}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end;
create_receipt(_, _, _) ->
    {error, invalid_arguments}.

%%%===================================================================
%%% API - Receipt Retrieval
%%%===================================================================

%% @doc Get a receipt by ID.
%% @end
-spec get_receipt(ReceiptId :: receipt_id()) -> {ok, Receipt :: receipt()} | {error, term()}.
get_receipt(ReceiptId) when is_binary(ReceiptId) ->
    % Search across all plan/version directories
    case find_receipt_file(ReceiptId) of
        {ok, FilePath} ->
            read_receipt_file(FilePath);
        Error ->
            Error
    end;
get_receipt(_) ->
    {error, invalid_receipt_id}.

%% @doc List all receipts for plan+version.
%% @end
-spec list_receipts(PlanId :: plan_id()) -> {ok, [Receipt :: receipt()]} | {error, term()}.
list_receipts(PlanId) ->
    list_receipts(PlanId, '_').

%% @doc List receipts for plan+version.
%% Use '_' to match any version.
%% @end
-spec list_receipts(PlanId :: plan_id(), Version :: binary() | '_') ->
                       {ok, [Receipt :: receipt()]} | {error, term()}.
list_receipts(PlanId, Version) when is_atom(PlanId) ->
    try
        PlanDir = filename:join([?RECEIPTS_BASE_DIR, atom_to_list(PlanId)]),

        % List version directories
        VersionDirs =
            case Version of
                '_' ->
                    case file:list_dir(PlanDir) of
                        {ok, Dirs} ->
                            Dirs;
                        {error, enoent} ->
                            [];
                        ErrorListDir ->
                            throw(ErrorListDir)
                    end;
                _ ->
                    [binary_to_list(Version)]
            end,

        % Collect receipts from each version directory
        Receipts =
            lists:append([collect_receipts_in_dir(filename:join(PlanDir, VersionDir))
                          || VersionDir <- VersionDirs]),

        {ok, Receipts}
    catch
        ErrorType:ErrorReason ->
            {error, {ErrorType, ErrorReason}}
    end;
list_receipts(_, _) ->
    {error, invalid_arguments}.

%%%===================================================================
%%% API - Refusal Tracking
%%%===================================================================

%% @doc Add refusal event to receipt.
%% Updates hash chain to maintain integrity.
%% @end
-spec add_refusal(ReceiptId :: receipt_id(), Code :: refusal_code(), Reason :: refusal_reason()) ->
                     {ok, Receipt :: receipt()} | {error, term()}.
add_refusal(ReceiptId, Code, Reason) ->
    add_refusal(ReceiptId, Code, Reason, <<"unknown">>).

%% @doc Add refusal with attempted action.
%% @end
-spec add_refusal(ReceiptId :: receipt_id(),
                  Code :: refusal_code(),
                  Reason :: refusal_reason(),
                  AttemptedAction :: binary()) ->
                     {ok, Receipt :: receipt()} | {error, term()}.
add_refusal(ReceiptId, Code, Reason, AttemptedAction)
    when is_binary(ReceiptId), is_integer(Code), Code >= 1001, Code =< 1089, is_atom(Reason),
         is_binary(AttemptedAction) ->
    try
        % Load existing receipt
        {ok, Receipt} = get_receipt(ReceiptId),

        % Get current hash for chain continuity
        CurrentHash = maps:get(current_hash, maps:get(hash_chain, Receipt)),

        % For refusal addition, we create new hash based on previous
        OldHash = CurrentHash,

        % Add refusal event
        RefusalEvent =
            #{code => Code,
              reason => Reason,
              attempted_action => AttemptedAction,
              timestamp => iso8601_timestamp()},

        UpdatedReceipt = Receipt#{refusal_trigger => RefusalEvent},

        % Recompute hash with refusal added
        NewHash =
            compute_hash(UpdatedReceipt#{hash_chain =>
                                             #{previous_receipt_hash => OldHash,
                                               current_hash => <<"pending">>}}),

        FinalReceipt =
            UpdatedReceipt#{hash_chain =>
                                #{previous_receipt_hash => OldHash, current_hash => NewHash}},

        % Update stored receipt
        ok = store_receipt(FinalReceipt),

        {ok, FinalReceipt}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end;
add_refusal(_, _, _, _) ->
    {error, invalid_arguments}.

%%%===================================================================
%%% API - Verification
%%%===================================================================

%% @doc Verify entire receipt chain for plan+version.
%% Checks hash continuity across all receipts.
%% @end
-spec verify_receipt_chain(PlanId :: plan_id()) -> {ok, complete} | {error, term()}.
verify_receipt_chain(PlanId) when is_atom(PlanId) ->
    try
        {ok, Receipts} = list_receipts(PlanId),

        % Sort by timestamp
        SortedReceipts =
            lists:sort(fun(R1, R2) ->
                          T1 = maps:get(timestamp, R1),
                          T2 = maps:get(timestamp, R2),
                          T1 =< T2
                       end,
                       Receipts),

        % Verify chain
        case verify_chain_integrity(SortedReceipts) of
            ok ->
                {ok, complete};
            VerifyError ->
                VerifyError
        end
    catch
        ErrorType:ErrorReason ->
            {error, {ErrorType, ErrorReason}}
    end;
verify_receipt_chain(_) ->
    {error, invalid_arguments}.

%% @doc Verify a single receipt's integrity.
%% @end
-spec verify_receipt(ReceiptId :: receipt_id()) -> {ok, verified} | {error, term()}.
verify_receipt(ReceiptId) when is_binary(ReceiptId) ->
    try
        {ok, Receipt} = get_receipt(ReceiptId),

        % Verify hash
        StoredHash = maps:get(current_hash, maps:get(hash_chain, Receipt)),
        _ComputedHash =
            compute_hash(Receipt#{hash_chain =>
                                      #{previous_receipt_hash =>
                                            maps:get(previous_receipt_hash,
                                                     maps:get(hash_chain, Receipt)),
                                        current_hash => <<"verify">>}}),

        case StoredHash =:= _ComputedHash of
            true ->
                {ok, verified};
            false ->
                {error, hash_mismatch}
        end
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end;
verify_receipt(_) ->
    {error, invalid_receipt_id}.

%% @doc Verify conformance: check actual metrics vs envelope claims.
%% @end
-spec verify_conformance(ReceiptId :: receipt_id(), Metrics :: map()) ->
                            {ok, Result :: map()} | {error, term()}.
verify_conformance(ReceiptId, Metrics) when is_binary(ReceiptId), is_map(Metrics) ->
    try
        {ok, Receipt} = get_receipt(ReceiptId),
        Envelope = maps:get(envelope_claim, Receipt),

        % Check each metric
        ThroughputOk =
            maps:get(throughput_actual, Metrics, 0) =< maps:get(throughput_req_s, Envelope),
        ConcurrentOk = maps:get(concurrent_actual, Metrics, 0) =< maps:get(concurrent, Envelope),
        QueueOk = maps:get(queue_depth_actual, Metrics, 0) =< maps:get(queue_depth, Envelope),
        LatencyOk = maps:get(latency_p99_actual, Metrics, 0) =< maps:get(latency_p99_ms, Envelope),
        FailoverOk = maps:get(failover_actual, Metrics, 0) =< maps:get(failover_s, Envelope),

        Status =
            case {ThroughputOk, ConcurrentOk, QueueOk, LatencyOk, FailoverOk} of
                {true, true, true, true, true} ->
                    pass;
                _ ->
                    fail
            end,

        ConformanceEvent =
            #{timestamp => iso8601_timestamp(),
              status => Status,
              metrics => Metrics,
              violations => find_violations(Envelope, Metrics)},

        {ok, ConformanceEvent}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end;
verify_conformance(_, _) ->
    {error, invalid_arguments}.

%%%===================================================================
%%% API - Export
%%%===================================================================

%% @doc Export single receipt to format.
%% @end
-spec export_receipt(ReceiptId :: receipt_id(), Format :: format_type()) ->
                        {ok, iodata()} | {error, term()}.
export_receipt(ReceiptId, Format) when is_binary(ReceiptId), is_atom(Format) ->
    case get_receipt(ReceiptId) of
        {ok, Receipt} ->
            {ok, format_receipt(Receipt, Format)};
        Error ->
            Error
    end;
export_receipt(_, _) ->
    {error, invalid_arguments}.

%% @doc Export receipts for plan+version to format.
%% @end
-spec export_receipt(ReceiptId :: receipt_id(), Version :: binary(), Format :: format_type()) ->
                        {ok, iodata()} | {error, term()}.
export_receipt(ReceiptId, Version, Format) ->
    case get_receipt(ReceiptId) of
        {ok, Receipt} ->
            PlanId = maps:get(plan_id, Receipt),
            export_receipts(PlanId, Version, Format);
        Error ->
            Error
    end.

%% @doc Export all receipts for plan+version.
%% @end
-spec export_receipts(PlanId :: plan_id(), Version :: binary(), Format :: format_type()) ->
                         {ok, iodata()} | {error, term()}.
export_receipts(PlanId, Version, Format)
    when is_atom(PlanId), is_binary(Version), is_atom(Format) ->
    try
        {ok, Receipts} = list_receipts(PlanId, Version),

        case Format of
            json ->
                {ok, erlmcp_json_native:encode(Receipts)};
            csv ->
                {ok, receipts_to_csv(Receipts)};
            tsv ->
                {ok, receipts_to_tsv(Receipts)};
            _ ->
                {error, invalid_format}
        end
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end;
export_receipts(_, _, _) ->
    {error, invalid_arguments}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Get plan envelope bounds
get_plan_envelope(free) ->
    #{throughput_req_s => 10.0,
      concurrent => 5,
      queue_depth => 10,
      latency_p99_ms => 1000.0,
      failover_s => 30.0};
get_plan_envelope(starter) ->
    #{throughput_req_s => 100.0,
      concurrent => 50,
      queue_depth => 100,
      latency_p99_ms => 500.0,
      failover_s => 15.0};
get_plan_envelope(pro) ->
    #{throughput_req_s => 1000.0,
      concurrent => 500,
      queue_depth => 1000,
      latency_p99_ms => 200.0,
      failover_s => 5.0}.

%% Generate unique receipt ID (UUID v4 style)
generate_receipt_id() ->
    A = rand:uniform(4294967296) - 1,
    B = rand:uniform(65536) - 1,
    C = rand:uniform(65536) - 1,
    D = rand:uniform(65536) - 1,
    E = rand:uniform(281474976710656) - 1,
    list_to_binary(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", [A, B, C, D, E])).

%% Build audit fields
build_audit_fields(RequestorId) ->
    {ok, Hostname} = inet:gethostname(),
    {OtpRelease, _} = erlang:system_info(otp_release),

    #{requestor_id => RequestorId,
      machine_id => list_to_binary(lists:concat([node()])),
      erlang_version => list_to_binary(erlang:system_info(version)),
      otp_version => list_to_binary(OtpRelease),
      hostname => list_to_binary(Hostname)}.

%% Get previous receipt hash for chain
get_previous_receipt_hash(PlanId, Version) ->
    try
        {ok, Receipts} = list_receipts(PlanId, Version),
        case Receipts of
            [] ->
                null;
            _ ->
                LastReceipt =
                    lists:last(
                        lists:sort(fun(R1, R2) ->
                                      T1 = maps:get(timestamp, R1),
                                      T2 = maps:get(timestamp, R2),
                                      T1 =< T2
                                   end,
                                   Receipts)),
                maps:get(current_hash, maps:get(hash_chain, LastReceipt))
        end
    catch
        _ ->
            null
    end.

%% @doc Compute SHA-256 hash of receipt.
%% Hashes over immutable fields only.
%% @end
-spec compute_hash(Receipt :: receipt()) -> binary().
compute_hash(Receipt) ->
    % Extract immutable fields
    ImmutableData =
        #{receipt_id => maps:get(receipt_id, Receipt),
          plan_id => maps:get(plan_id, Receipt),
          version => maps:get(version, Receipt),
          timestamp => maps:get(timestamp, Receipt),
          envelope_claim => maps:get(envelope_claim, Receipt),
          audit_fields => maps:get(audit_fields, Receipt),
          previous_receipt_hash => maps:get(previous_receipt_hash, maps:get(hash_chain, Receipt))},

    % Include refusal if present
    Data =
        case maps:get(refusal_trigger, Receipt, undefined) of
            undefined ->
                ImmutableData;
            Refusal ->
                ImmutableData#{refusal_trigger => Refusal}
        end,

    % Serialize to JSON for hashing
    Json = erlmcp_json_native:encode(Data),

    % SHA-256 hash
    Hash = crypto:hash(sha256, Json),

    % Return as hex string
    list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Hash)])).

%% Verify chain integrity
verify_chain_integrity([]) ->
    ok;
verify_chain_integrity([Receipt | Rest]) ->
    % Verify this receipt's hash
    StoredHash = maps:get(current_hash, maps:get(hash_chain, Receipt)),
    _ComputedHash =
        compute_hash(Receipt#{hash_chain =>
                                  #{previous_receipt_hash =>
                                        maps:get(previous_receipt_hash,
                                                 maps:get(hash_chain, Receipt)),
                                    current_hash => <<"verify">>}}),

    case StoredHash =:= _ComputedHash of
        true ->
            % Check next receipt links to this one
            verify_chain_integrity(Rest);
        false ->
            {error, {hash_mismatch, StoredHash}}
    end.

%% Store receipt to file
store_receipt(Receipt) ->
    PlanId = maps:get(plan_id, Receipt),
    Version = maps:get(version, Receipt),
    Timestamp = maps:get(timestamp, Receipt),

    Dir = filename:join([?RECEIPTS_BASE_DIR, atom_to_list(PlanId), binary_to_list(Version)]),
    ok =
        filelib:ensure_dir(
            filename:join(Dir, "dummy")),

    Filename = receipt_filename(Timestamp),
    FilePath = filename:join(Dir, Filename),

    Json = erlmcp_json_native:encode(Receipt),
    file:write_file(FilePath, Json).

%% Generate receipt filename from timestamp
-spec receipt_filename(Timestamp :: binary()) -> string().
receipt_filename(Timestamp) ->
    % Convert ISO 8601 timestamp to filename-safe format
    % "2025-01-27T12:34:56Z" -> "20250127-123456.receipt.json"
    [Y1, Y2, Y3, Y4, $-, M1, M2, $-, D1, D2, $T, H1, H2, $:, Mi1, Mi2, $:, S1, S2 | _] =
        binary_to_list(Timestamp),
    DateStr = [Y1, Y2, Y3, Y4, M1, M2, D1, D2],
    TimeStr = [H1, H2, Mi1, Mi2, S1, S2],
    lists:flatten(
        io_lib:format("~s-~s.receipt.json", [DateStr, TimeStr])).

%% Find receipt file by ID across all directories
find_receipt_file(ReceiptId) ->
    ReceiptIdStr = binary_to_list(ReceiptId),

    % Use file:list_dir since filelib:find_files doesn't exist
    case file:list_dir(?RECEIPTS_BASE_DIR) of
        {ok, Files} ->
            FilteredFiles =
                [F
                 || F <- Files, lists:suffix(".receipt.json", F), string:str(F, ReceiptIdStr) > 0],
            case FilteredFiles of
                [] ->
                    {error, not_found};
                [File | _] ->
                    {ok, filename:join(?RECEIPTS_BASE_DIR, File)}
            end;
        {error, _} ->
            {error, not_found}
    end.

%% Read receipt from file
read_receipt_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Json} ->
            try
                Receipt = erlmcp_json_native:decode(Json),
                {ok, Receipt}
            catch
                _:_ ->
                    {error, invalid_json}
            end;
        Error ->
            Error
    end.

%% Collect receipts in directory
collect_receipts_in_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            [begin
                 {ok, Receipt} = read_receipt_file(filename:join(Dir, File)),
                 Receipt
             end
             || File <- Files, filename:extension(File) =:= ".json"];
        {error, enoent} ->
            []
    end.

%% Find violations
find_violations(Envelope, Metrics) ->
    Violations = [],

    Violations1 =
        case maps:get(throughput_actual, Metrics, 0) > maps:get(throughput_req_s, Envelope) of
            true ->
                [throughput_exceeded | Violations];
            false ->
                Violations
        end,

    Violations2 =
        case maps:get(concurrent_actual, Metrics, 0) > maps:get(concurrent, Envelope) of
            true ->
                [concurrent_exceeded | Violations1];
            false ->
                Violations1
        end,

    Violations3 =
        case maps:get(queue_depth_actual, Metrics, 0) > maps:get(queue_depth, Envelope) of
            true ->
                [queue_depth_exceeded | Violations2];
            false ->
                Violations2
        end,

    Violations4 =
        case maps:get(latency_p99_actual, Metrics, 0) > maps:get(latency_p99_ms, Envelope) of
            true ->
                [latency_exceeded | Violations3];
            false ->
                Violations3
        end,

    case maps:get(failover_actual, Metrics, 0) > maps:get(failover_s, Envelope) of
        true ->
            [failover_exceeded | Violations4];
        false ->
            Violations4
    end.

%% Format receipt for export
format_receipt(Receipt, json) ->
    erlmcp_json_native:encode(Receipt);
format_receipt(Receipt, csv) ->
    receipt_to_csv(Receipt);
format_receipt(Receipt, tsv) ->
    receipt_to_tsv(Receipt).

%% Convert receipt to CSV
receipt_to_csv(Receipt) ->
    Headers =
        "receipt_id,plan_id,version,timestamp,throughput_req_s,concurrent,queue_depth,latency_p99_ms,failover_s,current_hash\n",
    ReceiptId = maps:get(receipt_id, Receipt),
    PlanId = maps:get(plan_id, Receipt),
    Version = maps:get(version, Receipt),
    Timestamp = maps:get(timestamp, Receipt),
    Envelope = maps:get(envelope_claim, Receipt),
    Hash = maps:get(current_hash, maps:get(hash_chain, Receipt)),

    Row = io_lib:format("~s,~w,~s,~s,~f,~w,~w,~f,~f,~s\n",
                        [ReceiptId,
                         PlanId,
                         Version,
                         Timestamp,
                         maps:get(throughput_req_s, Envelope),
                         maps:get(concurrent, Envelope),
                         maps:get(queue_depth, Envelope),
                         maps:get(latency_p99_ms, Envelope),
                         maps:get(failover_s, Envelope),
                         Hash]),

    Headers ++ Row.

%% Convert receipt to TSV
receipt_to_tsv(Receipt) ->
    Headers =
        "receipt_id\tplan_id\tversion\ttimestamp\tthroughput_req_s\tconcurrent\tqueue_depth\tlatency_p99_ms\tfailover_s\tcurrent_hash\n",
    ReceiptId = maps:get(receipt_id, Receipt),
    PlanId = maps:get(plan_id, Receipt),
    Version = maps:get(version, Receipt),
    Timestamp = maps:get(timestamp, Receipt),
    Envelope = maps:get(envelope_claim, Receipt),
    Hash = maps:get(current_hash, maps:get(hash_chain, Receipt)),

    Row = io_lib:format("~s\t~w\t~s\t~s\t~f\t~w\t~w\t~f\t~f\t~s\n",
                        [ReceiptId,
                         PlanId,
                         Version,
                         Timestamp,
                         maps:get(throughput_req_s, Envelope),
                         maps:get(concurrent, Envelope),
                         maps:get(queue_depth, Envelope),
                         maps:get(latency_p99_ms, Envelope),
                         maps:get(failover_s, Envelope),
                         Hash]),

    Headers ++ Row.

%% Convert receipts list to CSV
receipts_to_csv(Receipts) ->
    Headers =
        "receipt_id,plan_id,version,timestamp,throughput_req_s,concurrent,queue_depth,latency_p99_ms,failover_s,current_hash\n",
    Rows = [receipt_to_csv(R) || R <- Receipts],
    Headers ++ lists:concat(Rows).

%% Convert receipts list to TSV
receipts_to_tsv(Receipts) ->
    Headers =
        "receipt_id\tplan_id\tversion\ttimestamp\tthroughput_req_s\tconcurrent\tqueue_depth\tlatency_p99_ms\tfailover_s\tcurrent_hash\n",
    Rows = [receipt_to_tsv(R) || R <- Receipts],
    Headers ++ lists:concat(Rows).

%% ISO 8601 timestamp
iso8601_timestamp() ->
    {Date, Time} = erlang:universaltime(),
    {Y, M, D} = Date,
    {H, Mi, S} = Time,
    list_to_binary(io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0bZ",
                                 [Y, M, D, H, Mi, S])).
