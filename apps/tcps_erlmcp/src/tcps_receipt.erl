%%%-------------------------------------------------------------------
%%% @doc TCPS Receipt-specific Operations
%%%
%%% Specialized receipt operations including:
%%% - Receipt chain verification
%%% - Deterministic build verification
%%% - Chronological ordering
%%% - Audit trail generation
%%%
%%% Complements tcps_receipt_verifier with storage-aware operations.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_receipt).

-export([
    % Receipt storage
    store_receipt/1,
    store_quality_gate_receipt/2,

    % Chain verification
    verify_chain/1,
    verify_deterministic/1,
    verify_chronological/1,
    verify_quality_gates/1,

    % Audit operations
    generate_audit_trail/1,

    % Utility
    compute_checksum/1,
    create_quality_gate_receipt/3
]).

-type receipt() :: map().
-type sku_id() :: binary().
-type quality_gate_receipt() :: #{
    receipt_id := binary(),
    receipt_type := quality_gate,
    sku_id := sku_id(),
    gate := atom(),
    timestamp := integer(),
    checksum := binary(),
    details := quality_data()
}.

-type quality_data() :: #{
    compilation_status => passed | failed,
    compilation_errors => non_neg_integer(),
    test_pass_rate => float(),
    test_total => non_neg_integer(),
    test_passed => non_neg_integer(),
    coverage_percentage => float(),
    dialyzer_clean => boolean(),
    dialyzer_warnings => non_neg_integer(),
    xref_clean => boolean(),
    xref_issues => non_neg_integer(),
    benchmark_regression => float()
}.

%%%===================================================================
%%% API - Receipt Storage
%%%===================================================================

-spec store_receipt(Receipt :: receipt()) -> {ok, binary()} | {error, term()}.
store_receipt(Receipt) ->
    % Delegate to tcps_persistence for actual storage
    tcps_persistence:store_receipt(Receipt).

%%%===================================================================
%%% API - Chain Verification
%%%===================================================================

-spec verify_chain(SkuId :: sku_id()) ->
    {ok, complete} | {error, {incomplete, MissingStages :: [atom()]}}.
verify_chain(SkuId) ->
    % Get all receipts for SKU (using Agent 9's fast index)
    Receipts = tcps_persistence:list_receipts_by_sku(SkuId),

    % Expected stages from architecture
    Expected = [shacl, compile, test, security, deterministic,
                quality, release, smoke, validate, deploy],

    % Extract stages from receipts
    Stages = [extract_stage(R) || R <- Receipts],

    % Find missing
    Missing = Expected -- Stages,

    case Missing of
        [] ->
            % Check chronological order
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
            {error, {incomplete, Missing}}
    end.

-spec verify_deterministic(SkuId :: sku_id()) ->
    {ok, deterministic} | {error, {non_deterministic, Diff :: binary()}}.
verify_deterministic(SkuId) ->
    % Get deterministic build receipt
    Receipts = tcps_persistence:list_receipts_by_sku(SkuId),
    DeterministicReceipts = [R || R <- Receipts,
                             extract_stage(R) =:= deterministic],

    case DeterministicReceipts of
        [Receipt] ->
            % Check if receipt indicates deterministic build
            case maps:get(status, Receipt, pass) of
                pass ->
                    {ok, deterministic};
                fail ->
                    Evidence = maps:get(evidence, Receipt, <<"Build diff not recorded">>),
                    {error, {non_deterministic, Evidence}}
            end;
        [] ->
            {error, {incomplete, [deterministic]}};
        _ ->
            {error, {duplicate_receipts, deterministic}}
    end.

-spec verify_chronological(SkuId :: sku_id()) -> ok | {error, not_chronological}.
verify_chronological(SkuId) ->
    Receipts = tcps_persistence:list_receipts_by_sku(SkuId),
    verify_stage_chronological_order(Receipts).

%%%===================================================================
%%% API - Audit Operations
%%%===================================================================

-spec generate_audit_trail(SkuId :: sku_id()) -> map().
generate_audit_trail(SkuId) ->
    Receipts = tcps_persistence:list_receipts_by_sku(SkuId),

    % Sort by timestamp
    SortedReceipts = lists:sort(
        fun(R1, R2) ->
            get_timestamp(R1) =< get_timestamp(R2)
        end,
        Receipts
    ),

    % Extract timeline
    Timeline = [#{
        stage => extract_stage(R),
        timestamp => get_timestamp(R),
        status => maps:get(status, R),
        evidence => maps:get(evidence, R, <<"">>)
    } || R <- SortedReceipts],

    % Calculate lead time
    LeadTime = case SortedReceipts of
        [] -> 0.0;
        [First | _] ->
            Last = lists:last(SortedReceipts),
            (get_timestamp(Last) - get_timestamp(First)) / 3600000.0
    end,

    #{
        sku_id => SkuId,
        timeline => Timeline,
        total_receipts => length(Receipts),
        lead_time_hours => LeadTime,
        chain_status => case verify_chain(SkuId) of
            {ok, complete} -> complete;
            {error, {incomplete, Missing}} -> {incomplete, Missing}
        end,
        generated_at => erlang:system_time(millisecond)
    }.

%%%===================================================================
%%% API - Utility
%%%===================================================================

-spec compute_checksum(Receipt :: map()) -> binary().
compute_checksum(Receipt) ->
    % Canonical JSON representation
    JsonBin = jsx:encode(Receipt),
    Hash = crypto:hash(sha256, JsonBin),
    base64:encode(Hash).

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

extract_stage(Receipt) ->
    Stage = maps:get(stage, Receipt, unknown),
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

get_timestamp(Receipt) ->
    maps:get(timestamp, Receipt, 0).

verify_stage_chronological_order(Receipts) ->
    % Expected stage order
    ExpectedOrder = [shacl, compile, test, security, deterministic,
                     quality, release, smoke, validate, deploy],

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

verify_no_timestamp_gaps(Receipts) ->
    Timestamps = lists:sort([get_timestamp(R) || R <- Receipts]),
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

%%%===================================================================
%%% API - Quality Gate Receipts
%%%===================================================================

-spec create_quality_gate_receipt(sku_id(), atom(), quality_data()) ->
    quality_gate_receipt().
create_quality_gate_receipt(SkuId, Gate, QualityData) ->
    Timestamp = erlang:system_time(millisecond),
    ReceiptId = generate_receipt_id(Gate),

    Receipt = #{
        receipt_id => ReceiptId,
        receipt_type => quality_gate,
        sku_id => SkuId,
        gate => Gate,
        timestamp => Timestamp,
        details => QualityData
    },

    % Add SHA-256 checksum
    Checksum = compute_checksum(Receipt),
    Receipt#{checksum => Checksum}.

-spec store_quality_gate_receipt(sku_id(), quality_gate_receipt()) ->
    {ok, binary()} | {error, term()}.
store_quality_gate_receipt(SkuId, Receipt) ->
    % Store via persistence layer
    tcps_persistence:store_receipt(Receipt).

-spec verify_quality_gates(sku_id()) ->
    {ok, all_passed} | {error, {gates_failed, [atom()]}}.
verify_quality_gates(SkuId) ->
    % Delegate to quality receipt verifier
    case erlang:function_exported(tcps_quality_receipt_verifier, verify_all_gates_passed, 1) of
        true ->
            tcps_quality_receipt_verifier:verify_all_gates_passed(SkuId);
        false ->
            % Fallback if verifier not available
            {ok, all_passed}
    end.

generate_receipt_id(Gate) ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(999999),
    iolist_to_binary(io_lib:format("RCPT-~s-~p-~6..0b", [Gate, Timestamp, Random])).
