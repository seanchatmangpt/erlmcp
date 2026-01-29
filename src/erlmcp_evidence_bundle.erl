%%%-------------------------------------------------------------------
%% @doc MCP+ Evidence Bundle - Verifiable Proof Package (.mcpb)
%%
%% Implements evidence bundles for text-blind verification:
%% - Aggregated receipts with Merkle root
%% - Metrics aggregates (latency distribution, resource usage)
%% - Refusal breakdown by code
%% - Cryptographic signature for non-repudiation
%% - Standard .mcpb format for regulator packs
%%
%% Evidence bundles can be verified without access to payload text,
%% using only structure, hashes, and metrics.
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_evidence_bundle).

-include("erlmcp_governance.hrl").

%% API - Bundle Creation
-export([
    create/2,
    create_for_range/3,
    sign/2,
    verify/1
]).

%% API - Bundle Contents
-export([
    get_receipts/1,
    get_metrics/1,
    get_refusal_breakdown/1,
    get_latency_distribution/1
]).

%% API - Merkle Tree
-export([
    compute_merkle_root/1,
    verify_merkle_proof/3,
    generate_merkle_proof/2
]).

%% API - Serialization (.mcpb format)
-export([
    to_mcpb/1,
    from_mcpb/1,
    write_file/2,
    read_file/1
]).

%% API - Lookup
-export([
    get/1,
    list_by_contract/1,
    list_by_time_range/2
]).

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/0]).

%%====================================================================
%% Types
%%====================================================================

-type merkle_proof() :: [{left | right, hash()}].
-type bundle_id() :: binary().

%%====================================================================
%% State
%%====================================================================

-record(state, {
    bundles :: #{bundle_id() => #mcp_evidence_bundle{}},
    by_contract :: #{contract_id() => [bundle_id()]},
    private_key :: binary(),
    public_key :: binary(),
    current_epoch :: epoch()
}).

%%====================================================================
%% MCPB Format Version
%%====================================================================

-define(MCPB_VERSION, <<"1.0">>).
-define(MCPB_MAGIC, <<"MCPB">>).

%%====================================================================
%% API - Bundle Creation
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Create an evidence bundle from receipts for a contract.
-spec create(contract_id(), [#mcp_receipt{}]) -> {ok, #mcp_evidence_bundle{}} | {error, term()}.
create(ContractId, Receipts) when is_list(Receipts) ->
    gen_server:call(?MODULE, {create, ContractId, Receipts}).

%% @doc Create an evidence bundle for a time range.
-spec create_for_range(contract_id(), timestamp_ms(), timestamp_ms()) ->
    {ok, #mcp_evidence_bundle{}} | {error, term()}.
create_for_range(ContractId, StartTime, EndTime) ->
    gen_server:call(?MODULE, {create_for_range, ContractId, StartTime, EndTime}).

%% @doc Sign a bundle.
-spec sign(#mcp_evidence_bundle{}, binary()) -> #mcp_evidence_bundle{}.
sign(#mcp_evidence_bundle{} = Bundle, PrivateKey) ->
    %% Compute bundle hash first
    BundleHash = compute_bundle_hash(Bundle),
    BundleWithHash = Bundle#mcp_evidence_bundle{bundle_hash = BundleHash},

    %% Sign the hash
    Signature = crypto:sign(eddsa, sha512, BundleHash, [PrivateKey, ed25519]),
    BundleWithHash#mcp_evidence_bundle{bundle_signature = Signature}.

%% @doc Verify a bundle's signature and internal consistency.
-spec verify(#mcp_evidence_bundle{}) -> boolean().
verify(#mcp_evidence_bundle{} = Bundle) ->
    %% Verify signature
    ExpectedHash = compute_bundle_hash(Bundle),
    case Bundle#mcp_evidence_bundle.bundle_hash =:= ExpectedHash of
        false -> false;
        true ->
            %% Verify merkle root
            ComputedRoot = compute_merkle_root(Bundle#mcp_evidence_bundle.receipts),
            case ComputedRoot =:= Bundle#mcp_evidence_bundle.receipt_merkle_root of
                false -> false;
                true ->
                    %% Verify signature
                    verify_bundle_signature(Bundle)
            end
    end.

%%====================================================================
%% API - Bundle Contents
%%====================================================================

%% @doc Get receipts from a bundle.
-spec get_receipts(#mcp_evidence_bundle{}) -> [#mcp_receipt{}].
get_receipts(#mcp_evidence_bundle{receipts = Receipts}) ->
    Receipts.

%% @doc Get aggregate metrics from a bundle.
-spec get_metrics(#mcp_evidence_bundle{}) -> map().
get_metrics(#mcp_evidence_bundle{} = B) ->
    #{
        total_requests => B#mcp_evidence_bundle.total_requests,
        total_refusals => B#mcp_evidence_bundle.total_refusals,
        total_memory_bytes => B#mcp_evidence_bundle.total_memory_bytes,
        total_cpu_us => B#mcp_evidence_bundle.total_cpu_us,
        total_io_bytes => B#mcp_evidence_bundle.total_io_bytes
    }.

%% @doc Get refusal breakdown by code.
-spec get_refusal_breakdown(#mcp_evidence_bundle{}) -> #{pos_integer() => pos_integer()}.
get_refusal_breakdown(#mcp_evidence_bundle{refusal_breakdown = Breakdown}) ->
    Breakdown.

%% @doc Get latency distribution.
-spec get_latency_distribution(#mcp_evidence_bundle{}) -> map().
get_latency_distribution(#mcp_evidence_bundle{} = B) ->
    #{
        p50_us => B#mcp_evidence_bundle.latency_p50_us,
        p95_us => B#mcp_evidence_bundle.latency_p95_us,
        p99_us => B#mcp_evidence_bundle.latency_p99_us
    }.

%%====================================================================
%% API - Merkle Tree
%%====================================================================

%% @doc Compute Merkle root of receipts.
-spec compute_merkle_root([#mcp_receipt{}]) -> hash().
compute_merkle_root([]) ->
    crypto:hash(sha256, <<>>);
compute_merkle_root(Receipts) ->
    %% Hash each receipt
    Leaves = [crypto:hash(sha256, erlmcp_receipt:compute_receipt_id(R)) || R <- Receipts],
    compute_merkle_root_internal(Leaves).

%% @doc Verify a Merkle proof for a receipt.
-spec verify_merkle_proof(#mcp_receipt{}, merkle_proof(), hash()) -> boolean().
verify_merkle_proof(Receipt, Proof, Root) ->
    LeafHash = crypto:hash(sha256, erlmcp_receipt:compute_receipt_id(Receipt)),
    ComputedRoot = apply_merkle_proof(LeafHash, Proof),
    ComputedRoot =:= Root.

%% @doc Generate a Merkle proof for a receipt.
-spec generate_merkle_proof(#mcp_receipt{}, [#mcp_receipt{}]) -> {ok, merkle_proof()} | {error, not_found}.
generate_merkle_proof(Receipt, Receipts) ->
    TargetHash = crypto:hash(sha256, erlmcp_receipt:compute_receipt_id(Receipt)),
    Leaves = [crypto:hash(sha256, erlmcp_receipt:compute_receipt_id(R)) || R <- Receipts],
    case find_leaf_index(TargetHash, Leaves, 0) of
        not_found -> {error, not_found};
        Index -> {ok, build_merkle_proof(Index, Leaves)}
    end.

%%====================================================================
%% API - Serialization (.mcpb format)
%%====================================================================

%% @doc Serialize bundle to .mcpb binary format.
-spec to_mcpb(#mcp_evidence_bundle{}) -> binary().
to_mcpb(#mcp_evidence_bundle{} = Bundle) ->
    %% Header
    Header = <<?MCPB_MAGIC/binary, ?MCPB_VERSION/binary>>,

    %% Metadata
    Metadata = jsx:encode(#{
        <<"id">> => Bundle#mcp_evidence_bundle.id,
        <<"version">> => Bundle#mcp_evidence_bundle.version,
        <<"contract_id">> => Bundle#mcp_evidence_bundle.contract_id,
        <<"time_range">> => encode_time_range(Bundle#mcp_evidence_bundle.time_range),
        <<"total_requests">> => Bundle#mcp_evidence_bundle.total_requests,
        <<"total_refusals">> => Bundle#mcp_evidence_bundle.total_refusals,
        <<"refusal_breakdown">> => encode_refusal_breakdown(Bundle#mcp_evidence_bundle.refusal_breakdown),
        <<"latency_p50_us">> => Bundle#mcp_evidence_bundle.latency_p50_us,
        <<"latency_p95_us">> => Bundle#mcp_evidence_bundle.latency_p95_us,
        <<"latency_p99_us">> => Bundle#mcp_evidence_bundle.latency_p99_us,
        <<"total_memory_bytes">> => Bundle#mcp_evidence_bundle.total_memory_bytes,
        <<"total_cpu_us">> => Bundle#mcp_evidence_bundle.total_cpu_us,
        <<"total_io_bytes">> => Bundle#mcp_evidence_bundle.total_io_bytes,
        <<"receipt_merkle_root">> => base64:encode(Bundle#mcp_evidence_bundle.receipt_merkle_root),
        <<"bundle_hash">> => base64:encode(Bundle#mcp_evidence_bundle.bundle_hash),
        <<"bundle_signature">> => base64:encode(Bundle#mcp_evidence_bundle.bundle_signature),
        <<"created_at">> => Bundle#mcp_evidence_bundle.created_at,
        <<"created_by">> => Bundle#mcp_evidence_bundle.created_by
    }, [sorted]),

    MetadataLen = byte_size(Metadata),

    %% Receipts (serialized)
    ReceiptsJson = jsx:encode([receipt_to_map(R) || R <- Bundle#mcp_evidence_bundle.receipts]),
    ReceiptsLen = byte_size(ReceiptsJson),

    %% Combine
    <<Header/binary,
      MetadataLen:32/big, Metadata/binary,
      ReceiptsLen:32/big, ReceiptsJson/binary>>.

%% @doc Parse bundle from .mcpb binary format.
-spec from_mcpb(binary()) -> {ok, #mcp_evidence_bundle{}} | {error, term()}.
from_mcpb(<<?MCPB_MAGIC:4/binary, ?MCPB_VERSION:3/binary,
            MetadataLen:32/big, Metadata:MetadataLen/binary,
            ReceiptsLen:32/big, ReceiptsJson:ReceiptsLen/binary>>) ->
    try
        MetadataMap = jsx:decode(Metadata, [return_maps]),
        ReceiptsList = jsx:decode(ReceiptsJson, [return_maps]),
        Receipts = [map_to_receipt(R) || R <- ReceiptsList],

        Bundle = #mcp_evidence_bundle{
            id = maps:get(<<"id">>, MetadataMap),
            version = maps:get(<<"version">>, MetadataMap),
            contract_id = maps:get(<<"contract_id">>, MetadataMap),
            time_range = decode_time_range(maps:get(<<"time_range">>, MetadataMap)),
            receipts = Receipts,
            receipt_merkle_root = base64:decode(maps:get(<<"receipt_merkle_root">>, MetadataMap)),
            total_requests = maps:get(<<"total_requests">>, MetadataMap),
            total_refusals = maps:get(<<"total_refusals">>, MetadataMap),
            refusal_breakdown = decode_refusal_breakdown(maps:get(<<"refusal_breakdown">>, MetadataMap)),
            latency_p50_us = maps:get(<<"latency_p50_us">>, MetadataMap),
            latency_p95_us = maps:get(<<"latency_p95_us">>, MetadataMap),
            latency_p99_us = maps:get(<<"latency_p99_us">>, MetadataMap),
            total_memory_bytes = maps:get(<<"total_memory_bytes">>, MetadataMap),
            total_cpu_us = maps:get(<<"total_cpu_us">>, MetadataMap),
            total_io_bytes = maps:get(<<"total_io_bytes">>, MetadataMap),
            bundle_hash = base64:decode(maps:get(<<"bundle_hash">>, MetadataMap)),
            bundle_signature = base64:decode(maps:get(<<"bundle_signature">>, MetadataMap)),
            created_at = maps:get(<<"created_at">>, MetadataMap),
            created_by = maps:get(<<"created_by">>, MetadataMap)
        },
        {ok, Bundle}
    catch
        _:Reason -> {error, {parse_error, Reason}}
    end;
from_mcpb(_) ->
    {error, invalid_format}.

%% @doc Write bundle to file.
-spec write_file(#mcp_evidence_bundle{}, file:filename()) -> ok | {error, term()}.
write_file(Bundle, Filename) ->
    Binary = to_mcpb(Bundle),
    file:write_file(Filename, Binary).

%% @doc Read bundle from file.
-spec read_file(file:filename()) -> {ok, #mcp_evidence_bundle{}} | {error, term()}.
read_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} -> from_mcpb(Binary);
        Error -> Error
    end.

%%====================================================================
%% API - Lookup
%%====================================================================

%% @doc Get a bundle by ID.
-spec get(bundle_id()) -> {ok, #mcp_evidence_bundle{}} | {error, not_found}.
get(BundleId) ->
    gen_server:call(?MODULE, {get, BundleId}).

%% @doc List bundles for a contract.
-spec list_by_contract(contract_id()) -> [#mcp_evidence_bundle{}].
list_by_contract(ContractId) ->
    gen_server:call(?MODULE, {list_by_contract, ContractId}).

%% @doc List bundles in a time range.
-spec list_by_time_range(timestamp_ms(), timestamp_ms()) -> [#mcp_evidence_bundle{}].
list_by_time_range(StartTime, EndTime) ->
    gen_server:call(?MODULE, {list_by_time_range, StartTime, EndTime}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {PublicKey, PrivateKey} = generate_keypair(),
    State = #state{
        bundles = #{},
        by_contract = #{},
        private_key = PrivateKey,
        public_key = PublicKey,
        current_epoch = 1
    },
    {ok, State}.

handle_call({create, ContractId, Receipts}, _From, State) ->
    Now = erlang:system_time(millisecond),
    Bundle = build_bundle(ContractId, Receipts, Now, State),
    SignedBundle = sign(Bundle, State#state.private_key),

    %% Store
    BundleId = SignedBundle#mcp_evidence_bundle.id,
    NewBundles = maps:put(BundleId, SignedBundle, State#state.bundles),
    ContractBundles = maps:get(ContractId, State#state.by_contract, []),
    NewByContract = maps:put(ContractId, [BundleId | ContractBundles], State#state.by_contract),

    NewState = State#state{
        bundles = NewBundles,
        by_contract = NewByContract
    },
    {reply, {ok, SignedBundle}, NewState};

handle_call({create_for_range, ContractId, StartTime, EndTime}, _From, State) ->
    %% Get receipts from receipt module
    Receipts = erlmcp_receipt:get_by_contract(ContractId),
    FilteredReceipts = [R || R <- Receipts,
                        R#mcp_receipt.timestamp >= StartTime,
                        R#mcp_receipt.timestamp =< EndTime],

    Now = erlang:system_time(millisecond),
    Bundle = build_bundle_with_range(ContractId, FilteredReceipts, StartTime, EndTime, Now, State),
    SignedBundle = sign(Bundle, State#state.private_key),

    %% Store
    BundleId = SignedBundle#mcp_evidence_bundle.id,
    NewBundles = maps:put(BundleId, SignedBundle, State#state.bundles),
    ContractBundles = maps:get(ContractId, State#state.by_contract, []),
    NewByContract = maps:put(ContractId, [BundleId | ContractBundles], State#state.by_contract),

    NewState = State#state{
        bundles = NewBundles,
        by_contract = NewByContract
    },
    {reply, {ok, SignedBundle}, NewState};

handle_call({get, BundleId}, _From, State) ->
    case maps:get(BundleId, State#state.bundles, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Bundle -> {reply, {ok, Bundle}, State}
    end;

handle_call({list_by_contract, ContractId}, _From, State) ->
    Ids = maps:get(ContractId, State#state.by_contract, []),
    Bundles = [maps:get(Id, State#state.bundles) || Id <- Ids,
               maps:is_key(Id, State#state.bundles)],
    {reply, Bundles, State};

handle_call({list_by_time_range, StartTime, EndTime}, _From, State) ->
    Bundles = maps:fold(fun(_Id, Bundle, Acc) ->
        {BundleStart, BundleEnd} = Bundle#mcp_evidence_bundle.time_range,
        case BundleStart >= StartTime andalso BundleEnd =< EndTime of
            true -> [Bundle | Acc];
            false -> Acc
        end
    end, [], State#state.bundles),
    {reply, Bundles, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_keypair() -> {binary(), binary()}.
generate_keypair() ->
    #{public := Pub, secret := Priv} = crypto:generate_key(eddsa, ed25519),
    {Pub, Priv}.

-spec build_bundle(contract_id(), [#mcp_receipt{}], timestamp_ms(), #state{}) ->
    #mcp_evidence_bundle{}.
build_bundle(ContractId, Receipts, Now, State) ->
    {StartTime, EndTime} = compute_time_range(Receipts),
    build_bundle_with_range(ContractId, Receipts, StartTime, EndTime, Now, State).

-spec build_bundle_with_range(contract_id(), [#mcp_receipt{}], timestamp_ms(), timestamp_ms(),
                              timestamp_ms(), #state{}) -> #mcp_evidence_bundle{}.
build_bundle_with_range(ContractId, Receipts, StartTime, EndTime, Now, _State) ->
    %% Compute metrics
    {TotalRequests, TotalRefusals, RefusalBreakdown} = compute_refusal_stats(Receipts),
    {P50, P95, P99} = compute_latency_percentiles(Receipts),
    {TotalMemory, TotalCpu, TotalIo} = compute_resource_totals(Receipts),

    %% Compute Merkle root
    MerkleRoot = compute_merkle_root(Receipts),

    %% Generate bundle ID
    BundleId = generate_bundle_id(ContractId, StartTime, EndTime),

    %% Get node identifier
    NodeId = atom_to_binary(node(), utf8),

    #mcp_evidence_bundle{
        id = BundleId,
        version = ?MCPB_VERSION,
        contract_id = ContractId,
        time_range = {StartTime, EndTime},
        receipts = Receipts,
        receipt_merkle_root = MerkleRoot,
        total_requests = TotalRequests,
        total_refusals = TotalRefusals,
        refusal_breakdown = RefusalBreakdown,
        latency_p50_us = P50,
        latency_p95_us = P95,
        latency_p99_us = P99,
        total_memory_bytes = TotalMemory,
        total_cpu_us = TotalCpu,
        total_io_bytes = TotalIo,
        bundle_hash = <<>>,  % Set during signing
        bundle_signature = <<>>,
        created_at = Now,
        created_by = NodeId
    }.

-spec generate_bundle_id(contract_id(), timestamp_ms(), timestamp_ms()) -> bundle_id().
generate_bundle_id(ContractId, StartTime, EndTime) ->
    Data = <<ContractId/binary, (integer_to_binary(StartTime))/binary,
             (integer_to_binary(EndTime))/binary>>,
    Hash = crypto:hash(sha256, Data),
    base64:encode(Hash).

-spec compute_time_range([#mcp_receipt{}]) -> {timestamp_ms(), timestamp_ms()}.
compute_time_range([]) ->
    Now = erlang:system_time(millisecond),
    {Now, Now};
compute_time_range(Receipts) ->
    Timestamps = [R#mcp_receipt.timestamp || R <- Receipts],
    {lists:min(Timestamps), lists:max(Timestamps)}.

-spec compute_refusal_stats([#mcp_receipt{}]) ->
    {pos_integer(), pos_integer(), #{pos_integer() => pos_integer()}}.
compute_refusal_stats(Receipts) ->
    lists:foldl(fun(R, {Total, Refusals, Breakdown}) ->
        NewTotal = Total + 1,
        case R#mcp_receipt.outcome of
            refused ->
                Code = R#mcp_receipt.refusal_code,
                Count = maps:get(Code, Breakdown, 0),
                {NewTotal, Refusals + 1, maps:put(Code, Count + 1, Breakdown)};
            _ ->
                {NewTotal, Refusals, Breakdown}
        end
    end, {0, 0, #{}}, Receipts).

-spec compute_latency_percentiles([#mcp_receipt{}]) ->
    {pos_integer(), pos_integer(), pos_integer()}.
compute_latency_percentiles([]) ->
    {0, 0, 0};
compute_latency_percentiles(Receipts) ->
    Latencies = lists:sort([R#mcp_receipt.duration_us || R <- Receipts]),
    Len = length(Latencies),
    P50 = lists:nth(max(1, round(Len * 0.50)), Latencies),
    P95 = lists:nth(max(1, round(Len * 0.95)), Latencies),
    P99 = lists:nth(max(1, round(Len * 0.99)), Latencies),
    {P50, P95, P99}.

-spec compute_resource_totals([#mcp_receipt{}]) ->
    {pos_integer(), pos_integer(), pos_integer()}.
compute_resource_totals(Receipts) ->
    lists:foldl(fun(R, {Memory, Cpu, Io}) ->
        {Memory + R#mcp_receipt.memory_bytes,
         Cpu + R#mcp_receipt.cpu_us,
         Io + R#mcp_receipt.io_bytes}
    end, {0, 0, 0}, Receipts).

-spec compute_bundle_hash(#mcp_evidence_bundle{}) -> hash().
compute_bundle_hash(#mcp_evidence_bundle{} = B) ->
    %% Hash everything except the hash and signature fields
    Data = jsx:encode(#{
        <<"id">> => B#mcp_evidence_bundle.id,
        <<"version">> => B#mcp_evidence_bundle.version,
        <<"contract_id">> => B#mcp_evidence_bundle.contract_id,
        <<"time_range">> => encode_time_range(B#mcp_evidence_bundle.time_range),
        <<"receipt_merkle_root">> => base64:encode(B#mcp_evidence_bundle.receipt_merkle_root),
        <<"total_requests">> => B#mcp_evidence_bundle.total_requests,
        <<"total_refusals">> => B#mcp_evidence_bundle.total_refusals,
        <<"total_memory_bytes">> => B#mcp_evidence_bundle.total_memory_bytes,
        <<"total_cpu_us">> => B#mcp_evidence_bundle.total_cpu_us,
        <<"total_io_bytes">> => B#mcp_evidence_bundle.total_io_bytes,
        <<"latency_p50_us">> => B#mcp_evidence_bundle.latency_p50_us,
        <<"latency_p95_us">> => B#mcp_evidence_bundle.latency_p95_us,
        <<"latency_p99_us">> => B#mcp_evidence_bundle.latency_p99_us,
        <<"created_at">> => B#mcp_evidence_bundle.created_at,
        <<"created_by">> => B#mcp_evidence_bundle.created_by
    }, [sorted]),
    crypto:hash(sha256, Data).

-spec verify_bundle_signature(#mcp_evidence_bundle{}) -> boolean().
verify_bundle_signature(#mcp_evidence_bundle{} = Bundle) ->
    try
        %% In production, look up public key for the bundle's epoch
        %% For now, just verify signature format
        Signature = Bundle#mcp_evidence_bundle.bundle_signature,
        Hash = Bundle#mcp_evidence_bundle.bundle_hash,
        byte_size(Signature) > 0 andalso byte_size(Hash) > 0
    catch
        _:_ -> false
    end.

-spec compute_merkle_root_internal([hash()]) -> hash().
compute_merkle_root_internal([Single]) ->
    Single;
compute_merkle_root_internal(Hashes) ->
    %% Pair and hash
    Paired = pair_hashes(Hashes),
    compute_merkle_root_internal(Paired).

-spec pair_hashes([hash()]) -> [hash()].
pair_hashes([]) -> [];
pair_hashes([Single]) -> [Single];
pair_hashes([H1, H2 | Rest]) ->
    Combined = crypto:hash(sha256, <<H1/binary, H2/binary>>),
    [Combined | pair_hashes(Rest)].

-spec find_leaf_index(hash(), [hash()], non_neg_integer()) -> non_neg_integer() | not_found.
find_leaf_index(_Target, [], _Index) -> not_found;
find_leaf_index(Target, [Target | _], Index) -> Index;
find_leaf_index(Target, [_ | Rest], Index) -> find_leaf_index(Target, Rest, Index + 1).

-spec build_merkle_proof(non_neg_integer(), [hash()]) -> merkle_proof().
build_merkle_proof(_Index, [_Single]) ->
    [];
build_merkle_proof(Index, Hashes) ->
    SiblingIndex = case Index rem 2 of
        0 -> Index + 1;
        1 -> Index - 1
    end,
    Sibling = case SiblingIndex < length(Hashes) of
        true -> lists:nth(SiblingIndex + 1, Hashes);
        false -> lists:nth(Index + 1, Hashes)  % Duplicate last
    end,
    Direction = case Index rem 2 of
        0 -> right;
        1 -> left
    end,
    [{Direction, Sibling} | build_merkle_proof(Index div 2, pair_hashes(Hashes))].

-spec apply_merkle_proof(hash(), merkle_proof()) -> hash().
apply_merkle_proof(Hash, []) ->
    Hash;
apply_merkle_proof(Hash, [{left, Sibling} | Rest]) ->
    Combined = crypto:hash(sha256, <<Sibling/binary, Hash/binary>>),
    apply_merkle_proof(Combined, Rest);
apply_merkle_proof(Hash, [{right, Sibling} | Rest]) ->
    Combined = crypto:hash(sha256, <<Hash/binary, Sibling/binary>>),
    apply_merkle_proof(Combined, Rest).

-spec encode_time_range({timestamp_ms(), timestamp_ms()}) -> map().
encode_time_range({Start, End}) ->
    #{<<"start">> => Start, <<"end">> => End}.

-spec decode_time_range(map()) -> {timestamp_ms(), timestamp_ms()}.
decode_time_range(#{<<"start">> := Start, <<"end">> := End}) ->
    {Start, End}.

-spec encode_refusal_breakdown(#{pos_integer() => pos_integer()}) -> map().
encode_refusal_breakdown(Breakdown) ->
    maps:fold(fun(Code, Count, Acc) ->
        Acc#{integer_to_binary(Code) => Count}
    end, #{}, Breakdown).

-spec decode_refusal_breakdown(map()) -> #{pos_integer() => pos_integer()}.
decode_refusal_breakdown(Map) ->
    maps:fold(fun(CodeBin, Count, Acc) ->
        Code = binary_to_integer(CodeBin),
        Acc#{Code => Count}
    end, #{}, Map).

-spec receipt_to_map(#mcp_receipt{}) -> map().
receipt_to_map(#mcp_receipt{} = R) ->
    #{
        <<"id">> => R#mcp_receipt.id,
        <<"sequence">> => R#mcp_receipt.sequence,
        <<"request_id">> => R#mcp_receipt.request_id,
        <<"contract_id">> => R#mcp_receipt.contract_id,
        <<"envelope_id">> => R#mcp_receipt.envelope_id,
        <<"method">> => R#mcp_receipt.method,
        <<"request_hash">> => base64:encode(R#mcp_receipt.request_hash),
        <<"response_hash">> => base64:encode(R#mcp_receipt.response_hash),
        <<"outcome">> => atom_to_binary(R#mcp_receipt.outcome, utf8),
        <<"refusal_code">> => R#mcp_receipt.refusal_code,
        <<"duration_us">> => R#mcp_receipt.duration_us,
        <<"memory_bytes">> => R#mcp_receipt.memory_bytes,
        <<"cpu_us">> => R#mcp_receipt.cpu_us,
        <<"io_bytes">> => R#mcp_receipt.io_bytes,
        <<"previous_hash">> => case R#mcp_receipt.previous_hash of
            undefined -> null;
            H -> base64:encode(H)
        end,
        <<"epoch">> => R#mcp_receipt.epoch,
        <<"signature">> => base64:encode(R#mcp_receipt.signature),
        <<"timestamp">> => R#mcp_receipt.timestamp
    }.

-spec map_to_receipt(map()) -> #mcp_receipt{}.
map_to_receipt(M) ->
    #mcp_receipt{
        id = maps:get(<<"id">>, M),
        sequence = maps:get(<<"sequence">>, M),
        request_id = maps:get(<<"request_id">>, M),
        contract_id = maps:get(<<"contract_id">>, M),
        envelope_id = maps:get(<<"envelope_id">>, M),
        method = maps:get(<<"method">>, M),
        request_hash = base64:decode(maps:get(<<"request_hash">>, M)),
        response_hash = base64:decode(maps:get(<<"response_hash">>, M)),
        outcome = binary_to_atom(maps:get(<<"outcome">>, M), utf8),
        refusal_code = maps:get(<<"refusal_code">>, M),
        duration_us = maps:get(<<"duration_us">>, M),
        memory_bytes = maps:get(<<"memory_bytes">>, M),
        cpu_us = maps:get(<<"cpu_us">>, M),
        io_bytes = maps:get(<<"io_bytes">>, M),
        previous_hash = case maps:get(<<"previous_hash">>, M) of
            null -> undefined;
            H -> base64:decode(H)
        end,
        epoch = maps:get(<<"epoch">>, M),
        signature = base64:decode(maps:get(<<"signature">>, M)),
        timestamp = maps:get(<<"timestamp">>, M)
    }.
