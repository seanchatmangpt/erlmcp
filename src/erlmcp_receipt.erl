%%%-------------------------------------------------------------------
%% @doc MCP+ Receipt System - Cryptographic Audit Trail
%%
%% Implements text-blind receipts with:
%% - Hash-based request/response references (no payload text)
%% - Chained receipts with previous hash links
%% - Cryptographic signatures per epoch
%% - Monotonic sequence numbers
%% - Quantified metrics only
%%
%% Receipts provide non-repudiable proof of operation without
%% exposing payload contents - enabling text-blind verification.
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_receipt).

-include("erlmcp_governance.hrl").

%% API - Receipt Creation
-export([
    create/4,
    sign/2,
    verify/1
]).

%% API - Chain Management
-export([
    get_chain_head/1,
    append_to_chain/2,
    verify_chain/1,
    chain_length/1
]).

%% API - Lookup
-export([
    get/1,
    get_by_request/1,
    get_by_contract/1,
    get_range/2,
    list_recent/1
]).

%% API - Text-Blind Hashing
-export([
    hash_request/1,
    hash_response/1,
    compute_receipt_id/1
]).

%% API - Metrics
-export([
    record_metrics/2,
    get_aggregate_metrics/1
]).

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/0]).

%%====================================================================
%% Types
%%====================================================================

-type create_opts() :: #{
    request_id => binary(),
    contract_id => contract_id(),
    envelope_id => binary(),
    method => binary(),
    request_payload => binary() | map(),
    response_payload => binary() | map()
}.

-type metrics() :: #{
    duration_us => pos_integer(),
    memory_bytes => pos_integer(),
    cpu_us => pos_integer(),
    io_bytes => pos_integer()
}.

%%====================================================================
%% State
%%====================================================================

-record(state, {
    %% Receipt storage
    receipts :: #{binary() => #mcp_receipt{}},
    %% Index by request ID
    by_request :: #{binary() => binary()},
    %% Index by contract ID
    by_contract :: #{contract_id() => [binary()]},
    %% Chain heads per contract
    chain_heads :: #{contract_id() => #mcp_receipt{}},
    %% Sequence numbers per contract
    sequences :: #{contract_id() => pos_integer()},
    %% Signing keys
    private_key :: binary(),
    public_key :: binary(),
    current_epoch :: epoch()
}).

%%====================================================================
%% API - Receipt Creation
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Create a new receipt for an operation.
%% Request and response payloads are hashed, not stored.
-spec create(ok | refused | error, pos_integer() | undefined, create_opts(), metrics()) ->
    {ok, #mcp_receipt{}} | {error, term()}.
create(Outcome, RefusalCode, Opts, Metrics) when
    is_map(Opts), is_map(Metrics) ->
    gen_server:call(?MODULE, {create, Outcome, RefusalCode, Opts, Metrics}).

%% @doc Sign a receipt with current epoch key.
-spec sign(#mcp_receipt{}, binary()) -> #mcp_receipt{}.
sign(#mcp_receipt{} = Receipt, PrivateKey) ->
    SignableData = receipt_to_signable(Receipt),
    Signature = crypto:sign(eddsa, sha512, SignableData, [PrivateKey, ed25519]),
    Receipt#mcp_receipt{signature = Signature}.

%% @doc Verify a receipt's signature.
-spec verify(#mcp_receipt{}) -> boolean().
verify(#mcp_receipt{} = Receipt) ->
    SignableData = receipt_to_signable(Receipt),
    try
        %% Get public key for the receipt's epoch
        case get_epoch_public_key(Receipt#mcp_receipt.epoch) of
            {ok, PublicKey} ->
                crypto:verify(eddsa, sha512, SignableData,
                             Receipt#mcp_receipt.signature, [PublicKey, ed25519]);
            {error, _} ->
                false
        end
    catch
        _:_ -> false
    end.

%%====================================================================
%% API - Chain Management
%%====================================================================

%% @doc Get the head receipt of a contract's chain.
-spec get_chain_head(contract_id()) -> {ok, #mcp_receipt{}} | {error, not_found}.
get_chain_head(ContractId) ->
    gen_server:call(?MODULE, {get_chain_head, ContractId}).

%% @doc Append a receipt to the chain.
-spec append_to_chain(contract_id(), #mcp_receipt{}) -> {ok, #mcp_receipt{}} | {error, term()}.
append_to_chain(ContractId, Receipt) ->
    gen_server:call(?MODULE, {append_to_chain, ContractId, Receipt}).

%% @doc Verify an entire receipt chain.
-spec verify_chain(contract_id()) -> {ok, pos_integer()} | {error, {pos_integer(), term()}}.
verify_chain(ContractId) ->
    gen_server:call(?MODULE, {verify_chain, ContractId}).

%% @doc Get the length of a contract's receipt chain.
-spec chain_length(contract_id()) -> pos_integer().
chain_length(ContractId) ->
    gen_server:call(?MODULE, {chain_length, ContractId}).

%%====================================================================
%% API - Lookup
%%====================================================================

%% @doc Get a receipt by ID.
-spec get(binary()) -> {ok, #mcp_receipt{}} | {error, not_found}.
get(ReceiptId) ->
    gen_server:call(?MODULE, {get, ReceiptId}).

%% @doc Get a receipt by request ID.
-spec get_by_request(binary()) -> {ok, #mcp_receipt{}} | {error, not_found}.
get_by_request(RequestId) ->
    gen_server:call(?MODULE, {get_by_request, RequestId}).

%% @doc Get all receipts for a contract.
-spec get_by_contract(contract_id()) -> [#mcp_receipt{}].
get_by_contract(ContractId) ->
    gen_server:call(?MODULE, {get_by_contract, ContractId}).

%% @doc Get receipts in a time range.
-spec get_range(timestamp_ms(), timestamp_ms()) -> [#mcp_receipt{}].
get_range(StartTime, EndTime) ->
    gen_server:call(?MODULE, {get_range, StartTime, EndTime}).

%% @doc List recent receipts.
-spec list_recent(pos_integer()) -> [#mcp_receipt{}].
list_recent(Limit) ->
    gen_server:call(?MODULE, {list_recent, Limit}).

%%====================================================================
%% API - Text-Blind Hashing
%%====================================================================

%% @doc Hash a request payload (text-blind).
-spec hash_request(binary() | map()) -> hash().
hash_request(Payload) when is_binary(Payload) ->
    crypto:hash(sha256, Payload);
hash_request(Payload) when is_map(Payload) ->
    %% Canonical JSON encoding for consistent hashing
    Json = erlmcp_json:canonical_encode(Payload),
    crypto:hash(sha256, Json).

%% @doc Hash a response payload (text-blind).
-spec hash_response(binary() | map()) -> hash().
hash_response(Payload) when is_binary(Payload) ->
    crypto:hash(sha256, Payload);
hash_response(Payload) when is_map(Payload) ->
    Json = erlmcp_json:canonical_encode(Payload),
    crypto:hash(sha256, Json).

%% @doc Compute receipt ID from contents.
-spec compute_receipt_id(#mcp_receipt{}) -> binary().
compute_receipt_id(#mcp_receipt{} = R) ->
    Data = <<
        (R#mcp_receipt.request_id)/binary,
        (R#mcp_receipt.contract_id)/binary,
        (R#mcp_receipt.request_hash)/binary,
        (R#mcp_receipt.response_hash)/binary,
        (integer_to_binary(R#mcp_receipt.timestamp))/binary
    >>,
    Hash = crypto:hash(sha256, Data),
    base64:encode(Hash).

%%====================================================================
%% API - Metrics
%%====================================================================

%% @doc Record metrics to a receipt.
-spec record_metrics(binary(), metrics()) -> ok | {error, not_found}.
record_metrics(ReceiptId, Metrics) ->
    gen_server:call(?MODULE, {record_metrics, ReceiptId, Metrics}).

%% @doc Get aggregate metrics for a contract.
-spec get_aggregate_metrics(contract_id()) -> map().
get_aggregate_metrics(ContractId) ->
    gen_server:call(?MODULE, {get_aggregate_metrics, ContractId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {PublicKey, PrivateKey} = generate_keypair(),
    State = #state{
        receipts = #{},
        by_request = #{},
        by_contract = #{},
        chain_heads = #{},
        sequences = #{},
        private_key = PrivateKey,
        public_key = PublicKey,
        current_epoch = 1
    },
    {ok, State}.

handle_call({create, Outcome, RefusalCode, Opts, Metrics}, _From, State) ->
    Now = erlang:system_time(millisecond),
    ContractId = maps:get(contract_id, Opts, <<>>),

    %% Get next sequence number
    Seq = maps:get(ContractId, State#state.sequences, 0) + 1,

    %% Get previous hash for chain
    PreviousHash = case maps:get(ContractId, State#state.chain_heads, undefined) of
        undefined -> undefined;
        PrevReceipt -> compute_receipt_hash(PrevReceipt)
    end,

    %% Hash payloads (text-blind)
    RequestHash = hash_request(maps:get(request_payload, Opts, <<>>)),
    ResponseHash = hash_response(maps:get(response_payload, Opts, <<>>)),

    Receipt = #mcp_receipt{
        id = <<>>,  % Set after computing
        sequence = Seq,
        request_id = maps:get(request_id, Opts, <<>>),
        contract_id = ContractId,
        envelope_id = maps:get(envelope_id, Opts, <<>>),
        method = maps:get(method, Opts, <<>>),
        request_hash = RequestHash,
        response_hash = ResponseHash,
        outcome = Outcome,
        refusal_code = RefusalCode,
        duration_us = maps:get(duration_us, Metrics, 0),
        memory_bytes = maps:get(memory_bytes, Metrics, 0),
        cpu_us = maps:get(cpu_us, Metrics, 0),
        io_bytes = maps:get(io_bytes, Metrics, 0),
        previous_hash = PreviousHash,
        epoch = State#state.current_epoch,
        signature = <<>>,  % Set after signing
        timestamp = Now
    },

    %% Set ID based on content
    ReceiptId = compute_receipt_id(Receipt),
    ReceiptWithId = Receipt#mcp_receipt{id = ReceiptId},

    %% Sign receipt
    SignedReceipt = sign(ReceiptWithId, State#state.private_key),

    %% Store and index
    NewReceipts = maps:put(ReceiptId, SignedReceipt, State#state.receipts),
    NewByRequest = maps:put(SignedReceipt#mcp_receipt.request_id, ReceiptId, State#state.by_request),

    ContractReceipts = maps:get(ContractId, State#state.by_contract, []),
    NewByContract = maps:put(ContractId, [ReceiptId | ContractReceipts], State#state.by_contract),

    NewChainHeads = maps:put(ContractId, SignedReceipt, State#state.chain_heads),
    NewSequences = maps:put(ContractId, Seq, State#state.sequences),

    NewState = State#state{
        receipts = NewReceipts,
        by_request = NewByRequest,
        by_contract = NewByContract,
        chain_heads = NewChainHeads,
        sequences = NewSequences
    },

    {reply, {ok, SignedReceipt}, NewState};

handle_call({get, ReceiptId}, _From, State) ->
    case maps:get(ReceiptId, State#state.receipts, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Receipt -> {reply, {ok, Receipt}, State}
    end;

handle_call({get_by_request, RequestId}, _From, State) ->
    case maps:get(RequestId, State#state.by_request, undefined) of
        undefined -> {reply, {error, not_found}, State};
        ReceiptId ->
            Receipt = maps:get(ReceiptId, State#state.receipts),
            {reply, {ok, Receipt}, State}
    end;

handle_call({get_by_contract, ContractId}, _From, State) ->
    Ids = maps:get(ContractId, State#state.by_contract, []),
    Receipts = [maps:get(Id, State#state.receipts) || Id <- Ids,
                maps:is_key(Id, State#state.receipts)],
    {reply, Receipts, State};

handle_call({get_chain_head, ContractId}, _From, State) ->
    case maps:get(ContractId, State#state.chain_heads, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Receipt -> {reply, {ok, Receipt}, State}
    end;

handle_call({chain_length, ContractId}, _From, State) ->
    Length = maps:get(ContractId, State#state.sequences, 0),
    {reply, Length, State};

handle_call({verify_chain, ContractId}, _From, State) ->
    Ids = lists:reverse(maps:get(ContractId, State#state.by_contract, [])),
    Result = verify_chain_internal(Ids, State#state.receipts, undefined, 0),
    {reply, Result, State};

handle_call({get_range, StartTime, EndTime}, _From, State) ->
    Receipts = maps:fold(fun(_Id, Receipt, Acc) ->
        Ts = Receipt#mcp_receipt.timestamp,
        case Ts >= StartTime andalso Ts =< EndTime of
            true -> [Receipt | Acc];
            false -> Acc
        end
    end, [], State#state.receipts),
    {reply, Receipts, State};

handle_call({list_recent, Limit}, _From, State) ->
    All = maps:values(State#state.receipts),
    Sorted = lists:sort(fun(A, B) ->
        A#mcp_receipt.timestamp > B#mcp_receipt.timestamp
    end, All),
    {reply, lists:sublist(Sorted, Limit), State};

handle_call({record_metrics, ReceiptId, Metrics}, _From, State) ->
    case maps:get(ReceiptId, State#state.receipts, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Receipt ->
            Updated = Receipt#mcp_receipt{
                duration_us = maps:get(duration_us, Metrics, Receipt#mcp_receipt.duration_us),
                memory_bytes = maps:get(memory_bytes, Metrics, Receipt#mcp_receipt.memory_bytes),
                cpu_us = maps:get(cpu_us, Metrics, Receipt#mcp_receipt.cpu_us),
                io_bytes = maps:get(io_bytes, Metrics, Receipt#mcp_receipt.io_bytes)
            },
            NewReceipts = maps:put(ReceiptId, Updated, State#state.receipts),
            {reply, ok, State#state{receipts = NewReceipts}}
    end;

handle_call({get_aggregate_metrics, ContractId}, _From, State) ->
    Ids = maps:get(ContractId, State#state.by_contract, []),
    Aggregate = lists:foldl(fun(Id, Acc) ->
        case maps:get(Id, State#state.receipts, undefined) of
            undefined -> Acc;
            R ->
                #{
                    total_requests => maps:get(total_requests, Acc, 0) + 1,
                    total_duration_us => maps:get(total_duration_us, Acc, 0) + R#mcp_receipt.duration_us,
                    total_memory_bytes => maps:get(total_memory_bytes, Acc, 0) + R#mcp_receipt.memory_bytes,
                    total_cpu_us => maps:get(total_cpu_us, Acc, 0) + R#mcp_receipt.cpu_us,
                    total_io_bytes => maps:get(total_io_bytes, Acc, 0) + R#mcp_receipt.io_bytes,
                    refusals => maps:get(refusals, Acc, 0) +
                        case R#mcp_receipt.outcome of refused -> 1; _ -> 0 end
                }
        end
    end, #{}, Ids),
    {reply, Aggregate, State};

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

-spec receipt_to_signable(#mcp_receipt{}) -> binary().
receipt_to_signable(#mcp_receipt{} = R) ->
    %% Canonical representation for signing (excludes signature)
    Map = #{
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
        <<"timestamp">> => R#mcp_receipt.timestamp
    },
    erlmcp_json:canonical_encode(Map).

-spec compute_receipt_hash(#mcp_receipt{}) -> hash().
compute_receipt_hash(Receipt) ->
    SignableData = receipt_to_signable(Receipt),
    crypto:hash(sha256, SignableData).

-spec get_epoch_public_key(epoch()) -> {ok, binary()} | {error, term()}.
get_epoch_public_key(Epoch) ->
    %% In production, look up from key registry
    %% For now, use current key
    case erlmcp_contract:current_epoch() of
        Epoch ->
            %% Current epoch - use stored key
            {ok, get_current_public_key()};
        _ ->
            %% Historical epoch - would need key archive
            {error, epoch_key_not_found}
    end.

-spec get_current_public_key() -> binary().
get_current_public_key() ->
    %% Would normally retrieve from state or config
    <<>>.

-spec verify_chain_internal([binary()], map(), hash() | undefined, pos_integer()) ->
    {ok, pos_integer()} | {error, {pos_integer(), term()}}.
verify_chain_internal([], _Receipts, _ExpectedPrevHash, Count) ->
    {ok, Count};
verify_chain_internal([Id | Rest], Receipts, ExpectedPrevHash, Count) ->
    case maps:get(Id, Receipts, undefined) of
        undefined ->
            {error, {Count + 1, missing_receipt}};
        Receipt ->
            %% Verify previous hash
            case Receipt#mcp_receipt.previous_hash =:= ExpectedPrevHash of
                false when ExpectedPrevHash =/= undefined ->
                    {error, {Count + 1, chain_broken}};
                _ ->
                    %% Verify signature
                    case verify(Receipt) of
                        false ->
                            {error, {Count + 1, invalid_signature}};
                        true ->
                            NextPrevHash = compute_receipt_hash(Receipt),
                            verify_chain_internal(Rest, Receipts, NextPrevHash, Count + 1)
                    end
            end
    end.
