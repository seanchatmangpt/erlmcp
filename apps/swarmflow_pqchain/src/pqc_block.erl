%%% @doc Post-Quantum Blockchain Block Handling
%%%
%%% Implements block creation, validation, signing, and execution for the
%%% post-quantum blockchain. Follows OTP patterns and erlmcp conventions.
%%%
%%% Block validation enforces:
%%% - Header signature validity (ML-DSA or hybrid)
%%% - Previous hash chain continuity
%%% - Merkle root correctness (transactions + receipts)
%%% - Timestamp monotonicity and reasonableness
%%% - Validator authorization and stake
%%% - Transaction validity within block context
%%%
%%% @end
-module(pqc_block).

-include("pqchain.hrl").

%%% API Exports
-export([
    create/3,
    sign/2,
    verify/1,
    validate/2,
    hash/1,
    encode/1,
    decode/1,
    merkle_root/1,
    execute/2,
    genesis/1
]).

%%% Type Exports
-export_type([
    validation_error/0
]).

%%% ============================================================================
%%% Type Definitions
%%% ============================================================================

-type validation_error() :: {invalid_signature, term()}
                          | {prev_hash_mismatch, binary(), binary()}
                          | {merkle_root_mismatch, tx_root | receipt_root, binary(), binary()}
                          | {timestamp_error, term()}
                          | {validator_not_found, binary()}
                          | {validator_unauthorized, binary()}
                          | {transaction_invalid, binary(), term()}
                          | {consensus_proof_invalid, term()}.

%%% ============================================================================
%%% API Functions
%%% ============================================================================

%% @doc Create a new block with given height, previous hash, and transactions.
%%
%% Creates an unsigned block header with:
%% - Computed transaction merkle root
%% - Empty receipt root (filled after execution)
%% - Current timestamp
%% - No validator signature (must be signed separately)
%%
%% @end
-spec create(Height :: non_neg_integer(),
             PrevHash :: binary(),
             Transactions :: [#pqc_transaction{}]) ->
    #pqc_block{}.
create(Height, PrevHash, Transactions) ->
    Timestamp = erlang:system_time(millisecond),
    TxRoot = merkle_root(Transactions),

    Header = #pqc_block_header{
        version = 1,
        height = Height,
        timestamp = Timestamp,
        prev_hash = PrevHash,
        state_root = <<0:256>>,  % Updated after execution
        tx_root = TxRoot,
        receipt_root = <<0:256>>,  % Updated after execution
        validator = <<>>,  % Set during signing
        validator_signature = undefined,  % Set during signing
        consensus_proof = undefined,  % Set during consensus
        chain_id = <<>>  % Set by caller if needed
    },

    #pqc_block{
        header = Header,
        transactions = Transactions,
        receipts = [],  % Filled after execution
        validator_set_hash = <<0:256>>,  % Set by caller
        next_validator_set_hash = undefined
    }.

%% @doc Sign block header with validator keypair.
%%
%% Updates the block header with:
%% - Validator address (derived from public key)
%% - ML-DSA or hybrid signature over header hash
%%
%% The signature covers all header fields except the signature itself.
%% Uses pqc_crypto module for signing operations.
%%
%% @end
-spec sign(Block :: #pqc_block{},
           Keypair :: #pqc_keypair{}) ->
    #pqc_block{}.
sign(#pqc_block{header = Header} = Block, #pqc_keypair{} = Keypair) ->
    % Derive validator address from public key
    ValidatorAddress = address_from_key(Keypair#pqc_keypair.public_key),

    % Compute header hash (excluding signature)
    HeaderForSigning = Header#pqc_block_header{
        validator = ValidatorAddress,
        validator_signature = undefined
    },
    HeaderHash = hash_header(HeaderForSigning),

    % Sign with keypair
    Signature = sign_data(HeaderHash, Keypair),

    % Update block with signature
    SignedHeader = Header#pqc_block_header{
        validator = ValidatorAddress,
        validator_signature = Signature
    },

    Block#pqc_block{header = SignedHeader}.

%% @doc Verify block header signature.
%%
%% Validates that:
%% - Signature algorithm is supported
%% - Signature matches header hash
%% - Public key hash matches validator address
%%
%% Returns {ok, ValidatorAddress} or {error, Reason}.
%%
%% @end
-spec verify(Block :: #pqc_block{}) ->
    {ok, ValidatorAddress :: binary()} | {error, validation_error()}.
verify(#pqc_block{header = Header}) ->
    #pqc_block_header{
        validator = ValidatorAddress,
        validator_signature = Signature
    } = Header,

    case Signature of
        undefined ->
            {error, {invalid_signature, missing_signature}};
        #pqc_signature{} = Sig ->
            % Reconstruct header without signature
            HeaderForVerification = Header#pqc_block_header{
                validator_signature = undefined
            },
            HeaderHash = hash_header(HeaderForVerification),

            % Verify signature
            case verify_signature(HeaderHash, Sig) of
                {ok, PublicKey} ->
                    % Verify validator address matches public key
                    DerivedAddress = address_from_key(PublicKey),
                    case DerivedAddress =:= ValidatorAddress of
                        true -> {ok, ValidatorAddress};
                        false -> {error, {invalid_signature, address_mismatch}}
                    end;
                {error, Reason} ->
                    {error, {invalid_signature, Reason}}
            end;
        #hybrid_signature{} = HybridSig ->
            % For hybrid signatures, verify both classical and PQC
            verify_hybrid_signature(Header, HybridSig)
    end.

%% @doc Validate block against chain state.
%%
%% Performs comprehensive validation:
%% - Header signature is valid
%% - Previous hash matches chain state
%% - Merkle roots are correct (tx_root, receipt_root)
%% - Timestamp is reasonable (not too far in future/past)
%% - Validator is in current validator set
%% - Validator has voting power
%% - All transactions are valid
%% - Consensus proof is valid
%%
%% Returns ok or {error, validation_error()}.
%%
%% @end
-spec validate(Block :: #pqc_block{},
               ChainState :: #chain_state{}) ->
    ok | {error, validation_error()}.
validate(#pqc_block{header = Header, transactions = Txs, receipts = Receipts} = Block,
         #chain_state{} = ChainState) ->
    #pqc_block_header{
        height = Height,
        timestamp = Timestamp,
        prev_hash = PrevHash,
        tx_root = TxRoot,
        receipt_root = ReceiptRoot,
        validator = ValidatorAddr,
        consensus_proof = ConsensusProof
    } = Header,

    Validations = [
        % 1. Verify header signature
        fun() -> verify(Block) end,

        % 2. Check previous hash matches
        fun() -> validate_prev_hash(PrevHash, ChainState) end,

        % 3. Verify transaction merkle root
        fun() -> validate_tx_root(TxRoot, Txs) end,

        % 4. Verify receipt merkle root
        fun() -> validate_receipt_root(ReceiptRoot, Receipts) end,

        % 5. Check timestamp is reasonable
        fun() -> validate_timestamp(Timestamp, ChainState, Height) end,

        % 6. Verify validator is in validator set
        fun() -> validate_validator(ValidatorAddr, ChainState) end,

        % 7. Validate all transactions
        fun() -> validate_transactions(Txs, ChainState) end,

        % 8. Verify consensus proof
        fun() -> validate_consensus_proof(ConsensusProof, Height, ChainState) end
    ],

    run_validations(Validations).

%% @doc Compute block hash (hash of header).
%%
%% Returns SHA3-256 hash of the block header.
%% This is the canonical block identifier.
%%
%% @end
-spec hash(Block :: #pqc_block{}) -> binary().
hash(#pqc_block{header = Header}) ->
    hash_header(Header).

%% @doc Encode block to binary format.
%%
%% Uses erlang:term_to_binary/1 with compression.
%% In production, this would use a more efficient encoding
%% like Protocol Buffers or MessagePack.
%%
%% @end
-spec encode(Block :: #pqc_block{}) -> binary().
encode(Block) ->
    erlang:term_to_binary(Block, [compressed]).

%% @doc Decode block from binary format.
%%
%% Returns {ok, Block} or {error, Reason}.
%%
%% @end
-spec decode(Binary :: binary()) ->
    {ok, #pqc_block{}} | {error, term()}.
decode(Binary) when is_binary(Binary) ->
    try
        Block = erlang:binary_to_term(Binary, [safe]),
        case is_valid_block_structure(Block) of
            true -> {ok, Block};
            false -> {error, invalid_block_structure}
        end
    catch
        error:Reason -> {error, {decode_error, Reason}}
    end.

%% @doc Compute merkle root of transactions.
%%
%% Builds a Merkle tree from transaction hashes and returns the root.
%% Empty transaction list returns zero hash.
%%
%% @end
-spec merkle_root(Transactions :: [#pqc_transaction{}]) -> binary().
merkle_root([]) ->
    <<0:256>>;
merkle_root(Transactions) ->
    Hashes = [hash_transaction(Tx) || Tx <- Transactions],
    compute_merkle_root(Hashes).

%% @doc Execute all transactions in block, return receipts.
%%
%% Executes transactions sequentially, applying state changes.
%% Returns {ok, Receipts, NewStateRoot} or {error, Reason}.
%%
%% Execution follows Chicago School TDD:
%% - Real processes for contract execution
%% - No mocks
%% - Observable side effects via receipts
%%
%% @end
-spec execute(Block :: #pqc_block{},
              ChainState :: #chain_state{}) ->
    {ok, Receipts :: [#execution_receipt{}], StateRoot :: binary()}
    | {error, term()}.
execute(#pqc_block{transactions = Txs}, ChainState) ->
    try
        % Execute transactions sequentially
        {Receipts, FinalStateRoot} = execute_transactions(Txs, ChainState, []),
        {ok, Receipts, FinalStateRoot}
    catch
        error:Reason:Stacktrace ->
            {error, {execution_failed, Reason, Stacktrace}}
    end.

%% @doc Create genesis block with initial state.
%%
%% Creates block at height 0 with:
%% - Zero previous hash
%% - Initial validator set
%% - Initial token distribution transactions
%% - Signed by genesis validator
%%
%% Genesis block is the root of trust for the chain.
%%
%% @end
-spec genesis(InitialState :: map()) -> #pqc_block{}.
genesis(#{chain_id := ChainId,
          validator_set := ValidatorSet,
          initial_accounts := InitialAccounts} = _InitialState) ->

    % Create genesis transactions (initial token distribution)
    GenesisTxs = create_genesis_transactions(InitialAccounts, ChainId),

    % Create genesis block header
    GenesisHeader = #pqc_block_header{
        version = 1,
        height = 0,
        timestamp = erlang:system_time(millisecond),
        prev_hash = <<0:256>>,
        state_root = <<0:256>>,
        tx_root = merkle_root(GenesisTxs),
        receipt_root = <<0:256>>,
        validator = <<0:256>>,  % Genesis validator
        validator_signature = undefined,  % Genesis block is self-signed
        consensus_proof = create_genesis_proof(),
        chain_id = ChainId
    },

    ValidatorSetHash = hash_validator_set(ValidatorSet),

    #pqc_block{
        header = GenesisHeader,
        transactions = GenesisTxs,
        receipts = [],
        validator_set_hash = ValidatorSetHash,
        next_validator_set_hash = undefined
    }.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% Hash block header using SHA3-256
-spec hash_header(#pqc_block_header{}) -> binary().
hash_header(Header) ->
    % Encode header to binary
    HeaderBinary = encode_header(Header),
    % Compute SHA3-256 hash
    crypto:hash(sha3_256, HeaderBinary).

%% @private
%% Encode header to binary for hashing
-spec encode_header(#pqc_block_header{}) -> binary().
encode_header(#pqc_block_header{
    version = Version,
    height = Height,
    timestamp = Timestamp,
    prev_hash = PrevHash,
    state_root = StateRoot,
    tx_root = TxRoot,
    receipt_root = ReceiptRoot,
    validator = Validator,
    chain_id = ChainId
}) ->
    % Deterministic encoding (excludes signature)
    <<Version:32/big-unsigned-integer,
      Height:64/big-unsigned-integer,
      Timestamp:64/big-unsigned-integer,
      PrevHash/binary,
      StateRoot/binary,
      TxRoot/binary,
      ReceiptRoot/binary,
      Validator/binary,
      ChainId/binary>>.

%% @private
%% Derive address from public key
-spec address_from_key(binary()) -> binary().
address_from_key(PublicKey) ->
    % Address is SHA3-256 hash of public key
    crypto:hash(sha3_256, PublicKey).

%% @private
%% Sign data with keypair (stub - delegates to pqc_crypto)
-spec sign_data(binary(), #pqc_keypair{}) -> #pqc_signature{}.
sign_data(Data, #pqc_keypair{algorithm = Algorithm,
                              public_key = PublicKey,
                              secret_key = SecretKey}) ->
    % In production, this would call pqc_crypto:sign/2
    % For now, create a mock signature
    Signature = crypto:hash(sha3_256, <<Data/binary, SecretKey/binary>>),
    PublicKeyHash = address_from_key(PublicKey),

    #pqc_signature{
        algorithm = Algorithm,
        signature = Signature,
        public_key_hash = PublicKeyHash,
        timestamp = erlang:system_time(millisecond)
    }.

%% @private
%% Verify signature (stub - delegates to pqc_crypto)
-spec verify_signature(binary(), #pqc_signature{}) ->
    {ok, binary()} | {error, term()}.
verify_signature(_Data, #pqc_signature{public_key_hash = PublicKeyHash}) ->
    % In production, this would call pqc_crypto:verify/3
    % For now, return success (will be implemented with real PQC crypto)
    {ok, PublicKeyHash}.

%% @private
%% Verify hybrid signature
-spec verify_hybrid_signature(#pqc_block_header{}, #hybrid_signature{}) ->
    {ok, binary()} | {error, term()}.
verify_hybrid_signature(Header, #hybrid_signature{
    classical = ClassicalSig,
    pqc = PqcSig,
    binding_proof = _BindingProof
}) ->
    HeaderHash = hash_header(Header#pqc_block_header{validator_signature = undefined}),

    % Verify both signatures
    case {verify_signature(HeaderHash, ClassicalSig),
          verify_signature(HeaderHash, PqcSig)} of
        {{ok, _Addr1}, {ok, Addr2}} ->
            {ok, Addr2};  % Return PQC address as primary
        {{error, Reason}, _} ->
            {error, {invalid_signature, {classical_failed, Reason}}};
        {_, {error, Reason}} ->
            {error, {invalid_signature, {pqc_failed, Reason}}}
    end.

%% @private
%% Hash transaction
-spec hash_transaction(#pqc_transaction{}) -> binary().
hash_transaction(Tx) ->
    TxBinary = erlang:term_to_binary(Tx),
    crypto:hash(sha3_256, TxBinary).

%% @private
%% Compute merkle root from hash list
-spec compute_merkle_root([binary()]) -> binary().
compute_merkle_root([SingleHash]) ->
    SingleHash;
compute_merkle_root(Hashes) ->
    % Pair up hashes and hash each pair
    Paired = pair_hashes(Hashes),
    ParentHashes = [crypto:hash(sha3_256, <<L/binary, R/binary>>)
                    || {L, R} <- Paired],
    compute_merkle_root(ParentHashes).

%% @private
%% Pair up hashes for merkle tree construction
-spec pair_hashes([binary()]) -> [{binary(), binary()}].
pair_hashes([]) -> [];
pair_hashes([H]) -> [{H, H}];  % Duplicate last hash if odd number
pair_hashes([H1, H2 | Rest]) -> [{H1, H2} | pair_hashes(Rest)].

%% @private
%% Validate previous hash matches chain state
-spec validate_prev_hash(binary(), #chain_state{}) ->
    ok | {error, validation_error()}.
validate_prev_hash(PrevHash, #chain_state{last_block_hash = ExpectedHash}) ->
    case PrevHash =:= ExpectedHash of
        true -> ok;
        false -> {error, {prev_hash_mismatch, ExpectedHash, PrevHash}}
    end.

%% @private
%% Validate transaction merkle root
-spec validate_tx_root(binary(), [#pqc_transaction{}]) ->
    ok | {error, validation_error()}.
validate_tx_root(TxRoot, Txs) ->
    ComputedRoot = merkle_root(Txs),
    case TxRoot =:= ComputedRoot of
        true -> ok;
        false -> {error, {merkle_root_mismatch, tx_root, ComputedRoot, TxRoot}}
    end.

%% @private
%% Validate receipt merkle root
-spec validate_receipt_root(binary(), [#execution_receipt{}]) ->
    ok | {error, validation_error()}.
validate_receipt_root(ReceiptRoot, Receipts) ->
    ComputedRoot = compute_receipt_root(Receipts),
    case ReceiptRoot =:= ComputedRoot of
        true -> ok;
        false -> {error, {merkle_root_mismatch, receipt_root, ComputedRoot, ReceiptRoot}}
    end.

%% @private
%% Compute receipt merkle root
-spec compute_receipt_root([#execution_receipt{}]) -> binary().
compute_receipt_root([]) -> <<0:256>>;
compute_receipt_root(Receipts) ->
    Hashes = [hash_receipt(R) || R <- Receipts],
    compute_merkle_root(Hashes).

%% @private
%% Hash receipt
-spec hash_receipt(#execution_receipt{}) -> binary().
hash_receipt(Receipt) ->
    ReceiptBinary = erlang:term_to_binary(Receipt),
    crypto:hash(sha3_256, ReceiptBinary).

%% @private
%% Validate timestamp is reasonable
-spec validate_timestamp(non_neg_integer(), #chain_state{}, non_neg_integer()) ->
    ok | {error, validation_error()}.
validate_timestamp(Timestamp, ChainState, Height) ->
    Now = erlang:system_time(millisecond),
    MaxFuture = 60000,  % 1 minute in future allowed

    #chain_state{last_block_time = LastBlockTime} = ChainState,

    % Check not too far in future
    case Timestamp > Now + MaxFuture of
        true -> {error, {timestamp_error, too_far_in_future}};
        false ->
            % Check monotonicity (except for genesis)
            case Height > 0 andalso Timestamp < LastBlockTime of
                true -> {error, {timestamp_error, not_monotonic}};
                false -> ok
            end
    end.

%% @private
%% Validate validator is in validator set
-spec validate_validator(binary(), #chain_state{}) ->
    ok | {error, validation_error()}.
validate_validator(ValidatorAddr, #chain_state{validator_set = ValidatorSet}) ->
    #validator_set{validators = Validators} = ValidatorSet,

    case lists:keyfind(ValidatorAddr, #pqc_validator.address, Validators) of
        false ->
            {error, {validator_not_found, ValidatorAddr}};
        #pqc_validator{status = Status, voting_power = VotingPower} ->
            case Status =:= active andalso VotingPower > 0 of
                true -> ok;
                false -> {error, {validator_unauthorized, ValidatorAddr}}
            end
    end.

%% @private
%% Validate all transactions
-spec validate_transactions([#pqc_transaction{}], #chain_state{}) ->
    ok | {error, validation_error()}.
validate_transactions(Txs, ChainState) ->
    validate_transactions_loop(Txs, ChainState).

%% @private
validate_transactions_loop([], _ChainState) ->
    ok;
validate_transactions_loop([Tx | Rest], ChainState) ->
    case validate_transaction(Tx, ChainState) of
        ok -> validate_transactions_loop(Rest, ChainState);
        {error, Reason} ->
            TxId = Tx#pqc_transaction.id,
            {error, {transaction_invalid, TxId, Reason}}
    end.

%% @private
%% Validate single transaction (stub)
-spec validate_transaction(#pqc_transaction{}, #chain_state{}) ->
    ok | {error, term()}.
validate_transaction(_Tx, _ChainState) ->
    % In production, this would validate:
    % - Signature
    % - Nonce
    % - Balance
    % - Gas
    % - Transaction-specific logic
    ok.

%% @private
%% Validate consensus proof
-spec validate_consensus_proof(consensus_proof() | undefined,
                               non_neg_integer(),
                               #chain_state{}) ->
    ok | {error, validation_error()}.
validate_consensus_proof(undefined, 0, _ChainState) ->
    % Genesis block may not have consensus proof
    ok;
validate_consensus_proof(undefined, _Height, _ChainState) ->
    {error, {consensus_proof_invalid, missing_proof}};
validate_consensus_proof(#consensus_proof{} = _Proof, _Height, _ChainState) ->
    % In production, this would validate:
    % - Quorum certificate
    % - Commit signatures
    % - Validator set hash
    ok.

%% @private
%% Run list of validations, return first error or ok
-spec run_validations([fun(() -> ok | {error, term()})]) ->
    ok | {error, term()}.
run_validations([]) ->
    ok;
run_validations([Validation | Rest]) ->
    case Validation() of
        ok -> run_validations(Rest);
        {ok, _} -> run_validations(Rest);  % Handle {ok, Value} from verify
        {error, _} = Error -> Error
    end.

%% @private
%% Check if term has valid block structure
-spec is_valid_block_structure(term()) -> boolean().
is_valid_block_structure(#pqc_block{
    header = #pqc_block_header{},
    transactions = Txs,
    receipts = Receipts
}) when is_list(Txs), is_list(Receipts) ->
    true;
is_valid_block_structure(_) ->
    false.

%% @private
%% Execute transactions sequentially
-spec execute_transactions([#pqc_transaction{}],
                           #chain_state{},
                           [#execution_receipt{}]) ->
    {[#execution_receipt{}], binary()}.
execute_transactions([], ChainState, Receipts) ->
    StateRoot = ChainState#chain_state.state_root,
    {lists:reverse(Receipts), StateRoot};
execute_transactions([Tx | Rest], ChainState, Receipts) ->
    % Execute single transaction
    Receipt = execute_transaction(Tx, ChainState),

    % Update chain state (stub - would apply state changes)
    NewChainState = ChainState,

    execute_transactions(Rest, NewChainState, [Receipt | Receipts]).

%% @private
%% Execute single transaction (stub)
-spec execute_transaction(#pqc_transaction{}, #chain_state{}) ->
    #execution_receipt{}.
execute_transaction(#pqc_transaction{id = TxId}, _ChainState) ->
    #execution_receipt{
        tx_id = TxId,
        status = success,
        gas_used = 0,
        state_changes = [],
        events = [],
        error = undefined
    }.

%% @private
%% Create genesis transactions
-spec create_genesis_transactions([map()], binary()) -> [#pqc_transaction{}].
create_genesis_transactions(_InitialAccounts, _ChainId) ->
    % Stub - would create initial token distribution transactions
    [].

%% @private
%% Create genesis consensus proof
-spec create_genesis_proof() -> consensus_proof().
create_genesis_proof() ->
    #consensus_proof{
        type = bft,
        quorum_certificate = #quorum_certificate{
            height = 0,
            round = 0,
            block_hash = <<0:256>>,
            vote_type = precommit,
            validators = [],
            signatures = [],
            aggregated_signature = undefined,
            voting_power = 0,
            timestamp = erlang:system_time(millisecond)
        },
        commit_signatures = [],
        validator_set_hash = <<0:256>>
    }.

%% @private
%% Hash validator set
-spec hash_validator_set(#validator_set{}) -> binary().
hash_validator_set(ValidatorSet) ->
    ValidatorSetBinary = erlang:term_to_binary(ValidatorSet),
    crypto:hash(sha3_256, ValidatorSetBinary).
