%%%-------------------------------------------------------------------
%%% @doc SwarmFlow PQChain Public API
%%%
%%% Convenience API facade for SwarmFlow PQChain operations.
%%% Provides high-level functions for blockchain interaction.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(swarmflow_pqchain).

-include("pqchain.hrl").

%% Application control
-export([start/0, stop/0]).

%% Identity management
-export([create_identity/1,
         get_identity/1,
         rotate_key/3]).

%% Transaction operations
-export([send_transaction/1,
         get_transaction/1,
         get_transaction_receipt/1]).

%% Block queries
-export([get_block/1,
         get_block_by_hash/1,
         get_latest_block/0,
         get_block_height/0]).

%% Account queries
-export([get_balance/1,
         get_nonce/1,
         get_account_state/1]).

%% Smart contract operations
-export([deploy_contract/3,
         call_contract/4,
         get_contract_state/1]).

%% Crypto policy operations
-export([propose_policy/1,
         get_active_policy/0,
         get_policy/1]).

%% Chain state
-export([get_chain_state/0,
         get_validator_set/0]).

%% Peer network
-export([connect_peer/2,
         disconnect_peer/1,
         list_peers/0]).

%%====================================================================
%% Application Control
%%====================================================================

%% @doc Start the SwarmFlow PQChain application
-spec start() -> ok | {error, term()}.
start() ->
    case application:ensure_all_started(swarmflow_pqchain) of
        {ok, _Started} ->
            ok;
        {error, _Reason} = Error ->
            Error
    end.

%% @doc Stop the SwarmFlow PQChain application
-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(swarmflow_pqchain).

%%====================================================================
%% Identity Management
%%====================================================================

%% @doc Create a new PQC identity
%%
%% Options:
%%   - algorithm: Signature algorithm (default: ml_dsa_65)
%%   - backup_algorithm: Backup key algorithm (default: slh_dsa_128s)
%%   - kem_algorithm: KEM algorithm (default: ml_kem_768)
%%   - metadata: Additional metadata
%%
-spec create_identity(map()) -> {ok, #pqc_identity{}} | {error, term()}.
create_identity(Options) when is_map(Options) ->
    pqc_identity:create(Options).

%% @doc Get identity by address
-spec get_identity(binary()) -> {ok, #pqc_identity{}} | {error, not_found}.
get_identity(Address) when is_binary(Address) ->
    pqc_identity:get(Address).

%% @doc Rotate identity key
-spec rotate_key(binary(), #pqc_keypair{}, non_neg_integer()) ->
    {ok, #key_rotation{}} | {error, term()}.
rotate_key(Address, NewKey, EffectiveHeight)
  when is_binary(Address),
       is_record(NewKey, pqc_keypair),
       is_integer(EffectiveHeight) ->
    pqc_identity:rotate_key(Address, NewKey, EffectiveHeight).

%%====================================================================
%% Transaction Operations
%%====================================================================

%% @doc Send a transaction to the network
-spec send_transaction(#pqc_transaction{}) -> {ok, binary()} | {error, term()}.
send_transaction(Transaction) when is_record(Transaction, pqc_transaction) ->
    case whereis(pqc_mempool) of
        undefined ->
            {error, mempool_not_available};
        _Pid ->
            pqc_mempool:add_transaction(Transaction)
    end.

%% @doc Get transaction by ID
-spec get_transaction(binary()) -> {ok, #pqc_transaction{}} | {error, not_found}.
get_transaction(TxId) when is_binary(TxId) ->
    pqc_transaction:get(TxId).

%% @doc Get transaction execution receipt
-spec get_transaction_receipt(binary()) ->
    {ok, #execution_receipt{}} | {error, not_found}.
get_transaction_receipt(TxId) when is_binary(TxId) ->
    pqc_block:get_receipt(TxId).

%%====================================================================
%% Block Queries
%%====================================================================

%% @doc Get block by height
-spec get_block(non_neg_integer()) -> {ok, #pqc_block{}} | {error, not_found}.
get_block(Height) when is_integer(Height), Height >= 0 ->
    pqc_block:get_by_height(Height).

%% @doc Get block by hash
-spec get_block_by_hash(binary()) -> {ok, #pqc_block{}} | {error, not_found}.
get_block_by_hash(Hash) when is_binary(Hash) ->
    pqc_block:get_by_hash(Hash).

%% @doc Get the latest block
-spec get_latest_block() -> {ok, #pqc_block{}} | {error, not_found}.
get_latest_block() ->
    case get_chain_state() of
        {ok, #chain_state{height = Height}} ->
            get_block(Height);
        Error ->
            Error
    end.

%% @doc Get current block height
-spec get_block_height() -> {ok, non_neg_integer()} | {error, term()}.
get_block_height() ->
    case get_chain_state() of
        {ok, #chain_state{height = Height}} ->
            {ok, Height};
        Error ->
            Error
    end.

%%====================================================================
%% Account Queries
%%====================================================================

%% @doc Get account balance
-spec get_balance(binary()) -> {ok, non_neg_integer()} | {error, term()}.
get_balance(Address) when is_binary(Address) ->
    case whereis(pqc_chain) of
        undefined ->
            {error, chain_not_available};
        _Pid ->
            pqc_chain:get_balance(Address)
    end.

%% @doc Get account nonce (transaction count)
-spec get_nonce(binary()) -> {ok, non_neg_integer()} | {error, term()}.
get_nonce(Address) when is_binary(Address) ->
    case whereis(pqc_chain) of
        undefined ->
            {error, chain_not_available};
        _Pid ->
            pqc_chain:get_nonce(Address)
    end.

%% @doc Get full account state
-spec get_account_state(binary()) -> {ok, map()} | {error, term()}.
get_account_state(Address) when is_binary(Address) ->
    case whereis(pqc_chain) of
        undefined ->
            {error, chain_not_available};
        _Pid ->
            pqc_chain:get_account_state(Address)
    end.

%%====================================================================
%% Smart Contract Operations
%%====================================================================

%% @doc Deploy a workflow net smart contract
%%
%% Args:
%%   WorkflowNet: SwarmFlow workflow net definition
%%   InitialState: Initial contract state (marking, variables)
%%   Options: Deployment options (gas_limit, fee, etc.)
%%
-spec deploy_contract(term(), map(), map()) ->
    {ok, binary()} | {error, term()}.
deploy_contract(WorkflowNet, InitialState, Options)
  when is_map(InitialState), is_map(Options) ->
    Payload = #contract_deploy_payload{
        workflow_net = WorkflowNet,
        initial_state = InitialState,
        constructor_args = maps:get(constructor_args, Options, [])
    },

    Sender = maps:get(sender, Options),
    Nonce = maps:get(nonce, Options),
    Fee = maps:get(fee, Options),
    GasLimit = maps:get(gas_limit, Options),
    GasPrice = maps:get(gas_price, Options),

    Transaction = #pqc_transaction{
        id = generate_tx_id(),
        type = ?TX_TYPE_CONTRACT_DEPLOY,
        version = 1,
        chain_id = get_chain_id(),
        nonce = Nonce,
        sender = Sender,
        recipient = undefined,
        amount = 0,
        fee = Fee,
        gas_limit = GasLimit,
        gas_price = GasPrice,
        payload = Payload,
        signature = maps:get(signature, Options),
        created_at = erlang:system_time(millisecond),
        expires_at = maps:get(expires_at, Options, undefined)
    },

    case send_transaction(Transaction) of
        {ok, TxId} ->
            {ok, TxId};
        Error ->
            Error
    end.

%% @doc Call a smart contract (fire workflow transition)
%%
%% Args:
%%   ContractAddress: Contract blockchain address
%%   Transition: Workflow transition to fire
%%   Arguments: Transition arguments
%%   Options: Call options (gas_limit, fee, etc.)
%%
-spec call_contract(binary(), atom(), [term()], map()) ->
    {ok, binary()} | {error, term()}.
call_contract(ContractAddress, Transition, Arguments, Options)
  when is_binary(ContractAddress),
       is_atom(Transition),
       is_list(Arguments),
       is_map(Options) ->
    Payload = #contract_call_payload{
        contract_address = ContractAddress,
        transition = Transition,
        arguments = Arguments,
        artifacts = maps:get(artifacts, Options, [])
    },

    Sender = maps:get(sender, Options),
    Nonce = maps:get(nonce, Options),
    Fee = maps:get(fee, Options),
    GasLimit = maps:get(gas_limit, Options),
    GasPrice = maps:get(gas_price, Options),

    Transaction = #pqc_transaction{
        id = generate_tx_id(),
        type = ?TX_TYPE_CONTRACT_CALL,
        version = 1,
        chain_id = get_chain_id(),
        nonce = Nonce,
        sender = Sender,
        recipient = ContractAddress,
        amount = maps:get(amount, Options, 0),
        fee = Fee,
        gas_limit = GasLimit,
        gas_price = GasPrice,
        payload = Payload,
        signature = maps:get(signature, Options),
        created_at = erlang:system_time(millisecond),
        expires_at = maps:get(expires_at, Options, undefined)
    },

    case send_transaction(Transaction) of
        {ok, TxId} ->
            {ok, TxId};
        Error ->
            Error
    end.

%% @doc Get smart contract state
-spec get_contract_state(binary()) -> {ok, #contract_instance{}} | {error, term()}.
get_contract_state(ContractAddress) when is_binary(ContractAddress) ->
    case whereis(pqc_chain) of
        undefined ->
            {error, chain_not_available};
        _Pid ->
            pqc_chain:get_contract_state(ContractAddress)
    end.

%%====================================================================
%% Crypto Policy Operations
%%====================================================================

%% @doc Propose a new crypto policy
-spec propose_policy(#crypto_policy{}) -> {ok, binary()} | {error, term()}.
propose_policy(Policy) when is_record(Policy, crypto_policy) ->
    case whereis(pqc_crypto_policy) of
        undefined ->
            {error, crypto_policy_not_available};
        _Pid ->
            pqc_crypto_policy:propose(Policy)
    end.

%% @doc Get the currently active crypto policy
-spec get_active_policy() -> {ok, #crypto_policy{}} | {error, term()}.
get_active_policy() ->
    case whereis(pqc_crypto_policy) of
        undefined ->
            {error, crypto_policy_not_available};
        _Pid ->
            pqc_crypto_policy:get_active()
    end.

%% @doc Get a specific crypto policy by ID
-spec get_policy(binary()) -> {ok, #crypto_policy{}} | {error, not_found}.
get_policy(PolicyId) when is_binary(PolicyId) ->
    case whereis(pqc_crypto_policy) of
        undefined ->
            {error, crypto_policy_not_available};
        _Pid ->
            pqc_crypto_policy:get(PolicyId)
    end.

%%====================================================================
%% Chain State
%%====================================================================

%% @doc Get current chain state
-spec get_chain_state() -> {ok, #chain_state{}} | {error, term()}.
get_chain_state() ->
    case whereis(pqc_chain) of
        undefined ->
            {error, chain_not_available};
        _Pid ->
            pqc_chain:get_state()
    end.

%% @doc Get current validator set
-spec get_validator_set() -> {ok, #validator_set{}} | {error, term()}.
get_validator_set() ->
    case get_chain_state() of
        {ok, #chain_state{validator_set = ValidatorSet}} ->
            {ok, ValidatorSet};
        Error ->
            Error
    end.

%%====================================================================
%% Peer Network
%%====================================================================

%% @doc Connect to a peer
-spec connect_peer(#peer_identity{}, map()) -> {ok, pid()} | {error, term()}.
connect_peer(RemotePeer, Options)
  when is_record(RemotePeer, peer_identity), is_map(Options) ->
    case whereis(pqc_peer_sup) of
        undefined ->
            {error, peer_supervisor_not_available};
        _Pid ->
            %% Get local peer identity
            case get_local_peer_identity() of
                {ok, LocalPeer} ->
                    pqc_peer_sup:start_channel(LocalPeer, RemotePeer, Options);
                Error ->
                    Error
            end
    end.

%% @doc Disconnect from a peer
-spec disconnect_peer(binary()) -> ok | {error, term()}.
disconnect_peer(ChannelId) when is_binary(ChannelId) ->
    case whereis(pqc_peer_sup) of
        undefined ->
            {error, peer_supervisor_not_available};
        _Pid ->
            pqc_peer_sup:stop_channel(ChannelId)
    end.

%% @doc List all connected peers
-spec list_peers() -> [pid()].
list_peers() ->
    case whereis(pqc_peer_sup) of
        undefined ->
            [];
        _Pid ->
            pqc_peer_sup:list_channels()
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
generate_tx_id() ->
    %% Generate ULID for transaction ID
    ulid:generate().

%% @private
get_chain_id() ->
    case application:get_env(swarmflow_pqchain, chain_id) of
        {ok, ChainId} ->
            ChainId;
        undefined ->
            <<"pqchain-default">>
    end.

%% @private
get_local_peer_identity() ->
    %% This would retrieve the node's peer identity from configuration or pqc_identity
    case application:get_env(swarmflow_pqchain, local_peer_identity) of
        {ok, PeerIdentity} ->
            {ok, PeerIdentity};
        undefined ->
            {error, local_peer_identity_not_configured}
    end.
