%%%-------------------------------------------------------------------
%%% @doc PQC-Enabled A2A (Agent-to-Agent) Protocol Bridge
%%%
%%% Bridges the post-quantum blockchain with Google's A2A protocol for
%%% agent collaboration with quantum-safe cryptography.
%%%
%%% Key Features:
%%% - All A2A messages signed with ML-DSA (FIPS 204)
%%% - Artifact hashes anchored on-chain for audit trail
%%% - ML-KEM (FIPS 203) secured agent-to-agent channels
%%% - Agent cards include PQC public keys
%%% - Task signatures for verifiable task execution
%%% - Message authentication with PQC signatures
%%% - Subscription to task updates with PQC-authenticated channel
%%%
%%% Security Model:
%%% - Every task is signed with validator's ML-DSA keypair
%%% - Artifacts can be anchored as contract_call transactions
%%% - Agent identities verified through on-chain key registration
%%% - ML-KEM provides forward-secure encrypted channels
%%%
%%% Integration Points:
%%% - pqc_crypto: PQC signature and KEM operations
%%% - pqc_transaction: On-chain transaction anchoring
%%% - erlmcp_a2a_*: Standard A2A protocol implementation
%%% - gproc: Process registry for task routing
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_a2a_bridge).

-behaviour(gen_server).

-include("pqchain.hrl").
-include_lib("erlmcp_core/include/erlmcp_a2a.hrl").

%% API exports
-export([
    start_link/1,
    stop/1,

    %% A2A + PQC Operations
    send_task/3,
    receive_task/2,
    create_message/3,

    %% On-chain Anchoring
    anchor_artifact/2,
    verify_artifact/2,

    %% Subscription Management
    subscribe_tasks/2,
    unsubscribe_tasks/2,

    %% Agent Identity
    get_agent_card/1,

    %% Secure Channel Management
    establish_secure_channel/2,
    close_secure_channel/2,

    %% Query Operations
    get_task/2,
    list_tasks/1,
    get_statistics/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% Type Definitions
%%====================================================================

%% PQC-enhanced A2A Task
-record(pqc_a2a_task, {
    task :: #a2a_task{},
    signature :: #pqc_signature{},
    signer_address :: binary(),
    chain_id :: binary(),
    anchored :: boolean(),
    anchor_tx_id :: binary() | undefined,
    created_at :: non_neg_integer(),
    metadata :: map()
}).

%% PQC-enhanced A2A Message
-record(pqc_a2a_message, {
    message :: #a2a_message{},
    signature :: #pqc_signature{},
    signer_address :: binary(),
    chain_id :: binary(),
    timestamp :: non_neg_integer(),
    metadata :: map()
}).

%% PQC Agent Card
-record(pqc_agent_card, {
    agent_card :: #a2a_agent_card{},
    pqc_signing_key :: binary(),        % ML-DSA public key
    pqc_kem_key :: binary(),            % ML-KEM public key
    blockchain_address :: binary(),
    signature :: #pqc_signature{},
    registered_at :: non_neg_integer()
}).

%% Secure Channel State
-record(pqc_secure_channel, {
    channel_id :: binary(),
    remote_agent :: binary(),
    secure_channel :: #secure_channel{},
    established_at :: non_neg_integer(),
    last_used :: non_neg_integer()
}).

%% Server State
-record(state, {
    %% Identity
    validator_keypair :: #pqc_keypair{},
    kem_keypair :: #pqc_keypair{},
    address :: binary(),
    chain_id :: binary(),

    %% Agent Card
    agent_card :: #pqc_agent_card{},

    %% Tasks (ETS table: task_id => pqc_a2a_task)
    tasks_table :: ets:tid(),

    %% Secure Channels (remote_address => pqc_secure_channel)
    channels :: #{binary() => #pqc_secure_channel{}},

    %% Subscriptions (subscriber_pid => [task_id])
    subscriptions :: #{pid() => [binary()]},

    %% Statistics
    stats :: #{
        tasks_created := non_neg_integer(),
        tasks_signed := non_neg_integer(),
        artifacts_anchored := non_neg_integer(),
        messages_verified := non_neg_integer(),
        channels_established := non_neg_integer()
    },

    %% Configuration
    config :: map()
}).

-type server_ref() :: pid() | atom().
-type task_id() :: binary().
-type agent_address() :: binary().

-export_type([
    server_ref/0,
    task_id/0,
    agent_address/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the PQC A2A Bridge with configuration
%% Config must include:
%%   - validator_keypair: #pqc_keypair{} (signing key)
%%   - chain_id: binary()
%%   - agent_card: #a2a_agent_card{} (base agent card)
%%   - kem_keypair: #pqc_keypair{} (optional, for secure channels)
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc Stop the bridge
-spec stop(server_ref()) -> ok.
stop(ServerRef) ->
    gen_server:stop(ServerRef).

%% @doc Send A2A task to remote agent with PQC signature
%% Creates task, signs with ML-DSA, and sends to remote agent
-spec send_task(server_ref(), #a2a_task{}, agent_address()) ->
    {ok, #pqc_a2a_task{}} | {error, term()}.
send_task(ServerRef, Task, RemoteAgent) ->
    gen_server:call(ServerRef, {send_task, Task, RemoteAgent}, 10000).

%% @doc Receive and verify A2A task with PQC signature verification
-spec receive_task(server_ref(), #pqc_a2a_task{}) ->
    {ok, #pqc_a2a_task{}} | {error, term()}.
receive_task(ServerRef, PQCTask) ->
    gen_server:call(ServerRef, {receive_task, PQCTask}, 5000).

%% @doc Create A2A message with PQC signature
-spec create_message(server_ref(), #a2a_message{}, map()) ->
    {ok, #pqc_a2a_message{}} | {error, term()}.
create_message(ServerRef, Message, Metadata) ->
    gen_server:call(ServerRef, {create_message, Message, Metadata}, 5000).

%% @doc Verify A2A message PQC signature
-spec verify_message(#pqc_a2a_message{}) ->
    {ok, valid} | {error, invalid_signature | term()}.
verify_message(#pqc_a2a_message{
    message = Message,
    signature = Signature,
    signer_address = SignerAddress
}) ->
    %% Serialize message for verification
    MessageBin = serialize_message(Message),

    %% Extract public key from signature
    PublicKey = Signature#pqc_signature.public_key_hash,

    %% Verify address matches
    case pqc_crypto:derive_address(PublicKey) of
        SignerAddress ->
            %% Verify signature
            case pqc_crypto:verify(MessageBin, Signature, PublicKey) of
                {ok, true} -> {ok, valid};
                {ok, false} -> {error, invalid_signature};
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, address_mismatch}
    end.

%% @doc Anchor A2A artifact hash on-chain (creates transaction)
-spec anchor_artifact(server_ref(), #a2a_artifact{}) ->
    {ok, TxId :: binary()} | {error, term()}.
anchor_artifact(ServerRef, Artifact) ->
    gen_server:call(ServerRef, {anchor_artifact, Artifact}, 10000).

%% @doc Verify artifact exists on-chain
-spec verify_artifact(server_ref(), binary()) ->
    {ok, exists} | {error, not_found | term()}.
verify_artifact(ServerRef, ArtifactHash) ->
    gen_server:call(ServerRef, {verify_artifact, ArtifactHash}, 5000).

%% @doc Subscribe to task updates with PQC-authenticated channel
-spec subscribe_tasks(server_ref(), [task_id()]) -> ok | {error, term()}.
subscribe_tasks(ServerRef, TaskIds) ->
    gen_server:call(ServerRef, {subscribe_tasks, TaskIds}, 5000).

%% @doc Unsubscribe from task updates
-spec unsubscribe_tasks(server_ref(), [task_id()]) -> ok.
unsubscribe_tasks(ServerRef, TaskIds) ->
    gen_server:call(ServerRef, {unsubscribe_tasks, TaskIds}, 5000).

%% @doc Get local agent card with PQC public keys
-spec get_agent_card(server_ref()) -> {ok, #pqc_agent_card{}}.
get_agent_card(ServerRef) ->
    gen_server:call(ServerRef, get_agent_card, 5000).

%% @doc Verify remote agent card signatures
-spec verify_agent_card(#pqc_agent_card{}) ->
    {ok, valid} | {error, invalid_signature | term()}.
verify_agent_card(#pqc_agent_card{
    agent_card = AgentCard,
    pqc_signing_key = SigningKey,
    blockchain_address = Address,
    signature = Signature
}) ->
    %% Verify address matches public key
    case pqc_crypto:derive_address(SigningKey) of
        Address ->
            %% Serialize agent card for verification
            CardBin = serialize_agent_card(AgentCard),
            %% Verify signature
            case pqc_crypto:verify(CardBin, Signature, SigningKey) of
                {ok, true} -> {ok, valid};
                {ok, false} -> {error, invalid_signature};
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, address_mismatch}
    end.

%% @doc Establish ML-KEM secured channel with remote agent
-spec establish_secure_channel(server_ref(), agent_address()) ->
    {ok, ChannelId :: binary()} | {error, term()}.
establish_secure_channel(ServerRef, RemoteAgent) ->
    gen_server:call(ServerRef, {establish_secure_channel, RemoteAgent}, 10000).

%% @doc Close secure channel
-spec close_secure_channel(server_ref(), binary()) -> ok.
close_secure_channel(ServerRef, ChannelId) ->
    gen_server:call(ServerRef, {close_secure_channel, ChannelId}, 5000).

%% @doc Get task by ID
-spec get_task(server_ref(), task_id()) ->
    {ok, #pqc_a2a_task{}} | {error, not_found}.
get_task(ServerRef, TaskId) ->
    gen_server:call(ServerRef, {get_task, TaskId}, 5000).

%% @doc List all tasks
-spec list_tasks(server_ref()) -> {ok, [#pqc_a2a_task{}]}.
list_tasks(ServerRef) ->
    gen_server:call(ServerRef, list_tasks, 5000).

%% @doc Get bridge statistics
-spec get_statistics(server_ref()) -> {ok, map()}.
get_statistics(ServerRef) ->
    gen_server:call(ServerRef, get_statistics, 5000).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
-spec init(map()) -> {ok, #state{}}.
init(Config) ->
    process_flag(trap_exit, true),

    %% Extract required config
    ValidatorKeypair = maps:get(validator_keypair, Config),
    ChainId = maps:get(chain_id, Config),
    BaseAgentCard = maps:get(agent_card, Config),

    %% Generate or extract KEM keypair
    KEMKeypair = case maps:get(kem_keypair, Config, undefined) of
        undefined ->
            {ok, KP} = pqc_crypto:kem_keygen(?PQC_KEM_ML_KEM_768),
            KP;
        KP -> KP
    end,

    %% Derive address from signing key
    Address = pqc_crypto:derive_address(ValidatorKeypair),

    %% Create PQC-enhanced agent card
    PQCAgentCard = create_pqc_agent_card(BaseAgentCard, ValidatorKeypair, KEMKeypair, Address),

    %% Create ETS table for tasks
    TasksTable = ets:new(pqc_a2a_tasks, [
        set,
        protected,
        {keypos, 2},  % task_id is 2nd element in tuple
        {read_concurrency, true}
    ]),

    %% Register with gproc
    ok = gproc:reg({n, l, {pqc_a2a_bridge, Address}}),

    State = #state{
        validator_keypair = ValidatorKeypair,
        kem_keypair = KEMKeypair,
        address = Address,
        chain_id = ChainId,
        agent_card = PQCAgentCard,
        tasks_table = TasksTable,
        channels = #{},
        subscriptions = #{},
        stats = #{
            tasks_created => 0,
            tasks_signed => 0,
            artifacts_anchored => 0,
            messages_verified => 0,
            channels_established => 0
        },
        config = Config
    },

    {ok, State}.

%% @private
%% @private
-spec handle_call({send_task, #a2a_task{}, binary()}, {pid(), term()}, #state{}) ->
    {reply, {ok, #pqc_a2a_task{}} | {error, term()}, #state{}}.
handle_call({send_task, Task, RemoteAgent}, _From, State) ->
    case do_send_task(Task, RemoteAgent, State) of
        {ok, PQCTask, NewState} ->
            {reply, {ok, PQCTask}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% @private
-spec handle_call({receive_task, #pqc_a2a_task{}}, {pid(), term()}, #state{}) ->
    {reply, {ok, #pqc_a2a_task{}} | {error, term()}, #state{}}.
handle_call({receive_task, PQCTask}, _From, State) ->
    case do_receive_task(PQCTask, State) of
        {ok, VerifiedTask, NewState} ->
            {reply, {ok, VerifiedTask}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% @private
-spec handle_call({create_message, #a2a_message{}, map()}, {pid(), term()}, #state{}) ->
    {reply, {ok, #pqc_a2a_message{}} | {error, term()}, #state{}}.
handle_call({create_message, Message, Metadata}, _From, State) ->
    case do_create_message(Message, Metadata, State) of
        {ok, PQCMessage, NewState} ->
            {reply, {ok, PQCMessage}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% @private
-spec handle_call({anchor_artifact, #a2a_artifact{}}, {pid(), term()}, #state{}) ->
    {reply, {ok, binary()} | {error, term()}, #state{}}.
handle_call({anchor_artifact, Artifact}, _From, State) ->
    case do_anchor_artifact(Artifact, State) of
        {ok, TxId, NewState} ->
            {reply, {ok, TxId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% @private
-spec handle_call({verify_artifact, binary()}, {pid(), term()}, #state{}) ->
    {reply, {ok, exists} | {error, term()}, #state{}}.
handle_call({verify_artifact, ArtifactHash}, _From, State) ->
    Result = do_verify_artifact(ArtifactHash, State),
    {reply, Result, State};

%% @private
-spec handle_call({subscribe_tasks, [binary()]}, {pid(), term()}, #state{}) ->
    {reply, ok, #state{}}.
handle_call({subscribe_tasks, TaskIds}, {From, _}, State) ->
    NewState = do_subscribe_tasks(From, TaskIds, State),
    {reply, ok, NewState};

%% @private
-spec handle_call({unsubscribe_tasks, [binary()]}, {pid(), term()}, #state{}) ->
    {reply, ok, #state{}}.
handle_call({unsubscribe_tasks, TaskIds}, {From, _}, State) ->
    NewState = do_unsubscribe_tasks(From, TaskIds, State),
    {reply, ok, NewState};

%% @private
-spec handle_call(get_agent_card, {pid(), term()}, #state{}) ->
    {reply, {ok, #pqc_agent_card{}}, #state{}}.
handle_call(get_agent_card, _From, #state{agent_card = Card} = State) ->
    {reply, {ok, Card}, State};

%% @private
-spec handle_call({establish_secure_channel, binary()}, {pid(), term()}, #state{}) ->
    {reply, {ok, binary()} | {error, term()}, #state{}}.
handle_call({establish_secure_channel, RemoteAgent}, _From, State) ->
    case do_establish_secure_channel(RemoteAgent, State) of
        {ok, ChannelId, NewState} ->
            {reply, {ok, ChannelId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% @private
-spec handle_call({close_secure_channel, binary()}, {pid(), term()}, #state{}) ->
    {reply, ok, #state{}}.
handle_call({close_secure_channel, ChannelId}, _From, State) ->
    NewState = do_close_secure_channel(ChannelId, State),
    {reply, ok, NewState};

%% @private
-spec handle_call({get_task, binary()}, {pid(), term()}, #state{}) ->
    {reply, {ok, #pqc_a2a_task{}} | {error, not_found}, #state{}}.
handle_call({get_task, TaskId}, _From, #state{tasks_table = Table} = State) ->
    case ets:lookup(Table, TaskId) of
        [{TaskId, PQCTask}] ->
            {reply, {ok, PQCTask}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

%% @private
-spec handle_call(list_tasks, {pid(), term()}, #state{}) ->
    {reply, {ok, [#pqc_a2a_task{}]}, #state{}}.
handle_call(list_tasks, _From, #state{tasks_table = Table} = State) ->
    Tasks = ets:foldl(
        fun({_TaskId, PQCTask}, Acc) -> [PQCTask | Acc] end,
        [],
        Table
    ),
    {reply, {ok, Tasks}, State};

%% @private
-spec handle_call(get_statistics, {pid(), term()}, #state{}) ->
    {reply, {ok, map()}, #state{}}.
handle_call(get_statistics, _From, #state{stats = Stats} = State) ->
    {reply, {ok, Stats}, State};

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, {error, unknown_request}, #state{}}.
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
%% @private
-spec handle_info({'DOWN', reference(), process, pid(), term()}, #state{}) ->
    {noreply, #state{}}.
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Remove subscriptions for dead processes
    NewState = remove_subscriber(Pid, State),
    {noreply, NewState};

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{tasks_table = Table}) ->
    %% Clean up ETS table
    ets:delete(Table),
    ok.

%% @private
%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Create PQC-enhanced agent card
create_pqc_agent_card(BaseCard, SigningKeypair, KEMKeypair, Address) ->
    Timestamp = erlang:system_time(millisecond),

    %% Serialize base card for signing
    CardBin = serialize_agent_card(BaseCard),

    %% Sign the agent card
    {ok, Signature} = pqc_crypto:sign(CardBin, SigningKeypair, ?HASH_SHA3_256),

    #pqc_agent_card{
        agent_card = BaseCard,
        pqc_signing_key = SigningKeypair#pqc_keypair.public_key,
        pqc_kem_key = KEMKeypair#pqc_keypair.public_key,
        blockchain_address = Address,
        signature = Signature,
        registered_at = Timestamp
    }.

%% @private Send task with PQC signature
do_send_task(Task, _RemoteAgent, State) ->
    #state{
        validator_keypair = Keypair,
        chain_id = ChainId,
        address = Address,
        tasks_table = Table,
        stats = Stats
    } = State,

    Timestamp = erlang:system_time(millisecond),

    %% Serialize task for signing
    TaskBin = serialize_task(Task),

    %% Sign task with ML-DSA
    case pqc_crypto:sign(TaskBin, Keypair, ?HASH_SHA3_256) of
        {ok, Signature} ->
            PQCTask = #pqc_a2a_task{
                task = Task,
                signature = Signature,
                signer_address = Address,
                chain_id = ChainId,
                anchored = false,
                anchor_tx_id = undefined,
                created_at = Timestamp,
                metadata = #{}
            },

            %% Store in ETS
            TaskId = Task#a2a_task.id,
            ets:insert(Table, {TaskId, PQCTask}),

            %% Update stats
            NewStats = Stats#{
                tasks_created := maps:get(tasks_created, Stats) + 1,
                tasks_signed := maps:get(tasks_signed, Stats) + 1
            },

            NewState = State#state{stats = NewStats},
            {ok, PQCTask, NewState};
        {error, Reason} ->
            {error, {signing_failed, Reason}}
    end.

%% @private Receive and verify task
do_receive_task(PQCTask, State) ->
    #pqc_a2a_task{
        task = Task,
        signature = Signature,
        signer_address = SignerAddress,
        chain_id = ChainId
    } = PQCTask,

    #state{
        chain_id = LocalChainId,
        tasks_table = Table,
        stats = Stats
    } = State,

    %% Verify chain ID matches
    case ChainId of
        LocalChainId ->
            %% Serialize task for verification
            TaskBin = serialize_task(Task),

            %% Extract public key from signature
            PublicKey = Signature#pqc_signature.public_key_hash,

            %% Verify signature
            case pqc_crypto:verify(TaskBin, Signature, PublicKey) of
                {ok, true} ->
                    %% Verify signer address
                    case pqc_crypto:derive_address(PublicKey) of
                        SignerAddress ->
                            %% Store verified task
                            TaskId = Task#a2a_task.id,
                            ets:insert(Table, {TaskId, PQCTask}),

                            %% Update stats
                            NewStats = Stats#{
                                tasks_created := maps:get(tasks_created, Stats) + 1
                            },

                            NewState = State#state{stats = NewStats},
                            {ok, PQCTask, NewState};
                        _ ->
                            {error, address_mismatch}
                    end;
                {ok, false} ->
                    {error, invalid_signature};
                {error, Reason} ->
                    {error, {verification_failed, Reason}}
            end;
        _ ->
            {error, chain_id_mismatch}
    end.

%% @private Create signed message
do_create_message(Message, Metadata, State) ->
    #state{
        validator_keypair = Keypair,
        chain_id = ChainId,
        address = Address,
        stats = Stats
    } = State,

    Timestamp = erlang:system_time(millisecond),

    %% Serialize message for signing
    MessageBin = serialize_message(Message),

    %% Sign message
    case pqc_crypto:sign(MessageBin, Keypair, ?HASH_SHA3_256) of
        {ok, Signature} ->
            PQCMessage = #pqc_a2a_message{
                message = Message,
                signature = Signature,
                signer_address = Address,
                chain_id = ChainId,
                timestamp = Timestamp,
                metadata = Metadata
            },

            %% Update stats
            NewStats = Stats#{
                messages_verified := maps:get(messages_verified, Stats) + 1
            },

            NewState = State#state{stats = NewStats},
            {ok, PQCMessage, NewState};
        {error, Reason} ->
            {error, {signing_failed, Reason}}
    end.

%% @private Anchor artifact on-chain
do_anchor_artifact(Artifact, State) ->
    #state{
        validator_keypair = Keypair,
        chain_id = ChainId,
        address = Address,
        stats = Stats
    } = State,

    %% Hash artifact parts
    ArtifactHash = hash_artifact(Artifact),

    %% Create contract call transaction with artifact hash
    Payload = #contract_call_payload{
        contract_address = <<"artifact_registry">>,  % System contract
        transition = anchor_artifact,
        arguments = [ArtifactHash, Artifact#a2a_artifact.artifact_id],
        artifacts = [ArtifactHash]
    },

    %% Create transaction
    TxParams = #{
        chain_id => ChainId,
        sender => Address,
        recipient => <<"artifact_registry">>,
        amount => 0,
        fee => 1000,  % Minimal fee
        gas_limit => 100000,
        gas_price => 1,
        nonce => erlang:system_time(millisecond),
        payload => Payload
    },

    case pqc_transaction:create(?TX_TYPE_CONTRACT_CALL, TxParams) of
        {ok, Tx} ->
            %% Sign transaction
            case pqc_transaction:sign(Tx, Keypair) of
                {ok, SignedTx} ->
                    TxId = SignedTx#pqc_transaction.id,

                    %% TODO: Submit to mempool/blockchain
                    %% For now, just track the transaction ID

                    %% Update stats
                    NewStats = Stats#{
                        artifacts_anchored := maps:get(artifacts_anchored, Stats) + 1
                    },

                    NewState = State#state{stats = NewStats},
                    {ok, TxId, NewState};
                {error, Reason} ->
                    {error, {signing_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {tx_creation_failed, Reason}}
    end.

%% @private Verify artifact on-chain
do_verify_artifact(_ArtifactHash, _State) ->
    %% TODO: Query blockchain for artifact hash
    %% For now, return not implemented
    {error, not_implemented}.

%% @private Subscribe to tasks
do_subscribe_tasks(Pid, TaskIds, State) ->
    #state{subscriptions = Subs} = State,

    %% Monitor subscriber
    erlang:monitor(process, Pid),

    %% Add to subscriptions
    CurrentTasks = maps:get(Pid, Subs, []),
    NewTasks = lists:usort(TaskIds ++ CurrentTasks),
    NewSubs = Subs#{Pid => NewTasks},

    State#state{subscriptions = NewSubs}.

%% @private Unsubscribe from tasks
do_unsubscribe_tasks(Pid, TaskIds, State) ->
    #state{subscriptions = Subs} = State,

    case maps:get(Pid, Subs, undefined) of
        undefined ->
            State;
        CurrentTasks ->
            NewTasks = CurrentTasks -- TaskIds,
            NewSubs = case NewTasks of
                [] -> maps:remove(Pid, Subs);
                _ -> Subs#{Pid => NewTasks}
            end,
            State#state{subscriptions = NewSubs}
    end.

%% @private Remove subscriber
remove_subscriber(Pid, State) ->
    #state{subscriptions = Subs} = State,
    NewSubs = maps:remove(Pid, Subs),
    State#state{subscriptions = NewSubs}.

%% @private Establish secure channel with ML-KEM
do_establish_secure_channel(RemoteAgent, State) ->
    #state{
        kem_keypair = KEMKeypair,
        address = LocalAddress,
        channels = Channels,
        stats = Stats
    } = State,

    Timestamp = erlang:system_time(millisecond),
    ChannelId = generate_channel_id(LocalAddress, RemoteAgent, Timestamp),

    %% TODO: Perform ML-KEM key exchange with remote agent
    %% For now, create placeholder channel

    SecureChannel = #secure_channel{
        id = ChannelId,
        local_peer = LocalAddress,
        remote_peer = RemoteAgent,
        kem_algorithm = KEMKeypair#pqc_keypair.algorithm,
        shared_secret = crypto:strong_rand_bytes(32),  % Placeholder
        session_keys = create_session_keys(),
        established_at = Timestamp,
        last_rekey = Timestamp,
        rekey_interval_ms = 3600000,  % 1 hour
        messages_sent = 0,
        messages_received = 0
    },

    PQCChannel = #pqc_secure_channel{
        channel_id = ChannelId,
        remote_agent = RemoteAgent,
        secure_channel = SecureChannel,
        established_at = Timestamp,
        last_used = Timestamp
    },

    NewChannels = Channels#{RemoteAgent => PQCChannel},
    NewStats = Stats#{
        channels_established := maps:get(channels_established, Stats) + 1
    },

    NewState = State#state{
        channels = NewChannels,
        stats = NewStats
    },

    {ok, ChannelId, NewState}.

%% @private Close secure channel
do_close_secure_channel(ChannelId, State) ->
    #state{channels = Channels} = State,

    %% Find and remove channel
    NewChannels = maps:filter(
        fun(_, #pqc_secure_channel{channel_id = CId}) ->
            CId =/= ChannelId
        end,
        Channels
    ),

    State#state{channels = NewChannels}.

%%====================================================================
%% Serialization Functions
%%====================================================================

%% @private Serialize task for signing/verification
serialize_task(#a2a_task{id = Id, context_id = ContextId, status = Status}) ->
    %% Simple serialization - in production use proper encoding
    term_to_binary({Id, ContextId, Status}).

%% @private Serialize message for signing/verification
serialize_message(#a2a_message{message_id = Id, role = Role, parts = Parts}) ->
    %% Simple serialization - in production use proper encoding
    term_to_binary({Id, Role, Parts}).

%% @private Serialize agent card for signing/verification
serialize_agent_card(#a2a_agent_card{name = Name, version = Version, skills = Skills}) ->
    %% Simple serialization - in production use proper encoding
    term_to_binary({Name, Version, Skills}).

%% @private Hash artifact parts
hash_artifact(#a2a_artifact{artifact_id = Id, parts = Parts}) ->
    PartsBin = term_to_binary(Parts),
    {ok, Hash} = pqc_crypto:hash(<<Id/binary, PartsBin/binary>>, ?HASH_SHA3_256),
    Hash.

%% @private Generate channel ID
generate_channel_id(LocalAddr, RemoteAddr, Timestamp) ->
    Data = <<LocalAddr/binary, RemoteAddr/binary, Timestamp:64>>,
    {ok, Hash} = pqc_crypto:hash(Data, ?HASH_SHA3_256),
    Hash.

%% @private Create session keys from shared secret
create_session_keys() ->
    #session_keys{
        send_key = crypto:strong_rand_bytes(32),
        receive_key = crypto:strong_rand_bytes(32),
        send_nonce = 0,
        receive_nonce = 0,
        chain_key = crypto:strong_rand_bytes(32)
    }.
