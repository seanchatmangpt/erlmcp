%%%-------------------------------------------------------------------
%%% @doc PQC A2A Bridge Unit Tests
%%%
%%% Chicago School TDD: Real processes, no mocks.
%%% Tests verify actual PQC signature operations, ETS storage,
%%% task lifecycle, and agent card management.
%%%
%%% Coverage Requirements: >= 80%
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_a2a_bridge_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pqchain.hrl").
-include_lib("erlmcp_core/include/erlmcp_a2a.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Setup fixture - starts bridge with test configuration
setup() ->
    %% Generate test keypairs
    {ok, ValidatorKeypair} = pqc_crypto:keygen(?PQC_SIG_ML_DSA_65),
    {ok, KEMKeypair} = pqc_crypto:kem_keygen(?PQC_KEM_ML_KEM_768),

    %% Create base agent card
    AgentCard = create_test_agent_card(),

    %% Configuration
    Config = #{
        validator_keypair => ValidatorKeypair,
        chain_id => <<"test-chain-001">>,
        agent_card => AgentCard,
        kem_keypair => KEMKeypair
    },

    %% Start bridge
    {ok, Pid} = pqc_a2a_bridge:start_link(Config),

    #{
        pid => Pid,
        validator_keypair => ValidatorKeypair,
        kem_keypair => KEMKeypair,
        chain_id => <<"test-chain-001">>
    }.

%% Cleanup fixture
cleanup(#{pid := Pid}) ->
    pqc_a2a_bridge:stop(Pid).

%%====================================================================
%% Test Suite
%%====================================================================

%%--------------------------------------------------------------------
%% Initialization Tests
%%--------------------------------------------------------------------

start_stop_test() ->
    State = setup(),
    ?assertMatch(#{pid := Pid} when is_pid(Pid), State),
    cleanup(State).

agent_card_generation_test() ->
    State = setup(),
    #{pid := Pid} = State,

    %% Get agent card
    {ok, PQCCard} = pqc_a2a_bridge:get_agent_card(Pid),

    %% Verify card structure
    ?assertMatch(#pqc_agent_card{}, PQCCard),
    ?assert(is_binary(PQCCard#pqc_agent_card.pqc_signing_key)),
    ?assert(is_binary(PQCCard#pqc_agent_card.pqc_kem_key)),
    ?assert(is_binary(PQCCard#pqc_agent_card.blockchain_address)),
    ?assertMatch(#pqc_signature{}, PQCCard#pqc_agent_card.signature),

    %% Verify card signature
    ?assertEqual({ok, valid}, pqc_a2a_bridge:verify_agent_card(PQCCard)),

    cleanup(State).

%%--------------------------------------------------------------------
%% Task Management Tests
%%--------------------------------------------------------------------

send_task_test() ->
    State = setup(),
    #{pid := Pid, chain_id := ChainId} = State,

    %% Create test task
    Task = create_test_task(ChainId),
    RemoteAgent = <<"remote-agent-address">>,

    %% Send task
    {ok, PQCTask} = pqc_a2a_bridge:send_task(Pid, Task, RemoteAgent),

    %% Verify PQC task structure
    ?assertMatch(#pqc_a2a_task{}, PQCTask),
    ?assertEqual(Task, PQCTask#pqc_a2a_task.task),
    ?assertMatch(#pqc_signature{}, PQCTask#pqc_a2a_task.signature),
    ?assert(is_binary(PQCTask#pqc_a2a_task.signer_address)),
    ?assertEqual(ChainId, PQCTask#pqc_a2a_task.chain_id),
    ?assertEqual(false, PQCTask#pqc_a2a_task.anchored),

    %% Verify task is stored
    TaskId = Task#a2a_task.id,
    {ok, StoredTask} = pqc_a2a_bridge:get_task(Pid, TaskId),
    ?assertEqual(PQCTask, StoredTask),

    cleanup(State).

receive_task_test() ->
    State = setup(),
    #{pid := Pid, chain_id := ChainId} = State,

    %% Create and send task first
    Task = create_test_task(ChainId),
    RemoteAgent = <<"remote-agent-address">>,
    {ok, PQCTask} = pqc_a2a_bridge:send_task(Pid, Task, RemoteAgent),

    %% Receive the same task (simulating remote agent sending it)
    {ok, ReceivedTask} = pqc_a2a_bridge:receive_task(Pid, PQCTask),

    %% Verify received task matches
    ?assertEqual(PQCTask#pqc_a2a_task.task, ReceivedTask#pqc_a2a_task.task),
    ?assertEqual(PQCTask#pqc_a2a_task.signature, ReceivedTask#pqc_a2a_task.signature),

    cleanup(State).

receive_task_invalid_signature_test() ->
    State = setup(),
    #{pid := Pid, chain_id := ChainId} = State,

    %% Create task with invalid signature
    Task = create_test_task(ChainId),
    InvalidSignature = #pqc_signature{
        algorithm = ?PQC_SIG_ML_DSA_65,
        signature = <<"invalid-signature">>,
        public_key_hash = <<"invalid-key">>,
        timestamp = erlang:system_time(millisecond)
    },

    PQCTask = #pqc_a2a_task{
        task = Task,
        signature = InvalidSignature,
        signer_address = <<"fake-address">>,
        chain_id = ChainId,
        anchored = false,
        anchor_tx_id = undefined,
        created_at = erlang:system_time(millisecond),
        metadata = #{}
    },

    %% Attempt to receive task with invalid signature
    Result = pqc_a2a_bridge:receive_task(Pid, PQCTask),
    ?assertMatch({error, _}, Result),

    cleanup(State).

list_tasks_test() ->
    State = setup(),
    #{pid := Pid, chain_id := ChainId} = State,

    %% Create and send multiple tasks
    RemoteAgent = <<"remote-agent-address">>,
    Task1 = create_test_task(ChainId),
    Task2 = create_test_task(ChainId),
    Task3 = create_test_task(ChainId),

    {ok, _} = pqc_a2a_bridge:send_task(Pid, Task1, RemoteAgent),
    {ok, _} = pqc_a2a_bridge:send_task(Pid, Task2, RemoteAgent),
    {ok, _} = pqc_a2a_bridge:send_task(Pid, Task3, RemoteAgent),

    %% List all tasks
    {ok, Tasks} = pqc_a2a_bridge:list_tasks(Pid),

    %% Verify we got 3 tasks
    ?assertEqual(3, length(Tasks)),
    ?assert(lists:all(fun(T) -> is_record(T, pqc_a2a_task) end, Tasks)),

    cleanup(State).

%%--------------------------------------------------------------------
%% Message Management Tests
%%--------------------------------------------------------------------

create_message_test() ->
    State = setup(),
    #{pid := Pid, chain_id := ChainId} = State,

    %% Create test message
    Message = create_test_message(ChainId),
    Metadata = #{priority => high, tags => [test, demo]},

    %% Create PQC message
    {ok, PQCMessage} = pqc_a2a_bridge:create_message(Pid, Message, Metadata),

    %% Verify PQC message structure
    ?assertMatch(#pqc_a2a_message{}, PQCMessage),
    ?assertEqual(Message, PQCMessage#pqc_a2a_message.message),
    ?assertMatch(#pqc_signature{}, PQCMessage#pqc_a2a_message.signature),
    ?assert(is_binary(PQCMessage#pqc_a2a_message.signer_address)),
    ?assertEqual(ChainId, PQCMessage#pqc_a2a_message.chain_id),
    ?assertEqual(Metadata, PQCMessage#pqc_a2a_message.metadata),

    cleanup(State).

verify_message_test() ->
    State = setup(),
    #{pid := Pid, chain_id := ChainId} = State,

    %% Create and sign message
    Message = create_test_message(ChainId),
    {ok, PQCMessage} = pqc_a2a_bridge:create_message(Pid, Message, #{}),

    %% Verify message signature
    Result = pqc_a2a_bridge:verify_message(PQCMessage),
    ?assertMatch({ok, valid}, Result),

    cleanup(State).

%%--------------------------------------------------------------------
%% Subscription Tests
%%--------------------------------------------------------------------

subscribe_tasks_test() ->
    State = setup(),
    #{pid := Pid, chain_id := ChainId} = State,

    %% Create test tasks
    Task1 = create_test_task(ChainId),
    Task2 = create_test_task(ChainId),
    TaskIds = [Task1#a2a_task.id, Task2#a2a_task.id],

    %% Subscribe to tasks
    ok = pqc_a2a_bridge:subscribe_tasks(Pid, TaskIds),

    %% Unsubscribe
    ok = pqc_a2a_bridge:unsubscribe_tasks(Pid, TaskIds),

    cleanup(State).

%%--------------------------------------------------------------------
%% Secure Channel Tests
%%--------------------------------------------------------------------

establish_secure_channel_test() ->
    State = setup(),
    #{pid := Pid} = State,

    RemoteAgent = <<"remote-agent-address">>,

    %% Establish secure channel
    {ok, ChannelId} = pqc_a2a_bridge:establish_secure_channel(Pid, RemoteAgent),

    %% Verify channel ID is binary
    ?assert(is_binary(ChannelId)),

    %% Close channel
    ok = pqc_a2a_bridge:close_secure_channel(Pid, ChannelId),

    cleanup(State).

%%--------------------------------------------------------------------
%% Statistics Tests
%%--------------------------------------------------------------------

statistics_tracking_test() ->
    State = setup(),
    #{pid := Pid, chain_id := ChainId} = State,

    %% Get initial statistics
    {ok, InitialStats} = pqc_a2a_bridge:get_statistics(Pid),
    ?assertMatch(#{
        tasks_created := 0,
        tasks_signed := 0,
        artifacts_anchored := 0,
        messages_verified := 0,
        channels_established := 0
    }, InitialStats),

    %% Perform operations
    Task = create_test_task(ChainId),
    RemoteAgent = <<"remote-agent-address">>,
    {ok, _} = pqc_a2a_bridge:send_task(Pid, Task, RemoteAgent),

    Message = create_test_message(ChainId),
    {ok, _} = pqc_a2a_bridge:create_message(Pid, Message, #{}),

    {ok, _} = pqc_a2a_bridge:establish_secure_channel(Pid, RemoteAgent),

    %% Get updated statistics
    {ok, UpdatedStats} = pqc_a2a_bridge:get_statistics(Pid),
    ?assertEqual(1, maps:get(tasks_created, UpdatedStats)),
    ?assertEqual(1, maps:get(tasks_signed, UpdatedStats)),
    ?assertEqual(1, maps:get(messages_verified, UpdatedStats)),
    ?assertEqual(1, maps:get(channels_established, UpdatedStats)),

    cleanup(State).

%%--------------------------------------------------------------------
%% Anchor Artifact Tests
%%--------------------------------------------------------------------

anchor_artifact_test() ->
    State = setup(),
    #{pid := Pid} = State,

    %% Create test artifact
    Artifact = create_test_artifact(),

    %% Anchor artifact
    {ok, TxId} = pqc_a2a_bridge:anchor_artifact(Pid, Artifact),

    %% Verify transaction ID is binary
    ?assert(is_binary(TxId)),

    cleanup(State).

%%====================================================================
%% Test Helpers
%%====================================================================

%% @private Create test agent card
create_test_agent_card() ->
    #a2a_agent_card{
        name = <<"Test PQC Agent">>,
        description = <<"A test agent with PQC capabilities">>,
        supported_interfaces = [
            #a2a_agent_interface{
                url = <<"https://test-agent.example.com/a2a">>,
                protocol_binding = ?A2A_PROTOCOL_BINDING_JSONRPC,
                protocol_version = ?A2A_PROTOCOL_VERSION
            }
        ],
        version = <<"1.0.0">>,
        capabilities = #a2a_agent_capabilities{
            streaming = true,
            push_notifications = true,
            extended_agent_card = true
        },
        default_input_modes = [?A2A_MIME_TEXT_PLAIN, ?A2A_MIME_APPLICATION_JSON],
        default_output_modes = [?A2A_MIME_TEXT_PLAIN, ?A2A_MIME_APPLICATION_JSON],
        skills = [
            #a2a_agent_skill{
                id = <<"pqc-signing">>,
                name = <<"PQC Signing">>,
                description = <<"Sign messages with post-quantum signatures">>,
                tags = [<<"pqc">>, <<"security">>, <<"blockchain">>]
            }
        ]
    }.

%% @private Create test task
create_test_task(ChainId) ->
    TaskId = generate_uuid(),
    ContextId = generate_uuid(),

    #a2a_task{
        id = TaskId,
        context_id = ContextId,
        status = #a2a_task_status{
            state = ?A2A_TASK_STATE_SUBMITTED,
            timestamp = erlang:system_time(millisecond)
        },
        metadata = #{
            chain_id => ChainId,
            created_by => <<"test-agent">>
        }
    }.

%% @private Create test message
create_test_message(_ChainId) ->
    MessageId = generate_uuid(),

    #a2a_message{
        message_id = MessageId,
        role = ?A2A_ROLE_USER,
        parts = [
            #a2a_part{
                text = <<"Test message for PQC signing">>,
                media_type = ?A2A_MIME_TEXT_PLAIN
            }
        ]
    }.

%% @private Create test artifact
create_test_artifact() ->
    ArtifactId = generate_uuid(),

    #a2a_artifact{
        artifact_id = ArtifactId,
        name = <<"Test Artifact">>,
        description = <<"A test artifact for anchoring">>,
        parts = [
            #a2a_part{
                text = <<"Artifact content">>,
                media_type = ?A2A_MIME_TEXT_PLAIN
            }
        ]
    }.

%% @private Generate UUID (simplified)
generate_uuid() ->
    Bytes = crypto:strong_rand_bytes(16),
    list_to_binary(io_lib:format("~32.16.0b", [binary:decode_unsigned(Bytes)])).
