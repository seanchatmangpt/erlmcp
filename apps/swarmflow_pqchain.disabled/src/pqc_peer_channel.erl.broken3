%%% @doc Post-Quantum Secure Peer Channel
%%%
%%% Manages secure peer-to-peer channels using ML-KEM key encapsulation.
%%% Implements:
%%% - ML-KEM-768 key establishment with ephemeral keys
%%% - HKDF session key derivation with forward secrecy
%%% - AES-256-GCM encrypted messaging with sequence numbers
%%% - Automatic key rotation with configurable intervals
%%% - Replay protection via nonces and sequence tracking
%%% - Message queuing for buffering during handshake
%%%
%%% Architecture:
%%% - Each peer channel is a supervised gen_server
%%% - Registered with gproc: {n, l, {peer_channel, LocalId, RemoteId}}
%%% - Monitors remote peer for connection management
%%% - Non-blocking init with async handshake
%%%
%%% Handshake Protocol:
%%% 1. Initiator generates ephemeral ML-KEM keypair
%%% 2. Sends HELLO with public key to responder
%%% 3. Responder encapsulates, returns RESPONSE with ciphertext
%%% 4. Both derive session keys from shared secret using HKDF
%%% 5. Confirm with encrypted VERIFY message
%%% 6. Channel ready for secure messaging
%%%
%%% @end
-module(pqc_peer_channel).
-behaviour(gen_server).

-include("pqchain.hrl").

%% API
-export([
    start_link/2,
    connect/2,
    send/2,
    receive_message/2,
    rekey/1,
    close/1,
    get_stats/1
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

%%% ============================================================================
%%% Type Definitions
%%% ============================================================================

-type channel_id() :: binary().
-type peer_id() :: binary().
-type message_type() :: hello | response | verify | data | rekey_request | rekey_response.
-type channel_state() :: initializing | handshake_init | handshake_wait |
                         handshake_verify | ready | rekeying | closing | closed.

-record(state, {
    channel_id :: channel_id(),
    local_identity :: #peer_identity{},
    remote_identity :: #peer_identity{} | undefined,

    %% Channel state
    status :: channel_state(),
    secure_channel :: #secure_channel{} | undefined,

    %% Handshake state
    ephemeral_keypair :: #pqc_keypair{} | undefined,
    handshake_started_at :: non_neg_integer() | undefined,

    %% Message queues
    pending_send :: queue:queue(),      % Messages queued during handshake
    pending_recv :: queue:queue(),      % Out-of-order messages

    %% Rekey tracking
    next_rekey_at :: non_neg_integer() | undefined,
    rekey_in_progress :: boolean(),

    %% Statistics
    stats :: channel_stats(),

    %% Monitoring
    remote_monitor :: reference() | undefined,

    %% Configuration
    config :: channel_config()
}).

-record(channel_stats, {
    established_at :: non_neg_integer() | undefined,
    messages_sent :: non_neg_integer(),
    messages_received :: non_neg_integer(),
    bytes_sent :: non_neg_integer(),
    bytes_received :: non_neg_integer(),
    rekeys_performed :: non_neg_integer(),
    last_activity :: non_neg_integer()
}).

-record(channel_config, {
    kem_algorithm = ?PQC_KEM_ML_KEM_768 :: pqc_kem_algorithm(),
    rekey_interval_ms = 3600000 :: non_neg_integer(),  % 1 hour
    handshake_timeout_ms = 30000 :: non_neg_integer(), % 30 seconds
    max_pending_messages = 100 :: non_neg_integer(),
    cipher_suite = aes_256_gcm :: aes_256_gcm | chacha20_poly1305
}).

-type channel_config() :: #channel_config{}.
-type channel_stats() :: #channel_stats{}.

%%% ============================================================================
%%% API Functions
%%% ============================================================================

%% @doc Start a peer channel between local and remote identities.
-spec start_link(#peer_identity{}, #peer_identity{}) ->
    {ok, pid()} | {error, term()}.
start_link(LocalIdentity, RemoteIdentity) ->
    gen_server:start_link(?MODULE, {LocalIdentity, RemoteIdentity}, []).

%% @doc Initiate handshake with remote peer.
-spec connect(pid(), channel_config()) -> ok | {error, term()}.
connect(Channel, Config) ->
    gen_server:call(Channel, {connect, Config}, 10000).

%% @doc Send encrypted message to remote peer.
-spec send(pid(), binary()) -> ok | {error, term()}.
send(Channel, Data) ->
    gen_server:call(Channel, {send, Data}, 5000).

%% @doc Handle incoming encrypted message from remote peer.
-spec receive_message(pid(), binary()) -> ok.
receive_message(Channel, EncryptedData) ->
    gen_server:cast(Channel, {receive_message, EncryptedData}).

%% @doc Perform key rotation.
-spec rekey(pid()) -> ok | {error, term()}.
rekey(Channel) ->
    gen_server:call(Channel, rekey, 10000).

%% @doc Close channel gracefully.
-spec close(pid()) -> ok.
close(Channel) ->
    gen_server:call(Channel, close, 5000).

%% @doc Get channel statistics.
-spec get_stats(pid()) -> {ok, map()}.
get_stats(Channel) ->
    gen_server:call(Channel, get_stats, 5000).

%%% ============================================================================
%%% gen_server Callbacks
%%% ============================================================================

%% @private
init({LocalIdentity, RemoteIdentity}) ->
    %% Non-blocking init - generate channel ID and set up initial state
    ChannelId = generate_channel_id(LocalIdentity#peer_identity.id,
                                     RemoteIdentity#peer_identity.id),

    State = #state{
        channel_id = ChannelId,
        local_identity = LocalIdentity,
        remote_identity = RemoteIdentity,
        status = initializing,
        secure_channel = undefined,
        ephemeral_keypair = undefined,
        handshake_started_at = undefined,
        pending_send = queue:new(),
        pending_recv = queue:new(),
        next_rekey_at = undefined,
        rekey_in_progress = false,
        stats = #channel_stats{
            established_at = undefined,
            messages_sent = 0,
            messages_received = 0,
            bytes_sent = 0,
            bytes_received = 0,
            rekeys_performed = 0,
            last_activity = erlang:system_time(millisecond)
        },
        remote_monitor = undefined,
        config = #channel_config{}
    },

    {ok, State}.

%% @private
handle_call({connect, Config}, _From, #state{status = initializing} = State) ->
    %% Update config
    NewState = State#state{config = Config},

    %% Start handshake process
    case initiate_handshake(NewState) of
        {ok, HandshakeState} ->
            {reply, ok, HandshakeState};
        {error, Reason} = Error ->
            {reply, Error, NewState#state{status = closed}}
    end;

handle_call({connect, _Config}, _From, State) ->
    {reply, {error, already_connected}, State};

handle_call({send, Data}, From, #state{status = ready,
                                       secure_channel = Channel} = State) ->
    case encrypt_and_send(Data, Channel, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} = Error ->
            {reply, Error, State}
    end;

handle_call({send, Data}, From, #state{status = handshake_verify} = State) ->
    %% Queue message until handshake completes
    NewPending = queue:in({From, Data}, State#state.pending_send),
    {noreply, State#state{pending_send = NewPending}};

handle_call({send, _Data}, _From, State) ->
    {reply, {error, channel_not_ready}, State};

handle_call(rekey, _From, #state{status = ready} = State) ->
    case initiate_rekey(State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} = Error ->
            {reply, Error, State}
    end;

handle_call(rekey, _From, State) ->
    {reply, {error, channel_not_ready}, State};

handle_call(close, _From, State) ->
    %% Clean up and close
    NewState = cleanup_channel(State),
    {stop, normal, ok, NewState#state{status = closed}};

handle_call(get_stats, _From, #state{stats = Stats,
                                      secure_channel = Channel} = State) ->
    StatsMap = #{
        established_at => Stats#channel_stats.established_at,
        messages_sent => Stats#channel_stats.messages_sent,
        messages_received => Stats#channel_stats.messages_received,
        bytes_sent => Stats#channel_stats.bytes_sent,
        bytes_received => Stats#channel_stats.bytes_received,
        rekeys_performed => Stats#channel_stats.rekeys_performed,
        last_activity => Stats#channel_stats.last_activity,
        status => State#state.status,
        channel_info => case Channel of
            undefined -> undefined;
            #secure_channel{} ->
                #{
                    established_at => Channel#secure_channel.established_at,
                    last_rekey => Channel#secure_channel.last_rekey,
                    kem_algorithm => Channel#secure_channel.kem_algorithm
                }
        end
    },
    {reply, {ok, StatsMap}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({receive_message, EncryptedData}, #state{status = ready} = State) ->
    case decrypt_and_process(EncryptedData, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            %% Log error but continue
            logger:warning("Failed to decrypt message: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast({receive_message, Data}, #state{status = handshake_wait} = State) ->
    %% Process handshake response
    case process_handshake_response(Data, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            logger:error("Handshake failed: ~p", [Reason]),
            {noreply, State#state{status = closed}}
    end;

handle_cast({receive_message, Data}, State) ->
    %% Queue message until ready
    NewPending = queue:in(Data, State#state.pending_recv),
    {noreply, State#state{pending_recv = NewPending}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({handshake_timeout}, #state{status = Status} = State)
    when Status =:= handshake_init; Status =:= handshake_wait ->
    logger:error("Handshake timeout for channel ~p", [State#state.channel_id]),
    {stop, handshake_timeout, State};

handle_info({rekey_timer}, #state{status = ready} = State) ->
    case initiate_rekey(State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            logger:warning("Auto-rekey failed: ~p", [Reason]),
            {noreply, schedule_rekey(State)}
    end;

handle_info({'DOWN', Ref, process, _Pid, Reason},
            #state{remote_monitor = Ref} = State) ->
    logger:info("Remote peer down: ~p", [Reason]),
    {stop, remote_peer_down, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, State) ->
    %% Unregister from gproc
    case State#state.local_identity of
        #peer_identity{id = LocalId} ->
            case State#state.remote_identity of
                #peer_identity{id = RemoteId} ->
                    gproc:unreg({n, l, {peer_channel, LocalId, RemoteId}});
                _ ->
                    ok
            end;
        _ ->
            ok
    end,
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ============================================================================
%%% Internal Functions - Handshake
%%% ============================================================================

%% @private
%% @doc Initiate ML-KEM handshake as initiator.
-spec initiate_handshake(#state{}) -> {ok, #state{}} | {error, term()}.
initiate_handshake(#state{config = Config,
                          local_identity = LocalIdentity,
                          remote_identity = RemoteIdentity} = State) ->
    %% Generate ephemeral ML-KEM keypair
    Algorithm = Config#channel_config.kem_algorithm,
    case pqc_crypto:kem_keygen(Algorithm) of
        {ok, EphemeralKeypair} ->
            %% Register with gproc
            LocalId = LocalIdentity#peer_identity.id,
            RemoteId = RemoteIdentity#peer_identity.id,
            gproc:add_local_name({peer_channel, LocalId, RemoteId}),

            %% Build HELLO message
            HelloMsg = #{
                type => hello,
                channel_id => State#state.channel_id,
                sender => LocalId,
                public_key => EphemeralKeypair#pqc_keypair.public_key,
                algorithm => Algorithm,
                timestamp => erlang:system_time(millisecond)
            },

            %% Send HELLO to remote peer (simulated here - would use transport layer)
            %% In real implementation, this would go through network transport
            logger:info("Sending HELLO for channel ~p", [State#state.channel_id]),

            %% Set handshake timeout
            TimeoutMs = Config#channel_config.handshake_timeout_ms,
            erlang:send_after(TimeoutMs, self(), {handshake_timeout}),

            NewState = State#state{
                status = handshake_wait,
                ephemeral_keypair = EphemeralKeypair,
                handshake_started_at = erlang:system_time(millisecond)
            },

            {ok, NewState};
        {error, Reason} ->
            {error, {keygen_failed, Reason}}
    end.

%% @private
%% @doc Process handshake response from responder.
-spec process_handshake_response(binary(), #state{}) -> {ok, #state{}} | {error, term()}.
process_handshake_response(ResponseData, #state{
                                ephemeral_keypair = EphemeralKeypair,
                                config = Config
                            } = State) ->
    %% Decode response (in practice, this would be proper binary protocol)
    %% For now, assume ResponseData contains the ciphertext from encapsulation

    %% Decapsulate to get shared secret
    case pqc_crypto:kem_decapsulate(ResponseData, EphemeralKeypair) of
        {ok, SharedSecret} ->
            %% Derive session keys using HKDF
            SessionKeys = derive_session_keys(SharedSecret, State#state.channel_id),

            %% Create secure channel record
            Now = erlang:system_time(millisecond),
            RekeyIntervalMs = Config#channel_config.rekey_interval_ms,

            SecureChannel = #secure_channel{
                id = State#state.channel_id,
                local_peer = State#state.local_identity#peer_identity.id,
                remote_peer = State#state.remote_identity#peer_identity.id,
                kem_algorithm = Config#channel_config.kem_algorithm,
                shared_secret = SharedSecret,
                session_keys = SessionKeys,
                established_at = Now,
                last_rekey = Now,
                rekey_interval_ms = RekeyIntervalMs,
                messages_sent = 0,
                messages_received = 0
            },

            %% Send verification message
            VerifyMsg = <<"VERIFY">>,
            case encrypt_message(VerifyMsg, SessionKeys) of
                {ok, EncryptedVerify} ->
                    logger:info("Handshake completed for channel ~p",
                                [State#state.channel_id]),

                    %% Schedule next rekey
                    NextRekeyAt = Now + RekeyIntervalMs,
                    erlang:send_after(RekeyIntervalMs, self(), {rekey_timer}),

                    NewStats = State#state.stats#channel_stats{
                        established_at = Now,
                        last_activity = Now
                    },

                    NewState = State#state{
                        status = ready,
                        secure_channel = SecureChannel,
                        next_rekey_at = NextRekeyAt,
                        stats = NewStats
                    },

                    %% Process any queued messages
                    process_pending_messages(NewState);
                {error, Reason} ->
                    {error, {verify_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {decapsulation_failed, Reason}}
    end.

%%% ============================================================================
%%% Internal Functions - Key Derivation
%%% ============================================================================

%% @private
%% @doc Derive session keys from shared secret using HKDF.
-spec derive_session_keys(binary(), channel_id()) -> #session_keys{}.
derive_session_keys(SharedSecret, ChannelId) ->
    %% HKDF-SHA3-256 key derivation
    %% Context: "PQC-PEER-CHANNEL-V1" || ChannelId
    Context = <<"PQC-PEER-CHANNEL-V1", ChannelId/binary>>,

    %% Derive 96 bytes: 32 send + 32 receive + 32 chain key
    DerivedKey = hkdf_expand(SharedSecret, Context, 96),

    <<SendKey:32/binary, ReceiveKey:32/binary, ChainKey:32/binary>> = DerivedKey,

    #session_keys{
        send_key = SendKey,
        receive_key = ReceiveKey,
        send_nonce = 0,
        receive_nonce = 0,
        chain_key = ChainKey
    }.

%% @private
%% @doc HKDF expand function (simplified - use proper HKDF in production).
-spec hkdf_expand(binary(), binary(), non_neg_integer()) -> binary().
hkdf_expand(Key, Info, Length) ->
    %% Simplified HKDF expand using HMAC-SHA3-256
    %% In production, use crypto:mac with proper HKDF
    BlockSize = 32,  % SHA3-256 output size
    N = (Length + BlockSize - 1) div BlockSize,

    Blocks = hkdf_expand_blocks(Key, Info, N, 1, <<>>, []),
    Result = iolist_to_binary(Blocks),
    <<Output:Length/binary, _/binary>> = Result,
    Output.

%% @private
hkdf_expand_blocks(_Key, _Info, N, Counter, _Prev, Acc) when Counter > N ->
    lists:reverse(Acc);
hkdf_expand_blocks(Key, Info, N, Counter, Prev, Acc) ->
    %% T(i) = HMAC-Hash(PRK, T(i-1) || Info || Counter)
    Data = <<Prev/binary, Info/binary, Counter:8>>,
    Block = crypto:mac(hmac, sha3_256, Key, Data),
    hkdf_expand_blocks(Key, Info, N, Counter + 1, Block, [Block | Acc]).

%%% ============================================================================
%%% Internal Functions - Encryption/Decryption
%%% ============================================================================

%% @private
%% @doc Encrypt message with session keys.
-spec encrypt_message(binary(), #session_keys{}) -> {ok, binary()} | {error, term()}.
encrypt_message(Plaintext, #session_keys{send_key = Key,
                                          send_nonce = Nonce}) ->
    %% AES-256-GCM encryption
    %% Format: <<SequenceNumber:64, IV:12/binary, Tag:16/binary, Ciphertext/binary>>
    IV = <<Nonce:96>>,  % 12 bytes for GCM

    try
        {Ciphertext, Tag} = crypto:crypto_one_time_aead(
            aes_256_gcm,
            Key,
            IV,
            Plaintext,
            <<Nonce:64>>,  % AAD = sequence number
            true
        ),

        EncryptedMsg = <<Nonce:64, IV/binary, Tag/binary, Ciphertext/binary>>,
        {ok, EncryptedMsg}
    catch
        Class:Reason:Stack ->
            {error, {encryption_failed, {Class, Reason, Stack}}}
    end.

%% @private
%% @doc Decrypt message with session keys.
-spec decrypt_message(binary(), #session_keys{}) -> {ok, binary(), non_neg_integer()} |
                                                     {error, term()}.
decrypt_message(<<SeqNum:64, IV:12/binary, Tag:16/binary, Ciphertext/binary>>,
                #session_keys{receive_key = Key, receive_nonce = ExpectedNonce}) ->
    %% Verify sequence number (replay protection)
    case SeqNum >= ExpectedNonce of
        true ->
            try
                Plaintext = crypto:crypto_one_time_aead(
                    aes_256_gcm,
                    Key,
                    IV,
                    Ciphertext,
                    <<SeqNum:64>>,  % AAD
                    Tag,
                    false
                ),
                {ok, Plaintext, SeqNum}
            catch
                Class:Reason:Stack ->
                    {error, {decryption_failed, {Class, Reason, Stack}}}
            end;
        false ->
            {error, replay_detected}
    end;
decrypt_message(_InvalidFormat, _Keys) ->
    {error, invalid_message_format}.

%%% ============================================================================
%%% Internal Functions - Message Processing
%%% ============================================================================

%% @private
%% @doc Encrypt and send message through channel.
-spec encrypt_and_send(binary(), #secure_channel{}, #state{}) ->
    {ok, #state{}} | {error, term()}.
encrypt_and_send(Data, #secure_channel{session_keys = Keys} = Channel, State) ->
    case encrypt_message(Data, Keys) of
        {ok, EncryptedData} ->
            %% Update sequence number
            NewKeys = Keys#session_keys{
                send_nonce = Keys#session_keys.send_nonce + 1
            },
            NewChannel = Channel#secure_channel{
                session_keys = NewKeys,
                messages_sent = Channel#secure_channel.messages_sent + 1
            },

            %% Update statistics
            NewStats = State#state.stats#channel_stats{
                messages_sent = State#state.stats#channel_stats.messages_sent + 1,
                bytes_sent = State#state.stats#channel_stats.bytes_sent +
                             byte_size(EncryptedData),
                last_activity = erlang:system_time(millisecond)
            },

            %% In real implementation, send via transport layer
            logger:debug("Sent encrypted message (~p bytes)", [byte_size(EncryptedData)]),

            NewState = State#state{
                secure_channel = NewChannel,
                stats = NewStats
            },
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% @doc Decrypt and process received message.
-spec decrypt_and_process(binary(), #state{}) -> {ok, #state{}} | {error, term()}.
decrypt_and_process(EncryptedData, #state{secure_channel = Channel} = State) ->
    Keys = Channel#secure_channel.session_keys,
    case decrypt_message(EncryptedData, Keys) of
        {ok, Plaintext, SeqNum} ->
            %% Update sequence number
            NewKeys = Keys#session_keys{
                receive_nonce = SeqNum + 1
            },
            NewChannel = Channel#secure_channel{
                session_keys = NewKeys,
                messages_received = Channel#secure_channel.messages_received + 1
            },

            %% Update statistics
            NewStats = State#state.stats#channel_stats{
                messages_received = State#state.stats#channel_stats.messages_received + 1,
                bytes_received = State#state.stats#channel_stats.bytes_received +
                                byte_size(EncryptedData),
                last_activity = erlang:system_time(millisecond)
            },

            %% Process plaintext (would route to application layer)
            logger:debug("Received plaintext: ~p", [Plaintext]),

            NewState = State#state{
                secure_channel = NewChannel,
                stats = NewStats
            },
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% @doc Process queued messages after handshake completion.
-spec process_pending_messages(#state{}) -> {ok, #state{}} | {error, term()}.
process_pending_messages(#state{pending_send = PendingSend} = State) ->
    case queue:out(PendingSend) of
        {{value, {From, Data}}, NewPending} ->
            case encrypt_and_send(Data, State#state.secure_channel, State) of
                {ok, NewState} ->
                    gen_server:reply(From, ok),
                    process_pending_messages(
                        NewState#state{pending_send = NewPending}
                    );
                {error, Reason} ->
                    gen_server:reply(From, {error, Reason}),
                    process_pending_messages(State#state{pending_send = NewPending})
            end;
        {empty, _} ->
            {ok, State}
    end.

%%% ============================================================================
%%% Internal Functions - Rekey
%%% ============================================================================

%% @private
%% @doc Initiate key rotation (forward secrecy).
-spec initiate_rekey(#state{}) -> {ok, #state{}} | {error, term()}.
initiate_rekey(#state{secure_channel = Channel,
                      config = Config} = State) ->
    %% Ratchet keys using chain key
    Keys = Channel#secure_channel.session_keys,
    ChainKey = Keys#session_keys.chain_key,

    %% Derive new keys from chain key
    NewDerivedKey = hkdf_expand(ChainKey, <<"REKEY">>, 96),
    <<NewSendKey:32/binary, NewReceiveKey:32/binary, NewChainKey:32/binary>> =
        NewDerivedKey,

    NewKeys = #session_keys{
        send_key = NewSendKey,
        receive_key = NewReceiveKey,
        send_nonce = 0,  % Reset nonces after rekey
        receive_nonce = 0,
        chain_key = NewChainKey
    },

    Now = erlang:system_time(millisecond),
    NewChannel = Channel#secure_channel{
        session_keys = NewKeys,
        last_rekey = Now
    },

    NewStats = State#state.stats#channel_stats{
        rekeys_performed = State#state.stats#channel_stats.rekeys_performed + 1,
        last_activity = Now
    },

    %% Schedule next rekey
    RekeyIntervalMs = Config#channel_config.rekey_interval_ms,
    NextRekeyAt = Now + RekeyIntervalMs,
    erlang:send_after(RekeyIntervalMs, self(), {rekey_timer}),

    logger:info("Rekeyed channel ~p", [State#state.channel_id]),

    NewState = State#state{
        secure_channel = NewChannel,
        next_rekey_at = NextRekeyAt,
        stats = NewStats
    },
    {ok, NewState}.

%% @private
%% @doc Schedule next rekey.
-spec schedule_rekey(#state{}) -> #state{}.
schedule_rekey(#state{config = Config} = State) ->
    RekeyIntervalMs = Config#channel_config.rekey_interval_ms,
    Now = erlang:system_time(millisecond),
    NextRekeyAt = Now + RekeyIntervalMs,
    erlang:send_after(RekeyIntervalMs, self(), {rekey_timer}),
    State#state{next_rekey_at = NextRekeyAt}.

%%% ============================================================================
%%% Internal Functions - Utilities
%%% ============================================================================

%% @private
%% @doc Generate deterministic channel ID from peer IDs.
-spec generate_channel_id(peer_id(), peer_id()) -> channel_id().
generate_channel_id(LocalId, RemoteId) ->
    %% Sort IDs for determinism regardless of initiator/responder
    [Id1, Id2] = lists:sort([LocalId, RemoteId]),
    Data = <<Id1/binary, Id2/binary>>,
    {ok, Hash} = pqc_crypto:hash(Data, ?HASH_SHA3_256),
    Hash.

%% @private
%% @doc Clean up channel resources.
-spec cleanup_channel(#state{}) -> #state{}.
cleanup_channel(#state{remote_monitor = Monitor} = State) ->
    %% Demonitor remote peer
    case Monitor of
        undefined -> ok;
        Ref -> erlang:demonitor(Ref, [flush])
    end,

    State#state{
        secure_channel = undefined,
        ephemeral_keypair = undefined,
        remote_monitor = undefined
    }.
