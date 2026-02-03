# PQC A2A Bridge - Post-Quantum Agent-to-Agent Protocol

## Overview

The PQC A2A Bridge integrates Google's A2A (Agent-to-Agent) protocol with post-quantum cryptography, enabling quantum-safe agent collaboration with on-chain audit trails.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    PQC A2A Bridge                          │
│                                                             │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐    │
│  │  Task Mgmt   │  │  Message     │  │  Secure      │    │
│  │  + PQC Sign  │  │  Signing     │  │  Channels    │    │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘    │
│         │                  │                  │             │
│         └──────────────────┴──────────────────┘             │
│                            │                                │
│         ┌──────────────────┴──────────────────┐             │
│         │      PQC Crypto Module              │             │
│         │  (ML-DSA, ML-KEM, SHA3)             │             │
│         └──────────────┬────────────────────────┘             │
│                        │                                     │
└────────────────────────┼─────────────────────────────────────┘
                         │
         ┌───────────────┴───────────────┐
         │                               │
    ┌────▼─────┐                  ┌─────▼──────┐
    │  PQC     │                  │  A2A       │
    │  Chain   │                  │  Protocol  │
    └──────────┘                  └────────────┘
```

## Key Features

### 1. Post-Quantum Security
- **ML-DSA (FIPS 204)**: All messages and tasks signed with quantum-resistant signatures
- **ML-KEM (FIPS 203)**: Secure key exchange for encrypted agent channels
- **SHA3-256**: Quantum-resistant hashing for artifact integrity

### 2. On-Chain Anchoring
- Task signatures recorded on blockchain
- Artifact hashes stored in immutable ledger
- Full audit trail of agent interactions
- Verifiable execution history

### 3. Agent Identity Management
- Agent cards include PQC public keys
- Blockchain address derived from signing key
- Signature verification for all interactions
- Key rotation support

### 4. Secure Channels
- ML-KEM key encapsulation for forward secrecy
- Session key derivation
- Automatic rekey intervals
- Per-agent secure tunnels

## Usage

### Starting the Bridge

```erlang
%% Generate validator keypair
{ok, ValidatorKeypair} = pqc_crypto:keygen(ml_dsa_65),

%% Generate KEM keypair for secure channels
{ok, KEMKeypair} = pqc_crypto:kem_keygen(ml_kem_768),

%% Create base agent card
AgentCard = #a2a_agent_card{
    name = <<"My PQC Agent">>,
    description = <<"Agent with post-quantum security">>,
    supported_interfaces = [...],
    version = <<"1.0.0">>,
    capabilities = #a2a_agent_capabilities{
        streaming = true,
        push_notifications = true
    },
    default_input_modes = [<<"text/plain">>, <<"application/json">>],
    default_output_modes = [<<"text/plain">>, <<"application/json">>],
    skills = [...]
},

%% Start bridge
Config = #{
    validator_keypair => ValidatorKeypair,
    chain_id => <<"mainnet-001">>,
    agent_card => AgentCard,
    kem_keypair => KEMKeypair
},

{ok, Bridge} = pqc_a2a_bridge:start_link(Config).
```

### Sending Tasks

```erlang
%% Create A2A task
Task = #a2a_task{
    id = TaskId,
    context_id = ContextId,
    status = #a2a_task_status{
        state = submitted
    }
},

%% Send to remote agent with PQC signature
RemoteAgent = <<"remote-agent-blockchain-address">>,
{ok, PQCTask} = pqc_a2a_bridge:send_task(Bridge, Task, RemoteAgent).

%% PQCTask includes:
%% - Original task
%% - ML-DSA signature
%% - Signer blockchain address
%% - Chain ID
%% - Timestamp
```

### Receiving Tasks

```erlang
%% Receive task from remote agent
{ok, VerifiedTask} = pqc_a2a_bridge:receive_task(Bridge, PQCTask).

%% Bridge automatically:
%% 1. Verifies ML-DSA signature
%% 2. Checks chain ID matches
%% 3. Validates signer address
%% 4. Stores in ETS table
```

### Creating Messages

```erlang
%% Create A2A message
Message = #a2a_message{
    message_id = MessageId,
    role = user,
    parts = [
        #a2a_part{
            text = <<"Hello from PQC agent">>,
            media_type = <<"text/plain">>
        }
    ]
},

%% Sign message
Metadata = #{priority => high},
{ok, PQCMessage} = pqc_a2a_bridge:create_message(Bridge, Message, Metadata).
```

### Verifying Messages

```erlang
%% Verify message signature
case pqc_a2a_bridge:verify_message(PQCMessage) of
    {ok, valid} ->
        %% Signature is valid
        process_message(PQCMessage);
    {error, invalid_signature} ->
        %% Reject message
        reject_message(PQCMessage)
end.
```

### Anchoring Artifacts

```erlang
%% Create artifact
Artifact = #a2a_artifact{
    artifact_id = ArtifactId,
    name = <<"Agent Output">>,
    parts = [
        #a2a_part{
            data = ResultData,
            media_type = <<"application/json">>
        }
    ]
},

%% Anchor on blockchain
{ok, TxId} = pqc_a2a_bridge:anchor_artifact(Bridge, Artifact).

%% Creates contract_call transaction with:
%% - SHA3-256 hash of artifact
%% - Artifact ID
%% - Signed with validator key
```

### Establishing Secure Channels

```erlang
%% Establish ML-KEM secured channel
RemoteAgent = <<"remote-agent-address">>,
{ok, ChannelId} = pqc_a2a_bridge:establish_secure_channel(Bridge, RemoteAgent).

%% Channel provides:
%% - Forward secrecy via ML-KEM
%% - Automatic key rotation
%% - Session keys for encryption
%% - Message counters for replay protection
```

### Subscribing to Tasks

```erlang
%% Subscribe to task updates
TaskIds = [TaskId1, TaskId2, TaskId3],
ok = pqc_a2a_bridge:subscribe_tasks(Bridge, TaskIds).

%% Bridge will send updates when:
%% - Task status changes
%% - New artifacts added
%% - Task completed/failed
```

### Agent Cards

```erlang
%% Get local agent card with PQC keys
{ok, PQCCard} = pqc_a2a_bridge:get_agent_card(Bridge).

%% Card includes:
%% - Base A2A agent card
%% - ML-DSA public key
%% - ML-KEM public key
%% - Blockchain address
%% - Signature over card

%% Verify remote agent card
case pqc_a2a_bridge:verify_agent_card(RemoteCard) of
    {ok, valid} ->
        %% Card is valid, trust agent
        establish_connection(RemoteCard);
    {error, invalid_signature} ->
        %% Reject agent
        reject_agent(RemoteCard)
end.
```

## Data Structures

### PQC A2A Task

```erlang
-record(pqc_a2a_task, {
    task :: #a2a_task{},              % Standard A2A task
    signature :: #pqc_signature{},     % ML-DSA signature
    signer_address :: binary(),        % Blockchain address
    chain_id :: binary(),              % Chain identifier
    anchored :: boolean(),             % Is task on-chain?
    anchor_tx_id :: binary() | undefined,
    created_at :: non_neg_integer(),
    metadata :: map()
}).
```

### PQC A2A Message

```erlang
-record(pqc_a2a_message, {
    message :: #a2a_message{},         % Standard A2A message
    signature :: #pqc_signature{},     % ML-DSA signature
    signer_address :: binary(),        % Blockchain address
    chain_id :: binary(),              % Chain identifier
    timestamp :: non_neg_integer(),
    metadata :: map()
}).
```

### PQC Agent Card

```erlang
-record(pqc_agent_card, {
    agent_card :: #a2a_agent_card{},   % Standard A2A card
    pqc_signing_key :: binary(),       % ML-DSA public key
    pqc_kem_key :: binary(),           % ML-KEM public key
    blockchain_address :: binary(),     % Derived from signing key
    signature :: #pqc_signature{},     % Signature over card
    registered_at :: non_neg_integer()
}).
```

### PQC Secure Channel

```erlang
-record(pqc_secure_channel, {
    channel_id :: binary(),
    remote_agent :: binary(),
    secure_channel :: #secure_channel{},
    established_at :: non_neg_integer(),
    last_used :: non_neg_integer()
}).
```

## Security Model

### Threat Model

1. **Quantum Adversary**: Attacker with quantum computer
   - **Mitigation**: ML-DSA and ML-KEM provide quantum resistance

2. **Man-in-the-Middle**: Network interception
   - **Mitigation**: All messages signed, channels encrypted with ML-KEM

3. **Replay Attacks**: Reusing old messages
   - **Mitigation**: Timestamps, nonces, message counters

4. **Agent Impersonation**: Fake agent identity
   - **Mitigation**: Blockchain address verification, on-chain registration

5. **Data Tampering**: Modifying task results
   - **Mitigation**: On-chain artifact anchoring, immutable audit trail

### Cryptographic Algorithms

| Purpose | Algorithm | NIST Level | Key Size |
|---------|-----------|------------|----------|
| Signatures | ML-DSA-65 | Level 3 | ~4KB public, ~4KB private |
| Key Exchange | ML-KEM-768 | Level 3 | 1184 public, 2400 private |
| Hashing | SHA3-256 | 128-bit | N/A |

### Key Lifecycle

```
┌──────────┐      ┌──────────┐      ┌──────────┐      ┌──────────┐
│ Generate │─────>│ Register │─────>│  Active  │─────>│  Rotate  │
└──────────┘      └──────────┘      └──────────┘      └──────────┘
     │                  │                  │                  │
     │                  │                  │                  │
     v                  v                  v                  v
  ML-DSA            On-Chain          Sign Tasks         New Key
  ML-KEM          Transaction       Sign Messages      + Grace Period
```

## Integration Points

### PQC Crypto Module
```erlang
%% Generate keys
{ok, SigningKey} = pqc_crypto:keygen(ml_dsa_65),
{ok, KEMKey} = pqc_crypto:kem_keygen(ml_kem_768),

%% Sign data
{ok, Signature} = pqc_crypto:sign(Data, SigningKey, sha3_256),

%% Verify signature
{ok, Valid} = pqc_crypto:verify(Data, Signature, PublicKey),

%% Derive address
Address = pqc_crypto:derive_address(PublicKey).
```

### PQC Transaction Module
```erlang
%% Create transaction
Params = #{
    chain_id => ChainId,
    sender => Address,
    payload => Payload,
    nonce => Nonce
},
{ok, Tx} = pqc_transaction:create(contract_call, Params),

%% Sign transaction
{ok, SignedTx} = pqc_transaction:sign(Tx, Keypair).
```

### A2A Protocol
```erlang
%% Standard A2A operations work with PQC signatures
%% All A2A records (#a2a_task{}, #a2a_message{}, etc.) are wrapped
%% with PQC signatures and metadata
```

### gproc Registry
```erlang
%% Bridge registers itself with blockchain address
gproc:reg({n, l, {pqc_a2a_bridge, Address}}).

%% Lookup bridge by address
Pid = gproc:lookup_pid({n, l, {pqc_a2a_bridge, Address}}).
```

## Performance Characteristics

| Operation | Time | Notes |
|-----------|------|-------|
| ML-DSA Sign | ~1-2ms | Depends on security level |
| ML-DSA Verify | ~1ms | Faster than signing |
| ML-KEM Encapsulate | ~1ms | One-time per channel |
| Task Storage (ETS) | <0.1ms | In-memory lookup |
| SHA3-256 Hash | <0.1ms | Native implementation |

## Testing

### Running Tests

```bash
# Unit tests (Chicago School TDD - real processes, no mocks)
docker compose run --rm erlmcp-unit rebar3 eunit --module=pqc_a2a_bridge_tests

# Coverage report
docker compose run --rm erlmcp-check rebar3 cover
```

### Test Coverage

- Initialization and teardown: ✓
- Task signing and verification: ✓
- Message signing and verification: ✓
- Artifact anchoring: ✓
- Secure channel establishment: ✓
- Subscription management: ✓
- Agent card verification: ✓
- Statistics tracking: ✓

Target coverage: ≥80%

## Configuration

### Required Configuration

```erlang
#{
    validator_keypair => #pqc_keypair{},  % ML-DSA keypair
    chain_id => binary(),                 % Blockchain identifier
    agent_card => #a2a_agent_card{}       % Base A2A card
}
```

### Optional Configuration

```erlang
#{
    kem_keypair => #pqc_keypair{},        % ML-KEM keypair (generated if not provided)
    rekey_interval_ms => 3600000,         % Channel rekey interval (default 1 hour)
    task_ttl_ms => 86400000,              % Task expiry (default 24 hours)
    max_tasks => 10000                    % Max tasks in memory (default unlimited)
}
```

## Supervision

The bridge is designed to be supervised:

```erlang
%% In supervisor init/1
ChildSpec = #{
    id => pqc_a2a_bridge,
    start => {pqc_a2a_bridge, start_link, [Config]},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [pqc_a2a_bridge]
}.
```

## Future Enhancements

1. **Batch Signature Verification**: Verify multiple signatures in parallel
2. **Zero-Knowledge Proofs**: Prove task execution without revealing data
3. **Multi-Chain Support**: Bridge across multiple blockchains
4. **Agent Reputation**: On-chain reputation scores
5. **Distributed Validation**: Multi-signature task verification

## References

- [Google A2A Specification](https://github.com/google/a2a-spec)
- [NIST FIPS 203: ML-KEM](https://csrc.nist.gov/pubs/fips/203/final)
- [NIST FIPS 204: ML-DSA](https://csrc.nist.gov/pubs/fips/204/final)
- [NIST FIPS 205: SLH-DSA](https://csrc.nist.gov/pubs/fips/205/final)
- [erlmcp Documentation](https://github.com/seanchatmangpt/erlmcp)

## License

Same as erlmcp project license.
