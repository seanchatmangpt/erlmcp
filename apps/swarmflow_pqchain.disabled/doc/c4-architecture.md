# C4 Architecture Model for SwarmFlow PQChain

## Overview

This document describes the C4 (Context, Container, Component, Code) architecture model for SwarmFlow PQChain - a post-quantum blockchain with dual protocol interfaces (A2A and MCP).

The C4 model is implemented in `pqc_c4_model.erl` and provides multiple export formats for documentation and visualization.

## Architecture Philosophy

### One Kernel, Two Faces

SwarmFlow PQChain follows a "one kernel, two faces" architectural pattern:

- **Case Kernel**: Erlang/OTP runtime providing supervision, fault tolerance, hot code reload, and distributed computing
- **A2A Face**: Agent-to-Agent protocol interface (Google A2A) for autonomous AI agents
- **MCP Face**: Model Context Protocol interface (Anthropic MCP) for LLM and client integrations

### Post-Quantum Security

All cryptographic operations use NIST-standardized post-quantum algorithms:

- **ML-KEM** (FIPS 203): Key encapsulation for peer-to-peer channels
- **ML-DSA** (FIPS 204): Digital signatures for transactions and consensus
- **SLH-DSA** (FIPS 205): Stateless hash-based signatures for backup keys

### BFT Consensus

Byzantine Fault Tolerant consensus using supervised `gen_statem` processes:

- Each consensus round is a supervised state machine
- Phases: propose → prevote → precommit → commit → finalized
- PQC signatures for all votes
- Quorum certificates with aggregated proof

### Workflow-Based Contracts

Smart contracts are workflow nets (Petri nets with YAWL semantics):

- Powered by SwarmFlow OS
- Each contract instance is a supervised case
- Transitions with guards and receipts
- Deterministic execution with gas metering

## C4 Levels

### Level 1: System Context

External actors and systems interacting with PQChain.

#### External Actors (People)

- **Human User**: End users via wallets, block explorers, dApps
- **Autonomous Agent**: AI agents using A2A protocol
- **MCP Client**: LLMs and applications using MCP
- **Validator Operator**: Node operators running validators
- **Contract Developer**: Developers writing workflow contracts

#### External Systems

- **Other Blockchains**: Cross-chain interoperability
- **Key Management System**: HSM/KMS for secure key storage
- **Monitoring System**: Prometheus, Grafana, Jaeger
- **SwarmFlow OS**: Workflow runtime for contracts
- **IPFS/Filecoin**: Decentralized storage for artifacts

### Level 2: Containers (Runtime Processes)

Nine main containers form the system:

#### 1. Case Kernel
- **Technology**: Erlang/OTP 26+
- **Purpose**: Core runtime with supervision and fault tolerance
- **Components**:
  - Root Supervisor (one_for_all)
  - Case Registry (gproc)
  - Event Manager (gen_event)
  - Configuration Manager

#### 2. A2A Face
- **Technology**: HTTP/SSE (Google A2A Protocol)
- **Purpose**: Agent-to-Agent interface for autonomous agents
- **Components**:
  - A2A Bridge (gen_server)
  - A2A Router
  - A2A Projection
  - SSE Manager

#### 3. MCP Face
- **Technology**: JSON-RPC 2.0 (Anthropic MCP)
- **Purpose**: Model Context Protocol for LLMs
- **Components**:
  - MCP Bridge (gen_server)
  - MCP Router
  - MCP Projection
  - Tool Registry

#### 4. Consensus Engine
- **Technology**: gen_statem (BFT)
- **Purpose**: Byzantine Fault Tolerant consensus
- **Components**:
  - Consensus Supervisor (simple_one_for_one)
  - Consensus Round (gen_statem)
  - Validator Set (ets)
  - Quorum Checker

#### 5. Peer Network
- **Technology**: ranch/gun (ML-KEM channels)
- **Purpose**: P2P network with encrypted channels
- **Components**:
  - Peer Supervisor (simple_one_for_one)
  - Peer Channel (gen_server with ML-KEM)
  - Peer Discovery
  - Gossip Protocol

#### 6. Contract Runtime
- **Technology**: SwarmFlow OS (Workflow Nets)
- **Purpose**: Smart contract execution
- **Components**:
  - Contract Supervisor (simple_one_for_one)
  - Contract Instance (SwarmFlow case)
  - Contract Loader
  - Gas Meter

#### 7. Mempool Service
- **Technology**: gen_server + ets
- **Purpose**: Transaction pool management
- **Components**:
  - Mempool Supervisor (one_for_all)
  - Mempool Server (gen_server)
  - Transaction Validator
  - Priority Queue (gb_trees)

#### 8. Chain State DB
- **Technology**: Mnesia/RocksDB (Merkle Patricia)
- **Purpose**: Blockchain state storage
- **Components**:
  - Block Store (mnesia disc_copies)
  - State Trie (Merkle Patricia)
  - Receipt Store
  - Chain Indexer

#### 9. Crypto Services
- **Technology**: Rust NIFs (ML-KEM/ML-DSA/SLH-DSA)
- **Purpose**: Post-quantum cryptography
- **Components**:
  - Crypto NIF (Rust)
  - Crypto Policy Manager (gen_server)
  - Key Manager (gen_server)
  - Signature Verifier

### Level 3: Components

Each container contains multiple components. See the Container sections above for details.

Key component patterns:
- **Supervisors**: OTP supervisors managing child processes
- **gen_server**: Long-lived stateful processes
- **gen_statem**: State machines for consensus rounds
- **ets/mnesia**: In-memory and persistent storage
- **Modules**: Pure functional logic (validation, projection, etc.)

### Level 4: Code

Implementation details in Erlang/OTP:

- Records defined in `pqchain.hrl`
- OTP behaviors: `gen_server`, `gen_statem`, `supervisor`
- Supervision strategies: `one_for_all`, `simple_one_for_one`
- Let-it-crash philosophy with isolated failures
- No mocks in tests - real processes only (Chicago School TDD)

## Usage

### Querying the Model

```erlang
%% Get system context
Context = pqc_c4_model:context().

%% Get all containers
Containers = pqc_c4_model:containers().

%% Get components for a container
Components = pqc_c4_model:components(<<"consensus_engine">>).

%% Get all relationships
Relationships = pqc_c4_model:relationships().

%% Validate the model
ok = pqc_c4_model:validate_model().
```

### Generating Diagrams

#### PlantUML

```erlang
%% Context diagram
ContextDiagram = pqc_c4_model:to_plantuml(context).
file:write_file("context.puml", ContextDiagram).

%% Container diagram
ContainerDiagram = pqc_c4_model:to_plantuml(container).
file:write_file("container.puml", ContainerDiagram).

%% Component diagram (Consensus Engine example)
ComponentDiagram = pqc_c4_model:to_plantuml(component).
file:write_file("component.puml", ComponentDiagram).
```

Render with PlantUML:
```bash
plantuml context.puml
plantuml container.puml
plantuml component.puml
```

#### Structurizr DSL

```erlang
%% Export to Structurizr DSL
DSL = pqc_c4_model:to_structurizr_dsl().
file:write_file("workspace.dsl", DSL).
```

Upload to Structurizr cloud or use Structurizr Lite:
```bash
docker run -it --rm -p 8080:8080 \
  -v $PWD:/usr/local/structurizr \
  structurizr/lite
```

#### Mermaid

```erlang
%% Context diagram
MermaidContext = pqc_c4_model:to_mermaid(context).
file:write_file("context.mmd", MermaidContext).

%% Container diagram
MermaidContainer = pqc_c4_model:to_mermaid(container).
file:write_file("container.mmd", MermaidContainer).
```

Embed in Markdown:
````markdown
```mermaid
graph TB
  %% Paste content here
```
````

## Key Relationships

### External Actors to Faces

- **Human User** → **A2A Face**: Submits transactions via wallet
- **Autonomous Agent** ↔ **A2A Face**: Executes tasks, receives artifacts (bidirectional)
- **MCP Client** ↔ **MCP Face**: Calls tools, queries resources (bidirectional)
- **Validator Operator** → **Consensus Engine**: Operates validator node

### Faces to Kernel

- **A2A Face** ↔ **Case Kernel**: Routes via gproc registry
- **MCP Face** ↔ **Case Kernel**: Routes via gproc registry

### Kernel to Services

- **Case Kernel** ↔ **Consensus Engine**: OTP supervision
- **Case Kernel** ↔ **Peer Network**: OTP supervision
- **Case Kernel** ↔ **Contract Runtime**: OTP supervision
- **Case Kernel** ↔ **Mempool Service**: OTP supervision

### Cross-Service Communication

- **Consensus Engine** ↔ **Peer Network**: Broadcasts votes and blocks
- **Consensus Engine** → **Mempool Service**: Fetches transactions for blocks
- **Consensus Engine** → **Contract Runtime**: Executes contract transactions
- **Consensus Engine** → **Chain State DB**: Commits finalized blocks
- **Consensus Engine** → **Crypto Services**: Verifies signatures and quorum

- **Peer Network** → **Crypto Services**: ML-KEM handshake and encryption
- **Peer Network** → **Mempool Service**: Relays transactions from peers
- **Peer Network** → **Chain State DB**: Syncs blocks from peers

- **Contract Runtime** ↔ **Chain State DB**: Reads/writes contract state
- **Contract Runtime** → **SwarmFlow OS**: Executes workflow transitions

- **Mempool Service** → **Crypto Services**: Validates transaction signatures
- **Mempool Service** → **Chain State DB**: Checks nonces and balances

### External System Integrations

- **Crypto Services** → **Key Management System**: Stores and retrieves keys (HSM/KMS)
- **Case Kernel** → **Monitoring System**: Exports metrics, logs, traces (OpenTelemetry)
- **Peer Network** ↔ **Other Blockchains**: Cross-chain message relay (IBC/bridge)
- **Contract Runtime** → **IPFS**: Stores large artifacts

## Supervision Tree

```
swarmflow_pqchain_sup (one_for_all)
├── pqc_a2a_bridge (gen_server)
├── pqc_mcp_bridge (gen_server)
├── pqc_case_registry (gen_server - gproc)
├── pqc_events (gen_event)
├── pqc_consensus_sup (simple_one_for_one)
│   └── pqc_consensus_round (gen_statem) [dynamic]
├── pqc_peer_sup (simple_one_for_one)
│   └── pqc_peer_channel (gen_server) [dynamic]
├── pqc_mempool_sup (one_for_all)
│   ├── mempool_server (gen_server)
│   └── mempool_cache (ets owner)
├── pqc_contract_sup (simple_one_for_one)
│   └── contract_instance (SwarmFlow case) [dynamic]
└── pqc_crypto_policy (gen_server)
```

## Testing

Run tests via Docker:

```bash
# EUnit tests
docker compose run --rm erlmcp-unit rebar3 eunit --module=pqc_c4_model_test

# Full test suite
docker compose run --rm erlmcp-ct rebar3 ct --suite=pqc_c4_model_SUITE
```

## Validation

The model includes self-validation:

```erlang
case pqc_c4_model:validate_model() of
    ok ->
        io:format("Model is valid~n");
    {error, Errors} ->
        io:format("Model validation errors: ~p~n", [Errors])
end.
```

Validation checks:
- All IDs are unique
- All relationships reference valid elements
- All containers have components
- No orphaned elements

## Evolution

The C4 model should be updated when:

1. **New containers are added**: Update `containers/0` and relationships
2. **New components are added**: Update component lists in container definitions
3. **Relationships change**: Update `relationships/0`
4. **External systems change**: Update `external_systems/0`

Run validation after changes:
```erlang
ok = pqc_c4_model:validate_model().
```

## References

- [C4 Model](https://c4model.com/) - Simon Brown's C4 architecture model
- [C4-PlantUML](https://github.com/plantuml-stdlib/C4-PlantUML) - PlantUML macros for C4
- [Structurizr](https://structurizr.com/) - Software architecture documentation tool
- [Mermaid](https://mermaid.js.org/) - Diagramming and charting tool

## See Also

- `pqchain.hrl` - Core data structure definitions
- `docs/otp-patterns.md` - OTP design patterns for erlmcp
- `apps/swarmflow_pqchain/README.md` - PQChain overview
- `apps/swarmflow_os/README.md` - SwarmFlow OS workflow runtime
