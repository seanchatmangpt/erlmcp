# Architecture Diagrams

This directory contains generated C4 architecture diagrams for SwarmFlow PQChain.

## Generating Diagrams

Run from the Erlang shell:

```erlang
pqc_c4_example:generate_all_diagrams().
```

Or via Docker:

```bash
docker compose run --rm erlmcp-build erl -noshell \
  -pa _build/default/lib/*/ebin \
  -eval 'pqc_c4_example:generate_all_diagrams()' \
  -s init stop
```

## Diagram Formats

### PlantUML (`.puml`)

PlantUML diagrams using the C4-PlantUML library.

**Render:**
```bash
plantuml *.puml
```

Or online at: https://www.plantuml.com/plantuml/uml/

**Files:**
- `01-context.puml` - System context diagram
- `02-container.puml` - Container diagram
- `03-component-consensus.puml` - Component diagram for Consensus Engine

### Structurizr DSL (`.dsl`)

Structurizr workspace definition.

**View:**
- Upload `workspace.dsl` to https://structurizr.com/
- Or run Structurizr Lite locally:
  ```bash
  docker run -it --rm -p 8080:8080 \
    -v $PWD:/usr/local/structurizr \
    structurizr/lite
  ```
  Then visit http://localhost:8080

**Files:**
- `workspace.dsl` - Complete workspace with all views

### Mermaid (`.mmd`)

Mermaid diagrams for Markdown embedding.

**View:**
- Paste into https://mermaid.live/
- Embed in Markdown:
  ````markdown
  ```mermaid
  graph TB
    ... content ...
  ```
  ````

**Files:**
- `context.mmd` - System context diagram
- `container.mmd` - Container diagram

## Architecture Overview

### System Context

Shows SwarmFlow PQChain in its environment:
- External actors (users, agents, validators)
- External systems (other blockchains, KMS, monitoring)
- Key interactions

### Containers

Shows the runtime containers/processes:
- **Case Kernel**: Erlang/OTP runtime
- **A2A Face**: Agent-to-Agent protocol interface
- **MCP Face**: Model Context Protocol interface
- **Consensus Engine**: BFT consensus (gen_statem)
- **Peer Network**: P2P with ML-KEM encryption
- **Contract Runtime**: Workflow-based smart contracts
- **Mempool Service**: Transaction pool
- **Chain State DB**: Blockchain state (Merkle Patricia)
- **Crypto Services**: PQC cryptography (Rust NIFs)

### Components

Detailed view of components within containers. Currently shows:
- Consensus Engine components (round state machine, validator set, quorum checker)

## See Also

- `../c4-architecture.md` - Detailed architecture documentation
- `../../src/pqc_c4_model.erl` - C4 model implementation
- `../../test/pqc_c4_model_test.erl` - Model tests
