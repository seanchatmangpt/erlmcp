%%% @doc C4 Architecture Model for SwarmFlow PQChain
%%%
%%% Implements Simon Brown's C4 model (Context, Container, Component, Code) for
%%% documenting the architecture of SwarmFlow PQChain - a post-quantum blockchain
%%% with "one kernel, two faces" design.
%%%
%%% Architecture Philosophy:
%%% - Case Kernel: Erlang/OTP runtime with supervision, fault tolerance, hot code reload
%%% - A2A Face: Agent-to-Agent protocol (HTTP/SSE) for autonomous agent interactions
%%% - MCP Face: Model Context Protocol (JSON-RPC) for LLM/client integrations
%%% - Post-Quantum: ML-KEM, ML-DSA, SLH-DSA (NIST FIPS 203/204/205)
%%% - BFT Consensus: Byzantine Fault Tolerant with PQC signatures
%%% - Workflow Contracts: Petri nets with YAWL semantics via SwarmFlow OS
%%%
%%% Export Formats:
%%% - PlantUML C4 diagrams (with C4-PlantUML library)
%%% - Structurizr DSL (for Structurizr cloud/on-premise)
%%% - Mermaid diagrams (for Markdown documentation)
%%%
%%% @end
-module(pqc_c4_model).

-include("pqchain.hrl").

%% API
-export([
    context/0,
    containers/0,
    components/1,
    relationships/0,
    to_plantuml/1,
    to_structurizr_dsl/0,
    to_mermaid/1,
    validate_model/0
]).

%% Export for testing
-export([
    find_element/2,
    get_all_ids/0
]).

%%% ============================================================================
%%% C4 Model Records
%%% ============================================================================

-record(c4_person, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    external :: boolean()
}).

-record(c4_system, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    external :: boolean(),
    containers :: [#c4_container{}]
}).

-record(c4_container, {
    id :: binary(),
    name :: binary(),
    technology :: binary(),
    description :: binary(),
    components :: [#c4_component{}]
}).

-record(c4_component, {
    id :: binary(),
    name :: binary(),
    technology :: binary(),
    description :: binary(),
    responsibilities :: [binary()]
}).

-record(c4_relationship, {
    source :: binary(),
    target :: binary(),
    description :: binary(),
    technology :: binary() | undefined,
    bidirectional :: boolean()
}).

%%% ============================================================================
%%% Level 1: System Context
%%% ============================================================================

-spec context() -> #c4_system{}.
context() ->
    #c4_system{
        id = <<"pqchain">>,
        name = <<"SwarmFlow PQChain">>,
        description = <<"Post-quantum blockchain with A2A and MCP protocol support. "
                        "Provides Byzantine Fault Tolerant consensus, ML-KEM/ML-DSA cryptography, "
                        "and workflow-based smart contracts powered by SwarmFlow OS.">>,
        external = false,
        containers = containers()
    }.

%% External actors (people)
-spec external_actors() -> [#c4_person{}].
external_actors() ->
    [
        #c4_person{
            id = <<"human_user">>,
            name = <<"Human User">>,
            description = <<"End user interacting via wallets, block explorers, or dApps">>,
            external = true
        },
        #c4_person{
            id = <<"autonomous_agent">>,
            name = <<"Autonomous Agent">>,
            description = <<"AI agent communicating via A2A protocol (Google Agent-to-Agent)">>,
            external = true
        },
        #c4_person{
            id = <<"mcp_client">>,
            name = <<"MCP Client">>,
            description = <<"LLM or application using Model Context Protocol (Anthropic MCP)">>,
            external = true
        },
        #c4_person{
            id = <<"validator_operator">>,
            name = <<"Validator Operator">>,
            description = <<"Node operator running validator infrastructure">>,
            external = true
        },
        #c4_person{
            id = <<"contract_developer">>,
            name = <<"Contract Developer">>,
            description = <<"Developer writing workflow-based smart contracts">>,
            external = true
        }
    ].

%% External systems
-spec external_systems() -> [#c4_system{}].
external_systems() ->
    [
        #c4_system{
            id = <<"other_blockchains">>,
            name = <<"Other Blockchains">>,
            description = <<"External blockchain networks for cross-chain interoperability">>,
            external = true,
            containers = []
        },
        #c4_system{
            id = <<"key_management_system">>,
            name = <<"Key Management System">>,
            description = <<"External HSM or KMS for secure key storage and operations">>,
            external = true,
            containers = []
        },
        #c4_system{
            id = <<"monitoring_system">>,
            name = <<"Monitoring System">>,
            description = <<"External observability platform (Prometheus, Grafana, Jaeger)">>,
            external = true,
            containers = []
        },
        #c4_system{
            id = <<"swarmflow_os">>,
            name = <<"SwarmFlow OS">>,
            description = <<"Workflow runtime providing YAWL/Petri-net semantics for contracts">>,
            external = true,
            containers = []
        },
        #c4_system{
            id = <<"ipfs">>,
            name = <<"IPFS/Filecoin">>,
            description = <<"Decentralized storage for large contract artifacts">>,
            external = true,
            containers = []
        }
    ].

%%% ============================================================================
%%% Level 2: Containers (Runtime Processes/Systems)
%%% ============================================================================

-spec containers() -> [#c4_container{}].
containers() ->
    [
        case_kernel(),
        a2a_face(),
        mcp_face(),
        consensus_engine(),
        peer_network(),
        contract_runtime(),
        mempool_service(),
        chain_state_db(),
        crypto_services()
    ].

%% Case Kernel - The Erlang/OTP Runtime
-spec case_kernel() -> #c4_container{}.
case_kernel() ->
    #c4_container{
        id = <<"case_kernel">>,
        name = <<"Case Kernel">>,
        technology = <<"Erlang/OTP 26+">>,
        description = <<"Core OTP runtime providing supervision, fault tolerance, "
                        "hot code reload, distributed Erlang, and process registry. "
                        "Implements 'let-it-crash' philosophy with supervisor trees.">>,
        components = [
            #c4_component{
                id = <<"root_supervisor">>,
                name = <<"Root Supervisor">>,
                technology = <<"supervisor (one_for_all)">>,
                description = <<"Top-level supervisor managing all major subsystems">>,
                responsibilities = [
                    <<"Supervise A2A and MCP bridges">>,
                    <<"Supervise consensus, peer, mempool, contract supervisors">>,
                    <<"Ensure system-wide fault tolerance">>,
                    <<"Coordinate graceful shutdown">>
                ]
            },
            #c4_component{
                id = <<"case_registry">>,
                name = <<"Case Registry">>,
                technology = <<"gproc">>,
                description = <<"Global process registry for workflow cases and services">>,
                responsibilities = [
                    <<"Register named processes">>,
                    <<"Route messages by name or pattern">>,
                    <<"Publish process properties">>,
                    <<"Monitor process lifecycle">>
                ]
            },
            #c4_component{
                id = <<"event_manager">>,
                name = <<"Event Manager">>,
                technology = <<"gen_event">>,
                description = <<"System-wide event bus for observability and integration">>,
                responsibilities = [
                    <<"Distribute blockchain events">>,
                    <<"Emit consensus events">>,
                    <<"Publish transaction events">>,
                    <<"Support event subscriptions">>
                ]
            },
            #c4_component{
                id = <<"config_manager">>,
                name = <<"Configuration Manager">>,
                technology = <<"application env">>,
                description = <<"Runtime configuration and parameter management">>,
                responsibilities = [
                    <<"Load chain parameters">>,
                    <<"Manage crypto policy">>,
                    <<"Support hot config updates">>,
                    <<"Validate configuration">>
                ]
            }
        ]
    }.

%% A2A Face - Agent-to-Agent Protocol Interface
-spec a2a_face() -> #c4_container{}.
a2a_face() ->
    #c4_container{
        id = <<"a2a_face">>,
        name = <<"A2A Face">>,
        technology = <<"HTTP/SSE (Google A2A Protocol)">>,
        description = <<"Agent-to-Agent protocol interface for autonomous AI agents. "
                        "Supports task decomposition, artifact exchange, and async notifications "
                        "via Server-Sent Events.">>,
        components = [
            #c4_component{
                id = <<"a2a_bridge">>,
                name = <<"A2A Bridge">>,
                technology = <<"gen_server">>,
                description = <<"Main A2A protocol handler and HTTP server">>,
                responsibilities = [
                    <<"Accept HTTP requests from agents">>,
                    <<"Validate A2A protocol messages">>,
                    <<"Maintain SSE connections for subscriptions">>,
                    <<"Route to kernel services">>
                ]
            },
            #c4_component{
                id = <<"a2a_router">>,
                name = <<"A2A Router">>,
                technology = <<"router module">>,
                description = <<"Routes A2A operations to appropriate backend services">>,
                responsibilities = [
                    <<"Map A2A operations to blockchain RPC">>,
                    <<"Handle task submissions">>,
                    <<"Manage artifact references">>,
                    <<"Coordinate async workflows">>
                ]
            },
            #c4_component{
                id = <<"a2a_projection">>,
                name = <<"A2A Projection">>,
                technology = <<"projection module">>,
                description = <<"Projects blockchain state into A2A protocol responses">>,
                responsibilities = [
                    <<"Format blocks as A2A artifacts">>,
                    <<"Project transactions as tasks">>,
                    <<"Stream events via SSE">>,
                    <<"Maintain protocol compatibility">>
                ]
            },
            #c4_component{
                id = <<"sse_manager">>,
                name = <<"SSE Manager">>,
                technology = <<"gen_server">>,
                description = <<"Manages Server-Sent Events connections for subscriptions">>,
                responsibilities = [
                    <<"Maintain SSE connection pool">>,
                    <<"Broadcast events to subscribers">>,
                    <<"Handle client reconnection">>,
                    <<"Apply backpressure">>
                ]
            }
        ]
    }.

%% MCP Face - Model Context Protocol Interface
-spec mcp_face() -> #c4_container{}.
mcp_face() ->
    #c4_container{
        id = <<"mcp_face">>,
        name = <<"MCP Face">>,
        technology = <<"JSON-RPC 2.0 (Anthropic MCP)">>,
        description = <<"Model Context Protocol interface for LLMs and MCP clients. "
                        "Provides tools, resources, and prompts for blockchain operations.">>,
        components = [
            #c4_component{
                id = <<"mcp_bridge">>,
                name = <<"MCP Bridge">>,
                technology = <<"gen_server">>,
                description = <<"Main MCP protocol handler and JSON-RPC server">>,
                responsibilities = [
                    <<"Accept JSON-RPC 2.0 requests">>,
                    <<"Validate MCP messages">>,
                    <<"Manage MCP sessions">>,
                    <<"Route to kernel services">>
                ]
            },
            #c4_component{
                id = <<"mcp_router">>,
                name = <<"MCP Router">>,
                technology = <<"router module">>,
                description = <<"Routes MCP tools/resources to blockchain operations">>,
                responsibilities = [
                    <<"Map MCP tools to RPC methods">>,
                    <<"Expose blockchain as resources">>,
                    <<"Provide prompts for LLMs">>,
                    <<"Handle subscriptions">>
                ]
            },
            #c4_component{
                id = <<"mcp_projection">>,
                name = <<"MCP Projection">>,
                technology = <<"projection module">>,
                description = <<"Projects blockchain state into MCP protocol responses">>,
                responsibilities = [
                    <<"Format blocks as MCP resources">>,
                    <<"Expose tools for transactions">>,
                    <<"Provide contract inspection">>,
                    <<"Generate LLM-friendly prompts">>
                ]
            },
            #c4_component{
                id = <<"tool_registry">>,
                name = <<"Tool Registry">>,
                technology = <<"map module">>,
                description = <<"Registry of available MCP tools and their schemas">>,
                responsibilities = [
                    <<"Register blockchain tools">>,
                    <<"Provide JSON schemas">>,
                    <<"Support tool discovery">>,
                    <<"Validate tool calls">>
                ]
            }
        ]
    }.

%% Consensus Engine - BFT Consensus
-spec consensus_engine() -> #c4_container{}.
consensus_engine() ->
    #c4_container{
        id = <<"consensus_engine">>,
        name = <<"Consensus Engine">>,
        technology = <<"gen_statem (BFT)">>,
        description = <<"Byzantine Fault Tolerant consensus engine using gen_statem. "
                        "Each round is a supervised state machine with PQC signatures.">>,
        components = [
            #c4_component{
                id = <<"consensus_supervisor">>,
                name = <<"Consensus Supervisor">>,
                technology = <<"simple_one_for_one">>,
                description = <<"Supervises consensus round processes">>,
                responsibilities = [
                    <<"Start consensus rounds">>,
                    <<"Monitor round health">>,
                    <<"Restart failed rounds">>,
                    <<"Track active rounds">>
                ]
            },
            #c4_component{
                id = <<"consensus_round">>,
                name = <<"Consensus Round">>,
                technology = <<"gen_statem">>,
                description = <<"State machine for a single consensus round (propose/prevote/precommit/commit)">>,
                responsibilities = [
                    <<"Propose blocks">>,
                    <<"Collect prevotes">>,
                    <<"Collect precommits">>,
                    <<"Finalize blocks">>,
                    <<"Handle timeouts">>
                ]
            },
            #c4_component{
                id = <<"validator_set">>,
                name = <<"Validator Set">>,
                technology = <<"ets table">>,
                description = <<"Current and next validator set with voting power">>,
                responsibilities = [
                    <<"Store validator list">>,
                    <<"Calculate voting power">>,
                    <<"Select proposer">>,
                    <<"Track validator status">>
                ]
            },
            #c4_component{
                id = <<"quorum_checker">>,
                name = <<"Quorum Checker">>,
                technology = <<"module">>,
                description = <<"Validates quorum certificates and vote aggregation">>,
                responsibilities = [
                    <<"Verify vote signatures">>,
                    <<"Calculate voting power">>,
                    <<"Check quorum threshold">>,
                    <<"Detect double voting">>
                ]
            }
        ]
    }.

%% Peer Network - P2P with ML-KEM
-spec peer_network() -> #c4_container{}.
peer_network() ->
    #c4_container{
        id = <<"peer_network">>,
        name = <<"Peer Network">>,
        technology = <<"ranch/gun (ML-KEM channels)">>,
        description = <<"Peer-to-peer network with ML-KEM encrypted channels. "
                        "Each peer connection is a supervised process with session keys.">>,
        components = [
            #c4_component{
                id = <<"peer_supervisor">>,
                name = <<"Peer Supervisor">>,
                technology = <<"simple_one_for_one">>,
                description = <<"Supervises peer channel processes">>,
                responsibilities = [
                    <<"Start peer connections">>,
                    <<"Monitor peer health">>,
                    <<"Restart failed connections">>,
                    <<"Track active peers">>
                ]
            },
            #c4_component{
                id = <<"peer_channel">>,
                name = <<"Peer Channel">>,
                technology = <<"gen_server">>,
                description = <<"Secure channel to a single peer with ML-KEM encryption">>,
                responsibilities = [
                    <<"Perform KEM handshake">>,
                    <<"Encrypt/decrypt messages">>,
                    <<"Rekey periodically">>,
                    <<"Handle peer messages">>
                ]
            },
            #c4_component{
                id = <<"peer_discovery">>,
                name = <<"Peer Discovery">>,
                technology = <<"gen_server">>,
                description = <<"Discovers and connects to network peers">>,
                responsibilities = [
                    <<"Bootstrap from seed nodes">>,
                    <<"Exchange peer lists">>,
                    <<"Maintain peer database">>,
                    <<"Score peer quality">>
                ]
            },
            #c4_component{
                id = <<"gossip_protocol">>,
                name = <<"Gossip Protocol">>,
                technology = <<"module">>,
                description = <<"Epidemic broadcast for transactions and blocks">>,
                responsibilities = [
                    <<"Broadcast transactions">>,
                    <<"Broadcast blocks">>,
                    <<"Track message propagation">>,
                    <<"Prevent message loops">>
                ]
            }
        ]
    }.

%% Contract Runtime - Workflow Nets
-spec contract_runtime() -> #c4_container{}.
contract_runtime() ->
    #c4_container{
        id = <<"contract_runtime">>,
        name = <<"Contract Runtime">>,
        technology = <<"SwarmFlow OS (Workflow Nets)">>,
        description = <<"Smart contract runtime using workflow nets (Petri nets with YAWL semantics). "
                        "Each contract instance is a SwarmFlow case with receipts.">>,
        components = [
            #c4_component{
                id = <<"contract_supervisor">>,
                name = <<"Contract Supervisor">>,
                technology = <<"simple_one_for_one">>,
                description = <<"Supervises contract instance processes">>,
                responsibilities = [
                    <<"Start contract instances">>,
                    <<"Monitor execution">>,
                    <<"Restart failed contracts">>,
                    <<"Track active contracts">>
                ]
            },
            #c4_component{
                id = <<"contract_instance">>,
                name = <<"Contract Instance">>,
                technology = <<"SwarmFlow case">>,
                description = <<"Single contract execution as a workflow case">>,
                responsibilities = [
                    <<"Execute transitions">>,
                    <<"Update marking">>,
                    <<"Emit events">>,
                    <<"Generate receipts">>
                ]
            },
            #c4_component{
                id = <<"contract_loader">>,
                name = <<"Contract Loader">>,
                technology = <<"module">>,
                description = <<"Loads and validates contract workflow nets">>,
                responsibilities = [
                    <<"Load workflow definitions">>,
                    <<"Validate Petri net structure">>,
                    <<"Cache compiled contracts">>,
                    <<"Verify code hash">>
                ]
            },
            #c4_component{
                id = <<"gas_meter">>,
                name = <<"Gas Meter">>,
                technology = <<"module">>,
                description = <<"Meters gas consumption during contract execution">>,
                responsibilities = [
                    <<"Track gas usage">>,
                    <<"Enforce gas limits">>,
                    <<"Calculate gas costs">>,
                    <<"Refund unused gas">>
                ]
            }
        ]
    }.

%% Mempool Service - Transaction Pool
-spec mempool_service() -> #c4_container{}.
mempool_service() ->
    #c4_container{
        id = <<"mempool_service">>,
        name = <<"Mempool Service">>,
        technology = <<"gen_server + ets">>,
        description = <<"Transaction pool managing pending transactions. "
                        "Prioritizes by gas price and validates before inclusion.">>,
        components = [
            #c4_component{
                id = <<"mempool_supervisor">>,
                name = <<"Mempool Supervisor">>,
                technology = <<"one_for_all">>,
                description = <<"Supervises mempool server and cache">>,
                responsibilities = [
                    <<"Start mempool server">>,
                    <<"Initialize ets tables">>,
                    <<"Monitor mempool health">>,
                    <<"Coordinate restart">>
                ]
            },
            #c4_component{
                id = <<"mempool_server">>,
                name = <<"Mempool Server">>,
                technology = <<"gen_server">>,
                description = <<"Main mempool logic and transaction management">>,
                responsibilities = [
                    <<"Accept transactions">>,
                    <<"Validate transactions">>,
                    <<"Prioritize by gas price">>,
                    <<"Evict expired transactions">>
                ]
            },
            #c4_component{
                id = <<"tx_validator">>,
                name = <<"Transaction Validator">>,
                technology = <<"module">>,
                description = <<"Validates transaction format and signatures">>,
                responsibilities = [
                    <<"Check signature">>,
                    <<"Verify nonce">>,
                    <<"Check balance">>,
                    <<"Validate payload">>
                ]
            },
            #c4_component{
                id = <<"priority_queue">>,
                name = <<"Priority Queue">>,
                technology = <<"gb_trees">>,
                description = <<"Priority queue ordered by gas price">>,
                responsibilities = [
                    <<"Order by gas price">>,
                    <<"Extract top transactions">>,
                    <<"Support iteration">>,
                    <<"Handle updates">>
                ]
            }
        ]
    }.

%% Chain State DB - Blockchain State
-spec chain_state_db() -> #c4_container{}.
chain_state_db() ->
    #c4_container{
        id = <<"chain_state_db">>,
        name = <<"Chain State DB">>,
        technology = <<"Mnesia/RocksDB (Merkle Patricia)">>,
        description = <<"Blockchain state database with Merkle Patricia tries. "
                        "Stores blocks, transactions, accounts, and contract state.">>,
        components = [
            #c4_component{
                id = <<"block_store">>,
                name = <<"Block Store">>,
                technology = <<"mnesia (disc_copies)">>,
                description = <<"Persistent storage for blocks and headers">>,
                responsibilities = [
                    <<"Store blocks">>,
                    <<"Index by height">>,
                    <<"Index by hash">>,
                    <<"Support reorgs">>
                ]
            },
            #c4_component{
                id = <<"state_trie">>,
                name = <<"State Trie">>,
                technology = <<"Merkle Patricia">>,
                description = <<"Merkle Patricia trie for world state">>,
                responsibilities = [
                    <<"Store account state">>,
                    <<"Generate state root">>,
                    <<"Support proofs">>,
                    <<"Enable rollback">>
                ]
            },
            #c4_component{
                id = <<"receipt_store">>,
                name = <<"Receipt Store">>,
                technology = <<"mnesia (disc_copies)">>,
                description = <<"Execution receipts for all transactions">>,
                responsibilities = [
                    <<"Store receipts">>,
                    <<"Index by transaction">>,
                    <<"Support queries">>,
                    <<"Provide logs">>
                ]
            },
            #c4_component{
                id = <<"chain_indexer">>,
                name = <<"Chain Indexer">>,
                technology = <<"gen_server">>,
                description = <<"Indexes blockchain data for efficient queries">>,
                responsibilities = [
                    <<"Index transactions by sender">>,
                    <<"Index transactions by recipient">>,
                    <<"Index contract events">>,
                    <<"Maintain statistics">>
                ]
            }
        ]
    }.

%% Crypto Services - PQC Cryptography
-spec crypto_services() -> #c4_container{}.
crypto_services() ->
    #c4_container{
        id = <<"crypto_services">>,
        name = <<"Crypto Services">>,
        technology = <<"Rust NIFs (ML-KEM/ML-DSA/SLH-DSA)">>,
        description = <<"Post-quantum cryptography services via Rust NIFs. "
                        "Implements NIST FIPS 203/204/205 standards.">>,
        components = [
            #c4_component{
                id = <<"crypto_nif">>,
                name = <<"Crypto NIF">>,
                technology = <<"Rust NIF">>,
                description = <<"Native interface to PQC cryptography implementations">>,
                responsibilities = [
                    <<"ML-KEM key generation">>,
                    <<"ML-DSA signing/verification">>,
                    <<"SLH-DSA signing/verification">>,
                    <<"SHA3/BLAKE3 hashing">>
                ]
            },
            #c4_component{
                id = <<"crypto_policy_manager">>,
                name = <<"Crypto Policy Manager">>,
                technology = <<"gen_server">>,
                description = <<"Manages crypto agility and policy transitions">>,
                responsibilities = [
                    <<"Load crypto policies">>,
                    <<"Handle canary rollouts">>,
                    <<"Monitor policy metrics">>,
                    <<"Trigger rollbacks">>
                ]
            },
            #c4_component{
                id = <<"key_manager">>,
                name = <<"Key Manager">>,
                technology = <<"gen_server">>,
                description = <<"Manages keypair lifecycle and rotation">>,
                responsibilities = [
                    <<"Generate keypairs">>,
                    <<"Rotate keys">>,
                    <<"Archive old keys">>,
                    <<"Integrate with KMS">>
                ]
            },
            #c4_component{
                id = <<"signature_verifier">>,
                name = <<"Signature Verifier">>,
                technology = <<"module">>,
                description = <<"Batch signature verification with caching">>,
                responsibilities = [
                    <<"Verify signatures">>,
                    <<"Batch verification">>,
                    <<"Cache results">>,
                    <<"Handle hybrid signatures">>
                ]
            }
        ]
    }.

%%% ============================================================================
%%% Level 3: Components within Containers
%%% ============================================================================

-spec components(ContainerId :: binary()) -> [#c4_component{}] | {error, not_found}.
components(ContainerId) ->
    case find_container(ContainerId) of
        {ok, Container} ->
            Container#c4_container.components;
        error ->
            {error, not_found}
    end.

%% Helper to find a container by ID
-spec find_container(binary()) -> {ok, #c4_container{}} | error.
find_container(ContainerId) ->
    Containers = containers(),
    case lists:keyfind(ContainerId, #c4_container.id, Containers) of
        false -> error;
        Container -> {ok, Container}
    end.

%%% ============================================================================
%%% Relationships
%%% ============================================================================

-spec relationships() -> [#c4_relationship{}].
relationships() ->
    [
        %% External actors to system
        #c4_relationship{
            source = <<"human_user">>,
            target = <<"a2a_face">>,
            description = <<"Submits transactions via wallet or dApp">>,
            technology = <<"HTTPS/JSON">>,
            bidirectional = false
        },
        #c4_relationship{
            source = <<"autonomous_agent">>,
            target = <<"a2a_face">>,
            description = <<"Executes tasks and receives artifacts">>,
            technology = <<"A2A Protocol (HTTP/SSE)">>,
            bidirectional = true
        },
        #c4_relationship{
            source = <<"mcp_client">>,
            target = <<"mcp_face">>,
            description = <<"Calls tools and queries resources">>,
            technology = <<"MCP (JSON-RPC 2.0)">>,
            bidirectional = true
        },
        #c4_relationship{
            source = <<"validator_operator">>,
            target = <<"consensus_engine">>,
            description = <<"Operates validator node">>,
            technology = <<"Admin API">>,
            bidirectional = false
        },
        #c4_relationship{
            source = <<"contract_developer">>,
            target = <<"contract_runtime">>,
            description = <<"Deploys workflow-based contracts">>,
            technology = <<"SwarmFlow DSL">>,
            bidirectional = false
        },

        %% Faces to Kernel
        #c4_relationship{
            source = <<"a2a_face">>,
            target = <<"case_kernel">>,
            description = <<"Routes to registered services">>,
            technology = <<"Erlang messages (gproc)">>,
            bidirectional = true
        },
        #c4_relationship{
            source = <<"mcp_face">>,
            target = <<"case_kernel">>,
            description = <<"Routes to registered services">>,
            technology = <<"Erlang messages (gproc)">>,
            bidirectional = true
        },

        %% Kernel to Services
        #c4_relationship{
            source = <<"case_kernel">>,
            target = <<"consensus_engine">>,
            description = <<"Supervises and coordinates consensus">>,
            technology = <<"OTP supervision">>,
            bidirectional = true
        },
        #c4_relationship{
            source = <<"case_kernel">>,
            target = <<"peer_network">>,
            description = <<"Supervises peer connections">>,
            technology = <<"OTP supervision">>,
            bidirectional = true
        },
        #c4_relationship{
            source = <<"case_kernel">>,
            target = <<"contract_runtime">>,
            description = <<"Supervises contract execution">>,
            technology = <<"OTP supervision">>,
            bidirectional = true
        },
        #c4_relationship{
            source = <<"case_kernel">>,
            target = <<"mempool_service">>,
            description = <<"Supervises mempool">>,
            technology = <<"OTP supervision">>,
            bidirectional = true
        },

        %% Consensus to other services
        #c4_relationship{
            source = <<"consensus_engine">>,
            target = <<"peer_network">>,
            description = <<"Broadcasts consensus messages">>,
            technology = <<"Gossip protocol">>,
            bidirectional = true
        },
        #c4_relationship{
            source = <<"consensus_engine">>,
            target = <<"mempool_service">>,
            description = <<"Fetches transactions for blocks">>,
            technology = <<"gen_server:call">>,
            bidirectional = false
        },
        #c4_relationship{
            source = <<"consensus_engine">>,
            target = <<"contract_runtime">>,
            description = <<"Executes contract transactions">>,
            technology = <<"gen_server:call">>,
            bidirectional = false
        },
        #c4_relationship{
            source = <<"consensus_engine">>,
            target = <<"chain_state_db">>,
            description = <<"Commits finalized blocks">>,
            technology = <<"mnesia transactions">>,
            bidirectional = false
        },
        #c4_relationship{
            source = <<"consensus_engine">>,
            target = <<"crypto_services">>,
            description = <<"Verifies signatures and quorum">>,
            technology = <<"NIF calls">>,
            bidirectional = false
        },

        %% Peer network to other services
        #c4_relationship{
            source = <<"peer_network">>,
            target = <<"crypto_services">>,
            description = <<"ML-KEM handshake and encryption">>,
            technology = <<"NIF calls">>,
            bidirectional = false
        },
        #c4_relationship{
            source = <<"peer_network">>,
            target = <<"mempool_service">>,
            description = <<"Relays transactions from peers">>,
            technology = <<"gen_server:cast">>,
            bidirectional = false
        },
        #c4_relationship{
            source = <<"peer_network">>,
            target = <<"chain_state_db">>,
            description = <<"Syncs blocks from peers">>,
            technology = <<"gen_server:call">>,
            bidirectional = false
        },

        %% Contract runtime to other services
        #c4_relationship{
            source = <<"contract_runtime">>,
            target = <<"chain_state_db">>,
            description = <<"Reads/writes contract state">>,
            technology = <<"mnesia transactions">>,
            bidirectional = true
        },
        #c4_relationship{
            source = <<"contract_runtime">>,
            target = <<"swarmflow_os">>,
            description = <<"Executes workflow transitions">>,
            technology = <<"Erlang API">>,
            bidirectional = false
        },

        %% Mempool to other services
        #c4_relationship{
            source = <<"mempool_service">>,
            target = <<"crypto_services">>,
            description = <<"Validates transaction signatures">>,
            technology = <<"NIF calls">>,
            bidirectional = false
        },
        #c4_relationship{
            source = <<"mempool_service">>,
            target = <<"chain_state_db">>,
            description = <<"Checks account nonces and balances">>,
            technology = <<"mnesia read">>,
            bidirectional = false
        },

        %% External system integrations
        #c4_relationship{
            source = <<"crypto_services">>,
            target = <<"key_management_system">>,
            description = <<"Stores and retrieves keys">>,
            technology = <<"PKCS#11 or KMS API">>,
            bidirectional = false
        },
        #c4_relationship{
            source = <<"case_kernel">>,
            target = <<"monitoring_system">>,
            description = <<"Exports metrics, logs, and traces">>,
            technology = <<"OpenTelemetry">>,
            bidirectional = false
        },
        #c4_relationship{
            source = <<"peer_network">>,
            target = <<"other_blockchains">>,
            description = <<"Cross-chain message relay">>,
            technology = <<"IBC or custom bridge">>,
            bidirectional = true
        },
        #c4_relationship{
            source = <<"contract_runtime">>,
            target = <<"ipfs">>,
            description = <<"Stores large artifacts">>,
            technology = <<"IPFS HTTP API">>,
            bidirectional = false
        }
    ].

%%% ============================================================================
%%% Export: PlantUML C4 Diagrams
%%% ============================================================================

-spec to_plantuml(Level :: context | container | component) -> iolist().
to_plantuml(context) ->
    System = context(),
    Actors = external_actors(),
    ExternalSystems = external_systems(),
    Relationships = relationships(),

    [
        <<"@startuml\n">>,
        <<"!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Context.puml\n\n">>,
        <<"LAYOUT_WITH_LEGEND()\n\n">>,
        <<"title System Context diagram for SwarmFlow PQChain\n\n">>,

        %% Actors
        [[format_plantuml_person(A), <<"\n">>] || A <- Actors],
        <<"\n">>,

        %% Main system
        <<"System(pqchain, \"", (System#c4_system.name)/binary, "\", \"",
          (System#c4_system.description)/binary, "\")\n\n">>,

        %% External systems
        [[<<"System_Ext(", (S#c4_system.id)/binary, ", \"", (S#c4_system.name)/binary,
           "\", \"", (S#c4_system.description)/binary, "\")\n">>] || S <- ExternalSystems],
        <<"\n">>,

        %% Relationships (only context-level)
        [format_plantuml_relationship(R) || R <- Relationships,
                                              is_context_relationship(R)],

        <<"@enduml\n">>
    ];

to_plantuml(container) ->
    System = context(),
    Containers = System#c4_system.containers,
    Relationships = relationships(),

    [
        <<"@startuml\n">>,
        <<"!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Container.puml\n\n">>,
        <<"LAYOUT_WITH_LEGEND()\n\n">>,
        <<"title Container diagram for SwarmFlow PQChain\n\n">>,

        <<"System_Boundary(pqchain, \"SwarmFlow PQChain\") {\n">>,
        [[<<"  Container(", (C#c4_container.id)/binary, ", \"", (C#c4_container.name)/binary,
           "\", \"", (C#c4_container.technology)/binary, "\", \"",
           (C#c4_container.description)/binary, "\")\n">>] || C <- Containers],
        <<"}\n\n">>,

        %% External systems
        <<"System_Ext(swarmflow_os, \"SwarmFlow OS\", \"Workflow runtime\")\n">>,
        <<"System_Ext(key_management_system, \"Key Management\", \"HSM/KMS\")\n">>,
        <<"System_Ext(monitoring_system, \"Monitoring\", \"Observability\")\n\n">>,

        %% Relationships
        [format_plantuml_relationship(R) || R <- Relationships,
                                              is_container_relationship(R)],

        <<"@enduml\n">>
    ];

to_plantuml(component) ->
    %% Generate component diagram for consensus engine (example)
    {ok, Container} = find_container(<<"consensus_engine">>),
    Components = Container#c4_container.components,

    [
        <<"@startuml\n">>,
        <<"!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Component.puml\n\n">>,
        <<"LAYOUT_WITH_LEGEND()\n\n">>,
        <<"title Component diagram for Consensus Engine\n\n">>,

        <<"Container_Boundary(consensus_engine, \"Consensus Engine\") {\n">>,
        [[<<"  Component(", (C#c4_component.id)/binary, ", \"", (C#c4_component.name)/binary,
           "\", \"", (C#c4_component.technology)/binary, "\", \"",
           (C#c4_component.description)/binary, "\")\n">>] || C <- Components],
        <<"}\n\n">>,

        <<"Container(peer_network, \"Peer Network\", \"ranch/gun\")\n">>,
        <<"Container(mempool_service, \"Mempool\", \"gen_server\")\n">>,
        <<"Container(chain_state_db, \"Chain State\", \"Mnesia\")\n">>,
        <<"Container(crypto_services, \"Crypto\", \"Rust NIFs\")\n\n">>,

        %% Component relationships
        <<"Rel(consensus_supervisor, consensus_round, \"Supervises\")\n">>,
        <<"Rel(consensus_round, validator_set, \"Reads\")\n">>,
        <<"Rel(consensus_round, quorum_checker, \"Uses\")\n">>,
        <<"Rel(quorum_checker, crypto_services, \"Verifies signatures\")\n">>,
        <<"Rel(consensus_round, peer_network, \"Broadcasts votes\")\n">>,
        <<"Rel(consensus_round, mempool_service, \"Fetches transactions\")\n">>,
        <<"Rel(consensus_round, chain_state_db, \"Commits blocks\")\n">>,

        <<"@enduml\n">>
    ].

-spec format_plantuml_person(#c4_person{}) -> iolist().
format_plantuml_person(#c4_person{id = Id, name = Name, description = Desc}) ->
    [<<"Person(", Id/binary, ", \"", Name/binary, "\", \"", Desc/binary, "\")">>].

-spec format_plantuml_relationship(#c4_relationship{}) -> iolist().
format_plantuml_relationship(#c4_relationship{
    source = Source,
    target = Target,
    description = Desc,
    technology = Tech,
    bidirectional = Bidir
}) ->
    TechStr = case Tech of
        undefined -> <<"">>;
        T -> [<<", \"">>, T, <<"\"">>]
    end,
    RelType = case Bidir of
        true -> <<"BiRel">>;
        false -> <<"Rel">>
    end,
    [RelType, <<"(", Source/binary, ", ", Target/binary, ", \"", Desc/binary, "\"">>],
    TechStr,
    <<")\n">>].

-spec is_context_relationship(#c4_relationship{}) -> boolean().
is_context_relationship(#c4_relationship{source = Source, target = Target}) ->
    Actors = [A#c4_person.id || A <- external_actors()],
    ExternalSystems = [S#c4_system.id || S <- external_systems()],
    lists:member(Source, Actors) orelse
    lists:member(Target, Actors) orelse
    lists:member(Source, ExternalSystems) orelse
    lists:member(Target, ExternalSystems).

-spec is_container_relationship(#c4_relationship{}) -> boolean().
is_container_relationship(#c4_relationship{source = Source, target = Target}) ->
    Containers = [C#c4_container.id || C <- containers()],
    ExternalSystems = [<<"swarmflow_os">>, <<"key_management_system">>,
                       <<"monitoring_system">>, <<"other_blockchains">>, <<"ipfs">>],
    (lists:member(Source, Containers) orelse lists:member(Source, ExternalSystems)) andalso
    (lists:member(Target, Containers) orelse lists:member(Target, ExternalSystems)).

%%% ============================================================================
%%% Export: Structurizr DSL
%%% ============================================================================

-spec to_structurizr_dsl() -> iolist().
to_structurizr_dsl() ->
    System = context(),
    Actors = external_actors(),
    ExternalSystems = external_systems(),
    Containers = System#c4_system.containers,
    Relationships = relationships(),

    [
        <<"workspace \"SwarmFlow PQChain\" \"Post-quantum blockchain with A2A and MCP\" {\n\n">>,
        <<"  model {\n">>,

        %% People
        [[<<"    ", (A#c4_person.id)/binary, " = person \"", (A#c4_person.name)/binary,
           "\" \"", (A#c4_person.description)/binary, "\"\n">>] || A <- Actors],
        <<"\n">>,

        %% Main system
        <<"    pqchain = softwareSystem \"", (System#c4_system.name)/binary, "\" \"",
          (System#c4_system.description)/binary, "\" {\n">>,

        %% Containers
        [[<<"      ", (C#c4_container.id)/binary, " = container \"", (C#c4_container.name)/binary,
           "\" \"", (C#c4_container.description)/binary, "\" \"",
           (C#c4_container.technology)/binary, "\"\n">>] || C <- Containers],
        <<"    }\n\n">>,

        %% External systems
        [[<<"    ", (S#c4_system.id)/binary, " = softwareSystem \"", (S#c4_system.name)/binary,
           "\" \"", (S#c4_system.description)/binary, "\" {\n      tags \"External\"\n    }\n">>]
         || S <- ExternalSystems],
        <<"\n">>,

        %% Relationships
        [format_structurizr_relationship(R) || R <- Relationships],

        <<"  }\n\n">>,

        %% Views
        <<"  views {\n">>,
        <<"    systemContext pqchain \"SystemContext\" {\n">>,
        <<"      include *\n">>,
        <<"      autoLayout\n">>,
        <<"    }\n\n">>,
        <<"    container pqchain \"Containers\" {\n">>,
        <<"      include *\n">>,
        <<"      autoLayout\n">>,
        <<"    }\n\n">>,
        <<"    theme default\n">>,
        <<"  }\n">>,
        <<"}\n">>
    ].

-spec format_structurizr_relationship(#c4_relationship{}) -> iolist().
format_structurizr_relationship(#c4_relationship{
    source = Source,
    target = Target,
    description = Desc,
    technology = Tech
}) ->
    TechStr = case Tech of
        undefined -> <<"">>;
        T -> [<<" \"">>, T, <<"\"">>]
    end,
    [<<"    ", Source/binary, " -> ", Target/binary, " \"", Desc/binary, "\"">>],
    TechStr,
    <<"\n">>.

%%% ============================================================================
%%% Export: Mermaid Diagrams
%%% ============================================================================

-spec to_mermaid(Level :: context | container) -> iolist().
to_mermaid(context) ->
    System = context(),
    Actors = external_actors(),
    ExternalSystems = external_systems(),
    Relationships = relationships(),

    [
        <<"graph TB\n">>,
        <<"  classDef person fill:#08427b\n">>,
        <<"  classDef system fill:#1168bd\n">>,
        <<"  classDef external fill:#999999\n\n">>,

        %% Actors
        [[<<"  ", (A#c4_person.id)/binary, "[\"", (A#c4_person.name)/binary, "\"]\n">>,
          <<"  class ", (A#c4_person.id)/binary, " person\n">>] || A <- Actors],
        <<"\n">>,

        %% Main system
        <<"  pqchain[\"", (System#c4_system.name)/binary, "\"]\n">>,
        <<"  class pqchain system\n\n">>,

        %% External systems
        [[<<"  ", (S#c4_system.id)/binary, "[\"", (S#c4_system.name)/binary, "\"]\n">>,
          <<"  class ", (S#c4_system.id)/binary, " external\n">>] || S <- ExternalSystems],
        <<"\n">>,

        %% Relationships
        [format_mermaid_relationship(R) || R <- Relationships,
                                            is_context_relationship(R)],
        <<"\n">>
    ];

to_mermaid(container) ->
    System = context(),
    Containers = System#c4_system.containers,
    Relationships = relationships(),

    [
        <<"graph TB\n">>,
        <<"  classDef container fill:#438dd5\n">>,
        <<"  classDef external fill:#999999\n\n">>,

        %% Containers
        [[<<"  ", (C#c4_container.id)/binary, "[\"", (C#c4_container.name)/binary, "<br/>",
           (C#c4_container.technology)/binary, "\"]\n">>,
          <<"  class ", (C#c4_container.id)/binary, " container\n">>] || C <- Containers],
        <<"\n">>,

        %% External systems
        <<"  swarmflow_os[\"SwarmFlow OS\"]\n">>,
        <<"  class swarmflow_os external\n\n">>,

        %% Relationships
        [format_mermaid_relationship(R) || R <- Relationships,
                                            is_container_relationship(R)],
        <<"\n">>
    ].

-spec format_mermaid_relationship(#c4_relationship{}) -> iolist().
format_mermaid_relationship(#c4_relationship{
    source = Source,
    target = Target,
    description = Desc,
    bidirectional = Bidir
}) ->
    Arrow = case Bidir of
        true -> <<"<-->">>;
        false -> <<"-->">>
    end,
    [<<"  ", Source/binary, " ", Arrow/binary, " ", Target/binary,
      ": ", Desc/binary, "\n">>].

%%% ============================================================================
%%% Model Validation
%%% ============================================================================

-spec validate_model() -> ok | {error, [term()]}.
validate_model() ->
    Errors = lists:flatten([
        validate_unique_ids(),
        validate_relationships(),
        validate_container_components()
    ]),

    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

-spec validate_unique_ids() -> [term()].
validate_unique_ids() ->
    AllIds = get_all_ids(),
    Duplicates = AllIds -- lists:usort(AllIds),
    case Duplicates of
        [] -> [];
        _ -> [{duplicate_ids, lists:usort(Duplicates)}]
    end.

-spec validate_relationships() -> [term()].
validate_relationships() ->
    AllIds = sets:from_list(get_all_ids()),
    Relationships = relationships(),

    lists:filtermap(
        fun(#c4_relationship{source = Source, target = Target}) ->
            case {sets:is_element(Source, AllIds), sets:is_element(Target, AllIds)} of
                {true, true} -> false;
                {false, _} -> {true, {invalid_relationship, Source, not_found}};
                {_, false} -> {true, {invalid_relationship, Target, not_found}}
            end
        end,
        Relationships
    ).

-spec validate_container_components() -> [term()].
validate_container_components() ->
    Containers = containers(),
    lists:filtermap(
        fun(#c4_container{id = Id, components = Components}) ->
            case Components of
                [] -> {true, {empty_container, Id}};
                _ -> false
            end
        end,
        Containers
    ).

%%% ============================================================================
%%% Helper Functions
%%% ============================================================================

-spec get_all_ids() -> [binary()].
get_all_ids() ->
    ActorIds = [A#c4_person.id || A <- external_actors()],
    SystemIds = [S#c4_system.id || S <- [context() | external_systems()]],
    ContainerIds = [C#c4_container.id || C <- containers()],
    ComponentIds = lists:flatten([
        [Comp#c4_component.id || Comp <- C#c4_container.components]
        || C <- containers()
    ]),

    ActorIds ++ SystemIds ++ ContainerIds ++ ComponentIds.

-spec find_element(Type :: atom(), Id :: binary()) ->
    {ok, term()} | {error, not_found}.
find_element(person, Id) ->
    case lists:keyfind(Id, #c4_person.id, external_actors()) of
        false -> {error, not_found};
        Person -> {ok, Person}
    end;
find_element(system, Id) ->
    AllSystems = [context() | external_systems()],
    case lists:keyfind(Id, #c4_system.id, AllSystems) of
        false -> {error, not_found};
        System -> {ok, System}
    end;
find_element(container, Id) ->
    find_container(Id);
find_element(component, Id) ->
    AllComponents = lists:flatten([
        C#c4_container.components || C <- containers()
    ]),
    case lists:keyfind(Id, #c4_component.id, AllComponents) of
        false -> {error, not_found};
        Component -> {ok, Component}
    end.
