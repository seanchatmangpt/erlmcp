%%%-------------------------------------------------------------------
%%% @doc SwarmFlow PQChain Top-Level Supervisor
%%%
%%% Root supervisor for post-quantum blockchain runtime.
%%% Manages core blockchain services in a one_for_one supervision strategy.
%%%
%%% Architecture (start order matters for dependencies):
%%% 1. pqc_identity          - Identity registry (worker)
%%% 2. pqc_crypto_policy     - Crypto policy engine (worker)
%%% 3. pqc_mempool_sup       - Mempool management (supervisor)
%%% 4. pqc_consensus_sup     - Consensus rounds (supervisor)
%%% 5. pqc_peer_sup          - Peer channels (supervisor)
%%% 6. pqc_contract_sup      - Contract instances (supervisor)
%%% 7. pqc_a2a_bridge        - A2A bridge (worker)
%%% 8. pqc_mcp_bridge        - MCP bridge (worker)
%%% 9. pqc_chain             - Chain state management (worker)
%%%
%%% Restart Strategy:
%%% - one_for_one: Each child can restart independently
%%% - Allows continued operation if non-critical services restart
%%% - Critical services (identity, crypto_policy) start first
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(swarmflow_pqchain_sup).

-behaviour(supervisor).

-include("pqchain.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Restart intensity limits - conservative for blockchain consensus
-define(MAX_RESTARTS, 10).
-define(RESTART_PERIOD, 60).  % seconds

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the top-level supervisor
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%% @private
%% @doc Supervisor callback
%%
%% Initializes the supervision tree with all PQChain components.
%% Order is critical - dependencies must start before dependents.
%% @end
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => ?MAX_RESTARTS,
        period => ?RESTART_PERIOD,
        auto_shutdown => never
    },

    %% Identity registry - manages PQC identities and key registration
    %% Must start first as other components depend on identity lookups
    Identity = #{
        id => pqc_identity,
        start => {pqc_identity, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [pqc_identity]
    },

    %% Crypto policy engine - manages crypto-agility and algorithm transitions
    %% Critical for signature/KEM verification across the system
    CryptoPolicy = #{
        id => pqc_crypto_policy,
        start => {pqc_crypto_policy, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [pqc_crypto_policy]
    },

    %% Mempool supervisor - manages transaction mempool
    %% Simple_one_for_one for per-sender mempool partitions if needed
    MempoolSup = #{
        id => pqc_mempool_sup,
        start => {pqc_mempool_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,  % Supervisor - wait for children
        type => supervisor,
        modules => [pqc_mempool_sup]
    },

    %% Consensus supervisor - manages consensus round processes
    %% Each round is a supervised gen_statem with BFT state machine
    ConsensusSup = #{
        id => pqc_consensus_sup,
        start => {pqc_consensus_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,  % Supervisor - wait for children
        type => supervisor,
        modules => [pqc_consensus_sup]
    },

    %% Peer supervisor - manages secure ML-KEM peer channels
    %% Each peer connection runs as a supervised process
    PeerSup = #{
        id => pqc_peer_sup,
        start => {pqc_peer_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,  % Supervisor - wait for children
        type => supervisor,
        modules => [pqc_peer_sup]
    },

    %% Contract supervisor - manages workflow net contract instances
    %% Each contract runs as a SwarmFlow case under this supervisor
    ContractSup = #{
        id => pqc_contract_sup,
        start => {pqc_contract_sup, start_link, []},
        restart => permanent,
        shutdown => infinity,  % Supervisor - wait for children
        type => supervisor,
        modules => [pqc_contract_sup]
    },

    %% Chain state manager - maintains current blockchain state
    %% Single source of truth for current height, validator set, etc.
    Chain = #{
        id => pqc_block,
        start => {pqc_chain, start_link, []},
        restart => permanent,
        shutdown => 10000,  % Allow time to persist state
        type => worker,
        modules => [pqc_chain]
    },

    %% Build child spec list, conditionally including optional bridges
    ChildSpecs = build_child_specs([
        Identity,
        CryptoPolicy,
        MempoolSup,
        ConsensusSup,
        PeerSup,
        ContractSup,
        Chain
    ]),

    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Build the final child specs list, adding optional modules if present
build_child_specs(BaseSpecs) ->
    %% Add A2A bridge if module exists
    Specs1 = maybe_add_child(pqc_a2a_bridge, a2a_bridge_spec(), BaseSpecs),

    %% Add MCP bridge if module exists
    Specs2 = maybe_add_child(pqc_mcp_bridge, mcp_bridge_spec(), Specs1),

    Specs2.

%% @private
%% @doc Add a child spec if the module is available
maybe_add_child(Module, ChildSpec, Specs) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            Specs ++ [ChildSpec];
        {error, _} ->
            %% Module not available - skip
            Specs
    end.

%% @private
%% @doc Child spec for A2A bridge (optional)
%% Bridges PQChain with A2A agent protocol for cross-agent transactions
a2a_bridge_spec() ->
    #{
        id => pqc_a2a_bridge,
        start => {pqc_a2a_bridge, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [pqc_a2a_bridge]
    }.

%% @private
%% @doc Child spec for MCP bridge (optional)
%% Bridges PQChain with MCP for blockchain tools/resources
mcp_bridge_spec() ->
    #{
        id => pqc_mcp_bridge,
        start => {pqc_mcp_bridge, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [pqc_mcp_bridge]
    }.
