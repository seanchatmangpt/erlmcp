%%%-------------------------------------------------------------------
%% @doc MCP+ Governance Supervisor - OTP Supervision Tree
%%
%% Supervises all governance components:
%% - Contract registry (erlmcp_contract)
%% - Envelope enforcer (erlmcp_envelope)
%% - Receipt manager (erlmcp_receipt)
%% - Evidence bundler (erlmcp_evidence_bundle)
%% - Verifier (erlmcp_verifier)
%% - Kill switch (erlmcp_kill_switch)
%%
%% Uses one_for_all strategy: if any component fails, restart all
%% to ensure consistent governance state.
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_governance_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% Types
%%====================================================================

-type governance_config() :: #{
    enabled => boolean(),
    contract_authority => binary(),
    default_envelope => map(),
    text_blind => boolean(),
    receipt_chain_depth => pos_integer(),
    bundle_interval_ms => pos_integer()
}.

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(governance_config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Config]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 5,
        period => 60
    },

    %% Only start governance components if enabled
    case maps:get(enabled, Config, true) of
        false ->
            {ok, {SupFlags, []}};
        true ->
            ChildSpecs = [
                %% Contract Registry - must start first
                #{
                    id => erlmcp_contract,
                    start => {erlmcp_contract, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_contract]
                },

                %% Envelope Enforcer
                #{
                    id => erlmcp_envelope,
                    start => {erlmcp_envelope, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_envelope]
                },

                %% Receipt Manager
                #{
                    id => erlmcp_receipt,
                    start => {erlmcp_receipt, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_receipt]
                },

                %% Evidence Bundler
                #{
                    id => erlmcp_evidence_bundle,
                    start => {erlmcp_evidence_bundle, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_evidence_bundle]
                },

                %% Verifier
                #{
                    id => erlmcp_verifier,
                    start => {erlmcp_verifier, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_verifier]
                },

                %% Kill Switch - critical, must be available
                #{
                    id => erlmcp_kill_switch,
                    start => {erlmcp_kill_switch, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_kill_switch]
                },

                %% Governance Coordinator
                #{
                    id => erlmcp_governance,
                    start => {erlmcp_governance, start_link, [Config]},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_governance]
                }
            ],

            {ok, {SupFlags, ChildSpecs}}
    end.
