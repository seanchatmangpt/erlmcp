%%%-----------------------------------------------------------------------------
%%% @doc TCPS Rebar3 Plugin Entry Point
%%%
%%% Main plugin module that registers all TCPS providers with rebar3:
%%% - tcps_rebar3_shacl - SHACL validation
%%% - tcps_rebar3_receipt - Receipt generation
%%% - tcps_rebar3_andon - Andon event management
%%% - tcps_rebar3_quality - Quality gates enforcement
%%%
%%% This plugin integrates the Toyota Code Production System (TCPS)
%%% into the rebar3 build pipeline, enforcing zero-defect quality
%%% standards at every stage.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(rebar3_tcps_plugin).

-export([init/1]).

%%%=============================================================================
%%% Plugin Initialization
%%%=============================================================================

%% @doc Initialize the TCPS plugin
%% Called by rebar3 when the plugin is loaded
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    %% Print banner
    rebar_api:info("~n"
                   "╔═══════════════════════════════════════════════════════════╗~n"
                   "║  TCPS - Toyota Code Production System for Rebar3         ║~n"
                   "║  Zero-Defect Quality Enforcement                          ║~n"
                   "╚═══════════════════════════════════════════════════════════╝~n~n",
                   []),

    %% Initialize all TCPS providers
    {ok, State1} = tcps_rebar3_shacl:init(State),
    {ok, State2} = tcps_rebar3_receipt:init(State1),
    {ok, State3} = tcps_rebar3_andon:init(State2),
    {ok, State4} = tcps_rebar3_quality:init(State3),

    rebar_api:info("TCPS providers loaded:~n"
                   "  - tcps shacl_validate      SHACL ontology validation~n"
                   "  - tcps generate_receipt    Production receipt generation~n"
                   "  - tcps andon               Andon event management~n"
                   "  - tcps check_quality_gates Quality gates enforcement~n~n",
                   []),

    {ok, State4}.
