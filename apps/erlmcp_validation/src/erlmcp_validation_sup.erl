%%%-------------------------------------------------------------------
%%% @doc erlmcp_validation top-level supervisor
%%%
%%% Supervises validation infrastructure processes:
%%% - erlmcp_compliance_report: Compliance report generation and caching
%%% - erlmcp_memory_manager: Memory management and optimization
%%% - erlmcp_sbom: SBOM generation for supply chain security
%%% - erlmcp_vulnerability_scanner: Vulnerability scanning for CVEs
%%%
%%% Uses one_for_one strategy - if one worker fails, only that worker
%%% is restarted. Independent workers that don't depend on each other.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_validation_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%====================================================================
%%% API functions
%%%====================================================================

%% @doc Start the supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%====================================================================
%%% Supervisor callbacks
%%%====================================================================

%% @doc Initialize the supervisor
%%
%% Strategy: one_for_one
%%   - If a child process terminates, only that process is restarted
%%   - Suitable for independent workers
%%
%% Children:
%%   1. erlmcp_compliance_report - Report generation and caching
%%   2. erlmcp_memory_manager - Memory management and optimization
%%   3. erlmcp_sbom - SBOM generation for supply chain security
%%   4. erlmcp_vulnerability_scanner - Vulnerability scanning for CVEs
%%
%% Note: erlmcp_test_client is not supervised here as it's started
%% dynamically via start_test_client/2 API calls.
%%
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 10,
          period => 60},

    %% Child specifications
    ChildSpecs =
        [%% Compliance report server - generates and caches compliance reports
         #{id => erlmcp_compliance_report,
           start => {erlmcp_compliance_report, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_compliance_report]},
         %% Memory manager - manages memory usage and caching
         #{id => erlmcp_memory_manager,
           start => {erlmcp_memory_manager, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_memory_manager]},
         %% SBOM generator - generates SPDX 2.3 SBOM for supply chain security
         #{id => erlmcp_sbom,
           start => {erlmcp_sbom, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_sbom]},
         %% Vulnerability scanner - scans SBOM for CVEs and security issues
         #{id => erlmcp_vulnerability_scanner,
           start => {erlmcp_vulnerability_scanner, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_vulnerability_scanner]}],

    {ok, {SupFlags, ChildSpecs}}.

%%%====================================================================
%%% Internal functions
%%%====================================================================
