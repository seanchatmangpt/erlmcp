%% @doc erlmcp Compliance Supervisor
%% 3-tier supervision for compliance components
-module(erlmcp_compliance_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILDREN, [
    %% TIER 1: One-for-all critical services
    {erlmcp_compliance_policy_manager, {erlmcp_compliance_policy_manager, start_link, []},
     permanent, 5000, worker, [erlmcp_compliance_policy_manager]},

    {erlmcp_compliance_audit_logger, {erlmcp_compliance_audit_logger, start_link, []},
     permanent, 5000, worker, [erlmcp_compliance_audit_logger]},

    {erlmcp_compliance_monitor, {erlmcp_compliance_monitor, start_link, []},
     permanent, 5000, worker, [erlmcp_compliance_monitor]},

    %% TIER 2: One-for-one services
    {erlmcp_compliance_reporter, {erlmcp_compliance_reporter, start_link, []},
     permanent, 5000, worker, [erlmcp_compliance_reporter]},

    {erlmcp_compliance_enforcer, {erlmcp_compliance_enforcer, start_link, []},
     permanent, 5000, worker, [erlmcp_compliance_enforcer]},

    %% TIER 3: Isolated services
    {erlmcp_compliance_analyzer, {erlmcp_compliance_analyzer, start_link, []},
     temporary, 5000, worker, [erlmcp_compliance_analyzer]}
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_all, 10, 3600}, ?CHILDREN}}.