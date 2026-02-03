%% @doc Compliance Framework Supervisor
%% Manages compliance-related processes including monitoring, reporting, and enforcement
%%
%% Responsibilities:
%% - Supervise compliance monitoring processes
%% - Manage compliance reporting services
%% - Oversee incident response coordination
%% - Ensure continuous compliance checking
%% - Coordinate with other OTP supervisors
%%
%% @end
-module(erlmcp_compliance_supervisor).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the compliance supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

init([]) ->
    %% Define child processes for compliance framework
    Children = [
        %% Main compliance framework server
        #{
            id => erlmcp_compliance_framework,
            start => {erlmcp_compliance_framework, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_compliance_framework]
        },
        %% Compliance monitoring service
        #{
            id => erlmcp_compliance_monitor,
            start => {erlmcp_compliance_monitor, start_link, []},
            restart => permanent,
            shutdown => 3000,
            type => worker,
            modules => [erlmcp_compliance_monitor]
        },
        %% Compliance reporting service
        #{
            id => erlmcp_compliance_reporter,
            start => {erlmcp_compliance_reporter, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_compliance_reporter]
        },
        %% Incident response coordinator
        #{
            id => erlmcp_incident_response,
            start => {erlmcp_incident_response, start_link, []},
            restart => permanent,
            shutdown => 3000,
            type => worker,
            modules => [erlmcp_incident_response]
        },
        %% Policy enforcement service
        #{
            id => erlmcp_policy_enforcer,
            start => {erlmcp_policy_enforcer, start_link, []},
            restart => permanent,
            shutdown => 3000,
            type => worker,
            modules => [erlmcp_policy_enforcer]
        },
        %% Continuous compliance checker
        #{
            id => erlmcp_continuous_compliance,
            start => {erlmcp_continuous_compliance, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_continuous_compliance]
        }
    ],

    %% Strategy: One-for-all (restart all children if one crashes)
    Strategy = #{strategy => one_for_all,
                intensity => 1,
                period => 5},

    {ok, {Strategy, Children}}.

%%====================================================================
%% End of File
%%====================================================================