%% @doc erlmcp Compliance Application
%% Comprehensive compliance framework for Fortune 500 requirements
%% Supports SOC2, HIPAA, GDPR, ISO27001, and industry-specific compliance
-module(erlmcp_compliance_app).

-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).

%%====================================================================
%% Application API
%%====================================================================

start(_Type, _Args) ->
    case erlmcp_compliance_sup:start_link() of
        {ok, Pid} ->
            erlmcp_compliance_metrics:start(),
            erlmcp_compliance_policy_manager:start(),
            erlmcp_compliance_audit_logger:start(),
            erlmcp_compliance_monitor:start(),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    erlmcp_compliance_metrics:stop(),
    ok.

prep_stop(State) ->
    logger:info("Preparing erlmcp_compliance for graceful shutdown"),
    %% Finalize compliance audit records
    try
        erlmcp_compliance_audit_logger:flush()
    catch
        _:_ -> ok
    end,
    State.