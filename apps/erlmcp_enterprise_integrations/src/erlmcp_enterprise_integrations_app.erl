%% @doc erlmcp Enterprise Integrations Application
%% This application provides enterprise-grade integrations for Fortune 500 systems
-module(erlmcp_enterprise_integrations_app).

-behaviour(application).

-export([start/2, stop/1, prep_stop/1]).

%% OTP application callbacks
start(_Type, _Args) ->
    %% Start the top-level supervisor
    erlmcp_enterprise_integrations_sup:start_link().

stop(_State) ->
    ok.

prep_stop(State) ->
    logger:info("Preparing erlmcp_enterprise_integrations for graceful shutdown"),
    %% Close external connections gracefully
    try
        erlmcp_enterprise_integrations_sup:shutdown_connections()
    catch
        _:_ -> ok
    end,
    State.