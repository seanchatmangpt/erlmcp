%%%-------------------------------------------------------------------
%%% @doc Armstrong Supervisor Usage Examples
%%%
%%% Demonstrates how to use pqc_armstrong_sup for different tiers
%%% of supervision following Joe Armstrong's principles.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_armstrong_example).

-include("pqchain.hrl").

%% API
-export([
    start_error_kernel/0,
    start_critical_tier/0,
    start_normal_tier/0,
    start_peripheral_tier/0,
    example_escalation_policy/0,
    example_circuit_breaker/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start Error Kernel Supervisor (Tier 1)
%%
%% Armstrong Principle: Error Kernel
%% - one_for_all strategy (all must be available)
%% - Strictest restart limits (3 in 60 seconds)
%% - Core services: identity, crypto_policy, registry
%% - If one fails, restart all (shared dependencies)
%%
%% Example Usage:
%% ```
%% {ok, Pid} = pqc_armstrong_example:start_error_kernel().
%% '''
%%
%% @end
-spec start_error_kernel() -> {ok, pid()} | {error, term()}.
start_error_kernel() ->
    %% Define error kernel supervision policy
    Policy = #supervision_policy{
        strategy = one_for_all,
        intensity = 3,
        period = 60,
        escalation = fun error_kernel_escalation/3,
        circuit_breaker = undefined  % No circuit breaker for error kernel
    },

    %% Start supervisor
    {ok, Sup} = pqc_armstrong_sup:start_link(pqc_error_kernel_sup, Policy),

    %% Add error kernel children
    %% 1. Identity registry - must start first
    IdentitySpec = #armstrong_child{
        id = pqc_identity,
        module = pqc_identity,
        args = [],
        restart = permanent,
        shutdown = 5000,
        type = worker,
        tier = error_kernel,
        max_restarts = 3,
        restart_window_sec = 60
    },
    {ok, _IdPid} = pqc_armstrong_sup:add_child(Sup, IdentitySpec),

    %% 2. Crypto policy engine
    CryptoPolicySpec = #armstrong_child{
        id = pqc_crypto_policy,
        module = pqc_crypto_policy,
        args = [],
        restart = permanent,
        shutdown = 5000,
        type = worker,
        tier = error_kernel,
        max_restarts = 3,
        restart_window_sec = 60
    },
    {ok, _CryptoPid} = pqc_armstrong_sup:add_child(Sup, CryptoPolicySpec),

    {ok, Sup}.

%% @doc Start Critical Tier Supervisor (Tier 2)
%%
%% Armstrong Principle: Hierarchical Supervision
%% - rest_for_one strategy (ordered dependencies)
%% - Moderate restart limits (5 in 60 seconds)
%% - Critical services: consensus, peer channels
%% - Restart dependent services on failure
%%
%% @end
-spec start_critical_tier() -> {ok, pid()} | {error, term()}.
start_critical_tier() ->
    Policy = #supervision_policy{
        strategy = rest_for_one,
        intensity = 5,
        period = 60,
        escalation = fun critical_tier_escalation/3,
        circuit_breaker = #circuit_breaker_config{
            failure_threshold = 5,
            recovery_time_ms = 30000,
            half_open_attempts = 3
        }
    },

    {ok, Sup} = pqc_armstrong_sup:start_link(pqc_critical_sup, Policy),

    %% Add consensus supervisor (must start before peers)
    ConsensusSpec = #armstrong_child{
        id = pqc_consensus_sup,
        module = pqc_consensus_sup,
        args = [],
        restart = permanent,
        shutdown = infinity,  % Supervisor
        type = supervisor,
        tier = critical,
        max_restarts = 5,
        restart_window_sec = 60
    },
    {ok, _ConsPid} = pqc_armstrong_sup:add_child(Sup, ConsensusSpec),

    %% Add peer supervisor (depends on consensus)
    PeerSpec = #armstrong_child{
        id = pqc_peer_sup,
        module = pqc_peer_sup,
        args = [],
        restart = permanent,
        shutdown = infinity,
        type = supervisor,
        tier = critical,
        max_restarts = 5,
        restart_window_sec = 60
    },
    {ok, _PeerPid} = pqc_armstrong_sup:add_child(Sup, PeerSpec),

    {ok, Sup}.

%% @doc Start Normal Tier Supervisor (Tier 3)
%%
%% Armstrong Principle: Let It Crash
%% - one_for_one strategy (independent workers)
%% - Relaxed restart limits (10 in 60 seconds)
%% - Normal services: contracts, connections
%% - Isolated failures, no cascade
%%
%% @end
-spec start_normal_tier() -> {ok, pid()} | {error, term()}.
start_normal_tier() ->
    Policy = #supervision_policy{
        strategy = one_for_one,
        intensity = 10,
        period = 60,
        escalation = fun normal_tier_escalation/3,
        circuit_breaker = #circuit_breaker_config{
            failure_threshold = 10,
            recovery_time_ms = 60000,
            half_open_attempts = 2
        }
    },

    {ok, Sup} = pqc_armstrong_sup:start_link(pqc_normal_sup, Policy),

    %% Add contract supervisor
    ContractSpec = #armstrong_child{
        id = pqc_contract_sup,
        module = pqc_contract_sup,
        args = [],
        restart = permanent,
        shutdown = infinity,
        type = supervisor,
        tier = normal,
        max_restarts = 10,
        restart_window_sec = 60
    },
    {ok, _ContractPid} = pqc_armstrong_sup:add_child(Sup, ContractSpec),

    {ok, Sup}.

%% @doc Start Peripheral Tier Supervisor
%%
%% Armstrong Principle: Fail Fast
%% - one_for_one strategy
%% - Transient restart (only on abnormal termination)
%% - Peripheral services: observability, optional bridges
%% - Can fail without affecting core system
%%
%% @end
-spec start_peripheral_tier() -> {ok, pid()} | {error, term()}.
start_peripheral_tier() ->
    Policy = #supervision_policy{
        strategy = one_for_one,
        intensity = 5,
        period = 60,
        escalation = fun peripheral_tier_escalation/3,
        circuit_breaker = undefined  % No circuit breaker needed
    },

    {ok, Sup} = pqc_armstrong_sup:start_link(pqc_peripheral_sup, Policy),

    %% Add A2A bridge (optional)
    A2ASpec = #armstrong_child{
        id = pqc_a2a_bridge,
        module = pqc_a2a_bridge,
        args = [],
        restart = transient,  % Only restart on crash, not normal exit
        shutdown = 5000,
        type = worker,
        tier = peripheral,
        max_restarts = 5,
        restart_window_sec = 60
    },
    {ok, _A2APid} = pqc_armstrong_sup:add_child(Sup, A2ASpec),

    %% Add MCP bridge (optional)
    MCPSpec = #armstrong_child{
        id = pqc_mcp_bridge,
        module = pqc_mcp_bridge,
        args = [],
        restart = transient,
        shutdown = 5000,
        type = worker,
        tier = peripheral,
        max_restarts = 5,
        restart_window_sec = 60
    },
    {ok, _MCPPid} = pqc_armstrong_sup:add_child(Sup, MCPSpec),

    {ok, Sup}.

%% @doc Example escalation policy for different tiers
-spec example_escalation_policy() -> fun().
example_escalation_policy() ->
    fun(ChildId, Reason, RestartCount) ->
        case RestartCount of
            N when N < 3 ->
                %% Normal restart
                logger:info("Restarting ~p (attempt ~p): ~p",
                          [ChildId, N, Reason]),
                restart;
            N when N < 5 ->
                %% Apply circuit breaker
                logger:warning("Circuit breaking ~p (attempt ~p): ~p",
                             [ChildId, N, Reason]),
                circuit_break;
            N when N < 10 ->
                %% Isolate failing child
                logger:error("Isolating ~p (attempt ~p): ~p",
                           [ChildId, N, Reason]),
                isolate;
            _ ->
                %% Escalate to parent
                logger:critical("Escalating ~p (attempt ~p): ~p",
                              [ChildId, RestartCount, Reason]),
                escalate
        end
    end.

%% @doc Example circuit breaker configuration
-spec example_circuit_breaker() -> #circuit_breaker_config{}.
example_circuit_breaker() ->
    #circuit_breaker_config{
        failure_threshold = 5,        % Open after 5 failures
        recovery_time_ms = 30000,     % Wait 30s before half-open
        half_open_attempts = 3        % Allow 3 attempts in half-open
    }.

%%====================================================================
%% Escalation Functions
%%====================================================================

%% @private
error_kernel_escalation(_ChildId, _Reason, RestartCount) ->
    %% Error kernel failures are critical
    case RestartCount of
        N when N < 3 -> restart;
        _ -> escalate  % Escalate immediately after 3 failures
    end.

%% @private
critical_tier_escalation(_ChildId, _Reason, RestartCount) ->
    case RestartCount of
        N when N < 3 -> restart;
        N when N < 5 -> circuit_break;
        _ -> escalate
    end.

%% @private
normal_tier_escalation(_ChildId, _Reason, RestartCount) ->
    case RestartCount of
        N when N < 5 -> restart;
        N when N < 10 -> circuit_break;
        _ -> isolate  % Isolate instead of escalating
    end.

%% @private
peripheral_tier_escalation(_ChildId, _Reason, RestartCount) ->
    case RestartCount of
        N when N < 3 -> restart;
        _ -> isolate  % Just isolate, don't affect core system
    end.

%%====================================================================
%% Usage Example
%%====================================================================

%% @doc Complete PQChain supervision tree setup
%%
%% ```
%% % Start error kernel (Tier 1)
%% {ok, EKSup} = pqc_armstrong_example:start_error_kernel(),
%%
%% % Start critical services (Tier 2)
%% {ok, CritSup} = pqc_armstrong_example:start_critical_tier(),
%%
%% % Start normal services (Tier 3)
%% {ok, NormSup} = pqc_armstrong_example:start_normal_tier(),
%%
%% % Start peripheral services
%% {ok, PeriphSup} = pqc_armstrong_example:start_peripheral_tier(),
%%
%% % Get health report
%% Health = pqc_armstrong_sup:get_health(EKSup),
%% io:format("Error Kernel Health: ~p~n", [Health]),
%%
%% % Apply circuit breaker to specific child
%% CB = pqc_armstrong_example:example_circuit_breaker(),
%% ok = pqc_armstrong_sup:apply_circuit_breaker(CritSup, pqc_consensus_sup, CB),
%%
%% % Perform hot code upgrade
%% ok = pqc_armstrong_sup:hot_code_upgrade(NormSup, pqc_contract_sup, pqc_contract_sup_v2),
%%
%% % Isolate failing child
%% ok = pqc_armstrong_sup:isolate_failure(NormSup, some_failing_child),
%% '''
%%
%% @end
