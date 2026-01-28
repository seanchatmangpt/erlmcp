-module(erlmcp_zero_downtime_upgrade).

%% Zero-downtime upgrade orchestrator
%% Coordinates hot reload, graceful drain, and metrics for seamless upgrades

-export([
    prepare_upgrade/1,
    execute_upgrade/2,
    execute_upgrade/3,
    rollback_upgrade/0,
    get_upgrade_status/0,
    wait_for_upgrade_complete/1
]).

-include_lib("kernel/include/logger.hrl").

% Logger macro compatibility
-ifndef(LOG_notice).
-define(LOG_notice(Msg, Meta), logger:notice(Msg, Meta)).
-endif.
-ifndef(LOG_warning).
-define(LOG_warning(Msg, Meta), logger:warning(Msg, Meta)).
-endif.
-ifndef(LOG_info).
-define(LOG_info(Msg, Meta), logger:info(Msg, Meta)).
-endif.
-ifndef(LOG_error).
-define(LOG_error(Msg, Meta), logger:error(Msg, Meta)).
-endif.

%% Types
-type upgrade_spec() :: #{
    modules => [atom()],
    config => list() | undefined,
    timeout_ms => pos_integer()
}.

-type upgrade_status() :: #{
    phase := atom(),
    connections_active := non_neg_integer(),
    drain_status := idle | draining | complete,
    elapsed_ms := non_neg_integer(),
    modules_reloaded := [atom()],
    errors := [term()]
}.

%%====================================================================
%% API Functions
%%====================================================================

%% Prepare for upgrade: validate specs, check connections
-spec prepare_upgrade(upgrade_spec()) -> {ok, upgrade_spec()} | {error, term()}.
prepare_upgrade(UpgradeSpec) ->
    ?LOG_notice("Preparing for zero-downtime upgrade", #{spec => UpgradeSpec}),

    try
        % Validate spec
        Modules = maps:get(modules, UpgradeSpec, []),
        Config = maps:get(config, UpgradeSpec, undefined),
        TimeoutMs = maps:get(timeout_ms, UpgradeSpec, 30000),

        % Validate modules exist
        lists:foreach(fun(Module) ->
            case code:which(Module) of
                non_existing -> throw({module_not_found, Module});
                _ -> ok
            end
        end, Modules),

        % Validate config if provided
        case Config of
            undefined -> ok;
            _ -> erlmcp_hot_reload:validate_config(Config)
        end,

        {ok, UpgradeSpec#{timeout_ms => TimeoutMs}}
    catch
        throw:Error -> {error, Error};
        _:Error -> {error, {validation_error, Error}}
    end.

%% Execute upgrade with optional dry-run
-spec execute_upgrade(upgrade_spec(), pid()) -> {ok, map()} | {error, term()}.
execute_upgrade(UpgradeSpec, NotifyPid) ->
    execute_upgrade(UpgradeSpec, NotifyPid, false).

-spec execute_upgrade(upgrade_spec(), pid(), boolean()) -> {ok, map()} | {error, term()}.
execute_upgrade(UpgradeSpec, NotifyPid, DryRun) ->
    try
        case DryRun of
            true ->
                ?LOG_notice("DRY RUN: Zero-downtime upgrade validation", #{}),
                validate_upgrade_only(UpgradeSpec);
            false ->
                ?LOG_notice("Starting zero-downtime upgrade execution", #{}),
                execute_upgrade_sequence(UpgradeSpec, NotifyPid)
        end
    catch
        throw:Error ->
            ?LOG_error("Upgrade failed", #{error => Error}),
            {error, Error};
        _:Error ->
            ?LOG_error("Unexpected error during upgrade", #{error => Error}),
            {error, {upgrade_error, Error}}
    end.

%% Rollback to previous version
-spec rollback_upgrade() -> {ok, map()} | {error, term()}.
rollback_upgrade() ->
    ?LOG_warning("Rolling back zero-downtime upgrade", #{}),
    % In production: reload previous module versions from backup
    % For now: notify that rollback would occur
    {ok, #{
        message => "Rollback initiated",
        note => "Implement backup/restore mechanism for production"
    }}.

%% Get current upgrade status
-spec get_upgrade_status() -> upgrade_status().
get_upgrade_status() ->
    DrainStatus = erlmcp_graceful_drain:get_drain_status(),
    Connections = erlmcp_graceful_drain:get_active_connections(),
    Metrics = erlmcp_hot_reload:get_reload_metrics(),

    #{
        phase => determine_phase(DrainStatus),
        connections_active => length(Connections),
        drain_status => maps:get(status, DrainStatus),
        elapsed_ms => maps:get(elapsed_ms, DrainStatus, 0),
        modules_reloaded => maps:get(modules_reloaded, status_context(), []),
        errors => maps:get(errors, status_context(), [])
    }.

%% Wait for upgrade to complete
-spec wait_for_upgrade_complete(pos_integer()) -> {ok, map()} | {timeout, map()}.
wait_for_upgrade_complete(TimeoutMs) ->
    StartTime = erlang:now(),
    wait_upgrade_loop(StartTime, TimeoutMs).

%%====================================================================
%% Internal Functions
%%====================================================================

validate_upgrade_only(UpgradeSpec) ->
    Modules = maps:get(modules, UpgradeSpec, []),
    Config = maps:get(config, UpgradeSpec, undefined),

    % Validate all modules can be loaded
    ModuleStatus = lists:map(fun(Module) ->
        case code:load_file(Module) of
            {module, Module} -> {Module, ok};
            {error, Reason} -> {Module, {error, Reason}}
        end
    end, Modules),

    % Check if any failed
    Failures = [M || {M, {error, _}} <- ModuleStatus],
    case Failures of
        [] ->
            {ok, #{
                validation => passed,
                modules_checked => length(Modules),
                config_valid => Config =/= undefined
            }};
        _ ->
            {error, {validation_failed, Failures}}
    end.

execute_upgrade_sequence(UpgradeSpec, NotifyPid) ->
    Modules = maps:get(modules, UpgradeSpec, []),
    Config = maps:get(config, UpgradeSpec, undefined),
    TimeoutMs = maps:get(timeout_ms, UpgradeSpec, 30000),

    StartTime = erlang:now(),
    put(upgrade_start_time, StartTime),
    put(upgrade_modules_reloaded, []),
    put(upgrade_errors, []),

    % Phase 1: Begin graceful drain
    phase_1_begin_drain(TimeoutMs, NotifyPid),

    % Phase 2: Reload modules while connections drain
    phase_2_reload_modules(Modules, NotifyPid),

    % Phase 3: Reload configuration if needed
    case Config of
        undefined -> ok;
        _ -> phase_3_reload_config(Config, NotifyPid)
    end,

    % Phase 4: Wait for drain completion
    phase_4_wait_drain_completion(TimeoutMs, NotifyPid),

    % Collect results
    ElapsedMs = round(timer:now_diff(erlang:now(), StartTime) / 1000),
    ModulesReloaded = get(upgrade_modules_reloaded),
    Errors = get(upgrade_errors),

    ?LOG_notice("Zero-downtime upgrade completed", #{
        elapsed_ms => ElapsedMs,
        modules_reloaded => length(ModulesReloaded),
        errors => length(Errors)
    }),

    {ok, #{
        status => success,
        elapsed_ms => ElapsedMs,
        modules_reloaded => ModulesReloaded,
        config_reloaded => Config =/= undefined,
        errors => Errors,
        metrics => erlmcp_hot_reload:get_reload_metrics()
    }}.

phase_1_begin_drain(TimeoutMs, NotifyPid) ->
    ?LOG_notice("Phase 1: Beginning graceful drain", #{}),
    case erlmcp_graceful_drain:request_drain(TimeoutMs, NotifyPid) of
        {ok, _DrainRef} ->
            ?LOG_info("Graceful drain started successfully", #{}),
            ok;
        {error, Reason} ->
            ?LOG_error("Failed to start graceful drain", #{reason => Reason}),
            throw({drain_failed, Reason})
    end.

phase_2_reload_modules(Modules, _NotifyPid) ->
    ?LOG_notice("Phase 2: Reloading modules", #{count => length(Modules)}),
    Results = erlmcp_hot_reload:reload_modules(Modules),

    % Check for failures
    Failures = [M || {error, _} = Result <- Results, M <- [Result]],
    case Failures of
        [] ->
            SuccessfulModules = [M || {ok, M} <- Results],
            put(upgrade_modules_reloaded, SuccessfulModules),
            ?LOG_info("All modules reloaded successfully", #{
                modules => SuccessfulModules
            });
        _ ->
            put(upgrade_errors, Failures),
            ?LOG_warning("Some modules failed to reload", #{
                failures => Failures
            })
    end.

phase_3_reload_config(Config, _NotifyPid) ->
    ?LOG_notice("Phase 3: Reloading configuration", #{}),
    case erlmcp_hot_reload:reload_config(Config) of
        {ok, Version} ->
            ?LOG_info("Configuration reloaded successfully", #{version => Version});
        {error, Reason} ->
            Errors = get(upgrade_errors),
            put(upgrade_errors, [{config_reload_failed, Reason} | Errors]),
            ?LOG_warning("Configuration reload failed", #{reason => Reason})
    end.

phase_4_wait_drain_completion(TimeoutMs, _NotifyPid) ->
    ?LOG_notice("Phase 4: Waiting for drain completion", #{timeout_ms => TimeoutMs}),
    StartTime = erlang:now(),

    wait_drain_loop(StartTime, TimeoutMs).

wait_drain_loop(StartTime, TimeoutMs) ->
    ElapsedMs = round(timer:now_diff(erlang:now(), StartTime) / 1000),
    RemainingMs = TimeoutMs - ElapsedMs,

    case RemainingMs > 0 of
        false ->
            ?LOG_warning("Drain timeout reached", #{
                elapsed_ms => ElapsedMs,
                timeout_ms => TimeoutMs
            }),
            ok;
        true ->
            case erlmcp_graceful_drain:get_drain_status() of
                #{status := complete} ->
                    ?LOG_info("Drain completed successfully", #{
                        elapsed_ms => ElapsedMs
                    }),
                    ok;
                Status ->
                    timer:sleep(500),
                    wait_drain_loop(StartTime, TimeoutMs)
            end
    end.

wait_upgrade_loop(StartTime, TimeoutMs) ->
    ElapsedMs = round(timer:now_diff(erlang:now(), StartTime) / 1000),
    RemainingMs = TimeoutMs - ElapsedMs,

    case RemainingMs > 0 of
        false ->
            Status = get_upgrade_status(),
            {timeout, Status};
        true ->
            Status = get_upgrade_status(),
            case maps:get(phase, Status) of
                complete ->
                    {ok, Status};
                _ ->
                    timer:sleep(1000),
                    wait_upgrade_loop(StartTime, TimeoutMs)
            end
    end.

determine_phase(DrainStatus) ->
    case maps:get(status, DrainStatus) of
        idle -> idle;
        draining -> in_progress;
        complete -> complete
    end.

status_context() ->
    #{
        modules_reloaded => get(upgrade_modules_reloaded),
        errors => get(upgrade_errors)
    }.
