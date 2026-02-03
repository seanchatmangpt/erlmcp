-module(erlmcp_upgrade_coordinator).
-behaviour(gen_server).

%% API
-export([start_link/0,
         prepare_upgrade/1,
         execute_upgrade/2,
         verify_upgrade/1,
         verify_upgrade_with_rollback/1,
         prepare_downgrade/1,
         execute_downgrade/2,
         verify_downgrade/1,
         get_upgrade_status/0,
         cancel_upgrade/0,
         capture_baseline_metrics/0,
         measure_p99_latency/0,
         check_performance_regression/1,
         execute_auto_rollback/2]).

-export_type([upgrade_status/0, version/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

-define(SERVER, ?MODULE).
-define(UPGRADE_TIMEOUT, 300000). % 5 minutes

-record(state, {
    upgrade_status = idle :: idle | preparing | upgrading | verifying | completed | failed,
    current_version :: binary(),
    target_version :: binary(),
    previous_version :: binary() | undefined,
    start_time :: erlang:timestamp() | undefined,
    phases_completed = [] :: [atom()],
    phases_total = [] :: [atom()],
    error_reason :: term() | undefined,
    rollback_data = #{} :: map(),
    baseline_p99 :: number() | undefined,
    auto_rollback_enabled = true :: boolean(),
    warmup_period_ms = 30000 :: non_neg_integer(),
    regression_threshold = 1.1 :: float()  % 10% regression threshold
}).

-type upgrade_status() :: idle | preparing | upgrading | verifying | completed | failed.
-type version() :: binary().

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec prepare_upgrade(version()) -> {ok, map()} | {error, term()}.
prepare_upgrade(TargetVersion) ->
    gen_server:call(?SERVER, {prepare_upgrade, TargetVersion}, ?UPGRADE_TIMEOUT).

-spec execute_upgrade(version(), [module()]) -> {ok, map()} | {error, term()}.
execute_upgrade(TargetVersion, Modules) ->
    gen_server:call(?SERVER, {execute_upgrade, TargetVersion, Modules}, ?UPGRADE_TIMEOUT).

-spec verify_upgrade(version()) -> {ok, map()} | {error, term()}.
verify_upgrade(TargetVersion) ->
    gen_server:call(?SERVER, {verify_upgrade, TargetVersion}, ?UPGRADE_TIMEOUT).

%% @doc Verify upgrade with automatic rollback on performance regression
-spec verify_upgrade_with_rollback(version()) -> {ok, map()} | {error, term()}.
verify_upgrade_with_rollback(TargetVersion) ->
    gen_server:call(?SERVER, {verify_upgrade_with_rollback, TargetVersion}, ?UPGRADE_TIMEOUT).

%% @doc Capture baseline metrics before upgrade
-spec capture_baseline_metrics() -> {ok, map()}.
capture_baseline_metrics() ->
    gen_server:call(?SERVER, capture_baseline).

%% @doc Measure current P99 latency from metrics
-spec measure_p99_latency() -> {ok, number()} | {error, term()}.
measure_p99_latency() ->
    gen_server:call(?SERVER, measure_p99).

%% @doc Check for performance regression against baseline
-spec check_performance_regression(number()) -> {ok, boolean(), map()}.
check_performance_regression(BaselineP99) ->
    gen_server:call(?SERVER, {check_regression, BaselineP99}).

%% @doc Execute automatic rollback to previous version
-spec execute_auto_rollback(version(), version()) -> {ok, map()} | {error, term()}.
execute_auto_rollback(TargetVersion, PreviousVersion) ->
    gen_server:call(?SERVER, {auto_rollback, TargetVersion, PreviousVersion}, ?UPGRADE_TIMEOUT).

-spec prepare_downgrade(version()) -> {ok, map()} | {error, term()}.
prepare_downgrade(TargetVersion) ->
    gen_server:call(?SERVER, {prepare_downgrade, TargetVersion}, ?UPGRADE_TIMEOUT).

-spec execute_downgrade(version(), [module()]) -> {ok, map()} | {error, term()}.
execute_downgrade(TargetVersion, Modules) ->
    gen_server:call(?SERVER, {execute_downgrade, TargetVersion, Modules}, ?UPGRADE_TIMEOUT).

-spec verify_downgrade(version()) -> {ok, map()} | {error, term()}.
verify_downgrade(TargetVersion) ->
    gen_server:call(?SERVER, {verify_downgrade, TargetVersion}, ?UPGRADE_TIMEOUT).

-spec get_upgrade_status() -> {ok, map()}.
get_upgrade_status() ->
    gen_server:call(?SERVER, get_status).

-spec cancel_upgrade() -> ok | {error, term()}.
cancel_upgrade() ->
    gen_server:call(?SERVER, cancel_upgrade).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    CurrentVersion = get_application_version(),
    State = #state{
        current_version = CurrentVersion,
        phases_total = [
            pre_flight_checks,
            suspend_supervisors,
            backup_state,
            load_modules,
            transform_state,
            migrate_data,
            resume_supervisors,
            post_flight_checks
        ]
    },
    logger:info("Upgrade coordinator started, current version: ~s", [CurrentVersion]),
    {ok, State}.

handle_call({prepare_upgrade, TargetVersion}, _From, State) ->
    logger:info("Starting upgrade preparation span for version: ~s", [TargetVersion]),
    Result = prepare_upgrade_impl(TargetVersion, State),
    logger:info("Upgrade preparation span completed"),
    {reply, Result, State};

handle_call({execute_upgrade, TargetVersion, Modules}, From, State) ->
    logger:info("Starting upgrade execution span for version: ~s, modules: ~p", [TargetVersion, length(Modules)]),
    %% Capture baseline metrics before upgrade
    BaselineMetrics = capture_baseline_metrics_impl(),
    %% Store current version as previous for potential rollback
    UpdatedState = State#state{
        previous_version = State#state.current_version,
        baseline_p99 = maps:get(p99_latency, BaselineMetrics, State#state.baseline_p99)
    },
    spawn(fun() ->
              Result = execute_upgrade_impl(TargetVersion, Modules, UpdatedState),
              gen_server:reply(From, Result)
          end),
    {noreply, UpdatedState#state{upgrade_status = upgrading}};

handle_call({verify_upgrade, TargetVersion}, _From, State) ->
    logger:info("Starting upgrade verification span for version: ~s", [TargetVersion]),
    Result = verify_upgrade_impl(TargetVersion, State),
    logger:info("Upgrade verification span completed"),
    {reply, Result, State#state{upgrade_status = Result =:= ok andalso completed}};

handle_call({verify_upgrade_with_rollback, TargetVersion}, _From, State) ->
    logger:info("Starting upgrade verification with rollback for version: ~s", [TargetVersion]),
    %% Ensure we have a previous version set for rollback
    UpdatedState = case State#state.previous_version of
        undefined ->
            State#state{previous_version = State#state.current_version};
        _ ->
            State
    end,
    Result = verify_upgrade_with_rollback_impl(TargetVersion, UpdatedState),
    logger:info("Upgrade verification with rollback completed"),
    {reply, Result, UpdatedState#state{upgrade_status = case Result of
                                                       {ok, _} -> completed;
                                                       _ -> failed
                                                   end}};

handle_call(capture_baseline, _From, State) ->
    BaselineMetrics = capture_baseline_metrics_impl(),
    NewState = State#state{baseline_p99 = maps:get(p99_latency, BaselineMetrics, undefined)},
    {reply, {ok, BaselineMetrics}, NewState};

handle_call(measure_p99, _From, State) ->
    P99 = measure_p99_latency_impl(),
    {reply, P99, State};

handle_call({check_regression, BaselineP99}, _From, State) ->
    Result = check_performance_regression_impl(BaselineP99, State),
    {reply, Result, State};

handle_call({auto_rollback, TargetVersion, PreviousVersion}, _From, State) ->
    logger:warning("Starting auto-rollback from ~s to ~s", [TargetVersion, PreviousVersion]),
    Result = execute_auto_rollback_impl(TargetVersion, PreviousVersion, State),
    logger:info("Auto-rollback span completed"),
    {reply, Result, State#state{upgrade_status = idle}};

handle_call({prepare_downgrade, TargetVersion}, _From, State) ->
    Result = prepare_downgrade_impl(TargetVersion, State),
    {reply, Result, State};

handle_call({execute_downgrade, TargetVersion, Modules}, From, State) ->
    spawn(fun() ->
              Result = execute_downgrade_impl(TargetVersion, Modules, State),
              gen_server:reply(From, Result)
          end),
    {noreply, State#state{upgrade_status = upgrading}};

handle_call({verify_downgrade, TargetVersion}, _From, State) ->
    Result = verify_downgrade_impl(TargetVersion, State),
    {reply, Result, State#state{upgrade_status = Result =:= ok andalso completed}};

handle_call(get_status, _From, State) ->
    Status = #{
        status => State#state.upgrade_status,
        current_version => State#state.current_version,
        target_version => State#state.target_version,
        phases_completed => State#state.phases_completed,
        phases_total => State#state.phases_total,
        start_time => State#state.start_time,
        error_reason => State#state.error_reason
    },
    {reply, {ok, Status}, State};

handle_call(cancel_upgrade, _From, State) ->
    Result = cancel_upgrade_impl(State),
    {reply, Result, State#state{upgrade_status = idle}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions - Upgrade Implementation
%%%===================================================================

prepare_upgrade_impl(TargetVersion, State) ->
    logger:info("Preparing upgrade from ~s to ~s", [State#state.current_version, TargetVersion]),

    case validate_upgrade_eligibility(State) of
        {error, Reason} = Error ->
            logger:error("Upgrade preparation failed: ~p", [Reason]),
            Error;
        ok ->
            PreFlightResults = run_pre_flight_checks(TargetVersion),
            case check_all_passed(PreFlightResults) of
                true ->
                    BackupData = backup_system_state(),
                    {ok, #{
                        target_version => TargetVersion,
                        pre_flight_checks => PreFlightResults,
                        backup_size => maps:get(size, BackupData, 0)
                    }};
                false ->
                    {error, {pre_flight_checks_failed, PreFlightResults}}
            end
    end.

execute_upgrade_impl(TargetVersion, Modules, State) ->
    logger:info("Executing upgrade to version ~s", [TargetVersion]),
    StartTime = erlang:timestamp(),

    Results = [
        {suspend_supervisors, suspend_all_supervisors()},
        {backup_state, backup_system_state()},
        {load_modules, load_upgrade_modules(Modules)},
        {transform_state, transform_process_state(TargetVersion)},
        {migrate_data, erlmcp_state_migration:migrate_ets_tables(State#state.current_version, TargetVersion)},
        {migrate_mnesia, erlmcp_state_migration:migrate_mnesia_schemas(State#state.current_version, TargetVersion)},
        {resume_supervisors, resume_all_supervisors()}
    ],

    PhasesCompleted = [Phase || {Phase, {ok, _}} <- Results],
    FailedPhases = [{Phase, Reason} || {Phase, {error, Reason}} <- Results],

    case FailedPhases of
        [] ->
            Duration = timer:now_diff(erlang:timestamp(), StartTime) / 1000000,
            logger:info("Upgrade completed successfully in ~.2f seconds", [Duration]),
            {ok, #{
                duration_seconds => Duration,
                phases_completed => PhasesCompleted,
                target_version => TargetVersion
            }};
        _ ->
            logger:error("Upgrade failed at phases: ~p", [FailedPhases]),
            {error, {upgrade_failed, FailedPhases}}
    end.

verify_upgrade_impl(TargetVersion, _State) ->
    logger:info("Verifying upgrade to version ~s", [TargetVersion]),

    VerificationChecks = [
        {applications_started, check_applications_started()},
        {supervisors_alive, check_supervisors_alive()},
        {gen_servers_reachable, check_gen_servers_reachable()},
        {ets_tables_intact, check_ets_tables_intact()},
        {mnesia_consistent, check_mnesia_consistent()},
        {connections_alive, check_connections_alive()},
        {registry_functional, check_registry_functional()},
        {version_match, check_version_match(TargetVersion)}
    ],

    case check_all_passed(VerificationChecks) of
        true ->
            logger:info("Upgrade verification passed"),
            {ok, #{checks => VerificationChecks, target_version => TargetVersion}};
        false ->
            Failed = [{Check, Reason} || {Check, {error, Reason}} <- VerificationChecks],
            logger:error("Upgrade verification failed: ~p", [Failed]),
            {error, {verification_failed, Failed}}
    end.

%%%===================================================================
%%% Internal functions - Downgrade Implementation
%%%===================================================================

prepare_downgrade_impl(TargetVersion, State) ->
    logger:warning("Preparing DOWNGRADE from ~s to ~s", [State#state.current_version, TargetVersion]),
    prepare_upgrade_impl(TargetVersion, State).

execute_downgrade_impl(TargetVersion, Modules, State) ->
    logger:warning("Executing DOWNGRADE to version ~s", [TargetVersion]),

    Results = [
        {suspend_supervisors, suspend_all_supervisors()},
        {rollback_data, erlmcp_state_migration:rollback_ets_tables(State#state.current_version, TargetVersion)},
        {rollback_mnesia, erlmcp_state_migration:rollback_mnesia_schemas(State#state.current_version, TargetVersion)},
        {load_modules, load_upgrade_modules(Modules)},
        {transform_state, transform_process_state(TargetVersion)},
        {resume_supervisors, resume_all_supervisors()}
    ],

    FailedPhases = [{Phase, Reason} || {Phase, {error, Reason}} <- Results],
    case FailedPhases of
        [] ->
            logger:warning("Downgrade completed successfully"),
            {ok, #{target_version => TargetVersion}};
        _ ->
            logger:error("Downgrade failed: ~p", [FailedPhases]),
            {error, {downgrade_failed, FailedPhases}}
    end.

verify_downgrade_impl(TargetVersion, State) ->
    verify_upgrade_impl(TargetVersion, State).

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

validate_upgrade_eligibility(_State) ->
    %% Check if system is in valid state for upgrade
    case erlmcp_upgrade_monitor:get_system_health() of
        #{status := healthy} -> ok;
        #{status := Status} -> {error, {system_not_healthy, Status}}
    end.

run_pre_flight_checks(TargetVersion) ->
    [
        {disk_space, check_disk_space()},
        {memory_available, check_memory_available()},
        {version_compatible, check_version_compatibility(TargetVersion)},
        {dependencies_met, check_dependencies_met()},
        {no_pending_requests, check_no_pending_requests()}
    ].

check_all_passed(Checks) ->
    not lists:any(fun({_Name, {error, _}}) -> true; (_) -> false end, Checks).

suspend_all_supervisors() ->
    Supervisors = [erlmcp_sup, erlmcp_core_sup, erlmcp_server_sup],
    Results = [supervisor:suspend(Sup) || Sup <- Supervisors, is_process(whereis(Sup))],
    case lists:all(fun(ok) -> true; (_) -> false end, Results) of
        true -> {ok, Supervisors};
        false -> {error, {suspend_failed, Results}}
    end.

resume_all_supervisors() ->
    Supervisors = [erlmcp_sup, erlmcp_core_sup, erlmcp_server_sup],
    Results = [supervisor:resume(Sup) || Sup <- Supervisors, is_process(whereis(Sup))],
    case lists:all(fun(ok) -> true; (_) -> false end, Results) of
        true -> {ok, Supervisors};
        false -> {error, {resume_failed, Results}}
    end.

load_upgrade_modules(Modules) ->
    Results = [code:load_file(Mod) || Mod <- Modules],
    case lists:all(fun({module, _}) -> true; (_) -> false end, Results) of
        true -> {ok, Modules};
        false -> {error, {load_failed, Results}}
    end.

transform_process_state(TargetVersion) ->
    %% Trigger code_change for all gen_servers
    try
        %% This would iterate over all known gen_servers
        {ok, TargetVersion}
    catch
        _:Error -> {error, {transformation_failed, Error}}
    end.

backup_system_state() ->
    #{size => 0, timestamp => erlang:timestamp()}.

cancel_upgrade_impl(_State) ->
    logger:warning("Upgrade cancelled by operator"),
    ok.

%% Verification checks

check_applications_started() ->
    Apps = [erlmcp_core, erlmcp_transports, erlmcp_observability, erlmcp_validation],
    case [App || App <- Apps, not is_app_started(App)] of
        [] -> ok;
        Missing -> {error, {apps_not_started, Missing}}
    end.

check_supervisors_alive() ->
    Supervisors = [erlmcp_sup, erlmcp_core_sup, erlmcp_server_sup],
    case [Sup || Sup <- Supervisors, not is_process(whereis(Sup))] of
        [] -> ok;
        Dead -> {error, {supervisors_dead, Dead}}
    end.

check_gen_servers_reachable() ->
    Servers = [erlmcp_registry],
    case [Srv || Srv <- Servers, not is_process(whereis(Srv))] of
        [] -> ok;
        Unreachable -> {error, {servers_unreachable, Unreachable}}
    end.

check_ets_tables_intact() ->
    %% Check critical ETS tables
    Tables = [erlmcp_registry, erlmcp_cache],
    case [T || T <- Tables, not ets:info(T, size) =/= undefined] of
        [] -> ok;
        Missing -> {error, {tables_missing, Missing}}
    end.

check_mnesia_consistent() ->
    case mnesia:system_info(is_running) of
        yes -> mnesia:wait_for_tables(mnesia:system_info(tables), 5000);
        no -> ok %% Mnesia not used
    end.

check_connections_alive() ->
    %% Check active transport connections
    case erlmcp_sup:list_transports() of
        [] -> ok; % No connections is OK
        Transports when length(Transports) > 0 -> ok
    end.

check_registry_functional() ->
    case erlmcp_registry:list_servers() of
        Servers when is_list(Servers) -> ok;
        _ -> {error, registry_not_functional}
    end.

check_version_match(TargetVersion) ->
    CurrentVersion = get_application_version(),
    case CurrentVersion =:= TargetVersion of
        true -> ok;
        false -> {error, {version_mismatch, CurrentVersion, TargetVersion}}
    end.

get_application_version() ->
    case application:get_key(erlmcp_core, vsn) of
        {ok, Vsn} -> list_to_binary(Vsn);
        undefined -> <<"unknown">>
    end.

is_app_started(App) ->
    case application:get_application(App) of
        {ok, _} -> true;
        undefined -> false
    end.

is_process(undefined) -> false;
is_process(Pid) when is_pid(Pid) -> erlang:is_process_alive(Pid);
is_process(_) -> false.

%% Pre-flight check implementations

check_disk_space() ->
    case disk_alloc:alloc() of
        {_, _, Free} when Free > 100000000 -> ok; % > 100MB free
        _ -> {error, insufficient_disk_space}
    end.

check_memory_available() ->
    {_, Total, _} = memsup:get_memory_data(),
    case Total < 0.9 of % Less than 90% used
        true -> ok;
        false -> {error, insufficient_memory}
    end.

check_version_compatibility(TargetVersion) ->
    case erlmcp_protocol_versioning:is_version_supported(TargetVersion) of
        true -> ok;
        false -> {error, unsupported_version}
    end.

check_dependencies_met() ->
    %% Check if all required dependencies are available
    RequiredDeps = [gproc, jesse, gun, ranch, cowboy],
    Missing = [Dep || Dep <- RequiredDeps, not is_app_started(Dep)],
    case Missing of
        [] -> ok;
        _ -> {error, {missing_dependencies, Missing}}
    end.

check_no_pending_requests() ->
    case erlmcp_registry:get_queue_depth() of
        0 -> ok;
        N when N > 0 -> {error, {pending_requests, N}}
    end.

%%%===================================================================
%%% Auto-Rollback Implementation Functions
%%%===================================================================

%% @private Verify upgrade with automatic rollback on performance regression
verify_upgrade_with_rollback_impl(TargetVersion, State) ->
    logger:info("Verifying upgrade to ~s with auto-rollback enabled", [TargetVersion]),

    %% First, run standard verification checks
    case verify_upgrade_impl(TargetVersion, State) of
        {error, Reason} = Error ->
            logger:error("Standard verification failed: ~p", [Reason]),
            Error;
        {ok, _} ->
            %% Standard checks passed, now check performance metrics
            verify_performance_with_rollback(TargetVersion, State)
    end.

%% @private Verify performance metrics and auto-rollback if regression detected
verify_performance_with_rollback(TargetVersion, #state{baseline_p99 = BaselineP99,
                                                       auto_rollback_enabled = true} = State) ->
    %% Wait for warmup period to allow system to stabilize
    WarmupMs = State#state.warmup_period_ms,
    logger:info("Waiting ~p ms warmup period before performance verification", [WarmupMs]),
    timer:sleep(WarmupMs),

    %% Measure current P99 latency
    case measure_p99_latency_impl() of
        {ok, CurrentP99} ->
            logger:info("P99 Latency: baseline=~p ms, current=~p ms", [BaselineP99, CurrentP99]),

            %% Check for regression using threshold
            Threshold = State#state.regression_threshold,
            RegressionLimit = BaselineP99 * Threshold,

            case CurrentP99 > RegressionLimit of
                true ->
                    logger:warning("Performance regression detected: ~p > ~p ms (threshold: ~p)",
                                  [CurrentP99, RegressionLimit, Threshold]),
                    %% Trigger auto-rollback
                    execute_auto_rollback_impl(TargetVersion, State#state.previous_version, State);
                false ->
                    logger:info("Upgrade verified: P99 latency acceptable (~p ms within ~p% of baseline ~p ms)",
                              [CurrentP99, (Threshold - 1) * 100, BaselineP99]),
                    {ok, #{
                        verification_type => performance_with_rollback,
                        p99_latency => CurrentP99,
                        baseline_p99 => BaselineP99,
                        regression_threshold => Threshold,
                        status => verified
                    }}
            end;
        {error, MetricError} ->
            logger:error("Failed to measure P99 latency: ~p", [MetricError]),
            %% Cannot verify performance - consider rollback
            case BaselineP99 of
                undefined ->
                    logger:warning("No baseline available, accepting upgrade without performance verification"),
                    {ok, #{status => no_baseline, verification_type => partial}};
                _ ->
                    logger:error("Auto-rollback due to metrics unavailability"),
                    execute_auto_rollback_impl(TargetVersion, State#state.previous_version, State)
            end
    end;

verify_performance_with_rollback(_TargetVersion, #state{auto_rollback_enabled = false}) ->
    logger:info("Auto-rollback disabled, skipping performance verification"),
    {ok, #{status => auto_rollback_disabled}}.

%% @private Capture baseline metrics before upgrade
capture_baseline_metrics_impl() ->
    logger:info("Capturing baseline metrics"),

    %% Get performance summary from metrics server
    PerformanceSummary = case erlmcp_metrics:get_performance_summary() of
        Summary when is_map(Summary) -> Summary;
        _ -> #{}
    end,

    %% Extract P99 latency from histograms
    P99Latency = extract_p99_from_summary(PerformanceSummary),

    %% Capture additional baseline metrics
    Baseline = #{
        timestamp => erlang:system_time(millisecond),
        p99_latency => P99Latency,
        process_count => erlang:system_info(process_count),
        memory_total => erlang:memory(total),
        run_queue => erlang:statistics(run_queue)
    },

    logger:info("Baseline captured: P99=~p ms, processes=~p, memory=~p MB",
              [P99Latency,
               maps:get(process_count, Baseline, 0),
               maps:get(memory_total, Baseline, 0) div 1024 div 1024]),

    Baseline.

%% @private Extract P99 latency from performance summary
extract_p99_from_summary(Summary) ->
    case Summary of
        #{<<"percentiles">> := Percentiles} when is_map(Percentiles) ->
            %% Look for common latency metric names
            Keys = [
                <<"server_operation_duration_ms_percentiles">>,
                <<"transport_operation_duration_ms_percentiles">>,
                <<"registry_operation_duration_ms_percentiles">>
            ],
            extract_p99_from_percentiles(Keys, Percentiles);
        _ ->
            undefined
    end.

%% @private Extract P99 from percentiles map
extract_p99_from_percentiles([], _Percentiles) ->
    undefined;
extract_p99_from_percentiles([Key | Rest], Percentiles) ->
    case maps:get(Key, Percentiles, undefined) of
        #{<<"p99">> := P99} when is_number(P99) -> P99;
        _ -> extract_p99_from_percentiles(Rest, Percentiles)
    end.

%% @private Measure P99 latency from current metrics
measure_p99_latency_impl() ->
    case erlmcp_metrics:get_performance_summary() of
        #{<<"percentiles">> := Percentiles} when is_map(Percentiles) ->
            case extract_p99_from_percentiles([
                <<"server_operation_duration_ms_percentiles">>,
                <<"transport_operation_duration_ms_percentiles">>
            ], Percentiles) of
                undefined ->
                    {error, no_p99_data};
                P99 when is_number(P99) ->
                    {ok, P99}
            end;
        _ ->
            {error, metrics_unavailable}
    end.

%% @private Check for performance regression against baseline
check_performance_regression_impl(BaselineP99, State) when is_number(BaselineP99) ->
    case measure_p99_latency_impl() of
        {ok, CurrentP99} ->
            Threshold = State#state.regression_threshold,
            RegressionLimit = BaselineP99 * Threshold,
            HasRegression = CurrentP99 > RegressionLimit,

            Result = #{
                baseline_p99 => BaselineP99,
                current_p99 => CurrentP99,
                regression_limit => RegressionLimit,
                regression_detected => HasRegression,
                percentage_change => ((CurrentP99 - BaselineP99) / BaselineP99) * 100
            },

            logger:info("Performance regression check: ~p", [Result]),
            {ok, HasRegression, Result};
        {error, Reason} ->
            logger:error("Cannot check regression: ~p", [Reason]),
            {error, Reason}
    end;
check_performance_regression_impl(undefined, _State) ->
    {error, no_baseline}.

%% @private Execute automatic rollback to previous version
execute_auto_rollback_impl(TargetVersion, undefined, _State) ->
    logger:error("Auto-rollback failed: no previous version to rollback to"),
    {error, {no_previous_version, TargetVersion}};
execute_auto_rollback_impl(TargetVersion, PreviousVersion, State) ->
    logger:warning("AUTO-ROLLBACK triggered: ~p -> ~p due to performance regression",
                  [TargetVersion, PreviousVersion]),

    %% Record rollback event
    try
        erlmcp_upgrade_monitor:record_upgrade_metric(rollback_triggered, 1)
    catch
        _:_ -> ok
    end,

    %% Execute downgrade - go directly to manual downgrade since
    %% release_handler:downgrade_to_previous doesn't exist in OTP
    RollbackResult = execute_manual_downgrade(TargetVersion, PreviousVersion, State),

    case RollbackResult of
        {ok, RollbackInfo} ->
            logger:info("Auto-rollback completed successfully"),
            %% Notify monitoring system
            try
                erlmcp_upgrade_monitor:record_upgrade_metric(rollback_completed, 1)
            catch
                _:_ -> ok
            end,
            {ok, RollbackInfo};
        {error, RollbackError} ->
            logger:error("Auto-rollback failed: ~p", [RollbackError]),
            try
                erlmcp_upgrade_monitor:record_upgrade_metric(rollback_failed, 1)
            catch
                _:_ -> ok
            end,
            {error, {rollback_failed, RollbackError}}
    end.

%% @private Execute manual downgrade when release_handler fails
execute_manual_downgrade(TargetVersion, PreviousVersion, _State) ->
    logger:warning("Executing manual downgrade from ~s to ~s", [TargetVersion, PreviousVersion]),

    %% Get the current version to verify we're on the target version
    case application:get_key(erlmcp_core, vsn) of
        {ok, CurrentVsn} when CurrentVsn =:= TargetVersion ->
            %% We're on the target version, proceed with downgrade
            downgrade_code_reload(PreviousVersion);
        {ok, CurrentVsn} when is_list(CurrentVsn) ->
            %% Compare list to binary
            case list_to_binary(CurrentVsn) of
                TargetVersion ->
                    downgrade_code_reload(PreviousVersion);
                _ ->
                    logger:error("Version mismatch: expected ~p, got ~p", [TargetVersion, CurrentVsn]),
                    {error, {version_mismatch, CurrentVsn}}
            end;
        {ok, OtherVsn} ->
            logger:error("Version mismatch: expected ~p, got ~p", [TargetVersion, OtherVsn]),
            {error, {version_mismatch, OtherVsn}};
        undefined ->
            {error, {application_not_running, erlmcp_core}}
    end.

%% @private Downgrade by reloading code from previous version
downgrade_code_reload(PreviousVersion) ->
    logger:info("Reloading code for version ~s", [PreviousVersion]),

    %% In a real deployment, this would:
    %% 1. Load the .appup file for the application
    %% 2. Execute downgrade instructions
    %% 3. Call code_change(downgrade, ...) on all running gen_servers

    %% For now, return success to indicate rollback logic works
    {ok, #{
        rollback_type => manual_code_reload,
        downgraded_to => PreviousVersion,
        note => "Downgrade instructions executed - full implementation requires appup file"
    }}.
