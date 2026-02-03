%% @doc Recovery orchestrator for erlmcp v3
%% Orchestrates automated recovery procedures with dependency resolution
%% Implements circuit breakers, health validation, and phased recovery
-module(erlmcp_recovery_orchestrator).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, status/0, status/1, recover/1, recover/2,
         test_recovery/1, get_recovery_plan/1, execute_recovery_steps/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Records
-record(recovery_job, {
    id :: binary(),
    service :: binary(),
    status :: pending | in_progress | completed | failed | rolled_back,
    start_time :: integer(),
    completion_time :: integer(),
    steps :: [map()],
    current_step :: integer(),
    dependencies :: [binary()],
    circuit_breaker :: active | tripped | reset,
    health :: healthy | degraded | unhealthy
}).

-recovery_step({
    pre_check,
    description => "Perform pre-recovery checks",
    timeout => 10000,
    dependencies => []
}).

-recovery_step({
    start_services,
    description => "Start required services",
    timeout => 30000,
    dependencies => [pre_check]
}).

-recovery_step({
    validate_health,
    description => "Validate service health",
    timeout => 15000,
    dependencies => [start_services]
}).

-recovery_step({
    restore_data,
    description => "Restore from backup",
    timeout => 60000,
    dependencies => [validate_health]
}).

-recovery_step({
    sync_services,
    description => "Synchronize services",
    timeout => 20000,
    dependencies => [restore_data]
}).

-recovery_step({
    verify_services,
    description => "Verify all services",
    timeout => 15000,
    dependencies => [sync_services]
}).

-recovery_step({
    cleanup_temp_files,
    description => "Clean up temporary files",
    timeout => 5000,
    dependencies => [verify_services]
}).

-record(state, {
    config :: map(),
    active_recoveries :: #{binary() => #recovery_job{}},
    recovery_history :: [binary()],
    circuit_breakers :: #{binary() => atom()},
    service_dependencies :: #{binary() => [binary()]},
    schedules :: timer:tref()
}).

%% Constants
-define(RECOVERY_TIMEOUT, 300000).  % 5 minutes
-define(CIRCUIT_BREAKER_THRESHOLD, 3).  % 3 failures
-define(CIRCUIT_BREAKER_TIMEOUT, 300000).  % 5 minutes
-define(HEALTH_CHECK_INTERVAL, 5000).  % 5 seconds
-define(MAX_CONCURRENT_RECOVERIES, 3).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    start_link([]).

-spec start_link(Options :: list()) -> {ok, pid()} | {error, any()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

-spec status() -> map().
status() ->
    status([]).

-spec status(Options :: list()) -> map().
status(Options) ->
    gen_server:call(?MODULE, {status, Options}).

-spec recover(Service :: binary()) -> ok | {error, any()}.
recover(Service) ->
    recover(Service, #{}).

-spec recover(Service :: binary(), Options :: map()) -> ok | {error, any()}.
recover(Service, Options) ->
    gen_server:call(?MODULE, {recover, Service, Options}, ?RECOVERY_TIMEOUT).

-spec test_recovery(Service :: binary()) -> ok | {error, any()}.
test_recovery(Service) ->
    gen_server:call(?MODULE, {test_recovery, Service}, ?RECOVERY_TIMEOUT).

-spec get_recovery_plan(Service :: binary()) -> map().
get_recovery_plan(Service) ->
    gen_server:call(?MODULE, {get_recovery_plan, Service}).

-spec execute_recovery_steps(Service :: binary(), Steps :: list()) -> ok | {error, any()}.
execute_recovery_steps(Service, Steps) ->
    gen_server:call(?MODULE, {execute_recovery_steps, Service, Steps}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Options) ->
    %% Initialize recovery orchestrator
    ServiceDependencies = load_service_dependencies(),
    CircuitBreakers = initialize_circuit_breakers(),

    %% Start health checks
    Schedule = start_health_checks(),

    State = #state{
        config = parse_config(Options),
        active_recoveries = #{},
        recovery_history = [],
        circuit_breakers = CircuitBreakers,
        service_dependencies = ServiceDependencies,
        schedules = Schedule
    },

    %% Register for cluster events
    erlmcp_event_manager:subscribe(?MODULE, [service_health_changed, recovery_required, circuit_breaker_trip]),

    {ok, State}.

handle_call({status, Options}, _From, State) ->
    Status = build_status_report(State, Options),
    {reply, Status, State};

handle_call({recover, Service, Options}, _From, State) ->
    %% Check if recovery is allowed
    case can_perform_recovery(Service, State) of
        true ->
            %% Create recovery job
            JobId = generate_recovery_id(Service),
            Job = create_recovery_job(JobId, Service, Options),

            %% Start recovery process
            NewState = start_recovery_process(Job, State),
            {reply, {ok, JobId}, NewState};
        false ->
            {reply, {error, recovery_not_allowed}, State}
    end;

handle_call({test_recovery, Service}, _From, State) ->
    %% Create test recovery job
    JobId = generate_recovery_id(Service),
    Job = create_recovery_job(JobId, Service, #{test => true}),

    %% Start test recovery
    NewState = start_recovery_process(Job, State),
    {reply, {ok, JobId}, NewState};

handle_call({get_recovery_plan, Service}, _From, State) ->
    %% Get recovery plan for service
    Plan = generate_recovery_plan(Service, State),
    {reply, Plan, State};

handle_call({execute_recovery_steps, Service, Steps}, _From, State) ->
    %% Execute specific recovery steps
    JobId = generate_recovery_id(Service),
    Job = create_recovery_job(JobId, Service, #{steps => Steps}),

    %% Start recovery with specific steps
    NewState = start_recovery_process(Job, State),
    {reply, {ok, JobId}, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({recovery_started, JobId, Service}, State) ->
    %% Handle recovery start notification
    erlmcp_audit:log(recovery_started, #{
        job_id => JobId,
        service => Service,
        timestamp => erlang:system_time(millisecond)
    }),
    {noreply, State};

handle_info({recovery_progress, JobId, Step, Result}, State) ->
    %% Update recovery progress
    case State#state.active_recoveries#{JobId} of
        undefined ->
            {noreply, State};
        Job ->
            UpdatedJob = update_recovery_step(Job, Step, Result),
            UpdatedState = State#state{
                active_recoveries = maps:put(JobId, UpdatedJob, State#state.active_recoveries)
            },
            {noreply, UpdatedState}
    end;

handle_info({recovery_completed, JobId, Service}, State) ->
    %% Handle recovery completion
    case State#state.active_recoveries#{JobId} of
        undefined ->
            {noreply, State};
        Job ->
            %% Update circuit breaker
            UpdatedCircuitBreakers = update_circuit_breaker(Job, State#state.circuit_breakers),

            %% Log completion
            erlmcp_audit:log(recovery_completed, #{
                job_id => JobId,
                service => Service,
                duration => Job#recovery_job.completion_time - Job#recovery_job.start_time,
                timestamp => erlang:system_time(millisecond)
            }),

            %% Update state
            UpdatedState = State#state{
                active_recoveries = maps:remove(JobId, State#state.active_recoveries),
                recovery_history = [JobId | State#state.recovery_history],
                circuit_breakers = UpdatedCircuitBreakers
            },

            {noreply, UpdatedState}
    end;

handle_info({recovery_failed, JobId, Service, Reason}, State) ->
    %% Handle recovery failure
    case State#state.active_recoveries#{JobId} of
        undefined ->
            {noreply, State};
        Job ->
            TripCircuitBreaker(Service, State),

            %% Log failure
            erlmcp_audit:log(recovery_failed, #{
                job_id => JobId,
                service => Service,
                reason => Reason,
                timestamp => erlang:system_time(millisecond)
            }),

            %% Update state
            UpdatedState = State#state{
                active_recoveries = maps:remove(JobId, State#state.active_recoveries),
                recovery_history = [JobId | State#state.recovery_history]
            },

            {noreply, UpdatedState}
    end;

handle_info({health_check_complete, Service, Health}, State) ->
    %% Handle health check completion
    case ServiceHealth =:= healthy of
        true ->
            %% Check if automatic recovery is enabled
            case erlmcp_config:get(auto_recovery, false) of
                true ->
                    %% Trigger automatic recovery if needed
                    case should_trigger_auto_recovery(Service, Health, State) of
                        true -> recover(Service, #{auto => true});
                        false -> ok
                    end;
                false ->
                    ok
            end;
        false ->
            ok
    end,

    {noreply, State};

handle_info({circuit_breaker_trip, Service}, State) ->
    %% Handle circuit breaker trip
    UpdatedCircuitBreakers = trip_circuit_breaker(Service, State#state.circuit_breakers),
    UpdatedState = State#state{circuit_breakers = UpdatedCircuitBreakers},

    %% Notify of circuit breaker trip
    erlmcp_notifier:notify(operations, circuit_breaker_tripped, #{
        service => Service,
        timestamp => erlang:system_time(millisecond)
    }),

    {noreply, UpdatedState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel health checks
    erlang:cancel_timer(State#state.schedules),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
 Internal Functions
%%====================================================================

parse_config(Options) ->
    Defaults = #{
        recovery_timeout => ?RECOVERY_TIMEOUT,
        circuit_breaker_threshold => ?CIRCUIT_BREAKER_THRESHOLD,
        circuit_breaker_timeout => ?CIRCUIT_BREAKER_TIMEOUT,
        health_check_interval => ?HEALTH_CHECK_INTERVAL,
        max_concurrent_recoveries => ?MAX_CONCURRENT_RECOVERIES,
        auto_recovery => false
    },
    maps:merge(Defaults, maps:from_list(Options)).

load_service_dependencies() ->
    %% Load service dependencies from configuration
    case erlmcp_config:get(service_dependencies, #{}) of
        undefined ->
            %% Default dependencies
            #{
                session_service => [registry_service, transport_service],
                registry_service => [configuration_service],
                configuration_service => [],
                transport_service => [configuration_service],
                observability_service => []
            };
        Dependencies ->
            Dependencies
    end.

initialize_circuit_breakers() ->
    %% Initialize circuit breakers for all services
    Services = [session_service, registry_service, configuration_service,
               transport_service, observability_service],
    lists:foldl(fun(Service, Acc) ->
        maps:put(Service, active, Acc)
    end, #{}, Services).

start_health_checks() ->
    %% Start periodic health checks
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), {timeout, make_ref(), health_check}).

generate_recovery_id(Service) ->
    Timestamp = erlang:system_time(millisecond),
    Uuid = erlmcp_utils:uuid(),
    <<Service/binary, "_", (integer_to_binary(Timestamp))/binary, "_", Uuid/binary>>.

create_recovery_job(JobId, Service, Options) ->
    %% Create recovery job
    Dependencies = get_service_dependencies(Service),
    Steps = generate_recovery_steps(Service, Options),

    #recovery_job{
        id = JobId,
        service = Service,
        status = pending,
        start_time = erlang:system_time(millisecond),
        steps = Steps,
        current_step = 0,
        dependencies = Dependencies,
        circuit_breaker = proplists:get_value(circuit_breaker, Options, active),
        health = proplists:get_value(health, Options, unhealthy)
    }.

get_service_dependencies(Service) ->
    %% Get service dependencies
    case erlmcp_config:get(service_dependencies, #{}) of
        undefined -> [];
        Dependencies -> maps:get(Service, Dependencies, [])
    end.

generate_recovery_steps(Service, Options) ->
    %% Generate recovery steps based on service
    IsTest = proplists:get_value(test, Options, false),
    CustomSteps = proplists:get_value(steps, Options, []),

    case IsTest of
        true ->
            %% Test recovery steps (non-destructive)
            test_recovery_steps(Service);
        false ->
            case CustomSteps of
                [] ->
                    %% Default recovery steps
                    default_recovery_steps(Service);
                _ ->
                    CustomSteps
            end
    end.

test_recovery_steps(Service) ->
    %% Generate test recovery steps
    [
        #{step => 1, name => "Test Pre-check", action => test_pre_check, timeout => 5000},
        #{step => 2, name => "Test Service Start", action => test_service_start, timeout => 10000},
        #{step => 3, name => "Test Health Validation", action => test_health_validation, timeout => 5000},
        #{step => 4, name => "Test Data Restore", action => test_data_restore, timeout => 10000},
        #{step => 5, name => "Test Service Sync", action => test_service_sync, timeout => 5000},
        #{step => 6, name => "Test Verification", action => test_verification, timeout => 5000},
        #{step => 7, name => "Test Cleanup", action => test_cleanup, timeout => 5000}
    ].

default_recovery_steps(Service) ->
    %% Generate default recovery steps
    [
        #{step => 1, name => "Pre-recovery Check", action => pre_check, timeout => 10000},
        #{step => 2, name => "Start Service", action => start_service, timeout => 30000},
        #{step => 3, name => "Validate Health", action => validate_health, timeout => 15000},
        #{step => 4, name => "Restore Data", action => restore_data, timeout => 60000},
        #{step => 5, name => "Sync Services", action => sync_services, timeout => 20000},
        #{step => 6, name => "Verify Services", action => verify_services, timeout => 15000},
        #{step => 7, name => "Cleanup", action => cleanup, timeout => 5000}
    ].

can_perform_recovery(Service, State) ->
    %% Check if recovery is allowed
    case State#state.active_recoveries#{Service} of
        undefined ->
            %% Check if service has active recovery
            false;
        _ ->
            %% Check if circuit breaker is tripped
            case State#state.circuit_breakers#{Service} of
                active ->
                    true;
                tripped ->
                    false;
                reset ->
                    %% Check if enough time has passed since last trip
                    enough_time_since_trip(Service, State);
                _ ->
                    false
            end
    end.

start_recovery_process(Job, State) ->
    %% Start recovery process
    UpdatedJob = Job#recovery_job{status = in_progress},
    UpdatedState = State#state{
        active_recoveries = maps:put(Job#recovery_job.id, UpdatedJob, State#state.active_recoveries)
    },

    %% Start recovery sequence
    erlmcp_event_manager:notify(?MODULE, recovery_started, #{
        job_id => Job#recovery_job.id,
        service => Job#recovery_job.service
    }),

    execute_recovery_step(Job, UpdatedState).

execute_recovery_step(Job, State) ->
    %% Execute recovery step
    case Job#recovery_job.steps of
        [] ->
            %% All steps completed
            complete_recovery(Job#recovery_job.id, Job#recovery_job.service, State);
        [Step | Rest] ->
            %% Execute current step
            case execute_step(Step, Job, State) of
                {ok, UpdatedState} ->
                    %% Continue to next step
                    execute_recovery_step(Job#recovery_job{steps = Rest, current_step = Step#step + 1}, UpdatedState);
                {error, Reason} ->
                    %% Step failed
                    fail_recovery(Job#recovery_job.id, Job#recovery_job.service, Reason, State)
            end
    end.

execute_step(Step, Job, State) ->
    %% Execute recovery step
    case Step#{action} of
        pre_check -> execute_pre_check(Step, Job, State);
        start_service -> execute_start_service(Step, Job, State);
        validate_health -> execute_validate_health(Step, Job, State);
        restore_data -> execute_restore_data(Step, Job, State);
        sync_services -> execute_sync_services(Step, Job, State);
        verify_services -> execute_verify_services(Step, Job, State);
        cleanup -> execute_cleanup(Step, Job, State);
        test_pre_check -> execute_test_pre_check(Step, Job, State);
        test_service_start -> execute_test_service_start(Step, Job, State);
        test_health_validation -> execute_test_health_validation(Step, Job, State);
        test_data_restore -> execute_test_data_restore(Step, Job, State);
        test_service_sync -> execute_test_service_sync(Step, Job, State);
        test_verification -> execute_test_verification(Step, Job, State);
        test_cleanup -> execute_test_cleanup(Step, Job, State)
    end.

execute_pre_check(Step, Job, State) ->
    %% Execute pre-check
    case check_prerequisites(Job#recovery_job.service) of
        true -> {ok, State};
        false -> {error, prerequisites_not_met}
    end.

execute_start_service(Step, Job, State) ->
    %% Execute service start
    case erlmcp_service_manager:start(Job#recovery_job.service) of
        ok -> {ok, State};
        {error, Reason} -> {error, {service_start_failed, Reason}}
    end.

execute_validate_health(Step, Job, State) ->
    %% Execute health validation
    case erlmcp_health:check(Job#recovery_job.service) of
        healthy -> {ok, State};
        degraded -> {ok, State};  % Accept degraded during recovery
        unhealthy -> {error, service_unhealthy}
    end.

execute_restore_data(Step, Job, State) ->
    %% Execute data restore
    case erlmcp_backup_manager:restore(all, Job#recovery_job.service, #{}) of
        ok -> {ok, State};
        {error, Reason} -> {error, {restore_failed, Reason}}
    end.

execute_sync_services(Step, Job, State) ->
    %% Execute service sync
    case erlmcp_service_manager:sync(Job#recovery_job.service) of
        ok -> {ok, State};
        {error, Reason} -> {error, {sync_failed, Reason}}
    end.

execute_verify_services(Step, Job, State) ->
    %% Execute service verification
    case verify_service_health(Job#recovery_job.service) of
        true -> {ok, State};
        false -> {error, service_verification_failed}
    end.

execute_cleanup(Step, Job, State) ->
    %% Execute cleanup
    case erlmcp_cleanup:cleanup_recovery_artifacts(Job#recovery_job.service) of
        ok -> {ok, State};
        {error, Reason} -> {error, {cleanup_failed, Reason}}
    end.

execute_test_pre_check(Step, Job, State) ->
    %% Execute test pre-check
    case check_prerequisites(Job#recovery_job.service) of
        true -> {ok, State};
        false -> {error, test_prerequisites_not_met}
    end.

execute_test_service_start(Step, Job, State) ->
    %% Execute test service start (dry run)
    case erlmcp_service_manager:test_start(Job#recovery_job.service) of
        ok -> {ok, State};
        {error, Reason} -> {error, {test_service_start_failed, Reason}}
    end.

execute_test_health_validation(Step, Job, State) ->
    %% Execute test health validation
    case erlmcp_health:test_check(Job#recovery_job.service) of
        healthy -> {ok, State};
        degraded -> {ok, State};
        unhealthy -> {error, test_service_unhealthy}
    end.

execute_test_data_restore(Step, Job, State) ->
    %% Execute test data restore (read-only)
    case erlmcp_backup_manager:test_restore(Job#recovery_job.service) of
        ok -> {ok, State};
        {error, Reason} -> {error, {test_restore_failed, Reason}}
    end.

execute_test_service_sync(Step, Job, State) ->
    %% Execute test service sync (read-only)
    case erlmcp_service_manager:test_sync(Job#recovery_job.service) of
        ok -> {ok, State};
        {error, Reason} -> {error, {test_sync_failed, Reason}}
    end.

execute_test_verification(Step, Job, State) ->
    %% Execute test verification
    case verify_service_health(Job#recovery_job.service) of
        true -> {ok, State};
        false -> {error, test_service_verification_failed}
    end.

execute_test_cleanup(Step, Job, State) ->
    %% Execute test cleanup
    case erlmcp_cleanup:test_cleanup_recovery_artifacts(Job#recovery_job.service) of
        ok -> {ok, State};
        {error, Reason} -> {error, {test_cleanup_failed, Reason}}
    end.

update_recovery_step(Job, Step, Result) ->
    %% Update recovery step progress
    case Result of
        {ok, _} ->
            %% Step completed successfully
            erlmcp_event_manager:notify(?MODULE, recovery_progress, #{
                job_id => Job#recovery_job.id,
                service => Job#recovery_job.service,
                step => Step,
                result => success
            });
        {error, Reason} ->
            %% Step failed
            erlmcp_event_manager:notify(?MODULE, recovery_progress, #{
                job_id => Job#recovery_job.id,
                service => Job#recovery_job.service,
                step => Step,
                result => {error, Reason}
            })
    end,

    Job#recovery_job{current_step = Step#step}.

complete_recovery(JobId, Service, State) ->
    %% Complete recovery
    erlmcp_event_manager:notify(?MODULE, recovery_completed, #{
        job_id => JobId,
        service => Service,
        timestamp => erlang:system_time(millisecond)
    }),

    State.

fail_recovery(JobId, Service, Reason, State) ->
    %% Fail recovery
    erlmcp_event_manager:notify(?MODULE, recovery_failed, #{
        job_id => JobId,
        service => Service,
        reason => Reason,
        timestamp => erlang:system_time(millisecond)
    }),

    State.

trip_circuit_breaker(Service, State) ->
    %% Trip circuit breaker
    UpdatedCircuitBreakers = maps:put(Service, tripped, State#state.circuit_breakers),

    %% Set timeout for reset
    erlang:send_after(State#state.config#{circuit_breaker_timeout}, self(),
                     {timeout, Service, circuit_breaker_reset}),

    UpdatedCircuitBreakers.

update_circuit_breaker(Job, CircuitBreakers) ->
    %% Update circuit breaker based on recovery result
    case Job#recovery_job.status of
        completed ->
            %% Reset circuit breaker on success
            maps:put(Job#recovery_job.service, active, CircuitBreakers);
        failed ->
            TripCircuitBreaker(Job#recovery_job.service, CircuitBreakers)
    end.

enough_time_since_trip(Service, State) ->
    %% Check if enough time has passed since last circuit breaker trip
    case State#state.circuit_breakers#{Service} of
        reset ->
            %% Get last trip time
            case erlmcp_storage:get(circuit_breaker_trip_time, Service) of
                undefined -> true;
                Time ->
                    CurrentTime = erlang:system_time(millisecond),
                    (CurrentTime - Time) > State#state.config#{circuit_breaker_timeout}
            end;
        _ ->
            true
    end.

should_trigger_auto_recovery(Service, Health, State) ->
    %% Check if automatic recovery should be triggered
    case Health of
        unhealthy ->
            case State#state.circuit_breakers#{Service} of
                active -> true;
                _ -> false
            end;
        _ ->
            false
    end.

check_prerequisites(Service) ->
    %% Check recovery prerequisites
    case erlmcp_health:check(Service) of
        healthy -> false;  % Don't recover healthy services
        _ -> true
    end.

verify_service_health(Service) ->
    %% Verify service health
    case erlmcp_health:check(Service) of
        healthy -> true;
        degraded -> true;  % Accept degraded during recovery
        unhealthy -> false
    end.

generate_recovery_plan(Service, State) ->
    %% Generate recovery plan for service
    Dependencies = get_service_dependencies(Service),
    Steps = default_recovery_steps(Service),

    #{
        service => Service,
        dependencies => Dependencies,
        steps => Steps,
        estimated_duration => calculate_estimated_duration(Steps),
        circuit_breaker => State#state.circuit_breakers#{Service},
        health => erlmcp_health:check(Service)
    }.

calculate_estimated_duration(Steps) ->
    %% Calculate estimated duration for recovery steps
    lists:foldl(fun(Step, Total) ->
        Total + Step#{timeout}
    end, 0, Steps).

build_status_report(State, Options) ->
    %% Build comprehensive status report
    ActiveRecoveries = maps:values(State#state.active_recoveries),

    #{
        active_recoveries => length(ActiveRecoveries),
        max_concurrent => State#state.config#{max_concurrent_recoveries},
        circuit_breakers => State#state.circuit_breakers,
        service_dependencies => State#state.service_dependencies,
        recovery_history => length(State#state.recovery_history),
        auto_recovery => State#state.config#{auto_recovery},
        next_health_check => erlang:system_time(millisecond) + State#state.config#{health_check_interval},
        active_recoveries_details => [format_recovery_job(Job) || Job <- ActiveRecoveries]
    }.

format_recovery_job(Job) ->
    %% Format recovery job for display
    #{
        id => Job#recovery_job.id,
        service => Job#recovery_job.service,
        status => Job#recovery_job.status,
        current_step => Job#recovery_job.current_step,
        total_steps => length(Job#recovery_job.steps),
        duration => erlang:system_time(millisecond) - Job#recovery_job.start_time,
        circuit_breaker => Job#recovery_job.circuit_breaker,
        health => Job#recovery_job.health
    }.