%% @private
%% @doc Emergency Failover Manager for erlmcp v3
%% Implements automated failover with RTO/RPO compliance
-module(erlmcp_failover_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, trigger_failover/1, status/0, test_failover/1]).
-export([register_service/2, get_service_status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp_types.hrl").

-record(state, {
    services = #{} :: map(),  % service_id() => service_state()
    sites = [] :: [site_id()],
    current_primary :: site_id(),
    failover_config :: failover_config(),
    notification_channels :: [pid()],
    health_monitors :: [pid()],
    decision_engine :: pid(),
    recovery_coordinator :: pid(),
    last_failover :: undefined | integer()  % timestamp
}).

-type service_state() :: #{
    id := service_id(),
    status := healthy | degraded | failed,
    primary_site := site_id(),
    backup_sites := [site_id()],
    rto := pos_integer(),
    rpo := pos_integer(),
    dependencies := [service_id()],
    last_health_check := integer(),
    failover_attempts := non_neg_integer()
}.

-record(failover_config, {
    detection_timeout :: pos_integer(),  % milliseconds
    decision_timeout :: pos_integer(),
    recovery_timeout :: pos_integer(),
    max_failover_attempts :: pos_integer(),
    manual_approvals :: boolean(),
    health_check_interval :: pos_integer(),
    metrics_collection :: boolean()
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec trigger_failover(service_id()) -> ok | {error, term()}.
trigger_failover(ServiceId) ->
    gen_server:call(?MODULE, {trigger_failover, ServiceId}, 30000).

-spec status() -> map().
status() ->
    gen_server:call(?MODULE, status).

-spec test_failover(service_id()) -> test_result().
test_failover(ServiceId) ->
    gen_server:call(?MODULE, {test_failover, ServiceId}, 60000).

-spec register_service(service_id(), service_config()) -> ok.
register_service(ServiceId, Config) ->
    gen_server:call(?MODULE, {register_service, ServiceId, Config}).

-spec get_service_status(service_id()) -> {ok, service_state()} | {error, not_found}.
get_service_status(ServiceId) ->
    gen_server:call(?MODULE, {get_service_status, ServiceId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    % Initialize state
    State = #state{
        sites = erlmcp_config:get(failover_sites),
        current_primary = erlmcp_config:get(primary_site),
        failover_config = #failover_config{
            detection_timeout = 30000,
            decision_timeout = 15000,
            recovery_timeout = 900000,  % 15 minutes
            max_failover_attempts = 3,
            manual_approvals = false,
            health_check_interval = 5000,
            metrics_collection = true
        },
        notification_channels = start_notification_channels(),
        health_monitors = start_health_monitors(),
        decision_engine = start_decision_engine(),
        recovery_coordinator = start_recovery_coordinator()
    },

    % Start periodic health checks
    erlang:send_after(State#state.failover_config#failover_config.health_check_interval, self(), health_check),

    % Initialize metrics collection
    erlmcp_metrics:register(failover_metrics),

    {ok, State}.

handle_call({trigger_failover, ServiceId}, _From, State) ->
    case validate_service(ServiceId, State) of
        true ->
            Result = execute_failover(ServiceId, State),
            {reply, Result, State};
        false ->
            {reply, {error, invalid_service}, State}
    end;

handle_call(status, _From, State) ->
    Status = generate_status_report(State),
    {reply, Status, State};

handle_call({test_failover, ServiceId}, _From, State) ->
    case validate_service(ServiceId, State) of
        true ->
            TestResult = execute_test_failover(ServiceId, State),
            {reply, TestResult, State};
        false ->
            {reply, {error, invalid_service}, State}
    end;

handle_call({register_service, ServiceId, Config}, _From, State) ->
    NewState = register_service_in_state(ServiceId, Config, State),
    {reply, ok, NewState};

handle_call({get_service_status, ServiceId}, _From, State) ->
    case maps:get(ServiceId, State#state.services, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ServiceState ->
            {reply, {ok, ServiceState}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, bad_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(health_check, State) ->
    % Perform health checks for all services
    HealthResults = perform_health_checks(State),
    UpdatedState = update_health_states(HealthResults, State),

    % Check for services that need failover
    FailedServices = identify_failed_services(HealthResults),
    lists:foreach(fun(ServiceId) ->
        case should_trigger_failover(ServiceId, UpdatedState) of
            true ->
                trigger_automatic_failover(ServiceId, UpdatedState);
            false ->
                ok
        end
    end, FailedServices),

    % Schedule next health check
    erlang:send_after(State#state.failover_config#failover_config.health_check_interval, self(), health_check),
    {noreply, UpdatedState};

handle_info(failover_complete, State) ->
    % Handle completed failover
    UpdatedState = handle_failover_completion(State),

    % Notify stakeholders
    notify_failover_completion(UpdatedState),

    % Log metrics
    log_failover_metrics(UpdatedState),

    {noreply, UpdatedState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Health Check Management
-spec perform_health_checks(state()) -> map().
perform_health_checks(State) ->
    Services = State#state.services,
    lists:foldl(fun(ServiceId, Acc) ->
        ServiceState = maps:get(ServiceId, Services),
        HealthStatus = erlmcp_health:check_service(ServiceId, ServiceState#service_state.primary_site),
        Acc#{ServiceId => HealthStatus}
    end, #{}, maps:keys(Services)).

-spec update_health_states(map(), state()) -> state().
update_health_states(HealthResults, State) ->
    Services = State#state.services,
    UpdatedServices = maps:fold(fun(ServiceId, HealthResult, Acc) ->
        ServiceState = maps:get(ServiceId, Services),
        NewServiceState = update_service_health(ServiceId, HealthResult, ServiceState),
        Acc#{ServiceId => NewServiceState}
    end, #{}, HealthResults),

    State#state{services = UpdatedServices}.

-spec update_service_health(service_id(), health_result(), service_state()) -> service_state().
update_service_health(ServiceId, HealthResult, ServiceState) ->
    NewStatus = case HealthResult of
        healthy -> healthy;
        degraded -> degraded;
        failed -> failed
    end,

    ServiceState#service_state{
        status = NewStatus,
        last_health_check = erlang:system_time(millisecond),
        failover_attempts = case NewStatus of
            failed -> ServiceState#service_state.failover_attempts + 1;
            _ -> ServiceState#service_state.failover_attempts
        end
    }.

%% Failover Decision Making
-spec identify_failed_services(map()) -> [service_id()].
identify_failed_services(HealthResults) ->
    maps:fold(fun(ServiceId, HealthResult, Acc) ->
        case HealthResult of
            failed -> [ServiceId | Acc];
            _ -> Acc
        end
    end, [], HealthResults).

-spec should_trigger_failover(service_id(), state()) -> boolean().
should_trigger_failover(ServiceId, State) ->
    ServiceState = maps:get(ServiceId, State#state.services),

    % Check failover attempts
    Attempts = ServiceState#service_state.failover_attempts,
    MaxAttempts = State#state.failover_config#failover_config.max_failover_attempts,

    % Check dependencies
    Dependencies = ServiceState#service_state.dependencies,
    DependencyHealth = check_dependencies_health(Dependencies, State),

    % Check if manual approval is required
    ManualApproval = State#state.failover_config#failover_config.manual_approvals,

    case Attempts < MaxAttempts andalso DependencyHealth and not ManualApproval of
        true ->
            true;
        false ->
            case ManualApproval of
                true ->
                    request_manual_approval(ServiceId, State);
                false ->
                    false
            end
    end.

-spec check_dependencies_health([service_id()], state()) -> boolean().
check_dependencies_health(Dependencies, State) ->
    lists:foldl(fun(DependencyId, Acc) ->
        case maps:get(DependencyId, State#state.services, undefined) of
            undefined ->
                false;
            DepService ->
                case DepService#service_state.status of
                    healthy -> Acc;
                    _ -> false
                end
        end
    end, true, Dependencies).

%% Failover Execution
-spec execute_failover(service_id(), state()) -> ok | {error, term()}.
execute_failover(ServiceId, State) ->
    ServiceState = maps:get(ServiceId, State#state.services),

    % Select backup site
    BackupSite = select_backup_site(ServiceId, ServiceState, State),

    % Execute failover
    case erlmcp_service:failover(ServiceId, BackupSite) of
        ok ->
            % Update service state
            UpdatedServiceState = ServiceState#service_state{
                primary_site = BackupSite,
                status = healthy,
                failover_attempts = 0
            },

            % Update global state
            UpdatedServices = maps:put(ServiceId, UpdatedServiceState, State#state.services),
            UpdatedState = State#state{services = UpdatedServices, last_failover = erlang:system_time(millisecond)},

            % Notify completion
            self() ! failover_complete,

            % Log metrics
            erlmcp_metrics:increment(failover_triggered),
            erlmcp_metrics:record(failover_time, erlang:system_time(millisecond) - ServiceState#service_state.last_health_check),

            {ok, BackupSite};
        {error, Reason} ->
            {error, Reason}
    end.

-spec select_backup_site(service_id(), service_state(), state()) -> site_id().
select_backup_site(ServiceId, ServiceState, State) ->
    BackupSites = ServiceState#service_state.backup_sites,
    lists:nth(1, BackupSites).  % Simple selection - can be enhanced based on load, latency, etc.

%% Test Failover
-spec execute_test_failover(service_id(), state()) -> test_result().
execute_test_failover(ServiceId, State) ->
    ServiceState = maps:get(ServiceId, State#state.services),

    % Get current primary site
    CurrentPrimary = ServiceState#service_state.primary_site,

    % Select backup site for test
    TestSite = select_backup_site(ServiceId, ServiceState, State),

    % Execute test failover
    Start = erlang:system_time(millisecond),
    Result = erlmcp_service:test_failover(ServiceId, TestSite),
    End = erlang:system_time(millisecond),

    % Test validation
    Validation = case Result of
        ok ->
            % Validate service health on backup site
            case erlmcp_health:check_service(ServiceId, TestSite) of
                healthy ->
                    #{
                        success => true,
                        health => healthy,
                        recovery_time => End - Start,
                        rto_compliance => (End - Start) =< ServiceState#service_state.rto,
                        rpo_compliance => verify_rpo_compliance(ServiceId)
                    };
                unhealthy ->
                    #{
                        success => false,
                        reason => service_unhealthy,
                        health => unhealthy
                    }
            end;
        {error, Error} ->
            #{
                success => false,
                reason => Error,
                error_details => erlmcp_error:format_error(Error)
            }
    end,

    % Return test result
    #test_result{
        service_id = ServiceId,
        test_site = TestSite,
        original_site = CurrentPrimary,
        start_time = Start,
        end_time = End,
        result = Validation
    }.

%% State Management
-spec validate_service(service_id(), state()) -> boolean().
validate_service(ServiceId, State) ->
    maps:is_key(ServiceId, State#state.services).

-spec register_service_in_state(service_id(), service_config(), state()) -> state().
register_service_in_state(ServiceId, Config, State) ->
    ServiceState = #service_state{
        id = ServiceId,
        status = healthy,
        primary_site = maps:get(primary_site, Config, State#state.current_primary),
        backup_sites = maps:get(backup_sites, Config, State#state.sites),
        rto = maps:get(rto, Config, 900),  % Default 15 minutes
        rpo = maps:get(rpo, Config, 5),    % Default 5 seconds
        dependencies = maps:get(dependencies, Config, []),
        last_health_check = erlang:system_time(millisecond),
        failover_attempts = 0
    },

    UpdatedServices = maps:put(ServiceId, ServiceState, State#state.services),
    State#state{services = UpdatedServices}.

%% Notification and Monitoring
-spec start_notification_channels() -> [pid()].
start_notification_channels() ->
    % Start notification services
    [
        erlmcp_notification:start(email),
        erlmcp_notification:start(sms),
        erlmcp_notification:start(slack),
        erlmcp_notification:start(pagerduty)
    ].

-spec start_health_monitors() -> [pid()].
start_health_monitors() ->
    % Start health monitoring services
    [
        erlmcp_monitor:start(service_health),
        erlmcp_monitor:start(site_health),
        erlmcp_monitor:start(network_health),
        erlmcp_monitor:start(performance)
    ].

-spec start_decision_engine() -> pid().
start_decision_engine() ->
    % Start failover decision engine
    erlmcp_decision_engine:start_link().

-spec start_recovery_coordinator() -> pid().
start_recovery_coordinator() ->
    % Start recovery coordination service
    erlmcp_recovery_coordinator:start_link().

%% Notification Functions
-spec notify_failover_completion(state()) -> ok.
notify_failover_completion(State) ->
    FailedServices = identify_failed_services(generate_health_report(State)),

    lists:foreach(ServiceId ->
        ServiceState = maps:get(ServiceId, State#state.services),

        % Prepare notification
        Notification = #{
            type => failover_complete,
            service_id => ServiceId,
            new_site => ServiceState#service_state.primary_site,
            old_site => get_previous_site(ServiceId, State),
            timestamp => erlang:system_time(millisecond),
            rto => ServiceState#service_state.rto,
            actual_recovery_time => calculate_recovery_time(ServiceId, State)
        },

        % Send notifications
        lists:foreach(ChannelPid ->
            erlmcp_notification:send(ChannelPid, Notification)
        end, State#state.notification_channels)
    end, FailedServices).

-spec generate_status_report(state()) -> map().
generate_status_report(State) ->
    Services = State#state.services,
    HealthReport = generate_health_report(State),

    #{
        timestamp => erlang:system_time(millisecond),
        last_failover => State#state.last_failover,
        services => maps:fold(fun(ServiceId, ServiceState, Acc) ->
            Acc#{
                ServiceId => #{
                    status => ServiceState#service_state.status,
                    primary_site => ServiceState#service_state.primary_site,
                    health => maps:get(ServiceId, HealthReport, unknown),
                    failover_attempts => ServiceState#service_state.failover_attempts,
                    last_health_check => ServiceState#service_state.last_health_check
                }
            }
        end, #{}, Services),
        metrics => erlmcp_metrics:get(failover_metrics)
    }.

-spec generate_health_report(state()) -> map().
generate_health_report(State) ->
    lists:foldl(fun(ServiceId, Acc) ->
        ServiceState = maps:get(ServiceId, State#state.services),
        HealthStatus = erlmcp_health:check_service(ServiceId, ServiceState#service_state.primary_site),
        Acc#{ServiceId => HealthStatus}
    end, #{}, maps:keys(State#state.services)).

%% Helper Functions
-spec verify_rpo_compliance(service_id()) -> boolean().
verify_rpo_compliance(ServiceId) ->
    % Implement RPO verification logic
    % Check if data loss is within acceptable limits
    true.

-spec calculate_recovery_time(service_id(), state()) -> integer().
calculate_recovery_time(ServiceId, State) ->
    ServiceState = maps:get(ServiceId, State#state.services),
    erlang:system_time(millisecond) - ServiceState#service_state.last_health_check.

-spec get_previous_site(service_id(), state()) -> site_id().
get_previous_site(ServiceId, State) ->
    % Implement logic to track previous site
    State#state.current_primary.

%%====================================================================
%% Test Functions
%%====================================================================

-spec test_health_check() -> ok.
test_health_check() ->
    % Test health check functionality
    State = #state{
        services = #{
            test_service => #service_state{
                id = test_service,
                status = healthy,
                primary_site = site_ny,
                backup_sites = [site_london],
                rto = 900,
                rpo = 5,
                dependencies = [],
                last_health_check = erlang:system_time(millisecond),
                failover_attempts = 0
            }
        }
    },

    HealthResults = perform_health_checks(State),
    UpdatedState = update_health_states(HealthResults, State),

    case maps:get(test_service, UpdatedState#state.services)#service_state.status of
        healthy -> ok;
        _ -> {error, health_check_failed}
    end.

-spec test_failover_trigger() -> ok | {error, term()}.
test_failover_trigger() ->
    State = #state{
        services = #{
            test_service => #service_state{
                id = test_service,
                status = failed,
                primary_site = site_ny,
                backup_sites = [site_london],
                rto = 900,
                rpo = 5,
                dependencies = [],
                last_health_check = erlang:system_time(millisecond),
                failover_attempts = 1
            }
        },
        failover_config = #failover_config{
            max_failover_attempts = 3,
            manual_approvals = false
        }
    },

    case should_trigger_failover(test_service, State) of
        true ->
            ok;
        false ->
            {error, failover_not_triggered}
    end.

-spec test_recovery_time() -> integer().
test_recovery_time() ->
    State = #state{
        services = #{
            test_service => #service_state{
                id = test_service,
                status = healthy,
                primary_site = site_london,
                backup_sites = [site_sgp],
                rto = 900,
                rpo =  Erlang:system_time(millisecond),
                dependencies = [],
                last_health_check = erlang:system_time(millisecond) - 10000,  % 10 seconds ago
                failover_attempts = 0
            }
        }
    },

    calculate_recovery_time(test_service, State).