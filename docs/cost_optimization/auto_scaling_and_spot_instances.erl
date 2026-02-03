%%====================================================================
%% erlmcp Auto-Scaling and Spot Instances Implementation
%%====================================================================
%%
%% This module implements comprehensive auto-scaling and spot instance management
%% for erlmcp v3 deployment, including cost optimization and reliability features.
%%

-module(erlmcp_auto_scaler).

-behaviour(gen_server).

-export([start_link/0, enable_auto_scaling/1, enable_spot_instances/1,
         get_scaling_policy/0, get_spot_reservation/0, predict_costs/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Records for auto-scaling
-record(scaling_policy,
        {name :: binary(),
         min_replicas :: integer(),
         max_replicas :: integer(),
         target_cpu :: float(),
         target_memory :: float(),
         scale_up_threshold :: float(),
         scale_down_threshold :: float(),
         scale_up_step :: integer(),
         scale_down_step :: integer(),
         stabilization_window_seconds :: integer(),
         cooldown_period_seconds :: integer(),
         metrics :: [#{metric := binary(), target := float()}]}).

-record(scaling_activity,
        {id :: binary(),
         action :: scale_up | scale_down | maintain,
         replicas_before :: integer(),
         replicas_after :: integer(),
         trigger :: cpu | memory | custom,
         timestamp :: integer(),
         cost_impact :: float(),
         status :: pending | in_progress | completed | failed}).

%% Records for spot instances
-record(spot_reservation,
        {id :: binary(),
         instance_type :: binary(),
         availability_zone :: binary(),
         max_price :: float(),
         duration_seconds :: integer(),
         start_time :: integer(),
         end_time :: integer(),
         instance_count :: integer(),
         active_instances :: integer(),
         state :: pending | active | failed | completed,
         disruption_budget :: float()}).

-record(spot_price_history,
        {timestamp :: integer(),
         instance_type :: binary(),
         availability_zone :: binary(),
         price :: float(),
         spot_capacity :: integer()}).

%% Records for cost prediction
-record(cost_forecast,
        {timestamp :: integer(),
         period_hours :: integer(),
         scenarios :: [#{scenario := binary(),
                        cost :: float(),
                        confidence :: float(),
                        factors := [binary()]}]}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec enable_auto_scaling(binary()) -> ok | {error, term()}.
enable_auto_scaling(PolicyName) ->
    gen_server:call(?MODULE, {enable_auto_scaling, PolicyName}, 30000).

-spec enable_spot_instances(map()) -> ok | {error, term()}.
enable_spot_instances(Config) ->
    gen_server:call(?MODULE, {enable_spot_instances, Config}, 30000).

-spec get_scaling_policy() -> #scaling_policy{} | {error, term()}.
get_scaling_policy() ->
    gen_server:call(?MODULE, get_scaling_policy, 5000).

-spec get_spot_reservation() -> [#spot_reservation{}].
get_spot_reservation() ->
    gen_server:call(?MODULE, get_spot_reservation, 5000).

-spec predict_costs(integer()) -> #cost_forecast{}.
predict_costs(Hours) ->
    gen_server:call(?MODULE, {predict_costs, Hours}, 10000).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, map()}.
init([]) ->
    %% Initialize scaling policies
    ScalingPolicies = load_scaling_policies(),

    %% Initialize spot reservations
    SpotReservations = load_spot_reservations(),

    %% Initialize price history
    PriceHistory = load_price_history(),

    %% Start cost prediction
    start_cost_prediction(),

    %% Start monitoring
    schedule_monitoring(),

    State = #{
        scaling_policies => ScalingPolicies,
        active_policy => undefined,
        spot_reservations => SpotReservations,
        price_history => PriceHistory,
        scaling_activities => [],
        last_scale_event => undefined,
        monitoring_interval => 30000,  % 30 seconds
        prediction_interval => 3600000  % 1 hour
    },

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, map()) -> {reply, term(), map()}.
handle_call({enable_auto_scaling, PolicyName}, _From, State) ->
    %% Validate and enable scaling policy
    case validate_scaling_policy(PolicyName) of
        {ok, Policy} ->
            %% Enable the policy
            NewState = State#{
                active_policy => PolicyName,
                scaling_policies => maps:put(PolicyName, Policy, State#scaling_policies)
            },

            %% Initialize scaling
            initialize_scaling(Policy),

            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({enable_spot_instances, Config}, _From, State) ->
    %% Create spot reservation
    case create_spot_reservation(Config) of
        {ok, Reservation} ->
            %% Add to active reservations
            NewReservations = [Reservation | State#spot_reservations],
            NewState = State#{
                spot_reservations => NewReservations
            },

            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_scaling_policy, _From, State) ->
    case State#active_policy of
        undefined ->
            {reply, {error, no_active_policy}, State};
        PolicyName ->
            case maps:get(PolicyName, State#scaling_policies, undefined) of
                undefined ->
                    {reply, {error, policy_not_found}, State};
                Policy ->
                    {reply, Policy, State}
            end
    end;

handle_call(get_spot_reservation, _From, State) ->
    {reply, State#spot_reservations, State};

handle_call({predict_costs, Hours}, _From, State) ->
    %% Generate cost forecast
    Forecast = generate_cost_forecast(Hours, State),

    {reply, Forecast, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast(monitor_resources, State) ->
    %% Perform scaling check
    case State#active_policy of
        undefined ->
            %% No active policy, skip scaling
            ok;
        PolicyName ->
            %% Check if scaling is needed
            ScalingNeeded = check_scaling_needed(PolicyName, State),
            if
                ScalingNeeded ->
                    %% Perform scaling
                    ScalingAction = determine_scaling_action(PolicyName, State),
                    execute_scaling_action(ScalingAction, State);
                true ->
                    ok
            end
    end,

    %% Check spot instance health
    check_spot_instance_health(State),

    %% Schedule next monitoring
    schedule_monitoring(),

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(resource_monitoring, State) ->
    %% Trigger monitoring cast
    ?MODULE:monitor_resources(),

    {noreply, State};

handle_info(cost_prediction, State) ->
    %% Update cost forecast
    UpdatedForecast = generate_cost_forecast(24, State),  % 24 hours forecast

    %% Store updated forecast
    store_cost_forecast(UpdatedForecast),

    {noreply, State};

handle_info(spot_interruption, State) ->
    %% Handle spot instance interruption
    case handle_spot_interruption(State) of
        ok ->
            %% Successfully handled interruption
            log_info("Spot instance interruption handled successfully");
        {error, Reason} ->
            log_error("Failed to handle spot interruption: ~p", [Reason])
    end,

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    %% Clean up resources
    cleanup_scaling_resources(),
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Load scaling policies
load_scaling_policies() ->
    %% Load from configuration
    PoliciesConfig = get_config(scaling_policies, #{}),

    %% Convert to records
    maps:map(fun(PolicyName, Config) ->
        policy_to_record(PolicyName, Config)
    end, PoliciesConfig).

%% Load spot reservations
load_spot_reservations() ->
    %% Load from database
    Reservations = erlmcp_database:query(#{
        collection => spot_reservations,
        filter => #{state => #{'$in' => [active, pending]}}
    }),

    %% Convert to records
    lists:map(fun reservation_to_record/1, Reservations).

%% Load price history
load_price_history() ->
    %% Load from time series database
    History = erlmcp_metrics:query_time_series(
        os:system_time(millisecond) - 864000000,  % 10 days
        os:system_time(millisecond),
        [spot_price]
    ),

    %% Convert to records
    lists:map(fun price_to_record/1, History).

%% Schedule monitoring
schedule_monitoring() ->
    %% Schedule resource monitoring
    erlang:send_after(?MODULE, resource_monitoring, get_env(monitoring_interval, 30000)).

%% Start cost prediction
start_cost_prediction() ->
    %% Schedule cost prediction
    erlang:send_after(?MODULE, cost_prediction, get_env(prediction_interval, 3600000)).

%% Validate scaling policy
validate_scaling_policy(PolicyName) ->
    case get_config(scaling_policies, #{}) of
        #{PolicyName := Config} ->
            %% Validate policy configuration
            ValidConfig = validate_policy_config(Config),
            if
                ValidConfig ->
                    {ok, policy_to_record(PolicyName, Config)};
                true ->
                    {error, invalid_policy_config}
            end;
        _ ->
            {error, policy_not_found}
    end.

%% Validate policy configuration
validate_policy_config(Config) ->
    %% Check required fields
    RequiredFields = [
        min_replicas, max_replicas, target_cpu, target_memory,
        scale_up_threshold, scale_down_threshold
    ],

    %% Check if all required fields are present
    lists:all(fun(Field) ->
        maps:is_key(Field, Config)
    end, RequiredFields) andalso
    %% Check value ranges
    maps:get(min_replicas, Config) > 0 andalso
    maps:get(max_replicas, Config) >= maps:get(min_replicas, Config) andalso
    maps:get(target_cpu, Config) >= 0 andalso maps:get(target_cpu, Config) =< 100 andalso
    maps:get(target_memory, Config) >= 0 andalso maps:get(target_memory, Config) =< 100 andalso
    maps:get(scale_up_threshold, Config) > maps:get(target_cpu, Config) andalso
    maps:get(scale_down_threshold, Config) < maps:get(target_cpu, Config).

%% Policy to record conversion
policy_to_record(PolicyName, Config) ->
    #scaling_policy{
        name = PolicyName,
        min_replicas = maps:get(min_replicas, Config, 2),
        max_replicas = maps:get(max_replicas, Config, 10),
        target_cpu = maps:get(target_cpu, Config, 70),
        target_memory = maps:get(target_memory, Config, 80),
        scale_up_threshold = maps:get(scale_up_threshold, Config, 80),
        scale_down_threshold = maps:get(scale_down_threshold, Config, 30),
        scale_up_step = maps:get(scale_up_step, Config, 2),
        scale_down_step = maps:get(scale_down_step, Config, 1),
        stabilization_window_seconds = maps:get(stabilization_window_seconds, Config, 300),
        cooldown_period_seconds = maps:get(cooldown_period_seconds, Config, 300),
        metrics = maps:get(metrics, Config, [])
    }.

%% Create spot reservation
create_spot_reservation(Config) ->
    %% Extract parameters
    InstanceType = maps:get(instance_type, Config, "m5.large"),
    AvailabilityZone = maps:get(availability_zone, Config, "us-east-1a"),
    MaxPrice = maps:get(max_price, Config, 0.1),
    Count = maps:get(count, Config, 1),
    Duration = maps:get(duration_hours, Config, 1) * 3600,

    %% Create reservation
    ReservationId = generate_reservation_id(),
    StartTime = os:system_time(millisecond),
    EndTime = StartTime + Duration * 1000,

    Reservation = #{
        id => ReservationId,
        instance_type => InstanceType,
        availability_zone => AvailabilityZone,
        max_price => MaxPrice,
        duration_seconds => Duration,
        start_time => StartTime,
        end_time => EndTime,
        count => Count,
        state => pending
    },

    %% Submit reservation request
    case submit_spot_request(Reservation) of
        {ok, Response} ->
            %% Update reservation with response data
            UpdatedReservation = update_reservation_with_response(Reservation, Response),

            %% Store in database
            erlmcp_database:insert(#{
                collection => spot_reservations,
                document => UpdatedReservation
            }),

            {ok, UpdatedReservation};
        {error, Reason} ->
            {error, Reason}
    end.

%% Check if scaling is needed
check_scaling_needed(PolicyName, State) ->
    %% Get current resource metrics
    Metrics = collect_current_metrics(),

    %% Get policy
    Policy = maps:get(PolicyName, State#scaling_policies),

    %% Check if metrics exceed thresholds
    CPU = Metrics#resource_metrics.cpu_usage,
    Memory = Metrics#resource_metrics.memory_usage,

    ScalingNeeded = case CPU >= Policy#scaling_policy.scale_up_threshold of
        true ->
            true;
        false when CPU <= Policy#scaling_policy.scale_down_threshold andalso
                   Memory <= Policy#scaling_policy.scale_down_threshold ->
            true;
        false ->
            false
    end,

    %% Check stabilization window
    case State#last_scale_event of
        undefined ->
            ScalingNeeded;
        _ ->
            TimeSinceLast = os:system_time(millisecond) - State#last_scale_event,
            TimeWindow = Policy#scaling_policy.stabilization_window_seconds * 1000,
            TimeSinceLast >= TimeWindow andalso ScalingNeeded
    end.

%% Determine scaling action
determine_scaling_action(PolicyName, State) ->
    %% Get current metrics
    Metrics = collect_current_metrics(),

    %% Get policy
    Policy = maps:get(PolicyName, State#scaling_policies),

    %% Get current replica count
    CurrentReplicas = get_current_replica_count(),

    %% Determine action
    CPU = Metrics#resource_metrics.cpu_usage,
    Memory = Metrics#resource_metrics.memory_usage,

    if
        CPU >= Policy#scaling_policy.scale_up_threshold ->
            %% Scale up
            StepSize = min(Policy#scaling_policy.scale_up_step,
                          Policy#scaling_policy.max_replicas - CurrentReplicas),
            #{
                action => scale_up,
                target_replicas => CurrentReplicas + StepSize,
                trigger => cpu,
                policy => PolicyName
            };
        CPU <= Policy#scaling_policy.scale_down_threshold andalso
        Memory <= Policy#scaling_policy.scale_down_threshold andalso
        CurrentReplicas > Policy#scaling_policy.min_replicas ->
            %% Scale down
            StepSize = min(Policy#scaling_policy.scale_down_step,
                          CurrentReplicas - Policy#scaling_policy.min_replicas),
            #{
                action => scale_down,
                target_replicas => CurrentReplicas - StepSize,
                trigger => cpu,
                policy => PolicyName
            };
        true ->
            %% Maintain current size
            #{
                action => maintain,
                target_replicas => CurrentReplicas,
                trigger => none,
                policy => PolicyName
            }
    end.

%% Execute scaling action
execute_scaling_action(Action, State) ->
    %% Create scaling activity record
    ActivityId = generate_activity_id(),
    CurrentReplicas = get_current_replica_count(),
    TargetReplicas = Action#action.target_replicas,

    Activity = #scaling_activity{
        id = ActivityId,
        action = Action#action.action,
        replicas_before = CurrentReplicas,
        replicas_after = TargetReplicas,
        trigger = Action#action.trigger,
        timestamp = os:system_time(millisecond),
        cost_impact = calculate_scaling_cost(Action#action.action,
                                           TargetReplicas - CurrentReplicas),
        status = pending
    },

    %% Execute scaling
    case Action#action.action of
        scale_up ->
            case scale_up(TargetReplicas) of
                ok ->
                    %% Update activity status
                    Activity1 = Activity#scaling_activity{status = completed},
                    log_scaling_activity(Activity1),
                    %% Update state
                    State#{
                        last_scale_event => os:system_time(millisecond),
                        scaling_activities => [Activity1 | State#scaling_activities]
                    };
                {error, Reason} ->
                    %% Update activity status
                    Activity1 = Activity#scaling_activity{status = failed,
                                                       reason => Reason},
                    log_scaling_activity(Activity1),
                    State
            end;
        scale_down ->
            case scale_down(TargetReplicas) of
                ok ->
                    %% Update activity status
                    Activity1 = Activity#scaling_activity{status = completed},
                    log_scaling_activity(Activity1),
                    %% Update state
                    State#{
                        last_scale_event => os:system_time(millisecond),
                        scaling_activities => [Activity1 | State#scaling_activities]
                    };
                {error, Reason} ->
                    %% Update activity status
                    Activity1 = Activity#scaling_activity{status = failed,
                                                       reason => Reason},
                    log_scaling_activity(Activity1),
                    State
            end;
        maintain ->
            %% No action needed
            State
    end.

%% Scale up replicas
scale_up(TargetReplicas) ->
    %% Use Kubernetes API to scale deployment
    case k8s_scale_deployment(TargetReplicas) of
        ok ->
            ok;
        {error, Reason} ->
            %% Try alternative scaling method
            case alternative_scale_up(TargetReplicas) of
                ok ->
                    ok;
                Error ->
                    {error, Reason}
            end
    end.

%% Scale down replicas
scale_down(TargetReplicas) ->
    %% Use Kubernetes API to scale deployment
    case k8s_scale_deployment(TargetReplicas) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Check spot instance health
check_spot_instance_health(State) ->
    %% Get active spot reservations
    ActiveReservations = lists:filter(fun(R) ->
        R#spot_reservation.state =:= active
    end, State#spot_reservations),

    %% Check each reservation
    lists:foreach(fun check_reservation_health/1, ActiveReservations).

%% Check individual reservation health
check_reservation_health(Reservation) ->
    %% Get spot instance status
    InstanceStatus = get_spot_instance_status(Reservation#spot_reservation.id),

    case InstanceStatus of
        running ->
            %% Check for interruption notice
            case check_interruption_notice(Reservation#spot_reservation.id) of
                {interruption, Time} ->
                    %% Schedule termination
                    schedule_spot_termination(Reservation#spot_reservation.id, Time);
                no_interruption ->
                    ok
            end;
        terminating ->
            %% Handle termination
            handle_spot_termination(Reservation#spot_reservation.id);
        terminated ->
            %% Handle termination
            handle_spot_termination(Reservation#spot_reservation.id);
        {error, Reason} ->
            log_error("Spot instance error: ~p", [Reason])
    end.

%% Handle spot interruption
handle_spot_interruption(State) ->
    %% Get active spot reservations
    ActiveReservations = lists:filter(fun(R) ->
        R#spot_reservation.state =:= active
    end, State#spot_reservations),

    %% Handle each interruption
    lists:foreach(fun handle_reservation_interruption/1, ActiveReservations).

%% Handle individual reservation interruption
handle_reservation_interruption(Reservation) ->
    %% Calculate time until termination
    TimeUntilTermination = calculate_time_until_termination(Reservation),

    if
        TimeUntilTermination > 0 ->
            %% Replace spot instance with regular instance
            case replace_spot_instance(Reservation) of
                {ok, NewReservation} ->
                    %% Update reservation state
                    update_reservation_state(NewReservation),
                    ok;
                {error, Reason} ->
                    log_error("Failed to replace spot instance: ~p", [Reason]),
                    {error, Reason}
            end;
        true ->
            %% Too late to replace
            log_error("Spot instance terminating too soon"),
            {error, termination_imminent}
    end.

%% Generate cost forecast
generate_cost_forecast(Hours, State) ->
    %% Get current metrics
    CurrentMetrics = collect_current_metrics(),

    %% Get current costs
    CurrentCost = calculate_current_cost(State),

    %% Predict future costs
    Scenarios = generate_cost_scenarios(Hours, CurrentMetrics, State),

    %% Create forecast record
    Forecast = #cost_forecast{
        timestamp = os:system_time(millisecond),
        period_hours = Hours,
        scenarios = Scenarios
    },

    Forecast.

%% Generate cost scenarios
generate_cost_scenarios(Hours, CurrentMetrics, State) ->
    %% Base scenario - current trend
    BaseScenario = #{
        scenario => base_trend,
        cost => calculate_trend_cost(Hours, CurrentMetrics, State),
        confidence => 0.7,
        factors => ["current_performance", "no_scaling_changes"]
    },

    %% Optimized scenario - with optimizations
    OptimizedScenario = #{
        scenario => optimized,
        cost => calculate_optimized_cost(Hours, CurrentMetrics, State),
        confidence => 0.5,
        factors => ["spot_instances", "auto_scaling", "right_sizing"]
    },

    %% Peak scenario - worst case
    PeakScenario = #{
        scenario => peak_load,
        cost => calculate_peak_cost(Hours, CurrentMetrics, State),
        confidence => 0.3,
        factors => ["max_scaling", "no_spot_instances", "high_utilization"]
    },

    [BaseScenario, OptimizedScenario, PeakScenario].

%% Calculate current cost
calculate_current_cost(State) ->
    %% Calculate compute cost
    ComputeCost = calculate_compute_cost(State),

    %% Calculate storage cost
    StorageCost = calculate_storage_cost(State),

    %% Calculate network cost
    NetworkCost = calculate_network_cost(State),

    %% Calculate license cost
    LicenseCost = calculate_license_cost(State),

    TotalCost = ComputeCost + StorageCost + NetworkCost + LicenseCost,

    TotalCost.

%% Calculate compute cost
calculate_compute_cost(State) ->
    %% Get current replica count
    ReplicaCount = get_current_replica_count(),

    Calculate cost per replica
    CostPerReplica = 0.2,  $0.2 per hour

    %% Spot instance ratio
    SpotRatio = calculate_spot_ratio(State),

    %% Adjust cost for spot instances (70% savings)
    AdjustedCost = ReplicaCount * CostPerReplica * (1 - SpotRatio * 0.7),

    AdjustedCost.

%% Calculate storage cost
calculate_storage_cost(State) ->
    %% Get storage usage
    StorageGB = get_storage_usage(),

    CostPerGB = 0.1,  $0.10 per GB per month

    MonthlyCost = StorageGB * CostPerGB,

    %% Pro-rate for current month
    DaysInMonth = calendar:last_day_of_the_month(os:date()),
    DayOfMonth = os:date(),

    DailyCost = MonthlyCost / DaysInMonth,

    DailyCost.

%% Calculate network cost
calculate_network_cost(State) ->
    %% Get network usage
    NetworkGB = get_network_usage(),

    CostPerGB = 0.09,  $0.09 per GB

    DailyCost = NetworkGB * CostPerGB,

    DailyCost.

%% Calculate license cost
calculate_license_cost(State) ->
    %% Get license usage
    LicenseCount = get_license_count(),

    CostPerLicense = 1000,  $1000 per month

    MonthlyCost = LicenseCount * CostPerLicense,

    %% Pro-rate for current month
    DaysInMonth = calendar:last_day_of_the_month(os:date()),
    DayOfMonth = os:date(),

    DailyCost = MonthlyCost / DaysInMonth,

    DailyCost.

%% Calculate spot ratio
calculate_spot_ratio(State) ->
    %% Get active spot reservations
    ActiveReservations = lists:filter(fun(R) ->
        R#spot_reservation.state =:= active
    end, State#spot_reservations),

    %% Calculate total instances
    TotalInstances = get_current_replica_count(),

    %% Calculate spot instances
    SpotInstances = lists:sum([R#spot_reservation.active_instances ||
                               R <- ActiveReservations]),

    if
        TotalInstances > 0 ->
            SpotInstances / TotalInstances;
        true ->
            0.0
    end.

%% Calculate scaling cost
calculate_scaling_action(Action, ReplicasDiff) ->
    %% Calculate cost based on action and replica difference
    case Action of
        scale_up ->
            Cost = ReplicasDiff * 0.2,  $0.2 per instance per hour
            Cost;
        scale_down ->
            Savings = abs(ReplicasDiff) * 0.2,  $0.2 per instance per hour
            -Savings;
        maintain ->
            0.0
    end.

%% Schedule spot termination
schedule_spot_termination(ReservationId, TerminationTime) ->
    %% Calculate time until termination
    TimeUntil = TerminationTime - os:system_time(millisecond),

    if
        TimeUntil > 0 ->
            %% Schedule termination check
            erlang:send_after(TimeUntil - 60000, ?MODULE,
                            {spot_interruption, ReservationId});
        true ->
            %% Immediate termination
            ?MODULE ! {spot_interruption, ReservationId}
    end.

%% Alternative scaling method
alternative_scale_up(TargetReplicas) ->
    %% Use auto-scaling group API
    case asg_set_desired_capacity(TargetReplicas) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Log scaling activity
log_scaling_activity(Activity) ->
    %% Log to database
    LogEntry = #{
        timestamp => Activity#scaling_activity.timestamp,
        action => Activity#scaling_activity.action,
        replicas_before => Activity#scaling_activity.replicas_before,
        replicas_after => Activity#scaling_activity.replicas_after,
        trigger => Activity#scaling_activity.trigger,
        cost_impact => Activity#scaling_activity.cost_impact,
        status => Activity#scaling_activity.status
    },

    erlmcp_database:insert(#{
        collection => scaling_activities,
        document => LogEntry
    }),

    %% Log to system
    log_info("Scaling activity: ~p from ~p to ~p replicas (~p cost impact)", [
        Activity#scaling_activity.action,
        Activity#scaling_activity.replicas_before,
        Activity#scaling_activity.replicas_after,
        Activity#scaling_activity.cost_impact
    ]).

%% Helper functions
generate_reservation_id() ->
    erlmcp_utils:generate_id().

generate_activity_id() ->
    erlmcp_utils:generate_id().

get_env(Key, Default) ->
    case application:get_env(erlmcp, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

log_info(Format, Args) ->
    erlmcp_logger:info(Format, Args).

log_error(Format, Args) ->
    erlmcp_logger:error(Format, Args).

%% Conversion functions
reservation_to_record(Reservation) ->
    #spot_reservation{
        id = maps:get(id, Reservation),
        instance_type = maps:get(instance_type, Reservation),
        availability_zone = maps:get(availability_zone, Reservation),
        max_price = maps:get(max_price, Reservation),
        duration_seconds = maps:get(duration_seconds, Reservation),
        start_time = maps:get(start_time, Reservation),
        end_time = maps:get(end_time, Reservation),
        instance_count = maps:get(count, Reservation),
        active_instances = maps:get(active_instances, Reservation, 0),
        state = maps:get(state, Reservation),
        disruption_budget = maps:get(disruption_budget, Reservation, 0.1)
    }.

price_to_record(PriceData) ->
    #spot_price_history{
        timestamp = maps:get(timestamp, PriceData),
        instance_type = maps:get(instance_type, PriceData),
        availability_zone = maps:get(availability_zone, PriceData),
        price = maps:get(price, PriceData),
        spot_capacity = maps:get(spot_capacity, PriceData, 0)
    }.

%% Store cost forecast
store_cost_forecast(Forecast) ->
    %% Store in database
    erlmcp_database:insert(#{
        collection => cost_forecasts,
        document => Forecast
    }).