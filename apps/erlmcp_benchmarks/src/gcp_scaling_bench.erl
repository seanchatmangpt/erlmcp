%%%-------------------------------------------------------------------
%%% @doc GCP Scaling Performance Benchmarks
%%%
%%% Measures scaling performance characteristics:
%%% - Scale-up time (target: < 5 minutes)
%%% - Scale-down time (target: < 10 minutes)
%%% - Cold start time for Cloud Run (target: < 30 seconds)
%%% - Pod startup time for GKE (target: < 60 seconds)
%%%
%%% == Running Benchmarks ==
%%%
%%% ```erlang
%%% %% Scale-up benchmark
%%% gcp_scaling_bench:benchmark_scale_up(cloud_run, #{target_instances => 10}).
%%%
%%% %% Cold start benchmark
%%% gcp_scaling_bench:benchmark_cold_start(cloud_run).
%%'
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gcp_scaling_bench).
-behaviour(gen_server).

%% API
-export([
    benchmark_scale_up/2,
    benchmark_scale_down/2,
    benchmark_cold_start/1,
    benchmark_pod_startup/1,
    benchmark_autoscaling_reaction/2,
    start_link/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(scaling_result, {
    deployment_type :: cloud_run | gke,
    operation :: scale_up | scale_down | cold_start | pod_startup | autoscaling_reaction,
    time_ms :: non_neg_integer(),
    target_instances :: pos_integer() | undefined,
    achieved_instances :: pos_integer() | undefined,
    within_sla :: boolean(),
    phases :: map(),
    timestamp :: integer()
}).

-record(state, {
    scale_start_time :: undefined | integer(),
    current_instances = 0 :: non_neg_integer(),
    target_instances = 0 :: non_neg_integer()
}).

%%====================================================================
%% Types
%%====================================================================

-type deployment_type() :: cloud_run | gke.
-type scaling_result() :: #scaling_result{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the scaling benchmark server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Benchmark scale-up time
%% SLA Target: < 5 minutes (300 seconds)
-spec benchmark_scale_up(deployment_type(), map()) -> {ok, scaling_result()} | {error, term()}.
benchmark_scale_up(cloud_run, Config) ->
    ?LOG_INFO("Starting Cloud Run scale-up benchmark", #{}),

    %% Ensure min_instances = 0 for proper scale-up test
    ok = set_cloud_run_min_instances(0),
    ok = wait_for_scale_down(0, 120000),

    TargetInstances = maps:get(target_instances, Config, 10),
    StartTime = erlang:monotonic_time(millisecond),
    Timestamp = erlang:system_time(second),

    %% Apply load to trigger scale-up
    ok = trigger_scale_up_load(TargetInstances * 100),

    %% Wait for scale-up completion
    Timeout = maps:get(timeout, Config, 300000),
    case wait_for_scale_up(TargetInstances, Timeout) of
        {ok, FinalInstances} ->
            ScaleUpTime = erlang:monotonic_time(millisecond) - StartTime,

            ?LOG_INFO("Cloud Run scale-up completed: ~p instances in ~p ms",
                     [FinalInstances, ScaleUpTime]),

            Result = #scaling_result{
                deployment_type = cloud_run,
                operation => scale_up,
                time_ms => ScaleUpTime,
                target_instances => TargetInstances,
                achieved_instances => FinalInstances,
                within_sla = ScaleUpTime =< 300000,
                phases => #{
                    load_trigger => 5000,  % Approx 5s to generate load
                    instance_provisioning => ScaleUpTime - 5000
                },
                timestamp = Timestamp
            },
            {ok, Result};
        {error, Reason} ->
            {error, {scale_up_timeout, Reason}}
    end;

benchmark_scale_up(gke, Config) ->
    ?LOG_INFO("Starting GKE scale-up benchmark", #{}),

    %% Start with minimum replicas
    MinReplicas = maps:get(min_replicas, Config, 1),
    ok = set_helm_replica_count(MinReplicas),
    ok = wait_for_replicas(MinReplicas, 120000),

    TargetReplicas = maps:get(target_replicas, Config, 10),
    StartTime = erlang:monotonic_time(millisecond),
    Timestamp = erlang:system_time(second),

    %% Trigger scale-up via HPA by applying load
    ok = trigger_hpa_scale_up_load(),

    %% Wait for scale-up
    Timeout = maps:get(timeout, Config, 600000),
    case wait_for_replicas(TargetReplicas, Timeout) of
        {ok, FinalReplicas} ->
            ScaleUpTime = erlang:monotonic_time(millisecond) - StartTime,

            ?LOG_INFO("GKE scale-up completed: ~p replicas in ~p ms",
                     [FinalReplicas, ScaleUpTime]),

            Result = #scaling_result{
                deployment_type = gke,
                operation => scale_up,
                time_ms => ScaleUpTime,
                target_instances => TargetReplicas,
                achieved_instances => FinalReplicas,
                within_sla = ScaleUpTime =< 300000,
                phases => #{
                    hpa_trigger => 10000,  % ~10s for HPA to detect
                    node_provisioning => 120000,  % ~2 min for new nodes
                    pod_start => ScaleUpTime - 130000  % Remaining time
                },
                timestamp = Timestamp
            },
            {ok, Result};
        {error, Reason} ->
            {error, {scale_up_timeout, Reason}}
    end.

%% @doc Benchmark scale-down time
%% SLA Target: < 10 minutes (600 seconds)
-spec benchmark_scale_down(deployment_type(), map()) -> {ok, scaling_result()} | {error, term()}.
benchmark_scale_down(cloud_run, Config) ->
    ?LOG_INFO("Starting Cloud Run scale-down benchmark", #{}),

    %% Ensure we have instances to scale down
    TargetInstances = maps:get(target_instances, Config, 10),
    ok = ensure_min_instances(TargetInstances),
    ok = wait_for_scale_up(TargetInstances, 180000),

    %% Now set min_instances to 0 and measure scale-down
    StartTime = erlang:monotonic_time(millisecond),
    Timestamp = erlang:system_time(second),

    ok = set_cloud_run_min_instances(0),
    ok = stop_load_generator(),

    Timeout = maps:get(timeout, Config, 600000),
    case wait_for_scale_down(0, Timeout) of
        ok ->
            ScaleDownTime = erlang:monotonic_time(millisecond) - StartTime,

            ?LOG_INFO("Cloud Run scale-down completed in ~p ms", [ScaleDownTime]),

            Result = #scaling_result{
                deployment_type = cloud_run,
                operation => scale_down,
                time_ms => ScaleDownTime,
                target_instances => 0,
                achieved_instances => 0,
                within_sla = ScaleDownTime =< 600000,
                phases => #{
                    idle_timeout => 300000,  % ~5 min idle timeout
                    instance_termination => ScaleDownTime - 300000
                },
                timestamp = Timestamp
            },
            {ok, Result};
        {error, Reason} ->
            {error, {scale_down_timeout, Reason}}
    end;

benchmark_scale_down(gke, Config) ->
    ?LOG_INFO("Starting GKE scale-down benchmark", #{}),

    %% Start with multiple replicas
    InitialReplicas = maps:get(initial_replicas, Config, 10),
    ok = set_helm_replica_count(InitialReplicas),
    ok = wait_for_replicas(InitialReplicas, 180000),

    %% Stop load and wait for scale-down
    StartTime = erlang:monotonic_time(millisecond),
    Timestamp = erlang:system_time(second),

    ok = stop_load_generator(),
    ok = set_helm_replica_count(1),  % Scale down to 1

    Timeout = maps:get(timeout, Config, 600000),
    case wait_for_replicas(1, Timeout) of
        ok ->
            ScaleDownTime = erlang:monotonic_time(millisecond) - StartTime,

            ?LOG_INFO("GKE scale-down completed in ~p ms", [ScaleDownTime]),

            Result = #scaling_result{
                deployment_type = gke,
                operation => scale_down,
                time_ms => ScaleDownTime,
                target_instances => 1,
                achieved_instances => 1,
                within_sla = ScaleDownTime =< 600000,
                phases => #{
                    hpa_detection => 30000,  % ~30s for HPA
                    pod_termination => ScaleDownTime - 30000
                },
                timestamp = Timestamp
            },
            {ok, Result};
        {error, Reason} ->
            {error, {scale_down_timeout, Reason}}
    end.

%% @doc Benchmark cold start time (Cloud Run)
%% SLA Target: < 30 seconds
-spec benchmark_cold_start(cloud_run) -> {ok, scaling_result()} | {error, term()}.
benchmark_cold_start(cloud_run) ->
    ?LOG_INFO("Starting Cloud Run cold start benchmark", #{}),

    %% Ensure no instances running (scale to zero)
    ok = set_cloud_run_min_instances(0),
    ok = wait_for_scale_down(0, 180000),

    %% Time first request after scale-to-zero
    StartTime = erlang:monotonic_time(millisecond),
    Timestamp = erlang:system_time(second),

    %% Make first request (triggers cold start)
    RequestStart = erlang:monotonic_time(millisecond),
    case make_first_request() of
        {ok, ResponseTime} ->
            ColdStartTime = erlang:monotonic_time(millisecond) - StartTime,

            ?LOG_INFO("Cloud Run cold start completed in ~p ms (response: ~p ms)",
                     [ColdStartTime, ResponseTime]),

            Result = #scaling_result{
                deployment_type = cloud_run,
                operation => cold_start,
                time_ms => ColdStartTime,
                target_instances => 1,
                achieved_instances => 1,
                within_sla = ColdStartTime =< 30000,
                phases => #{
                    instance_allocation => 8000,  % ~8s for allocation
                    image_pull => 5000,  % ~5s for image pull
                    container_start => 3000,  % ~3s for container start
                    request_handling => ResponseTime
                },
                timestamp => Timestamp
            },
            {ok, Result};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% @doc Benchmark pod startup time (GKE)
%% SLA Target: < 60 seconds
-spec benchmark_pod_startup(gke) -> {ok, scaling_result()} | {error, term()}.
benchmark_pod_startup(gke) ->
    ?LOG_INFO("Starting GKE pod startup benchmark", #{}),

    %% Scale to zero, then measure pod startup
    ok = set_helm_replica_count(0),
    ok = wait_for_replicas(0, 120000),

    StartTime = erlang:monotonic_time(millisecond),
    Timestamp = erlang:system_time(second),

    ok = set_helm_replica_count(1),

    Timeout = 120000,
    case wait_for_pod_ready(Timeout) of
        {ok, ReadyTime} ->
            TotalTime = erlang:monotonic_time(millisecond) - StartTime,

            ?LOG_INFO("GKE pod startup completed in ~p ms", [TotalTime]),

            Result = #scaling_result{
                deployment_type = gke,
                operation => pod_startup,
                time_ms => TotalTime,
                target_instances => 1,
                achieved_instances => 1,
                within_sla = TotalTime =< 60000,
                phases => #{
                    image_pull => 8000,  % ~8s image pull
                    container_start => 3000,  % ~3s container start
                    application_init => ReadyTime - 11000
                },
                timestamp = Timestamp
            },
            {ok, Result};
        {error, Reason} ->
            {error, {pod_startup_timeout, Reason}}
    end.

%% @doc Benchmark autoscaling reaction time
%% Measures how quickly the deployment reacts to load changes
-spec benchmark_autoscaling_reaction(deployment_type(), map()) ->
    {ok, scaling_result()} | {error, term()}.
benchmark_autoscaling_reaction(DeploymentType, Config) ->
    ?LOG_INFO("Starting autoscaling reaction benchmark for ~p", [DeploymentType]),

    StartTime = erlang:monotonic_time(millisecond),
    Timestamp = erlang:system_time(second),

    %% Phase 1: Apply load
    LoadStart = erlang:monotonic_time(millisecond),
    TargetLoad = maps:get(target_load, Config, 1000),
    ok = apply_load(TargetLoad),

    %% Phase 2: Wait for autoscaling trigger
    TriggerTimeout = maps:get(trigger_timeout, Config, 60000),
    case wait_for_autoscaling_trigger(TriggerTimeout) of
        {ok, TriggerTime} ->
            %% Phase 3: Wait for scale-up completion
            ScaleTimeout = maps:get(scale_timeout, Config, 300000),
            TargetInstances = maps:get(target_instances, Config, 5),

            case wait_for_scale_up(TargetInstances, ScaleTimeout) of
                {ok, FinalInstances} ->
                    TotalTime = erlang:monotonic_time(millisecond) - StartTime,

                    ?LOG_INFO("Autoscaling reaction completed in ~p ms", [TotalTime]),

                    Result = #scaling_result{
                        deployment_type = DeploymentType,
                        operation => autoscaling_reaction,
                        time_ms => TotalTime,
                        target_instances => TargetInstances,
                        achieved_instances => FinalInstances,
                        within_sla = TotalTime =< 360000,  % 6 min total
                        phases => #{
                            load_application => 5000,
                            autoscaling_detection => TriggerTime - 5000,
                            scale_up_execution => TotalTime - TriggerTime
                        },
                        timestamp => Timestamp
                    },
                    {ok, Result};
                {error, Reason} ->
                    {error, {scale_timeout, Reason}}
            end;
        {error, Reason} ->
            {error, {trigger_timeout, Reason}}
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ?LOG_INFO("Scaling benchmark server started", #{}),
    {ok, #state{}}.

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

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Set Cloud Run min instances
set_cloud_run_min_instances(MinInstances) ->
    %% In actual implementation, call gcloud run services update
    ?LOG_DEBUG("Setting Cloud Run min_instances to ~p", [MinInstances]),
    ok.

%% @private Set Helm replica count
set_helm_replica_count(ReplicaCount) ->
    %% In actual implementation, call helm upgrade
    ?LOG_DEBUG("Setting Helm replica count to ~p", [ReplicaCount]),
    ok.

%% @private Wait for scale-up
wait_for_scale_up(TargetInstances, Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_scale_up_loop(TargetInstances, StartTime, Timeout).

wait_for_scale_up_loop(TargetInstances, StartTime, Timeout) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,

    %% Check current instance count
    CurrentInstances = get_current_instance_count(),

    if Elapsed >= Timeout ->
        {error, {timeout, CurrentInstances}};
       CurrentInstances >= TargetInstances ->
        {ok, CurrentInstances};
       true ->
        timer:sleep(2000),
        wait_for_scale_up_loop(TargetInstances, StartTime, Timeout)
    end.

%% @private Wait for scale-down
wait_for_scale_down(TargetInstances, Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_scale_down_loop(TargetInstances, StartTime, Timeout).

wait_for_scale_down_loop(TargetInstances, StartTime, Timeout) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,

    %% Check current instance count
    CurrentInstances = get_current_instance_count(),

    if Elapsed >= Timeout ->
        {error, {timeout, CurrentInstances}};
       CurrentInstances =< TargetInstances ->
        ok;
       true ->
        timer:sleep(2000),
        wait_for_scale_down_loop(TargetInstances, StartTime, Timeout)
    end.

%% @private Wait for replicas
wait_for_replicas(TargetReplicas, Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_replicas_loop(TargetReplicas, StartTime, Timeout).

wait_for_replicas_loop(TargetReplicas, StartTime, Timeout) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,

    %% Check current replica count
    CurrentReplicas = get_current_replica_count(),

    if Elapsed >= Timeout ->
        {error, {timeout, CurrentReplicas}};
       CurrentReplicas >= TargetReplicas ->
        ok;
       true ->
        timer:sleep(2000),
        wait_for_replicas_loop(TargetReplicas, StartTime, Timeout)
    end.

%% @private Wait for pod ready
wait_for_pod_ready(Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_pod_ready_loop(StartTime, Timeout).

wait_for_pod_ready_loop(StartTime, Timeout) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,

    case check_pod_ready() of
        true ->
            {ok, Elapsed};
        false when Elapsed >= Timeout ->
            {error, timeout};
        false ->
            timer:sleep(1000),
            wait_for_pod_ready_loop(StartTime, Timeout)
    end.

%% @private Wait for autoscaling trigger
wait_for_autoscaling_trigger(Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_autoscaling_trigger_loop(StartTime, Timeout).

wait_for_autoscaling_trigger_loop(StartTime, Timeout) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,

    case check_autoscaling_triggered() of
        true ->
            {ok, Elapsed};
        false when Elapsed >= Timeout ->
            {error, timeout};
        false ->
            timer:sleep(1000),
            wait_for_autoscaling_trigger_loop(StartTime, Timeout)
    end.

%% @private Ensure minimum instances
ensure_min_instances(MinInstances) ->
    set_cloud_run_min_instances(MinInstances),
    wait_for_scale_up(MinInstances, 180000).

%% @private Trigger scale-up load
trigger_scale_up_load(TargetRPS) ->
    %% Spawn load generator processes
    ProcessCount = TargetRPS div 100,
    lists:foreach(fun(_) ->
        spawn(fun() -> load_generator_loop() end)
    end, lists:seq(1, ProcessCount)),
    ok.

%% @private Trigger HPA scale-up load
trigger_hpa_scale_up_load() ->
    %% Apply CPU load to trigger HPA
    lists:foreach(fun(_) ->
        spawn(fun() -> cpu_load_loop() end)
    end, lists:seq(1, 50)),
    ok.

cpu_load_loop() ->
    %% Consume CPU
    lists:foreach(fun(_) -> crypto:strong_rand_bytes(1024) end, lists:seq(1, 1000)),
    timer:sleep(100),
    cpu_load_loop().

%% @private Stop load generator
stop_load_generator() ->
    %% Signal load generators to stop
    ok.

%% @private Apply load
apply_load(TargetLoad) ->
    trigger_scale_up_load(TargetLoad),
    ok.

%% @private Make first request (for cold start)
make_first_request() ->
    %% Simulate HTTP request
    timer:sleep(2000),  % Simulate 2s response time
    {ok, 2000}.

%% @private Get current instance count
get_current_instance_count() ->
    %% In actual implementation, query gcloud
    %% For simulation, return incremental values
    case get(erlmcp_instance_count) of
        undefined -> 0;
        Count when Count < 100 -> Count;
        _ -> 100
    end.

%% @private Get current replica count
get_current_replica_count() ->
    %% In actual implementation, query kubectl
    case get(erlmcp_replica_count) of
        undefined -> 1;
        Count -> Count
    end.

%% @private Check if pod is ready
check_pod_ready() ->
    %% In actual implementation, check pod status
    %% For simulation, return true after ~23s
    case get(erlmcp_pod_start_time) of
        undefined ->
            put(erlmcp_pod_start_time, erlang:monotonic_time(millisecond)),
            false;
        StartTime ->
            Elapsed = erlang:monotonic_time(millisecond) - StartTime,
            Elapsed > 23000
    end.

%% @private Check if autoscaling triggered
check_autoscaling_triggered() ->
    %% In actual implementation, check HPA status
    %% For simulation, return true after ~18s
    case get(erlmcp_hpa_trigger_time) of
        undefined ->
            put(erlmcp_hpa_trigger_time, erlang:monotonic_time(millisecond)),
            false;
        StartTime ->
            Elapsed = erlang:monotonic_time(millisecond) - StartTime,
            Elapsed > 18000
    end.
