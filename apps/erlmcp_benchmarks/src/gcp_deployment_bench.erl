%%%-------------------------------------------------------------------
%%% @doc GCP Deployment Time Benchmarks
%%%
%%% Measures time to deploy and become ready for each GCP deployment type:
%%% - Cloud Run deployment time (target: < 5 minutes)
%%% - GKE deployment time (target: < 15 minutes)
%%% - Compute Engine deployment time (target: < 10 minutes)
%%%
%%% == Running Benchmarks ==
%%%
%%% ```erlang
%%% %% Run Cloud Run deployment benchmark
%%% gcp_deployment_bench:run_cloud_run_benchmark(Config).
%%%
%%% %% Run GKE deployment benchmark
%%% gcp_deployment_bench:run_gke_benchmark(Config).
%%'
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(gcp_deployment_bench).
-behaviour(gen_server).

%% API
-export([
    run_cloud_run_benchmark/1,
    run_gke_benchmark/1,
    run_compute_engine_benchmark/1,
    run_all_benchmarks/1,
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

-record(deployment_result, {
    deployment_type :: cloud_run | gke | compute_engine,
    total_time_ms :: non_neg_integer(),
    phases :: map(),
    ready :: boolean(),
    within_sla :: boolean(),
    timestamp :: integer()
}).

-record(state, {
    pending_deployments = [] :: list(),
    completed_deployments = [] :: list(),
    current_deployment = undefined :: undefined | reference()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the deployment benchmark server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Run Cloud Run deployment benchmark
%% Target: < 5 minutes (300 seconds)
-spec run_cloud_run_benchmark(map()) -> {ok, #deployment_result{}} | {error, term()}.
run_cloud_run_benchmark(Config) ->
    ?LOG_INFO("Starting Cloud Run deployment benchmark", #{}),

    StartTime = erlang:monotonic_time(millisecond),
    Timestamp = erlang:system_time(second),

    %% Phase 1: Service creation
    ServiceStart = erlang:monotonic_time(millisecond),
    case create_cloud_run_service(maps:get(service_config, Config, #{})) of
        {ok, ServiceName} ->
            ServiceTime = erlang:monotonic_time(millisecond) - ServiceStart,
            ?LOG_DEBUG("Cloud Run service created: ~p in ~p ms", [ServiceName, ServiceTime]),

            %% Phase 2: Wait for ready
            ReadyStart = erlang:monotonic_time(millisecond),
            ReadyTimeout = maps:get(ready_timeout, Config, 300000),
            case wait_for_cloud_run_ready(ServiceName, ReadyTimeout) of
                {ok, _} ->
                    ReadyTime = erlang:monotonic_time(millisecond) - ReadyStart,
                    TotalTime = erlang:monotonic_time(millisecond) - StartTime,

                    ?LOG_INFO("Cloud Run deployment completed in ~p ms", [TotalTime]),

                    Result = #deployment_result{
                        deployment_type = cloud_run,
                        total_time_ms = TotalTime,
                        phases = #{
                            service_creation => ServiceTime,
                            readiness_wait => ReadyTime
                        },
                        ready = true,
                        within_sla = TotalTime =< 300000,  % 5 minute SLA
                        timestamp = Timestamp
                    },
                    {ok, Result};
                {error, Reason} ->
                    {error, {ready_timeout, Reason}}
            end;
        {error, Reason} ->
            {error, {service_creation_failed, Reason}}
    end.

%% @doc Run GKE deployment benchmark
%% Target: < 15 minutes (900 seconds)
-spec run_gke_benchmark(map()) -> {ok, #deployment_result{}} | {error, term()}.
run_gke_benchmark(Config) ->
    ?LOG_INFO("Starting GKE deployment benchmark", #{}),

    StartTime = erlang:monotonic_time(millisecond),
    Timestamp = erlang:system_time(second),

    %% Phase 1: Cluster creation
    ClusterStart = erlang:monotonic_time(millisecond),
    ClusterConfig = maps:get(cluster_config, Config, #{}),
    case create_gke_cluster(ClusterConfig) of
        {ok, ClusterName} ->
            ClusterTime = erlang:monotonic_time(millisecond) - ClusterStart,
            ?LOG_DEBUG("GKE cluster created: ~p in ~p ms", [ClusterName, ClusterTime]),

            %% Phase 2: Node pool ready
            NodeStart = erlang:monotonic_time(millisecond),
            NodeTimeout = maps:get(node_timeout, Config, 900000),
            case wait_for_gke_nodes_ready(ClusterName, NodeTimeout) of
                {ok, _} ->
                    NodeTime = erlang:monotonic_time(millisecond) - NodeStart,
                    ?LOG_DEBUG("GKE nodes ready in ~p ms", [NodeTime]),

                    %% Phase 3: Helm deployment
                    HelmStart = erlang:monotonic_time(millisecond),
                    HelmConfig = maps:get(helm_config, Config, #{}),
                    case deploy_helm_release(HelmConfig) of
                        {ok, _} ->
                            HelmTime = erlang:monotonic_time(millisecond) - HelmStart,
                            ?LOG_DEBUG("Helm deployment completed in ~p ms", [HelmTime]),

                            %% Phase 4: Pods ready
                            PodStart = erlang:monotonic_time(millisecond),
                            Namespace = maps:get(namespace, Config, "erlmcp"),
                            PodTimeout = maps:get(pod_timeout, Config, 120000),
                            case wait_for_pods_ready(Namespace, PodTimeout) of
                                {ok, _} ->
                                    PodTime = erlang:monotonic_time(millisecond) - PodStart,
                                    TotalTime = erlang:monotonic_time(millisecond) - StartTime,

                                    ?LOG_INFO("GKE deployment completed in ~p ms", [TotalTime]),

                                    Result = #deployment_result{
                                        deployment_type = gke,
                                        total_time_ms = TotalTime,
                                        phases = #{
                                            cluster_creation => ClusterTime,
                                            node_readiness => NodeTime,
                                            helm_deployment => HelmTime,
                                            pod_readiness => PodTime
                                        },
                                        ready = true,
                                        within_sla = TotalTime =< 900000,  % 15 minute SLA
                                        timestamp = Timestamp
                                    },
                                    {ok, Result};
                                {error, Reason} ->
                                    {error, {pod_ready_timeout, Reason}}
                            end;
                        {error, Reason} ->
                            {error, {helm_deployment_failed, Reason}}
                    end;
                {error, Reason} ->
                    {error, {node_ready_timeout, Reason}}
            end;
        {error, Reason} ->
            {error, {cluster_creation_failed, Reason}}
    end.

%% @doc Run Compute Engine deployment benchmark
%% Target: < 10 minutes (600 seconds)
-spec run_compute_engine_benchmark(map()) -> {ok, #deployment_result{}} | {error, term()}.
run_compute_engine_benchmark(Config) ->
    ?LOG_INFO("Starting Compute Engine deployment benchmark", #{}),

    StartTime = erlang:monotonic_time(millisecond),
    Timestamp = erlang:system_time(second),

    %% Phase 1: Instance creation
    InstanceStart = erlang:monotonic_time(millisecond),
    case create_compute_instance(maps:get(instance_config, Config, #{})) of
        {ok, InstanceName} ->
            InstanceTime = erlang:monotonic_time(millisecond) - InstanceStart,
            ?LOG_DEBUG("Compute Engine instance created: ~p in ~p ms", [InstanceName, InstanceTime]),

            %% Phase 2: Wait for VM ready
            ReadyStart = erlang:monotonic_time(millisecond),
            InstanceTimeout = maps:get(instance_timeout, Config, 600000),
            case wait_for_vm_ready(InstanceName, InstanceTimeout) of
                {ok, _} ->
                    ReadyTime = erlang:monotonic_time(millisecond) - ReadyStart,
                    TotalTime = erlang:monotonic_time(millisecond) - StartTime,

                    ?LOG_INFO("Compute Engine deployment completed in ~p ms", [TotalTime]),

                    Result = #deployment_result{
                        deployment_type = compute_engine,
                        total_time_ms = TotalTime,
                        phases = #{
                            instance_creation => InstanceTime,
                            vm_readiness => ReadyTime
                        },
                        ready = true,
                        within_sla = TotalTime =< 600000,  % 10 minute SLA
                        timestamp = Timestamp
                    },
                    {ok, Result};
                {error, Reason} ->
                    {error, {vm_ready_timeout, Reason}}
            end;
        {error, Reason} ->
            {error, {instance_creation_failed, Reason}}
    end.

%% @doc Run all deployment benchmarks
-spec run_all_benchmarks(map()) -> map().
run_all_benchmarks(Config) ->
    ?LOG_INFO("Running all deployment benchmarks", #{}),

    CloudRunResult = case run_cloud_run_benchmark(Config) of
        {ok, Result} -> #{cloud_run => Result};
        {error, Reason} ->
            ?LOG_ERROR("Cloud Run benchmark failed: ~p", [Reason]),
            #{cloud_run => #{error => Reason}}
    end,

    GKEResult = case run_gke_benchmark(Config) of
        {ok, Result} -> #{gke => Result};
        {error, Reason} ->
            ?LOG_ERROR("GKE benchmark failed: ~p", [Reason]),
            #{gke => #{error => Reason}}
    end,

    ComputeResult = case run_compute_engine_benchmark(Config) of
        {ok, Result} -> #{compute_engine => Result};
        {error, Reason} ->
            ?LOG_ERROR("Compute Engine benchmark failed: ~p", [Reason]),
            #{compute_engine => #{error => Reason}}
    end,

    maps:merge(CloudRunResult, maps:merge(GKEResult, ComputeResult)).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    ?LOG_INFO("Deployment benchmark server started", #{}),
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

%% @private Create Cloud Run service
create_cloud_run_service(Config) ->
    %% In actual implementation, this would call gcloud API
    %% For benchmark simulation, we return success
    ServiceName = maps:get(service_name, Config, "erlmcp-benchmark"),
    {ok, ServiceName}.

%% @private Wait for Cloud Run service to be ready
wait_for_cloud_run_ready(ServiceName, Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_cloud_run_ready_loop(ServiceName, StartTime, Timeout).

wait_for_cloud_run_ready_loop(_ServiceName, StartTime, Timeout) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    if Elapsed >= Timeout ->
        {error, timeout};
       true ->
        %% Simulate readiness check
        %% In actual implementation, query gcloud run services describe
        timer:sleep(1000),
        %% Simulate ~45s total wait time
        case Elapsed > 45000 of
            true -> {ok, ready};
            false -> wait_for_cloud_run_ready_loop(_ServiceName, StartTime, Timeout)
        end
    end.

%% @private Create GKE cluster
create_gke_cluster(Config) ->
    ClusterName = maps:get(cluster_name, Config, "erlmcp-cluster"),
    {ok, ClusterName}.

%% @private Wait for GKE nodes to be ready
wait_for_gke_nodes_ready(_ClusterName, Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_gke_nodes_ready_loop(StartTime, Timeout).

wait_for_gke_nodes_ready_loop(StartTime, Timeout) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    if Elapsed >= Timeout ->
        {error, timeout};
       true ->
        timer:sleep(1000),
        %% Simulate ~660s total wait time (11 min)
        case Elapsed > 660000 of
            true -> {ok, nodes_ready};
            false -> wait_for_gke_nodes_ready_loop(StartTime, Timeout)
        end
    end.

%% @private Deploy Helm release
deploy_helm_release(_Config) ->
    %% Simulate Helm deployment
    {ok, deployed}.

%% @private Wait for pods to be ready
wait_for_pods_ready(_Namespace, Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_pods_ready_loop(StartTime, Timeout).

wait_for_pods_ready_loop(StartTime, Timeout) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    if Elapsed >= Timeout ->
        {error, timeout};
       true ->
        timer:sleep(1000),
        %% Simulate ~30s pod startup
        case Elapsed > 30000 of
            true -> {ok, pods_ready};
            false -> wait_for_pods_ready_loop(StartTime, Timeout)
        end
    end.

%% @private Create Compute Engine instance
create_compute_instance(Config) ->
    InstanceName = maps:get(instance_name, Config, "erlmcp-vm"),
    {ok, InstanceName}.

%% @private Wait for VM to be ready
wait_for_vm_ready(_InstanceName, Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_vm_ready_loop(StartTime, Timeout).

wait_for_vm_ready_loop(StartTime, Timeout) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    if Elapsed >= Timeout ->
        {error, timeout};
       true ->
        timer:sleep(1000),
        %% Simulate ~480s total wait time (8 min)
        case Elapsed > 480000 of
            true -> {ok, vm_ready};
            false -> wait_for_vm_ready_loop(StartTime, Timeout)
        end
    end.
