-module(erlmcp_api_gateway_lifecycle).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2, code_change/3]).

-export([
    create_api/1, deploy_api/2, update_api/2, retire_api/2, archive_api/2,
    promote_api/2, rollback_api/2, clone_api/2, version_api/2,
    get_deployment_status/1, get_api_versions/1
]).

-export([schedule_deployment/3, automate_deployment/1, monitor_deployment/1]).

-define(DEPLOYMENT_TIMEOUT, 300000).
-define(HEALTH_CHECK_INTERVAL, 5000).

-record(deployment, {
    id :: binary(),
    api_id :: binary(),
    version :: binary(),
    environment :: binary(),
    status :: pending | deploying | deployed | failed | rollback,
    start_time :: integer(),
    end_time :: integer() | undefined,
    health_checks :: list(),
    rollback_data :: map()
}).

-record(api_version, {
    id :: binary(),
    api_id :: binary(),
    version :: binary(),
    config :: map(),
    created_at :: integer(),
    deployed_at :: integer() | undefined,
    status :: draft | deployed | archived,
    diff :: map()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:send_after(60000, self(), scan_deployments),
    {ok, #{deployments => #{}, versions => #{}, scheduled => []}}.

create_api(ApiSpec) ->
    gen_server:call(?MODULE, {create_api, ApiSpec}).

deploy_api(ApiId, Environment) ->
    gen_server:call(?MODULE, {deploy_api, ApiId, Environment}).

update_api(ApiId, Updates) ->
    gen_server:call(?MODULE, {update_api, ApiId, Updates}).

retire_api(ApiId, Reason) ->
    gen_server:call(?MODULE, {retire_api, ApiId, Reason}).

archive_api(ApiId, Reason) ->
    gen_server:call(?MODULE, {archive_api, ApiId, Reason}).

promote_api(ApiId, TargetEnv) ->
    gen_server:call(?MODULE, {promote_api, ApiId, TargetEnv}).

rollback_api(ApiId, Version) ->
    gen_server:call(?MODULE, {rollback_api, ApiId, Version}).

clone_api(ApiId, NewName) ->
    gen_server:call(?MODULE, {clone_api, ApiId, NewName}).

version_api(ApiId, VersionName) ->
    gen_server:call(?MODULE, {version_api, ApiId, VersionName}).

get_deployment_status(DeploymentId) ->
    gen_server:call(?MODULE, {get_deployment_status, DeploymentId}).

get_api_versions(ApiId) ->
    gen_server:call(?MODULE, {get_api_versions, ApiId}).

schedule_deployment(ApiId, Environment, Time) ->
    gen_server:call(?MODULE, {schedule_deployment, ApiId, Environment, Time}).

automate_deployment(DeploymentSpec) ->
    gen_server:call(?MODULE, {automate_deployment, DeploymentSpec}).

monitor_deployment(DeploymentId) ->
    gen_server:call(?MODULE, {monitor_deployment, DeploymentId}).

handle_call({create_api, ApiSpec}, _From, State) ->
    ApiId = maps:get(id, ApiSpec),
    VersionId = generate_version_id(ApiId),

    Version = #api_version{
        id = VersionId,
        api_id = ApiId,
        version = <<"v1.0.0">>,
        config = ApiSpec,
        created_at = erlang:system_time(millisecond),
        status = draft
    },

    Versions = State#{versions},
    NewVersions = maps:put(VersionId, Version, Versions),

    {reply, {ok, Version}, State#{versions => NewVersions}};

handle_call({deploy_api, ApiId, Environment}, _From, State) ->
    case get_latest_version(ApiId, State) of
        {ok, Version} ->
            DeploymentId = generate_deployment_id(),
            Deployment = #deployment{
                id = DeploymentId,
                api_id = ApiId,
                version = Version#api_version.version,
                environment = Environment,
                status = deploying,
                start_time = erlang:system_time(millisecond)
            },

            Deployments = State#{deployments},
            NewDeployments = maps:put(DeploymentId, Deployment, Deployments),

            erlang:send_after(?DEPLOYMENT_TIMEOUT, self(), {deployment_timeout, DeploymentId}),
            erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), {health_check, DeploymentId}),

            {reply, {ok, DeploymentId}, State#{deployments => NewDeployments}};
        {error, not_found} ->
            {reply, {error, api_not_found}, State}
    end;

handle_call({update_api, ApiId, Updates}, _From, State) ->
    case get_latest_version(ApiId, State) of
        {ok, Version} ->
            NewConfig = maps:merge(Version#api_version.config, Updates),
            Diff = calculate_diff(Version#api_version.config, NewConfig),

            NewVersionId = generate_version_id(ApiId),
            NewVersion = Version#api_version{
                id = NewVersionId,
                config = NewConfig,
                diff = Diff,
                status = draft
            },

            Versions = State#{versions},
            NewVersions = maps:put(NewVersionId, NewVersion, Versions),

            {reply, {ok, NewVersion}, State#{versions => NewVersions}};
        {error, not_found} ->
            {reply, {error, api_not_found}, State}
    end;

handle_call({retire_api, ApiId, Reason}, _From, State) ->
    case get_latest_version(ApiId, State) of
        {ok, Version} ->
            UpdatedVersion = Version#api_version{
                status = archived,
                deployed_at = erlang:system_time(millisecond)
            },

            Versions = State#{versions},
            NewVersions = maps:put(UpdatedVersion#api_version.id, UpdatedVersion, Versions),

            {reply, {ok, retired}, State#{versions => NewVersions}};
        {error, not_found} ->
            {reply, {error, api_not_found}, State}
    end;

handle_call({archive_api, ApiId, Reason}, _From, State) ->
    {reply, {ok, archived}, State};

handle_call({promote_api, ApiId, TargetEnv}, _From, State) ->
    case get_current_deployment(ApiId, State) of
        {ok, Deployment} ->
            PromotionId = generate_deployment_id(),
            NewDeployment = Deployment#deployment{
                id = PromotionId,
                environment = TargetEnv,
                status = deploying,
                start_time = erlang:system_time(millisecond)
            },

            Deployments = State#{deployments},
            NewDeployments = maps:put(PromotionId, NewDeployment, Deployments),

            {reply, {ok, PromotionId}, State#{deployments => NewDeployments}};
        {error, not_found} ->
            {reply, {error, no_deployment}, State}
    end;

handle_call({rollback_api, ApiId, Version}, _From, State) ->
    DeploymentId = generate_deployment_id(),
    Deployment = #deployment{
        id = DeploymentId,
        api_id = ApiId,
        version = Version,
        environment = <<"rollback">>,
        status = rollback,
        start_time = erlang:system_time(millisecond)
    },

    Deployments = State#{deployments},
    NewDeployments = maps:put(DeploymentId, Deployment, Deployments),

    {reply, {ok, DeploymentId}, State#{deployments => NewDeployments}};

handle_call({clone_api, ApiId, NewName}, _From, State) ->
    case get_latest_version(ApiId, State) of
        {ok, Version} ->
            NewApiId = generate_api_id(),
            NewVersionId = generate_version_id(NewApiId),

            NewVersion = Version#api_version{
                id = NewVersionId,
                api_id = NewApiId,
                version = <<"v1.0.0">>,
                created_at = erlang:system_time(millisecond)
            },

            Versions = State#{versions},
            NewVersions = maps:put(NewVersionId, NewVersion, Versions),

            {reply, {ok, {api_id, NewApiId}}, State#{versions => NewVersions}};
        {error, not_found} ->
            {reply, {error, api_not_found}, State}
    end;

handle_call({version_api, ApiId, VersionName}, _From, State) ->
    VersionId = generate_version_id(ApiId),
    Version = #api_version{
        id = VersionId,
        api_id = ApiId,
        version = VersionName,
        config = #{},
        created_at = erlang:system_time(millisecond),
        status = draft
    },

    Versions = State#{versions},
    NewVersions = maps:put(VersionId, Version, Versions),

    {reply, {ok, Version}, State#{versions => NewVersions}};

handle_call({get_deployment_status, DeploymentId}, _From, State) ->
    case maps:find(DeploymentId, State#{deployments}) of
        {ok, Deployment} ->
            Status = check_deployment_health(Deployment),
            UpdatedDeployment = Deployment#deployment{status = Status},
            NewDeployments = maps:put(DeploymentId, UpdatedDeployment, State#{deployments}),
            {reply, {ok, UpdatedDeployment}, State#{deployments => NewDeployments}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_api_versions, ApiId}, _From, State) ->
    VersionIds = maps:keys(State#{versions}),
    Versions = lists:filtermap(fun(Id) ->
        case maps:find(Id, State#{versions}) of
            {ok, #api_version{api_id = ApiId} = Version} ->
                {true, Version};
            _ ->
                false
        end
    end, VersionIds),

    {reply, Versions, State};

handle_call({schedule_deployment, ApiId, Environment, Time}, _From, State) ->
    Scheduled = Time,
    NewScheduled = [#{api_id => ApiId, environment => Environment, time => Scheduled} | State#{scheduled}],

    {reply, {ok, scheduled}, State#{scheduled => NewScheduled}};

handle_call({automate_deployment, DeploymentSpec}, _From, State) ->
    #{api_id := ApiId, environment := Environment} = DeploymentSpec,
    {ok, DeploymentId} = deploy_api(ApiId, Environment),

    Automation = #{
        id = DeploymentId,
        type => automated,
        schedule => maps:get(schedule, DeploymentSpec, immediate),
        pipeline => maps:get(pipeline, DeploymentSpec, []),
        notifications => maps:get(notifications, DeploymentSpec, [])
    },

    {reply, {ok, Automation}, State};

handle_call({monitor_deployment, DeploymentId}, _From, State) ->
    case maps:find(DeploymentId, State#{deployments}) of
        {ok, Deployment} ->
            MonitorResult = monitor_deployment_progress(Deployment),
            {reply, {ok, MonitorResult}, State};
        error ->
            {reply, {error, not_found}, State}
    end.

handle_info({deployment_timeout, DeploymentId}, State) ->
    case maps:find(DeploymentId, State#{deployments}) of
        {ok, Deployment} ->
            UpdatedDeployment = Deployment#deployment{
                status = failed,
                end_time = erlang:system_time(millisecond)
            },
            NewDeployments = maps:put(DeploymentId, UpdatedDeployment, State#{deployments}),
            {noreply, State#{deployments => NewDeployments}};
        error ->
            {noreply, State}
    end;

handle_info({health_check, DeploymentId}, State) ->
    case maps:find(DeploymentId, State#{deployments}) of
        {ok, #deployment{status = deployed} = Deployment} ->
            HealthResult = check_api_health(Deployment#deployment.api_id),
            HealthCheck = #{time => erlang:system_time(millisecond), result => HealthResult},
            UpdatedDeployment = Deployment#deployment{
                health_checks = [HealthCheck | Deployment#deployment.health_checks]
            },

            if
                HealthResult =:= unhealthy ->
                    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), {rollback, DeploymentId});
                true ->
                    ok
            end,

            NewDeployments = maps:put(DeploymentId, UpdatedDeployment, State#{deployments}),
            {noreply, State#{deployments => NewDeployments}};
        _ ->
            erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), {health_check, DeploymentId}),
            {noreply, State}
    end;

handle_info({rollback, DeploymentId}, State) ->
    case maps:find(DeploymentId, State#{deployments}) of
        {ok, Deployment} ->
            RollbackVersion = get_healthy_version(Deployment#deployment.api_id),
            if
                RollbackVersion =/= undefined ->
                    rollback_api(Deployment#deployment.api_id, RollbackVersion),
                    UpdatedDeployment = Deployment#deployment{
                        status = rollback,
                        rollback_data = #{rollback_to => RollbackVersion}
                    },
                    NewDeployments = maps:put(DeploymentId, UpdatedDeployment, State#{deployments}),
                    {noreply, State#{deployments => NewDeployments}};
                true ->
                    {noreply, State}
            end;
        error ->
            {noreply, State}
    end;

handle_info(scan_deployments, State) ->
    Scheduled = State#{scheduled},
    Now = erlang:system_time(millisecond),

    ReadyDeployments = lists:filter(fun(Scheduled) ->
        Scheduled#{time} =< Now
    end, Scheduled),

    lists:foreach(fun(Scheduled) ->
        deploy_api(Scheduled#{api_id}, Scheduled#{environment})
    end, ReadyDeployments),

    CleanScheduled = lists:filter(fun(Scheduled) ->
        Scheduled#{time} > Now
    end, Scheduled),

    erlang:send_after(60000, self(), scan_deployments),
    {noreply, State#{scheduled => CleanScheduled}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

generate_api_id() ->
    uuid:uuid_to_list(uuid:uuid4()).

generate_version_id(ApiId) ->
    list_to_binary([ApiId, "-", uuid:uuid4()]).

generate_deployment_id() ->
    uuid:uuid4().

get_latest_version(ApiId, State) ->
    Versions = lists:filter(fun(V) ->
        V#api_version.api_id =:= ApiId
    end, maps:values(State#{versions})),

    case Versions of
        [] -> {error, not_found};
        _ -> {ok, hd(lists:keysort(#api_version.created_at, Versions))}
    end.

get_current_deployment(ApiId, State) ->
    Deployments = lists:filter(fun(D) ->
        D#deployment.api_id =:= ApiId andalso
        D#deployment.status =:= deployed
    end, maps:values(State#{deployments})),

    case Deployments of
        [] -> {error, not_found};
        _ -> {ok, hd(lists:keysort(#deployment.start_time, Deployments))}
    end.

calculate_diff(OldConfig, NewConfig) ->
    Added = maps:without(maps:keys(OldConfig), NewConfig),
    Removed = maps:without(maps:keys(NewConfig), OldConfig),
    Updated = maps:filter(fun(K, V) ->
        case maps:find(K, OldConfig) of
            {ok, OldV} when OldV =/= V -> true;
            _ -> false
        end
    end, NewConfig),

    #{added => Added, removed => Removed, updated => Updated}.

check_deployment_health(Deployment) ->
    case erlmcp_api_gateway_monitor:get_health() of
        {ok, Health} when Health#{status} =:= healthy ->
            deployed;
        {ok, Health} when Health#{status} =:= degraded ->
            warning;
        _ ->
            failed
    end.

monitor_deployment_progress(Deployment) ->
    case check_deployment_health(Deployment) of
        deployed ->
            #{progress => 100, status => completed};
        failed ->
            #{progress => 0, status => failed};
        _ ->
            #{progress => 50, status => in_progress}
    end.

check_api_health(ApiId) ->
    {ok, Health} = erlmcp_api_gateway_monitor:get_health(),
    case Health#{status} of
        healthy -> healthy;
        degraded -> degraded;
        _ -> unhealthy
    end.

get_healthy_version(ApiId) ->
    case erlmcp_api_gateway_registry:list_apis() of
        [Api | _] when Api#{id} =:= ApiId ->
            <<"v1.0.0">>;
        _ ->
            undefined
    end.