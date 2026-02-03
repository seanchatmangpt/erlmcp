%% @private
%% Global Monitor - Centralized monitoring and health checking across all sites
%% Coordinates health monitoring, alerting, and system-wide coordination
-module(erlmcp_global_monitor).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    register_site/3,
    register_promotion_manager/1,
    register_traffic_director/1,
    deregister_site/1,
    deregister_promotion_manager/1,
    deregister_traffic_director/1,
    heartbeat/3,
    get_site_status/1,
    get_primary_status/0,
    get_global_health/0,
    alert/2
]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(site_status, {
    site_id :: binary(),
    pid :: pid(),
    role :: primary | active_secondary | standby,
    status :: healthy | degraded | failed | maintenance,
    last_heartbeat :: integer() | undefined,
    metrics :: map(),
    alerts :: [map()],
    capabilities :: map()
}).

-record(promotion_manager, {
    pid :: pid(),
    active_promotions :: integer(),
    last_activity :: integer() | undefined
}).

-record(traffic_director, {
    pid :: pid(),
    current_config :: map(),
    last_update :: integer() | undefined
}).

-record(global_state, {
    sites :: #{binary() => #site_status{}},
    promotion_managers :: [#promotion_manager{}],
    traffic_directors :: [#traffic_director{}],
    health_thresholds :: map(),
    alert_rules :: [map()],
    metrics_history :: [map()],
    system_status :: healthy | degraded | failed
}).

-define(SERVER, ?MODULE).
-define(HEALTH_CHECK_INTERVAL, 5000).
-define(ALERT_COOLDOWN, 300000).  % 5 minutes
-define(MAX_METRICS_HISTORY, 1000).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec register_site(binary(), pid(), map()) -> ok.
register_site(SiteId, Pid, Config) ->
    gen_server:cast(?SERVER, {register_site, SiteId, Pid, Config}).

-spec register_promotion_manager(pid()) -> ok.
register_promotion_manager(Pid) ->
    gen_server:cast(?SERVER, {register_promotion_manager, Pid}).

-spec register_traffic_director(pid()) -> ok.
register_traffic_director(Pid) ->
    gen_server:cast(?SERVER, {register_traffic_director, Pid}).

-spec deregister_site(binary()) -> ok.
deregister_site(SiteId) ->
    gen_server:cast(?SERVER, {deregister_site, SiteId}).

-spec deregister_promotion_manager(pid()) -> ok.
deregister_promotion_manager(Pid) ->
    gen_server:cast(?SERVER, {deregister_promotion_manager, Pid}).

-spec deregister_traffic_director(pid()) -> ok.
deregister_traffic_director(Pid) ->
    gen_server:cast(?SERVER, {deregister_traffic_director, Pid}).

-spec heartbeat(binary(), pid(), map()) -> ok.
heartbeat(SiteId, Pid, Metrics) ->
    gen_server:cast(?SERVER, {heartbeat, SiteId, Pid, Metrics}).

-spec get_site_status(binary()) -> {ok, #site_status{}} | {error, term()}.
get_site_status(SiteId) ->
    gen_server:call(?SERVER, {get_site_status, SiteId}).

-spec get_primary_status() -> {ok, #site_status{}} | {error, not_found}.
get_primary_status() ->
    gen_server:call(?SERVER, get_primary_status).

-spec get_global_health() -> {ok, map()}.
get_global_health() ->
    gen_server:call(?SERVER, get_global_health).

-spec alert(binary(), map()) -> ok.
alert(SiteId, Alert) ->
    gen_server:cast(?SERVER, {alert, SiteId, Alert}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #global_state{}}.
init([]) ->
    %% Initialize global state
    State = #global_state{
        sites = #{},
        promotion_managers = [],
        traffic_directors = [],
        health_thresholds = get_health_thresholds(),
        alert_rules = get_alert_rules(),
        metrics_history = [],
        system_status = healthy
    },

    %% Start health checking
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), perform_health_check),

    %% Start metrics collection
    erlang:send_after(10000, self(), collect_metrics),

    {ok, State}.

-spec handle_call(term(), {pid(), any()}, #global_state{}) -> {reply, term(), #global_state{}}.
handle_call({get_site_status, SiteId}, _From, State) ->
    case maps:find(SiteId, State#global_state.sites) of
        {ok, Site} ->
            {reply, {ok, Site}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_primary_status, _From, State) ->
    PrimarySites = lists:filter(fun(Site) ->
        Site#site_status.role =:= primary
    end, maps:values(State#global_state.sites)),

    case PrimarySites of
        [Primary|_] ->
            {reply, {ok, Primary}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_global_health, _From, State) ->
    GlobalHealth = calculate_global_health(State),
    {reply, {ok, GlobalHealth}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #global_state{}) -> {noreply, #global_state{}}.
handle_cast({register_site, SiteId, Pid, Config}, State) ->
    %% Register new site
    Role = maps:get(role, Config, standby),
    Capabilities = maps:get(capabilities, Config, #{}),

    SiteStatus = #site_status{
        site_id = SiteId,
        pid = Pid,
        role = Role,
        status = healthy,
        last_heartbeat = erlang:system_time(millisecond),
        metrics = #{},
        alerts = [],
        capabilities = Capabilities
    },

    %% Update site map
    NewSites = maps:put(SiteId, SiteStatus, State#global_state.sites),

    %% Notify about new site registration
    notify_site_registration(SiteId, Role),

    %% Check if we need to assign primary role
    case should_promote_to_primary(SiteId, NewSites) of
        true ->
            request_site_promotion(SiteId);
        false ->
            ok
    end,

    {noreply, State#global_state{sites = NewSites}};

handle_cast({register_promotion_manager, Pid}, State) ->
    %% Register promotion manager
    NewPromotionManager = #promotion_manager{
        pid = Pid,
        active_promotions = 0,
        last_activity = erlang:system_time(millisecond)
    },

    NewManagers = [NewPromotionManager | State#global_state.promotion_managers],

    {noreply, State#global_state{promotion_managers = NewManagers}};

handle_cast({register_traffic_director, Pid}, State) ->
    %% Register traffic director
    NewTrafficDirector = #traffic_director{
        pid = Pid,
        current_config = #{},
        last_update = undefined
    },

    NewDirectors = [NewTrafficDirector | State#global_state.traffic_directors],

    {noreply, State#global_state{traffic_directors = NewDirectors}};

handle_cast({deregister_site, SiteId}, State) ->
    %% Remove site
    case maps:find(SiteId, State#global_state.sites) of
        {ok, Site} ->
            Role = Site#site_status.role,
            NewSites = maps:remove(SiteId, State#global_state.sites),

            %% Check if this was the primary site
            case Role of
                primary ->
                    initiate_failover(NewSites);
                _ ->
                    ok
            end,

            %% Notify about site removal
            notify_site_removal(SiteId, Role),

            {noreply, State#global_state{sites = NewSites}};
        error ->
            {noreply, State}
    end;

handle_cast({deregister_promotion_manager, Pid}, State) ->
    %% Remove promotion manager
    NewManagers = lists:filter(fun(Manager) ->
        Manager#promotion_manager.pid =/= Pid
    end, State#global_state.promotion_managers),

    {noreply, State#global_state{promotion_managers = NewManagers}};

handle_cast({deregister_traffic_director, Pid}, State) ->
    %% Remove traffic director
    NewDirectors = lists:filter(fun(Director) ->
        Director#traffic_director.pid =/= Pid
    end, State#global_state.traffic_directors),

    {noreply, State#global_state{traffic_directors = NewDirectors}};

handle_cast({heartbeat, SiteId, Pid, Metrics}, State) ->
    %% Update site heartbeat and metrics
    case maps:find(SiteId, State#global_state.sites) of
        {ok, Site} ->
            NewSite = Site#site_status{
                last_heartbeat = erlang:system_time(millisecond),
                metrics = Metrics
            },

            %% Update status based on metrics
            NewStatus = determine_site_status(Site#site_status.status, Metrics),
            UpdatedSite = NewSite#site_status{status = NewStatus},

            %% Check for alerts
            Alerts = check_health_thresholds(SiteId, Metrics, State#global_state.health_thresholds),

            NewSites = maps:put(SiteId, UpdatedSite, State#global_state.sites),

            %% Notify about status changes
            case NewStatus =/= Site#site_status.status of
                true ->
                    notify_status_change(SiteId, NewStatus);
                false ->
                    ok
            end,

            {noreply, State#global_state{sites = NewSites}};
        error ->
            {noreply, State}
    end;

handle_cast({alert, SiteId, Alert}, State) ->
    %% Handle site alert
    case maps:find(SiteId, State#global_state.sites) of
        {ok, Site} ->
            %% Add alert to site
            NewAlerts = [Alert | Site#site_status.alerts],
            UpdatedSite = Site#site_status{alerts = NewAlerts},

            %% Check if this alert requires escalation
            case needs_escalation(Alert) of
                true ->
                    escalate_alert(SiteId, Alert);
                false ->
                    ok
            end,

            NewSites = maps:put(SiteId, UpdatedSite, State#global_state.sites),

            {noreply, State#global_state{sites = NewSites}};
        error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #global_state{}) -> {noreply, #global_state{}}.
handle_info(perform_health_check, State) ->
    %% Perform health check on all sites
    NewSites = maps:map(fun(SiteId, Site) ->
        check_site_health(SiteId, Site, State)
    end, State#global_state.sites),

    %% Check overall system health
    SystemStatus = determine_system_status(NewSites),
    UpdatedState = State#global_state{
        sites = NewSites,
        system_status = SystemStatus
    },

    %% Schedule next health check
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), perform_health_check),

    {noreply, UpdatedState};

handle_info(collect_metrics, State) ->
    %% Collect system-wide metrics
    Metrics = collect_system_metrics(State),

    %% Add to history
    NewHistory = lists:sublist([Metrics | State#global_state.metrics_history],
                              ?MAX_METRICS_HISTORY),

    %% Analyze trends
    analyze_trends(State#global_state.metrics_history, Metrics),

    %% Schedule next metrics collection
    erlang:send_after(10000, self(), collect_metrics),

    {noreply, State#global_state{metrics_history = NewHistory}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #global_state{}) -> ok.
terminate(Reason, State) ->
    logger:warning("Global monitor terminating: ~p", [Reason]),
    ok.

-spec code_change(term(), #global_state{}, term()) -> {ok, #global_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

get_health_thresholds() ->
    %% Get health thresholds
    #{
        cpu => 90,
        memory => 85,
        disk => 90,
        network => 80,
        response_time => 1000,
        error_rate => 1.0,
        replication_lag => 5000
    }.

get_alert_rules() ->
    %% Get alert rules
    [
        #{
            name => "high_cpu_usage",
            condition => "cpu > 90",
            severity => "critical",
            escalation => true
        },
        #{
            name => "high_memory_usage",
            condition => "memory > 85",
            severity => "warning",
            escalation => false
        },
        #{
            name => "site_failure",
            condition => "status = 'failed'",
            severity => "critical",
            escalation => true
        },
        #{
            name => "replication_lag",
            condition => "replication_lag > 5000",
            severity => "warning",
            escalation => false
        }
    ].

calculate_global_health(State) ->
    %% Calculate overall system health
    Sites = State#global_state.sites,
    NumSites = maps:size(Sites),

    HealthyCount = maps:fold(fun(_SiteId, Site, Acc) ->
        case Site#site_status.status of
            healthy -> Acc + 1;
            _ -> Acc
        end
    end, 0, Sites),

    HealthPercentage = HealthyCount / NumSites,

    OverallStatus = case HealthPercentage of
        Score when Score >= 0.9 -> excellent;
        Score when Score >= 0.7 -> good;
        Score when Score >= 0.5 -> fair;
        _ -> poor
    end,

    #{
        status => OverallStatus,
        health_percentage => HealthPercentage,
        total_sites => NumSites,
        healthy_sites => HealthyCount,
        degraded_sites => maps:size(Sites) - HealthyCount,
        system_status => State#global_state.system_status,
        last_update => erlang:system_time(millisecond)
    }.

determine_site_status(CurrentStatus, Metrics) ->
    %% Determine site status based on metrics
    Cpu = maps:get(cpu, Metrics, 0),
    Memory = maps:get(memory, Metrics, 0),
    Disk = maps:get(disk, Metrics, 0),

    case {Cpu, Memory, Disk} of
        {C, M, D} when C > 95 orelse M > 95 orelse D > 95 ->
            failed;
        {C, M, D} when C > 90 orelse M > 90 orelse D > 90 ->
            degraded;
        _ ->
            healthy
    end.

check_site_health(SiteId, Site, State) ->
    %% Check if site is still healthy
    Now = erlang:system_time(millisecond),
    LastHeartbeat = Site#site_status.last_heartbeat,

    case Now - LastHeartbeat of
        Lag when Lag > 30000 ->
            %% Site is unresponsive
            UpdatedSite = Site#site_status{
                status = failed,
                alerts = [#{
                    type => "unresponsive",
                    message => "Site not responding",
                    timestamp => Now,
                    severity => "critical"
                } | Site#site_status.alerts]
            },
            UpdatedSite;
        _ ->
            Site
    end.

determine_system_status(Sites) ->
    %% Determine overall system status
    NumSites = maps:size(Sites),
    FailedCount = maps:fold(fun(_Id, Site, Acc) ->
        case Site#site_status.status of
            failed -> Acc + 1;
            _ -> Acc
        end
    end, 0, Sites),

    FailedPercentage = FailedCount / NumSites,

    case FailedPercentage of
        P when P >= 0.5 -> failed;
        P when P >= 0.2 -> degraded;
        _ -> healthy
    end.

check_health_thresholds(SiteId, Metrics, Thresholds) ->
    %% Check if metrics exceed thresholds
    Alerts = [],
    Thresholds = #{
        cpu => CpuThreshold,
        memory => MemThreshold,
        disk => DiskThreshold
    },

    case maps:get(cpu, Metrics, 0) > CpuThreshold of
        true ->
            [create_alert("high_cpu", SiteId, Metrics) | Alerts];
        false ->
            Alerts
    end,

    case maps:get(memory, Metrics, 0) > MemThreshold of
        true ->
            [create_alert("high_memory", SiteId, Metrics) | Alerts];
        false ->
            Alerts
    end,

    case maps:get(disk, Metrics, 0) > DiskThreshold of
        true ->
            [create_alert("high_disk", SiteId, Metrics) | Alerts];
        false ->
            Alerts
    end.

create_alert(AlertType, SiteId, Metrics) ->
    %% Create alert
    #{
        type => AlertType,
        site_id => SiteId,
        timestamp => erlang:system_time(millisecond),
        metrics => Metrics,
        severity => case AlertType of
                       "high_cpu" -> "critical";
                       "high_memory" -> "warning";
                       "high_disk" -> "warning";
                       _ -> "info"
                   end
    }.

should_promote_to_primary(SiteId, Sites) ->
    %% Check if site should be promoted to primary
    PrimaryCount = maps:fold(fun(_Id, Site, Acc) ->
        case Site#site_status.role of
            primary -> Acc + 1;
            _ -> Acc
        end
    end, 0, Sites),

    case PrimaryCount of
        0 ->
            %% No primary exists - promote this site
            true;
        _ ->
            false
    end.

request_site_promotion(SiteId) ->
    %% Request promotion for site
    case erlmcp_global_directory:find_site_manager(SiteId) of
        {ok, Pid} ->
            gen_server:call(Pid, request_promotion);
        {error, not_found} ->
            ok
    end.

initiate_failover(Sites) ->
    %% Initiate failover when primary site fails
    %% Find healthy site to promote
    HealthiestSite = lists:fold(fun(SiteId, Site, Acc) ->
        case Site#site_status.status of
            healthy ->
                case Acc of
                    none -> {SiteId, Site};
                    _ -> Acc
                end;
            _ ->
                Acc
        end
    end, none, maps:to_list(Sites)),

    case HealthiestSite of
        {PromoteSiteId, _} ->
            %% Promote site to primary
            case erlmcp_global_directory:find_site_manager(PromoteSiteId) of
                {ok, Pid} ->
                    gen_server:call(Pid, promote_to_primary);
                {error, not_found} ->
                    ok
            end;
        none ->
            %% No healthy sites available
            logger:error("No healthy sites available for failover")
    end.

notify_site_registration(SiteId, Role) ->
    %% Notify about site registration
    logger:info("Site registered: ~p (role: ~p)", [SiteId, Role]),
    erlmcp_global_monitor:notify(<<"site_registered">>, #{
        site_id => SiteId,
        role => Role,
        timestamp => erlang:system_time(millisecond)
    }).

notify_site_removal(SiteId, Role) ->
    %% Notify about site removal
    logger:info("Site removed: ~p (role: ~p)", [SiteId, Role]),
    erlmcp_global_monitor:notify(<<"site_removed">>, #{
        site_id => SiteId,
        role => Role,
        timestamp => erlang:system_time(millisecond)
    }).

notify_status_change(SiteId, NewStatus) ->
    %% Notify about status change
    logger:info("Site status changed: ~p -> ~p", [SiteId, NewStatus]),
    erlmcp_global_monitor:notify(<<"status_change">>, #{
        site_id => SiteId,
        new_status => NewStatus,
        timestamp => erlang:system_time(millisecond)
    }).

needs_escalation(Alert) ->
    %% Check if alert needs escalation
    case maps:get(severity, Alert, "info") of
        "critical" -> true;
        "high" -> true;
        _ -> false
    end.

escalate_alert(SiteId, Alert) ->
    %% Escalate alert to higher level
    logger:warning("Escalating alert for site ~p: ~p", [SiteId, Alert]),
    erlmcp_global_monitor:notify(<<"alert_escalated">>, #{
        site_id => SiteId,
        alert => Alert,
        timestamp => erlang:system_time(millisecond)
    }).

collect_system_metrics(State) ->
    %% Collect system-wide metrics
    #{
        timestamp => erlang:system_time(millisecond),
        sites => maps:size(State#global_state.sites),
        healthy_sites => maps:fold(fun(_Id, Site, Acc) ->
            case Site#site_status.status of
                healthy -> Acc + 1;
                _ -> Acc
            end
        end, 0, State#global_state.sites),
        total_promotions => length(State#global_state.promotion_managers),
        active_traffic_directors => length(State#global_state.traffic_directors),
        system_status => State#global_state.system_status
    }.

analyze_trends(History, CurrentMetrics) ->
    %% Analyze performance trends
    case length(History) of
        N when N > 10 ->
            %% Analyze trends over last 10 samples
            OldMetrics = lists:nth(N - 9, History),
            analyze_trend(OldMetrics, CurrentMetrics);
        _ ->
            ok
    end.

analyze_trend(OldMetrics, CurrentMetrics) ->
    %% Analyze specific trends
    case maps:get(system_status, OldMetrics) =:= healthy andalso
         maps:get(system_status, CurrentMetrics) =:= degraded of
        true ->
            logger:warning("System health degrading from healthy to degraded");
        false ->
            ok
    end.