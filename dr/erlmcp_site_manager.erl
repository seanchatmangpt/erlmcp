%% @private
%% Site Manager - Manages individual disaster recovery site operations
-module(erlmcp_site_manager).
-behaviour(gen_server).

%% API
-export([
    start_link/2,
    get_state/1,
    get_status/1,
    heartbeat/1,
    update_metrics/2,
    handle_removal/2,
    replicate_data/3
]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(state, {
    id :: binary(),
    config :: map(),
    role :: primary | active_secondary | standby,
    status :: healthy | degraded | failed | maintenance,
    last_heartbeat :: integer() | undefined,
    metrics :: map(),
    replication_sup :: pid() | undefined,
    health_sup :: pid() | undefined,
    connections :: map(),
    sessions :: map(),
    data :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(SiteId, Config) ->
    gen_server:start_link({via, gproc, {n, l, {erlmcp_site, SiteId}}}, ?MODULE, [SiteId, Config], []).

-spec get_state(pid()) -> {ok, map()} | {error, not_found}.
get_state(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_state).

-spec get_status(binary()) -> {ok, map()} | {error, not_found}.
get_status(SiteId) ->
    case whereis({via, gproc, {n, l, {erlmcp_site, SiteId}}}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, get_status);
        undefined ->
            {error, not_found}
    end.

-spec heartbeat(binary()) -> ok | {error, not_found}.
heartbeat(SiteId) ->
    case whereis({via, gproc, {n, l, {erlmcp_site, SiteId}}}) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, {heartbeat, erlang:system_time(millisecond)});
        undefined ->
            {error, not_found}
    end.

-spec update_metrics(binary(), map()) -> ok | {error, not_found}.
update_metrics(SiteId, Metrics) ->
    case whereis({via, gproc, {n, l, {erlmcp_site, SiteId}}}) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, {update_metrics, Metrics});
        undefined ->
            {error, not_found}
    end.

-spec handle_removal(binary(), binary()) -> ok.
handle_removal(SiteId, RemovedSiteId) ->
    %% Notify about removed site
    case whereis({via, gproc, {n, l, {erlmcp_site, SiteId}}}) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, {site_removed, RemovedSiteId});
        undefined ->
            ok
    end.

-spec replicate_data(binary(), binary(), any()) -> ok | {error, term()}.
replicate_data(SiteId, Key, Value) ->
    case whereis({via, gproc, {n, l, {erlmcp_site, SiteId}}}) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, {replicate_data, Key, Value});
        undefined ->
            {error, site_not_found}
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([binary(), map()]) -> {ok, #state{}}.
init([SiteId, Config]) ->
    %% Initialize site state
    Role = maps:get(role, Config, standby),
    InitialMetrics = #{
        capacity => maps:get(capacity, Config, #{
            max_connections => 1000,
            max_sessions => 500,
            max_bandwidth => 1000
        }),
        usage => #{
            connections => 0,
            sessions => 0,
            bandwidth => 0
        },
        health => #{
            cpu => 0,
            memory => 0,
            disk => 0,
            network => 0
        }
    },

    State = #state{
        id = SiteId,
        config = Config,
        role = Role,
        status = healthy,
        last_heartbeat => erlang:system_time(millisecond),
        metrics = InitialMetrics,
        connections = #{},
        sessions = #{},
        data = #{}
    },

    %% Start replication services
    ReplicationConfig = maps:get(replication, Config, #{
        mode => async,
        compression => true,
        encryption => true,
        batch_size => 100,
        interval => 1000
    }),

    %% Register with global monitoring
    erlmcp_global_monitor:register_site(SiteId, self(), Role),

    %% Start heartbeat
    erlang:send_after(1000, self(), heartbeat),

    %% Start metrics collection
    erlang:send_after(5000, self(), collect_metrics),

    {ok, State}.

-spec handle_call(term(), {pid(), any()}, #state{}) -> {reply, term(), #state{}}.
handle_call(get_state, _From, State) ->
    Reply = #{
        id => State#state.id,
        role => State#state.role,
        status => State#state.status,
        last_heartbeat => State#state.last_heartbeat,
        metrics => State#state.metrics
    },
    {reply, {ok, Reply}, State};

handle_call(get_status, _From, State) ->
    Status = #{
        site_id => State#state.id,
        role => State#state.role,
        status => State#state.status,
        last_heartbeat => State#state.last_heartbeat,
        health => calculate_health_status(State#state.metrics),
        readiness => calculate_readiness(State)
    },
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({heartbeat, Timestamp}, State) ->
    %% Update heartbeat
    NewState = State#state{
        last_heartbeat = Timestamp,
        status => case Timestamp - erlang:system_time(millisecond) of
                     Lag when Lag > 30000 -> degraded;
                     _ -> healthy
                 end
    },

    %% Check if we need to promote
    check_promotion_readiness(NewState),

    {noreply, NewState};

handle_cast({update_metrics, Metrics}, State) ->
    %% Update site metrics
    UpdatedMetrics = maps:merge(State#state.metrics, Metrics),
    NewState = State#state{metrics = UpdatedMetrics},

    %% Check thresholds
    check_metric_thresholds(NewState),

    {noreply, NewState};

handle_cast({site_removed, RemovedSiteId}, State) ->
    %% Handle site removal
    logger:info("Site ~p removed from DR configuration", [RemovedSiteId]),

    %% Update replication if this was a replication target
    case State#state.role of
        primary ->
            %% Redirect replication to other sites
            redirect_replication(State, RemovedSiteId);
        _ ->
            %% Update configuration
            ok
    end,

    {noreply, State};

handle_cast({replicate_data, Key, Value}, State) ->
    %% Handle data replication
    ReplicatedData = State#state.data,
    NewData = maps:put(Key, Value, ReplicatedData),
    NewState = State#state{data = NewData},

    %% Replicate to other sites if primary
    case State#state.role of
        primary ->
            replicate_to_other_sites(Key, Value);
        _ ->
            ok
    end,

    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(heartbeat, State) ->
    %% Send heartbeat to global monitor
    erlmcp_global_monitor:heartbeat(State#state.id, self(), State#state.metrics),

    %% Schedule next heartbeat
    erlang:send_after(1000, self(), heartbeat),

    {noreply, State};

handle_info(collect_metrics, State) ->
    %% Collect system metrics
    SystemMetrics = collect_system_metrics(),
    NewState = State#state{metrics = maps:merge(State#state.metrics, SystemMetrics)},

    %% Schedule next metrics collection
    erlang:send_after(5000, self(), collect_metrics),

    {noreply, NewState};

handle_info(promote_to_primary, State) ->
    %% Handle promotion
    logger:info("Promoting site ~p to primary", [State#state.id]),
    NewState = State#state{role = primary},

    %% Notify other sites
    notify_role_change(NewState),

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(Reason, State) ->
    logger:warning("Site manager ~p terminating: ~p", [State#state.id, Reason]),

    %% Cleanup
    erlmcp_global_monitor:deregister_site(State#state.id),

    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

check_promotion_readiness(State) ->
    %% Check if current primary is unhealthy and this site should be promoted
    case State#state.role of
        active_secondary ->
            PrimaryStatus = erlmcp_global_monitor:get_primary_status(),
            case PrimaryStatus of
                {ok, Status} ->
                    case Status#site_status.status =:= failed of
                        true ->
                            RequestPromotion = State#state.id,
                            erlmcp_promotion_manager:request_promotion(RequestPromotion);
                        false ->
                            ok
                    end;
                {error, not_found} ->
                    %% No primary found, promote to primary
                    RequestPromotion = State#state.id,
                    erlmcp_promotion_manager:request_promotion(RequestPromotion);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

calculate_health_status(Metrics) ->
    Health = maps:get(health, Metrics, #{}),
    Cpu = maps:get(cpu, Health, 0),
    Memory = maps:get(memory, Health, 0),
    Disk = maps:get(disk, Health, 0),
    Network = maps:get(network, Health, 0),

    %% Calculate overall health
    HealthScore = (Cpu + Memory + Disk + Network) / 4,

    case HealthScore of
        Score when Score >= 90 -> excellent;
        Score when Score >= 80 -> good;
        Score when Score >= 60 -> fair;
        _ -> poor
    end.

calculate_readiness(State) ->
    %% Calculate site readiness for promotion
    Health = maps:get(health, State#state.metrics, #{}),
    Usage = maps:get(usage, State#state.metrics, #{}),
    Connections = maps:get(connections, Usage, 0),
    Sessions = maps:get(sessions, Usage, 0),
    Capacity = maps:get(capacity, State#state.metrics, #{max_connections => 1000}),

    %% Check capacity utilization
    ConnectionUtilization = Connections / maps:get(max_connections, Capacity, 1000),
    SessionUtilization = Sessions / maps:get(max_sessions, Capacity, 500),

    %% Check health
    Cpu = maps:get(cpu, Health, 100),
    Memory = maps:get(memory, Health, 100),
    Disk = maps:get(disk, Health, 100),

    ReadinessScore = calculate_readiness_score(
        ConnectionUtilization, SessionUtilization, Cpu, Memory, Disk
    ),

    case ReadinessScore of
        Score when Score >= 0.9 -> ready;
        Score when Score >= 0.7 -> ready_with_warnings;
        _ -> not_ready
    end.

calculate_readiness_score(CConn, CSess, CCpu, CMem, CDisk) ->
    %% Weighted score calculation
    ConnWeight = 0.3,
    SessionWeight = 0.3,
    CpuWeight = 0.15,
    MemWeight = 0.15,
    DiskWeight = 0.1,

    Score = (ConnWeight * (1 - CConn)) +
            (SessionWeight * (1 - CSess)) +
            (CpuWeight * (1 - CCpu/100)) +
            (MemWeight * (1 - CMem/100)) +
            (DiskWeight * (1 - CDisk/100)),

    min(Score, 1.0).

check_metric_thresholds(State) ->
    Metrics = State#state.metrics,
    Health = maps:get(health, Metrics, #{}),

    %% Check CPU threshold
    Cpu = maps:get(cpu, Health, 0),
    if
        Cpu > 90 ->
            logger:warning("Site ~p CPU utilization high: ~p%", [State#state.id, Cpu]);
        true ->
            ok
    end,

    %% Check memory threshold
    Memory = maps:get(memory, Health, 0),
    if
        Memory > 85 ->
            logger:warning("Site ~p memory utilization high: ~p%", [State#state.id, Memory]);
        true ->
            ok
    end,

    %% Check disk threshold
    Disk = maps:get(disk, Health, 0),
    if
        Disk > 90 ->
            logger:warning("Site ~p disk utilization high: ~p%", [State#state.id, Disk]);
        true ->
            ok
    end.

collect_system_metrics() ->
    %% Collect system metrics
    Cpu = erlmcp_system_metrics:cpu_utilization(),
    Memory = erlmcp_system_metrics:memory_utilization(),
    Disk = erlmcp_system_metrics:disk_utilization(),
    Network = erlmcp_system_metrics:network_utilization(),

    #{
        health => #{
            cpu => Cpu,
            memory => Memory,
            disk => Disk,
            network => Network
        }
    }.

redirect_replication(State, RemovedSiteId) ->
    %% Redirect replication to other sites
    OtherSites = erlmcp_global_directory:get_all_site_ids(),
    ActiveSites = lists:filter(fun(SiteId) ->
        SiteId =/= RemovedSiteId andalso SiteId =/= State#state.id
    end, OtherSites),

    %% Update replication configuration
    lists:foreach(fun(SiteId) ->
        erlmcp_site_replication:add_target(SiteId)
    end, ActiveSites).

replicate_to_other_sites(Key, Value) ->
    %% Replicate data to other active sites
    OtherSites = erlmcp_global_directory:get_all_site_ids(),
    lists:foreach(fun(SiteId) ->
        erlmcp_site_manager:replicate_data(SiteId, Key, Value)
    end, OtherSites).

notify_role_change(State) ->
    %% Notify other sites about role change
    OtherSites = erlmcp_global_directory:get_all_site_ids(),
    lists:foreach(fun(SiteId) ->
        erlmcp_site_manager:handle_removal(SiteId, State#state.id)
    end, OtherSites).