%% @private
%% Multi-Site Disaster Recovery Supervisor
%% Coordinates failover, replication, and site management across geographic locations
-module(erlmcp_multi_site_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_site/2, stop_site/1, promote_site/2, get_site_status/1]).

%% Internal exports
-export([init/1]).

%% Records
-record(site_state, {
    id :: binary(),
    pid :: pid(),
    config :: map(),
    status :: healthy | degraded | failed | maintenance,
    role :: primary | active_secondary | standby,
    replication_sup :: pid(),
    health_sup :: pid(),
    last_heartbeat :: integer() | undefined,
    metrics :: map()
}).

-define(SERVER, ?MODULE).
-define(REPLICATION_INTERVAL, 1000).  % 1 second for critical replication
-define(HEALTH_CHECK_INTERVAL, 5000).  % 5 seconds
-define(PROMOTION_TIMEOUT, 60000).   % 1 minute for promotion

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_site(binary(), map()) -> {ok, pid()} | {error, term()}.
start_site(SiteId, Config) ->
    case supervisor:start_child(?SERVER, make_site_child_spec(SiteId, Config)) of
        {ok, SitePid} ->
            start_site_services(SiteId, SitePid, Config),
            {ok, SitePid};
        {error, Reason} = Error ->
            Error
    end.

-spec stop_site(binary()) -> ok | {error, term()}.
stop_site(SiteId) ->
    case get_site_pid(SiteId) of
        {ok, SitePid} ->
            ok = supervisor:terminate_child(?SERVER, SiteId),
            ok = supervisor:delete_child(?SERVER, SiteId),
            %% Notify other sites
            notify_site_removal(SiteId),
            ok;
        {error, not_found} ->
            ok
    end.

-spec promote_site(binary(), term()) -> ok | {error, term()}.
promote_site(SiteId, Reason) ->
    case get_site_state(SiteId) of
        {ok, #site_state{role = active_secondary}} ->
            %% Validate promotion readiness
            case validate_promotion_readiness(SiteId) of
                true ->
                    %% Initiate promotion process
                    erlmcp_promotion_manager:start_promotion(SiteId, Reason);
                false ->
                    {error, promotion_not_ready}
            end;
        {ok, _} ->
            {error, invalid_site_role};
        {error, not_found} ->
            {error, site_not_found}
    end.

-spec get_site_status(binary()) -> {ok, map()} | {error, not_found}.
get_site_status(SiteId) ->
    case get_site_state(SiteId) of
        {ok, State} ->
            Status = #{
                id => State#site_state.id,
                status => State#site_state.status,
                role => State#site_state.role,
                last_heartbeat => State#site_state.last_heartbeat,
                metrics => State#site_state.metrics
            },
            {ok, Status};
        {error, not_found} ->
            {error, not_found}
    end.

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Multi-Site Supervisor Strategy
    %% - one_for_one: Individual site failures don't affect others
    %% - Max restarts: 5 per 60 seconds
    %% - Max time: 60 seconds
    %% - Auto-hibernation after 30 seconds idle
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60,
        auto_hibernation => ?MODULE
    },

    ChildSpecs = [
        %% Site replication supervisor
        #{id => erlmcp_site_replication_sup,
          start => {erlmcp_site_replication_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor,
          modules => [erlmcp_site_replication_sup]},

        %% Site health supervisor
        #{id => erlmcp_site_health_sup,
          start => {erlmcp_site_health_sup, start_link, []},
          restart => permanent,
          shutdown => infinity,
          type => supervisor,
          modules => [erlmcp_site_health_sup]},

        %% Promotion manager
        #{id => erlmcp_promotion_manager,
          start => {erlmcp_promotion_manager, start_link, []},
          restart => transient,
          shutdown => 30000,
          type => worker,
          modules => [erlmcp_promotion_manager]},

        %% Traffic director
        #{id => erlmcp_traffic_director,
          start => {erlmcp_traffic_director, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_traffic_director]}
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal Functions
%%====================================================================

make_site_child_spec(SiteId, Config) ->
    #{id => SiteId,
      start => {erlmcp_site_manager, start_link, [SiteId, Config]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [erlmcp_site_manager]}.

start_site_services(SiteId, SitePid, Config) ->
    %% Start replication services
    erlmcp_site_replication_sup:start_replication(SiteId, Config),

    %% Start health monitoring
    erlmcp_site_health_sup:start_health_monitor(SiteId, Config),

    %% Register with global directory
    erlmcp_global_directory:register_site(SiteId, SitePid, Config).

get_site_pid(SiteId) ->
    case supervisor:which_children(?SERVER) of
        Children when is_list(Children) ->
            case lists:keyfind(SiteId, 1, Children) of
                {SiteId, Pid, _, _} when is_pid(Pid) ->
                    {ok, Pid};
                _ ->
                    {error, not_found}
            end;
        _ ->
            {error, not_found}
    end.

get_site_state(SiteId) ->
    case get_site_pid(SiteId) of
        {ok, SitePid} ->
            erlmcp_site_manager:get_state(SitePid);
        {error, not_found} ->
            {error, not_found}
    end.

validate_promotion_readiness(SiteId) ->
    %% Check all systems are healthy
    HealthStatus = erlmcp_site_health:get_site_health(SiteId),
    case HealthStatus#health_status.overall_status of
        healthy ->
            %% Check replication is healthy
            ReplicationStatus = erlmcp_site_replication:get_status(SiteId),
            case ReplicationStatus#replication_status.status of
                healthy ->
                    %% Check capacity is sufficient
                    CapacityCheck = check_site_capacity(SiteId),
                    CapacityCheck;
                _ ->
                    false
            end;
        _ ->
            false
    end.

notify_site_removal(SiteId) ->
    %% Notify all remaining sites
    RemainingSites = get_all_active_sites(),
    lists:foreach(fun(RemainingSite) ->
        erlmcp_site_manager:notify_site_removed(RemainingSite, SiteId)
    end, RemainingSites).

check_site_capacity(SiteId) ->
    %% Check if site has sufficient capacity to take over primary role
    Metrics = erlmcp_metrics:get_site_metrics(SiteId),
    Capacity = Metrics#site_metrics.capacity,
    Usage = Metrics#site_metrics.usage,

    %% Ensure usage is below 80% of capacity
    UsagePercent = (Usage / Capacity) * 100,
    UsagePercent < 80.

%% Auto-hibernation callback
-spec hibernate_after() -> non_neg_integer().
hibernate_after() -> 30000.  % 30 seconds idle

%%====================================================================
%% Debug Functions
%%====================================================================

-spec get_all_site_ids() -> [binary()].
get_all_site_ids() ->
    %% Get all registered sites
    erlmcp_global_directory:get_all_site_ids().

-spec get_all_active_sites() -> [binary()].
get_all_active_sites() ->
    %% Get sites with healthy status
    Sites = get_all_site_ids(),
    lists:filter(fun(SiteId) ->
        case get_site_status(SiteId) of
            {ok, Status} ->
                maps:get(status, Status) =:= healthy;
            {error, _} ->
                false
        end
    end, Sites).