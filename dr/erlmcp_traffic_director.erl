%% @private
%% Traffic Director - Manages global traffic distribution and failover
-module(erlmcp_traffic_director).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_current_config/0,
    update_weights/1,
    promote_site/1,
    demote_site/1,
    enable_site/1,
    disable_site/1
]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(traffic_config, {
    weights :: #{binary() => integer()},
    health :: #{binary() => healthy | degraded | failed},
    status :: normal | failover | emergency,
    last_updated :: integer(),
    history :: [#{}]
}).

-record(site_health, {
    site_id :: binary(),
    status :: healthy | degraded | failed,
    last_heartbeat :: integer() | undefined,
    metrics :: map()
}).

-define(SERVER, ?MODULE).
-define(CONFIG_VERSION, 1).
-define(HEALTH_CHECK_INTERVAL, 5000).
-define(FORCE_PROMOTION_THRESHOLD, 0.7).  % 70% of sites failed
-define(MAX_HISTORY, 1000).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_current_config() -> {ok, map()}.
get_current_config() ->
    gen_server:call(?SERVER, get_current_config).

-spec update_weights(#{binary() => integer()}) -> ok | {error, term()}.
update_weights(NewWeights) ->
    gen_server:call(?SERVER, {update_weights, NewWeights}).

-spec promote_site(binary()) -> ok | {error, term()}.
promote_site(SiteId) ->
    gen_server:call(?SERVER, {promote_site, SiteId}).

-spec demote_site(binary()) -> ok | {error, term()}.
demote_site(SiteId) ->
    gen_server:call(?SERVER, {demote_site, SiteId}).

-spec enable_site(binary()) -> ok | {error, term()}.
enable_site(SiteId) ->
    gen_server:call(?SERVER, {enable_site, SiteId}).

-spec disable_site(binary()) -> ok | {error, term()}.
disable_site(SiteId) ->
    gen_server:call(?SERVER, {disable_site, SiteId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #traffic_config{}}.
init([]) ->
    %% Initialize traffic configuration
    InitialWeights = #{
        <<"dc01">> => 80,  % Primary site
        <<"dc02">> => 20,  % DR site 1
        <<"dc03">> => 0    % DR site 2 (standby)
    },

    InitialHealth = #{
        <<"dc01">> => healthy,
        <<"dc02">> => healthy,
        <<"dc03">> => healthy
    },

    State = #traffic_config{
        weights = InitialWeights,
        health = InitialHealth,
        status = normal,
        last_updated = erlang:system_time(millisecond),
        history = []
    },

    %% Start health checking
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), check_health),

    %% Register with global monitoring
    erlmcp_global_monitor:register_traffic_director(self()),

    %% Initial health check
    check_all_sites_health(State),

    {ok, State}.

-spec handle_call(term(), {pid(), any()}, #traffic_config{}) -> {reply, term(), #traffic_config{}}.
handle_call(get_current_config, _From, State) ->
    Config = #{
        weights => State#traffic_config.weights,
        health => State#traffic_config.health,
        status => State#traffic_config.status,
        last_updated => State#traffic_config.last_updated,
        version => ?CONFIG_VERSION
    },
    {reply, {ok, Config}, State};

handle_call({update_weights, NewWeights}, _From, State) ->
    %% Validate weights
    case validate_weights(NewWeights) of
        valid ->
            %% Update weights
            OldWeights = State#traffic_config.weights,
            UpdatedState = State#traffic_config{
                weights = NewWeights,
                last_updated = erlang:system_time(millisecond),
                history = add_to_history(OldWeights, State#traffic_config.history)
            },

            %% Notify global monitoring
            notify_weight_change(UpdatedState),

            {reply, ok, UpdatedState};
        invalid ->
            {reply, {error, invalid_weights}, State}
    end;

handle_call({promote_site, SiteId}, _From, State) ->
    case State#traffic_config.health of
        #{SiteId := healthy} ->
            %% Promote site - set weight to 100%
            NewWeights = maps:put(SiteId, 100, State#traffic_config.weights),
            %% Demote other sites
            OtherSites = maps:keys(NewWeights) -- [SiteId],
            UpdatedWeights = lists:foldl(fun(Site, Acc) ->
                maps:put(Site, 0, Acc)
            end, NewWeights, OtherSites),

            UpdatedState = State#traffic_config{
                weights = UpdatedWeights,
                last_updated = erlang:system_time(millisecond),
                status => failover,
                history = add_to_history(State#traffic_config.weights, State#traffic_config.history)
            },

            %% Notify about promotion
            notify_promotion(SiteId, UpdatedState),

            {reply, ok, UpdatedState};
        _ ->
            {reply, {error, site_not_healthy}, State}
    end;

handle_call({demote_site, SiteId}, _From, State) ->
    case State#traffic_config.weights of
        #{SiteId := Weight} when Weight > 0 ->
            %% Demote site - set weight to 0%
            NewWeights = maps:put(SiteId, 0, State#traffic_config.weights),

            %% Redistribute weights to other healthy sites
            UpdatedWeights = redistribute_weights(NewWeights, SiteId, State),

            UpdatedState = State#traffic_config{
                weights = UpdatedWeights,
                last_updated = erlang:system_time(millisecond),
                history = add_to_history(State#traffic_config.weights, State#traffic_config.history)
            },

            %% Notify about demotion
            notify_demotion(SiteId, UpdatedState),

            {reply, ok, UpdatedState};
        _ ->
            {reply, ok, State}  % Already demoted
    end;

handle_call({enable_site, SiteId}, _From, State) ->
    case State#traffic_config.health of
        #{SiteId := _} ->
            %% Enable site - set default weight
            DefaultWeights = get_default_weights(),
            NewWeight = maps:get(SiteId, DefaultWeights, 0),
            NewWeights = maps:put(SiteId, NewWeight, State#traffic_config.weights),

            UpdatedState = State#traffic_config{
                weights = NewWeights,
                last_updated = erlang:system_time(millisecond),
                history = add_to_history(State#traffic_config.weights, State#traffic_config.history)
            },

            {reply, ok, UpdatedState};
        _ ->
            {reply, {error, site_not_found}, State}
    end;

handle_call({disable_site, SiteId}, _From, State) ->
    NewWeights = maps:put(SiteId, 0, State#traffic_config.weights),

    UpdatedState = State#traffic_config{
        weights = NewWeights,
        last_updated = erlang:system_time(millisecond),
        history = add_to_history(State#traffic_config.weights, State#traffic_config.history)
    },

    {reply, ok, UpdatedState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #traffic_config{}) -> {noreply, #traffic_config{}}.
handle_cast({site_health_update, SiteId, Status}, State) ->
    %% Update site health status
    OldHealth = State#traffic_config.health,
    NewHealth = maps:put(SiteId, Status, OldHealth),

    %% Check if we need to force promotion
    case check_force_promotion(NewHealth, State#traffic_config.weights) of
        true ->
            %% Force promotion of healthiest site
            PromoteSite = get_healthiest_site(NewHealth),
            case PromoteSite of
                none ->
                    {noreply, State#traffic_config{health = NewHealth}};
                _ ->
                    gen_server:call(?SERVER, {promote_site, PromoteSite})
            end;
        false ->
            {noreply, State#traffic_config{health = NewHealth}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #traffic_config{}) -> {noreply, #traffic_config{}}.
handle_info(check_health, State) ->
    %% Check health of all sites
    NewState = check_all_sites_health(State),

    %% Schedule next health check
    erlang:send_after(?HEALTH_CHECK_INTERVAL, self(), check_health),

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #traffic_config{}) -> ok.
terminate(Reason, State) ->
    logger:warning("Traffic director terminating: ~p", [Reason]),
    erlmcp_global_monitor:deregister_traffic_director().

-spec code_change(term(), #traffic_config{}, term()) -> {ok, #traffic_config{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

validate_weights(Weights) ->
    %% Validate weight configuration
    Total = maps:fold(fun(_, W, Acc) -> Acc + W end, 0, Weights),
    Sites = maps:size(Weights),

    case Sites of
        0 ->
            invalid;
        _ ->
            case Total of
                100 ->
                    valid;
                _ ->
                    invalid
            end
    end.

check_all_sites_health(State) ->
    %% Check health of all sites
    Sites = maps:keys(State#traffic_config.health),
    HealthResults = lists:foldl(fun(SiteId, Acc) ->
        Status = check_site_health(SiteId),
        maps:put(SiteId, Status, Acc)
    end, #{}, Sites),

    %% Update health status
    NewHealth = HealthResults,

    %% Check if we need to change status
    NewStatus = case NewHealth of
        #{failed := FailedCount} when FailedCount > 0 ->
            case FailedCount / maps:size(NewHealth) >= ?FORCE_PROMOTION_THRESHOLD of
                true ->
                    emergency;
                false ->
                    failover
            end;
        _ ->
            normal
    end,

    State#traffic_config{
        health = NewHealth,
        status = NewStatus
    }.

check_site_health(SiteId) ->
    %% Check health of individual site
    case erlmcp_global_monitor:get_site_status(SiteId) of
        {ok, Status} ->
            case Status#site_status.status of
                healthy -> healthy;
                degraded -> degraded;
                failed -> failed;
                maintenance -> degraded  % Treat maintenance as degraded
            end;
        {error, _} ->
            failed
    end.

get_healthiest_site(Health) ->
    %% Get healthiest site for promotion
    HealthySites = maps:fold(fun(SiteId, Status, Acc) ->
        case Status of
            healthy ->
                [SiteId | Acc];
            _ ->
                Acc
        end
    end, [], Health),

    case HealthySites of
        [] ->
            none;
        _ ->
            %% Pick site with lowest ID (arbitrary choice)
            lists:min(HealthySites)
    end.

check_force_promotion(Health, Weights) ->
    %% Check if we need to force promotion
    HealthyCount = maps:fold(fun(_, Status, Acc) ->
        case Status of
            healthy -> Acc + 1;
            _ -> Acc
        end
    end, 0, Health),

    HealthyPercentage = HealthyCount / maps:size(Health),
    FailedPercentage = 1 - HealthyPercentage,

    FailedPercentage >= ?FORCE_PROMOTION_THRESHOLD andalso HealthyCount > 0.

redistribute_weights(Weights, DemotedSiteId, State) ->
    %% Redistribute weights after demotion
    TotalWeight = maps:fold(fun(_, W, Acc) -> Acc + W end, 0, Weights),
    OtherSites = maps:keys(Weights) -- [DemotedSiteId],

    case OtherSites of
        [] ->
            Weights;  % No other sites
        _ ->
            %% Calculate weight per site
            WeightPerSite = 100 / length(OtherSites),

            %% Redistribute weights
            lists:foldl(fun(SiteId, Acc) ->
                maps:put(SiteId, round(WeightPerSite), Acc)
            end, Weights, OtherSites)
    end.

get_default_weights() ->
    %% Get default weights for sites
    #{
        <<"dc01">> => 80,
        <<"dc02">> => 20,
        <<"dc03">> => 0
    }.

add_to_history(Config, History) ->
    %% Add configuration to history
    Entry = #{
        timestamp => erlang:system_time(millisecond),
        weights => Config,
        status => case maps:values(Config) of
                     [100 | _] -> failover;
                     _ -> normal
                 end
    },

    lists:sublist([Entry | History], ?MAX_HISTORY).

notify_weight_change(State) ->
    %% Notify about weight changes
    erlmcp_global_monitor:traffic_config_updated(
        State#traffic_config.weights,
        State#traffic_config.status
    ).

notify_promotion(SiteId, State) ->
    %% Notify about site promotion
    erlmcp_global_monitor:site_promoted(
        SiteId,
        State#traffic_config.weights
    ).

notify_demotion(SiteId, State) ->
    %% Notify about site demotion
    erlmcp_global_monitor:site_demoted(
        SiteId,
        State#traffic_config.weights
    ).