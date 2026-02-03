%% @doc High Availability Failover Manager for erlmcp
%% Implements automated failover across multiple regions
-module(erlmcp_failover_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, check_health/0, initiate_failover/1, get_status/1,
         get_active_region/0, get_failover_history/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

-record(state, {
    primary_nodes :: [node()],
    secondary_nodes :: [node()],
    tertiary_nodes :: [node()],
    health_check_interval :: pos_integer(),
    failover_threshold :: pos_integer(),
    current_active :: 'primary' | 'secondary' | 'tertiary',
    failover_history :: list(),
    monitoring_pid :: pid() | undefined,
    region_status :: map(),
    last_health_check :: integer()
}).

-define(HEALTH_CHECK_INTERVAL, 5000). % 5 seconds
-define(FAILOVER_THRESHOLD, 3). % 3 consecutive failures
-define(FAILOVER_TIMEOUT, 10000). % 10 seconds
-define(HEALTH_CHECK_TIMEOUT, 2000). % 2 seconds

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_health() ->
    gen_server:call(?MODULE, check_health).

initiate_failover(TargetRegion) ->
    gen_server:call(?MODULE, {initiate_failover, TargetRegion}).

get_status(Region) ->
    gen_server:call(?MODULE, {get_status, Region}).

get_active_region() ->
    gen_server:call(?MODULE, get_active_region).

get_failover_history() ->
    gen_server:call(?MODULE, get_failover_history).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize node configurations
    PrimaryNodes = get_primary_nodes(),
    SecondaryNodes = get_secondary_nodes(),
    TertiaryNodes = get_tertiary_nodes(),

    %% Initialize region status
    RegionStatus = #{
        primary => check_region_nodes(PrimaryNodes),
        secondary => check_region_nodes(SecondaryNodes),
        tertiary => check_region_nodes(TertiaryNodes)
    },

    State = #state{
        primary_nodes = PrimaryNodes,
        secondary_nodes = SecondaryNodes,
        tertiary_nodes = TertiaryNodes,
        health_check_interval = ?HEALTH_CHECK_INTERVAL,
        failover_threshold = ?FAILOVER_THRESHOLD,
        current_active = primary,
        failover_history = [],
        monitoring_pid = undefined,
        region_status = RegionStatus,
        last_health_check = 0
    },

    %% Start health monitoring process
    MonitoringPid = spawn_monitor_health_check(State),

    {ok, State#state{monitoring_pid = MonitoringPid}}.

handle_call(check_health, _From, State) ->
    {Reply, NewState} = perform_health_check(State),
    {reply, Reply, NewState};

handle_call({initiate_failover, TargetRegion}, _From, State) ->
    case validate_failover_request(TargetRegion, State) of
        {ok, NewState} ->
            Reply = execute_failover(TargetRegion, State),
            {reply, Reply, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_status, Region}, _From, State) ->
    Reply = get_region_status(Region, State),
    {reply, Reply, State};

handle_call(get_active_region, _From, State) ->
    {reply, {ok, State#state.current_active}, State};

handle_call(get_failover_history, _From, State) ->
    {reply, {ok, State#state.failover_history}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    logger:warning("Health check process ~p crashed: ~p", [Pid, Reason]),
    %% Restart health check process
    NewMonitoringPid = spawn_monitor_health_check(State),
    {noreply, State#state{monitoring_pid = NewMonitoringPid}};

handle_info(health_check_interval, State) ->
    NewState = perform_health_check_periodic(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

get_primary_nodes() ->
    %% Get configured primary nodes
    case application:get_env(erlmcp, primary_nodes) of
        undefined -> ['us-east-1-node1@10.0.1.10', 'us-east-1-node2@10.0.1.11', 'us-east-1-node3@10.0.1.12'];
        Nodes -> Nodes
    end.

get_secondary_nodes() ->
    case application:get_env(erlmcp, secondary_nodes) of
        undefined -> ['eu-west-1-node1@10.0.2.10', 'eu-west-1-node2@10.0.2.11', 'eu-west-1-node3@10.0.2.12'];
        Nodes -> Nodes
    end.

get_tertiary_nodes() ->
    case application:get_env(erlmcp, tertiary_nodes) of
        undefined -> ['ap-southeast-1-node1@10.0.3.10', 'ap-southeast-1-node2@10.0.3.11'];
        Nodes -> Nodes
    end.

spawn_monitor_health_check(State) ->
    Pid = spawn(fun() -> health_check_loop(State) end),
    erlang:monitor(process, Pid),
    Pid.

health_check_loop(State) ->
    %% Perform health check
    NewState = perform_health_check_periodic(State),

    %% Schedule next check
    timer:sleep(State#state.health_check_interval),
    health_check_loop(NewState).

perform_health_check_periodic(State) ->
    Now = erlang:system_time(millisecond),

    %% Only perform check if enough time has passed
    case Now - State#state.last_health_check >= State#state.health_check_interval of
        true ->
            perform_health_check(State);
        false ->
            State
    end.

perform_health_check(State) ->
    Now = erlang:system_time(millisecond),

    %% Check primary region
    PrimaryStatus = check_region_nodes(State#state.primary_nodes),

    %% Check secondary region
    SecondaryStatus = check_region_nodes(State#state.secondary_nodes),

    %% Check tertiary region
    TertiaryStatus = check_region_nodes(State#state.tertiary_nodes),

    %% Update region status
    RegionStatus = #{
        primary => PrimaryStatus,
        secondary => SecondaryStatus,
        tertiary => TertiaryStatus
    },

    %% Check if failover is needed
    NewState = State#state{
        region_status = RegionStatus,
        last_health_check = Now
    },

    case should_initiate_failover(NewState) of
        {true, TargetRegion} ->
            logger:info("Initiating failover to ~p region", [TargetRegion]),
            execute_failover(TargetRegion, NewState),
            NewState#state{current_active = TargetRegion};
        false ->
            NewState
    end.

check_region_nodes(Nodes) ->
    Healthy = [Node || Node <- Nodes, is_node_healthy(Node)],
    Unhealthy = Nodes -- Healthy,
    #{
        healthy => Healthy,
        unhealthy => Unhealthy,
        total => length(Nodes),
        healthy_count => length(Healthy)
    }.

is_node_healthy(Node) ->
    %% Check if node is reachable and responsive
    case net_adm:ping(Node) of
        pong ->
            %% Check health endpoint
            case rpc:call(Node, erlmcp_health_check, status, [], ?HEALTH_CHECK_TIMEOUT) of
                ok -> true;
                _ -> false
            end;
        _ ->
            false
    end.

should_initiate_failover(State) ->
    PrimaryStatus = maps:get(primary, State#state.region_status),
    SecondaryStatus = maps:get(secondary, State#state.region_status),
    TertiaryStatus = maps:get(tertiary, State#state.region_status),

    case State#state.current_active of
        primary ->
            %% Check if primary region has failed
            case maps:get(healthy_count, PrimaryStatus) >= State#state.failover_threshold of
                true -> false; % Primary is healthy
                false ->
                    %% Check if secondary has enough healthy nodes
                    case maps:get(healthy_count, SecondaryStatus) >= State#state.failover_threshold of
                        true -> {true, secondary};
                        false ->
                            %% Check tertiary as last resort
                            case maps:get(healthy_count, TertiaryStatus) >= 1 of
                                true -> {true, tertiary};
                                false -> false
                            end
                    end
            end;
        secondary ->
            %% Check if secondary has failed
            case maps:get(healthy_count, SecondaryStatus) >= State#state.failover_threshold of
                true -> false; % Secondary is healthy
                false ->
                    %% Check if primary has recovered
                    case maps:get(healthy_count, PrimaryStatus) >= State#state.failover_threshold of
                        true -> {true, primary};
                        false ->
                            %% Check tertiary
                            case maps:get(healthy_count, TertiaryStatus) >= 1 of
                                true -> {true, tertiary};
                                false -> false
                            end
                    end
            end;
        tertiary ->
            %% Check if tertiary has failed
            case maps:get(healthy_count, TertiaryStatus) >= 1 of
                true -> false; % Tertiary is healthy
                false ->
                    %% Check if primary or secondary has recovered
                    PrimaryHealthy = maps:get(healthy_count, PrimaryStatus) >= State#state.failover_threshold,
                    SecondaryHealthy = maps:get(healthy_count, SecondaryStatus) >= State#state.failover_threshold,
                    case PrimaryHealthy of
                        true -> {true, primary};
                        false ->
                            case SecondaryHealthy of
                                true -> {true, secondary};
                                false -> false
                            end
                    end
            end
    end.

validate_failover_request(TargetRegion, State) ->
    %% Check if target region is healthy enough
    RegionStatus = maps:get(TargetRegion, State#state.region_status),
    HealthyCount = maps:get(healthy_count, RegionStatus),
    FailoverThreshold = State#state.failover_threshold,

    case TargetRegion of
        primary when HealthyCount >= FailoverThreshold ->
            {ok, State};
        secondary when HealthyCount >= FailoverThreshold ->
            {ok, State};
        tertiary when HealthyCount >= 1 ->
            {ok, State};
        _ ->
            {error, {insufficient_healthy_nodes, TargetRegion}}
    end.

execute_failover(TargetRegion, State) ->
    %% Log failover event
    FailoverEvent = #{
        timestamp => erlang:system_time(millisecond),
        from => State#state.current_active,
        to => TargetRegion,
        reason => manual,
        status => in_progress
    },

    %% Update DNS to point to new active region
    case update_dns_target(TargetRegion) of
        ok ->
            %% Notify all nodes of the failover
            notify_failover(TargetRegion, State),

            %% Add to failover history
            NewHistory = [FailoverEvent | State#state.failover_history],

            logger:info("Failover to ~p region completed successfully", [TargetRegion]),

            %% Return success
            {ok, NewHistory};
        Error ->
            logger:error("Failed to update DNS for failover: ~p", [Error]),
            {error, dns_update_failed}
    end.

update_dns_target(Region) ->
    %% Implementation for updating DNS records
    %% This would typically involve calling AWS Route 53 or similar DNS service
    case Region of
        primary ->
            %% Update to point to primary region IPs
            ok;
        secondary ->
            %% Update to point to secondary region IPs
            ok;
        tertiary ->
            %% Update to point to tertiary region IPs
            ok
    end.

notify_failover(Region, _State) ->
    %% Notify all nodes in the cluster about the failover
    Nodes = get_nodes_for_region(Region),

    %% Notify session manager to switch active region
    [begin
        rpc:call(Node, erlmcp_session_manager, set_active_region, [Region]),
        rpc:call(Node, erlmcp_shard_manager, set_active_region, [Region])
    end || Node <- Nodes].

get_nodes_for_region(primary) ->
    get_primary_nodes();
get_nodes_for_region(secondary) ->
    get_secondary_nodes();
get_nodes_for_region(tertiary) ->
    get_tertiary_nodes().

get_region_status(Region, State) ->
    case maps:get(Region, State#state.region_status, undefined) of
        undefined -> {error, unknown_region};
        Status -> {ok, Status}
    end.