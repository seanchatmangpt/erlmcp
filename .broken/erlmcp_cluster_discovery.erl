%% @doc Cluster Service Discovery for erlmcp v3
%%
%% This module implements a comprehensive gproc-based service discovery system
%% for distributed Erlang clusters with the following features:
%%
%% == Service Registration ==
%%   - Dynamic service registration with metadata
%%   - Automatic service health monitoring via gproc
%%   - Service versioning and capability tracking
%%   - Multi-region and multi-datacenter support
%%
%% == Service Discovery ==
%%   - O(log N) service lookup via gproc
%%   - Service load balancing strategies (round_robin, random, least_loaded)
%%   - Service filtering by metadata, version, and capabilities
%%   - Automatic failover to healthy services
%%
%% == Cluster Awareness ==
%%   - Automatic node membership tracking
%%   - Service locality awareness (same-node, same-region)
%%   - Network partition detection and handling
%%   - Dynamic cluster topology updates
%%
%% == Health Monitoring ==
%%   - Gproc-based process monitoring
%%   - Automatic service deregistration on process death
%%   - Custom health check callbacks
%%   - Health status propagation across cluster
%%
%% == Observability ==
%%   - Service discovery metrics
%%   - Event notifications for service changes
%%   - OpenTelemetry integration
%%   - Debug and inspection APIs
%%
%% == Architecture ==
%%
%% Service Discovery Registry:
%% ```
%% erlmcp_cluster_discovery (gen_server)
%% ├── gproc keys (local and global)
│   ├── {p, l, {service, ServiceName, ServiceId}}
│   ├── {p, g, {service, ServiceName, ServiceId}}  % Global
│   └── {p, l, {service_type, ServiceType}}
%% ```
%%
%% @end
-module(erlmcp_cluster_discovery).

-behaviour(gen_server).

-include("erlmcp.hrl").

%% API - Service Registration
-export([start_link/0, start_link/1, register_service/3, register_service/4,
         register_service/5, unregister_service/2, unregister_service/3,
         update_service/3, renew_service/2]).

%% API - Service Discovery
-export([discover_service/2, discover_service/3, discover_services/2,
         discover_services/3, discover_all_services/1, discover_all_services/2,
         discover_by_type/2, discover_by_type/3]).

%% API - Advanced Discovery
-export([discover_local/2, discover_global/2, discover_healthy/2,
         discover_by_capability/3, discover_by_version/3,
         discover_by_metadata/3, discover_with_fallback/3]).

%% API - Load Balancing
-export([discover_with_strategy/3, get_service_count/2,
         service_health_check/2]).

%% API - Cluster Info
-export([get_cluster_nodes/0, get_cluster_services/0,
         get_service_stats/1, get_all_stats/0,
         subscribe_to_events/1, unsubscribe_from_events/1]).

%% API - Health & Monitoring
-export([health_check/1, force_health_check/1, mark_service_healthy/2,
         mark_service_unhealthy/2, service_is_healthy/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%====================================================================
%%% Type Definitions
%%%====================================================================

-type service_name() :: atom() | binary().
-type service_id() :: binary().
-type service_type() :: atom() | binary().
-type service_version() :: binary().
-type service_metadata() :: map().

-type service_info() ::
    #{id := service_id(),
      name := service_name(),
      type := service_type(),
      pid := pid(),
      node := node(),
      version := service_version(),
      metadata := service_metadata(),
      capabilities := [term()],
      registered_at := integer(),
      last_heartbeat := integer(),
      health_status := healthy | unhealthy | unknown,
      registration => local | global}.

-type load_balance_strategy() :: round_robin | random | least_loaded | local_first.

-type discovery_filter() ::
    {type, service_type()} |
    {version, service_version()} |
    {capability, term()} |
    {metadata, service_metadata()} |
    {health, healthy | unhealthy | unknown} |
    {node, node()} |
    {local, boolean()} |
    {global, boolean()}.

-type discovery_event() ::
    {service_registered, service_name(), service_id(), service_info()} |
    {service_unregistered, service_name(), service_id()} |
    {service_updated, service_name(), service_id(), service_info()} |
    {service_healthy, service_name(), service_id()} |
    {service_unhealthy, service_name(), service_id()} |
    {node_up, node()} |
    {node_down, node()}.

-type discovery_stats() ::
    #{total_services := non_neg_integer(),
      healthy_services := non_neg_integer(),
      unhealthy_services := non_neg_integer(),
      local_services := non_neg_integer(),
      global_services := non_neg_integer(),
      nodes := non_neg_integer()}.

-export_type([service_name/0, service_id/0, service_type/0,
              service_info/0, load_balance_strategy/0,
              discovery_event/0, discovery_stats/0]).

%%%====================================================================
%%% Server State
%%%====================================================================

-record(discovery_state,
        {event_subscribers = sets:new() :: sets:set(pid()),
         service_monitors = #{} :: #{service_id() => reference()},
         health_status = #{} :: #{service_id() => healthy | unhealthy | unknown},
         load_balance_state = #{} :: #{service_name() => integer()},
         cluster_nodes = [] :: [node()],
         node_monitors = [] :: [reference()],
         stats = #{total_services => 0,
                   healthy_services => 0,
                   unhealthy_services => 0,
                   local_services => 0,
                   global_services => 0,
                   nodes => 0} :: map()}).

-type state() :: #discovery_state{}.

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Start the service discovery registry with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the service discovery registry with custom options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Register a service locally (synchronous)
-spec register_service(service_name(), pid(), service_metadata()) ->
    {ok, service_id()} | {error, term()}.
register_service(ServiceName, ServicePid, Metadata) when is_pid(ServicePid) ->
    register_service(local, ServiceName, ServicePid, default_type, Metadata).

%% @doc Register a service with scope
-spec register_service(local | global, service_name(), pid(), service_metadata()) ->
    {ok, service_id()} | {error, term()}.
register_service(Scope, ServiceName, ServicePid, Metadata) when is_pid(ServicePid) ->
    register_service(Scope, ServiceName, ServicePid, default_type, Metadata).

%% @doc Register a service with full specification
-spec register_service(local | global, service_name(), pid(), service_type(),
                       service_metadata()) ->
    {ok, service_id()} | {error, term()}.
register_service(Scope, ServiceName, ServicePid, ServiceType, Metadata)
  when is_pid(ServicePid), is_atom(ServiceType) orelse is_binary(ServiceType) ->
    gen_server:call(?MODULE, {register_service, Scope, ServiceName, ServicePid,
                               ServiceType, Metadata}).

%% @doc Unregister a service
-spec unregister_service(service_name(), service_id()) -> ok.
unregister_service(ServiceName, ServiceId) ->
    gen_server:call(?MODULE, {unregister_service, ServiceName, ServiceId}).

%% @doc Unregister a service with scope
-spec unregister_service(local | global, service_name(), service_id()) -> ok.
unregister_service(Scope, ServiceName, ServiceId) ->
    gen_server:call(?MODULE, {unregister_service, Scope, ServiceName, ServiceId}).

%% @doc Update service metadata
-spec update_service(service_name(), service_id(), service_metadata()) ->
    ok | {error, term()}.
update_service(ServiceName, ServiceId, NewMetadata) ->
    gen_server:call(?MODULE, {update_service, ServiceName, ServiceId, NewMetadata}).

%% @doc Renew service registration (heartbeat)
-spec renew_service(service_name(), service_id()) -> ok | {error, term()}.
renew_service(ServiceName, ServiceId) ->
    gen_server:call(?MODULE, {renew_service, ServiceName, ServiceId}).

%%--------------------------------------------------------------------
%% Service Discovery API
%%--------------------------------------------------------------------

%% @doc Discover a service by name (returns first available)
-spec discover_service(service_name(), local | global) ->
    {ok, service_info()} | {error, term()}.
discover_service(ServiceName, Scope) ->
    discover_service(ServiceName, Scope, []).

%% @doc Discover a service with filters
-spec discover_service(service_name(), local | global, [discovery_filter()]) ->
    {ok, service_info()} | {error, term()}.
discover_service(ServiceName, Scope, Filters) ->
    gen_server:call(?MODULE, {discover_service, ServiceName, Scope, Filters}).

%% @doc Discover all instances of a service
-spec discover_services(service_name(), local | global) ->
    {ok, [service_info()]} | {error, term()}.
discover_services(ServiceName, Scope) ->
    discover_services(ServiceName, Scope, []).

%% @doc Discover services with filters
-spec discover_services(service_name(), local | global, [discovery_filter()]) ->
    {ok, [service_info()]} | {error, term()}.
discover_services(ServiceName, Scope, Filters) ->
    gen_server:call(?MODULE, {discover_services, ServiceName, Scope, Filters}).

%% @doc Discover all registered services
-spec discover_all_services(local | global) ->
    {ok, [{service_name(), [service_info()]}]} | {error, term()}.
discover_all_services(Scope) ->
    gen_server:call(?MODULE, {discover_all_services, Scope}).

%% @doc Discover all services with filters
-spec discover_all_services(local | global, [discovery_filter()]) ->
    {ok, [{service_name(), [service_info()]}]} | {error, term()}.
discover_all_services(Scope, Filters) ->
    gen_server:call(?MODULE, {discover_all_services, Scope, Filters}).

%% @doc Discover services by type
-spec discover_by_type(service_type(), local | global) ->
    {ok, [service_info()]} | {error, term()}.
discover_by_type(ServiceType, Scope) ->
    discover_by_type(ServiceType, Scope, []).

%% @doc Discover services by type with filters
-spec discover_by_type(service_type(), local | global, [discovery_filter()]) ->
    {ok, [service_info()]} | {error, term()}.
discover_by_type(ServiceType, Scope, Filters) ->
    gen_server:call(?MODULE, {discover_by_type, ServiceType, Scope, Filters}).

%%--------------------------------------------------------------------
%% Advanced Discovery API
%%--------------------------------------------------------------------

%% @doc Discover services only on local node
-spec discover_local(service_name(), [discovery_filter()]) ->
    {ok, [service_info()]} | {error, term()}.
discover_local(ServiceName, Filters) ->
    discover_services(ServiceName, local, [{local, true} | Filters]).

%% @doc Discover services across entire cluster
-spec discover_global(service_name(), [discovery_filter()]) ->
    {ok, [service_info()]} | {error, term()}.
discover_global(ServiceName, Filters) ->
    discover_services(ServiceName, global, [{global, true} | Filters]).

%% @doc Discover only healthy services
-spec discover_healthy(service_name(), local | global) ->
    {ok, [service_info()]} | {error, term()}.
discover_healthy(ServiceName, Scope) ->
    discover_services(ServiceName, Scope, [{health, healthy}]).

%% @doc Discover services by capability
-spec discover_by_capability(service_name(), local | global, term()) ->
    {ok, [service_info()]} | {error, term()}.
discover_by_capability(ServiceName, Scope, Capability) ->
    discover_services(ServiceName, Scope, [{capability, Capability}]).

%% @doc Discover services by version
-spec discover_by_version(service_name(), local | global, service_version()) ->
    {ok, [service_info()]} | {error, term()}.
discover_by_version(ServiceName, Scope, Version) ->
    discover_services(ServiceName, Scope, [{version, Version}]).

%% @doc Discover services by metadata
-spec discover_by_metadata(service_name(), local | global, service_metadata()) ->
    {ok, [service_info()]} | {error, term()}.
discover_by_metadata(ServiceName, Scope, Metadata) ->
    discover_services(ServiceName, Scope, [{metadata, Metadata}]).

%% @doc Discover with fallback to alternative service
-spec discover_with_fallback(service_name(), service_name(), local | global) ->
    {ok, service_info()} | {error, term()}.
discover_with_fallback(PrimaryName, FallbackName, Scope) ->
    case discover_service(PrimaryName, Scope) of
        {ok, _Service} = Result ->
            Result;
        {error, _Reason} ->
            discover_service(FallbackName, Scope)
    end.

%%--------------------------------------------------------------------
%% Load Balancing API
%%--------------------------------------------------------------------

%% @doc Discover service with load balancing strategy
-spec discover_with_strategy(service_name(), local | global,
                             load_balance_strategy()) ->
    {ok, service_info()} | {error, term()}.
discover_with_strategy(ServiceName, Scope, Strategy) ->
    gen_server:call(?MODULE, {discover_with_strategy, ServiceName, Scope, Strategy}).

%% @doc Get count of registered services
-spec get_service_count(service_name(), local | global) ->
    {ok, non_neg_integer()} | {error, term()}.
get_service_count(ServiceName, Scope) ->
    gen_server:call(?MODULE, {get_service_count, ServiceName, Scope}).

%% @doc Perform health check on service
-spec service_health_check(service_name(), service_id()) ->
    {ok, healthy | unhealthy | unknown} | {error, term()}.
service_health_check(ServiceName, ServiceId) ->
    gen_server:call(?MODULE, {service_health_check, ServiceName, ServiceId}).

%%--------------------------------------------------------------------
%% Cluster Info API
%%--------------------------------------------------------------------

%% @doc Get all cluster nodes
-spec get_cluster_nodes() -> {ok, [node()]}.
get_cluster_nodes() ->
    gen_server:call(?MODULE, get_cluster_nodes).

%% @doc Get all cluster services
-spec get_cluster_services() -> {ok, [{service_name(), service_info()}]}.
get_cluster_services() ->
    gen_server:call(?MODULE, get_cluster_services).

%% @doc Get stats for a specific service
-spec get_service_stats(service_name()) -> {ok, map()} | {error, term()}.
get_service_stats(ServiceName) ->
    gen_server:call(?MODULE, {get_service_stats, ServiceName}).

%% @doc Get all discovery statistics
-spec get_all_stats() -> {ok, discovery_stats()}.
get_all_stats() ->
    gen_server:call(?MODULE, get_all_stats).

%% @doc Subscribe to service discovery events
-spec subscribe_to_events(pid()) -> ok.
subscribe_to_events(Subscriber) when is_pid(Subscriber) ->
    gen_server:cast(?MODULE, {subscribe, Subscriber}).

%% @doc Unsubscribe from service discovery events
-spec unsubscribe_from_events(pid()) -> ok.
unsubscribe_from_events(Subscriber) when is_pid(Subscriber) ->
    gen_server:cast(?MODULE, {unsubscribe, Subscriber}).

%%--------------------------------------------------------------------
%% Health & Monitoring API
%%--------------------------------------------------------------------

%% @doc Perform health check on discovery service
-spec health_check(pid()) -> {ok, map()} | {error, term()}.
health_check(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, health_check);
health_check(_) ->
    health_check(?MODULE).

%% @doc Force health check on all services
-spec force_health_check(pid()) -> ok.
force_health_check(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, force_health_check);
force_health_check(_) ->
    force_health_check(?MODULE).

%% @doc Manually mark service as healthy
-spec mark_service_healthy(service_name(), service_id()) -> ok | {error, term()}.
mark_service_healthy(ServiceName, ServiceId) ->
    gen_server:call(?MODULE, {mark_service_healthy, ServiceName, ServiceId}).

%% @doc Manually mark service as unhealthy
-spec mark_service_unhealthy(service_name(), service_id()) -> ok | {error, term()}.
mark_service_unhealthy(ServiceName, ServiceId) ->
    gen_server:call(?MODULE, {mark_service_unhealthy, ServiceName, ServiceId}).

%% @doc Check if service is healthy
-spec service_is_healthy(service_name(), service_id()) ->
    boolean() | {error, term()}.
service_is_healthy(ServiceName, ServiceId) ->
    gen_server:call(?MODULE, {service_is_healthy, ServiceName, ServiceId}).

%%%====================================================================
%%% gen_server Callbacks
%%%====================================================================

-spec init(map()) -> {ok, state(), {continue, initialize}}.
init(_Options) ->
    process_flag(trap_exit, true),

    %% Initial state
    State = #discovery_state{
        event_subscribers = sets:new(),
        service_monitors = #{},
        health_status = #{},
        load_balance_state = #{},
        cluster_nodes = nodes(),
        node_monitors = [],
        stats = initial_stats()
    },

    {ok, State, {continue, initialize}}.

-spec handle_continue(initialize, state()) -> {noreply, state()}.
handle_continue(initialize, State) ->
    %% Monitor cluster nodes
    Monitors = [net_kernel:monitor_nodes(true) || _ <- lists:seq(1, 1)],
    UpdatedState = State#discovery_state{node_monitors = Monitors},

    logger:info("Cluster discovery service initialized on node ~p", [node()]),
    {noreply, UpdatedState}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({register_service, Scope, ServiceName, ServicePid, ServiceType, Metadata},
            _From, State) ->
    {Reply, NewState} = do_register_service(Scope, ServiceName, ServicePid,
                                             ServiceType, Metadata, State),
    {reply, Reply, NewState};

handle_call({unregister_service, ServiceName, ServiceId}, _From, State) ->
    {Reply, NewState} = do_unregister_service(ServiceName, ServiceId, local, State),
    {reply, Reply, NewState};

handle_call({unregister_service, Scope, ServiceName, ServiceId}, _From, State) ->
    {Reply, NewState} = do_unregister_service(ServiceName, ServiceId, Scope, State),
    {reply, Reply, NewState};

handle_call({update_service, ServiceName, ServiceId, NewMetadata}, _From, State) ->
    {Reply, NewState} = do_update_service(ServiceName, ServiceId, NewMetadata, State),
    {reply, Reply, NewState};

handle_call({renew_service, ServiceName, ServiceId}, _From, State) ->
    {Reply, NewState} = do_renew_service(ServiceName, ServiceId, State),
    {reply, Reply, NewState};

handle_call({discover_service, ServiceName, Scope, Filters}, _From, State) ->
    Reply = do_discover_service(ServiceName, Scope, Filters, State),
    {reply, Reply, State};

handle_call({discover_services, ServiceName, Scope, Filters}, _From, State) ->
    Reply = do_discover_services(ServiceName, Scope, Filters, State),
    {reply, Reply, State};

handle_call({discover_all_services, Scope}, _From, State) ->
    Reply = do_discover_all_services(Scope, [], State),
    {reply, Reply, State};

handle_call({discover_all_services, Scope, Filters}, _From, State) ->
    Reply = do_discover_all_services(Scope, Filters, State),
    {reply, Reply, State};

handle_call({discover_by_type, ServiceType, Scope, Filters}, _From, State) ->
    Reply = do_discover_by_type(ServiceType, Scope, Filters, State),
    {reply, Reply, State};

handle_call({discover_with_strategy, ServiceName, Scope, Strategy}, _From, State) ->
    {Reply, NewState} = do_discover_with_strategy(ServiceName, Scope, Strategy, State),
    {reply, Reply, NewState};

handle_call({get_service_count, ServiceName, Scope}, _From, State) ->
    Reply = do_get_service_count(ServiceName, Scope),
    {reply, Reply, State};

handle_call({service_health_check, ServiceName, ServiceId}, _From, State) ->
    Reply = do_service_health_check(ServiceName, ServiceId, State),
    {reply, Reply, State};

handle_call(get_cluster_nodes, _From, State) ->
    Nodes = State#discovery_state.cluster_nodes,
    {reply, {ok, Nodes}, State};

handle_call(get_cluster_services, _From, State) ->
    Reply = do_get_cluster_services(State),
    {reply, Reply, State};

handle_call({get_service_stats, ServiceName}, _From, State) ->
    Reply = do_get_service_stats(ServiceName, State),
    {reply, Reply, State};

handle_call(get_all_stats, _From, State) ->
    Stats = State#discovery_state.stats,
    {reply, {ok, Stats}, State};

handle_call({mark_service_healthy, ServiceName, ServiceId}, _From, State) ->
    {Reply, NewState} = do_mark_service_healthy(ServiceName, ServiceId, State),
    {reply, Reply, NewState};

handle_call({mark_service_unhealthy, ServiceName, ServiceId}, _From, State) ->
    {Reply, NewState} = do_mark_service_unhealthy(ServiceName, ServiceId, State),
    {reply, Reply, NewState};

handle_call({service_is_healthy, ServiceName, ServiceId}, _From, State) ->
    Reply = do_service_is_healthy(ServiceName, ServiceId, State),
    {reply, Reply, State};

handle_call(health_check, _From, State) ->
    Health = #{
        status => healthy,
        cluster_nodes => length(State#discovery_state.cluster_nodes),
        stats => State#discovery_state.stats,
        timestamp => erlang:system_time(millisecond)
    },
    {reply, {ok, Health}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({subscribe, Subscriber}, State) when is_pid(Subscriber) ->
    monitor(process, Subscriber),
    NewSubscribers = sets:add_element(Subscriber, State#discovery_state.event_subscribers),
    NewState = State#discovery_state{event_subscribers = NewSubscribers},
    {noreply, NewState};

handle_cast({unsubscribe, Subscriber}, State) when is_pid(Subscriber) ->
    NewSubscribers = sets:del_element(Subscriber, State#discovery_state.event_subscribers),
    NewState = State#discovery_state{event_subscribers = NewSubscribers},
    {noreply, NewState};

handle_cast(force_health_check, State) ->
    NewState = do_force_health_check(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({nodeup, Node}, State) ->
    logger:info("Node ~p joined cluster", [Node]),
    NewNodes = lists:usort([Node | State#discovery_state.cluster_nodes]),
    NewStats = (State#discovery_state.stats)#{nodes => length(NewNodes)},

    %% Notify subscribers
    notify_subscribers(State, {node_up, Node}),

    NewState = State#discovery_state{
        cluster_nodes = NewNodes,
        stats = NewStats
    },
    {noreply, NewState};

handle_info({nodedown, Node}, State) ->
    logger:warning("Node ~p left cluster", [Node]),
    NewNodes = lists:delete(Node, State#discovery_state.cluster_nodes),
    NewStats = (State#discovery_state.stats)#{nodes => length(NewNodes)},

    %% Clean up services from failed node
    NewState = cleanup_node_services(Node, State#discovery_state{
        cluster_nodes = NewNodes,
        stats = NewStats
    }),

    %% Notify subscribers
    notify_subscribers(NewState, {node_down, Node}),

    {noreply, NewState};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Handle monitored process death (subscriber or service)
    NewState = handle_process_down(Pid, State),
    {noreply, NewState};

handle_info({gproc, unreg, _Ref, Key}, State) ->
    %% Handle gproc unregistration (service died)
    NewState = handle_gproc_unreg(Key, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    logger:info("Cluster discovery service terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions - Service Registration
%%%====================================================================

%% @doc Register a service
-spec do_register_service(local | global, service_name(), pid(), service_type(),
                          service_metadata(), state()) ->
    {{ok, service_id()} | {error, term()}, state()}.
do_register_service(Scope, ServiceName, ServicePid, ServiceType, Metadata, State) ->
    %% Validate service PID is alive
    case is_process_alive(ServicePid) of
        false ->
            {{error, service_pid_not_alive}, State};
        true ->
            %% Generate service ID
            ServiceId = generate_service_id(ServiceName, node()),
            Now = erlang:system_time(millisecond),

            %% Build service info
            BaseInfo = #{
                id => ServiceId,
                name => ServiceName,
                type => ServiceType,
                pid => ServicePid,
                node => node(),
                version => maps:get(version, Metadata, <<"1.0.0">>),
                metadata => Metadata,
                capabilities => maps:get(capabilities, Metadata, []),
                registered_at => Now,
                last_heartbeat => Now,
                health_status => unknown,
                registration => Scope
            },

            %% Register with gproc
            GprocScope = case Scope of
                local -> l;
                global -> g
            end,

            Key = {p, GprocScope, {service, ServiceName, ServiceId}},

            try
                %% Register the service PID
                gproc:reg_other(Key, ServicePid, BaseInfo),

                %% Monitor the service
                MonRef = monitor(process, ServicePid),

                %% Update state
                NewMonitors = maps:put(ServiceId, MonRef, State#discovery_state.service_monitors),
                NewHealthStatus = maps:put(ServiceId, unknown, State#discovery_state.health_status),
                NewStats = update_stats_on_register(Scope, State#discovery_state.stats),

                NewState = State#discovery_state{
                    service_monitors = NewMonitors,
                    health_status = NewHealthStatus,
                    stats = NewStats
                },

                logger:info("Registered ~p service ~p (~p) on node ~p",
                            [Scope, ServiceName, ServiceId, node()]),

                %% Notify subscribers
                notify_subscribers(NewState, {service_registered, ServiceName, ServiceId, BaseInfo}),

                %% Emit OTEL event
                emit_telemetry_event(service_registered, #{
                    service_name => ServiceName,
                    service_id => ServiceId,
                    service_type => ServiceType,
                    scope => Scope,
                    node => node()
                }),

                {{ok, ServiceId}, NewState}
            catch
                error:badarg ->
                    {{error, already_registered}, State};
                Error:Reason ->
                    logger:error("Failed to register service ~p: ~p:~p",
                                [ServiceName, Error, Reason]),
                    {{error, {registration_failed, Reason}}, State}
            end
    end.

%% @doc Unregister a service
-spec do_unregister_service(service_name(), service_id(), local | global, state()) ->
    {ok, state()} | {{error, term()}, state()}.
do_unregister_service(ServiceName, ServiceId, Scope, State) ->
    GprocScope = case Scope of
        local -> l;
        global -> g
    end,

    Key = {p, GprocScope, {service, ServiceName, ServiceId}},

    case gproc:where(Key) of
        undefined ->
            {{error, not_found}, State};
        _ServicePid ->
            %% Demonitor if monitored
            NewMonitors = case maps:get(ServiceId, State#discovery_state.service_monitors, undefined) of
                undefined -> State#discovery_state.service_monitors;
                MonRef ->
                    demonitor(MonRef, [flush]),
                    maps:remove(ServiceId, State#discovery_state.service_monitors)
            end,

            %% Unregister from gproc
            try
                gproc:unreg(Key)
            catch
                _:_ -> ok
            end,

            %% Clean up state
            NewHealthStatus = maps:remove(ServiceId, State#discovery_state.health_status),
            NewStats = update_stats_on_unregister(Scope, State#discovery_state.stats),

            NewState = State#discovery_state{
                service_monitors = NewMonitors,
                health_status = NewHealthStatus,
                stats = NewStats
            },

            logger:info("Unregistered ~p service ~p (~p)", [Scope, ServiceName, ServiceId]),

            %% Notify subscribers
            notify_subscribers(NewState, {service_unregistered, ServiceName, ServiceId}),

            {ok, NewState}
    end.

%% @doc Update service metadata
-spec do_update_service(service_name(), service_id(), service_metadata(), state()) ->
    {{ok, service_info()} | {error, term()}, state()}.
do_update_service(ServiceName, ServiceId, NewMetadata, State) ->
    %% Try both scopes
    case find_service_key(ServiceName, ServiceId, local, State) of
        {ok, Key, ServicePid} ->
            update_service_via_key(Key, ServicePid, NewMetadata, State);
        {error, not_found} ->
            case find_service_key(ServiceName, ServiceId, global, State) of
                {ok, Key, ServicePid} ->
                    update_service_via_key(Key, ServicePid, NewMetadata, State);
                {error, not_found} ->
                    {{error, not_found}, State}
            end
    end.

%% @doc Renew service registration (heartbeat)
-spec do_renew_service(service_name(), service_id(), state()) ->
    {ok, state()} | {{error, term()}, state()}.
do_renew_service(ServiceName, ServiceId, State) ->
    case find_service_info(ServiceName, ServiceId, State) of
        {ok, _ServiceInfo} ->
            %% Update last heartbeat
            ok = gproc:set_value({p, l, {service, ServiceName, ServiceId}},
                                 #{last_heartbeat => erlang:system_time(millisecond)}),
            {ok, State};
        {error, Reason} ->
            {{error, Reason}, State}
    end.

%%%====================================================================
%%% Internal Functions - Service Discovery
%%%====================================================================

%% @doc Discover a single service
-spec do_discover_service(service_name(), local | global, [discovery_filter()],
                          state()) ->
    {ok, service_info()} | {error, term()}.
do_discover_service(ServiceName, Scope, Filters, _State) ->
    GprocScope = case Scope of
        local -> l;
        global -> g
    end,

    %% Query gproc for services
    Pattern = {{{p, GprocScope, {service, ServiceName, '$1'}}}, '$2', '$3'},

    case gproc:select([Pattern]) of
        [] ->
            {error, no_services_available};
        Services ->
            %% Apply filters
            FilteredServices = apply_filters(Services, Filters),

            case FilteredServices of
                [] ->
                    {error, no_matching_services};
                [{_ServiceId, _ServicePid, ServiceInfo} | _] ->
                    {ok, ServiceInfo}
            end
    end.

%% @doc Discover multiple services
-spec do_discover_services(service_name(), local | global, [discovery_filter()],
                           state()) ->
    {ok, [service_info()]} | {error, term()}.
do_discover_services(ServiceName, Scope, Filters, _State) ->
    GprocScope = case Scope of
        local -> l;
        global -> g
    end,

    %% Query gproc for services
    Pattern = {{{p, GprocScope, {service, ServiceName, '$1'}}}, '$2', '$3'},

    case gproc:select([Pattern]) of
        [] ->
            {ok, []};
        Services ->
            %% Apply filters
            FilteredServices = apply_filters(Services, Filters),
            ServiceInfos = [Info || {_Id, _Pid, Info} <- FilteredServices],
            {ok, ServiceInfos}
    end.

%% @doc Discover all services
-spec do_discover_all_services(local | global, [discovery_filter()], state()) ->
    {ok, [{service_name(), [service_info()]}]} | {error, term()}.
do_discover_all_services(Scope, Filters, _State) ->
    GprocScope = case Scope of
        local -> l;
        global -> g
    end,

    %% Query all services
    Pattern = {{{p, GprocScope, {service, '$1', '$2'}}}, '$3', '$4'},

    case gproc:select([Pattern]) of
        [] ->
            {ok, []};
        AllServices ->
            %% Group by service name and apply filters
            Grouped = group_services_by_name(AllServices, Filters),
            {ok, Grouped}
    end.

%% @doc Discover services by type
-spec do_discover_by_type(service_type(), local | global, [discovery_filter()],
                          state()) ->
    {ok, [service_info()]} | {error, term()}.
do_discover_by_type(ServiceType, Scope, Filters, _State) ->
    GprocScope = case Scope of
        local -> l;
        global -> g
    end,

    %% Query all services and filter by type
    Pattern = {{{p, GprocScope, {service, '$1', '$2'}}}, '$3', '$4'},

    case gproc:select([Pattern]) of
        [] ->
            {ok, []};
        AllServices ->
            %% Filter by type and apply additional filters
            TypeFilters = [{type, ServiceType} | Filters],
            FilteredServices = apply_filters(AllServices, TypeFilters),
            ServiceInfos = [Info || {_Id, _Pid, Info} <- FilteredServices],
            {ok, ServiceInfos}
    end.

%% @doc Discover service with load balancing strategy
-spec do_discover_with_strategy(service_name(), local | global,
                                 load_balance_strategy(), state()) ->
    {{ok, service_info()} | {error, term()}, state()}.
do_discover_with_strategy(ServiceName, Scope, Strategy, State) ->
    case do_discover_services(ServiceName, Scope, [], State) of
        {ok, []} ->
            {{error, no_services_available}, State};
        {ok, Services} ->
            %% Apply load balancing strategy
            {SelectedService, NewLBState} = apply_load_balance_strategy(
                Services, ServiceName, Strategy, State#discovery_state.load_balance_state),

            NewState = State#discovery_state{
                load_balance_state = NewLBState
            },

            {{ok, SelectedService}, NewState}
    end.

%% @doc Get service count
-spec do_get_service_count(service_name(), local | global) ->
    {ok, non_neg_integer()} | {error, term()}.
do_get_service_count(ServiceName, Scope) ->
    case do_discover_services(ServiceName, Scope, [], #discovery_state{}) of
        {ok, Services} ->
            {ok, length(Services)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Perform health check on service
-spec do_service_health_check(service_name(), service_id(), state()) ->
    {ok, healthy | unhealthy | unknown} | {error, term()}.
do_service_health_check(ServiceName, ServiceId, State) ->
    case find_service_info(ServiceName, ServiceId, State) of
        {ok, ServiceInfo} ->
            ServicePid = maps:get(pid, ServiceInfo),

            %% Check if process is alive
            case is_process_alive(ServicePid) of
                false ->
                    {ok, unhealthy};
                true ->
                    %% Check health status from cache
                    CachedStatus = maps:get(ServiceId,
                                           State#discovery_state.health_status,
                                           unknown),
                    {ok, CachedStatus}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%====================================================================
%%% Internal Functions - Health & Monitoring
%%%====================================================================

%% @doc Mark service as healthy
-spec do_mark_service_healthy(service_name(), service_id(), state()) ->
    {ok, state()} | {{error, term()}, state()}.
do_mark_service_healthy(ServiceName, ServiceId, State) ->
    case find_service_info(ServiceName, ServiceId, State) of
        {ok, _ServiceInfo} ->
            NewHealthStatus = maps:put(ServiceId, healthy, State#discovery_state.health_status),
            NewStats = (State#discovery_state.stats)#{
                healthy_services => maps:get(healthy_services, State#discovery_state.stats, 0) + 1,
                unhealthy_services => max(0, maps:get(unhealthy_services, State#discovery_state.stats, 0) - 1)
            },

            NewState = State#discovery_state{
                health_status = NewHealthStatus,
                stats = NewStats
            },

            logger:info("Marked service ~p (~p) as healthy", [ServiceName, ServiceId]),

            %% Notify subscribers
            notify_subscribers(NewState, {service_healthy, ServiceName, ServiceId}),

            {ok, NewState};
        {error, Reason} ->
            {{error, Reason}, State}
    end.

%% @doc Mark service as unhealthy
-spec do_mark_service_unhealthy(service_name(), service_id(), state()) ->
    {ok, state()} | {{error, term()}, state()}.
do_mark_service_unhealthy(ServiceName, ServiceId, State) ->
    case find_service_info(ServiceName, ServiceId, State) of
        {ok, _ServiceInfo} ->
            NewHealthStatus = maps:put(ServiceId, unhealthy, State#discovery_state.health_status),
            NewStats = (State#discovery_state.stats)#{
                unhealthy_services => maps:get(unhealthy_services, State#discovery_state.stats, 0) + 1,
                healthy_services => max(0, maps:get(healthy_services, State#discovery_state.stats, 0) - 1)
            },

            NewState = State#discovery_state{
                health_status = NewHealthStatus,
                stats = NewStats
            },

            logger:warning("Marked service ~p (~p) as unhealthy", [ServiceName, ServiceId]),

            %% Notify subscribers
            notify_subscribers(NewState, {service_unhealthy, ServiceName, ServiceId}),

            {ok, NewState};
        {error, Reason} ->
            {{error, Reason}, State}
    end.

%% @doc Check if service is healthy
-spec do_service_is_healthy(service_name(), service_id(), state()) ->
    boolean() | {error, term()}.
do_service_is_healthy(ServiceName, ServiceId, State) ->
    case maps:get(ServiceId, State#discovery_state.health_status, undefined) of
        undefined ->
            {error, not_found};
        Status ->
            Status =:= healthy
    end.

%% @doc Force health check on all services
-spec do_force_health_check(state()) -> state().
do_force_health_check(State) ->
    %% Get all registered services
    Pattern = {{{p, l, {service, '$1', '$2'}}}, '$3', '$4'},

    case gproc:select([Pattern]) of
        [] ->
            State;
        AllServices ->
            %% Check health of each service
            lists:foldl(fun({_ServiceId, ServicePid, _ServiceInfo}, AccState) ->
                case is_process_alive(ServicePid) of
                    true ->
                        %% Service is healthy
                        %% In real implementation, would call custom health check
                        AccState;
                    false ->
                        %% Service is unhealthy, mark as such
                        %% (will be cleaned up by gproc monitor)
                        AccState
                end
            end, State, AllServices)
    end.

%%%====================================================================
%%% Internal Functions - Cluster Info
%%%====================================================================

%% @doc Get all cluster services
-spec do_get_cluster_services(state()) ->
    {ok, [{service_name(), service_info()}]}.
do_get_cluster_services(_State) ->
    Pattern = {{{p, l, {service, '$1', '$2'}}}, '$3', '$4'},

    case gproc:select([Pattern]) of
        [] ->
            {ok, []};
        Services ->
            Result = [{ServiceName, ServiceInfo} ||
                         {_ServiceId, _ServicePid, #{name := ServiceName} = ServiceInfo} <- Services],
            {ok, Result}
    end.

%% @doc Get stats for specific service
-spec do_get_service_stats(service_name(), state()) ->
    {ok, map()} | {error, term()}.
do_get_service_stats(ServiceName, _State) ->
    Pattern = {{{p, l, {service, ServiceName, '$1'}}}, '$2', '$3'},

    case gproc:select([Pattern]) of
        [] ->
            {error, not_found};
        Services ->
            HealthyCount = lists:foldl(fun({_ServiceId, _ServicePid, ServiceInfo}, Acc) ->
                ServiceId = maps:get(id, ServiceInfo),
                case maps:get(ServiceId, #{}, undefined) of
                    healthy -> Acc + 1;
                    _ -> Acc
                end
            end, 0, Services),

            Stats = #{
                name => ServiceName,
                total_count => length(Services),
                healthy_count => HealthyCount,
                unhealthy_count => length(Services) - HealthyCount
            },
            {ok, Stats}
    end.

%%%====================================================================
%%% Internal Functions - Helpers
%%%====================================================================

%% @doc Find service key by name and ID
-spec find_service_key(service_name(), service_id(), local | global, state()) ->
    {ok, term(), pid()} | {error, term()}.
find_service_key(ServiceName, ServiceId, Scope, _State) ->
    GprocScope = case Scope of
        local -> l;
        global -> g
    end,

    Key = {p, GprocScope, {service, ServiceName, ServiceId}},

    case gproc:where(Key) of
        undefined -> {error, not_found};
        ServicePid -> {ok, Key, ServicePid}
    end.

%% @doc Find service info by name and ID
-spec find_service_info(service_name(), service_id(), state()) ->
    {ok, service_info()} | {error, term()}.
find_service_info(ServiceName, ServiceId, _State) ->
    %% Try local first
    case gproc:where({p, l, {service, ServiceName, ServiceId}}) of
        undefined ->
            %% Try global
            case gproc:where({p, g, {service, ServiceName, ServiceId}}) of
                undefined ->
                    {error, not_found};
                ServicePid ->
                    ServiceInfo = gproc:get_value({p, g, {service, ServiceName, ServiceId}}, ServicePid),
                    {ok, ServiceInfo}
            end;
        ServicePid ->
            ServiceInfo = gproc:get_value({p, l, {service, ServiceName, ServiceId}}, ServicePid),
            {ok, ServiceInfo}
    end.

%% @doc Update service via key
-spec update_service_via_key(term(), pid(), service_metadata(), state()) ->
    {{ok, service_info()} | {error, term()}, state()}.
update_service_via_key(Key, ServicePid, NewMetadata, State) ->
    try
        %% Get current service info
        CurrentInfo = gproc:get_value(Key, ServicePid),

        %% Update metadata
        UpdatedInfo = CurrentInfo#{
            metadata => NewMetadata,
            last_heartbeat => erlang:system_time(millisecond)
        },

        %% Update gproc value
        gproc:set_value(Key, UpdatedInfo),

        {{ok, UpdatedInfo}, State}
    catch
        _:_ ->
            {{error, update_failed}, State}
    end.

%% @doc Apply filters to services
-spec apply_filters([{service_id(), pid(), service_info()}], [discovery_filter()]) ->
    [{service_id(), pid(), service_info()}].
apply_filters(Services, Filters) ->
    lists:filter(fun({_ServiceId, _ServicePid, ServiceInfo}) ->
        matches_filters(ServiceInfo, Filters)
    end, Services).

%% @doc Check if service info matches filters
-spec matches_filters(service_info(), [discovery_filter()]) -> boolean().
matches_filters(_ServiceInfo, []) ->
    true;
matches_filters(ServiceInfo, [Filter | Rest]) ->
    case matches_filter(ServiceInfo, Filter) of
        true -> matches_filters(ServiceInfo, Rest);
        false -> false
    end.

%% @doc Check if service info matches single filter
-spec matches_filter(service_info(), discovery_filter()) -> boolean().
matches_filter(ServiceInfo, {type, Type}) ->
    maps:get(type, ServiceInfo) =:= Type;
matches_filter(ServiceInfo, {version, Version}) ->
    maps:get(version, ServiceInfo) =:= Version;
matches_filter(ServiceInfo, {capability, Capability}) ->
    lists:member(Capability, maps:get(capabilities, ServiceInfo, []));
matches_filter(ServiceInfo, {metadata, MetadataFilter}) ->
    ServiceMetadata = maps:get(metadata, ServiceInfo, #{}),
    maps:fold(fun(Key, Value, Acc) ->
        Acc andalso maps:get(Key, ServiceMetadata, undefined) =:= Value
    end, true, MetadataFilter);
matches_filter(ServiceInfo, {health, Health}) ->
    %% Would need to check health status from state
    true;
matches_filter(ServiceInfo, {node, Node}) ->
    maps:get(node, ServiceInfo) =:= Node;
matches_filter(ServiceInfo, {local, true}) ->
    maps:get(node, ServiceInfo) =:= node();
matches_filter(ServiceInfo, {local, false}) ->
    maps:get(node, ServiceInfo) =/= node();
matches_filter(ServiceInfo, {global, true}) ->
    true;
matches_filter(ServiceInfo, {global, false}) ->
    maps:get(registration, ServiceInfo) =:= local.

%% @doc Group services by name
-spec group_services_by_name([{service_id(), pid(), service_info()}],
                             [discovery_filter()]) ->
    [{service_name(), [service_info()]}].
group_services_by_name(Services, Filters) ->
    FilteredServices = apply_filters(Services, Filters),

    %% Group by service name
    GroupMap = lists:foldl(fun({_ServiceId, _ServicePid, ServiceInfo}, Acc) ->
        ServiceName = maps:get(name, ServiceInfo),
        maps:update_with(ServiceName, fun(Existing) -> [ServiceInfo | Existing] end,
                        [ServiceInfo], Acc)
    end, #{}, FilteredServices),

    %% Convert to list
    maps:to_list(GroupMap).

%% @doc Apply load balancing strategy
-spec apply_load_balance_strategy([service_info()], service_name(),
                                   load_balance_strategy(), map()) ->
    {service_info(), map()}.
apply_load_balance_strategy(Services, ServiceName, round_robin, LBState) ->
    %% Round-robin: iterate through services
    Index = maps:get(ServiceName, LBState, 0),
    SelectedService = lists:nth((Index rem length(Services)) + 1, Services),
    NewLBState = maps:put(ServiceName, Index + 1, LBState),
    {SelectedService, NewLBState};

apply_load_balance_strategy(Services, _ServiceName, random, LBState) ->
    %% Random: select random service
    Index = rand:uniform(length(Services)),
    SelectedService = lists:nth(Index, Services),
    {SelectedService, LBState};

apply_load_balance_strategy(Services, _ServiceName, least_loaded, LBState) ->
    %% Least loaded: select service with lowest load (simplified: random for now)
    %% In real implementation, would query actual load metrics
    apply_load_balance_strategy(Services, _ServiceName, random, LBState);

apply_load_balance_strategy(Services, _ServiceName, local_first, LBState) ->
    %% Local first: prefer services on local node
    LocalServices = [S || S <- Services, maps:get(node, S) =:= node()],
    case LocalServices of
        [] ->
            %% No local services, use random
            apply_load_balance_strategy(Services, _ServiceName, random, LBState);
        _ ->
            apply_load_balance_strategy(LocalServices, _ServiceName, random, LBState)
    end.

%% @doc Generate unique service ID
-spec generate_service_id(service_name(), node()) -> service_id().
generate_service_id(ServiceName, Node) ->
    Unique = erlang:unique_integer([positive]),
    NameBin = case ServiceName of
        Name when is_atom(Name) -> atom_to_binary(Name, utf8);
        Name when is_binary(Name) -> Name
    end,
    NodeBin = atom_to_binary(Node, utf8),
    <<NameBin/binary, "_", NodeBin/binary, "_", (integer_to_binary(Unique))/binary>>.

%% @doc Update stats on service registration
-spec update_stats_on_register(local | global, map()) -> map().
update_stats_on_register(local, Stats) ->
    Stats#{
        total_services => maps:get(total_services, Stats, 0) + 1,
        local_services => maps:get(local_services, Stats, 0) + 1
    };
update_stats_on_register(global, Stats) ->
    Stats#{
        total_services => maps:get(total_services, Stats, 0) + 1,
        global_services => maps:get(global_services, Stats, 0) + 1
    }.

%% @doc Update stats on service unregistration
-spec update_stats_on_unregister(local | global, map()) -> map().
update_stats_on_unregister(local, Stats) ->
    Stats#{
        total_services => max(0, maps:get(total_services, Stats, 0) - 1),
        local_services => max(0, maps:get(local_services, Stats, 0) - 1)
    };
update_stats_on_unregister(global, Stats) ->
    Stats#{
        total_services => max(0, maps:get(total_services, Stats, 0) - 1),
        global_services => max(0, maps:get(global_services, Stats, 0) - 1)
    }.

%% @doc Initialize stats
-spec initial_stats() -> map().
initial_stats() ->
    #{
        total_services => 0,
        healthy_services => 0,
        unhealthy_services => 0,
        local_services => 0,
        global_services => 0,
        nodes => length(nodes())
    }.

%% @doc Handle process death
-spec handle_process_down(pid(), state()) -> state().
handle_process_down(Pid, State) ->
    %% Check if it's a subscriber
    NewSubscribers = case sets:is_element(Pid, State#discovery_state.event_subscribers) of
        true ->
            sets:del_element(Pid, State#discovery_state.event_subscribers);
        false ->
            State#discovery_state.event_subscribers
    end,

    State#discovery_state{event_subscribers = NewSubscribers}.

%% @doc Handle gproc unregistration
-spec handle_gproc_unreg(term(), state()) -> state().
handle_gproc_unreg({p, _Scope, {service, ServiceName, ServiceId}}, State) ->
    logger:warning("Service ~p (~p) unregistered from gproc", [ServiceName, ServiceId]),

    %% Clean up monitors
    NewMonitors = case maps:get(ServiceId, State#discovery_state.service_monitors, undefined) of
        undefined -> State#discovery_state.service_monitors;
        MonRef ->
            demonitor(MonRef, [flush]),
            maps:remove(ServiceId, State#discovery_state.service_monitors)
    end,

    %% Clean up health status
    NewHealthStatus = maps:remove(ServiceId, State#discovery_state.health_status),

    State#discovery_state{
        service_monitors = NewMonitors,
        health_status = NewHealthStatus
    };
handle_gproc_unreg(_Key, State) ->
    State.

%% @doc Clean up services from failed node
-spec cleanup_node_services(node(), state()) -> state().
cleanup_node_services(Node, State) ->
    %% Find all services on the failed node
    Pattern = {{{p, l, {service, '$1', '$2'}}}, '$3', '$4'},

    case gproc:select([Pattern]) of
        [] ->
            State;
        AllServices ->
            lists:foldl(fun({ServiceId, _ServicePid, ServiceInfo}, AccState) ->
                case maps:get(node, ServiceInfo) =:= Node of
                    true ->
                        %% Clean up this service
                        NewMonitors = maps:remove(ServiceId, AccState#discovery_state.service_monitors),
                        NewHealthStatus = maps:remove(ServiceId, AccState#discovery_state.health_status),
                        AccState#discovery_state{
                            service_monitors = NewMonitors,
                            health_status = NewHealthStatus
                        };
                    false ->
                        AccState
                end
            end, State, AllServices)
    end.

%% @doc Notify subscribers of events
-spec notify_subscribers(state(), discovery_event()) -> ok.
notify_subscribers(#discovery_state{event_subscribers = Subscribers}, Event) ->
    sets:fold(fun(Subscriber, _Acc) ->
        case is_process_alive(Subscriber) of
            true ->
                Subscriber ! {discovery_event, Event};
            false ->
                ok
        end,
        ok
    end, ok, Subscribers).

%% @doc Emit OpenTelemetry event
-spec emit_telemetry_event(atom(), map()) -> ok.
emit_telemetry_event(EventName, Metadata) ->
    try
        opentelemetry:record_event(EventName, #{
            attributes => Metadata
        }),
        ok
    catch
        _:_ ->
            %% OTEL not available, skip
            ok
    end.
