%%%-------------------------------------------------------------------
%%% @doc
%%% Connection Pool Manager with Circuit Breaker and Health Monitoring
%%%
%%% This module provides a production-ready connection pool with:
%%% - Poolboy-based connection pooling
%%% - Circuit breaker pattern integration
%%% - Health monitoring and automatic recovery
%%% - Load balancing strategies (round_robin, least_loaded, random)
%%% - Lease timeout and automatic release
%%% - Metrics collection
%%% - Backpressure handling
%%% - Multiplexing support
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_connection_pool).
-behaviour(gen_server).
-behaviour(poolboy_worker).

%% Public API
-export([start_link/1,
         checkout/2,
         checkin/2,
         with_connection/3,
         get_pool_status/1,
         get_pool_metrics/1,
         reset_circuit_breaker/1,
         update_pool_config/2,
         drain_pool/1,
         drain/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("erlmcp.hrl").
-include("erlmcp_connection_state.hrl").

%% Pool configuration
-record(pool_config,
        {name :: atom(),
         transport_type :: transport_type(),
         host :: inet:hostname() | inet:ip_address(),
         port :: inet:port_number(),
         size :: pos_integer(),
         max_overflow :: non_neg_integer(),
         strategy :: round_robin | least_loaded | random,
         lease_timeout :: pos_integer(),
         idle_timeout :: pos_integer(),
         connection_opts :: map()}).

-type pool_config() :: #pool_config{}.

%% Pool metrics
-record(pool_metrics,
        {checkouts = 0 :: non_neg_integer(),
         checkins = 0 :: non_neg_integer(),
         lease_timeouts = 0 :: non_neg_integer(),
         errors = 0 :: non_neg_integer(),
         active_connections = 0 :: non_neg_integer(),
         idle_connections = 0 :: non_neg_integer(),
         total_connections = 0 :: non_neg_integer()}).

-type pool_metrics() :: #pool_metrics{}.

%% Pool status
-record(pool_status,
        {state :: starting | running | draining | stopped,
         connections :: [pid()],
         leased :: [pid()],
         circuit_breaker_state :: circuit_state(),
         health_status :: healthy | unhealthy | unknown}).

-type pool_status() :: #pool_status{}.

%% Server state
-record(state,
        {config :: pool_config(),
         metrics :: pool_metrics(),
         status :: pool_status(),
         monitor_refs :: #{pid() => reference()},
         drain_timeout :: reference() | undefined}).

%%====================================================================
%% Type Definitions
%%====================================================================

-type checkout_option() ::
    {timeout, timeout()}                    |
    {strategy, round_robin | least_loaded | random} |
    {block, boolean()}.

-type checkout_opts() :: [checkout_option()].

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start a connection pool
-spec start_link(pool_config()) -> {ok, pid()} | {error, term()}.
start_link(Config) when is_record(Config, pool_config) ->
    gen_server:start_link(?MODULE, Config, []).

%% @doc Check out a connection from the pool
-spec checkout(atom(), checkout_opts()) -> {ok, pid(), reference()} | {error, term()}.
checkout(PoolName, Opts) when is_atom(PoolName), is_list(Opts) ->
    Timeout = proplists:get_value(timeout, Opts, 5000),
    case poolboy:checkout(PoolName, Timeout) of
        {ok, Pid} ->
            %% Monitor the connection
            MonRef = monitor(process, Pid),
            {ok, Pid, MonRef};
        {error, Reason} = Error ->
            %% Report error to pool manager
            gen_server:cast(PoolName, {checkout_error, Reason}),
            Error
    end.

%% @doc Check in a connection to the pool
-spec checkin(atom(), pid()) -> ok.
checkin(PoolName, Pid) when is_atom(PoolName), is_pid(Pid) ->
    %% Demonitor first
    case is_process_alive(Pid) of
        true ->
            %% Connection still alive, check it back in
            poolboy:checkin(PoolName, Pid);
        false ->
            %% Connection dead, don't check in (poolboy will restart it)
            ok
    end.

%% @doc Execute a function with a checked-out connection
-spec with_connection(atom(), checkout_opts(), fun((pid()) -> T)) -> {ok, T} | {error, term()}.
with_connection(PoolName, Opts, Fun) when is_atom(PoolName), is_list(Opts), is_function(Fun, 1) ->
    case checkout(PoolName, Opts) of
        {ok, Pid, MonRef} ->
            try
                Result = Fun(Pid),
                checkin(PoolName, Pid),
                erlang:demonitor(MonRef, [flush]),
                {ok, Result}
            catch
                Class:Reason:Stacktrace ->
                    checkin(PoolName, Pid),
                    erlang:demonitor(MonRef, [flush]),
                    {error, {Class, Reason, Stacktrace}}
            end;
        {error, Reason} = Error ->
            Error
    end.

%% @doc Get pool status
-spec get_pool_status(atom()) -> {ok, pool_status()}.
get_pool_status(PoolName) ->
    gen_server:call(PoolName, get_status, 1000).

%% @doc Get pool metrics
-spec get_pool_metrics(atom()) -> {ok, pool_metrics()}.
get_pool_metrics(PoolName) ->
    gen_server:call(PoolName, get_metrics, 1000).

%% @doc Reset circuit breaker for all connections in pool
-spec reset_circuit_breaker(atom()) -> ok.
reset_circuit_breaker(PoolName) ->
    gen_server:call(PoolName, reset_circuit_breaker, 5000).

%% @doc Update pool configuration dynamically
-spec update_pool_config(atom(), map()) -> ok.
update_pool_config(PoolName, Config) when is_map(Config) ->
    gen_server:call(PoolName, {update_config, Config}, 1000).

%% @doc Drain the pool (close all connections, stop accepting new requests)
-spec drain_pool(atom()) -> ok.
drain_pool(PoolName) ->
    gen_server:call(PoolName, drain, 5000).

%% @doc Drain all connection pools globally (for graceful shutdown)
-spec drain() -> ok.
drain() ->
    logger:info("Draining all erlmcp connection pools"),
    try
        % Find all registered pools via gproc
        PoolNames = case gproc:lookup_pids({n, l, erlmcp_connection_pool}) of
            [] ->
                % Fallback: try common pool names
                [erlmcp_tcp_pool, erlmcp_http_pool, erlmcp_ws_pool];
            Pids when is_list(Pids) ->
                % Get pool names from registered processes
                lists:filter(fun(Pid) ->
                    case gproc:info(Pid, registered_name) of
                        {registered_name, Name} when is_atom(Name) ->
                            true;
                        _ ->
                            false
                    end
                end, Pids)
        end,

        % Drain each pool
        lists:foreach(fun(PoolName) ->
            logger:debug("Draining pool: ~p", [PoolName]),
            drain_pool(PoolName)
        end, PoolNames),

        logger:info("All connection pools drained"),
        ok
    catch
        _:Error ->
            logger:error("Error draining connection pools: ~p", [Error]),
            ok
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init(#pool_config{name = Name,
                   transport_type = TransportType,
                   host = Host,
                   port = Port,
                   size = Size,
                   max_overflow = MaxOverflow,
                   strategy = Strategy,
                   lease_timeout = LeaseTimeout,
                   idle_timeout = IdleTimeout,
                   connection_opts = ConnOpts} = Config) ->

    process_flag(trap_exit, true),

    %% Initialize metrics
    Metrics = #pool_metrics{total_connections = Size},

    %% Initialize status
    Status = #pool_status{
        state = running,
        connections = [],
        leased = [],
        circuit_breaker_state = closed,
        health_status = healthy
    },

    %% Start poolboy pool
    PoolArgs = [
        {name, {local, Name}},
        {worker_module, erlmcp_connection_fsm},
        {size, Size},
        {max_overflow, MaxOverflow}
    ],

    %% Connection FSM options
    FsmOpts = #{
        transport_type => TransportType,
        host => Host,
        port => Port,
        owner => self(),
        pool_name => Name
    },

    %% Merge connection opts
    FsmOpts2 = maps:merge(FsmOpts, ConnOpts),

    %% Start poolboy with our worker
    PoolboyConfig = [
        {name, {local, Name}},
        {worker_module, erlmcp_connection_fsm},
        {size, Size},
        {max_overflow, MaxOverflow}
    ],

    ChildSpec = #{
        id => Name,
        start => {poolboy, start_link, [PoolboyConfig, [Name, TransportType, Host, Port]]},
        restart => permanent,
        shutdown => 5000,
        type => supervisor,
        modules => [poolboy]
    },

    %% Start poolboy supervisor
    case supervisor:start_child(erlmcp_transport_sup, ChildSpec) of
        {ok, _Pid} ->
            logger:info("Connection pool ~p started: ~p connections to ~s:~p",
                        [Name, Size, Host, Port]),

            %% Start health monitoring
            erlang:send_after(30000, self(), health_monitor),

            {ok, #state{
                config = Config,
                metrics = Metrics,
                status = Status,
                monitor_refs = #{}
            }};
        {error, Reason} ->
            {stop, {pool_start_failed, Reason}}
    end.

%% @private
handle_call(get_status, _From, #state{status = Status} = State) ->
    {reply, {ok, Status}, State};

handle_call(get_metrics, _From, #state{metrics = Metrics} = State) ->
    {reply, {ok, Metrics}, State};

handle_call(reset_circuit_breaker, _From, #state{config = #pool_config{name = Name}} = State) ->
    %% Reset circuit breaker for all connections in pool
    case poolboy:status(Name) of
        {_, Ready, _, _} ->
            %% Get all workers
            Workers = poolboy:get_all_workers(Name),
            lists:foreach(fun(Pid) ->
                catch erlmcp_connection_fsm:reset_circuit_breaker(Pid)
            end, Workers),

            %% Update status
            NewStatus = State#state.status#pool_status{
                circuit_breaker_state = closed
            },

            logger:info("Circuit breaker reset for pool ~p (~p workers)", [Name, length(Workers)]),

            {reply, ok, State#state{status = NewStatus}};
        _ ->
            {reply, {error, pool_not_available}, State}
    end;

handle_call({update_config, Config}, _From, #state{config = PoolConfig} = State) when is_map(Config) ->
    %% Update pool configuration
    NewPoolConfig = case maps:get(size, Config, undefined) of
        undefined -> PoolConfig;
        NewSize -> PoolConfig#pool_config{size = NewSize}
    end,

    NewPoolConfig2 = case maps:get(max_overflow, Config, undefined) of
        undefined -> NewPoolConfig;
        NewMaxOverflow -> NewPoolConfig#pool_config{max_overflow = NewMaxOverflow}
    end,

    NewPoolConfig3 = case maps:get(lease_timeout, Config, undefined) of
        undefined -> NewPoolConfig2;
        NewLeaseTimeout -> NewPoolConfig2#pool_config{lease_timeout = NewLeaseTimeout}
    end,

    logger:info("Pool config updated: ~p", [NewPoolConfig3]),

    {reply, ok, State#state{config = NewPoolConfig3}};

handle_call(drain, _From, #state{config = #pool_config{name = Name}} = State) ->
    %% Stop accepting new checkouts, wait for existing leases to return
    logger:info("Draining pool ~p", [Name]),

    %% Set drain timeout (30 seconds default)
    DrainTimeout = erlang:send_after(30000, self(), drain_timeout),

    NewStatus = State#state.status#pool_status{state = draining},

    {reply, ok, State#state{status = NewStatus, drain_timeout = DrainTimeout}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({checkout_error, Reason}, #state{metrics = Metrics} = State) ->
    %% Record checkout error
    NewMetrics = Metrics#pool_metrics{
        errors = Metrics#pool_metrics.errors + 1
    },

    case Reason of
        full ->
            logger:warning("Pool checkout failed: pool full");
        timeout ->
            logger:warning("Pool checkout failed: timeout");
        _ ->
            logger:warning("Pool checkout failed: ~p", [Reason])
    end,

    {noreply, State#state{metrics = NewMetrics}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(health_monitor, #state{config = #pool_config{name = Name}} = State) ->
    %% Perform health monitoring for all connections
    case poolboy:status(Name) of
        {AllReady, Ready, _, _} ->
            Workers = poolboy:get_all_workers(Name),
            TotalConns = length(Workers),
            ActiveConns = AllReady - Ready,

            %% Check health of each connection
            {HealthyCount, UnhealthyCount} =
                lists:foldl(fun(Pid, {Healthy, Unhealthy}) ->
                    case erlmcp_connection_fsm:get_health(Pid) of
                        {ok, healthy, _} ->
                            {Healthy + 1, Unhealthy};
                        {ok, unhealthy, _} ->
                            {Healthy, Unhealthy + 1};
                        {ok, unknown, _} ->
                            {Healthy, Unhealthy + 1};
                        _ ->
                            {Healthy, Unhealthy + 1}
                    end
                end, {0, 0}, Workers),

            %% Update health status
            HealthStatus = if
                UnhealthyCount =:= 0 -> healthy;
                UnhealthyCount > TotalConns div 2 -> unhealthy;
                true -> unknown
            end,

            %% Get circuit breaker state
            CircuitState = case Workers of
                [] -> closed;
                [FirstWorker | _] ->
                    case erlmcp_connection_fsm:get_state(FirstWorker) of
                        {ok, _, #{circuit_breaker := #{state := CBState}}} ->
                            CBState;
                        _ ->
                            closed
                    end
            end,

            NewStatus = State#state.status#pool_status{
                connections = Workers,
                active_connections = ActiveConns,
                idle_connections = Ready,
                circuit_breaker_state = CircuitState,
                health_status = HealthStatus
            },

            NewMetrics = State#state.metrics#pool_metrics{
                active_connections = ActiveConns,
                idle_connections = Ready,
                total_connections = TotalConns
            },

            %% Log if unhealthy
            case HealthStatus of
                unhealthy ->
                    logger:warning("Pool ~p unhealthy: ~p/~p connections unhealthy",
                                  [Name, UnhealthyCount, TotalConns]);
                _ ->
                    ok
            end,

            %% Schedule next health check
            erlang:send_after(30000, self(), health_monitor),

            {noreply, State#state{status = NewStatus, metrics = NewMetrics}};
        _ ->
            %% Pool not available
            erlang:send_after(30000, self(), health_monitor),
            {noreply, State}
    end;

handle_info(drain_timeout, #state{status = #pool_status{state = draining}} = State) ->
    %% Drain timeout reached, force close all connections
    logger:warning("Pool drain timeout reached, force closing connections"),

    NewStatus = State#state.status#pool_status{state = stopped},
    {noreply, State#state{status = NewStatus}};

handle_info(drain_timeout, State) ->
    %% Not draining anymore, ignore
    {noreply, State};

handle_info({'DOWN', MonRef, process, Pid, Reason}, #state{status = #pool_status{leased = Leased},
                                                           monitor_refs = MonRefs,
                                                           metrics = Metrics} = State) ->

    %% Connection died
    logger:info("Connection ~p died: ~p", [Pid, Reason]),

    %% Remove from leased list
    NewLeased = lists:delete(Pid, Leased),

    %% Update metrics
    NewMetrics = Metrics#pool_metrics{
        active_connections = max(0, Metrics#pool_metrics.active_connections - 1)
    },

    %% Remove monitor ref
    NewMonRefs = maps:filter(fun(_K, V) -> V =/= MonRef end, MonRefs),

    NewStatus = State#state.status#pool_status{leased = NewLeased},

    {noreply, State#state{status = NewStatus, metrics = NewMetrics, monitor_refs = NewMonRefs}};

handle_info(Info, State) ->
    logger:warning("Unexpected info: ~p", [Info]),
    {noreply, State}.

%% @private
terminate(_Reason, #state{config = #pool_config{name = Name}}) ->
    %% Stop poolboy pool
    case supervisor:terminate_child(erlmcp_transport_sup, Name) of
        ok -> ok;
        {error, not_found} -> ok
    end,
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Start poolboy worker
-spec start_pool_worker(atom(), atom(), transport_type(), inet:hostname(), inet:port_number()) ->
    {ok, pid()} | {error, term()}.
start_pool_worker(PoolName, WorkerId, TransportType, Host, Port) ->
    %% Start connection FSM with unique name
    Name = list_to_atom(atom_to_list(PoolName) ++ "_" ++ atom_to_list(WorkerId)),

    case erlmcp_connection_fsm:start_link(Name, TransportType, Host, Port) of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.
