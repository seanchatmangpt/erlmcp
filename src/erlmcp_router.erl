-module(erlmcp_router).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% Enhanced routing capabilities for ERLMCP
%% Implements load balancing, circuit breakers, dead letter queues, and performance monitoring

%% API exports
-export([
    start_link/0,
    route_message/3,
    setup_load_balancer/2,
    setup_circuit_breaker/2,
    get_routing_metrics/0,
    get_routing_metrics/1,
    enable_adaptive_routing/1,
    set_routing_policy/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type routing_policy() :: round_robin | weighted | adaptive | least_connections.
-type circuit_breaker_state() :: closed | open | half_open.
-type load_balancer_config() :: #{
    policy => routing_policy(),
    weights => #{server_id() => non_neg_integer()},
    health_check => boolean()
}.

%% State record
-record(router_state, {
    % Registry reference
    registry_pid :: pid(),
    
    % Load balancing
    load_balancers = #{} :: #{atom() => load_balancer_config()},
    round_robin_counters = #{} :: #{atom() => non_neg_integer()},
    server_weights = #{} :: #{server_id() => non_neg_integer()},
    
    % Circuit breakers
    circuit_breakers = #{} :: #{server_id() => #{
        state => circuit_breaker_state(),
        failure_count => non_neg_integer(),
        failure_threshold => non_neg_integer(),
        timeout => non_neg_integer(),
        last_failure => non_neg_integer(),
        half_open_timeout => non_neg_integer()
    }},
    
    % Performance monitoring
    message_counts = #{} :: #{server_id() => non_neg_integer()},
    latency_stats = #{} :: #{server_id() => [non_neg_integer()]},
    error_counts = #{} :: #{server_id() => non_neg_integer()},
    queue_depths = #{} :: #{server_id() => non_neg_integer()},
    
    % Dead letter queue
    dead_letters = [] :: [{server_id(), transport_id(), term(), erlang:timestamp()}],
    max_dead_letters = 1000 :: non_neg_integer(),
    
    % Adaptive routing
    adaptive_routing = false :: boolean(),
    server_loads = #{} :: #{server_id() => float()},
    last_load_update = 0 :: non_neg_integer(),
    
    % Backpressure
    backpressure_enabled = true :: boolean(),
    max_queue_depth = 1000 :: non_neg_integer(),
    backpressure_threshold = 0.8 :: float()
}).

-type state() :: #router_state{}.

%% Server ID type from registry
-type server_id() :: erlmcp_registry:server_id().
-type transport_id() :: erlmcp_registry:transport_id().

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec route_message(server_id() | transport_id(), transport_id() | server_id(), term()) -> 
    ok | {error, term()}.
route_message(Destination, Source, Message) ->
    gen_server:cast(?MODULE, {route_message, Destination, Source, Message}).

-spec setup_load_balancer(atom(), load_balancer_config()) -> ok.
setup_load_balancer(Name, Config) ->
    gen_server:call(?MODULE, {setup_load_balancer, Name, Config}).

-spec setup_circuit_breaker(server_id(), map()) -> ok.
setup_circuit_breaker(ServerId, Config) ->
    gen_server:call(?MODULE, {setup_circuit_breaker, ServerId, Config}).

-spec get_routing_metrics() -> map().
get_routing_metrics() ->
    gen_server:call(?MODULE, get_routing_metrics).

-spec get_routing_metrics(server_id()) -> map().
get_routing_metrics(ServerId) ->
    gen_server:call(?MODULE, {get_routing_metrics, ServerId}).

-spec enable_adaptive_routing(boolean()) -> ok.
enable_adaptive_routing(Enable) ->
    gen_server:call(?MODULE, {enable_adaptive_routing, Enable}).

-spec set_routing_policy(atom(), routing_policy()) -> ok.
set_routing_policy(LoadBalancerName, Policy) ->
    gen_server:call(?MODULE, {set_routing_policy, LoadBalancerName, Policy}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    
    % Find registry process
    RegistryPid = whereis(erlmcp_registry),
    case RegistryPid of
        undefined ->
            logger:error("Registry not found during router initialization"),
            {stop, registry_not_found};
        _ ->
            logger:info("Starting enhanced message router"),
            
            % Start periodic tasks
            erlang:send_after(1000, self(), update_load_metrics),
            erlang:send_after(5000, self(), cleanup_dead_letters),
            erlang:send_after(10000, self(), update_circuit_breakers),
            
            {ok, #router_state{registry_pid = RegistryPid}}
    end.

handle_call({setup_load_balancer, Name, Config}, _From, State) ->
    NewLoadBalancers = maps:put(Name, Config, State#router_state.load_balancers),
    NewState = State#router_state{load_balancers = NewLoadBalancers},
    logger:info("Setup load balancer ~p with policy ~p", [Name, maps:get(policy, Config)]),
    {reply, ok, NewState};

handle_call({setup_circuit_breaker, ServerId, Config}, _From, State) ->
    CircuitBreaker = #{
        state => closed,
        failure_count => 0,
        failure_threshold => maps:get(failure_threshold, Config, 5),
        timeout => maps:get(timeout, Config, 60000),
        last_failure => 0,
        half_open_timeout => maps:get(half_open_timeout, Config, 30000)
    },
    NewCircuitBreakers = maps:put(ServerId, CircuitBreaker, State#router_state.circuit_breakers),
    NewState = State#router_state{circuit_breakers = NewCircuitBreakers},
    logger:info("Setup circuit breaker for server ~p", [ServerId]),
    {reply, ok, NewState};

handle_call(get_routing_metrics, _From, State) ->
    Metrics = #{
        message_counts => State#router_state.message_counts,
        error_counts => State#router_state.error_counts,
        queue_depths => State#router_state.queue_depths,
        circuit_breaker_states => get_circuit_breaker_states(State),
        dead_letter_count => length(State#router_state.dead_letters),
        adaptive_routing_enabled => State#router_state.adaptive_routing,
        server_loads => State#router_state.server_loads,
        latency_percentiles => calculate_latency_percentiles(State#router_state.latency_stats)
    },
    {reply, Metrics, State};

handle_call({get_routing_metrics, ServerId}, _From, State) ->
    Metrics = #{
        message_count => maps:get(ServerId, State#router_state.message_counts, 0),
        error_count => maps:get(ServerId, State#router_state.error_counts, 0),
        queue_depth => maps:get(ServerId, State#router_state.queue_depths, 0),
        circuit_breaker_state => get_circuit_breaker_state(ServerId, State),
        server_load => maps:get(ServerId, State#router_state.server_loads, 0.0),
        latency_stats => calculate_server_latency_stats(ServerId, State)
    },
    {reply, Metrics, State};

handle_call({enable_adaptive_routing, Enable}, _From, State) ->
    NewState = State#router_state{adaptive_routing = Enable},
    logger:info("Adaptive routing ~s", [case Enable of true -> "enabled"; false -> "disabled" end]),
    {reply, ok, NewState};

handle_call({set_routing_policy, LoadBalancerName, Policy}, _From, State) ->
    case maps:get(LoadBalancerName, State#router_state.load_balancers, undefined) of
        undefined ->
            {reply, {error, load_balancer_not_found}, State};
        Config ->
            NewConfig = maps:put(policy, Policy, Config),
            NewLoadBalancers = maps:put(LoadBalancerName, NewConfig, State#router_state.load_balancers),
            NewState = State#router_state{load_balancers = NewLoadBalancers},
            logger:info("Updated load balancer ~p policy to ~p", [LoadBalancerName, Policy]),
            {reply, ok, NewState}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({route_message, Destination, Source, Message}, State) ->
    StartTime = erlang:system_time(microsecond),
    
    try
        NewState = case determine_destination_type(Destination) of
            server ->
                route_to_server_enhanced(Destination, Source, Message, State, StartTime);
            transport ->
                route_to_transport_enhanced(Destination, Source, Message, State, StartTime);
            unknown ->
                logger:warning("Unknown destination type for ~p", [Destination]),
                State
        end,
        {noreply, NewState}
    catch
        Class:Reason:Stacktrace ->
            logger:error("Error routing message: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(update_load_metrics, State) ->
    NewState = update_server_loads(State),
    erlang:send_after(1000, self(), update_load_metrics),
    {noreply, NewState};

handle_info(cleanup_dead_letters, State) ->
    NewState = cleanup_old_dead_letters(State),
    erlang:send_after(5000, self(), cleanup_dead_letters),
    {noreply, NewState};

handle_info(update_circuit_breakers, State) ->
    NewState = update_circuit_breaker_states(State),
    erlang:send_after(10000, self(), update_circuit_breakers),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    logger:info("Enhanced message router terminating"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Enhanced Routing Logic
%%====================================================================

-spec route_to_server_enhanced(server_id(), transport_id(), term(), state(), non_neg_integer()) -> state().
route_to_server_enhanced(ServerId, TransportId, Message, State, StartTime) ->
    % Check circuit breaker
    case check_circuit_breaker(ServerId, State) of
        {allow, NewState1} ->
            % Check backpressure
            case check_backpressure(ServerId, NewState1) of
                {allow, NewState2} ->
                    % Attempt routing
                    case erlmcp_registry:route_to_server(ServerId, TransportId, Message) of
                        ok ->
                            EndTime = erlang:system_time(microsecond),
                            Latency = EndTime - StartTime,
                            record_successful_routing(ServerId, TransportId, Latency, NewState2);
                        {error, Reason} ->
                            record_routing_failure(ServerId, TransportId, Message, Reason, NewState2)
                    end;
                {backpressure, NewState2} ->
                    logger:warning("Backpressure active for server ~p, dropping message", [ServerId]),
                    add_to_dead_letter_queue(ServerId, TransportId, Message, backpressure, NewState2)
            end;
        {deny, NewState1} ->
            logger:warning("Circuit breaker open for server ~p, routing to fallback", [ServerId]),
            route_to_fallback_server(ServerId, TransportId, Message, NewState1)
    end.

-spec route_to_transport_enhanced(transport_id(), server_id(), term(), state(), non_neg_integer()) -> state().
route_to_transport_enhanced(TransportId, ServerId, Message, State, StartTime) ->
    case erlmcp_registry:route_to_transport(TransportId, ServerId, Message) of
        ok ->
            EndTime = erlang:system_time(microsecond),
            Latency = EndTime - StartTime,
            record_successful_response_routing(TransportId, ServerId, Latency, State);
        {error, Reason} ->
            logger:warning("Failed to route response to transport ~p: ~p", [TransportId, Reason]),
            record_transport_routing_failure(TransportId, ServerId, Message, Reason, State)
    end.

%%====================================================================
%% Circuit Breaker Logic
%%====================================================================

-spec check_circuit_breaker(server_id(), state()) -> {allow | deny, state()}.
check_circuit_breaker(ServerId, State) ->
    case maps:get(ServerId, State#router_state.circuit_breakers, undefined) of
        undefined ->
            {allow, State};
        CircuitBreaker ->
            Now = erlang:system_time(millisecond),
            case maps:get(state, CircuitBreaker) of
                closed ->
                    {allow, State};
                open ->
                    Timeout = maps:get(timeout, CircuitBreaker),
                    LastFailure = maps:get(last_failure, CircuitBreaker),
                    if
                        Now - LastFailure >= Timeout ->
                            % Move to half-open state
                            NewCircuitBreaker = maps:put(state, half_open, CircuitBreaker),
                            NewCircuitBreakers = maps:put(ServerId, NewCircuitBreaker, 
                                                        State#router_state.circuit_breakers),
                            NewState = State#router_state{circuit_breakers = NewCircuitBreakers},
                            logger:info("Circuit breaker for ~p moved to half-open", [ServerId]),
                            {allow, NewState};
                        true ->
                            {deny, State}
                    end;
                half_open ->
                    {allow, State}
            end
    end.

-spec record_circuit_breaker_success(server_id(), state()) -> state().
record_circuit_breaker_success(ServerId, State) ->
    case maps:get(ServerId, State#router_state.circuit_breakers, undefined) of
        undefined ->
            State;
        CircuitBreaker ->
            NewCircuitBreaker = maps:merge(CircuitBreaker, #{
                state => closed,
                failure_count => 0
            }),
            NewCircuitBreakers = maps:put(ServerId, NewCircuitBreaker, 
                                        State#router_state.circuit_breakers),
            State#router_state{circuit_breakers = NewCircuitBreakers}
    end.

-spec record_circuit_breaker_failure(server_id(), state()) -> state().
record_circuit_breaker_failure(ServerId, State) ->
    case maps:get(ServerId, State#router_state.circuit_breakers, undefined) of
        undefined ->
            State;
        CircuitBreaker ->
            FailureCount = maps:get(failure_count, CircuitBreaker) + 1,
            FailureThreshold = maps:get(failure_threshold, CircuitBreaker),
            Now = erlang:system_time(millisecond),
            
            NewState = if
                FailureCount >= FailureThreshold ->
                    NewCircuitBreaker = maps:merge(CircuitBreaker, #{
                        state => open,
                        failure_count => FailureCount,
                        last_failure => Now
                    }),
                    NewCircuitBreakers = maps:put(ServerId, NewCircuitBreaker, 
                                                State#router_state.circuit_breakers),
                    logger:warning("Circuit breaker opened for server ~p after ~p failures", 
                                   [ServerId, FailureCount]),
                    State#router_state{circuit_breakers = NewCircuitBreakers};
                true ->
                    NewCircuitBreaker = maps:put(failure_count, FailureCount, CircuitBreaker),
                    NewCircuitBreakers = maps:put(ServerId, NewCircuitBreaker, 
                                                State#router_state.circuit_breakers),
                    State#router_state{circuit_breakers = NewCircuitBreakers}
            end,
            NewState
    end.

%%====================================================================
%% Backpressure Logic
%%====================================================================

-spec check_backpressure(server_id(), state()) -> {allow | backpressure, state()}.
check_backpressure(ServerId, State) ->
    case State#router_state.backpressure_enabled of
        false ->
            {allow, State};
        true ->
            CurrentDepth = maps:get(ServerId, State#router_state.queue_depths, 0),
            MaxDepth = State#router_state.max_queue_depth,
            Threshold = State#router_state.backpressure_threshold,
            
            case CurrentDepth >= trunc(MaxDepth * Threshold) of
                true ->
                    {backpressure, State};
                false ->
                    {allow, State}
            end
    end.

%%====================================================================
%% Load Balancing Logic
%%====================================================================

-spec select_server_by_policy(atom(), [server_id()], state()) -> {server_id(), state()}.
select_server_by_policy(LoadBalancerName, Servers, State) ->
    case maps:get(LoadBalancerName, State#router_state.load_balancers, undefined) of
        undefined ->
            % Default to round-robin
            select_server_round_robin(Servers, State);
        Config ->
            Policy = maps:get(policy, Config, round_robin),
            case Policy of
                round_robin ->
                    select_server_round_robin(Servers, State);
                weighted ->
                    select_server_weighted(Servers, Config, State);
                adaptive ->
                    select_server_adaptive(Servers, State);
                least_connections ->
                    select_server_least_connections(Servers, State)
            end
    end.

-spec select_server_round_robin([server_id()], state()) -> {server_id(), state()}.
select_server_round_robin(Servers, State) ->
    Counter = maps:get(round_robin, State#router_state.round_robin_counters, 0),
    Index = Counter rem length(Servers),
    SelectedServer = lists:nth(Index + 1, Servers),
    NewCounters = maps:put(round_robin, Counter + 1, State#router_state.round_robin_counters),
    NewState = State#router_state{round_robin_counters = NewCounters},
    {SelectedServer, NewState}.

-spec select_server_weighted([server_id()], map(), state()) -> {server_id(), state()}.
select_server_weighted(Servers, Config, State) ->
    Weights = maps:get(weights, Config, #{}),
    WeightedServers = [{maps:get(Server, Weights, 1), Server} || Server <- Servers],
    TotalWeight = lists:sum([W || {W, _} <- WeightedServers]),
    
    case TotalWeight > 0 of
        true ->
            Target = rand:uniform(TotalWeight),
            SelectedServer = select_by_weight(WeightedServers, Target, 0),
            {SelectedServer, State};
        false ->
            select_server_round_robin(Servers, State)
    end.

-spec select_by_weight([{non_neg_integer(), server_id()}], non_neg_integer(), non_neg_integer()) -> server_id().
select_by_weight([{Weight, Server} | _], Target, Acc) when Acc + Weight >= Target ->
    Server;
select_by_weight([{Weight, _} | Rest], Target, Acc) ->
    select_by_weight(Rest, Target, Acc + Weight);
select_by_weight([], _, _) ->
    % Fallback - should not happen with proper weights
    error(no_server_selected).

-spec select_server_adaptive([server_id()], state()) -> {server_id(), state()}.
select_server_adaptive(Servers, State) ->
    case State#router_state.adaptive_routing of
        false ->
            select_server_round_robin(Servers, State);
        true ->
            ServerLoads = State#router_state.server_loads,
            LoadedServers = [{maps:get(Server, ServerLoads, 0.0), Server} || Server <- Servers],
            SortedServers = lists:sort(LoadedServers),
            {_, SelectedServer} = hd(SortedServers), % Select least loaded
            {SelectedServer, State}
    end.

-spec select_server_least_connections([server_id()], state()) -> {server_id(), state()}.
select_server_least_connections(Servers, State) ->
    QueueDepths = State#router_state.queue_depths,
    ServerConnections = [{maps:get(Server, QueueDepths, 0), Server} || Server <- Servers],
    SortedServers = lists:sort(ServerConnections),
    {_, SelectedServer} = hd(SortedServers), % Select server with fewest connections
    {SelectedServer, State}.

%%====================================================================
%% Metrics and Monitoring
%%====================================================================

-spec record_successful_routing(server_id(), transport_id(), non_neg_integer(), state()) -> state().
record_successful_routing(ServerId, _TransportId, Latency, State) ->
    % Update message count
    MessageCount = maps:get(ServerId, State#router_state.message_counts, 0),
    NewMessageCounts = maps:put(ServerId, MessageCount + 1, State#router_state.message_counts),
    
    % Update latency stats
    Latencies = maps:get(ServerId, State#router_state.latency_stats, []),
    NewLatencies = [Latency | lists:sublist(Latencies, 999)], % Keep last 1000 samples
    NewLatencyStats = maps:put(ServerId, NewLatencies, State#router_state.latency_stats),
    
    % Record circuit breaker success
    NewState1 = record_circuit_breaker_success(ServerId, State),
    
    NewState1#router_state{
        message_counts = NewMessageCounts,
        latency_stats = NewLatencyStats
    }.

-spec record_routing_failure(server_id(), transport_id(), term(), term(), state()) -> state().
record_routing_failure(ServerId, TransportId, Message, Reason, State) ->
    % Update error count
    ErrorCount = maps:get(ServerId, State#router_state.error_counts, 0),
    NewErrorCounts = maps:put(ServerId, ErrorCount + 1, State#router_state.error_counts),
    
    % Record circuit breaker failure
    NewState1 = record_circuit_breaker_failure(ServerId, State),
    
    % Add to dead letter queue
    NewState2 = add_to_dead_letter_queue(ServerId, TransportId, Message, Reason, NewState1),
    
    NewState2#router_state{error_counts = NewErrorCounts}.

-spec record_successful_response_routing(transport_id(), server_id(), non_neg_integer(), state()) -> state().
record_successful_response_routing(_TransportId, _ServerId, _Latency, State) ->
    % For now, just return state - could track transport metrics here
    State.

-spec record_transport_routing_failure(transport_id(), server_id(), term(), term(), state()) -> state().
record_transport_routing_failure(_TransportId, _ServerId, _Message, _Reason, State) ->
    % For now, just return state - could track transport failures here
    State.

%%====================================================================
%% Dead Letter Queue
%%====================================================================

-spec add_to_dead_letter_queue(server_id(), transport_id(), term(), term(), state()) -> state().
add_to_dead_letter_queue(ServerId, TransportId, Message, Reason, State) ->
    Timestamp = erlang:timestamp(),
    DeadLetter = {ServerId, TransportId, Message, Reason, Timestamp},
    
    DeadLetters = State#router_state.dead_letters,
    NewDeadLetters = [DeadLetter | lists:sublist(DeadLetters, State#router_state.max_dead_letters - 1)],
    
    logger:warning("Message added to dead letter queue: ~p -> ~p (reason: ~p)", 
                   [TransportId, ServerId, Reason]),
    
    State#router_state{dead_letters = NewDeadLetters}.

-spec cleanup_old_dead_letters(state()) -> state().
cleanup_old_dead_letters(State) ->
    Now = erlang:timestamp(),
    MaxAge = 3600000000, % 1 hour in microseconds
    
    FilterFun = fun({_, _, _, _, Timestamp}) ->
        timer:now_diff(Now, Timestamp) < MaxAge
    end,
    
    NewDeadLetters = lists:filter(FilterFun, State#router_state.dead_letters),
    State#router_state{dead_letters = NewDeadLetters}.

%%====================================================================
%% Adaptive Load Monitoring
%%====================================================================

-spec update_server_loads(state()) -> state().
update_server_loads(State) ->
    case State#router_state.adaptive_routing of
        false ->
            State;
        true ->
            Now = erlang:system_time(millisecond),
            case Now - State#router_state.last_load_update > 1000 of % Update every second
                true ->
                    NewServerLoads = calculate_server_loads(State),
                    State#router_state{
                        server_loads = NewServerLoads,
                        last_load_update = Now
                    };
                false ->
                    State
            end
    end.

-spec calculate_server_loads(state()) -> #{server_id() => float()}.
calculate_server_loads(State) ->
    MessageCounts = State#router_state.message_counts,
    ErrorCounts = State#router_state.error_counts,
    QueueDepths = State#router_state.queue_depths,
    LatencyStats = State#router_state.latency_stats,
    
    ServerIds = maps:keys(MessageCounts),
    
    maps:from_list(lists:map(fun(ServerId) ->
        Messages = maps:get(ServerId, MessageCounts, 0),
        Errors = maps:get(ServerId, ErrorCounts, 0),
        QueueDepth = maps:get(ServerId, QueueDepths, 0),
        Latencies = maps:get(ServerId, LatencyStats, []),
        
        % Calculate load based on multiple factors
        ErrorRate = case Messages of
            0 -> 0.0;
            _ -> Errors / Messages
        end,
        
        AvgLatency = case Latencies of
            [] -> 0.0;
            _ -> lists:sum(Latencies) / length(Latencies)
        end,
        
        % Normalize factors (0.0 to 1.0)
        NormalizedQueueDepth = min(QueueDepth / 100, 1.0),
        NormalizedErrorRate = min(ErrorRate * 10, 1.0),
        NormalizedLatency = min(AvgLatency / 10000, 1.0), % 10ms baseline
        
        % Weighted load calculation
        Load = (NormalizedQueueDepth * 0.5) + 
               (NormalizedErrorRate * 0.3) + 
               (NormalizedLatency * 0.2),
        
        {ServerId, Load}
    end, ServerIds)).

%%====================================================================
%% Circuit Breaker Updates
%%====================================================================

-spec update_circuit_breaker_states(state()) -> state().
update_circuit_breaker_states(State) ->
    Now = erlang:system_time(millisecond),
    
    NewCircuitBreakers = maps:map(fun(ServerId, CircuitBreaker) ->
        case maps:get(state, CircuitBreaker) of
            half_open ->
                HalfOpenTimeout = maps:get(half_open_timeout, CircuitBreaker),
                LastFailure = maps:get(last_failure, CircuitBreaker),
                case Now - LastFailure > HalfOpenTimeout of
                    true ->
                        logger:info("Circuit breaker for ~p moved to closed after half-open timeout", [ServerId]),
                        maps:merge(CircuitBreaker, #{
                            state => closed,
                            failure_count => 0
                        });
                    false ->
                        CircuitBreaker
                end;
            _ ->
                CircuitBreaker
        end
    end, State#router_state.circuit_breakers),
    
    State#router_state{circuit_breakers = NewCircuitBreakers}.

%%====================================================================
%% Utility Functions
%%====================================================================

-spec determine_destination_type(term()) -> server | transport | unknown.
determine_destination_type(Destination) ->
    % Simple heuristic - in practice, you might want a registry lookup
    DestStr = io_lib:format("~p", [Destination]),
    case string:str(DestStr, "server") of
        0 ->
            case string:str(DestStr, "transport") of
                0 -> unknown;
                _ -> transport
            end;
        _ ->
            server
    end.

-spec route_to_fallback_server(server_id(), transport_id(), term(), state()) -> state().
route_to_fallback_server(FailedServerId, TransportId, Message, State) ->
    % Simple fallback strategy - in practice, you'd have configured fallbacks
    AllServers = maps:keys(State#router_state.message_counts),
    AvailableServers = [S || S <- AllServers, S =/= FailedServerId,
                            get_circuit_breaker_state_direct(S, State) =/= open],
    
    case AvailableServers of
        [] ->
            logger:error("No fallback servers available for ~p", [FailedServerId]),
            add_to_dead_letter_queue(FailedServerId, TransportId, Message, no_fallback, State);
        [FallbackServer | _] ->
            logger:info("Routing to fallback server ~p for failed server ~p", 
                        [FallbackServer, FailedServerId]),
            case erlmcp_registry:route_to_server(FallbackServer, TransportId, Message) of
                ok ->
                    record_successful_routing(FallbackServer, TransportId, 0, State);
                {error, Reason} ->
                    record_routing_failure(FallbackServer, TransportId, Message, Reason, State)
            end
    end.

-spec get_circuit_breaker_states(state()) -> #{server_id() => circuit_breaker_state()}.
get_circuit_breaker_states(State) ->
    maps:map(fun(_, CircuitBreaker) ->
        maps:get(state, CircuitBreaker)
    end, State#router_state.circuit_breakers).

-spec get_circuit_breaker_state(server_id(), state()) -> circuit_breaker_state() | undefined.
get_circuit_breaker_state(ServerId, State) ->
    case maps:get(ServerId, State#router_state.circuit_breakers, undefined) of
        undefined -> undefined;
        CircuitBreaker -> maps:get(state, CircuitBreaker)
    end.

-spec get_circuit_breaker_state_direct(server_id(), state()) -> circuit_breaker_state() | closed.
get_circuit_breaker_state_direct(ServerId, State) ->
    case get_circuit_breaker_state(ServerId, State) of
        undefined -> closed;
        State -> State
    end.

-spec calculate_latency_percentiles(#{server_id() => [non_neg_integer()]}) -> 
    #{server_id() => #{p50 => float(), p95 => float(), p99 => float()}}.
calculate_latency_percentiles(LatencyStats) ->
    maps:map(fun(_, Latencies) ->
        case length(Latencies) of
            0 ->
                #{p50 => 0.0, p95 => 0.0, p99 => 0.0};
            Len ->
                Sorted = lists:sort(Latencies),
                P50Index = max(1, trunc(Len * 0.5)),
                P95Index = max(1, trunc(Len * 0.95)),
                P99Index = max(1, trunc(Len * 0.99)),
                #{
                    p50 => lists:nth(P50Index, Sorted) / 1.0,
                    p95 => lists:nth(P95Index, Sorted) / 1.0,
                    p99 => lists:nth(P99Index, Sorted) / 1.0
                }
        end
    end, LatencyStats).

-spec calculate_server_latency_stats(server_id(), state()) -> map().
calculate_server_latency_stats(ServerId, State) ->
    case maps:get(ServerId, State#router_state.latency_stats, []) of
        [] ->
            #{count => 0, avg => 0.0, min => 0.0, max => 0.0};
        Latencies ->
            Count = length(Latencies),
            Sum = lists:sum(Latencies),
            Avg = Sum / Count,
            Min = lists:min(Latencies),
            Max = lists:max(Latencies),
            #{
                count => Count,
                avg => Avg,
                min => Min / 1.0,
                max => Max / 1.0
            }
    end.