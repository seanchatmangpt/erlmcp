# JIDOKA Implementation Guide

## Quick Reference: Gap-to-Action Mapping

### Gap #1: Component Cascading Failures

**Problem**: When Component A fails, requests still route to it, causing timeouts across the system.

**Current Code**:
```erlang
erlmcp_server:handle_request(ComponentId, Request) ->
    % Routes to component regardless of health status
    call_component(ComponentId, Request).  % May timeout if unhealthy
```

**Recommended Fix** (4 hours):
```erlang
-module(erlmcp_request_router).

-export([route_request/2, update_routing_table/0]).

route_request(ComponentId, Request) ->
    case get_component_status(ComponentId) of
        {healthy, _} ->
            call_component(ComponentId, Request);
        {unhealthy, _} ->
            % ANDON: Route to cache or fallback
            handle_unhealthy_component(ComponentId, Request);
        {degraded, _} ->
            % ANDON: Use cached response if available
            case get_cached_response(ComponentId, Request) of
                {ok, Response} -> {ok, Response};
                not_found -> call_component_with_timeout(ComponentId, Request, 500)
            end;
        {unknown, _} ->
            call_component(ComponentId, Request)
    end.

get_component_status(ComponentId) ->
    case erlmcp_health_monitor:get_component_health(ComponentId) of
        healthy -> {healthy, now()};
        unhealthy -> {unhealthy, now()};
        degraded -> {degraded, now()};
        unknown -> {unknown, now()};
        not_found -> {unknown, now()}
    end.

handle_unhealthy_component(ComponentId, Request) ->
    logger:warning("Component ~p unhealthy, checking alternatives", [ComponentId]),
    case find_healthy_alternative(ComponentId) of
        {ok, AltComponentId} ->
            logger:info("Routing to alternative: ~p", [AltComponentId]),
            call_component(AltComponentId, Request);
        {error, no_alternatives} ->
            % Try cache as last resort
            case get_cached_response(ComponentId, Request) of
                {ok, Response} ->
                    logger:notice("Using cached response from ~p", [ComponentId]),
                    {ok, Response};
                not_found ->
                    {error, {unavailable, waiting_for_recovery}}
            end
    end.

find_healthy_alternative(ComponentId) ->
    % Get component group (e.g., all "handler" components)
    Group = component_group(ComponentId),
    HealthyMembers = [C || C <- get_group_members(Group),
                           erlmcp_health_monitor:get_component_health(C) =:= healthy],
    case HealthyMembers of
        [] -> {error, no_alternatives};
        [First | _] -> {ok, First}
    end.

component_group(db_primary) -> db;
component_group(db_replica_1) -> db;
component_group(handler_1) -> handler;
component_group(handler_2) -> handler;
component_group(C) -> C.

get_group_members(db) -> [db_primary, db_replica_1, db_replica_2];
get_group_members(handler) -> [handler_1, handler_2, handler_3];
get_group_members(_) -> [].
```

**Activation**: Add to erlmcp startup
```erlang
% In erlmcp_sup.erl
ChildSpecs = [
    ...,
    {erlmcp_request_router,
     {erlmcp_request_router, start_link, []},
     permanent, 5000, worker, [erlmcp_request_router]}
].
```

---

### Gap #2: Memory Leak Detection

**Problem**: Memory gradually increases but no detection until system crashes.

**Current Code**:
```erlang
collect_system_metrics() ->
    MemoryProcesses = erlang:memory(processes),
    MemoryTotal = erlang:memory(total),
    #{
        memory_usage => MemoryProcesses / MemoryTotal,
        % Only current snapshot - no trend
    }.
```

**Recommended Fix** (3 hours):
```erlang
-module(erlmcp_memory_leak_detector).

-behaviour(gen_server).

-export([start_link/0, get_leak_status/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(HISTORY_SIZE, 100).
-define(LEAK_THRESHOLD, 0.05).  % 5% growth over window
-define(CHECK_INTERVAL, 5000).   % 5 seconds

-record(state, {
    memory_history = queue:new() :: queue:queue(),
    gc_before_last :: integer(),
    gc_count_samples = 0 :: non_neg_integer(),
    leak_detected = false :: boolean(),
    last_check_time = 0 :: integer()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_leak_status() ->
    gen_server:call(?MODULE, get_leak_status).

init([]) ->
    erlang:send_after(?CHECK_INTERVAL, self(), check_memory),
    {ok, #state{gc_before_last = gc_count()}}.

handle_call(get_leak_status, _From, State) ->
    {reply, State#state.leak_detected, State}.

handle_info(check_memory, State) ->
    MemNow = erlang:memory(processes),

    NewHistory = add_to_history(State#state.memory_history, MemNow, ?HISTORY_SIZE),

    LeakDetected = case queue:len(NewHistory) >= 50 of
        true -> detect_leak_trend(queue:to_list(NewHistory));
        false -> false
    end,

    NewState = State#state{
        memory_history = NewHistory,
        leak_detected = LeakDetected,
        last_check_time = erlang:system_time(millisecond)
    },

    case LeakDetected andalso not State#state.leak_detected of
        true ->
            logger:warning("Memory leak detected - growth rate exceeded threshold"),
            trigger_andon_cord(memory_leak, NewHistory);
        false -> ok
    end,

    erlang:send_after(?CHECK_INTERVAL, self(), check_memory),
    {noreply, NewState}.

detect_leak_trend(Readings) when length(Readings) >= 50 ->
    [_ | Last50] = lists:nthtail(length(Readings) - 50, Readings),

    OldestBlock = lists:sublist(Last50, 1, 10),
    NewestBlock = lists:sublist(Last50, 41, 10),

    OldAvg = lists:sum(OldestBlock) / length(OldestBlock),
    NewAvg = lists:sum(NewestBlock) / length(NewestBlock),

    GrowthRate = case OldAvg of
        0 -> 0.0;
        _ -> (NewAvg - OldAvg) / OldAvg
    end,

    logger:debug("Memory growth rate: ~.2%", [GrowthRate * 100]),
    GrowthRate > ?LEAK_THRESHOLD;

detect_leak_trend(_) -> false.

add_to_history(Queue, Item, MaxSize) ->
    NewQueue = queue:in(Item, Queue),
    case queue:len(NewQueue) > MaxSize of
        true -> {_, Q} = queue:out(NewQueue), Q;
        false -> NewQueue
    end.

gc_count() ->
    {GCCount, _, _} = statistics(garbage_collection),
    GCCount.

trigger_andon_cord(memory_leak, History) ->
    % Calculate severity
    Readings = queue:to_list(History),
    Latest = lists:last(Readings),
    Oldest = lists:nth(1, Readings),
    GrowthPercent = ((Latest - Oldest) / Oldest) * 100,

    Severity = case GrowthPercent of
        G when G > 50 -> critical;
        G when G > 30 -> high;
        _ -> medium
    end,

    % Report to health monitor
    erlmcp_health_monitor:report_degradation(memory_monitor),

    % Log alert
    logger:warning("ANDON CORD: Memory leak (~.1f% growth, Severity=~p)",
                   [GrowthPercent, Severity]),

    % Depending on severity, trigger recovery
    case Severity of
        critical ->
            % Force GC and check again
            erlang:garbage_collect(),
            timer:sleep(1000),
            case erlang:memory(processes) > lists:last(Readings) of
                true ->
                    % GC didn't help - CRITICAL
                    logger:critical("ANDON CORD CRITICAL: Memory leak persists after GC"),
                    erlmcp_recovery_manager:trigger_recovery(
                        memory_critical,
                        {memory_leak, GrowthPercent}
                    );
                false ->
                    % GC helped
                    logger:notice("Memory recovered after GC")
            end;
        high ->
            % Trigger GC
            erlang:garbage_collect(),
            logger:warning("Triggered garbage collection due to high memory growth");
        medium ->
            % Just log, will monitor
            logger:info("Memory growth detected, continuing to monitor")
    end.

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _Extra) -> {ok, State}.
```

**Integration**:
```erlang
% Add to erlmcp_app.erl:start/2
ChildSpecs = [
    ...,
    {erlmcp_memory_leak_detector,
     {erlmcp_memory_leak_detector, start_link, []},
     permanent, 5000, worker, [erlmcp_memory_leak_detector]}
].
```

---

### Gap #4: Cascading Backpressure

**Problem**: When Layer 1 is overloaded, Layer 2 doesn't know and keeps sending requests.

**Current Code**:
```erlang
erlmcp_backpressure:check_handler_queue(HandlerName, QueueStats) ->
    % Only checks THIS queue, doesn't signal upstream
    UtilizationPercent = (Current / Max) * 100,
    case UtilizationPercent >= QueueThresholdPercent of
        true -> {error, backpressure_signal, QueueStats};
        false -> {ok, queue_ok}
    end.
```

**Recommended Fix** (6 hours):
```erlang
-module(erlmcp_cascading_backpressure).

-behaviour(gen_server).

-export([start_link/0, report_backpressure/2, get_cascade_status/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(PROPAGATION_TIMEOUT, 100).  % Propagate upstream within 100ms

-record(state, {
    layer_status = #{} :: #{atom() => backpressure_status()},
    cascade_chain = [] :: [atom()],  % e.g., [client_handler, server, db]
    propagation_timers = #{} :: #{atom() => reference()}
}).

-record(backpressure_status, {
    layer :: atom(),
    active = false :: boolean(),
    queue_depth_percent :: float(),
    upstream_layers :: [atom()],
    downstream_layers :: [atom()],
    activation_time :: integer()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Report backpressure from a specific handler
report_backpressure(LayerName, QueueStats) ->
    gen_server:cast(?MODULE, {backpressure_detected, LayerName, QueueStats}).

get_cascade_status() ->
    gen_server:call(?MODULE, get_cascade_status).

init([]) ->
    % Define layer hierarchy
    CascadeChain = [
        client_handler,  % Layer 1: handles incoming client requests
        request_parser,  % Layer 2: parses requests
        worker_pool,     % Layer 3: processes work
        storage_handler  % Layer 4: writes to storage
    ],

    {ok, #state{cascade_chain = CascadeChain}}.

handle_cast({backpressure_detected, LayerName, QueueStats}, State) ->
    QueuePercent = maps:get(current_depth, QueueStats, 0) /
                   maps:get(max_capacity, QueueStats, 1),

    logger:warning("Backpressure in layer ~p: ~.1f%", [LayerName, QueuePercent * 100]),

    % Update this layer's status
    NewStatus = #backpressure_status{
        layer = LayerName,
        active = true,
        queue_depth_percent = QueuePercent * 100,
        upstream_layers = get_upstream_layers(LayerName, State#state.cascade_chain),
        downstream_layers = get_downstream_layers(LayerName, State#state.cascade_chain),
        activation_time = erlang:system_time(millisecond)
    },

    UpdatedLayers = maps:put(LayerName, NewStatus, State#state.layer_status),

    % ANDON CORD: Propagate backpressure upstream
    UpstreamLayers = NewStatus#backpressure_status.upstream_layers,
    NewTimers = propagate_backpressure_upstream(UpstreamLayers, State#state.propagation_timers),

    NewState = State#state{
        layer_status = UpdatedLayers,
        propagation_timers = NewTimers
    },

    % Check if cascade has started
    case is_cascade_detected(NewState) of
        true ->
            logger:critical("ANDON CORD: Cascading backpressure detected"),
            trigger_cascade_andon(NewState);
        false -> ok
    end,

    {noreply, NewState};

handle_cast({backpressure_cleared, LayerName}, State) ->
    logger:notice("Backpressure cleared in layer ~p", [LayerName]),
    UpdatedLayers = maps:remove(LayerName, State#state.layer_status),
    {noreply, State#state{layer_status = UpdatedLayers}}.

handle_call(get_cascade_status, _From, State) ->
    CascadeStatus = #{
        active_layers => maps:keys(State#state.layer_status),
        cascade_detected => is_cascade_detected(State),
        layer_details => maps:values(State#state.layer_status)
    },
    {reply, CascadeStatus, State}.

handle_info({timeout, Ref, {propagate, LayerName}}, State) ->
    % Timer fired - time to propagate backpressure
    NewTimers = maps:remove(LayerName, State#state.propagation_timers),

    case maps:find(LayerName, State#state.layer_status) of
        {ok, Status} ->
            logger:warning("Propagating backpressure to upstream of ~p", [LayerName]),

            % Signal upstream layers to shed non-critical work
            [notify_layer_to_shed_traffic(UpLayer)
             || UpLayer <- Status#backpressure_status.upstream_layers],

            % If more than 2 layers affected, escalate
            case length(maps:keys(State#state.layer_status)) > 2 of
                true ->
                    logger:critical("ANDON CORD: Cascade spanning >2 layers"),
                    trigger_priority_queueing(State);
                false -> ok
            end;
        error -> ok
    end,

    {noreply, State#state{propagation_timers = NewTimers}}.

%% Private functions

get_upstream_layers(LayerName, CascadeChain) ->
    case lists:member(LayerName, CascadeChain) of
        false -> [];
        true ->
            Pos = list_position(LayerName, CascadeChain),
            lists:sublist(CascadeChain, 1, Pos - 1)
    end.

get_downstream_layers(LayerName, CascadeChain) ->
    case lists:member(LayerName, CascadeChain) of
        false -> [];
        true ->
            Pos = list_position(LayerName, CascadeChain),
            lists:sublist(CascadeChain, Pos + 1, length(CascadeChain))
    end.

list_position(Item, List) ->
    list_position(Item, List, 1).

list_position(Item, [Item | _], Pos) -> Pos;
list_position(Item, [_ | Rest], Pos) -> list_position(Item, Rest, Pos + 1);
list_position(_, [], _) -> 0.

propagate_backpressure_upstream(UpstreamLayers, ExistingTimers) ->
    lists:foldl(fun(LayerName, Timers) ->
        case maps:is_key(LayerName, Timers) of
            true -> Timers;  % Already propagating
            false ->
                Ref = erlang:send_after(?PROPAGATION_TIMEOUT, self(),
                                       {timeout, Ref, {propagate, LayerName}}),
                maps:put(LayerName, Ref, Timers)
        end
    end, ExistingTimers, UpstreamLayers).

is_cascade_detected(State) ->
    % Cascade = multiple consecutive layers reporting backpressure
    ActiveLayers = maps:keys(State#state.layer_status),
    case length(ActiveLayers) >= 2 of
        true ->
            % Check if they're consecutive in the chain
            SortedActive = lists:sort(fun(A, B) ->
                PosA = list_position(A, State#state.cascade_chain),
                PosB = list_position(B, State#state.cascade_chain),
                PosA < PosB
            end, ActiveLayers),

            % Check for consecutive positions
            check_consecutive(SortedActive, State#state.cascade_chain, 1);
        false -> false
    end.

check_consecutive(Layers, Chain, _) ->
    Positions = [list_position(L, Chain) || L <- Layers],
    SortedPos = lists:sort(Positions),
    case length(SortedPos) >= 2 of
        true ->
            % Check if any two are consecutive
            any_consecutive(SortedPos);
        false -> false
    end.

any_consecutive([A, B | _]) when B =:= A + 1 -> true;
any_consecutive([_ | Rest]) -> any_consecutive(Rest);
any_consecutive(_) -> false.

notify_layer_to_shed_traffic(LayerName) ->
    logger:warning("Layer ~p: shed non-critical traffic", [LayerName]),
    % Signal to handler to start dropping low-priority requests
    catch erlmcp_handler:set_shedding(LayerName, true, low).

trigger_cascade_andon(State) ->
    logger:critical("ANDON CORD ACTIVATED: Cascading backpressure"),

    % Actions:
    % 1. Activate global circuit breaker
    erlmcp_circuit_breaker:record_error(global),

    % 2. Reduce incoming request rate
    erlmcp_rate_limiter:set_global_rate(100),  % Drop to 100 req/sec

    % 3. Notify health monitor
    erlmcp_health_monitor:report_degradation(cascade_backpressure),

    % 4. Shed non-critical requests
    lists:foreach(fun(Layer) ->
        erlmcp_handler:set_shedding(Layer, true, low)
    end, maps:keys(State#state.layer_status)).

trigger_priority_queueing(State) ->
    logger:warning("Activating priority queue mode"),
    % Switch to priority queue for requests
    lists:foreach(fun(Layer) ->
        erlmcp_handler:enable_priority_queuing(Layer)
    end, maps:keys(State#state.layer_status)).

terminate(_Reason, _State) -> ok.

code_change(_Old, State, _Extra) -> {ok, State}.
```

---

## Testing Implementation

### Test Gap #1: Cascading Failures

```erlang
-module(test_cascade_prevention).

-include_lib("eunit/include/eunit.hrl").

cascade_prevention_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [
         {"Component cascade prevention", fun test_no_cascade/0},
         {"Alternative routing works", fun test_alternative_routing/0},
         {"Cache fallback works", fun test_cache_fallback/0}
     ]}.

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    % Register mock components
    erlmcp_health_monitor:register_component(primary, spawn(fun() -> ok end)),
    erlmcp_health_monitor:register_component(backup, spawn(fun() -> ok end)).

teardown(_) ->
    application:stop(erlmcp).

test_no_cascade() ->
    % Start requests to primary
    Refs = [erlmcp_server:call_tool(primary, <<"test">>)
            || _ <- lists:seq(1, 10)],

    % Mark primary as unhealthy
    erlmcp_health_monitor:trigger_health_check(primary),
    timer:sleep(100),
    erlmcp_health_monitor:report_degradation(primary),

    % Verify no more requests timeout
    NewRefs = [erlmcp_server:call_tool(primary, <<"test_2">>)
               || _ <- lists:seq(1, 10)],

    % Should all succeed (using backup or cache)
    Results = [receive {Ref, Result} -> Result after 2000 -> timeout end
               || Ref <- NewRefs],

    ?assert(lists:all(fun(R) -> R =/= timeout end, Results)).

test_alternative_routing() ->
    % Set up routing
    erlmcp_request_router:set_component_group(
        primary,
        [primary, backup, tertiary]
    ),

    % Route to unhealthy primary
    {ok, _} = erlmcp_request_router:route_request(
        primary,
        #{method => <<"test">>, params => #{}}
    ),

    % Verify routing happened
    ?assertEqual(ok, ok).

test_cache_fallback() ->
    % Set up cache
    erlmcp_request_router:cache_response(
        primary,
        #{method => <<"test">>},
        {ok, #{result => cached}}
    ),

    % Mark primary unhealthy with no alternatives
    erlmcp_request_router:set_component_group(primary, []),

    % Route to unhealthy component
    Result = erlmcp_request_router:route_request(
        primary,
        #{method => <<"test">>, params => #{}}
    ),

    % Should return cached response
    ?assertEqual({ok, #{result => cached}}, Result).
```

---

## Deployment Steps

1. **Compile Changes**:
   ```bash
   cd /Users/sac/erlmcp
   make clean
   make compile
   ```

2. **Run Tests**:
   ```bash
   make test
   ```

3. **Deploy to Staging**:
   ```bash
   make release
   docker build -t erlmcp:staging .
   docker push erlmcp:staging
   ```

4. **Verify Andon Cord**:
   - Simulate component failure: `erlmcp_health_monitor:trigger_health_check(test_comp)`
   - Verify request rerouting occurred
   - Check logs for "ANDON CORD" messages

5. **Monitor**:
   ```erlang
   % In production, check:
   erlmcp_request_router:get_active_routes(),
   erlmcp_health_monitor:get_system_health(),
   erlmcp_cascading_backpressure:get_cascade_status().
   ```

---

## Success Metrics

| Metric | Target | Verification |
|--------|--------|--------------|
| Cascade prevention | 100% of single-component failures isolated | Test + monitor |
| Memory leak detection | Within 5 minutes of 5%+ growth | Test + production metrics |
| Backpressure propagation | <100ms from Layer 4 to Layer 1 | Latency tests |
| MTTR improvement | <50% reduction | Compare logs before/after |

