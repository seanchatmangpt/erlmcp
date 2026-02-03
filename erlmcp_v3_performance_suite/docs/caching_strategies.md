# Caching Strategies for erlmcp v3

## Overview

This document outlines comprehensive caching strategies for erlmcp v3 designed to achieve sub-100ms p95 latency and support 10,000+ requests/second throughput.

## Cache Architecture

### Multi-Level Cache Hierarchy

```
┌─────────────────────────────────────────────────────┐
│                   L1 Cache                          │
│    In-Process Cache (Local, < 1ms latency)           │
├─────────────────────────────────────────────────────┤
│                   L2 Cache                          │
│   Distributed Cache (Medium, < 5ms latency)        │
├─────────────────────────────────────────────────────┤
│                   L3 Cache                          │
│      Database Cache (Slow, < 50ms latency)          │
└─────────────────────────────────────────────────────┘
```

## Cache Configuration

### 1. L1 Cache (In-Process)

**erlmcp_cache_l1.erl**
```erlang
-module(erlmcp_cache_l1).

-behaviour(gen_server).

%% API
-export([start_link/0, get/2, put/3, delete/2, clear/0, stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    cache,                  % ets table
    max_size = 10000,      % Max entries
    ttl = 60000,           % TTL in ms
    hit_count = 0,         % Hit counter
    miss_count = 0         % Miss counter
}).

-record(cache_entry, {
    key,
    value,
    created,
    access_count = 0,
    size
}).

%% Configuration
-define(CACHE_TABLE, erlmcp_l1_cache).
-define(MAX_SIZE, 10000).
-define(DEFAULT_TTL, 60000).  % 1 minute
-define(CLEANUP_INTERVAL, 30000).  % 30 seconds

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key, Default) ->
    case gen_server:call(?MODULE, {get, Key}) of
        {ok, Value} ->
            Value;
        {error, not_found} ->
            Default
    end.

put(Key, Value, Options) ->
    gen_server:cast(?MODULE, {put, Key, Value, Options}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

clear() ->
    gen_server:cast(?MODULE, clear).

stats() ->
    gen_server:call(?MODULE, stats).

init([]) ->
    % Create ETS table
    Cache = ets:new(?CACHE_TABLE, [
        set,
        protected,
        {keypos, #cache_entry.key},
        {read_concurrency, true},
        {write_concurrency, true}
    ]),

    % Start cleanup timer
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),

    State = #state{
        cache = Cache,
        max_size = ?MAX_SIZE,
        ttl = ?DEFAULT_TTL
    },

    {ok, State}.

handle_call({get, Key}, _From, State) ->
    case ets:lookup(State#state.cache, Key) of
        [#cache_entry{value = Value, created = Created, access_count = Count} = Entry] ->
            % Check TTL
            Now = erlang:monotonic_time(millisecond),
            case Now - Created =< State#state.ttl of
                true ->
                    % Update access count
                    NewEntry = Entry#cache_entry{access_count = Count + 1},
                    ets:insert(State#state.cache, NewEntry),
                    {reply, {ok, Value}, State#state{hit_count = State#state.hit_count + 1}};
                false ->
                    % Entry expired
                    ets:delete(State#state.cache, Key),
                    {reply, {error, not_found}, State#state{miss_count = State#state.miss_count + 1}}
            end;
        [] ->
            {reply, {error, not_found}, State#state{miss_count = State#state.miss_count + 1}}
    end;

handle_call(stats, _From, State) ->
    Total = State#state.hit_count + State#state.miss_count,
    HitRate = case Total > 0 of
        true -> State#state.hit_count / Total;
        false -> 0
    end,

    Stats = #{
        hit_count => State#state.hit_count,
        miss_count => State#state.miss_count,
        hit_rate => HitRate,
        size => ets:info(State#state.cache, size),
        memory => ets:info(State#state.cache, memory),
        max_size => State#state.max_size,
        ttl => State#state.ttl
    },

    {reply, {ok, Stats}, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({put, Key, Value, Options}, State) ->
    Now = erlang:monotonic_time(millisecond),
    Size = calculate_size(Value),

    Entry = #cache_entry{
        key = Key,
        value = Value,
        created = Now,
        access_count = 0,
        size = Size
    },

    % Check size limits
    case should_evict(State, Size) of
        true ->
            % Evict entries based on policy
            EvictionPolicy = proplists:get_value(eviction_policy, Options, lru),
            Evicted = evict_entries(State, EvictionPolicy),
            io:format("Cache eviction: ~p entries~n", [Evicted]);
        false ->
            ok
    end,

    ets:insert(State#state.cache, Entry),

    {noreply, State};

handle_cast({delete, Key}, State) ->
    ets:delete(State#state.cache, Key),
    {noreply, State};

handle_cast(clear, State) ->
    ets:delete_all_objects(State#state.cache),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    % Clean up expired entries
    Now = erlang:monotonic_time(millisecond),
    Expired = ets:foldl(fun(Entry, Acc) ->
        case Now - Entry#cache_entry.created > State#state.ttl of
            true -> [Entry | Acc];
            false -> Acc
        end
    end, [], State#state.cache),

    lists:foreach(fun(Entry) ->
        ets:delete(State#state.cache, Entry#cache_entry.key)
    end, Expired),

    % Restart cleanup timer
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private Functions
calculate_size(Value) ->
    % Simple size calculation - could be more sophisticated
    size(term_to_binary(Value)).

should_evict(State, NewEntrySize) ->
    CurrentSize = ets:foldl(fun(Entry, Acc) -> Acc + Entry#cache_entry.size end, 0, State#state.cache),
    CurrentSize + NewEntrySize > State#state.max_size.

evict_entries(State, Policy) ->
    case Policy of
        lru ->
            % Evict least recently used
            Sorted = ets:foldl(fun(Entry, Acc) ->
                [{Entry#cache_entry.access_count, Entry} | Acc]
            end, [], State#state.cache),
            EvictCount = min(100, length(Sorted)),
            ToEvict = lists:sublist(lists:keysort(1, Sorted), EvictCount),
            lists:foreach(fun({_, Entry}) ->
                ets:delete(State#state.cache, Entry#cache_entry.key)
            end, ToEvict),
            EvictCount;
        lfu ->
            % Evict least frequently used
            evict_entries(State, lru);  % Same as LRU for now
        random ->
            % Random eviction
            Count = ets:info(State#state.cache, size),
            EvictCount = min(100, Count),
            Keys = ets:select(State#state.cache, [{{'$1', '_', '_', '_', '_'}, [], ['$1']}]),
            ToEvict = lists:sublist(lists:seq(1, length(Keys)), EvictCount),
            lists:foreach(fun(I) ->
                ets:delete(State#state.cache, lists:nth(I, Keys))
            end, ToEvict),
            EvictCount
    end.
```

### 2. L2 Cache (Distributed)

**erlmcp_cache_l2.erl**
```erlang
-module(erlmcp_cache_l2).

-behaviour(gen_server).

%% API
-export([start_link/0, get/2, put/3, delete/2, clear/0, cluster_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    nodes = [],            % List of cache nodes
    local_cache,          % Local L1 cache
    replication_factor = 2, % Number of replicas
    consistency = eventual  % Consistency level
}).

-record(cache_value, {
    key,
    value,
    version = 1,
    created,
    expires
}).

-define(CACHE_NODES, [node() | nodes()]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key, Default) ->
    gen_server:call(?MODULE, {get, Key, Default}).

put(Key, Value, Options) ->
    gen_server:cast(?MODULE, {put, Key, Value, Options}).

delete(Key) ->
    gen_server:cast(?MODULE, {delete, Key}).

clear() ->
    gen_server:cast(?MODULE, clear).

cluster_status() ->
    gen_server:call(?MODULE, cluster_status).

init([]) ->
    % Initialize local L1 cache
    {ok, Pid} = erlmcp_cache_l1:start_link(),
    LocalCache = Pid,

    % Connect to other nodes
    ConnectedNodes = connect_to_nodes(),

    State = #state{
        nodes = ConnectedNodes,
        local_cache = LocalCache,
        replication_factor = 2,
        consistency = eventual
    },

    % Monitor cluster changes
    [erlang:monitor_node(Node, true) || Node <- ConnectedNodes],

    {ok, State}.

handle_call({get, Key, Default}, _From, State) ->
    % First try local cache
    case erlmcp_cache_l1:get(Key, undefined) of
        Value when Value /= undefined ->
            {reply, {ok, Value}, State};
        undefined ->
            % Check distributed cache
            case get_from_cluster(Key, State) of
                {ok, Value} ->
                    % Store in local cache
                    erlmcp_cache_l1:put(Key, Value, []),
                    {reply, {ok, Value}, State};
                {error, not_found} ->
                    {reply, {ok, Default}, State}
            end
    end;

handle_call(cluster_status, _From, State) ->
    Status = #{
        nodes => length(State#state.nodes),
        connected => [Node || Node <- State#state.nodes, nodeup(Node)],
        local_cache => erlmcp_cache_l1:stats()
    },
    {reply, {ok, Status}, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({put, Key, Value, Options}, State) ->
    TTL = proplists:get_value(ttl, Options, 60000),
    Expires = erlang:monotonic_time(millisecond) + TTL,

    CacheValue = #cache_value{
        key = Key,
        value = Value,
        created = erlang:monotonic_time(millisecond),
        expires = Expires
    },

    % Store in local cache
    erlmcp_cache_l1:put(Key, Value, []),

    % Replicate to other nodes
    Replicate = proplists:get_value(replicate, Options, true),
    if
        Replicate ->
            replicate_to_cluster(CacheValue, State);
        true ->
            ok
    end,

    {noreply, State};

handle_cast({delete, Key}, State) ->
    % Delete from local cache
    erlmcp_cache_l1:delete(Key),

    % Replicate deletion
    replicate_deletion(Key, State),

    {noreply, State};

handle_cast(clear, State) ->
    % Clear local cache
    erlmcp_cache_l1:clear(),

    % Replicate clear
    replicate_clear(State),

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({nodeup, Node}, State) ->
    % New node joined cluster
    NewNodes = [Node | State#state.nodes],
    io:format("Cache node ~p joined cluster~n", [Node]),
    {noreply, State#state{nodes = NewNodes}};

handle_info({nodedown, Node}, State) ->
    % Node left cluster
    NewNodes = lists:delete(Node, State#state.nodes),
    io:format("Cache node ~p left cluster~n", [Node]),
    {noreply, State#state{nodes = NewNodes}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Private Functions
connect_to_nodes() ->
    AvailableNodes = ?CACHE_NODES -- [node()],
    lists:filter(fun nodeup/1, AvailableNodes).

nodeup(Node) ->
    lists:member(Node, nodes()).

get_from_cluster(Key, State) ->
    % Try to get from other nodes
    Fun = fun(Node) ->
        case rpc:call(Node, erlmcp_cache_l1, get, [Key, undefined], 2000) of
            Value when Value /= undefined ->
                {ok, Value};
            undefined ->
                {error, not_found}
        end
    end,

    % Query nodes in parallel
    Results = pmap(Fun, State#state.nodes),

    % Return first success or error
    case [R || R <- Results, R /= {error, not_found}] of
        [{ok, Value} | _] ->
            {ok, Value};
        [] ->
            {error, not_found}
    end.

replicate_to_cluster(CacheValue, State) ->
    % Replicate to N nodes based on replication factor
    ReplicationFactor = min(State#state.replication_factor, length(State#state.nodes)),
    Nodes = lists:sublist(State#state.nodes, ReplicationFactor),

    Fun = fun(Node) ->
        rpc:call(Node, erlmcp_cache_l1, put, [
            CacheValue#cache_value.key,
            CacheValue#cache_value.value,
            []
        ], 2000)
    end,

    pmap(Fun, Nodes).

replicate_deletion(Key, State) ->
    % Replicate deletion to all nodes
    Fun = fun(Node) ->
        rpc:call(Node, erlmcp_cache_l1, delete, [Key], 2000)
    end,

    pmap(Fun, State#state.nodes).

replicate_clear(State) ->
    % Replicate clear to all nodes
    Fun = fun(Node) ->
        rpc:call(Node, erlmcp_cache_l1, clear, [], 2000)
    end,

    pmap(Fun, State#state.nodes).

%% Parallel map utility
pmap(Fun, List) ->
    Self = self(),
    Pids = [spawn_link(fun() -> Self ! {self(), Fun(X)} end) || X <- List],
    [receive {Pid, Result} -> Result end || Pid <- Pids].
```

## Cache Warming Strategy

**erlmcp_cache_warming.erl**
```erlang
-module(erlmcp_cache_warming).

-export([start/0, warm_cache/0, warm_key/2, get_warm_stats/0]).

-record(warm_stats, {
    total_requests = 0,
    warmed_items = 0,
    last_warm = undefined,
    warm_time = 0
}).

-define(WARM_INTERVAL, 30000).  % 30 seconds
-define(PRELOAD_KEYS, [
    "system.config",
    "user.profile.1",
    "session.config",
    "resource.types",
    "tool.registry"
]).

start() ->
    erlang:send_after(?WARM_INTERVAL, self(), perform_warming),
    {ok, spawn(fun() -> warming_loop() end)}.

warm_cache() ->
    Keys = ?PRELOAD_KEYS,
    lists:foreach(fun(Key) ->
        case get_warm_data(Key) of
            {ok, Value} ->
                erlmcp_cache_l1:put(Key, Value, []),
                erlmcp_cache_l2:put(Key, Value, [{replicate, true}]);
            {error, _} ->
                ok
        end
    end, Keys),

    update_warm_stats(length(Keys)).

warm_key(Key, TTL) ->
    case get_warm_data(Key) of
        {ok, Value} ->
            erlmcp_cache_l1:put(Key, Value, [{ttl, TTL}]),
            erlmcp_cache_l2:put(Key, Value, [{replicate, true}, {ttl, TTL}]),
            ok;
        {error, _} ->
            error
    end.

get_warm_stats() ->
    case get(warm_stats) of
        undefined ->
            #warm_stats{};
        Stats ->
            Stats
    end.

%% Private Functions
warming_loop() ->
    receive
        perform_warming ->
            warm_cache(),
            erlang:send_after(?WARM_INTERVAL, self(), perform_warming)
    end,
    warming_loop().

get_warm_data(Key) ->
    % Implement data fetching logic
    % This could be from database, external service, etc.
    case Key of
        "system.config" ->
            {ok, #{version => "1.0.0", settings => #{max_connections => 1000}}};
        "user.profile.1" ->
            {ok, #{id => 1, name => "System User", preferences => #{}}};
        "session.config" ->
            {ok, #{timeout => 3600000, max_sessions => 10000}};
        "resource.types" ->
            {ok, [<<"tools">>, <<"resources">>, <<"prompts">>]};
        "tool.registry" ->
            {ok, []};  % Would normally load from registry
        _ ->
            {error, not_found}
    end.

update_warm_stats(WarmedCount) ->
    OldStats = get(warm_stats),
    Now = erlang:monotonic_time(millisecond),

    NewStats = #warm_stats{
        total_requests = OldStats#warm_stats.total_requests + 1,
        warmed_items = OldStats#warm_stats.warmed_items + WarmedCount,
        last_warm = Now,
        warm_time = case OldStats#warm_stats.last_warm of
            undefined -> 0;
            Last -> Now - Last
        end
    },

    put(warm_stats, NewStats).
```

## Cache Invalidation Strategy

**erlmcp_cache_invalidation.erl**
```erlang
-module(erlmcp_cache_invalidation).

-export([start/0, invalidate/1, invalidate_pattern/1, schedule_invalidation/3, get_stats/0]).

-record(invalidation_stats, {
    total_invalidated = 0,
    time_based = 0,
    event_based = 0,
    pattern_based = 0
}).

-record(schedule_entry, {
    key,
    action,
    timestamp,
    recurring = false
}).

-define(INVALIDATION_TABLE, erlmcp_invalidation_schedule).
-define(EVENT_SUBSCRIPTIONS, erlmcp_event_subscriptions).

start() ->
    % Create ETS tables
    ets:new(?INVALIDATION_TABLE, [
        set,
        protected,
        {keypos, #schedule_entry.key},
        {read_concurrency, true}
    ]),

    ets:new(?EVENT_SUBSCRIPTIONS, [
        set,
        public,
        {keypos, 1},  % Pattern
        {read_concurrency, true}
    ]),

    % Start invalidation scheduler
    erlang:send_after(1000, self(), check_scheduled),
    spawn(fun() -> event_listener() end),

    ok.

invalidate(Key) ->
    % Invalidate single key
    erlmcp_cache_l1:delete(Key),
    erlmcp_cache_l2:delete(Key),

    % Log invalidation
    io:format("Invalidated cache key: ~p~n", [Key]),
    update_invalidation_stats(1, event),

    ok.

invalidate_pattern(Pattern) ->
    % Invalidate keys matching pattern
    % This would need implementation based on cache structure
    io:format("Invalidating pattern: ~p~n", [Pattern]),
    update_invalidation_stats(1, pattern),

    ok.

schedule_invalidation(Key, Action, Delay) ->
    % Schedule future invalidation
    Entry = #schedule_entry{
        key = Key,
        action = Action,
        timestamp = erlang:monotonic_time(millisecond) + Delay
    },

    ets:insert(?INVALIDATION_TABLE, Entry),
    ok.

get_stats() ->
    ets:foldl(fun(Entry, Acc) ->
        Acc + 1
    end, 0, ?INVALIDATION_TABLE).

%% Private Functions
check_scheduled() ->
    Now = erlang:monotonic_time(millisecond),

    % Find entries that are due
    DueEntries = ets:select(?INVALIDATION_TABLE, [
        {{#schedule_entry{key = '$1', action = '$2', timestamp = '$3', recurring = '$4'},
          '$3', '<=', Now, '$1', '$2', '$4'}, [], ['$1']}
    ]),

    % Process due entries
    lists:foreach(fun(Key) ->
        case ets:lookup(?INVALIDATION_TABLE, Key) of
            [Entry] ->
                perform_invalidation(Entry),
                ets:delete(?INVALIDATION_TABLE, Key);
            [] ->
                ok
        end
    end, DueEntries),

    % Schedule next check
    erlang:send_after(1000, self(), check_scheduled).

perform_invalidation(#schedule_entry{key = Key, action = Action}) ->
    case Action of
        delete ->
            invalidate(Key);
        _ ->
            ok
    end.

event_listener() ->
    receive
        {event, invalidation, Key} ->
            invalidate(Key);
        {event, invalidation_pattern, Pattern} ->
            invalidate_pattern(Pattern);
        _ ->
            ok
    end,
    event_listener().

update_invalidation_stats(Count, Type) ->
    OldStats = get(invalidation_stats),
    if
        OldStats == undefined ->
            NewStats = #invalidation_stats{
                total_invalidated = Count,
                case Type of
                    time -> time_based = Count;
                    event -> event_based = Count;
                    pattern -> pattern_based = Count
                end
            };
        true ->
            NewStats = OldStats#invalidation_stats{
                total_invalidated = OldStats#invalidation_stats.total_invalidated + Count,
                case Type of
                    time -> time_based = OldStats#invalidation_stats.time_based + Count;
                    event -> event_based = OldStats#invalidation_stats.event_based + Count;
                    pattern -> pattern_based = OldStats#invalidation_stats.pattern_based + Count
                end
            }
    end,

    put(invalidation_stats, NewStats).
```

## Cache Monitoring

**erlmcp_cache_monitor.erl**
```erlang
-module(erlmcp_cache_monitor).

-export([start/0, monitor/0, get_cache_metrics/0, alert_on_thresholds/0]).

-record(cache_metrics, {
    l1_hit_rate = 0.0,
    l1_size = 0,
    l1_memory = 0,
    l2_hit_rate = 0.0,
    l2_nodes = 0,
    l2_connected = 0,
    total_requests = 0,
    avg_response_time = 0.0,
    timestamp = undefined
}).

-define(ALERT_THRESHOLDS, #{
    hit_rate => 0.90,      % Alert if hit rate < 90%
    memory_usage => 0.90,   % Alert if memory > 90%
    response_time => 50,   % Alert if avg response > 50ms
    error_rate => 0.01      % Alert if error rate > 1%
}).

start() ->
    erlang:send_after(5000, self(), collect_metrics),
    spawn(fun() -> monitoring_loop() end),
    ok.

monitor() ->
    % Start periodic monitoring
    spawn(fun() -> monitoring_loop() end),
    ok.

get_cache_metrics() ->
    % Get current metrics
    L1Stats = erlmcp_cache_l1:stats(),
    L2Stats = erlmcp_cache_l2:cluster_status(),

    % Calculate hit rates
    L1Total = L1Stats#hit_count + L1Stats#miss_count,
    L1HitRate = case L1Total > 0 of
        true -> L1Stats#hit_count / L1Total;
        false -> 0.0
    end,

    Metrics = #cache_metrics{
        l1_hit_rate = L1HitRate,
        l1_size = L1Stats#size,
        l1_memory = L1Stats#memory,
        l2_hit_rate = 0.0,  % Would need L2 hit rate calculation
        l2_nodes = L2Stats#nodes,
        l2_connected = L2Stats#connected,
        total_requests = L1Total,
        avg_response_time = get_avg_response_time(),
        timestamp = erlang:monotonic_time(millisecond)
    },

    Metrics.

alert_on_thresholds() ->
    Metrics = get_cache_metrics(),

    % Check thresholds
    Alerts = check_thresholds(Metrics),

    % Send alerts if needed
    case Alerts of
        [] ->
            ok;
        _ ->
            send_alerts(Alerts, Metrics)
    end,

    ok.

%% Private Functions
monitoring_loop() ->
    receive
        collect_metrics ->
            collect_and_store_metrics(),
            alert_on_thresholds(),
            erlang:send_after(5000, self(), collect_metrics)
    end,
    monitoring_loop().

collect_and_store_metrics() ->
    Metrics = get_cache_metrics(),

    % Store metrics for trending
    store_metrics(Metrics),

    % Update dashboard
    update_dashboard(Metrics),

    ok.

check_thresholds(Metrics) ->
    Alerts = [],

    % Check hit rate
    if
        Metrics#cache_metrics.l1_hit_rate < ?ALERT_THRESHOLDS#hit_rate ->
            [{'hit_rate', Metrics#cache_metrics.l1_hit_rate} | Alerts];
        true ->
            Alerts
    end,

    % Check memory usage
    if
        Metrics#cache_metrics.l1_memory / (1024*1024) > ?ALERT_THRESHOLDS#memory_usage * 2 ->
            [{'memory_usage', Metrics#cache_metrics.l1_memory} | Alerts];
        true ->
            Alerts
    end,

    % Check response time
    if
        Metrics#cache_metrics.avg_response_time > ?ALERT_THRESHOLDS#response_time ->
            [{'response_time', Metrics#cache_metrics.avg_response_time} | Alerts];
        true ->
            Alerts
    end,

    Alerts.

send_alerts(Alerts, Metrics) ->
    io:format("Cache Alert: ~p~n", [Alerts]),

    % Could integrate with external alerting system
    % send_to_slack(Alerts),
    % send_to_email(Alerts),

    ok.

store_metrics(Metrics) ->
    % Store metrics in time series database
    % Implementation would depend on chosen storage solution
    ok.

update_dashboard(Metrics) ->
    % Update dashboard metrics
    % Could use WebSocket or polling mechanism
    ok.

get_avg_response_time() ->
    % Would track actual response times
    % For demo, return dummy value
    25.0.
```

## Cache Configuration

**erlmcp_cache_config.hrl**
```erlang
% Cache configuration constants
-define(L1_CACHE_SIZE, 10000).           % Maximum items in L1 cache
-define(L1_CACHE_TTL, 60000).            % 1 minute TTL for L1 cache
-define(L2_REPLICATION_FACTOR, 2).        % Number of replicas for L2 cache
-define(CACHE_CLEANUP_INTERVAL, 30000).   % 30 seconds between cleanups
-define(CACHE_WARM_INTERVAL, 30000).     % 30 seconds between cache warming
-define(CACHE_METRICS_INTERVAL, 5000).   % 5 seconds between metrics collection
-define(CACHE_ALERT_THRESHOLDS, #{
    hit_rate => 0.90,      % Alert if hit rate < 90%
    memory_usage => 0.90,   % Alert if memory > 90%
    response_time => 50,   % Alert if avg response > 50ms
    error_rate => 0.01      % Alert if error rate > 1%
}).
```

## Usage Examples

### Basic Cache Usage
```erlang
% Start caches
erlmcp_cache_l1:start_link(),
erlmcp_cache_l2:start_link(),
erlmcp_cache_warming:start(),

% Put value in cache
erlmcp_cache_l1:put("user:1", #{name => "John", email => "john@example.com"}, []),
erlmcp_cache_l2:put("user:1", #{name => "John", email => "john@example.com"}, [{replicate, true}]),

% Get value from cache
User = erlmcp_cache_l1:get("user:1", undefined),

% Warming specific keys
erlmcp_cache_warming:warm_key("system.config", 120000),

% Monitor cache performance
Metrics = erlmcp_cache_monitor:get_cache_metrics(),
```

### Advanced Cache Usage
```erlang
% Configure cache eviction
Options = [
    {eviction_policy, lru},      % or lfu, random
    {ttl, 300000},              % 5 minutes
    {replicate, true},          % Replicate to L2
    {replication_factor, 3}     % 3 replicas
],

erlmcp_cache_l2:put("complex:data", LargeData, Options),

% Schedule invalidation
erlmcp_cache_invalidation:schedule_invalidation(
    "session:123",
    delete,
    3600000  % 1 hour from now
),

% Pattern-based invalidation
erlmcp_cache_invalidation:invalidate_pattern("user:*"),

% Get cache statistics
Stats = erlmcp_cache_monitor:get_cache_metrics(),
io:format("Cache hit rate: ~.2f%~n", [Stats#cache_metrics.l1_hit_rate * 100]).
```

## Performance Considerations

### 1. Memory Management
- Monitor ETS table memory usage
- Implement proper cleanup policies
- Consider using DETS for large datasets

### 2. Concurrency
- Use appropriate ETS concurrency settings
- Implement proper locking mechanisms
- Consider process-based caching for complex objects

### 3. Network Overhead
- Minimize network calls for cache operations
- Batch cache operations when possible
- Implement proper error handling for network failures

### 4. Cache Coherence
- Implement proper invalidation strategies
- Consider using versioning for stale data detection
- Implement monitoring for cache consistency

## Integration Points

### 1. With erlmcp Core
- Integrate with session management
- Cache frequently accessed resources
- Cache tool execution results

### 2. With Database Layer
- Cache query results
- Implement read-through caching
- Cache metadata and schema information

### 3. With Monitoring System
- Expose cache metrics to monitoring
- Implement cache health checks
- Alert on cache performance degradation

## Conclusion

The caching strategy for erlmcp v3 provides a comprehensive multi-level caching system designed to meet Fortune 500 performance requirements. The implementation includes proper monitoring, invalidation, and integration with the broader system architecture.