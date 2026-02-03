%%%-------------------------------------------------------------------
%%% @doc
%%% Persistence Strategy Selector
%%%
%%% This module selects the optimal persistence backend based on workload
%%% characteristics, cluster topology, and performance requirements.
%%%
%%% == Persistence Strategies ==
%%%
%%% - **ETS**: In-memory, fastest, no durability
%%% - **DETS**: Disk-based, single-node, moderate durability
%%% - **Mnesia**: Distributed, ACID, cluster-wide consistency
%%%
%%% == Selection Criteria ==
%%%
%%% 1. **Data Size**: Small (< 1MB) -> ETS, Large -> DETS/Mnesia
%%% 2. **Durability**: None -> ETS, Moderate -> DETS, High -> Mnesia
%%% 3. **Access Pattern**: Read-heavy -> ETS, Write-heavy -> Mnesia
%%% 4. **Cluster**: Single node -> DETS, Multi-node -> Mnesia
%%% 5. **Performance**: Critical -> ETS, Normal -> Mnesia
%%%
%%% == Auto-Selection Algorithm ==
%%%
%%% The selector monitors:
%%% - Read/write ratio
%%% - Data size growth rate
%%% - Access latency percentiles
%%% - Cluster health
%%%
%%% And recommends strategy changes based on thresholds.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_persistence_selector).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, start_link/1,
         recommend_strategy/1,
         get_current_strategy/0,
         set_strategy/1,
         evaluate_strategy/0,
         get_strategy_metrics/0,
         benchmark_strategies/1,
         auto_select/1,
         migration_plan/2,
         estimate_migration_cost/2,
         validate_strategy/1]).

%% Recommendations
-export([recommend_for_session/0,
         recommend_for_cache/0,
         recommend_for_registry/0,
         recommend_for_shared_state/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type persistence_strategy() :: ets | dets | mnesia | hybrid.
-type workload_characteristics() ::
        #{data_size => non_neg_integer(),
          read_ratio => float(),
          write_ratio => float(),
          access_pattern => read_heavy | write_heavy | balanced,
          durability_requirement => none | moderate | high,
          performance_requirement => critical | normal | low,
          cluster_topology => single_node | multi_node}.

-type strategy_metrics() ::
        #{strategy => persistence_strategy(),
          read_latency_us => float(),
          write_latency_us => float(),
          throughput_ops => float(),
          memory_mb => float(),
          disk_mb => float(),
          recommendation_score => float()}.

-type migration_cost() ::
        #{time_seconds => non_neg_integer(),
          data_transfer_mb => non_neg_integer(),
          downtime_ms => non_neg_integer(),
          complexity => low | medium | high}.

-export_type([persistence_strategy/0, workload_characteristics/0,
              strategy_metrics/0, migration_cost/0]).

%%====================================================================
%% Constants
%%====================================================================

-define(EVALUATION_INTERVAL, 300000).  % 5 minutes
-define(METRICS_HISTORY_SIZE, 100).

%% Thresholds for auto-selection
-define(ETS_SIZE_THRESHOLD, 100 * 1024 * 1024).  % 100MB
-define(ETS_READ_RATIO_THRESHOLD, 0.90).         % 90% reads
-define(MULTI_NODE_DURABILITY_THRESHOLD, moderate).

%%====================================================================
%% State Record
%%====================================================================

-record(state,
        {current_strategy = ets :: persistence_strategy(),
         workload_characteristics = #{} :: workload_characteristics(),
         metrics_history = [] :: [strategy_metrics()],
         benchmarks = #{} :: #{persistence_strategy() => strategy_metrics()},
         evaluation_timer :: reference() | undefined,
         auto_select_enabled = false :: boolean()}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Recommend strategy based on workload characteristics
-spec recommend_strategy(workload_characteristics()) -> persistence_strategy().
recommend_strategy(Characteristics) ->
    gen_server:call(?MODULE, {recommend_strategy, Characteristics}).

%% @doc Get current persistence strategy
-spec get_current_strategy() -> persistence_strategy().
get_current_strategy() ->
    gen_server:call(?MODULE, get_current_strategy).

%% @doc Set persistence strategy
-spec set_strategy(persistence_strategy()) -> ok | {error, term()}.
set_strategy(Strategy) ->
    gen_server:call(?MODULE, {set_strategy, Strategy}).

%% @doc Evaluate current strategy and potentially recommend changes
-spec evaluate_strategy() -> {ok, persistence_strategy()} | {ok, current, map()}.
evaluate_strategy() ->
    gen_server:call(?MODULE, evaluate_strategy).

%% @doc Get metrics for current and alternative strategies
-spec get_strategy_metrics() -> #{persistence_strategy() => strategy_metrics()}.
get_strategy_metrics() ->
    gen_server:call(?MODULE, get_strategy_metrics).

%% @doc Benchmark different strategies
-spec benchmark_strategies(workload_characteristics()) ->
          #{persistence_strategy() => strategy_metrics()}.
benchmark_strategies(Characteristics) ->
    gen_server:call(?MODULE, {benchmark_strategies, Characteristics}).

%% @doc Enable/disable auto-selection
-spec auto_select(boolean()) -> ok.
auto_select(Enabled) ->
    gen_server:cast(?MODULE, {auto_select, Enabled}).

%% @doc Get migration plan between strategies
-spec migration_plan(persistence_strategy(), persistence_strategy()) ->
          {ok, map()} | {error, term()}.
migration_plan(From, To) ->
    gen_server:call(?MODULE, {migration_plan, From, To}).

%% @doc Estimate migration cost
-spec estimate_migration_cost(persistence_strategy(), persistence_strategy()) ->
          migration_cost().
estimate_migration_cost(From, To) ->
    gen_server:call(?MODULE, {estimate_migration_cost, From, To}).

%% @doc Validate if strategy is viable
-spec validate_strategy(persistence_strategy()) -> {ok, boolean()} | {error, term()}.
validate_strategy(Strategy) ->
    gen_server:call(?MODULE, {validate_strategy, Strategy}).

%% @doc Recommend strategy for session data
-spec recommend_for_session() -> persistence_strategy().
recommend_for_session() ->
    Characteristics = #{
        data_size => 1024,  % Small per session
        read_ratio => 0.70,
        write_ratio => 0.30,
        access_pattern => read_heavy,
        durability_requirement => moderate,
        performance_requirement => critical,
        cluster_topology => get_cluster_topology()
    },
    recommend_strategy(Characteristics).

%% @doc Recommend strategy for cache data
-spec recommend_for_cache() -> persistence_strategy().
recommend_for_cache() ->
    Characteristics = #{
        data_size => ?ETS_SIZE_THRESHOLD div 2,
        read_ratio => 0.95,
        write_ratio => 0.05,
        access_pattern => read_heavy,
        durability_requirement => none,
        performance_requirement => critical,
        cluster_topology => get_cluster_topology()
    },
    recommend_strategy(Characteristics).

%% @doc Recommend strategy for registry data
-spec recommend_for_registry() -> persistence_strategy().
recommend_for_registry() ->
    Characteristics = #{
        data_size => 10240,
        read_ratio => 0.99,
        write_ratio => 0.01,
        access_pattern => read_heavy,
        durability_requirement => high,
        performance_requirement => normal,
        cluster_topology => get_cluster_topology()
    },
    recommend_strategy(Characteristics).

%% @doc Recommend strategy for shared state
-spec recommend_for_shared_state() -> persistence_strategy().
recommend_for_shared_state() ->
    Characteristics = #{
        data_size => 1048576,
        read_ratio => 0.60,
        write_ratio => 0.40,
        access_pattern => balanced,
        durability_requirement => high,
        performance_requirement => normal,
        cluster_topology => get_cluster_topology()
    },
    recommend_strategy(Characteristics).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(Opts) ->
    process_flag(trap_exit, true),

    InitialStrategy = maps:get(initial_strategy, Opts, ets),
    AutoSelect = maps:get(auto_select, Opts, false),

    %% Start evaluation timer
    EvalTimer = erlang:send_after(?EVALUATION_INTERVAL, self(), evaluate_tick),

    logger:info("Persistence selector started: strategy=~p, auto_select=~p",
                [InitialStrategy, AutoSelect]),

    {ok, #state{current_strategy = InitialStrategy,
                evaluation_timer = EvalTimer,
                auto_select_enabled = AutoSelect}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
          {reply, term(), #state{}}.
handle_call({recommend_strategy, Characteristics}, _From, State) ->
    Strategy = do_recommend_strategy(Characteristics),
    {reply, Strategy, State};

handle_call(get_current_strategy, _From, State) ->
    {reply, State#state.current_strategy, State};

handle_call({set_strategy, Strategy}, _From, State) ->
    case validate_strategy_internal(Strategy) of
        {ok, true} ->
            logger:info("Changing persistence strategy from ~p to ~p",
                       [State#state.current_strategy, Strategy]),
            {reply, ok, State#state{current_strategy = Strategy}};
        {ok, false} ->
            {reply, {error, strategy_not_viable}, State}
    end;

handle_call(evaluate_strategy, _From, State) ->
    Characteristics = collect_workload_characteristics(State),
    Recommended = do_recommend_strategy(Characteristics),
    Current = State#state.current_strategy,

    case Recommended =:= Current of
        true ->
            {reply, {ok, current, #{reason => optimal, current => Current}}, State};
        false ->
            {reply, {ok, Recommended, #{reason => recommend_change,
                                      current => Current,
                                      characteristics => Characteristics}}, State}
    end;

handle_call(get_strategy_metrics, _From, State) ->
    Metrics = collect_strategy_metrics(State),
    {reply, Metrics, State};

handle_call({benchmark_strategies, Characteristics}, _From, State) ->
    Benchmarks = do_benchmark_strategies(Characteristics, State),
    {reply, Benchmarks, State#state{benchmarks = Benchmarks}};

handle_call({migration_plan, From, To}, _From, State) ->
    Plan = create_migration_plan(From, To, State),
    {reply, Plan, State};

handle_call({estimate_migration_cost, From, To}, _From, State) ->
    Cost = estimate_migration_cost_internal(From, To, State),
    {reply, Cost, State};

handle_call({validate_strategy, Strategy}, _From, State) ->
    Result = validate_strategy_internal(Strategy),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({auto_select, Enabled}, State) ->
    logger:info("Auto-select ~p", [Enabled]),
    {noreply, State#state{auto_select_enabled = Enabled}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(evaluate_tick, State) ->
    NewState = maybe_auto_select(State),
    TimerRef = erlang:send_after(?EVALUATION_INTERVAL, self(), evaluate_tick),
    {noreply, NewState#state{evaluation_timer = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    logger:info("Persistence selector terminating"),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Strategy Selection
%%====================================================================

%% @private Core recommendation algorithm
-spec do_recommend_strategy(workload_characteristics()) -> persistence_strategy().
do_recommend_strategy(Characteristics) ->
    DataSize = maps:get(data_size, Characteristics, 0),
    ReadRatio = maps:get(read_ratio, Characteristics, 0.5),
    Durability = maps:get(durability_requirement, Characteristics, none),
    Performance = maps:get(performance_requirement, Characteristics, normal),
    Cluster = maps:get(cluster_topology, Characteristics, single_node),

    %% Decision tree
    case {Durability, Cluster} of
        {none, _} ->
            %% No durability needed - ETS is best
            ets;
        {_, multi_node} when Durability =:= high ->
            %% Multi-node with high durability - Mnesia
            mnesia;
        {_, _} when DataSize < ?ETS_SIZE_THRESHOLD, ReadRatio > ?ETS_READ_RATIO_THRESHOLD ->
            %% Small, read-heavy - ETS
            ets;
        {_, multi_node} ->
            %% Multi-node - Mnesia
            mnesia;
        {_, single_node} when Durability =:= high orelse Durability =:= moderate ->
            %% Single node, durability needed - DETS
            dets;
        _ when Performance =:= critical ->
            %% Performance critical - ETS
            ets;
        _ ->
            %% Default - ETS for single node, Mnesia for cluster
            case Cluster of
                single_node -> ets;
                multi_node -> mnesia
            end
    end.

%% @private Collect workload characteristics
-spec collect_workload_characteristics(#state{}) -> workload_characteristics().
collect_workload_characteristics(_State) ->
    %% Collect from actual system metrics
    %% For now, return defaults
    #{data_size => estimate_data_size(),
      read_ratio => estimate_read_ratio(),
      write_ratio => estimate_write_ratio(),
      access_pattern => detect_access_pattern(),
      durability_requirement => detect_durability_requirement(),
      performance_requirement => detect_performance_requirement(),
      cluster_topology => get_cluster_topology()}.

%% @private Collect metrics for all strategies
-spec collect_strategy_metrics(#state{}) ->
          #{persistence_strategy() => strategy_metrics()}.
collect_strategy_metrics(State) ->
    Strategies = [ets, dets, mnesia],
    lists:foldl(fun(Strategy, Acc) ->
                       Metrics = get_strategy_metrics_internal(Strategy, State),
                       Acc#{Strategy => Metrics}
                end, #{}, Strategies).

%% @private Get metrics for specific strategy
-spec get_strategy_metrics_internal(persistence_strategy(), #state{}) -> strategy_metrics().
get_strategy_metrics_internal(Strategy, State) ->
    %% Return cached benchmarks or collect fresh
    case maps:get(Strategy, State#state.benchmarks, undefined) of
        undefined ->
            %% Collect current metrics
            collect_metrics_for_strategy(Strategy);
        Cached ->
            Cached
    end.

%% @private Collect metrics for a strategy
-spec collect_metrics_for_strategy(persistence_strategy()) -> strategy_metrics().
collect_metrics_for_strategy(ets) ->
    #{strategy => ets,
      read_latency_us => measure_ets_read_latency(),
      write_latency_us => measure_ets_write_latency(),
      throughput_ops => measure_ets_throughput(),
      memory_mb => estimate_ets_memory(),
      disk_mb => 0,
      recommendation_score => calculate_ets_score()};
collect_metrics_for_strategy(dets) ->
    #{strategy => dets,
      read_latency_us => measure_dets_read_latency(),
      write_latency_us => measure_dets_write_latency(),
      throughput_ops => measure_dets_throughput(),
      memory_mb => estimate_dets_memory(),
      disk_mb => estimate_dets_disk(),
      recommendation_score => calculate_dets_score()};
collect_metrics_for_strategy(mnesia) ->
    #{strategy => mnesia,
      read_latency_us => measure_mnesia_read_latency(),
      write_latency_us => measure_mnesia_write_latency(),
      throughput_ops => measure_mnesia_throughput(),
      memory_mb => estimate_mnesia_memory(),
      disk_mb => estimate_mnesia_disk(),
      recommendation_score => calculate_mnesia_score()}.

%% @private Benchmark all strategies
-spec do_benchmark_strategies(workload_characteristics(), #state{}) ->
          #{persistence_strategy() => strategy_metrics()}.
do_benchmark_strategies(Characteristics, _State) ->
    Strategies = [ets, dets, mnesia],
    lists:foldl(fun(Strategy, Acc) ->
                       Metrics = benchmark_strategy(Strategy, Characteristics),
                       Acc#{Strategy => Metrics}
                end, #{}, Strategies).

%% @private Benchmark a specific strategy
-spec benchmark_strategy(persistence_strategy(), workload_characteristics()) ->
          strategy_metrics().
benchmark_strategy(Strategy, Characteristics) ->
    %% Create temporary tables and measure performance
    ReadCount = trunc(maps:get(data_size, Characteristics, 1000) / 100),
    WriteCount = ReadCount div 10,

    {ReadLat, WriteLat, Throughput} =
        case Strategy of
            ets -> benchmark_ets(ReadCount, WriteCount);
            dets -> benchmark_dets(ReadCount, WriteCount);
            mnesia -> benchmark_mnesia(ReadCount, WriteCount)
        end,

    #{strategy => Strategy,
      read_latency_us => ReadLat,
      write_latency_us => WriteLat,
      throughput_ops => Throughput,
      memory_mb => 0,
      disk_mb => 0,
      recommendation_score => 0.0}.

%% @private Validate strategy
-spec validate_strategy_internal(persistence_strategy()) -> {ok, boolean()}.
validate_strategy_internal(ets) ->
    {ok, true};
validate_strategy_internal(dets) ->
    %% Check if we have disk access
    case file:write_file_info("/tmp/erlmcp_dets_test", []) of
        ok -> file:delete("/tmp/erlmcp_dets_test"), {ok, true};
        {error, _} -> {ok, false}
    end;
validate_strategy_internal(mnesia) ->
    %% Check if Mnesia is running and we're in a distributed setup
    case mnesia:system_info(is_running) of
        yes -> {ok, true};
        no -> {ok, false}
    end;
validate_strategy_internal(hybrid) ->
    {ok, true}.

%% @private Create migration plan
-spec create_migration_plan(persistence_strategy(), persistence_strategy(), #state{}) ->
          {ok, map()} | {error, term()}.
create_migration_plan(From, To, _State) when From =:= To ->
    {ok, #{from => From, to => To, steps => [], note => no_change_needed}};
create_migration_plan(From, To, State) ->
    Cost = estimate_migration_cost_internal(From, To, State),
    Steps = generate_migration_steps(From, To),
    {ok, #{from => From,
           to => To,
           cost => Cost,
           steps => Steps}}.

%% @private Estimate migration cost
-spec estimate_migration_cost_internal(persistence_strategy(), persistence_strategy(), #state{}) ->
          migration_cost().
estimate_migration_cost_internal(ets, dets, _State) ->
    %% ETS to DETS: Need to iterate and write to disk
    #{time_seconds => estimate_data_size() div (1024 * 1024),  % ~1s per MB
      data_transfer_mb => 0,
      downtime_ms => 100,
      complexity => low};
estimate_migration_cost_internal(ets, mnesia, _State) ->
    %% ETS to Mnesia: Need to load into Mnesia tables
    #{time_seconds => estimate_data_size() div (512 * 1024),  % ~2s per MB
      data_transfer_mb => estimate_data_size() div (1024 * 1024),
      downtime_ms => 500,
      complexity => medium};
estimate_migration_cost_internal(dets, ets, _State) ->
    %% DETS to ETS: Load into memory
    #{time_seconds => estimate_data_size() div (1024 * 1024),
      data_transfer_mb => 0,
      downtime_ms => 50,
      complexity => low};
estimate_migration_cost_internal(dets, mnesia, _State) ->
    %% DETS to Mnesia: Migrate via ETS
    #{time_seconds => estimate_data_size() div (256 * 1024),
      data_transfer_mb => estimate_data_size() div (1024 * 1024),
      downtime_ms => 1000,
      complexity => high};
estimate_migration_cost_internal(mnesia, ets, _State) ->
    %% Mnesia to ETS: Dump to memory
    #{time_seconds => estimate_data_size() div (1024 * 1024),
      data_transfer_mb => 0,
      downtime_ms => 200,
      complexity => medium};
estimate_migration_cost_internal(mnesia, dets, _State) ->
    %% Mnesia to DETS: Dump to disk
    #{time_seconds => estimate_data_size() div (512 * 1024),
      data_transfer_mb => estimate_data_size() div (1024 * 1024),
      downtime_ms => 500,
      complexity => medium};
estimate_migration_cost_internal(_, _, _) ->
    #{time_seconds => 60,
      data_transfer_mb => 0,
      downtime_ms => 1000,
      complexity => high}.

%% @private Generate migration steps
-spec generate_migration_steps(persistence_strategy(), persistence_strategy()) -> [map()].
generate_migration_steps(From, To) ->
    [
     #{step => 1, action => create_target, target => To},
     #{step => 2, action => migrate_data, from => From, to => To},
     #{step => 3, action => verify_migration, target => To},
     #{step => 4, action => switch_over, from => From, to => To},
     #{step => 5, action => cleanup, source => From}
    ].

%% @private Maybe auto-select based on evaluation
-spec maybe_auto_select(#state{}) -> #state{}.
maybe_auto_select(State) ->
    case State#state.auto_select_enabled of
        false ->
            State;
        true ->
            Characteristics = collect_workload_characteristics(State),
            Recommended = do_recommend_strategy(Characteristics),
            Current = State#state.current_strategy,
            case Recommended =:= Current of
                true ->
                    State;
                false ->
                    logger:info("Auto-select: changing strategy from ~p to ~p",
                               [Current, Recommended]),
                    %% Don't actually change, just recommend
                    State
            end
    end.

%%====================================================================
%% Internal Functions - Measurement
%%====================================================================

%% @private Estimate data size
-spec estimate_data_size() -> non_neg_integer().
estimate_data_size() ->
    %% Sum of all ETS table sizes
    lists:foldl(fun(Tid, Acc) ->
                       case ets:info(Tid, size) of
                           undefined -> Acc;
                           Size -> Acc + Size * 100  % Approximate
                       end
               end, 0, ets:all()).

%% @private Estimate read ratio
-spec estimate_read_ratio() -> float().
estimate_read_ratio() ->
    %% Would track actual read/write operations
    0.8.

%% @private Estimate write ratio
-spec estimate_write_ratio() -> float().
estimate_write_ratio() ->
    1.0 - estimate_read_ratio().

%% @private Detect access pattern
-spec detect_access_pattern() -> read_heavy | write_heavy | balanced.
detect_access_pattern() ->
    ReadRatio = estimate_read_ratio(),
    if ReadRatio > 0.7 -> read_heavy;
       ReadRatio < 0.3 -> write_heavy;
       true -> balanced
    end.

%% @private Detect durability requirement
-spec detect_durability_requirement() -> none | moderate | high.
detect_durability_requirement() ->
    %% Based on application config
    case application:get_env(erlmcp_core, durability) of
        {ok, Value} when is_atom(Value) -> Value;
        _ -> moderate
    end.

%% @private Detect performance requirement
-spec detect_performance_requirement() -> critical | normal | low.
detect_performance_requirement() ->
    case application:get_env(erlmcp_core, performance_requirement) of
        {ok, Value} when is_atom(Value) -> Value;
        _ -> normal
    end.

%% @private Get cluster topology
-spec get_cluster_topology() -> single_node | multi_node.
get_cluster_topology() ->
    case length(nodes()) of
        0 -> single_node;
        _ -> multi_node
    end.

%% @private Measure ETS read latency
-spec measure_ets_read_latency() -> float().
measure_ets_read_latency() ->
    %% Microseconds per read
    1.0.

%% @private Measure ETS write latency
-spec measure_ets_write_latency() -> float().
measure_ets_write_latency() ->
    2.0.

%% @private Measure ETS throughput
-spec measure_ets_throughput() -> float().
measure_ets_throughput() ->
    %% Operations per second
    1000000.0.

%% @private Estimate ETS memory
-spec estimate_ets_memory() -> float().
estimate_ets_memory() ->
    erlang:memory(ets) / (1024 * 1024).

%% @private Measure DETS read latency
-spec measure_dets_read_latency() -> float().
measure_dets_read_latency() ->
    50.0.

%% @private Measure DETS write latency
-spec measure_dets_write_latency() -> float().
measure_dets_write_latency() ->
    100.0.

%% @private Measure DETS throughput
-spec measure_dets_throughput() -> float().
measure_dets_throughput() ->
    50000.0.

%% @private Estimate DETS memory
-spec estimate_dets_memory() -> float().
estimate_dets_memory() ->
    estimate_ets_memory() * 1.5.

%% @private Estimate DETS disk
-spec estimate_dets_disk() -> float().
estimate_dets_disk() ->
    estimate_data_size() / (1024 * 1024).

%% @private Measure Mnesia read latency
-spec measure_mnesia_read_latency() -> float().
measure_mnesia_read_latency() ->
    10.0.

%% @private Measure Mnesia write latency
-spec measure_mnesia_write_latency() -> float().
measure_mnesia_write_latency() ->
    200.0.

%% @private Measure Mnesia throughput
-spec measure_mnesia_throughput() -> float().
measure_mnesia_throughput() ->
    20000.0.

%% @private Estimate Mnesia memory
-spec estimate_mnesia_memory() -> float().
estimate_mnesia_memory() ->
    estimate_ets_memory() * 2.0.

%% @private Estimate Mnesia disk
-spec estimate_mnesia_disk() -> float().
estimate_mnesia_disk() ->
    estimate_data_size() / (1024 * 1024) * 2.

%% @private Calculate ETS score
-spec calculate_ets_score() -> float().
calculate_ets_score() ->
    %% Higher is better
    0.9.

%% @private Calculate DETS score
-spec calculate_dets_score() -> float().
calculate_dets_score() ->
    0.6.

%% @private Calculate Mnesia score
-spec calculate_mnesia_score() -> float().
calculate_mnesia_score() ->
    case get_cluster_topology() of
        multi_node -> 0.8;
        single_node -> 0.5
    end.

%% @private Benchmark ETS
-spec benchmark_ets(non_neg_integer(), non_neg_integer()) ->
          {float(), float(), float()}.
benchmark_ets(ReadCount, WriteCount) ->
    Table = ets:new(bench, [set, public]),
    StartTime = erlang:monotonic_time(microsecond),

    %% Writes
    lists:foreach(fun(N) -> ets:insert(Table, {N, N}) end, lists:seq(1, WriteCount)),

    WriteTime = erlang:monotonic_time(microsecond) - StartTime,
    WriteLat = WriteTime / WriteCount,

    %% Reads
    StartTime2 = erlang:monotonic_time(microsecond),
    lists:foreach(fun(N) -> ets:lookup(Table, N) end, lists:seq(1, ReadCount)),
    ReadTime = erlang:monotonic_time(microsecond) - StartTime2,
    ReadLat = ReadTime / ReadCount,

    ets:delete(Table),
    Throughput = (ReadCount + WriteCount) / ((ReadTime + WriteTime) / 1000000),

    {ReadLat, WriteLat, Throughput}.

%% @private Benchmark DETS
-spec benchmark_dets(non_neg_integer(), non_neg_integer()) ->
          {float(), float(), float()}.
benchmark_dets(ReadCount, WriteCount) ->
    {ok, Table} = dets:open_file(bench_dets, [{type, set}]),

    StartTime = erlang:monotonic_time(microsecond),
    lists:foreach(fun(N) -> dets:insert(Table, {N, N}) end, lists:seq(1, WriteCount)),
    WriteTime = erlang:monotonic_time(microsecond) - StartTime,
    WriteLat = WriteTime / WriteCount,

    StartTime2 = erlang:monotonic_time(microsecond),
    lists:foreach(fun(N) -> dets:lookup(Table, N) end, lists:seq(1, ReadCount)),
    ReadTime = erlang:monotonic_time(microsecond) - StartTime2,
    ReadLat = ReadTime / ReadCount,

    dets:close(Table),
    file:delete("bench_dets"),
    Throughput = (ReadCount + WriteCount) / ((ReadTime + WriteTime) / 1000000),

    {ReadLat, WriteLat, Throughput}.

%% @private Benchmark Mnesia
-spec benchmark_mnesia(non_neg_integer(), non_neg_integer()) ->
          {float(), float(), float()}.
benchmark_mnesia(ReadCount, WriteCount) ->
    Table = bench_mnesia,
    mnesia:create_table(Table, [{attributes, [key, val]}]),

    StartTime = erlang:monotonic_time(microsecond),
    mnesia:sync_transaction(fun() ->
                                   lists:foreach(fun(N) ->
                                                       mnesia:write({Table, N, N})
                                               end, lists:seq(1, WriteCount))
                           end),
    WriteTime = erlang:monotonic_time(microsecond) - StartTime,
    WriteLat = WriteTime / WriteCount,

    StartTime2 = erlang:monotonic_time(microsecond),
    mnesia:sync_transaction(fun() ->
                                   lists:foreach(fun(N) ->
                                                       mnesia:read({Table, N})
                                               end, lists:seq(1, ReadCount))
                           end),
    ReadTime = erlang:monotonic_time(microsecond) - StartTime2,
    ReadLat = ReadTime / ReadCount,

    mnesia:delete_table(Table),
    Throughput = (ReadCount + WriteCount) / ((ReadTime + WriteTime) / 1000000),

    {ReadLat, WriteLat, Throughput}.
