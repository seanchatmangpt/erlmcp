%%%-------------------------------------------------------------------
%%% @doc
%%% Runtime Adaptation and Optimization
%%%
%%% This module provides runtime adaptation to optimize performance
%%% based on available OTP features and system capabilities.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_runtime_adapter).

%% API
-export([
    adapt_to_otp_version/1,
    optimize_for_performance/1,
    handle_feature_gaps/1,
    get_optimal_pool_size/0,
    get_optimal_timeout/0,
    get_optimal_batch_size/0,
    monitor_performance/0,
    adjust_settings_dynamically/1
]).

%% Types
-type optimization_level() :: conservative | balanced | optimal.
-type otp_version() :: {pos_integer(), pos_integer(), pos_integer()}.
-type feature_flag() :: atom().

%%====================================================================
%% API Functions
%%====================================================================

-spec adapt_to_otp_version(otp_version()) -> ok.
adapt_to_otp_version(OtpVersion) ->
    apply_version_optimizations(),
    configure_resource_pools(),
    configure_timeouts_and_limits(),
    configure_monitoring(),
    apply_feature_optimizations(),
    ok.

-spec optimize_for_performance(optimization_level()) -> ok.
optimize_for_performance(OptimizationLevel) ->
    adjust_memory_settings(OptimizationLevel),
    optimize_connections(OptimizationLevel),
    optimize_message_processing(OptimizationLevel),
    enable_performance_monitoring(OptimizationLevel),
    ok.

-spec handle_feature_gaps([feature_flag()]) -> ok.
handle_feature_gaps(FeatureFlags) ->
    case lists:member(priority_messages, FeatureFlags) of
        false ->
            erlmcp_message_normal:enable_normal_mode();
        true ->
            ok
    end,
    ok.

-spec get_optimal_pool_size() -> pos_integer().
get_optimal_pool_size() ->
    20.

-spec get_optimal_timeout() -> pos_integer().
get_optimal_timeout() ->
    15000.

-spec get_optimal_batch_size() -> pos_integer().
get_optimal_batch_size() ->
    100.

-spec monitor_performance() -> ok.
monitor_performance() ->
    ok.

-spec adjust_settings_dynamically(map()) -> ok.
adjust_settings_dynamically(SystemMetrics) ->
    Metrics = normalize_metrics(SystemMetrics),
    adjust_pool_sizes(Metrics),
    adjust_timeouts(Metrics),
    adjust_batch_sizes(Metrics),
    check_alert_thresholds(Metrics),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec apply_version_optimizations() -> ok.
apply_version_optimizations() ->
    application:set_env(erlmcp, optimization_mode, stable),
    ok.

-spec configure_resource_pools() -> ok.
configure_resource_pools() ->
    PoolSize = get_optimal_pool_size(),
    MaxOverflow = PoolSize div 2,
    Timeout = get_optimal_timeout(),
    PoolConfig = #{
        pool_size => PoolSize,
        max_overflow => MaxOverflow,
        timeout => Timeout
    },
    application:set_env(erlmcp, resource_pools, PoolConfig),
    ok.

-spec configure_timeouts_and_limits() -> ok.
configure_timeouts_and_limits() ->
    Timeout = get_optimal_timeout(),
    BatchSize = get_optimal_batch_size(),
    application:set_env(erlmcp, timeouts, #{
        connection => Timeout,
        request => Timeout div 2,
        response => Timeout,
        operation => Timeout div 4
    }),
    application:set_env(erlmcp, limits, #{
        batch_size => BatchSize,
        max_retries => 10,
        max_pending => 2000
    }),
    ok.

-spec configure_monitoring() -> ok.
configure_monitoring() ->
    application:set_env(erlmcp, monitoring, #{
        enabled => true,
        interval => 15000,
        metrics => [
            memory_usage,
            cpu_usage,
            process_count,
            response_time,
            error_rate
        ]
    }),
    ok.

-spec apply_feature_optimizations() -> ok.
apply_feature_optimizations() ->
    application:set_env(erlmcp, json_library, native),
    application:set_env(erlmcp, process_method, legacy),
    application:set_env(erlmcp, message_priority, false),
    application:set_env(erlmcp, gc_optimization, legacy),
    ok.

-spec adjust_memory_settings(optimization_level()) -> ok.
adjust_memory_settings(_OptimizationLevel) ->
    ok.

-spec optimize_connections(optimization_level()) -> ok.
optimize_connections(_OptimizationLevel) ->
    ok.

-spec optimize_message_processing(optimization_level()) -> ok.
optimize_message_processing(_OptimizationLevel) ->
    ok.

-spec enable_performance_monitoring(optimization_level()) -> ok.
enable_performance_monitoring(_OptimizationLevel) ->
    ok.

-spec normalize_metrics(map()) -> map().
normalize_metrics(Metrics) ->
    #{
        memory_usage => maps:get(memory_usage, Metrics, 0),
        cpu_usage => maps:get(cpu_usage, Metrics, 0),
        process_count => maps:get(process_count, Metrics, 0),
        response_time => maps:get(response_time, Metrics, 0),
        error_rate => maps:get(error_rate, Metrics, 0)
    }.

-spec adjust_pool_sizes(map()) -> ok.
adjust_pool_sizes(Metrics) ->
    BasePoolSize = get_optimal_pool_size(),
    Mem = maps:get(memory_usage, Metrics, 0),
    Cpu = maps:get(cpu_usage, Metrics, 0),

    case {Mem, Cpu} of
        {M, C} when M > 90 orelse C > 90 ->
            application:set_env(erlmcp, resource_pools, #{
                pool_size => max(1, BasePoolSize div 2),
                max_overflow => max(1, BasePoolSize div 4),
                timeout => get_optimal_timeout() * 2
            });
        {M, C} when M < 50 andalso C < 50 ->
            application:set_env(erlmcp, resource_pools, #{
                pool_size => BasePoolSize * 2,
                max_overflow => BasePoolSize,
                timeout => max(1000, get_optimal_timeout() div 2)
            });
        _ ->
            ok
    end,
    ok.

-spec adjust_timeouts(map()) -> ok.
adjust_timeouts(Metrics) ->
    BaseTimeout = get_optimal_timeout(),
    RT = maps:get(response_time, Metrics, 0),

    case RT of
        R when R > 5000 ->
            application:set_env(erlmcp, timeouts, #{
                connection => BaseTimeout * 2,
                request => BaseTimeout,
                response => BaseTimeout * 2,
                operation => max(1000, BaseTimeout div 2)
            });
        R when R < 1000 ->
            application:set_env(erlmcp, timeouts, #{
                connection => max(1000, BaseTimeout div 2),
                request => max(500, BaseTimeout div 4),
                response => max(1000, BaseTimeout div 2),
                operation => max(250, BaseTimeout div 8)
            });
        _ ->
            ok
    end,
    ok.

-spec adjust_batch_sizes(map()) -> ok.
adjust_batch_sizes(Metrics) ->
    BaseBatchSize = get_optimal_batch_size(),
    Mem = maps:get(memory_usage, Metrics, 0),

    case Mem of
        M when M > 85 ->
            NewSize = max(1, BaseBatchSize div 2),
            application:set_env(erlmcp, limits, #{batch_size => NewSize});
        M when M < 40 ->
            NewSize = BaseBatchSize * 2,
            application:set_env(erlmcp, limits, #{batch_size => NewSize});
        _ ->
            ok
    end,
    ok.

-spec check_alert_thresholds(map()) -> ok.
check_alert_thresholds(Metrics) ->
    Mem = maps:get(memory_usage, Metrics, 0),
    Cpu = maps:get(cpu_usage, Metrics, 0),
    PidCount = maps:get(process_count, Metrics, 0),

    case {Mem, Cpu, PidCount} of
        {M, _, _} when M >= 90 ->
            logger:alert("Memory usage critical: ~p%", [M]);
        {_, C, _} when C >= 85 ->
            logger:alert("CPU usage critical: ~p%", [C]);
        {_, _, P} when P >= 100000 ->
            logger:alert("Process count critical: ~p", [P]);
        _ ->
            ok
    end,
    ok.
