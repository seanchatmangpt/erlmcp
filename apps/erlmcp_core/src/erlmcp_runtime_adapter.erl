%%%-------------------------------------------------------------------
%%% @doc
%%% Runtime Adaptation and Optimization
%%%
%%% This module provides runtime adaptation to optimize performance
%%% based on available OTP features and system capabilities.
%%%
%%% Adaptation Strategies:
%%%   - Version-specific optimizations
%%%   - Feature-based configuration
%%%   - Performance monitoring
%%%   - Resource allocation
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

-include("otp_compat.hrl").

%% Types
-type optimization_level() :: conservative | balanced | optimal.
-type resource_pool() :: {
    pool_size :: pos_integer(),
    max_overflow :: pos_integer(),
    timeout :: pos_integer()
}.
-type batch_config() :: {
    size :: pos_integer(),
    timeout :: pos_integer(),
    max_retries :: pos_integer()
}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Adapt to current OTP version
-spec adapt_to_otp_version(otp_version()) -> any().
adapt_to_otp_version(OtpVersion) ->
    SupportLevel = erlmcp_version_detector:get_support_level(OtpVersion),
    logger:info("Adapting to OTP version ~p with support level ~p", [OtpVersion, SupportLevel]),

    % Apply version-specific optimizations
    apply_version_optimizations(SupportLevel),

    % Configure resource pools
    configure_resource_pools(SupportLevel),

    % Set timeouts and limits
    configure_timeouts_and_limits(SupportLevel),

    % Configure monitoring
    configure_monitoring(SupportLevel),

    % Apply feature-specific optimizations
    apply_feature_optimizations(),

    ok.

%% @doc Optimize system for performance
-spec optimize_for_performance(optimization_level()) -> any().
optimize_for_performance(OptimizationLevel) ->
    logger:info("Optimizing for performance level: ~p", [OptimizationLevel]),

    % Adjust memory settings
    adjust_memory_settings(OptimizationLevel),

    % Optimize connection handling
    optimize_connections(OptimizationLevel),

    % Optimize message processing
    optimize_message_processing(OptimizationLevel),

    % Enable performance monitoring
    enable_performance_monitoring(OptimizationLevel),

    ok.

%% @doc Handle feature gaps gracefully
-spec handle_feature_gaps([feature_flag()]) -> any().
handle_feature_gaps(FeatureFlags) ->
    logger:info("Handling feature gaps: ~p", [FeatureFlags]),

    % Handle JSON library gap
    case maps:get(native_json, FeatureFlags, false) of
        false ->
            logger:info("Using JSON fallback (JSX)"),
            erlmcp_json_fallback:enable_fallback();
        true ->
            logger:info("Using native JSON module"),
            ok
    end,

    % Handle process iterator gap
    case maps:get(process_iterator, FeatureFlags, false) of
        false ->
            logger:warning("Process iterator not available, using legacy enumeration"),
            erlmcp_process_legacy:enable_legacy_mode();
        true ->
            logger:info("Using modern process iterator"),
            ok
    end,

    % Handle priority messages gap
    case maps:get(priority_messages, FeatureFlags, false) of
        false ->
            logger:info("Priority messages not available, using normal ordering"),
            erlmcp_message_normal:enable_normal_mode();
        true ->
            logger:info("Using priority message scheduling"),
            ok
    end,

    % Handle advanced features gap
    handle_advanced_feature_gaps(FeatureFlags),

    ok.

%% @doc Get optimal pool size based on available features
-spec get_optimal_pool_size() -> pos_integer().
get_optimal_pool_size() ->
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(Version),

    case SupportLevel of
        unsupported -> 5;
        legacy -> 10;
        stable -> 20;
        recommended -> 50
    end.

%% @doc Get optimal timeout based on available features
-spec get_optimal_timeout() -> pos_integer().
get_optimal_timeout() ->
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(Version),

    case SupportLevel of
        unsupported -> 30000;  % 30s
        legacy -> 20000;      % 20s
        stable -> 15000;      % 15s
        recommended -> 5000   % 5s
    end.

%% @doc Get optimal batch size based on available features
-spec get_optimal_batch_size() -> pos_integer().
get_optimal_batch_size() ->
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(Version),

    case SupportLevel of
        unsupported -> 10;
        legacy -> 50;
        stable -> 100;
        recommended -> 500
    end.

%% @doc Start performance monitoring
-spec monitor_performance() -> ok.
monitor_performance() ->
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(Version),

    case SupportLevel of
        unsupported ->
            start_monitoring_timer(60000);  % 60s interval
        legacy ->
            start_monitoring_timer(30000);  % 30s interval
        stable ->
            start_monitoring_timer(15000);  % 15s interval
        recommended ->
            start_monitoring_timer(5000)    % 5s interval
    end,

    logger:info("Performance monitoring started for ~p", [SupportLevel]),
    ok.

%% @doc Dynamically adjust settings based on system load
-spec adjust_settings_dynamically(map()) -> any().
adjust_settings_dynamically(SystemMetrics) ->
    Metrics = normalize_metrics(SystemMetrics),

    % Adjust pool sizes based on load
    adjust_pool_sizes(Metrics),

    % Adjust timeouts based on performance
    adjust_timeouts(Metrics),

    % Adjust batch sizes based on memory
    adjust_batch_sizes(Metrics),

    % Alert if needed
    check_alert_thresholds(Metrics),

    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Apply version-specific optimizations
-spec apply_version_optimizations(erlmcp_version_detector:support_level()) -> any().
apply_version_optimizations(unsupported) ->
    % Basic configuration for unsupported versions
    application:set_env(erlmcp, optimization_mode, basic);
apply_version_optimizations(legacy) ->
    % Legacy mode optimizations
    application:set_env(erlmcp, optimization_mode, legacy),
    erlmcp_legacy_optimizations:apply();
apply_version_optimizations(stable) ->
    % Stable mode optimizations
    application:set_env(erlmcp, optimization_mode, stable),
    erlmcp_stable_optimizations:apply();
apply_version_optimizations(recommended) ->
    % Optimal mode optimizations
    application:set_env(erlmcp, optimization_mode, optimal),
    erlmcp_optimal_optimizations:apply().

%% @private Configure resource pools
-spec configure_resource_pools(erlmcp_version_detector:support_level()) -> any().
configure_resource_pools(SupportLevel) ->
    PoolSize = get_optimal_pool_size(),
    MaxOverflow = PoolSize div 2,
    Timeout = get_optimal_timeout(),

    PoolConfig = #{
        pool_size => PoolSize,
        max_overflow => MaxOverflow,
        timeout => Timeout
    },

    application:set_env(erlmcp, resource_pools, PoolConfig),

    % Configure specific pools
    configure_specific_pools(SupportLevel, PoolConfig).

%% @private Configure specific pools
-spec configure_specific_pools(erlmcp_version_detector:support_level(), map()) -> any().
configure_specific_pools(SupportLevel, BaseConfig) ->
    % Connection pool
    ConnectionPool = maps:merge(BaseConfig, #{
        max_connections => case SupportLevel of
            unsupported -> 100;
            legacy -> 500;
            stable -> 2000;
            recommended -> 5000
        end
    }),

    % Process pool
    ProcessPool = maps:merge(BaseConfig, #{
        max_processes => case SupportLevel of
            unsupported -> 50;
            legacy -> 200;
            stable -> 1000;
            recommended -> 5000
        end
    }),

    % Database pool
    DbPool = maps:merge(BaseConfig, #{
        max_connections => case SupportLevel of
            unsupported -> 10;
            legacy -> 20;
            stable -> 50;
            recommended -> 100
        end
    }),

    Pools = #{
        connection => ConnectionPool,
        process => ProcessPool,
        database => DbPool
    },

    application:set_env(erlmcp, specific_pools, Pools).

%% @private Configure timeouts and limits
-spec configure_timeouts_and_limits(erlmcp_version_detector:support_level()) -> any().
configure_timeouts_and_limits(SupportLevel) ->
    Timeout = get_optimal_timeout(),
    BatchSize = get_optimal_batch_size(),

    % Configure timeouts
    application:set_env(erlmcp, timeouts, #{
        connection => Timeout,
        request => Timeout div 2,
        response => Timeout,
        operation => Timeout div 4
    }),

    % Configure limits
    application:set_env(erlmcp, limits, #{
        batch_size => BatchSize,
        max_retries => case SupportLevel of
            unsupported -> 3;
            legacy -> 5;
            stable -> 10;
            recommended -> 20
        end,
        max_pending => case SupportLevel of
            unsupported -> 100;
            legacy -> 500;
            stable -> 2000;
            recommended -> 10000
        end
    }).

%% @private Configure monitoring
-spec configure_monitoring(erlmcp_version_detector:support_level()) -> any().
configure_monitoring(SupportLevel) ->
    MonitorInterval = case SupportLevel of
        unsupported -> 60000;  % 60s
        legacy -> 30000;      % 30s
        stable -> 15000;      % 15s
        recommended -> 5000    % 5s
    end,

    application:set_env(erlmcp, monitoring, #{
        enabled => true,
        interval => MonitorInterval,
        metrics => [
            memory_usage,
            cpu_usage,
            process_count,
            response_time,
            error_rate
        ]
    }).

%% @private Apply feature-specific optimizations
-spec apply_feature_optimizations() -> any().
apply_feature_optimizations() ->
    FeatureFlags = erlmcp_feature_detector:get_feature_flags(),

    % JSON optimization
    case maps:get(native_json, FeatureFlags, false) of
        true ->
            application:set_env(erlmcp, json_library, native);
        false ->
            application:set_env(erlmcp, json_library, native)
    end,

    % Process optimization
    case maps:get(process_iterator, FeatureFlags, false) of
        true ->
            application:set_env(erlmcp, process_method, iterator);
        false ->
            application:set_env(erlmcp, process_method, legacy)
    end,

    % Message optimization
    case maps:get(priority_messages, FeatureFlags, false) of
        true ->
            application:set_env(erlmcp, message_priority, true);
        false ->
            application:set_env(erlmcp, message_priority, false)
    end,

    % Memory optimization
    case maps:get(eep76_gc, FeatureFlags, false) of
        true ->
            application:set_env(erlmcp, gc_optimization, modern);
        false ->
            application:set_env(erlmcp, gc_optimization, legacy)
    end.

%% @private Adjust memory settings
-spec adjust_memory_settings(optimization_level()) -> any().
adjust_memory_settings(conservative) ->
    application:set_env(kernel, process_limit, 32768),
    application:set_env(erlang, max_heap_size, {64, mb});
adjust_memory_settings(balanced) ->
    application:set_env(kernel, process_limit, 65536),
    application:set_env(erlang, max_heap_size, {128, mb});
adjust_memory_settings(optimal) ->
    application:set_env(kernel, process_limit, 131072),
    application:set_env(erlang, max_heap_size, {256, mb}).

%% @private Optimize connections
-spec optimize_connections(optimization_level()) -> any().
optimize_connections(conservative) ->
    application:set_env(erlmcp, connection_optimization, basic);
optimize_connections(balanced) ->
    application:set_env(erlmcp, connection_optimization, standard);
optimize_connections(optimal) ->
    application:set_env(erlmcp, connection_optimization, advanced).

%% @private Optimize message processing
-spec optimize_message_processing(optimization_level()) -> any().
optimize_message_processing(conservative) ->
    application:set_env(erlmcp, message_optimization, sequential);
optimize_message_processing(balanced) ->
    application:set_env(erlmcp, message_optimization, parallel);
optimize_message_processing(optimal) ->
    application:set_env(erlmcp, message_optimization, priority_queue).

%% @private Enable performance monitoring
-spec enable_performance_monitoring(optimization_level()) -> any().
enable_performance_monitoring(conservative) ->
    application:set_env(erlmcp, performance_monitoring, light);
enable_performance_monitoring(balanced) ->
    application:set_env(erlmcp, performance_monitoring, standard);
enable_performance_monitoring(optimal) ->
    application:set_env(erlmcp, performance_monitoring, advanced).

%% @private Start monitoring timer
-spec start_monitoring_timer(pos_integer()) -> ok.
start_monitoring_timer(Interval) ->
    case erlang:whereis(erlmcp_performance_monitor) of
        undefined ->
            ok;
        Pid ->
            erlang:send_after(Interval, Pid, check_metrics)
    end.

%% @private Normalize system metrics
-spec normalize_metrics(map()) -> map().
normalize_metrics(Metrics) ->
    #{
        memory_usage => maps:get(memory_usage, Metrics, 0),
        cpu_usage => maps:get(cpu_usage, Metrics, 0),
        process_count => maps:get(process_count, Metrics, 0),
        response_time => maps:get(response_time, Metrics, 0),
        error_rate => maps:get(error_rate, Metrics, 0)
    }.

%% @private Adjust pool sizes based on metrics
-spec adjust_pool_sizes(map()) -> any().
adjust_pool_sizes(Metrics) ->
    BasePoolSize = get_optimal_pool_size(),

    case Metrics#{
        memory_usage := maps:get(memory_usage, Metrics, 0),
        cpu_usage := maps:get(cpu_usage, Metrics, 0)
    } of
        #{memory_usage := Mem, cpu_usage := Cpu} when Mem > 90 orelse Cpu > 90 ->
            % High load, reduce pool sizes
            application:set_env(erlmcp, resource_pools, #{
                pool_size => max(1, BasePoolSize div 2),
                max_overflow => max(1, BasePoolSize div 4),
                timeout => get_optimal_timeout() * 2
            });
        #{memory_usage := Mem, cpu_usage := Cpu} when Mem < 50 and Cpu < 50 ->
            % Low load, increase pool sizes
            application:set_env(erlmcp, resource_pools, #{
                pool_size => BasePoolSize * 2,
                max_overflow => BasePoolSize,
                timeout => max(1000, get_optimal_timeout() div 2)
            });
        _ ->
            % Normal load, keep default
            ok
    end.

%% @private Adjust timeouts based on metrics
-spec adjust_timeouts(map()) -> any().
adjust_timeouts(Metrics) ->
    BaseTimeout = get_optimal_timeout(),

    case Metrics#{
        response_time := maps:get(response_time, Metrics, 0)
    } of
        #{response_time := RT} when RT > 5000 ->
            % Slow responses, increase timeouts
            application:set_env(erlmcp, timeouts, #{
                connection => BaseTimeout * 2,
                request => BaseTimeout,
                response => BaseTimeout * 2,
                operation => max(1000, BaseTimeout div 2)
            });
        #{response_time := RT} when RT < 1000 ->
            % Fast responses, decrease timeouts
            application:set_env(erlmcp, timeouts, #{
                connection => max(1000, BaseTimeout div 2),
                request => max(500, BaseTimeout div 4),
                response => max(1000, BaseTimeout div 2),
                operation => max(250, BaseTimeout div 8)
            });
        _ ->
            ok
    end.

%% @private Adjust batch sizes based on metrics
-spec adjust_batch_sizes(map()) -> any().
adjust_batch_sizes(Metrics) ->
    BaseBatchSize = get_optimal_batch_size(),

    case Metrics#{
        memory_usage := maps:get(memory_usage, Metrics, 0),
        process_count := maps:get(process_count, Metrics, 0)
    } of
        #{memory_usage := Mem} when Mem > 85 ->
            % High memory usage, reduce batch size
            NewSize = max(1, BaseBatchSize div 2),
            application:set_env(erlmcp, limits, #{batch_size => NewSize});
        #{memory_usage := Mem} when Mem < 40 ->
            % Low memory usage, increase batch size
            NewSize = BaseBatchSize * 2,
            application:set_env(erlmcp, limits, #{batch_size => NewSize});
        _ ->
            ok
    end.

%% @private Check alert thresholds
-spec check_alert_thresholds(map()) -> any().
check_alert_thresholds(Metrics) ->
    Thresholds = application:get_env(erlmcp, alert_thresholds, #{
        memory_usage => 90,
        cpu_usage => 85,
        process_count => 100000
    }),

    case Metrics#{
        memory_usage := maps:get(memory_usage, Metrics, 0),
        cpu_usage := maps:get(cpu_usage, Metrics, 0),
        process_count := maps:get(process_count, Metrics, 0)
    } of
        #{memory_usage := Mem} when Mem >= maps:get(memory_usage, Thresholds) ->
            logger:alert("Memory usage critical: ~p%", [Mem]);
        #{cpu_usage := Cpu} when Cpu >= maps:get(cpu_usage, Thresholds) ->
            logger:alert("CPU usage critical: ~p%", [Cpu]);
        #{process_count := PidCount} when PidCount >= maps:get(process_count, Thresholds) ->
            logger:alert("Process count critical: ~p", [PidCount]);
        _ ->
            ok
    end.

%% @private Handle advanced feature gaps
-spec handle_advanced_feature_gaps(map()) -> any().
handle_advanced_feature_gaps(FeatureFlags) ->
    % Handle EEP 72 streams
    case maps:get(eep72_streams, FeatureFlags, false) of
        true ->
            logger:info("Using EEP 72 streams for data processing");
        false ->
            logger:info("Using legacy stream processing"),
            erlmcp_stream_fallback:enable_fallback()
    end,

    % Handle advanced maps
    case maps:get(advanced_maps, FeatureFlags, false) of
        true ->
            logger:info("Using advanced map operations");
        false ->
            logger:info("Using standard map operations")
    end.