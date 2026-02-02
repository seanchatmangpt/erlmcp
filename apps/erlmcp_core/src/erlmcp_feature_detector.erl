%%%-------------------------------------------------------------------
%%% @doc
%%% Feature Detection and Runtime Configuration
%%%
%%% This module detects available features at runtime and provides
%%% version-specific configuration optimization.
%%%
%%% Features Detected:
%%%   - Native JSON module availability
%%%   - Process iterator API availability
%%%   - Priority message support
%%%   - Map capabilities
%%%   - Stream capabilities
%%%   - Garbage collection features
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_feature_detector).

%% API
-export([
    detect_features/0,
    get_feature_flags/0,
    is_feature_available/1,
    configure_for_version/1,
    warn_unsupported_features/0,
    get_optimal_settings/0
]).

%% Types
-include("otp_compat.hrl").

-type feature() ::
    native_json |
    process_iterator |
    priority_messages |
    eep48_maps |
    eep72_streams |
    eep76_gc |
    advanced_maps.

-type feature_flag() :: {feature(), boolean()}.
-type config_optimization() :: conservative | balanced | optimal.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Detect all available features
-spec detect_features() -> [feature_flag()].
detect_features() ->
    Version = erlmcp_version_detector:otp_version(),
    detect_features_for_version(Version).

%% @doc Get feature flags as a map
-spec get_feature_flags() -> map().
get_feature_flags() ->
    Features = detect_features(),
    maps:from_list(Features).

%% @doc Check if a specific feature is available
-spec is_feature_available(feature()) -> boolean().
is_feature_available(Feature) ->
    FeatureFlags = get_feature_flags(),
    maps:get(Feature, FeatureFlags, false).

%% @doc Configure system based on available features
-spec configure_for_version(otp_version()) -> any().
configure_for_version(Version) ->
    SupportLevel = erlmcp_version_detector:get_support_level(Version),
    case SupportLevel of
        unsupported ->
            throw({unsupported_version, Version});
        legacy ->
            configure_legacy_mode();
        stable ->
            configure_stable_mode();
        recommended ->
            configure_optimal_mode()
    end.

%% @doc Warn about unsupported or degraded features
-spec warn_unsupported_features() -> ok.
warn_unsupported_features() ->
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(Version),

    case SupportLevel of
        unsupported ->
            logger:error("OTP version ~p is not supported. Minimum required: OTP 26", [Version]);
        legacy ->
            logger:warning("OTP version ~p is legacy. Some features are degraded. Consider upgrade to OTP 28+", [Version]);
        stable ->
            logger:info("OTP version ~p provides stable features. Consider upgrade for optimal performance", [Version]);
        recommended ->
            logger:info("OTP version ~p provides optimal performance", [Version])
    end,

    % Specific feature warnings
    case erlmcp_version_detector:otp_version() of
        {V, _, _} when V < 28 ->
            logger:warning("Process iterators not available. Process enumeration may use more memory.");
        _ ->
            ok
    end,

    case erlmcp_version_detector:otp_version() of
        {V, _, _} when V < 27 ->
            logger:warning("Native JSON not available. Using JSX fallback.");
        _ ->
            ok
    end,

    ok.

%% @doc Get optimal settings based on available features
-spec get_optimal_settings() -> config_optimization().
get_optimal_settings() ->
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(Version),

    case SupportLevel of
        unsupported -> conservative;
        legacy -> conservative;
        stable -> balanced;
        recommended -> optimal
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Detect features for specific version
-spec detect_features_for_version(otp_version()) -> [feature_flag()].
detect_features_for_version(Version) ->
    [
        {native_json, has_native_json()},
        {process_iterator, has_process_iterator()},
        {priority_messages, has_priority_messages()},
        {eep48_maps, has_eep48_maps()},
        {eep72_streams, has_eep72_streams()},
        {eep76_gc, has_eep76_gc()},
        {advanced_maps, has_advanced_maps()}
    ].

%% @private Check if native JSON is available at runtime
-spec has_native_json() -> boolean().
has_native_json() ->
    erlang:function_exported(json, encode, 1).

%% @private Check if process iterator is available
-spec has_process_iterator() -> boolean().
has_process_iterator() ->
    erlang:function_exported(erlang, processes_iterator, 0).

%% @private Check if priority messages are available
-spec has_priority_messages() -> boolean().
has_priority_messages() ->
    erlmcp_version_detector:is_version_at_least(
        erlmcp_version_detector:otp_version(),
        {28, 0, 0}
    ).

%% @private Check if EEP 48 maps are available
-spec has_eep48_maps() -> boolean().
has_eep48_maps() ->
    true.  % Available since OTP 17

%% @private Check if EEP 72 streams are available
-spec has_eep72_streams() -> boolean().
has_eep72_streams() ->
    erlang:function_exported(gen_stream, new, 1).

%% @private Check if EEP 76 GC is available
-spec has_eep76_gc() -> boolean().
has_eep76_gc() ->
    erlmcp_version_detector:is_version_at_least(
        erlmcp_version_detector:otp_version(),
        {28, 3, 0}
    ).

%% @private Check if advanced maps are available
-spec has_advanced_maps() -> boolean().
has_advanced_maps() ->
    erlang:module_loaded(maps_utils).  % Check if maps_utils is available

%% @private Configure legacy mode (OTP 26)
-spec configure_legacy_mode() -> any().
configure_legacy_mode() ->
    logger:info("Configuring legacy mode for OTP 26"),

    % Memory optimizations for OTP 26
    application:set_env(kernel, process_limit, 32768),

    % Conservative resource limits
    application:set_env(erlmcp, max_connections, 1000),
    application:set_env(erlmcp, max_subscriptions, 1000),

    % Disable intensive features
    application:set_env(erlmcp, enable_process_optimization, false),
    application:set_env(erlmcp, enable_priority_processing, false),

    % Performance monitoring
    enable_performance_monitoring(conservative).

%% @private Configure stable mode (OTP 27)
-spec configure_stable_mode() -> any().
configure_stable_mode() ->
    logger:info("Configuring stable mode for OTP 27"),

    % Balanced resource limits
    application:set_env(kernel, process_limit, 65536),
    application:set_env(erlmcp, max_connections, 5000),
    application:set_env(erlmcp, max_subscriptions, 5000),

    % Enable JSON optimization
    application:set_env(erlmcp, native_json_enabled, true),
    application:set_env(erlmcp, enable_native_json, true),

    % Enable basic optimizations
    application:set_env(erlmcp, enable_process_optimization, true),
    application:set_env(erlmcp, enable_priority_processing, false),

    % Performance monitoring
    enable_performance_monitoring(balanced).

%% @private Configure optimal mode (OTP 28+)
-spec configure_optimal_mode() -> any().
configure_optimal_mode() ->
    logger:info("Configuring optimal mode for OTP 28+"),

    % High performance settings
    application:set_env(kernel, process_limit, 131072),
    application:set_env(erlmcp, max_connections, 10000),
    application:set_env(erlmcp, max_subscriptions, 10000),

    % Enable all optimizations
    application:set_env(erlmcp, native_json_enabled, true),
    application:set_env(erlmcp, process_optimization_enabled, true),
    application:set_env(erlmcp, priority_processing_enabled, true),
    application:set_env(erlmcp, advanced_memory_management, true),

    % Enable all modern features
    application:set_env(erlmcp, enable_process_iterator, true),
    application:set_env(erlmcp, enable_priority_messages, true),
    application:set_env(erlmcp, enable_memory_optimization, true),

    % Performance monitoring
    enable_performance_monitoring(optimal).

%% @private Enable performance monitoring
-spec enable_performance_monitoring(config_optimization()) -> ok.
enable_performance_monitoring(conservative) ->
    application:set_env(erlmcp, performance_monitoring, true),
    application:set_env(erlmcp, monitoring_interval, 30000),  % 30 seconds
    application:set_env(erlmcp, alert_thresholds, #{
        memory_usage => 80,
        cpu_usage => 70,
        process_count => 25000
    });

enable_performance_monitoring(balanced) ->
    application:set_env(erlmcp, performance_monitoring, true),
    application:set_env(erlmcp, monitoring_interval, 15000),  % 15 seconds
    application:set_env(erlmcp, alert_thresholds, #{
        memory_usage => 85,
        cpu_usage => 75,
        process_count => 50000
    });

enable_performance_monitoring(optimal) ->
    application:set_env(erlmcp, performance_monitoring, true),
    application:set_env(erlmcp, monitoring_interval, 5000),   % 5 seconds
    application:set_env(erlmcp, alert_thresholds, #{
        memory_usage => 90,
        cpu_usage => 80,
        process_count => 100000
    }).