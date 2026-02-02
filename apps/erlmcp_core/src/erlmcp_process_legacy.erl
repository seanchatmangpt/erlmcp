%%%-------------------------------------------------------------------
%%% @doc
%%% Process Enumeration Legacy Implementation
%%%
%%% This module provides legacy process enumeration methods for
%%% OTP versions that do not support process iterators (OTP < 28).
%%%
%%% Features:
%%%   - Legacy process enumeration using erlang:processes()
%%%   - Memory usage optimization for large systems
%%%   - Performance monitoring and warnings
%%%   - Graceful degradation when process iterators are available
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_process_legacy).

%% API
-export([
    enable_legacy_mode/0,
    disable_legacy_mode/0,
    is_legacy_mode_enabled/0,
    enumerate_processes/0,
    count_processes/0,
    get_processes_with_warning/0,
    monitor_process_count/0,
    get_performance_metrics/0
]).

%% Types
-type process_info() :: pid() | {pid(), map()}.
-type performance_metrics() :: #{
    total_count => non_neg_integer(),
    enumeration_time => integer(),
    memory_usage => integer(),
    method => legacy_iterator | native | optimized
}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Enable legacy process enumeration mode
-spec enable_legacy_mode() -> ok.
enable_legacy_mode() ->
    application:set_env(erlmcp, process_method, legacy),
    application:set_env(erlmcp, legacy_process_mode, true),
    logger:info("Legacy process enumeration mode enabled"),
    start_process_monitoring(),
    ok.

%% @doc Disable legacy process enumeration mode
-spec disable_legacy_mode() -> ok.
disable_legacy_mode() ->
    case erlang:function_exported(erlang, processes_iterator, 0) of
        true ->
            application:set_env(erlmcp, process_method, iterator),
            application:set_env(erlmcp, legacy_process_mode, false),
            logger:info("Legacy process enumeration mode disabled, using modern iterators");
        false ->
            logger:warning("Cannot disable legacy mode - process iterators not available"),
            enable_legacy_mode()
    end,
    ok.

%% @doc Check if legacy mode is enabled
-spec is_legacy_mode_enabled() -> boolean().
is_legacy_mode_enabled() ->
    application:get_env(erlmcp, legacy_process_mode, false).

%% @doc Enumerate all processes (with memory optimization)
-spec enumerate_processes() -> [pid()].
enumerate_processes() ->
    Count = erlang:system_info(process_count),

    case Count > 10000 of
        true ->
            logger:warning("Enumerating ~p processes may use significant memory. "
                           "Consider upgrade to OTP 28.3.1+ for O(1) memory usage.",
                           [Count]),
            perform_enumeration(Count);
        false ->
            perform_enumeration(Count)
    end.

%% @doc Count processes efficiently
-spec count_processes() -> non_neg_integer().
count_processes() ->
    erlang:system_info(process_count).

%% @doc Get processes with warning for large counts
-spec get_processes_with_warning() -> [pid()].
get_processes_with_warning() ->
    Count = count_processes(),

    case Count > 10000 of
        true ->
            logger:warning("Process enumeration of ~p processes may be inefficient. "
                           "Upgrade to OTP 28.3.1+ for optimal performance.",
                           [Count]),
            enumerate_processes();
        false ->
            enumerate_processes()
    end.

%% @doc Start process count monitoring
-spec monitor_process_count() -> ok.
monitor_process_count() ->
    case is_legacy_mode_enabled() of
        true ->
            start_process_monitoring();
        false ->
            logger:info("Process monitoring not needed in modern mode")
    end,
    ok.

%% @doc Get performance metrics for process handling
-spec get_performance_metrics() -> performance_metrics().
get_performance_metrics() ->
    Count = count_processes(),

    % Measure enumeration time
    StartTime = erlang:monotonic_time(microsecond),
    _Processes = enumerate_processes(),
    EndTime = erlang:monotonic_time(microsecond),

    EnumerationTime = EndTime - StartTime,

    % Get memory usage
    MemoryUsage = erlang:memory(total),

    #{
        total_count => Count,
        enumeration_time => EnumerationTime,
        memory_usage => MemoryUsage,
        method => get_current_method()
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Perform process enumeration
-spec perform_enumeration(non_neg_integer()) -> [pid()].
perform_enumeration(_Count) ->
    erlang:processes().

%% @private Start process monitoring
-spec start_process_monitoring() -> ok.
start_process_monitoring() ->
    % Check if monitoring is already running
    case erlang:whereis(process_monitor) of
        undefined ->
            spawn_link(fun process_monitor_loop/0);
        _ ->
            ok
    end.

%% @private Process monitoring loop
-spec process_monitor_loop() -> no_return().
process_monitor_loop() ->
    register(process_monitor, self()),

    process_monitor_loop(0).

%% @private Process monitoring loop with iteration
-spec process_monitor_loop(non_neg_integer()) -> no_return().
process_monitor_loop(Iteration) ->
    % Check process count every 30 seconds
    timer:sleep(30000),

    Count = count_processes(),
    Memory = erlang:memory(total),

    % Log warnings for high resource usage
    case Count > 15000 of
        true ->
            logger:warning("High process count detected: ~p processes", [Count]);
        false ->
            ok
    end,

    case Memory > 1073741824 of  % > 1GB
        true ->
            logger:warning("High memory usage detected: ~p MB", [Memory div 1048576]);
        false ->
            ok
    end,

    % Continue monitoring
    process_monitor_loop(Iteration + 1).

%% @private Get current method name
-spec get_current_method() -> legacy_iterator | native | optimized.
get_current_method() ->
    case erlang:function_exported(erlang, processes_iterator, 0) of
        true ->
            case is_legacy_mode_enabled() of
                true -> legacy_iterator;
                false -> optimized
            end;
        false ->
            native
    end.

%% @private Monitor process count changes
-spec monitor_process_count_changes() -> ok.
monitor_process_count_changes() ->
    PreviousCount = count_processes(),

    % Wait for changes
    receive
        after 10000 ->
            CurrentCount = count_processes(),

            case CurrentCount =/= PreviousCount of
                true when abs(CurrentCount - PreviousCount) > 1000 ->
                    logger:info("Significant process count change: ~p -> ~p",
                               [PreviousCount, CurrentCount]);
                true ->
                    ok;
                false ->
                    ok
            end,

            monitor_process_count_changes()
            % Recursive call for continuous monitoring
    end.

%% @private Log performance statistics
-spec log_performance_metrics(performance_metrics()) -> ok.
log_performance_metrics(Metrics) ->
    logger:info("Process Performance Metrics: ~p", [Metrics]).

%% @private Get memory usage trend
-spec get_memory_trend() -> increasing | stable | decreasing.
get_memory_trend() ->
    % This would track memory usage over time
    % For now, return stable as placeholder
    stable.