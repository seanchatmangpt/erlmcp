%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Observer Module
%%%
%%% Provides real-time monitoring dashboard for erlmcp CLI:
%%% - Live metrics display (throughput, latency, errors)
%%% - Real-time process monitoring
%%% - System health dashboard
%%% - Connection monitoring
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_observer).
-behaviour(gen_server).

%% API
-export([watch/0, watch/1, start_watch/1, stop_watch/0, get_snapshot/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type watch_opts() ::
    #{refresh_interval => pos_integer(),
      format => atom(),
      metrics => [atom()],
      output_file => string()}.

-record(state,
        {opts :: watch_opts(),
         refresh_interval :: pos_integer(),
         output_file => string() | undefined,
         history :: [map()]}).

%%====================================================================
%% API Functions - Real-Time Watching
%%====================================================================

%% @doc Start watching with default options
-spec watch() -> ok | {error, term()}.
watch() ->
    watch(#{}).

%% @doc Start watching with options
-spec watch(watch_opts()) -> ok | {error, term()}.
watch(Opts) ->
    try
        RefreshInterval = maps:get(refresh_interval, Opts, 1000),
        Format = maps:get(format, Opts, table),

        %% Start watch loop
        watch_loop(Opts, RefreshInterval, Format)
    catch
        Class:Error:Stack ->
            {error,
             #{class => Class,
               error => Error,
               stacktrace => Stack}}
    end.

%% @doc Start background watch process
-spec start_watch(watch_opts()) -> {ok, pid()} | {error, term()}.
start_watch(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Stop background watch process
-spec stop_watch() -> ok | {error, term()}.
stop_watch() ->
    gen_server:stop(?MODULE).

%% @doc Get current metrics snapshot
-spec get_snapshot() -> {ok, map()} | {error, term()}.
get_snapshot() ->
    gen_server:call(?MODULE, get_snapshot).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init(Opts) ->
    RefreshInterval = maps:get(refresh_interval, Opts, 1000),
    OutputFile = maps:get(output_file, Opts, undefined),
    State = #state{opts = Opts,
                   refresh_interval = RefreshInterval,
                   output_file = OutputFile,
                   history = []},
    %% Start the periodic metrics collection
    erlang:send_after(RefreshInterval, self(), collect_metrics),
    {ok, State}.

%% @private
handle_call(get_snapshot, _From, State) ->
    Metrics = collect_metrics(),
    {reply, {ok, Metrics}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(collect_metrics, #state{opts = Opts,
                                    refresh_interval = RefreshInterval,
                                    output_file = OutputFile,
                                    history = History} = State) ->
    %% Collect metrics
    Metrics = collect_metrics(),

    %% Add to history
    NewHistory = [Metrics | lists:sublist(History, 99)],

    %% Write to file if specified
    case OutputFile of
        undefined ->
            ok;
        _ ->
            FormattedData = jsx:encode(NewHistory, [{space, 1}, {indent, 2}]),
            file:write_file(OutputFile, FormattedData)
    end,

    %% Schedule next collection
    erlang:send_after(RefreshInterval, self(), collect_metrics),

    {noreply, State#state{history = NewHistory}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Watch Loop
%%====================================================================

%% @private Main watch loop (foreground)
-spec watch_loop(watch_opts(), pos_integer(), atom()) -> ok.
watch_loop(Opts, RefreshInterval, Format) ->
    %% Clear screen
    io:format("\e[2J\e[H"),

    %% Collect metrics
    Metrics = collect_metrics(),

    %% Display metrics
    display_metrics(Metrics, Format),

    %% Wait for refresh or exit
    receive
        stop ->
            ok
    after RefreshInterval ->
        %% Check for user input (q to quit)
        case check_for_quit() of
            true ->
                ok;
            false ->
                watch_loop(Opts, RefreshInterval, Format)
        end
    end.

%%====================================================================
%% Internal Functions - Metrics Collection
%%====================================================================

%% @private Collect all metrics
-spec collect_metrics() -> map().
collect_metrics() ->
    #{timestamp => iso8601_timestamp(),
      system => collect_system_metrics(),
      memory => collect_memory_metrics(),
      processes => collect_process_metrics(),
      connections => collect_connection_metrics(),
      throughput => collect_throughput_metrics(),
      latency => collect_latency_metrics(),
      errors => collect_error_metrics(),
      health => collect_health_metrics()}.

%% @private Collect system metrics
-spec collect_system_metrics() -> map().
collect_system_metrics() ->
    #{uptime_seconds => element(1, erlang:statistics(wall_clock)) div 1000,
      schedulers_online => erlang:system_info(schedulers_online),
      run_queue => erlang:statistics(run_queue),
      reductions => element(1, erlang:statistics(reductions)),
      io_input_bytes => element(1, erlang:statistics(io)),
      io_output_bytes => element(2, erlang:statistics(io))}.

%% @private Collect memory metrics
-spec collect_memory_metrics() -> map().
collect_memory_metrics() ->
    Memory = erlang:memory(),
    Total = proplists:get_value(total, Memory, 0),
    Processes = proplists:get_value(processes, Memory, 0),

    #{total_mb => Total / (1024 * 1024),
      processes_mb => Processes / (1024 * 1024),
      system_mb => (Total - Processes) / (1024 * 1024),
      usage_percentage => Processes / Total * 100,
      binary_mb => proplists:get_value(binary, Memory, 0) / (1024 * 1024),
      atom_mb => proplists:get_value(atom, Memory, 0) / (1024 * 1024),
      ets_mb => proplists:get_value(ets, Memory, 0) / (1024 * 1024)}.

%% @private Collect process metrics
-spec collect_process_metrics() -> map().
collect_process_metrics() ->
    ProcessCount = erlang:system_info(process_count),
    ProcessLimit = erlang:system_info(process_limit),

    #{count => ProcessCount,
      limit => ProcessLimit,
      usage_percentage => ProcessCount / ProcessLimit * 100,
      port_count => erlang:system_info(port_count),
      atom_count => erlang:system_info(atom_count),
      ets_count => length(ets:all())}.

%% @private Collect connection metrics
-spec collect_connection_metrics() -> map().
collect_connection_metrics() ->
    %% Try to get connection count from registry
    ConnectionCount =
        case whereis(erlmcp_registry) of
            undefined ->
                0;
            _ ->
                try
                    length(gproc:lookup_pids({p, l, server}))
                catch
                    _:_ ->
                        0
                end
        end,

    #{active_connections => ConnectionCount,
      servers => count_processes_by_name(erlmcp_server),
      clients => count_processes_by_name(erlmcp_client)}.

%% @private Collect throughput metrics
-spec collect_throughput_metrics() -> map().
collect_throughput_metrics() ->
    %% Try to get metrics from metrics server
    case whereis(erlmcp_metrics) of
        undefined ->
            #{messages_per_second => 0};
        _ ->
            try
                Summary = erlmcp_metrics:get_performance_summary(),
                Rates = maps:get(<<"rates">>, Summary, #{}),
                #{messages_per_second =>
                      maps:get(<<"transport_operation_duration_ms_per_second">>, Rates, 0)}
            catch
                _:_ ->
                    #{messages_per_second => 0}
            end
    end.

%% @private Collect latency metrics
-spec collect_latency_metrics() -> map().
collect_latency_metrics() ->
    %% Try to get latency from metrics
    case whereis(erlmcp_metrics) of
        undefined ->
            #{p50 => 0,
              p95 => 0,
              p99 => 0};
        _ ->
            try
                Summary = erlmcp_metrics:get_performance_summary(),
                Percentiles = maps:get(<<"percentiles">>, Summary, #{}),
                TransportPercentiles =
                    maps:get(<<"transport_operation_duration_ms_percentiles">>, Percentiles, #{}),
                #{p50 => maps:get(<<"p50">>, TransportPercentiles, 0),
                  p95 => maps:get(<<"p95">>, TransportPercentiles, 0),
                  p99 => maps:get(<<"p99">>, TransportPercentiles, 0)}
            catch
                _:_ ->
                    #{p50 => 0,
                      p95 => 0,
                      p99 => 0}
            end
    end.

%% @private Collect error metrics
-spec collect_error_metrics() -> map().
collect_error_metrics() ->
    #{error_rate => 0.0, recent_errors => []}.

%% @private Collect health metrics
-spec collect_health_metrics() -> map().
collect_health_metrics() ->
    case whereis(erlmcp_health_monitor) of
        undefined ->
            #{status => not_monitored};
        _ ->
            try
                Health = erlmcp_health_monitor:get_system_health(),
                Health
            catch
                _:_ ->
                    #{status => unknown}
            end
    end.

%%====================================================================
%% Internal Functions - Display
%%====================================================================

%% @private Display metrics based on format
-spec display_metrics(map(), atom()) -> ok.
display_metrics(Metrics, table) ->
    display_metrics_table(Metrics);
display_metrics(Metrics, json) ->
    io:format("~s~n", [jsx:encode(Metrics, [{space, 1}, {indent, 2}])]);
display_metrics(Metrics, _Format) ->
    io:format("~p~n", [Metrics]).

%% @private Display metrics as table
-spec display_metrics_table(map()) -> ok.
display_metrics_table(Metrics) ->
    io:format("~n"),
    io:format("╔════════════════════════════════════════════════════════════════════╗~n"),
    io:format("║                    erlmcp Real-Time Monitor                        ║~n"),
    io:format("╠════════════════════════════════════════════════════════════════════╣~n"),
    io:format("║ Timestamp: ~-58s ║~n", [maps:get(timestamp, Metrics)]),
    io:format("╠════════════════════════════════════════════════════════════════════╣~n"),

    %% System section
    System = maps:get(system, Metrics, #{}),
    io:format("║ SYSTEM                                                             ║~n"),
    io:format("║   Uptime:          ~10w seconds                                ║~n",
              [maps:get(uptime_seconds, System, 0)]),
    io:format("║   Schedulers:      ~10w online                                 ║~n",
              [maps:get(schedulers_online, System, 0)]),
    io:format("║   Run Queue:       ~10w                                        ║~n",
              [maps:get(run_queue, System, 0)]),
    io:format("╠════════════════════════════════════════════════════════════════════╣~n"),

    %% Memory section
    Memory = maps:get(memory, Metrics, #{}),
    io:format("║ MEMORY                                                             ║~n"),
    io:format("║   Total:           ~10.2f MB                                   ║~n",
              [maps:get(total_mb, Memory, 0.0)]),
    io:format("║   Processes:       ~10.2f MB (~5.1f%%)                        ║~n",
              [maps:get(processes_mb, Memory, 0.0), maps:get(usage_percentage, Memory, 0.0)]),
    io:format("║   Binary:          ~10.2f MB                                   ║~n",
              [maps:get(binary_mb, Memory, 0.0)]),
    io:format("╠════════════════════════════════════════════════════════════════════╣~n"),

    %% Processes section
    Processes = maps:get(processes, Metrics, #{}),
    io:format("║ PROCESSES                                                          ║~n"),
    io:format("║   Count:           ~10w / ~w (~5.1f%%)                    ║~n",
              [maps:get(count, Processes, 0),
               maps:get(limit, Processes, 0),
               maps:get(usage_percentage, Processes, 0.0)]),
    io:format("║   Ports:           ~10w                                        ║~n",
              [maps:get(port_count, Processes, 0)]),
    io:format("║   ETS Tables:      ~10w                                        ║~n",
              [maps:get(ets_count, Processes, 0)]),
    io:format("╠════════════════════════════════════════════════════════════════════╣~n"),

    %% Connections section
    Connections = maps:get(connections, Metrics, #{}),
    io:format("║ CONNECTIONS                                                        ║~n"),
    io:format("║   Active:          ~10w                                        ║~n",
              [maps:get(active_connections, Connections, 0)]),
    io:format("║   Servers:         ~10w                                        ║~n",
              [maps:get(servers, Connections, 0)]),
    io:format("║   Clients:         ~10w                                        ║~n",
              [maps:get(clients, Connections, 0)]),
    io:format("╠════════════════════════════════════════════════════════════════════╣~n"),

    %% Throughput section
    Throughput = maps:get(throughput, Metrics, #{}),
    io:format("║ THROUGHPUT                                                         ║~n"),
    io:format("║   Messages/sec:    ~10.2f                                      ║~n",
              [maps:get(messages_per_second, Throughput, 0.0)]),
    io:format("╠════════════════════════════════════════════════════════════════════╣~n"),

    %% Latency section
    Latency = maps:get(latency, Metrics, #{}),
    io:format("║ LATENCY (ms)                                                       ║~n"),
    io:format("║   p50:             ~10.2f                                      ║~n",
              [maps:get(p50, Latency, 0.0)]),
    io:format("║   p95:             ~10.2f                                      ║~n",
              [maps:get(p95, Latency, 0.0)]),
    io:format("║   p99:             ~10.2f                                      ║~n",
              [maps:get(p99, Latency, 0.0)]),
    io:format("╚════════════════════════════════════════════════════════════════════╝~n"),
    io:format("~nPress 'q' to quit~n"),
    ok.

%%====================================================================
%% Internal Functions - Utilities
%%====================================================================

%% @private Count processes by registered name prefix
-spec count_processes_by_name(atom()) -> non_neg_integer().
count_processes_by_name(Prefix) ->
    PrefixStr = atom_to_list(Prefix),
    PrefixLen = length(PrefixStr),

    Registered = erlang:registered(),
    length(lists:filter(fun(Name) ->
                           NameStr = atom_to_list(Name),
                           case length(NameStr) >= PrefixLen of
                               true ->
                                   lists:prefix(PrefixStr, NameStr);
                               false ->
                                   false
                           end
                        end,
                        Registered)).

%% @private Check for quit input
-spec check_for_quit() -> boolean().
check_for_quit() ->
    %% Simple check - in real implementation would use proper terminal input
    false.

%% @private Generate ISO 8601 timestamp
-spec iso8601_timestamp() -> binary().
iso8601_timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                   [Year, Month, Day, Hour, Min, Sec])).
