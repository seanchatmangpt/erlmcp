%%%-------------------------------------------------------------------
%%% @doc ErlMCP Monitor Dashboard
%%% Real-time monitoring dashboard with metrics visualization.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_monitor_dashboard).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    start_dashboard/0,
    stop_dashboard/0,
    update_metrics/2,
    get_dashboard_data/0,
    export_metrics/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Records
-record(state, {
    config :: map(),
    metrics_history :: list(),
    health_history :: list(),
    alerts_history :: list(),
    http_server_pid :: pid() | undefined,
    websocket_clients :: [pid()]
}).

%% Types
-type dashboard_data() :: #{
    current_health := map(),
    current_metrics := map(),
    health_history := [map()],
    metrics_history := [map()],
    recent_alerts := [map()],
    summary := map()
}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the dashboard server
-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Start dashboard
-spec start_dashboard() -> ok | {error, term()}.
start_dashboard() ->
    gen_server:call(?MODULE, start_dashboard).

%% @doc Stop dashboard
-spec stop_dashboard() -> ok.
stop_dashboard() ->
    gen_server:call(?MODULE, stop_dashboard).

%% @doc Update dashboard with new metrics
-spec update_metrics(HealthStatus :: map(), Metrics :: map()) -> ok.
update_metrics(HealthStatus, Metrics) ->
    gen_server:cast(?MODULE, {update_metrics, HealthStatus, Metrics}).

%% @doc Get current dashboard data
-spec get_dashboard_data() -> {ok, dashboard_data()} | {error, term()}.
get_dashboard_data() ->
    gen_server:call(?MODULE, get_dashboard_data).

%% @doc Export metrics to file
-spec export_metrics(Format :: json | csv | prometheus) -> {ok, binary()} | {error, term()}.
export_metrics(Format) ->
    gen_server:call(?MODULE, {export_metrics, Format}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Config]) ->
    State = #state{
        config = Config,
        metrics_history = [],
        health_history = [],
        alerts_history = [],
        http_server_pid = undefined,
        websocket_clients = []
    },
    
    % Start HTTP server if dashboard is enabled
    NewState = case maps:get(dashboard_enabled, Config, true) of
        true ->
            case start_http_server(Config) of
                {ok, Pid} ->
                    State#state{http_server_pid = Pid};
                {error, Reason} ->
                    io:format("Failed to start dashboard HTTP server: ~p~n", [Reason]),
                    State
            end;
        false ->
            State
    end,
    
    {ok, NewState}.

handle_call(start_dashboard, _From, State) ->
    case State#state.http_server_pid of
        undefined ->
            case start_http_server(State#state.config) of
                {ok, Pid} ->
                    {reply, ok, State#state{http_server_pid = Pid}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        _Pid ->
            {reply, {error, already_running}, State}
    end;

handle_call(stop_dashboard, _From, State) ->
    case State#state.http_server_pid of
        undefined -> 
            {reply, ok, State};
        Pid ->
            stop_http_server(Pid),
            {reply, ok, State#state{http_server_pid = undefined}}
    end;

handle_call(get_dashboard_data, _From, State) ->
    DashboardData = create_dashboard_data(State),
    {reply, {ok, DashboardData}, State};

handle_call({export_metrics, Format}, _From, State) ->
    Result = case Format of
        json -> export_metrics_json(State);
        csv -> export_metrics_csv(State);
        prometheus -> export_metrics_prometheus(State);
        _ -> {error, unsupported_format}
    end,
    {reply, Result, State}.

handle_cast({update_metrics, HealthStatus, Metrics}, State) ->
    Timestamp = erlang:system_time(millisecond),
    
    % Add timestamp to data
    TimestampedHealth = HealthStatus#{timestamp => Timestamp},
    TimestampedMetrics = Metrics#{timestamp => Timestamp},
    
    % Update histories with retention
    MaxHistory = maps:get(max_history_entries, State#state.config, 1000),
    
    NewHealthHistory = trim_history([TimestampedHealth | State#state.health_history], MaxHistory),
    NewMetricsHistory = trim_history([TimestampedMetrics | State#state.metrics_history], MaxHistory),
    
    NewState = State#state{
        health_history = NewHealthHistory,
        metrics_history = NewMetricsHistory
    },
    
    % Notify WebSocket clients
    notify_websocket_clients(NewState, #{
        type => <<"metrics_update">>,
        health => TimestampedHealth,
        metrics => TimestampedMetrics
    }),
    
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.http_server_pid of
        undefined -> ok;
        Pid -> stop_http_server(Pid)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Start HTTP server for dashboard (simplified version)
start_http_server(Config) ->
    Port = maps:get(dashboard_port, Config, 8080),
    io:format("Dashboard would start on port ~p~n", [Port]),
    % Return a mock PID for testing
    {ok, spawn(fun() -> timer:sleep(infinity) end)}.

%% @doc Stop HTTP server
stop_http_server(_Pid) ->
    % Would stop cowboy listener
    ok.

%% @doc Create dashboard data structure
create_dashboard_data(State) ->
    CurrentHealth = case State#state.health_history of
        [H | _] -> H;
        [] -> #{}
    end,
    
    CurrentMetrics = case State#state.metrics_history of
        [M | _] -> M;
        [] -> #{}
    end,
    
    Summary = create_summary(State),
    
    #{
        current_health => CurrentHealth,
        current_metrics => CurrentMetrics,
        health_history => lists:sublist(State#state.health_history, 100),
        metrics_history => lists:sublist(State#state.metrics_history, 100),
        recent_alerts => lists:sublist(State#state.alerts_history, 50),
        summary => Summary
    }.

%% @doc Create summary statistics
create_summary(State) ->
    HealthHistory = State#state.health_history,
    MetricsHistory = State#state.metrics_history,
    
    % Calculate averages over last hour
    OneHourAgo = erlang:system_time(millisecond) - (60 * 60 * 1000),
    
    RecentHealth = [H || H <- HealthHistory, maps:get(timestamp, H, 0) > OneHourAgo],
    RecentMetrics = [M || M <- MetricsHistory, maps:get(timestamp, M, 0) > OneHourAgo],
    
    AvgHealthScore = case RecentHealth of
        [] -> 0.0;
        _ ->
            Scores = [maps:get(score, H, 0.0) || H <- RecentHealth],
            lists:sum(Scores) / length(Scores)
    end,
    
    AvgResponseTime = case RecentMetrics of
        [] -> 0.0;
        _ ->
            Times = [maps:get(avg_response_time_ms, M, 0) || M <- RecentMetrics],
            lists:sum(Times) / length(Times)
    end,
    
    #{
        avg_health_score_1h => round(AvgHealthScore * 100) / 100,
        avg_response_time_1h_ms => round(AvgResponseTime * 100) / 100,
        total_alerts_1h => count_recent_alerts(State, OneHourAgo),
        uptime_percent_1h => calculate_uptime_percent(RecentHealth),
        data_points_1h => length(RecentHealth)
    }.

%% @doc Count recent alerts
count_recent_alerts(State, Since) ->
    length([A || A <- State#state.alerts_history, maps:get(timestamp, A, 0) > Since]).

%% @doc Calculate uptime percentage
calculate_uptime_percent([]) -> 100.0;
calculate_uptime_percent(HealthHistory) ->
    HealthyCount = length([H || H <- HealthHistory, maps:get(overall, H) =:= healthy]),
    (HealthyCount / length(HealthHistory)) * 100.

%% @doc Trim history to maximum entries
trim_history(History, MaxEntries) when length(History) > MaxEntries ->
    lists:sublist(History, MaxEntries);
trim_history(History, _MaxEntries) ->
    History.

%% @doc Notify WebSocket clients
notify_websocket_clients(State, Message) ->
    lists:foreach(
        fun(ClientPid) ->
            try
                ClientPid ! {websocket_message, Message}
            catch
                _:_ -> ok % Client might be disconnected
            end
        end,
        State#state.websocket_clients
    ).

%% @doc Export metrics as JSON
export_metrics_json(State) ->
    DashboardData = create_dashboard_data(State),
    try
        % For testing without jsx dependency
        JSON = iolist_to_binary(io_lib:format("~p", [DashboardData])),
        {ok, JSON}
    catch
        Error:Reason ->
            {error, {json_encode_failed, Error, Reason}}
    end.

%% @doc Export metrics as CSV
export_metrics_csv(State) ->
    try
        Headers = ["timestamp", "health_score", "response_time_ms", "error_rate", "memory_usage_percent"],
        HeaderLine = string:join(Headers, ",") ++ "\n",
        
        DataLines = lists:map(
            fun({Health, Metrics}) ->
                Timestamp = maps:get(timestamp, Health, 0),
                HealthScore = maps:get(score, Health, 0.0),
                ResponseTime = maps:get(avg_response_time_ms, Metrics, 0),
                ErrorRate = maps:get(error_rate_percent, Metrics, 0),
                MemoryUsage = maps:get(memory_usage_percent, Metrics, 0),
                
                io_lib:format("~p,~.2f,~.1f,~.2f,~.1f~n", [
                    Timestamp, HealthScore, ResponseTime, ErrorRate, MemoryUsage
                ])
            end,
            zip_health_metrics(State#state.health_history, State#state.metrics_history)
        ),
        
        CSV = iolist_to_binary([HeaderLine | DataLines]),
        {ok, CSV}
    catch
        Error:Reason ->
            {error, {csv_export_failed, Error, Reason}}
    end.

%% @doc Export metrics in Prometheus format
export_metrics_prometheus(State) ->
    try
        CurrentMetrics = case State#state.metrics_history of
            [Latest | _] -> Latest;
            [] -> #{}
        end,
        
        CurrentHealth = case State#state.health_history of
            [LatestHealth | _] -> LatestHealth;
            [] -> #{}
        end,
        
        Lines = [
            "# HELP erlmcp_health_score Overall system health score (0-1)\n",
            "# TYPE erlmcp_health_score gauge\n",
            io_lib:format("erlmcp_health_score ~.2f~n", [maps:get(score, CurrentHealth, 0.0)]),
            "\n",
            "# HELP erlmcp_response_time_ms Average response time in milliseconds\n",
            "# TYPE erlmcp_response_time_ms gauge\n",
            io_lib:format("erlmcp_response_time_ms ~.1f~n", [maps:get(avg_response_time_ms, CurrentMetrics, 0)]),
            "\n"
        ],
        
        Prometheus = iolist_to_binary(Lines),
        {ok, Prometheus}
    catch
        Error:Reason ->
            {error, {prometheus_export_failed, Error, Reason}}
    end.

%% @doc Zip health and metrics histories together
zip_health_metrics(HealthHistory, MetricsHistory) ->
    MinLen = min(length(HealthHistory), length(MetricsHistory)),
    lists:zip(lists:sublist(HealthHistory, MinLen), lists:sublist(MetricsHistory, MinLen)).

%% Note: atom_to_binary/1 function removed as it was unused