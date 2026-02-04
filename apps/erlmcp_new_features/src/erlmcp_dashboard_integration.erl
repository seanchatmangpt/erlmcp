-module(erlmcp_dashboard_integration).
-behaviour(gen_server).

%% API
-export([start_link/0, get_dashboard_data/0, get_realtime_metrics/0, get_health_summary/0]).
-export([register_dashboard/1, unregister_dashboard/1]).
-export([enable_websocket/0, disable_websocket/0]).
-export([set_alert_threshold/2, get_alert_thresholds/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8080).
-define(DEFAULT_REFRESH_INTERVAL, 5000).  % 5 seconds
-define(DEFAULT_ALERT_THRESHOLD, 0.8).     % 80%
-define(WEBSOCKET_INTERVAL, 1000).        % 1 second for real-time updates

%% Records
-record(dashboard_client, {
    id :: binary(),
    pid :: pid(),
    subscriptions :: list(),
    last_activity :: erlang:timestamp()
}).

-record(alert_threshold, {
    cpu_usage :: number(),
    memory_usage :: number(),
    error_rate :: number(),
    response_time :: number()
}).

-record(state, {
    port :: pos_integer(),
    refresh_interval :: pos_integer(),
    clients :: #{binary() => #dashboard_client{}},
    alerts :: map(),
    thresholds :: #alert_threshold{},
    websocket_enabled :: boolean(),
    websocket_pid :: pid() | undefined,
    update_timer :: reference() | undefined,
    metrics_buffer :: list(),
    health_data :: map()
}).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_dashboard_data() -> map().
get_dashboard_data() ->
    gen_server:call(?SERVER, get_dashboard_data, 5000).

-spec get_realtime_metrics() -> map().
get_realtime_metrics() ->
    gen_server:call(?SERVER, get_realtime_metrics, 2000).

-spec get_health_summary() -> map().
get_health_summary() ->
    gen_server:call(?SERVER, get_health_summary, 2000).

-spec register_dashboard(binary()) -> ok | {error, term()}.
register_dashboard(ClientId) when is_binary(ClientId) ->
    gen_server:call(?SERVER, {register_dashboard, ClientId}).

-spec unregister_dashboard(binary()) -> ok.
unregister_dashboard(ClientId) when is_binary(ClientId) ->
    gen_server:cast(?SERVER, {unregister_dashboard, ClientId}).

-spec enable_websocket() -> ok.
enable_websocket() ->
    gen_server:cast(?SERVER, enable_websocket).

-spec disable_websocket() -> ok.
disable_websocket() ->
    gen_server:cast(?SERVER, disable_websocket).

-spec set_alert_threshold(atom(), number()) -> ok.
set_alert_threshold(Type, Threshold) when is_atom(Type), is_number(Threshold) ->
    gen_server:cast(?SERVER, {set_alert_threshold, Type, Threshold}).

-spec get_alert_thresholds() -> map().
get_alert_thresholds() ->
    gen_server:call(?SERVER, get_alert_thresholds, 2000).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % Initialize dashboard state
    State = #state{
        port = ?DEFAULT_PORT,
        refresh_interval = ?DEFAULT_REFRESH_INTERVAL,
        clients = #{},
        alerts = #{},
        thresholds = #alert_threshold{
            cpu_usage = ?DEFAULT_ALERT_THRESHOLD,
            memory_usage = ?DEFAULT_ALERT_THRESHOLD,
            error_rate = ?DEFAULT_ALERT_THRESHOLD,
            response_time = ?DEFAULT_ALERT_THRESHOLD
        },
        websocket_enabled = false,
        metrics_buffer = [],
        health_data = #{}
    },

    % Start metrics collection
    start_metrics_collection(State),

    % Start WebSocket server if enabled
    WebSocketPid = case State#state.websocket_enabled of
        true -> start_websocket_server();
        false -> undefined
    end,

    % Start periodic updates
    UpdateTimer = erlang:send_after(?DEFAULT_REFRESH_INTERVAL, self(), {update_dashboard}),

    {ok, State#state{websocket_pid = WebSocketPid, update_timer = UpdateTimer}}.

handle_call(get_dashboard_data, _From, State) ->
    % Get comprehensive dashboard data
    DashboardData = generate_dashboard_data(State),
    {reply, DashboardData, State};

handle_call(get_realtime_metrics, _From, State) ->
    % Get real-time metrics
    RealtimeMetrics = generate_realtime_metrics(State),
    {reply, RealtimeMetrics, State};

handle_call(get_health_summary, _From, State) ->
    % Get health summary
    HealthSummary = generate_health_summary(State),
    {reply, HealthSummary, State};

handle_call({register_dashboard, ClientId}, _From, State) ->
    case maps:is_key(ClientId, State#state.clients) of
        false ->
            % Create new dashboard client
            Client = #dashboard_client{
                id = ClientId,
                pid = self(),
                subscriptions = [all],
                last_activity = erlang:timestamp()
            },
            NewClients = maps:put(ClientId, Client, State#state.clients),

            % Send initial data
            send_dashboard_data(ClientId, State),

            {reply, ok, State#state{clients = NewClients}};
        true ->
            {reply, {error, client_already_registered}, State}
    end;

handle_call(get_alert_thresholds, _From, State) ->
    Thresholds = #{
        cpu_usage => State#state.thresholds#alert_threshold.cpu_usage,
        memory_usage => State#state.thresholds#alert_threshold.memory_usage,
        error_rate => State#state.thresholds#alert_threshold.error_rate,
        response_time => State#state.thresholds#alert_threshold.response_time
    },
    {reply, Thresholds, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({unregister_dashboard, ClientId}, State) ->
    NewClients = maps:remove(ClientId, State#state.clients),
    {noreply, State#state{clients = NewClients}};

handle_cast({set_alert_threshold, Type, Threshold}, State) ->
    NewThresholds = case Type of
        cpu_usage -> State#state.thresholds#alert_threshold{cpu_usage = Threshold};
        memory_usage -> State#state.thresholds#alert_threshold{memory_usage = Threshold};
        error_rate -> State#state.thresholds#alert_threshold{error_rate = Threshold};
        response_time -> State#state.thresholds#alert_threshold{response_time = Threshold}
    end,
    {noreply, State#state{thresholds = NewThresholds}};

handle_cast(enable_websocket, State) ->
    case State#state.websocket_enabled of
        false ->
            NewWebSocketPid = start_websocket_server(),
            UpdateTimer = erlang:send_after(?WEBSOCKET_INTERVAL, self(), {websocket_update}),
            {noreply, State#state{
                websocket_enabled = true,
                websocket_pid = NewWebSocketPid,
                update_timer = UpdateTimer
            }};
        true ->
            {noreply, State}
    end;

handle_cast(disable_websocket, State) ->
    case State#state.websocket_enabled of
        true ->
            case State#state.websocket_pid of
                undefined -> ok;
                Pid -> erlang:exit(Pid, shutdown)
            end,
            {noreply, State#state{
                websocket_enabled = false,
                websocket_pid = undefined
            }};
        false ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({update_dashboard}, State) ->
    % Update dashboard data
    NewState = update_dashboard_data(State),

    % Update all clients
    notify_clients_updated(NewState),

    % Schedule next update
    UpdateTimer = erlang:send_after(State#state.refresh_interval, self(), {update_dashboard}),

    {noreply, NewState#state{update_timer = UpdateTimer}};

handle_info({websocket_update}, State) ->
    % Send real-time updates to WebSocket clients
    case State#state.websocket_enabled of
        true ->
            send_websocket_updates(State);
        false ->
            ok
    end,

    % Schedule next update
    UpdateTimer = erlang:send_after(?WEBSOCKET_INTERVAL, self(), {websocket_update}),

    {noreply, State#state{update_timer = UpdateTimer}};

handle_info({client_activity, ClientId}, State) ->
    % Update client activity timestamp
    case maps:find(ClientId, State#state.clients) of
        {ok, Client} ->
            NewClient = Client#dashboard_client{
                last_activity = erlang:timestamp()
            },
            NewClients = maps:put(ClientId, NewClient, State#state.clients),
            {noreply, State#state{clients = NewClients}};
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    % Clean up WebSocket server
    case State#state.websocket_pid of
        undefined -> ok;
        Pid -> erlang:exit(Pid, shutdown)
    end,

    % Clean up timers
    case State#state.update_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,

    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

generate_dashboard_data(State) ->
    % Generate comprehensive dashboard data
    #{
        timestamp => timestamp_to_iso8601(erlang:timestamp()),
        health => get_system_health(State),
        metrics => get_system_metrics(State),
        alerts => State#state.alerts,
        thresholds => #{
            cpu_usage => State#state.thresholds#alert_threshold.cpu_usage,
            memory_usage => State#state.thresholds#alert_threshold.memory_usage,
            error_rate => State#state.thresholds#alert_threshold.error_rate,
            response_time => State#state.thresholds#alert_threshold.response_time
        },
        service_status => get_service_status(State),
        performance => get_performance_metrics(State),
        connections => get_connection_metrics(State),
        clients => map_size(State#state.clients)
    }.

generate_realtime_metrics(State) ->
    % Generate real-time metrics
    RecentMetrics = lists:sublist(State#state.metrics_buffer, 10),
    CurrentMetrics = erlmcp_observability:get_metrics_summary(),

    #{
        current => CurrentMetrics,
        historical => RecentMetrics,
        timestamp => timestamp_to_iso8601(erlang:timestamp())
    }.

generate_health_summary(State) ->
    % Generate health summary
    HealthSummary = erlmcp_health_check:health_check(),

    #{
        overall => HealthSummary,
        detailed => erlmcp_health_check:detailed_health_check(),
        timestamp => timestamp_to_iso8601(erlang:timestamp())
    }.

start_metrics_collection(State) ->
    % Start collecting metrics
    erlmcp_observability:counter(<<"dashboard_requests_total">>, #{}),
    ok.

start_websocket_server() ->
    % Start WebSocket server for real-time updates
    % This would use a WebSocket library
    ok.

update_dashboard_data(State) ->
    % Update dashboard data and check alerts
    NewHealthData = erlmcp_health_check:health_check(),
    NewMetrics = erlmcp_observability:get_metrics_summary(),

    % Check for alerts
    NewAlerts = check_alerts(NewMetrics, NewHealthData, State#state.thresholds),

    % Update metrics buffer
    NewMetricsBuffer = [NewMetrics | State#state.metrics_buffer],
    if
        length(NewMetricsBuffer) > 100 ->
            lists:sublist(NewMetricsBuffer, 100);
        true ->
            NewMetricsBuffer
    end,

    State#state{
        health_data = NewHealthData,
        metrics_buffer = NewMetricsBuffer,
        alerts = NewAlerts
    }.

check_alerts(Metrics, HealthData, Thresholds) ->
    % Check for various alert conditions
    Alerts = #{},

    % Check CPU usage
    CpuUsage = get_metric_value(Metrics, "cpu_usage"),
    if
        CpuUsage > Thresholds#alert_threshold.cpu_usage ->
            Alerts#{cpu_alert => #{
                level => warning,
                message => <<"CPU usage high">>,
                value => CpuUsage,
                threshold => Thresholds#alert_threshold.cpu_usage
            }};
        true ->
            Alerts
    end,

    % Check memory usage
    MemoryUsage = get_metric_value(Metrics, "memory_usage"),
    if
        MemoryUsage > Thresholds#alert_threshold.memory_usage ->
            Alerts#{memory_alert => #{
                level => warning,
                message => <<"Memory usage high">>,
                value => MemoryUsage,
                threshold => Thresholds#alert_threshold.memory_usage
            }};
        true ->
            Alerts
    end,

    % Check error rate
    ErrorRate = get_metric_value(Metrics, "error_rate"),
    if
        ErrorRate > Thresholds#alert_threshold.error_rate ->
            Alerts#{error_rate_alert => #{
                level => error,
                message => <<"Error rate high">>,
                value => ErrorRate,
                threshold => Thresholds#alert_threshold.error_rate
            }};
        true ->
            Alerts
    end,

    % Check response time
    ResponseTime = get_metric_value(Metrics, "response_time"),
    if
        ResponseTime > Thresholds#alert_threshold.response_time ->
            Alerts#{response_time_alert => #{
                level => warning,
                message => <<"Response time high">>,
                value => ResponseTime,
                threshold => Thresholds#alert_threshold.response_time
            }};
        true ->
            Alerts
    end,

    Alerts.

get_metric_value(Metrics, Key) ->
    case maps:get(Key, Metrics, 0) of
        Value when is_number(Value) -> Value;
        _ -> 0
    end.

notify_clients_updated(State) ->
    % Notify all clients of updates
    lists:foreach(fun({ClientId, _}) ->
        send_dashboard_data(ClientId, State)
    end, maps:to_list(State#state.clients)).

send_dashboard_data(ClientId, State) ->
    % Send dashboard data to client
    DashboardData = generate_dashboard_data(State),
    Message = jsone:encode(#{
        type => dashboard_update,
        data => DashboardData,
        timestamp => timestamp_to_iso8601(erlang:timestamp())
    }),

    % Send to client (would use WebSocket or other transport)
    io:format("Dashboard update to ~s: ~s~n", [ClientId, Message]).

send_websocket_updates(State) ->
    % Send real-time updates via WebSocket
    RealtimeData = generate_realtime_metrics(State),
    WebSocketMessage = jsone:encode(#{
        type => metrics_update,
        data => RealtimeData,
        timestamp => timestamp_to_iso8601(erlang:timestamp())
    }),

    % Send to WebSocket clients
    io:format("WebSocket update: ~s~n", [WebSocketMessage]).

get_system_health(State) ->
    % Get system health information
    case erlmcp_health_check:health_check() of
        HealthData when is_map(HealthData) ->
            HealthData;
        _ ->
            #{status => unknown}
    end.

get_system_metrics(State) ->
    % Get system metrics
    case erlmcp_observability:get_metrics_summary() of
        Metrics when is_map(Metrics) ->
            Metrics;
        _ ->
            #{}
    end.

get_service_status(State) ->
    % Get service status information
    #{
        erlmcp_mcp_proxy_relay => get_service_health(erlmcp_mcp_proxy_relay),
        erlmcp_batch_processor => get_service_health(erlmcp_batch_processor),
        erlmcp_json_schema_validator => get_service_health(erlmcp_json_schema_validator),
        erlmcp_event_bus => get_service_health(erlmcp_event_bus)
    }.

get_service_health(Service) ->
    case erlmcp_health_check:health_check(Service) of
        HealthData when is_map(HealthData) ->
            HealthData;
        _ ->
            #{status => unknown}
    end.

get_performance_metrics(State) ->
    % Get performance metrics
    #{
        cpu => erlang:system_info(process_count),
        memory => erlang:memory(total),
        uptime => erlang:system_info(uptime) div 1000
    }.

get_connection_metrics(State) ->
    % Get connection metrics
    case erlmcp_mcp_proxy_relay:get_stats() of
        Stats when is_map(Stats) ->
            Stats;
        _ ->
            #{}
    end.

timestamp_to_iso8601({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    MicroSecPart = integer_to_binary(MicroSecs div 1000),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~sZ",
                 [Year, Month, Day, Hour, Min, Sec, MicroSecPart]).