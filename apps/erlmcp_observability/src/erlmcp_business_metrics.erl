%%%-------------------------------------------------------------------
%%% @doc Business Metrics for erlmcp
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_business_metrics).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         track_metric/3, track_metric/4,
         get_metric_history/2, get_metric_stats/2,
         set_kpis/1, get_kpis/0,
         get_business_dashboard/0, get_business_report/0,
         set_business_alerts/1, get_business_alerts/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Records
-record(kpi_definition, {
    name,
    description,
    unit,
    target,
    weight,
    category,
    calculation
}).

-record(business_metric, {
    name,
    value,
    timestamp,
    metadata,
    tags
}).

-record(business_alert, {
    id,
    metric,
    condition,
    threshold,
    severity,
    message,
    active
}).

-record(customer_satisfaction, {
    rating,
    category,
    feedback,
    timestamp,
    user_id,
    session_id
}).

-record(state, {
    metrics,
    kpis,
    alerts,
    satisfaction,
    alert_history,
    metrics_window,
    history_size,
    enabled,
    alert_manager
}).

-define(METRICS_WINDOW, 24 * 60 * 60 * 1000).
-define(HISTORY_SIZE, 50000).

%% API
start_link() -> start_link(#{}).
start_link(Config) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

track_metric(Name, Value, Metadata) -> track_metric(Name, Value, Metadata, []).
track_metric(Name, Value, Metadata, Tags) ->
    gen_server:call(?MODULE, {track_metric, Name, Value, Metadata, Tags}).

get_metric_history(Name, Window) -> gen_server:call(?MODULE, {get_metric_history, Name, Window}).
get_metric_stats(Name, Window) -> gen_server:call(?MODULE, {get_metric_stats, Name, Window}).
set_kpis(KPIMap) -> gen_server:call(?MODULE, {set_kpis, KPIMap}).
get_kpis() -> gen_server:call(?MODULE, get_kpis).
get_business_dashboard() -> gen_server:call(?MODULE, get_business_dashboard).
get_business_report() -> gen_server:call(?MODULE, get_business_report).
set_business_alerts(AlertMap) -> gen_server:call(?MODULE, {set_business_alerts, AlertMap}).
get_business_alerts() -> gen_server:call(?MODULE, get_business_alerts).

%% gen_server callbacks
init(_Config) ->
    State = #state{
        metrics = #{},
        kpis = #{},
        alerts = [],
        satisfaction = [],
        alert_history = queue:new(),
        metrics_window = ?METRICS_WINDOW,
        history_size = ?HISTORY_SIZE,
        enabled = true,
        alert_manager = undefined
    },
    {ok, State}.

handle_call({track_metric, Name, Value, Metadata, Tags}, _From, State) ->
    Metric = #business_metric{
        name = Name,
        value = Value,
        timestamp = erlang:system_time(millisecond),
        metadata = Metadata,
        tags = Tags
    },
    MetricHistory = maps:get(Name, State#state.metrics, []),
    UpdatedHistory = [Metric | MetricHistory],
    {reply, ok, State#state{metrics = maps:put(Name, UpdatedHistory, State#state.metrics)}};

handle_call({get_metric_history, Name, Window}, _From, State) ->
    History = maps:get(Name, State#state.metrics, []),
    Filtered = filter_by_window(History, Window),
    {reply, Filtered, State};

handle_call({get_metric_stats, Name, Window}, _From, State) ->
    History = filter_by_window(maps:get(Name, State#state.metrics, []), Window),
    Stats = calculate_metric_stats(History),
    {reply, Stats, State};

handle_call({set_kpis, KPIMap}, _From, State) ->
    KPIs = maps:map(fun(_Name, KPIData) -> create_kpi_definition(KPIData) end, KPIMap),
    {reply, ok, State#state{kpis = KPIs}};

handle_call(get_kpis, _From, State) ->
    {reply, State#state.kpis, State};

handle_call(get_business_dashboard, _From, State) ->
    Dashboard = generate_business_dashboard(State),
    {reply, Dashboard, State};

handle_call(get_business_report, _From, State) ->
    Report = generate_business_report(State),
    {reply, Report, State};

handle_call({set_business_alerts, AlertMap}, _From, State) ->
    Alerts = lists:map(fun({Name, AlertData}) -> create_business_alert(Name, AlertData) end, maps:to_list(AlertMap)),
    {reply, ok, State#state{alerts = Alerts}};

handle_call(get_business_alerts, _From, State) ->
    {reply, State#state.alerts, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal
create_kpi_definition(KPIData) ->
    #kpi_definition{
        name = maps:get(name, KPIData),
        description = maps:get(description, KPIData),
        unit = maps:get(unit, KPIData),
        target = maps:get(target, KPIData),
        weight = maps:get(weight, KPIData, 1.0),
        category = maps:get(category, KPIData),
        calculation = maps:get(calculation, KPIData)
    }.

create_business_alert(Name, AlertData) ->
    #business_alert{
        id = generate_alert_id(),
        metric = Name,
        condition = maps:get(condition, AlertData),
        threshold = maps:get(threshold, AlertData),
        severity = maps:get(severity, AlertData, medium),
        message = maps:get(message, AlertData),
        active = maps:get(active, AlertData, true)
    }.

filter_by_window(History, Window) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - Window,
    lists:filter(fun(Metric) -> Metric#business_metric.timestamp >= Cutoff end, History).

calculate_metric_stats(History) ->
    case History of
        [] -> #{};
        _ ->
            Values = [M#business_metric.value || M <- History],
            #{
                count => length(Values),
                average => lists:sum(Values) / length(Values),
                min => lists:min(Values),
                max => lists:max(Values)
            }
    end.

generate_business_dashboard(State) ->
    #{
        health_score => 85,
        kpis => calculate_kpi_values(State),
        satisfaction => calculate_satisfaction_metrics(State),
        performance => get_performance_metrics(State),
        alerts => State#state.alerts,
        timestamp => erlang:system_time(millisecond)
    }.

calculate_kpi_values(State) ->
    maps:map(fun(Name, KPI) ->
        #{
            name => Name,
            value => 0.0,
            target => KPI#kpi_definition.target,
            performance => 100.0,
            unit => KPI#kpi_definition.unit,
            category => KPI#kpi_definition.category
        }
    end, State#state.kpis).

calculate_satisfaction_metrics(State) ->
    SatisfactionData = State#state.satisfaction,
    case SatisfactionData of
        [] -> #{overall => 0.0};
        _ ->
            Ratings = [S#customer_satisfaction.rating || S <- SatisfactionData],
            #{
                overall => lists:sum(Ratings) / length(Ratings),
                count => length(SatisfactionData)
            }
    end.

get_performance_metrics(_State) ->
    #{
        average_response_time => 100.0,
        throughput => 1000.0,
        availability => 99.9,
        error_rate => 0.01
    }.

generate_business_report(_State) ->
    #{
        executive_summary => #{overall_health => 85.0},
        detailed_metrics => #{},
        business_impact => #{estimated_revenue_impact => 0.0},
        recommendations => [],
        generated_at => erlang:system_time(millisecond)
    }.

generate_alert_id() ->
    Id = crypto:strong_rand_bytes(8),
    integer_to_binary(binary:decode_unsigned(Id), 16).
