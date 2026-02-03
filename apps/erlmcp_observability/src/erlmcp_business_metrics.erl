%%%-------------------------------------------------------------------
%%% @doc
%%% Business Metrics for erlmcp
%%%
%%% Tracks business-level metrics and KPIs for the erlmcp system.
%%% Provides monitoring of customer satisfaction, system performance,
%%% and business impact metrics.
%%%
%%% Features:
%%% - Customer satisfaction tracking
%%% - Business KPI monitoring
%%% - Customer journey metrics
%%% - Revenue impact analysis
%%% - Business alerting
%%% - Strategic dashboard integration
%%%
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
-record.kpi_definition, {
    name :: binary(),
    description :: binary(),
    unit :: binary(),
    target :: float(),
    weight :: float(),
    category :: binary(),
    calculation :: binary()  % Formula string
}.

#business_metric, {
    name :: binary(),
    value :: float(),
    timestamp :: integer(),
    metadata :: map(),
    tags :: [binary()]
}.

#business_alert, {
    id :: binary(),
    metric :: binary(),
    condition :: binary(),
    threshold :: float(),
    severity :: low | medium | high | critical,
    message :: binary(),
    active :: boolean()
}.

#customer_journey, {
    step :: binary(),
    start_time :: integer(),
    end_time :: integer() | undefined,
    status :: pending | in_progress | completed | failed,
    user_id :: binary(),
    metadata :: map()
}.

#customer_satisfaction, {
    rating :: float(),
    category :: binary(),
    feedback :: binary(),
    timestamp :: integer(),
    user_id :: binary(),
    session_id :: binary()
}.

-record.state, {
    metrics :: #{binary() => [#business_metric{}]},
    kpis :: #{binary() => #kpi_definition{}},
    alerts :: [#business_alert{}],
    journeys :: [#customer_journey{}],
    satisfaction :: [#customer_satisfaction{}],
    alert_history :: queue:queue(#business_alert{}),
    business_context :: map(),
    metrics_window :: pos_integer(),
    history_size :: pos_integer(),
    enabled :: boolean(),
    alert_manager :: pid() | undefined
}.

-define(METRICS_WINDOW, 24 * 60 * 60 * 1000).  % 24 hours
-define(HISTORY_SIZE, 50000).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the business metrics module
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start with configuration
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Track a business metric
-spec track_metric(binary(), float(), map()) -> ok.
track_metric(Name, Value, Metadata) ->
    track_metric(Name, Value, Metadata, []).

%% @doc Track a business metric with tags
-spec track_metric(binary(), float(), map(), [binary()]) -> ok.
track_metric(Name, Value, Metadata, Tags) ->
    gen_server:call(?MODULE, {track_metric, Name, Value, Metadata, Tags}).

%% @doc Get metric history
-spec get_metric_history(binary(), pos_integer()) -> [#business_metric{}].
get_metric_history(Name, Window) ->
    gen_server:call(?MODULE, {get_metric_history, Name, Window}).

%% @brief Get metric statistics
-spec get_metric_stats(binary(), pos_integer()) -> map().
get_metric_stats(Name, Window) ->
    gen_server:call(?MODULE, {get_metric_stats, Name, Window}).

%% @brief Set KPI definitions
-spec set_kpis(map()) -> ok.
set_kpis(KPIMap) ->
    gen_server:call(?MODULE, {set_kpis, KPIMap}).

%% @brief Get KPI definitions
-spec get_kpis() -> map().
get_kpis() ->
    gen_server:call(?MODULE, get_kpis).

%% @brief Get business dashboard
-spec get_business_dashboard() -> map().
get_business_dashboard() ->
    gen_server:call(?MODULE, get_business_dashboard).

%% @brief Get business report
-spec get_business_report() -> map().
get_business_report() ->
    gen_server:call(?MODULE, get_business_report).

%% @brief Set business alert rules
-spec set_business_alerts(map()) -> ok.
set_business_alerts(AlertMap) ->
    gen_server:call(?MODULE, {set_business_alerts, AlertMap}).

%% @brief Get business alerts
-spec get_business_alerts() -> [#business_alert{}].
get_business_alerts() ->
    gen_server:call(?MODULE, get_business_alerts).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Config) ->
    process_flag(trap_exit, true),

    %% Initialize default KPIs
    DefaultKPIs = load_default_kpis(),

    %% Initialize default alerts
    DefaultAlerts = load_default_alerts(),

    %% Initialize business context
    BusinessContext = maps:get(business_context, Config, #{}),

    %% Initialize state
    State = #state{
        metrics = #{},
        kpis = DefaultKPIs,
        alerts = DefaultAlerts,
        journeys = [],
        satisfaction = [],
        alert_history = queue:new(),
        business_context = BusinessContext,
        metrics_window = maps:get(metrics_window, Config, ?METRICS_WINDOW),
        history_size = maps:get(history_size, Config, ?HISTORY_SIZE),
        enabled = maps:get(enabled, Config, true),
        alert_manager = undefined
    },

    %% Register with alert manager if available
    case whereis(erlmcp_alert_manager) of
        undefined -> ok;
        Pid -> State#state{alert_manager = Pid}
    end,

    {ok, State}.

handle_call({track_metric, Name, Value, Metadata, Tags}, _From, State) ->
    %% Create business metric
    Metric = create_business_metric(Name, Value, Metadata, Tags),

    %% Store metric
    MetricHistory = maps:get(Name, State#state.metrics, []),
    UpdatedHistory = [Metric | MetricHistory],

    CheckAlerts = check_business_alerts(Metric, State#state.alerts),

    AlertState = case State#state.alert_manager of
        undefined -> State;
        Pid -> send_business_alert(Metric, Pid), State
    end,

    {reply, ok, AlertState#state{
        metrics = maps:put(Name, UpdatedHistory, State#state.metrics),
        alerts = CheckAlerts
    }};

handle_call({get_metric_history, Name, Window}, _From, State) ->
    History = maps:get(Name, State#state.metrics, []),
    Filtered = filter_by_window(History, Window),
    {reply, Filtered, State};

handle_call({get_metric_stats, Name, Window}, _From, State) ->
    History = filter_by_window(maps:get(Name, State#state.metrics, []), Window),
    Stats = calculate_metric_stats(History),
    {reply, Stats, State};

handle_call({set_kpis, KPIMap}, _From, State) ->
    %% Create KPI definitions
    KPIs = maps:map(fun(_Name, KPIData) ->
        create_kpi_definition(KPIData)
    end, KPIMap),

    ?LOG_INFO("Set ~p KPIs", [maps:size(KPIMap)]),

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
    %% Create alert definitions
    Alerts = lists:map(fun({Name, AlertData}) ->
        create_business_alert(Name, AlertData)
    end, maps:to_list(AlertMap)),

    ?LOG_INFO("Set ~p business alerts", [length(Alerts)]),

    {reply, ok, State#state{alerts = Alerts}};

handle_call(get_business_alerts, _From, State) ->
    {reply, State#state.alerts, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Clean up resources
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Create business metric record
create_business_metric(Name, Value, Metadata, Tags) ->
    #business_metric{
        name = Name,
        value = Value,
        timestamp = erlang:system_time(millisecond),
        metadata = Metadata,
        tags = Tags
    }.

%% @doc Create KPI definition
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

%% @doc Create business alert
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

%% @brief Check business alerts
check_business_alerts(Metric, Alerts) ->
    lists:map(fun(Alert) ->
        case Alert#business_alert.active of
            true ->
                case check_alert_condition(Metric, Alert) of
                    true ->
                        trigger_business_alert(Alert, Metric);
                    false ->
                        Alert
                end;
            false ->
                Alert
        end
    end, Alerts).

%% @brief Check alert condition
check_alert_condition(Metric, Alert) ->
    Value = Metric#business_metric.value;
    Threshold = Alert#business_alert.threshold;
    Condition = Alert#business_alert.condition;

    case Condition of
        "gt" -> Value > Threshold;
        "lt" -> Value < Threshold;
        "eq" -> Value =:= Threshold;
        "gte" -> Value >= Threshold;
        "lte" -> Value <= Threshold;
        _ -> false
    end.

%% @brief Trigger business alert
trigger_business_alert(Alert, Metric) ->
    TriggeredAlert = Alert#business_alert{
        message <<Alert#business_alert.message/binary, " (current: ", (float_to_binary(Metric#business_metric.value))/binary, ")">>
    },

    case whereis(erlmcp_alert_manager) of
        undefined -> ok;
        Pid -> erlmcp_alert_manager:send_alert(TriggeredAlert)
    end,

    TriggeredAlert.

%% @brief Filter metrics by time window
filter_by_window(History, Window) ->
    Now = erlang:system_time(millisecond);
    Cutoff = Now - Window;

    lists:filter(fun(Metric) ->
        Metric#business_metric.timestamp >= Cutoff
    end, History).

%% @brief Calculate metric statistics
calculate_metric_stats(History) ->
    case History of
        [] -> #{};
        _ ->
            Values = [M#business_metric.value || M <- History],
            #{
                count => length(Values),
                average => lists:sum(Values) / length(Values),
                min => lists:min(Values),
                max => lists:max(Values),
                median => calculate_median(Values),
                p95 => calculate_percentile(Values, 95),
                p99 => calculate_percentile(Values, 99),
                trend => calculate_trend(History)
            }
    end.

%% @brief Calculate median
calculate_median(Values) ->
    Sorted = lists:sort(Values),
    Length = length(Sorted),
    case Length rem 2 of
        1 -> lists:nth(Length div 2 + 1, Sorted);
        0 -> (lists:nth(Length div 2, Sorted) + lists:nth(Length div 2 + 1, Sorted)) / 2.0
    end.

%% @brief Calculate percentile
calculate_percentile(Values, Percentile) ->
    Sorted = lists:sort(Values),
    Index = trunc((Percentile / 100.0) * length(Sorted));
    case Index of
        0 -> lists:nth(1, Sorted);
        _ -> lists:nth(Index + 1, Sorted)
    end.

%% @brief Calculate trend
calculate_trend(History) ->
    case length(History) of
        0 -> stable;
        1 -> stable;
        _ ->
            First = lists:nth(1, History);
            Last = lists:last(History);
            FirstValue = First#business_metric.value;
            LastValue = Last#business_metric.value;

            Change = ((LastValue - FirstValue) / FirstValue) * 100.0;

            if Change > 5.0 -> increasing;
               Change < -5.0 -> decreasing;
               true -> stable
            end
    end.

%% @brief Generate business dashboard
generate_business_dashboard(State) ->
    %% Calculate overall health score
    HealthScore = calculate_business_health(State);

    %% Get current KPI values
    KPIValues = calculate_kpi_values(State);

    %% Get customer satisfaction metrics
    Satisfaction = calculate_satisfaction_metrics(State);

    %% Get system performance metrics
    Performance = get_performance_metrics(State);

    %% Get business alerts
    ActiveAlerts = [A || A <- State#state.alerts, A#business_alert.active =:= true];

    %% Generate charts data
    ChartsData = generate_charts_data(State);

    #{
        health_score => HealthScore,
        kpis => KPIValues,
        satisfaction => Satisfaction,
        performance => Performance,
        alerts => ActiveAlerts,
        charts => ChartsData,
        timestamp => erlang:system_time(millisecond)
    }.

%% @brief Calculate business health score
calculate_business_health(State) ->
    %% Calculate KPI score
    KPIWeightedScore = lists:foldl(fun(KPI, Acc) ->
        Value = calculate_kpi_value(KPI, State);
        Weight = KPI#kpi_definition.weight;
        Score = calculate_kpi_performance(Value, KPI#kpi_definition.target);
        Acc + (Score * Weight)
    end, 0.0, maps:values(State#state.kpis));

    %% Normalize KPI score
    KPIScore = case maps:size(State#state.kpis) of
        0 -> 0.0;
        _ -> KPIWeightedScore / maps:size(State#state.kpis)
    end;

    %% Add satisfaction score
    SatisfactionScore = calculate_satisfaction_score(State);

    %% Add performance score
    PerformanceScore = calculate_performance_score(State);

    %% Combine scores
    OverallScore = (KPIScore * 0.5) + (SatisfactionScore * 0.3) + (PerformanceScore * 0.2);

    round(OverallScore).

%% @brief Calculate KPI values
calculate_kpi_values(State) ->
    maps:map(fun(Name, KPI) ->
        Value = calculate_kpi_value(KPI, State);
        #{
            name => Name,
            value => Value,
            target => KPI#kpi_definition.target,
            performance => calculate_kpi_performance(Value, KPI#kpi_definition.target),
            unit => KPI#kpi_definition.unit,
            category => KPI#kpi_definition.category
        }
    end, State#state.kpis).

%% @brief Calculate individual KPI value
calculate_kpi_value(KPI, State) ->
    %% Parse calculation formula and evaluate
    Formula = KPI#kpi_definition.calculation;

    case Formula of
        "avg(last_day)" ->
            %% Calculate average of last day
            History = get_metric_history(KPI#kpi_definition.name, 86400000),
            calculate_metric_stats(History)#{average};
        "count(last_hour)" ->
            %% Count metrics in last hour
            History = get_metric_history(KPI#kpi_definition.name, 3600000),
            length(History);
        "max(last_week)" ->
            %% Get maximum in last week
            History = get_metric_history(KPI#kpi_definition.name, 604800000),
            lists:max([M#business_metric.value || M <- History]);
        _ ->
            0.0  % Default value
    end.

%% @brief Calculate KPI performance
calculate_kpi_performance(Value, Target) ->
    case Target of
        0 -> 0.0;
        _ -> (Value / Target) * 100.0
    end.

%% @brief Calculate satisfaction metrics
calculate_satisfaction_metrics(State) ->
    SatisfactionData = State#state.satisfaction;

    %% Calculate overall satisfaction
    Ratings = [S#customer_satisfaction.rating || S <- SatisfactionData],
    Overall = case Ratings of
        [] -> 0.0;
        _ -> lists:sum(Ratings) / length(Ratings)
    end;

    %% Calculate by category
    CategoryRatings = lists:foldl(fun(S, Acc) ->
        Category = S#customer_satisfaction.category;
        maps:update_with(Category, fun(List) -> [S#customer_satisfaction.rating | List] end, [S#customer_satisfaction.rating], Acc)
    end, #{}, SatisfactionData);

    CategoryScores = maps:map(fun(_Category, Values) ->
        lists:sum(Values) / length(Values)
    end, CategoryRatings);

    %% Calculate trend
    Trend = calculate_satisfaction_trend(SatisfactionData);

    #{
        overall => Overall,
        categories => CategoryScores,
        trend => Trend,
        count => length(SatisfactionData),
        period => "last_24h"
    }.

%% @brief Calculate satisfaction trend
calculate_satisfaction_trend(SatisfactionData) ->
    case length(SatisfactionData) of
        0 -> stable;
        N when N < 10 -> unknown;
        _ ->
            Recent = lists:sublist(SatisfactionData, 10),
            Old = lists:sublist(SatisfactionData, length(SatisfactionData) - 10, 10);

            RecentAvg = lists:sum([S#customer_satisfaction.rating || S <- Recent]) / 10;
            OldAvg = lists:sum([S#customer_satisfaction.rating || S <- Old]) / 10;

            Diff = RecentAvg - OldAvg;

            if Diff > 0.1 -> improving;
               Diff < -0.1 -> declining;
               true -> stable
            end
    end.

%% @brief Get performance metrics
get_performance_metrics(State) ->
    %% Get system performance metrics
    Metrics = maps:values(State#state.metrics),

    %% Extract performance-related metrics
    PerformanceMetrics = lists:filter(fun(M) ->
        lists:member(M#business_metric.name, [
            "response_time",
            "throughput",
            "availability",
            "error_rate"
        ])
    end, Metrics);

    #{
        average_response_time => get_metric_value("response_time", Metrics),
        throughput => get_metric_value("throughput", Metrics),
        availability => get_metric_value("availability", Metrics),
        error_rate => get_metric_value("error_rate", Metrics)
    }.

%% @brief Get metric value from list
get_metric_value(Name, Metrics) ->
    case lists:filter(fun(M) -> M#business_metric.name =:= Name end, Metrics) of
        [] -> 0.0;
        [M | _] -> M#business_metric.value
    end.

%% @brief Generate charts data
generate_charts_data(State) ->
    %% Generate data for various charts
    #{
        kpi_progress => generate_kpi_progress_chart(State),
        satisfaction_trend => generate_satisfaction_chart(State),
        performance_metrics => generate_performance_chart(State),
        alert_timeline => generate_alert_chart(State)
    }.

%% @brief Generate KPI progress chart
generate_kpi_progress_chart(State) ->
    KPIs = maps:values(State#state.kpis);
    lists:map(fun(KPI) ->
        Value = calculate_kpi_value(KPI, State);
        Target = KPI#kpi_definition.target;
        #{
            name => KPI#kpi_definition.name,
            value => Value,
            target => Target,
            percentage => calculate_kpi_performance(Value, Target)
        }
    end, KPIs).

%% @brief Generate satisfaction chart
generate_satisfaction_chart(State) ->
    SatisfactionData = State#state.satisfaction;
    Timeline = lists:map(fun(S) ->
        #{
            timestamp => S#customer_satisfaction.timestamp,
            rating => S#customer_satisfaction.rating,
            category => S#customer_satisfaction.category
        }
    end, SatisfactionData).

%% @brief Generate performance chart
generate_performance_chart(State) ->
    Metrics = maps:values(State#state.metrics);
    lists:map(fun(M) ->
        #{
            timestamp => M#business_metric.timestamp,
            name => M#business_metric.name,
            value => M#business_metric.value
        }
    end, Metrics).

%% @brief Generate alert chart
generate_alert_chart(State) ->
    Alerts = State#state.alerts;
    lists:map(fun(A) ->
        #{
            timestamp => erlang:system_time(millisecond),  % Would need to track alert timestamps
            severity => A#business_alert.severity,
            metric => A#business_alert.metric,
            active => A#business_alert.active
        }
    end, Alerts).

%% @brief Generate business report
generate_business_report(State) ->
    Dashboard = generate_business_dashboard(State);

    %% Calculate business impact
    BusinessImpact = calculate_business_impact(State);

    %% Generate recommendations
    Recommendations = generate_business_recommendations(Dashboard);

    #{
        executive_summary => generate_executive_summary(Dashboard),
        detailed_metrics => Dashboard,
        business_impact => BusinessImpact,
        recommendations => Recommendations,
        generated_at => erlang:system_time(millisecond)
    }.

%% @brief Calculate business impact
calculate_business_impact(State) ->
    %% Estimate business impact based on metrics
    #{
        estimated_revenue_impact => calculate_revenue_impact(State),
        customer_retention_impact => calculate_retention_impact(State),
        operational_efficiency => calculate_efficiency_impact(State),
        risk_assessment => calculate_risk_assessment(State)
    }.

%% @brief Generate business recommendations
generate_business_recommendations(Dashboard) ->
    Recommendations = [];

    %% Check KPI performance
    maps:map(fun(Name, KPIData) ->
        case KPIData#{performance} of
            Perf when Perf < 80 ->
                #{
                    category => "KPI Improvement",
                    priority => "High",
                    metric => Name,
                    recommendation => io_lib:format("Improve ~s to meet target", [Name]),
                    impact => "Positive"
                };
            _ ->
                undefined
        end
    end, Dashboard#{kpis});

    %% Filter out undefined recommendations
    lists:filter(fun(R) -> R =/= undefined end, Recommendations).

%% @brief Generate executive summary
generate_executive_summary(Dashboard) ->
    #{
        overall_health => Dashboard#{health_score},
        key_highlights => identify_key_highlights(Dashboard),
        critical_issues => identify_critical_issues(Dashboard),
        strategic_outlook => generate_strategic_outlook(Dashboard)
    }.

%% @brief Identify key highlights
identify_key_highlights(Dashboard) ->
    Highlights = [];

    %% Check for good performance
    case Dashboard#{health_score} of
        Score when Score >= 90 ->
            [#{area => "Overall Performance", status => "Excellent"}];
        _ ->
            []
    end.

%% @brief Identify critical issues
identify_critical_issues(Dashboard) ->
    Issues = [];

    %% Check for critical alerts
    CriticalAlerts = [A || A <- Dashboard#{alerts}, A#business_alert.severity =:= critical];

    case CriticalAlerts of
        [] -> Issues;
        _ ->
            [#{issue => "Critical Business Alerts", count => length(CriticalAlerts)} | Issues]
    end.

%% @brief Generate strategic outlook
generate_strategic_outlook(Dashboard) ->
    #{
        growth_prospects => assess_growth_prospects(Dashboard),
        risk_factors => assess_risk_factors(Dashboard),
        opportunities => identify_opportunities(Dashboard)
    }.

%% @brief Assess growth prospects
assess_growth_prospects(Dashboard) ->
    case Dashboard#{satisfaction}#{trend} of
        improving -> "High";
        stable -> "Medium";
        declining -> "Low";
        _ -> "Unknown"
    end.

%% @brief Assess risk factors
assess_risk_factors(Dashboard) ->
    case Dashboard#{performance}#{error_rate} of
        Error when Error > 0.05 -> "High";
        Error when Error > 0.02 -> "Medium";
        _ -> "Low"
    end.

%% @brief Identify opportunities
identify_opportunities(Dashboard) ->
    Opportunities = [];

    %% Check for growth areas
    case Dashboard#{kpis} of
        #{some_kpi := Perf} when Perf < 100 ->
            [#{opportunity => "Improve KPI performance", impact => "High"} | Opportunities];
        _ ->
            Opportunities
    end.

%% @brief Calculate satisfaction score
calculate_satisfaction_score(State) ->
    SatisfactionData = State#state.satisfaction;
    case SatisfactionData of
        [] -> 0.0;
        _ ->
            Ratings = [S#customer_satisfaction.rating || S <- SatisfactionData];
            lists:sum(Ratings) / length(Ratings)
    end.

%% @brief Calculate performance score
calculate_performance_score(State) ->
    Performance = get_performance_metrics(State);
    WeightedScore = (Performance#{availability} * 0.4) +
                   ((100.0 - Performance#{error_rate} * 100.0) * 0.3) +
                   (min(Performance#{throughput}, 100.0) * 0.3);
    WeightedScore.

%% @brief Calculate revenue impact
calculate_revenue_impact(State) ->
    %% Simplified revenue calculation
    Satisfaction = calculate_satisfaction_score(State);
    Impact = Satisfaction * 1000.0;  // Assuming $1000 per satisfaction point
    Impact.

%% @brief Calculate retention impact
calculate_retention_impact(State) ->
    Satisfaction = calculate_satisfaction_score(State);
    Impact = Satisfaction * 0.95;  // 95% retention per satisfaction point
    Impact.

%% @brief Calculate efficiency impact
calculate_efficiency_impact(State) ->
    Performance = get_performance_metrics(State);
    avg([Performance#{throughput}, Performance#{availability}]).

%% @brief Calculate risk assessment
calculate_risk_assessment(State) ->
    ErrorRate = get_performance_metrics(State)#{error_rate};
    case ErrorRate of
        Rate when Rate > 0.1 -> "High";
        Rate when Rate > 0.05 -> "Medium";
        _ -> "Low"
    end.

%% Helper functions
avg(List) -> lists:sum(List) / length(List).

%% @brief Load default KPIs
load_default_kpis() ->
    maps:from_list([
        {<<"customer_satisfaction">>, #kpi_definition{
            name => <<"customer_satisfaction">>,
            description => <<"Customer satisfaction score">>,
            unit => <<"score">>,
            target => 4.5,
            weight => 1.0,
            category => <<"customer">>,
            calculation => <<"avg(last_day)">>
        }},
        {<<"system_uptime">>, #kpi_definition{
            name => <<"system_uptime">>,
            description => <<"System availability percentage">>,
            unit => <<"percent">>,
            target => 99.9,
            weight => 1.0,
            category => <<"reliability">>,
            calculation => <<"avg(last_day)">>
        }},
        {<<"average_response_time">>, #kpi_definition{
            name => <<"average_response_time">>,
            description => <<"Average response time in ms">>,
            unit => <<"ms">>,
            target => 100.0,
            weight => 1.0,
            category => <<"performance">>,
            calculation => <<"avg(last_day)">>
        }},
        {<<"transaction_volume">>, #kpi_definition{
            name => <<"transaction_volume">>,
            description => <<"Number of transactions per hour">>,
            unit => <<"count">>,
            target => 1000.0,
            weight => 1.0,
            category => <<"growth">>,
            calculation => <<"count(last_hour)">>
        }}
    ]).

%% @brief Load default alerts
load_default_alerts() ->
    [
        #business_alert{
            id => <<"csat_low">>,
            metric => <<"customer_satisfaction">>,
            condition => <<"lt">>,
            threshold => 3.0,
            severity => high,
            message => <<"Customer satisfaction below target">>,
            active => true
        },
        #business_alert{
            id => <<"downtime">>,
            metric => <<"system_uptime">>,
            condition => <<"lt">>,
            threshold => 99.0,
            severity => critical,
            message => <<"System experiencing downtime">>,
            active => true
        },
        #business_alert{
            id => <<"slow_responses">>,
            metric => <<"average_response_time">>,
            condition => <<"gt">>,
            threshold => 500.0,
            severity => medium,
            message => <<"Response time exceeding threshold">>,
            active => true
        }
    ].

%% @brief Generate unique alert ID
generate_alert_id() ->
    Id = crypto:strong_rand_bytes(8);
    integer_to_binary(binary:decode_unsigned(Id), 16).