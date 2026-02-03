%%====================================================================
%% erlmcp Cost Monitoring and Alerting Implementation
%%====================================================================
%%
%% This module implements real-time cost monitoring, budget tracking, and
%% intelligent alerting for erlmcp v3 deployment.
%%

-module(erlmcp_cost_monitor).

-behaviour(gen_server).

-export([start_link/0, track_cost/2, set_budget/2, get_cost_report/1,
         check_thresholds/0, send_alert/1, get_forecast/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Records for cost tracking
-record(cost_entry,
        {id :: binary(),
         timestamp :: integer(),
         category :: binary(),
         subcategory :: binary(),
         amount :: float(),
         currency :: binary(),
         allocation :: binary(),
         metadata :: map()}).

-record(budget,
        {id :: binary(),
         name :: binary(),
         amount :: float(),
         currency :: binary(),
         period :: daily | weekly | monthly | quarterly,
         start_date :: integer(),
         end_date :: integer(),
         spent :: float(),
          remaining :: float(),
          threshold :: float(),
          alert_channels :: [binary()]}).

%% Records for cost analysis
-record(cost_analysis,
        {period :: {integer(), integer()},
        total_cost :: float(),
        by_category :: map(),
        by_allocation :: map(),
        trends :: list(),
        forecasts :: list(),
        anomalies :: list()}).

%% Records for alerts
-record(alert,
        {id :: binary(),
        type :: binary(),
        severity :: critical | high | medium | low | info,
        title :: binary(),
        message :: binary(),
        timestamp :: integer(),
        resolved :: boolean(),
        resolved_at :: integer() | undefined,
        metadata :: map()}).

%% Records for forecast
-record(cost_forecast,
        {period :: {integer(), integer()},
        scenarios :: [#{scenario := binary(),
                       cost := float(),
                       confidence := float(),
                       factors := [binary()]}],
        recommendations :: [binary()]}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec track_cost(binary(), binary(), float(), binary(), binary()) -> ok.
track_cost(Category, Subcategory, Amount, Allocation, Currency) ->
    gen_server:cast(?MODULE, {track_cost, Category, Subcategory, Amount, Allocation, Currency}).

-spec set_budget(binary(), map()) -> ok | {error, term()}.
set_budget(BudgetId, Config) ->
    gen_server:call(?MODULE, {set_budget, BudgetId, Config}, 10000).

-spec get_cost_report({integer(), integer()}) -> #cost_analysis{}.
get_cost_report(Period) ->
    gen_server:call(?MODULE, {get_cost_report, Period}, 5000).

-spec check_thresholds() -> ok.
check_thresholds() ->
    gen_server:cast(?MODULE, check_thresholds).

-spec send_alert(map()) -> ok.
send_alert(AlertConfig) ->
    gen_server:cast(?MODULE, {send_alert, AlertConfig}).

-spec get_forecast() -> #cost_forecast{}.
get_forecast() ->
    gen_server:call(?MODULE, get_forecast, 5000).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, map()}.
init([]) ->
    %% Initialize cost tracking
    CostEntries = load_cost_entries(),

    %% Initialize budgets
    Budgets = load_budgets(),

    %% Initialize alerts
    Alerts = load_alerts(),

    %% Start monitoring
    schedule_monitoring(),

    %% Start budget checking
    schedule_budget_check(),

    %% Start forecasting
    schedule_forecasting(),

    State = #{
        cost_entries => CostEntries,
        budgets => Budgets,
        alerts => Alerts,
        last_check => undefined,
        monitoring_interval => 60000,  % 1 minute
        budget_check_interval => 300000,  % 5 minutes
        forecast_interval => 3600000,  % 1 hour
        currency => "USD"
    },

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, map()) -> {reply, term(), map()}.
handle_call({set_budget, BudgetId, Config}, _From, State) ->
    %% Create budget record
    Budget = create_budget(BudgetId, Config),

    %% Validate budget
    case validate_budget(Budget) of
        valid ->
            %% Store budget
            NewBudgets = maps:put(BudgetId, Budget, State#budgets),
            NewState = State#{
                budgets => NewBudgets
            },

            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_cost_report, Period}, _From, State) ->
    %% Generate cost analysis
    Analysis = generate_cost_analysis(Period, State),

    {reply, Analysis, State};

handle_call(get_forecast, _From, State) ->
    %% Generate cost forecast
    Forecast = generate_cost_forecast(State),

    {reply, Forecast, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast({track_cost, Category, Subcategory, Amount, Allocation, Currency}, State) ->
    %% Create cost entry
    CostEntry = create_cost_entry(Category, Subcategory, Amount, Allocation, Currency),

    %% Store cost entry
    NewEntries = [CostEntry | State#cost_entries],
    NewState = State#{
        cost_entries => NewEntries
    },

    %% Update budgets
    UpdatedBudgets = update_budgets(NewState),

    %% Check thresholds
    CostCategories = [Category, Subcategory],
    ThresholdExceeded = check_cost_thresholds(CostCategories, Amount),

    if
        ThresholdExceeded ->
            %% Send threshold alert
            send_threshold_alert(Category, Subcategory, Amount, Allocation);
        true ->
            ok
    end,

    {noreply, NewState#{
        budgets => UpdatedBudgets,
        last_check => os:system_time(millisecond)
    }};

handle_cast(check_thresholds, State) ->
    %% Check all budget thresholds
    check_all_budgets(State),

    %% Check cost anomalies
    check_cost_anomalies(State),

    %% Schedule next check
    schedule_budget_check(),

    {noreply, State};

handle_cast({send_alert, AlertConfig}, State) ->
    %% Create alert
    Alert = create_alert(AlertConfig),

    %% Store alert
    NewAlerts = [Alert | State#alerts],
    NewState = State#{
        alerts => NewAlerts
    },

    %% Send notifications
    send_alert_notifications(Alert),

    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(cost_monitoring, State) ->
    %% Perform cost monitoring
    CostSummary = calculate_cost_summary(State),

    %% Update metrics
    update_cost_metrics(CostSummary),

    %% Schedule next monitoring
    schedule_monitoring(),

    {noreply, State};

handle_info(budget_check, State) ->
    %% Check budgets
    ?MODULE:check_thresholds(),

    {noreply, State};

handle_info(cost_forecast, State) ->
    %% Generate forecast
    Forecast = generate_cost_forecast(State),

    %% Store forecast
    store_forecast(Forecast),

    %% Schedule next forecast
    schedule_forecasting(),

    {noreply, State};

handle_info(alert_resolution, State) ->
    %% Check for alerts to resolve
    ResolvedAlerts = check_resolved_alerts(State),

    %% Update state
    NewAlerts = lists:filter(fun(A) ->
        not lists:member(A, ResolvedAlerts)
    end, State#alerts),

    {noreply, State#{
        alerts => NewAlerts
    }};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    %% Clean up resources
    cleanup_cost_tracking(),
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Load cost entries
load_cost_entries() ->
    %% Query cost database
    Entries = erlmcp_database:find(#{
        collection => cost_entries,
        sort => #{timestamp => -1},
        limit => 1000
    }),

    %% Convert to records
    lists:map(fun cost_entry_to_record/1, Entries).

%% Load budgets
load_budgets() ->
    %% Query budget database
    Budgets = erlmcp_database:find(#{
        collection => budgets,
        filter => #{end_date => #{'$gt' => os:system_time(millisecond)}}
    }),

    %% Convert to records
    lists:map(fun budget_to_record/1, Budgets).

%% Load alerts
load_alerts() ->
    %% Query alert database
    Alerts = erlmcp_database:find(#{
        collection => alerts,
        filter => #{resolved => false}
    }),

    %% Convert to records
    lists:map(fun alert_to_record/1, Alerts).

%% Create cost entry
create_cost_entry(Category, Subcategory, Amount, Allocation, Currency) ->
    CostEntry = #cost_entry{
        id = generate_cost_id(),
        timestamp = os:system_time(millisecond),
        category = Category,
        subcategory = Subcategory,
        amount = Amount,
        currency = Currency,
        allocation = Allocation,
        metadata = #{}
    },

    %% Store in database
    erlmcp_database:insert(#{
        collection => cost_entries,
        document => CostEntry
    }),

    CostEntry.

%% Create budget
create_budget(BudgetId, Config) ->
    Amount = maps:get(amount, Config),
    Currency = maps:get(currency, Config, "USD"),
    Period = maps:get(period, Config, monthly),
    Threshold = maps:get(threshold, Config, 0.8),
    AlertChannels = maps:get(alert_channels, Config, []),

    %% Calculate dates
    {StartDate, EndDate} = calculate_budget_period(Period),

    %% Calculate initial values
    Spent = calculate_spent_amount(BudgetId, StartDate, EndDate),
    Remaining = Amount - Spent,

    Budget = #budget{
        id = BudgetId,
        name = maps:get(name, Config, BudgetId),
        amount = Amount,
        currency = Currency,
        period = Period,
        start_date = StartDate,
        end_date = EndDate,
        spent = Spent,
        remaining = Remaining,
        threshold = Threshold,
        alert_channels = AlertChannels
    },

    %% Store in database
    erlmcp_database:insert(#{
        collection => budgets,
        document => Budget
    }),

    Budget.

%% Create alert
create_alert(AlertConfig) ->
    Type = maps:get(type, AlertConfig),
    Severity = maps:get(severity, AlertConfig),
    Title = maps:get(title, AlertConfig),
    Message = maps:get(message, AlertConfig),
    Metadata = maps:get(metadata, AlertConfig, #{}),

    Alert = #alert{
        id = generate_alert_id(),
        type = Type,
        severity = Severity,
        title = Title,
        message = Message,
        timestamp = os:system_time(millisecond),
        resolved = false,
        metadata = Metadata
    },

    %% Store in database
    erlmcp_database:insert(#{
        collection => alerts,
        document => Alert
    }),

    Alert.

%% Generate cost analysis
generate_cost_analysis(Period, State) ->
    %% Filter cost entries for period
    FilteredEntries = filter_cost_entries_by_period(State#cost_entries, Period),

    %% Calculate total cost
    TotalCost = calculate_total_cost(FilteredEntries),

    %% Group by category
    ByCategory = group_cost_by_category(FilteredEntries),

    %% Group by allocation
    ByAllocation = group_cost_by_allocation(FilteredEntries);

    %% Calculate trends
    Trends = calculate_cost_trends(FilteredEntries);

    %% Generate forecasts
    Forecasts = generate_category_forecasts(FilteredEntries);

    %% Detect anomalies
    Anomalies = detect_cost_anomalies(FilteredEntries),

    #cost_analysis{
        period = Period,
        total_cost = TotalCost,
        by_category = ByCategory,
        by_allocation = ByAllocation,
        trends = Trends,
        forecasts = Forecasts,
        anomalies = Anomalies
    }.

%% Generate cost forecast
generate_cost_forecast(State) ->
    %% Get recent cost trends
    RecentTrends = get_recent_cost_trends(7),  % 7 days

    %% Generate scenarios
    Scenarios = generate_forecast_scenarios(RecentTrends);

    %% Generate recommendations
    Recommendations = generate_forecast_recommendations(Scenarios, State);

    #cost_forecast{
        period = get_current_period(),
        scenarios = Scenarios,
        recommendations = Recommendations
    }.

%% Update budgets
update_budgets(State) ->
    %% Update all budgets
    UpdatedBudgets = lists:map(fun(Budget) ->
        update_budget(Budget, State)
    end, maps:values(State#budgets)),

    %% Convert back to map
    lists:foldl(fun(Budget, Acc) ->
        maps:put(Budget#budget.id, Budget, Acc)
    end, #{}, UpdatedBudgets).

%% Update individual budget
update_budget(Budget, State) ->
    %% Calculate new spent amount
    NewSpent = calculate_spent_amount(Budget#budget.id,
                                     Budget#budget.start_date,
                                     os:system_time(millisecond)),

    %% Calculate new remaining
    NewRemaining = Budget#budget.amount - NewSpent;

    %% Check if threshold exceeded
    ThresholdExceeded = NewSpent / Budget#budget.amount >= Budget#budget.threshold;

    %% Create alert if threshold exceeded
    if
        ThresholdExceeded ->
            AlertConfig = #{
                type => budget_threshold,
                severity => high,
                title => "Budget Threshold Exceeded",
                message => io_lib:format("Budget ~p exceeded by ~p%",
                                        [Budget#budget.name,
                                         round((NewSpent / Budget#budget.amount) * 100)]),
                metadata => #{budget_id => Budget#budget.id,
                             spent => NewSpent,
                             amount => Budget#budget.amount}
            },
            ?MODULE:send_alert(AlertConfig);
        true ->
            ok
    end,

    %% Update budget
    Budget#budget{
        spent = NewSpent,
        remaining = NewRemaining
    }.

%% Check cost thresholds
check_cost_thresholds(CostCategories, Amount) ->
    %% Get threshold configuration
    ThresholdConfig = get_threshold_config(),

    %% Check each category
    lists:any(fun(Category) ->
        case maps:get(Category, ThresholdConfig, undefined) of
            undefined ->
                false;
            Threshold ->
                Amount >= Threshold
        end
    end, CostCategories).

%% Check all budgets
check_all_budgets(State) ->
    %% Check each budget
    lists:foreach(fun(Budget) ->
        check_budget_status(Budget)
    end, maps:values(State#budgets)).

%% Check budget status
check_budget_status(Budget) ->
    SpentRatio = Budget#budget.spent / Budget#budget.amount;

    case SpentRatio of
        Ratio when Ratio >= 1.0 ->
            %% Budget exceeded
            send_budget_alert(Budget, exceeded);
        Ratio when Ratio >= Budget#budget.threshold ->
            %% Threshold exceeded
            send_budget_alert(Budget, threshold);
        _ ->
            ok
    end.

%% Check cost anomalies
check_cost_anomalies(State) ->
    %% Get recent costs
    RecentCosts = get_recent_costs(7),  % 7 days

    %% Calculate moving average
    MovingAverage = calculate_moving_average(RecentCosts);

    %% Detect anomalies
    Anomalies = lists:filter(fun(Cost) ->
        is_cost_anomaly(Cost, MovingAverage)
    end, RecentCosts),

    %% Create alerts for anomalies
    lists:foreach(fun(Anomaly) ->
        AnomalyAlert = #{
            type => cost_anomaly,
            severity => medium,
            title => "Cost Anomaly Detected",
            message => io_lib:format("Unusual cost spike detected: ~p for ~p",
                                     [Anomaly#cost_entry.amount, Anomaly#cost_entry.category]),
            metadata => #{cost_id => Anomaly#cost_entry.id,
                         cost_amount => Anomaly#cost_entry.amount,
                         expected_amount => MovingAverage}
        },
        ?MODULE:send_alert(AnomalyAlert)
    end, Anomalies).

%% Send threshold alert
send_threshold_alert(Category, Subcategory, Amount, Allocation) ->
    AlertConfig = #{
        type => cost_threshold,
        severity => high,
        title => "Cost Threshold Exceeded",
        message = io_lib:format("Cost threshold exceeded for ~p/~p: ~p",
                               [Category, Subcategory, Amount]),
        metadata => #{category => Category,
                     subcategory => Subcategory,
                     amount => Amount,
                     allocation => Allocation}
    },

    ?MODULE:send_alert(AlertConfig).

%% Send budget alert
send_budget_alert(Budget, Reason) ->
    AlertConfig = #{
        type => budget_alert,
        severity => case Reason of
                       exceeded -> critical;
                       threshold -> high
                   end,
        title = io_lib:format("Budget ~p ~p", [Budget#budget.name, Reason]),
        message = io_lib:format("Budget ~p is ~p with ~p spent of ~p",
                               [Budget#budget.name, Reason,
                                Budget#budget.spent, Budget#budget.amount]),
        metadata = #{budget_id => Budget#budget.id,
                     reason => Reason,
                     spent => Budget#budget.spent,
                     amount => Budget#budget.amount}
    },

    ?MODULE:send_alert(AlertConfig).

%% Send alert notifications
send_alert_notifications(Alert) ->
    %% Get alert channels
    Channels = Alert#alert.metadata#{default => email, email => [],
                                     slack => [], webhook => []},

    %% Send through each channel
    lists:foreach(fun(Channel) ->
        send_alert_notification(Channel, Alert)
    end, maps:keys(Channels)).

%% Send alert notification through specific channel
send_alert_notification(Channel, Alert) ->
    case Channel of
        email ->
            send_email_alert(Alert);
        slack ->
            send_slack_alert(Alert);
        webhook ->
            send_webhook_alert(Alert);
        _ ->
            ok
    end.

%% Schedule monitoring
schedule_monitoring() ->
    erlang:send_after(?MODULE, cost_monitoring, get_env(monitoring_interval, 60000)).

%% Schedule budget check
schedule_budget_check() ->
    erlang:send_after(?MODULE, budget_check, get_env(budget_check_interval, 300000)).

%% Schedule forecasting
schedule_forecasting() ->
    erlang:send_after(?MODULE, cost_forecast, get_env(forecast_interval, 3600000)).

%% Helper functions
generate_cost_id() ->
    erlmcp_utils:generate_id().

generate_alert_id() ->
    erlmcp_utils:generate_id().

get_env(Key, Default) ->
    case application:get_env(erlmcp, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

log_info(Format, Args) ->
    erlmcp_logger:info(Format, Args).

log_error(Format, Args) ->
    erlmcp_logger:error(Format, Args).

%% Period calculation
calculate_budget_period(Period) ->
    Now = os:system_time(millisecond),
    case Period of
        daily ->
            Start = calendar:datetime_to_gregorian_seconds(calendar:local_time()) * 1000,
            End = Start + 86400000;  % 24 hours
        weekly ->
            Start = Now - (Now - 86400000 * (calendar:day_of_the_week(os:date()) - 1)),
            End = Start + 604800000;  % 7 days
        monthly ->
            Start = calendar:datetime_to_gregorian_seconds(calendar:date_to_gregorian_days(os:date()) - 1) * 1000,
            End = Start + 2592000000;  % 30 days
        quarterly ->
            Start = calendar:datetime_to_gregorian_seconds(calendar:date_to_gregorian_days(os:date()) - 1) * 1000,
            End = Start + 7776000000  % 90 days
    end.

%% Cost calculation
calculate_total_cost(CostEntries) ->
    lists:sum([C#cost_entry.amount || C <- CostEntries]).

group_cost_by_category(CostEntries) ->
    lists:foldl(fun(C, Acc) ->
        Category = C#cost_entry.category,
        maps:update(Category, maps:get(Category, Acc, 0) + C#cost_entry.amount, Acc)
    end, #{}, CostEntries).

group_cost_by_allocation(CostEntries) ->
    lists:foldl(fun(C, Acc) ->
        Allocation = C#cost_entry.allocation,
        maps:update(Allocation, maps:get(Allocation, Acc, 0) + C#cost_entry.amount, Acc)
    end, #{}, CostEntries).

calculate_cost_trends(CostEntries) ->
    %% Calculate cost trend over time
    SortedEntries = lists:sort(fun(C1, C2) ->
        C1#cost_entry.timestamp < C2#cost_entry.timestamp
    end, CostEntries),

    %% Calculate daily totals
    DailyTotals = calculate_daily_totals(SortedEntries),

    %% Calculate trend
    Trend = calculate_trend_line(DailyTotals),

    Trend.

generate_category_forecasts(CostEntries) ->
    %% Generate forecast for each category
    Categories = lists:usort([C#cost_entry.category || C <- CostEntries]),

    Forecasts = lists:map(fun(Category) ->
        CategoryCosts = [C#cost_entry.amount || C <- CostEntries,
                                             C#cost_entry.category =:= Category],

        case length(CategoryCosts) > 3 of
            true ->
                Forecast = forecast_category_cost(CategoryCosts),
                #{category => Category,
                  forecast => Forecast,
                  confidence => calculate_forecast_confidence(CategoryCosts)};
            false ->
                #{category => Category,
                  forecast => undefined,
                  confidence => 0.0}
        end
    end, Categories),

    Forecasts.

detect_cost_anomalies(CostEntries) ->
    %% Calculate statistical anomalies
    Amounts = [C#cost_entry.amount || C <- CostEntries],
    Mean = lists:sum(Amounts) / length(Amounts),
    StdDev = calculate_standard_deviation(Amounts),

    %% Find anomalies (values > 2 standard deviations from mean)
    Anomalies = lists:filter(fun(C) ->
        Amount = C#cost_entry.amount,
        abs(Amount - Mean) > 2 * StdDev
    end, CostEntries),

    Anomalies.

is_cost_anomaly(Cost, MovingAverage) ->
    Amount = Cost#cost_entry.amount,
    abs(Amount - MovingAverage) / MovingAverage > 0.5.

get_threshold_config() ->
    %% Get threshold configuration
    case application:get_env(erlmcp, cost_thresholds) of
        {ok, Config} -> Config;
        undefined -> #{}
    end.

validate_budget(Budget) ->
    %% Validate budget configuration
    case Budget#budget.amount > 0 of
        true ->
            validate_budget_dates(Budget);
        false ->
            {error, invalid_amount}
    end.

validate_budget_dates(Budget) ->
    StartDate = Budget#budget.start_date,
    EndDate = Budget#budget.end_date,

    case EndDate > StartDate of
        true ->
            valid;
        false ->
            {error, invalid_dates}
    end.

calculate_spent_amount(BudgetId, StartDate, EndDate) ->
    %% Query spent amount for budget period
    Query = #{
        collection => cost_entries,
        filter => #{allocation => BudgetId,
                    timestamp => #{'$gte' => StartDate, '$lte' => EndDate}}
    },

    Entries = erlmcp_database:find(Query),
    lists:sum([E#cost_entry.amount || E <- Entries]).

update_cost_metrics(CostSummary) ->
    %% Update Prometheus metrics
    erlmcp_metrics:record_metric(<<"total_cost_usd">>, CostSummary#cost_analysis.total_cost, #{}),

    %% Update category metrics
    maps:foreach(fun(Category, Amount) ->
        erlmcp_metrics:record_metric(<<"cost_by_category">>, Amount,
                                    #{<<"category">> => Category})
    end, CostSummary#cost_analysis.by_category).

store_forecast(Forecast) ->
    %% Store forecast in database
    erlmcp_database:insert(#{
        collection => cost_forecasts,
        document => Forecast
    }).

check_resolved_alerts(State) ->
    %% Check alerts that should be resolved
    CurrentTime = os:system_time(millisecond),
    ResolvedThreshold = CurrentTime - 86400000,  % 24 hours

    lists:filter(fun(Alert) ->
        Alert#alert.resolved =:= true andalso
        Alert#alert.resolved_at =/= undefined andalso
        Alert#alert.resolved_at > ResolvedThreshold
    end, State#alerts).

get_current_period() ->
    Now = os:system_time(millisecond),
    {calendar:datetime_to_gregorian_seconds(calendar:local_time()) div 86400,
     calendar:datetime_to_gregorian_seconds(calendar:local_time()) div 86400}.

get_recent_days(Days) ->
    End = os:system_time(millisecond),
    Start = End - (Days * 86400000),
    {Start div 86400, End div 86400}.

get_recent_costs(Days) ->
    Period = get_recent_days(Days),
    filter_cost_entries_by_period(get_all_cost_entries(), Period).

get_all_cost_entries() ->
    %% Get all cost entries
    Entries = erlmcp_database:find(#{
        collection => cost_entries,
        sort => #{timestamp => -1},
        limit => 10000
    }),

    %% Convert to records
    lists:map(fun cost_entry_to_record/1, Entries).

filter_cost_entries_by_period(CostEntries, Period) ->
    Start = Period#period.start * 86400,
    End = Period#period.end * 86400,

    lists:filter(fun(Entry) ->
        Timestamp = Entry#cost_entry.timestamp,
        Timestamp >= Start andalso Timestamp =< End
    end, CostEntries).

calculate_daily_totals(CostEntries) ->
    %% Group by day
    DailyGroups = lists:foldl(fun(C, Acc) ->
        Day = C#cost_entry.timestamp div 86400,
        maps:update(Day, maps:get(Day, Acc, 0) + C#cost_entry.amount, Acc)
    end, #{}, CostEntries),

    %% Convert to list
    lists:sort(fun({D1, _}, {D2, _}) -> D1 < D2 end, maps:to_list(DailyGroups)).

calculate_trend_line(DailyTotals) ->
    if
        length(DailyTotals) < 2 ->
            undefined;
        true ->
            %% Simple linear regression
            {Slope, Intercept} = linear_regression(DailyTotals),
            {Slope, Intercept}
    end.

linear_regression(Data) ->
    N = length(Data),
    XValues = [X || {X, _} <- Data],
    YValues = [Y || {_, Y} <- Data],

    SumX = lists:sum(XValues),
    SumY = lists:sum(YValues),
    SumXY = lists:sum([X * Y || {X, Y} <- Data]),
    SumXX = lists:sum([X * X || X <- XValues]),

    Slope = (N * SumXY - SumX * SumY) / (N * SumXX - SumX * SumX),
    Intercept = (SumY - Slope * SumX) / N,

    {Slope, Intercept}.

calculate_standard_deviation(Values) ->
    Mean = lists:sum(Values) / length(Values),
    Variance = lists:sum([(X - Mean) * (X - Mean) || X <- Values]) / length(Values),
    math:sqrt(Variance).

forecast_category_cost(Costs) ->
    if
        length(Costs) < 3 ->
            undefined;
        true ->
            %% Simple moving average
            Recent = lists:sublist(Costs, min(7, length(Costs))),
            lists:sum(Recent) / length(Recent)
    end.

calculate_forecast_confidence(Costs) ->
    if
        length(Costs) < 3 ->
            0.0;
        length(Costs) < 7 ->
            0.5;
        length(Costs) < 14 ->
            0.7;
        true ->
            0.9
    end.

generate_forecast_scenarios(TrendData) ->
    %% Generate different forecast scenarios
    BaseScenario = #{
        scenario => base_trend,
        cost => calculate_trend_cost(TrendData),
        confidence => 0.7,
        factors => ["historical_trends"]
    },

    OptimisticScenario = #{
        scenario => optimistic,
        cost => calculate_optimistic_cost(TrendData),
        confidence => 0.5,
        factors => ["optimistic_growth", "efficiency_improvements"]
    },

    PessimisticScenario = #{
        scenario => pessimistic,
        cost => calculate_pessimistic_cost(TrendData),
        confidence => 0.5,
        factors => ["pessimistic_growth", "unforeseen_costs"]
    },

    [BaseScenario, OptimisticScenario, PessimisticScenario].

calculate_trend_cost(TrendData) ->
    %% Calculate trend-based forecast
    case calculate_trend_line(TrendData) of
        undefined ->
            0.0;
        {Slope, _} ->
            %% Project forward 30 days
            Recent = lists:last(TrendData),
            CurrentDay = element(1, Recent),
            FutureDay = CurrentDay + 30,
            Slope * FutureDay
    end.

calculate_optimistic_cost(TrendData) ->
    %% Optimistic scenario (80% of trend)
    TrendCost = calculate_trend_cost(TrendData),
    TrendCost * 0.8.

calculate_pessimistic_cost(TrendData) ->
    %% Pessimistic scenario (120% of trend)
    TrendCost = calculate_trend_cost(TrendData),
    TrendCost * 1.2.

generate_forecast_recommendations(Scenarios, State) ->
    %% Generate recommendations based on forecasts
    Recommendations = [],

    %% Check for high cost scenarios
    lists:foreach(fun(Scenario) ->
        Cost = maps:get(cost, Scenario),
        if
            Cost > get_cost_threshold() ->
                Recommendation = generate_cost_reduction_recommendation(Scenario),
                Recommendations = [Recommendation | Recommendations];
            true ->
                ok
        end
    end, Scenarios),

    %% Budget recommendations
    BudgetRecommendations = generate_budget_recommendations(State),
    Recommendations ++ BudgetRecommendations.

get_cost_threshold() ->
    %% Get cost threshold for recommendations
    case application:get_env(erlmcp, cost_recommendation_threshold) of
        {ok, Threshold} -> Threshold;
        undefined -> 10000.0  % $10,000
    end.

generate_cost_reduction_recommendation(Scenario) ->
    %% Generate cost reduction recommendation
    ScenarioName = maps:get(scenario, Scenario),
    Cost = maps:get(cost, Scenario),

    Recommendation = case ScenarioName of
        base_trend ->
            "Review cost optimization opportunities";
        optimistic ->
            "Maintain current cost management strategy";
        pessimistic ->
            "Implement cost reduction measures"
    end,

    Recommendation.

generate_budget_recommendations(State) ->
    %% Generate budget recommendations
    Recommendations = [],

    lists:foreach(fun(Budget) ->
        Ratio = Budget#budget.spent / Budget#budget.amount,
        if
            Ratio >= 0.9 ->
                Recommendation = io_lib:format("Consider increasing budget ~p from ~p to ~p",
                                             [Budget#budget.name, Budget#budget.amount,
                                              Budget#budget.amount * 1.2]),
                Recommendations = [Recommendation | Recommendations];
            Ratio <= 0.5 ->
                Recommendation = io_lib:format("Consider reducing budget ~p from ~p to ~p",
                                             [Budget#budget.name, Budget#budget.amount,
                                              Budget#budget.amount * 0.8]),
                Recommendations = [Recommendation | Recommendations];
            true ->
                ok
        end
    end, maps:values(State#budgets)),

    Recommendations.

%% Notification functions
send_email_alert(Alert) ->
    %% Send email notification
    EmailConfig = get_email_config(),
    send_email(EmailConfig, Alert).

send_slack_alert(Alert) ->
    %% Send Slack notification
    SlackConfig = get_slack_config(),
    send_slack_message(SlackConfig, Alert).

send_webhook_alert(Alert) ->
    %% Send webhook notification
    WebhookConfig = get_webhook_config(),
    send_webhook(WebhookConfig, Alert).

get_email_config() ->
    %% Get email configuration
    case application:get_env(erlmcp, email_config) of
        {ok, Config} -> Config;
        undefined -> #{}
    end.

get_slack_config() ->
    %% Get Slack configuration
    case application:get_env(erlmcp, slack_config) of
        {ok, Config} -> Config;
        undefined -> #{}
    end.

get_webhook_config() ->
    %% Get webhook configuration
    case application:get_env(erlmcp, webhook_config) of
        {ok, Config} -> Config;
        undefined -> #{}
    end.

%% Conversion functions
cost_entry_to_record(Entry) ->
    #cost_entry{
        id = maps:get(id, Entry),
        timestamp = maps:get(timestamp, Entry),
        category = maps:get(category, Entry),
        subcategory = maps:get(subcategory, Entry),
        amount = maps:get(amount, Entry),
        currency = maps:get(currency, Entry),
        allocation = maps:get(allocation, Entry),
        metadata = maps:get(metadata, Entry, #{})
    }.

budget_to_record(Budget) ->
    #budget{
        id = maps:get(id, Budget),
        name = maps:get(name, Budget),
        amount = maps:get(amount, Budget),
        currency = maps:get(currency, Budget),
        period = maps:get(period, Budget),
        start_date = maps:get(start_date, Budget),
        end_date = maps:get(end_date, Budget),
        spent = maps:get(spent, Budget),
        remaining = maps:get(remaining, Budget),
        threshold = maps:get(threshold, Budget),
        alert_channels = maps:get(alert_channels, Budget, [])
    }.

alert_to_record(Alert) ->
    #alert{
        id = maps:get(id, Alert),
        type = maps:get(type, Alert),
        severity = maps:get(severity, Alert),
        title = maps:get(title, Alert),
        message = maps:get(message, Alert),
        timestamp = maps:get(timestamp, Alert),
        resolved = maps:get(resolved, Alert, false),
        resolved_at = maps:get(resolved_at, Alert, undefined),
        metadata = maps:get(metadata, Alert, #{})
    }.