%%%-------------------------------------------------------------------
%%% @doc
%%% SLA monitoring module for erlmcp v3
%%% Provides comprehensive SLA monitoring, reporting, and alerting
%%% with support for multiple SLA definitions and compliance reporting
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sla_monitor).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         register_sla/4,
         unregister_sla/1,
         check_sla_compliance/1,
         get_sla_report/0,
         get_sla_report/1,
         generate_sla_report/0,
         generate_sla_report/1,
         trigger_alert/2,
         get_sla_metrics/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_METRICS_RETENTION, 2592000000).  % 30 days in milliseconds
-define(DEFAULT_REPORT_INTERVAL, 86400000).     % 24 hours in milliseconds
-define(DEFAULT_ALERT_THRESHOLD, 0.8).          % 80% threshold
-define(SLA_THRESHOLD_BREACHED, sla_threshold_breached).

-include("erlmcp_observability.hrl").

%%====================================================================
%% Records
%%====================================================================
-record(uptime_data, {timestamp, value, metadata}).
-record(response_time_data, {timestamp, value, metadata}).
-record(error_rate_data, {timestamp, value, metadata}).
-record(throughput_data, {timestamp, value, metadata}).
-record(session_data, {timestamp, value, metadata}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start the SLA monitor with default configuration
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(#{
        metrics_retention => ?DEFAULT_METRICS_RETENTION,
        report_interval => ?DEFAULT_REPORT_INTERVAL,
        alert_threshold => ?DEFAULT_ALERT_THRESHOLD
    }).

%%--------------------------------------------------------------------
%% @doc
%% Start the SLA monitor with custom configuration
%% @end
%%--------------------------------------------------------------------
start_link(Config) when is_map(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%--------------------------------------------------------------------
%% @doc
%% Register a new SLA definition
%% @end
%%--------------------------------------------------------------------
-spec register_sla(binary(), float(), binary(), map()) -> ok | {error, term()}.
register_sla(SlaName, Target, Unit, Config) when is_binary(SlaName), is_float(Target), is_binary(Unit) ->
    gen_server:call(?MODULE, {register_sla, SlaName, Target, Unit, Config}).

%%--------------------------------------------------------------------
%% @doc
%% Unregister an SLA definition
%% @end
%%--------------------------------------------------------------------
-spec unregister_sla(binary()) -> ok.
unregister_sla(SlaName) when is_binary(SlaName) ->
    gen_server:call(?MODULE, {unregister_sla, SlaName}).

%%--------------------------------------------------------------------
%% @doc
%% Check SLA compliance for all SLAs
%% @end
%%--------------------------------------------------------------------
-spec check_sla_compliance(binary()) -> map().
check_sla_compliance(SlaName) ->
    gen_server:call(?MODULE, {check_sla_compliance, SlaName}).

%%--------------------------------------------------------------------
%% @doc
%% Get SLA report for all SLAs
%% @end
%%--------------------------------------------------------------------
-spec get_sla_report() -> map().
get_sla_report() ->
    gen_server:call(?MODULE, get_sla_report).

%%--------------------------------------------------------------------
%% @doc
%% Get SLA report for specific SLA
%% @end
%%--------------------------------------------------------------------
-spec get_sla_report(binary()) -> map() | {error, not_found}.
get_sla_report(SlaName) ->
    gen_server:call(?MODULE, {get_sla_report, SlaName}).

%%--------------------------------------------------------------------
%% @doc
%% Generate SLA report (pdf, html, json)
%% @end
%%--------------------------------------------------------------------
-spec generate_sla_report() -> {ok, binary()} | {error, term()}.
generate_sla_report() ->
    generate_sla_report(all).

generate_sla_report(SlaName) ->
    gen_server:call(?MODULE, {generate_sla_report, SlaName}).

%%--------------------------------------------------------------------
%% @doc
%% Trigger an SLA alert
%% @end
%%--------------------------------------------------------------------
-spec trigger_alert(binary(), float()) -> ok.
trigger_alert(SlaName, Value) ->
    gen_server:cast(?MODULE, {trigger_alert, SlaName, Value}).

%%--------------------------------------------------------------------
%% @doc
%% Get SLA metrics
%% @end
%%--------------------------------------------------------------------
-spec get_sla_metrics(binary()) -> map() | {error, not_found}.
get_sla_metrics(SlaName) ->
    gen_server:call(?MODULE, {get_sla_metrics, SlaName}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Config) ->
    %% Initialize SLA registry
    ets:new(erlmcp_sla_registry, [set, public, named_table]),
    ets:new(erlmcp_sla_metrics, [set, public, named_table]),
    ets:new(erlmcp_sla_compliance, [set, public, named_table]),
    ets:new(erlmcp_sla_alerts, [set, public, named_table]),

    %% Register default SLAs
    register_default_slas(),

    %% Start periodic report generation
    ReportInterval = maps:get(report_interval, Config, ?DEFAULT_REPORT_INTERVAL),
    erlang:send_after(ReportInterval, self(), generate_report),

    State = #{
        config => Config,
        slas => #{},
        metrics => #{},
        last_report => undefined,
        alert_threshold => maps:get(alert_threshold, Config, ?DEFAULT_ALERT_THRESHOLD)
    },

    {ok, State}.

handle_call({register_sla, SlaName, Target, Unit, Config}, _From, State) ->
    %% Validate SLA configuration
    case validate_sla_config(SlaName, Target, Unit, Config) of
        {ok, ValidConfig} ->
            %% Store SLA definition
            SlaDef = #{
                name => SlaName,
                target => Target,
                unit => Unit,
                config => ValidConfig,
                created_at => erlang:system_time(millisecond),
                metrics => []
            },
            ets:insert(erlmcp_sla_registry, {SlaName, SlaDef}),

            %% Initialize metrics storage
            init_sla_metrics(SlaName, ValidConfig),

            CurrentSlas = maps:get(slas, State, #{}),
            UpdatedSlas = maps:put(SlaName, SlaDef, CurrentSlas),
            {reply, ok, State#{slas => UpdatedSlas}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({unregister_sla, SlaName}, _From, State) ->
    %% Remove SLA definition
    ets:delete(erlmcp_sla_registry, SlaName),
    ets:delete(erlmcp_sla_metrics, SlaName),
    ets:delete(erlmcp_sla_compliance, SlaName),
    ets:delete(erlmcp_sla_alerts, SlaName),

    CurrentSlas = maps:get(slas, State, #{}),
    UpdatedSlas = maps:remove(SlaName, CurrentSlas),
    {reply, ok, State#{slas => UpdatedSlas}};

handle_call({check_sla_compliance, SlaName}, _From, State) ->
    Result = check_sla_compliance_internal(SlaName),
    ets:insert(erlmcp_sla_compliance, {SlaName, Result}),
    {reply, Result, State};

handle_call(get_sla_report, _From, State) ->
    Report = generate_slareport_internal(),
    {reply, Report, State};

handle_call({get_sla_report, SlaName}, _From, State) ->
    case ets:lookup(erlmcp_sla_compliance, SlaName) of
        [{_, Report}] ->
            {reply, Report, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({generate_sla_report, SlaName}, _From, State) ->
    case generate_slareport_internal(SlaName) of
        {ok, Report} ->
            {reply, Report, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_sla_metrics, SlaName}, _From, State) ->
    case ets:lookup(erlmcp_sla_metrics, SlaName) of
        [{_, Metrics}] ->
            {reply, Metrics, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({trigger_alert, SlaName, Value}, State) ->
    %% Check if alert threshold is breached
    case check_alert_threshold(SlaName, Value, State) of
        true ->
            %% Store alert
            Alert = #{
                sla_name => SlaName,
                value => Value,
                timestamp => erlang:system_time(millisecond),
                triggered => true
            },
            ets:insert(erlmcp_sla_alerts, {SlaName, Alert}),

            %% Send notification
            send_alert_notification(SlaName, Value);
        false ->
            ok
    end,

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(generate_report, State) ->
    %% Generate SLA report
    Report = generate_slareport_internal(),
    store_report(Report),

    %% Schedule next report
    Config = maps:get(config, State, #{}),
    ReportInterval = maps:get(report_interval, Config, ?DEFAULT_REPORT_INTERVAL),
    erlang:send_after(ReportInterval, self(), generate_report),

    {noreply, State#{last_report => erlang:system_time(millisecond)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

register_default_slas() ->
    %% Register common SLAs
    %% Uptime SLA
    register_sla(<<"uptime">>, 99.9, <<"percent">>, #{
        description => "System uptime",
        period => 86400000,  % 24 hours
        window_size => 60000  % 1 minute
    }),

    %% Response Time SLA
    register_sla(<<"response_time">>, 100.0, <<"milliseconds">>, #{
        description => "Average response time",
        period => 86400000,  % 24 hours
        window_size => 5000,   % 5 seconds
        percentile => 95
    }),

    %% Error Rate SLA
    register_sla(<<"error_rate">>, 0.01, <<"percent">>, #{
        description => "Error rate",
        period => 86400000,  % 24 hours
        window_size => 60000  % 1 minute
    }),

    %% Throughput SLA
    register_sla(<<"throughput">>, 10000.0, <<"requests_per_second">>, #{
        description => "Minimum throughput",
        period => 3600000,    % 1 hour
        window_size => 5000    % 5 seconds
    }),

    %% Session Success SLA
    register_sla(<<"session_success">>, 99.0, <<"percent">>, #{
        description => "Session success rate",
        period => 3600000,    % 1 hour
        window_size => 30000  % 30 seconds
    }),

    ok.

validate_sla_config(SlaName, Target, Unit, Config) ->
    %% Validate SLA configuration
    case is_binary(SlaName) of
        false -> {error, invalid_sla_name};
        true ->
            case is_float(Target) of
                false -> {error, invalid_target};
                true ->
                    case is_binary(Unit) of
                        false -> {error, invalid_unit};
                        true ->
                            %% Check for required config fields
                            case maps:get(description, Config, undefined) of
                                undefined -> {error, missing_description};
                                _ ->
                                    case maps:get(period, Config, undefined) of
                                        undefined -> {error, missing_period};
                                        _ ->
                                            {ok, Config}
                                    end
                            end
                    end
            end
    end.

init_sla_metrics(SlaName, Config) ->
    %% Initialize metrics storage for SLA
    Metrics = #{
        name => SlaName,
        target => maps:get(target, Config),
        period => maps:get(period, Config),
        window_size => maps:get(window_size, Config, 60000),
        percentile => maps:get(percentile, Config, 95),
        created_at => erlang:system_time(millisecond),
        time_series => [],
        compliance => [],
        alerts => []
    },

    ets:insert(erlmcp_sla_metrics, {SlaName, Metrics}).

check_sla_compliance_internal(SlaName) ->
    case ets:lookup(erlmcp_sla_metrics, SlaName) of
        [{_, Metrics}] ->
            %% Calculate compliance
            Compliance = calculate_compliance(Metrics),
            %% Store compliance result
            ComplianceResult = #{
                sla_name => SlaName,
                compliance => Compliance,
                timestamp => erlang:system_time(millisecond),
                status => calculate_compliance_status(Compliance)
            },
            ComplianceResult;
        [] ->
            {error, not_found}
    end.

calculate_compliance(Metrics) ->
    %% Extract relevant metrics
    TimeSeries = maps:get(time_series, Metrics),
    Target = maps:get(target, Metrics),

    %% Calculate compliance based on SLA type
    case maps:get(name, Metrics) of
        <<"uptime">> ->
            calculate_uptime_compliance(TimeSeries, Target);
        <<"response_time">> ->
            calculate_response_time_compliance(TimeSeries, Target);
        <<"error_rate">> ->
            calculate_error_rate_compliance(TimeSeries, Target);
        <<"throughput">> ->
            calculate_throughput_compliance(TimeSeries, Target);
        <<"session_success">> ->
            calculate_session_success_compliance(TimeSeries, Target);
        _ ->
            %% Default compliance calculation
            #{
                measured => 0.0,
                target => Target,
                achieved => false,
                gap => Target,
                percentage => 0.0,
                metric => unknown
            }
    end.

calculate_uptime_compliance(TimeSeries, Target) ->
    %% Calculate uptime compliance
    UptimeData = extract_uptime_data(TimeSeries),
    Uptime = calculate_uptime_percentage(UptimeData),

    Compliance = #{
        measured => Uptime,
        target => Target,
        achieved => Uptime >= Target,
        gap => Target - Uptime,
        percentage => (Uptime / Target) * 100
    },
    Compliance.

calculate_response_time_compliance(TimeSeries, Target) ->
    %% Calculate response time compliance
    ResponseTimeData = extract_response_time_data(TimeSeries),
    Percentile95 = calculate_percentile(ResponseTimeData, 95),

    Compliance = #{
        measured => Percentile95,
        target => Target,
        achieved => Percentile95 =< Target,
        gap => Percentile95 - Target,
        percentage => (Target / Percentile95) * 100
    },
    Compliance.

calculate_error_rate_compliance(TimeSeries, Target) ->
    %% Calculate error rate compliance
    ErrorRateData = extract_error_rate_data(TimeSeries),
    ErrorRate = calculate_error_rate(ErrorRateData),

    Compliance = #{
        measured => ErrorRate,
        target => Target,
        achieved => ErrorRate =< Target,
        gap => Target - ErrorRate,
        percentage => (1 - (ErrorRate / Target)) * 100
    },
    Compliance.

calculate_throughput_compliance(TimeSeries, Target) ->
    %% Calculate throughput compliance
    ThroughputData = extract_throughput_data(TimeSeries),
    AvgThroughput = calculate_average(ThroughputData),

    Compliance = #{
        measured => AvgThroughput,
        target => Target,
        achieved => AvgThroughput >= Target,
        gap => AvgThroughput - Target,
        percentage => (AvgThroughput / Target) * 100
    },
    Compliance.

calculate_session_success_compliance(TimeSeries, Target) ->
    %% Calculate session success compliance
    SessionData = extract_session_data(TimeSeries),
    SuccessRate = calculate_session_success_rate(SessionData),

    Compliance = #{
        measured => SuccessRate,
        target => Target,
        achieved => SuccessRate >= Target,
        gap => SuccessRate - Target,
        percentage => (SuccessRate / Target) * 100
    },
    Compliance.

calculate_compliance_status(Compliance) ->
    case maps:get(achieved, Compliance) of
        true -> compliant;
        false -> non_compliant
    end.

check_alert_threshold(SlaName, Value, State) ->
    %% Check if alert threshold is breached
    Config = maps:get(config, State, #{}),
    Threshold = maps:get(alert_threshold, Config, ?DEFAULT_ALERT_THRESHOLD),

    case ets:lookup(erlmcp_sla_registry, SlaName) of
        [{_, #{config := Config}}] ->
            case maps:get(alert_threshold, Config, Threshold) of
                Thresh ->
                    Value < Thresh
            end;
        [] ->
            false
    end.

send_alert_notification(SlaName, Value) ->
    %% Send notification about SLA alert
    AlertMessage = #{
        message => "SLA threshold breached",
        sla_name => SlaName,
        value => Value,
        timestamp => erlang:system_time(millisecond)
    },

    %% Send to monitoring system
    try
        erlmcp_monitoring_metrics:record_security_event(
            sla_threshold_breached,
            AlertMessage
        )
    catch
        _:_ -> ok
    end.

generate_slareport_internal() ->
    generate_slareport_internal(all).

generate_slareport_internal(SlaName) ->
    case SlaName of
        all ->
            %% Generate report for all SLAs
            SLAs = ets:tab2list(erlmcp_sla_compliance),
            generate_comprehensive_report(SLAs);
        _ ->
            case ets:lookup(erlmcp_sla_compliance, SlaName) of
                [{_, Report}] ->
                    generate_single_report(SlaName, Report);
                [] ->
                    {error, not_found}
            end
    end.

generate_comprehensive_report(SLAs) ->
    %% Generate comprehensive SLA report
    Report = #{
        report_type => comprehensive,
        generated_at => erlang:system_time(millisecond),
        period => get_report_period(),
        slas => lists:map(fun({_, Report}) -> Report end, SLAs),
        summary => generate_summary(SLAs),
        recommendations => generate_recommendations(SLAs)
    },

    {ok, Report}.

generate_single_report(SlaName, Report) ->
    %% Generate single SLA report
    SingleReport = #{
        report_type => single,
        sla_name => SlaName,
        generated_at => erlang:system_time(millisecond),
        compliance => Report,
        details => generate_detailed_report(SlaName)
    },

    {ok, SingleReport}.

generate_summary(SLAs) ->
    %% Generate summary of SLA compliance
    Total = length(SLAs),
    Compliant = lists:foldl(fun({_, #{status := Status}}, Acc) ->
        case Status of
            compliant -> Acc + 1;
            _ -> Acc
        end
    end, 0, SLAs),

    #{
        total_slas => Total,
        compliant_slas => Compliant,
        non_compliant_slas => Total - Compliant,
        compliance_percentage => case Total of
            0 -> 0;
            _ -> (Compliant / Total) * 100
        end
    }.

generate_recommendations(SLAs) ->
    %% Generate recommendations for improvement
    Recommendations = lists:flat_map(fun({_, Report}) ->
        generate_sla_recommendations(Report)
    end, SLAs),

    Recommendations.

generate_sla_recommendations(Compliance) ->
    %% Generate recommendations for specific SLA
    case maps:get(achieved, Compliance) of
        true -> [];
        false ->
            case maps:get(gap, Compliance) of
                Gap when Gap < 0.01 ->
                    [#{recommendation => "Minor SLA breach - monitoring required"}];
                Gap when Gap < 0.05 ->
                    [#{recommendation => "Moderate SLA breach - investigate cause"}];
                Gap when Gap >= 0.05 ->
                    [#{recommendation => "Critical SLA breach - immediate action required"}]
            end
    end.

store_report(Report) ->
    %% Store SLA report
    ReportId = generate_report_id(),
    StoredReport = Report#{report_id => ReportId, stored_at => erlang:system_time(millisecond)},
    ets:insert(erlmcp_sla_reports, {ReportId, StoredReport}).

generate_report_id() ->
    binary_to_list(base64:encode(crypto:strong_rand_bytes(16))).

get_report_period() ->
    %% Get current report period
    %% This would typically be based on the current date/time
    {_, _, Day} = erlang:date(),
    Day.

%%====================================================================
%% Data Extraction Helpers
%%====================================================================

extract_uptime_data(TimeSeries) ->
    %% Extract uptime data from time series
    lists:filter(fun(Item) ->
        case Item of
            #uptime_data{} -> true;
            _ -> false
        end
    end, TimeSeries).

extract_response_time_data(TimeSeries) ->
    %% Extract response time data from time series
    lists:filter(fun(Timestamp) ->
        is_record(Timestamp, response_time_data)
    end, TimeSeries).

extract_error_rate_data(TimeSeries) ->
    %% Extract error rate data from time series
    lists:filter(fun(Timestamp) ->
        is_record(Timestamp, error_rate_data)
    end, TimeSeries).

extract_throughput_data(TimeSeries) ->
    %% Extract throughput data from time series
    lists:filter(fun(Timestamp) ->
        is_record(Timestamp, throughput_data)
    end, TimeSeries).

extract_session_data(TimeSeries) ->
    %% Extract session data from time series
    lists:filter(fun(Timestamp) ->
        is_record(Timestamp, session_data)
    end, TimeSeries).

%%====================================================================
%% Calculation Helpers
%%====================================================================

calculate_uptime_percentage(UptimeData) ->
    %% Calculate uptime percentage
    case UptimeData of
        [] -> 0;
        _ ->
            TotalDuration = lists:sum([maps:get(duration, Data) || Data <- UptimeData]),
            DowntimeDuration = lists:sum([maps:get(downtime, Data) || Data <- UptimeData]),
            (TotalDuration - DowntimeDuration) / TotalDuration * 100
    end.

calculate_percentile(Data, Percentile) ->
    %% Calculate percentile value
    Sorted = lists:sort(Data),
    Position = ceil(length(Data) * (Percentile / 100)),
    lists:nth(min(Position, length(Sorted)), Sorted).

calculate_error_rate(ErrorRateData) ->
    %% Calculate error rate
    case ErrorRateData of
        [] -> 0;
        _ ->
            TotalRequests = lists:sum([maps:get(total, Data) || Data <- ErrorRateData]),
            ErrorRequests = lists:sum([maps:get(errors, Data) || Data <- ErrorRateData]),
            ErrorRequests / TotalRequests * 100
    end.

calculate_average(Data) ->
    %% Calculate average value
    case Data of
        [] -> 0;
        _ ->
            lists:sum(Data) / length(Data)
    end.

calculate_session_success_rate(SessionData) ->
    %% Calculate session success rate
    case SessionData of
        [] -> 0;
        _ ->
            TotalSessions = lists:sum([maps:get(total, Data) || Data <- SessionData]),
            SuccessfulSessions = lists:sum([maps:get(successful, Data) || Data <- SessionData]),
            SuccessfulSessions / TotalSessions * 100
    end.

generate_detailed_report(SlaName) ->
    %% Generate detailed report for specific SLA
    case ets:lookup(erlmcp_sla_metrics, SlaName) of
        [{_, Metrics}] ->
            %% Generate detailed metrics
            DetailedMetrics = generate_detailed_metrics(Metrics),
            DetailedMetrics;
        [] ->
            not_found
    end.

generate_detailed_metrics(Metrics) ->
    %% Generate detailed metrics breakdown
    #{
        time_series => maps:get(time_series, Metrics),
        statistics => calculate_statistics(maps:get(time_series, Metrics)),
        trends => analyze_trends(maps:get(time_series, Metrics))
    }.

calculate_statistics(TimeSeries) ->
    %% Calculate statistics from time series
    Values = [maps:get(value, Item) || Item <- TimeSeries],
    #{
        min => lists:min(Values),
        max => lists:max(Values),
        mean => calculate_average(Values),
        median => calculate_median(Values),
        standard_deviation => calculate_standard_deviation(Values)
    }.

calculate_median(Values) ->
    Sorted = lists:sort(Values),
    Length = length(Sorted),
    case Length of
        0 -> 0;
        _ ->
            Pos = (Length + 1) div 2,
            lists:nth(Pos, Sorted)
    end.

calculate_standard_deviation(Values) ->
    Mean = calculate_average(Values),
    Variance = calculate_variance(Values, Mean),
    math:sqrt(Variance).

calculate_variance(Values, Mean) ->
    case Values of
        [] -> 0;
        _ ->
            SquaredDiffs = lists:map(fun(X) -> math:pow(X - Mean, 2) end, Values),
            calculate_average(SquaredDiffs)
    end.

analyze_trends(TimeSeries) ->
    %% Analyze trends in time series
    %% This would perform trend analysis and pattern recognition
    #{
        trend => "stable",
        seasonality => "none",
        anomalies => []
    }.