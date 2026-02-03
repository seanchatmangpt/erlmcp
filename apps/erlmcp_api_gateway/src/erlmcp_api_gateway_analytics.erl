-module(erlmcp_api_gateway_analytics).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    record_event/2, get_usage_stats/2, generate_report/1,
    get_api_metrics/1, get_consumer_metrics/1, set_alerts/1
]).

-record.event, {
    timestamp :: integer(),
    api_id :: binary(),
    consumer_id :: binary(),
    method :: binary(),
    status :: integer(),
    response_time :: integer(),
    bytes_in :: integer(),
    bytes_out :: integer()
}.

-record.usage_stats, {
    period :: binary(),
    total_requests :: integer(),
    unique_consumers :: integer(),
    avg_response_time :: integer(),
    p95_response_time :: integer(),
    error_rate :: float(),
    top_apis :: list(),
    peak_hour :: integer()
}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{events => [], alerts => #{}, stats => #{}}}.

record_event(EventType, EventData) ->
    gen_server:cast(?MODULE, {record_event, EventType, EventData}).

get_usage_stats(ApiId, Period) ->
    gen_server:call(?MODULE, {get_usage_stats, ApiId, Period}).

generate_report(ReportType) ->
    gen_server:call(?MODULE, {generate_report, ReportType}).

get_api_metrics(ApiId) ->
    gen_server:call(?MODULE, {get_api_metrics, ApiId}).

get_consumer_metrics(ConsumerId) ->
    gen_server:call(?MODULE, {get_consumer_metrics, ConsumerId}).

set_alerts(AlertSpec) ->
    gen_server:call(?MODULE, {set_alerts, AlertSpec}).

handle_call({get_usage_stats, ApiId, Period}, _From, State) ->
    Stats = calculate_usage_stats(ApiId, Period, State),
    {reply, {ok, Stats}, State};

handle_call({generate_report, ReportType}, _From, State) ->
    Report = generate_analytics_report(ReportType, State),
    {reply, {ok, Report}, State};

handle_call({get_api_metrics, ApiId}, _From, State) ->
    Metrics = calculate_api_metrics(ApiId, State),
    {reply, {ok, Metrics}, State};

handle_call({get_consumer_metrics, ConsumerId}, _From, State) ->
    Metrics = calculate_consumer_metrics(ConsumerId, State),
    {reply, {ok, Metrics}, State};

handle_call({set_alerts, AlertSpec}, _From, State) ->
    Alerts = State#{alerts},
    NewAlerts = maps:put(AlertSpec#{id}, AlertSpec, Alerts),
    {reply, {ok, alerts_updated}, State#{alerts => NewAlerts}}.

handle_cast({record_event, api_call, EventData}, State) ->
    Event = #event{
        timestamp = erlang:system_time(millisecond),
        api_id = maps:get(api_id, EventData),
        consumer_id = maps:get(consumer_id, EventData),
        method = maps:get(method, EventData),
        status = maps:get(status, EventData),
        response_time = maps:get(response_time, EventData),
        bytes_in = maps:get(bytes_in, EventData, 0),
        bytes_out = maps:get(bytes_out, EventData, 0)
    },

    Events = State#{events},
    CleanEvents = cleanup_old_events(Events, 86400000),
    NewEvents = [Event | CleanEvents],

    check_alerts(Event, State#{alerts}),

    {noreply, State#{events => NewEvents}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

calculate_usage_stats(ApiId, Period, State) ->
    Events = filter_events(ApiId, Period, State#{events}),

    TotalRequests = length(Events),
    UniqueConsumers = length(lists:usort([E#event.consumer_id || E <- Events])),

    ResponseTimes = [E#event.response_time || E <- Events],
    AvgResponseTime = calculate_average(ResponseTimes),
    P95ResponseTime = calculate_percentile(ResponseTimes, 95),

    ErrorEvents = [E || E <- Events, E#event.status >= 400],
    ErrorRate = length(ErrorEvents) / TotalRequests * 100,

    ApiCounts = lists:foldl(fun(E, Acc) ->
        maps:update_with(E#event.api_id, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Events),
    TopApis = lists:sublist(lists:keysort(2, maps:to_list(ApiCounts)), 5),

    HourCounts = lists:foldl(fun(E, Acc) ->
        Hour = hour_from_timestamp(E#event.timestamp),
        maps:update_with(Hour, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Events),
    PeakHour = case lists:keysort(2, maps:to_list(HourCounts)) of
        [] -> 0;
        [{Hour, _}|_] -> Hour
    end,

    #usage_stats{
        period = Period,
        total_requests = TotalRequests,
        unique_consumers = UniqueConsumers,
        avg_response_time = AvgResponseTime,
        p95_response_time = P95ResponseTime,
        error_rate = ErrorRate,
        top_apis = TopApis,
        peak_hour = PeakHour
    }.

calculate_api_metrics(ApiId, State) ->
    Events = filter_events(ApiId, <<"all">>, State#{events}),

    TotalRequests = length(Events),

    Methods = lists:foldl(fun(E, Acc) ->
        maps:update_with(E#event.method, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Events),

    StatusCodes = lists:foldl(fun(E, Acc) ->
        maps:update_with(integer_to_binary(E#event.status), fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Events),

    ResponseTimes = [E#event.response_time || E <- Events],
    AvgResponseTime = calculate_average(ResponseTimes),
    MaxResponseTime = lists:max(ResponseTimes),

    TotalBytesIn = lists:sum([E#event.bytes_in || E <- Events]),
    TotalBytesOut = lists:sum([E#event.bytes_out || E <- Events]),

    #{
        total_requests => TotalRequests,
        method_distribution => Methods,
        status_distribution => StatusCodes,
        avg_response_time => AvgResponseTime,
        max_response_time => MaxResponseTime,
        total_bytes_in => TotalBytesIn,
        total_bytes_out => TotalBytesOut,
        unique_consumers => length(lists:usort([E#event.consumer_id || E <- Events]))
    }.

calculate_consumer_metrics(ConsumerId, State) ->
    Events = filter_consumer_events(ConsumerId, State#{events}),

    TotalRequests = length(Events),
    ApiUsage = lists:foldl(fun(E, Acc) ->
        maps:update_with(E#event.api_id, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Events),

    ResponseTimes = [E#event.response_time || E <- Events],
    AvgResponseTime = calculate_average(ResponseTimes),

    StatusCounts = lists:foldl(fun(E, Acc) ->
        maps:update_with(integer_to_binary(E#event.status), fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Events),

    ErrorCount = maps:get(<<"4xx", StatusCounts), 0) + maps:get(<<"5xx", StatusCounts), 0),
    ErrorRate = ErrorCount / TotalRequests * 100,

    #{
        total_requests => TotalRequests,
        api_usage => ApiUsage,
        avg_response_time => AvgResponseTime,
        error_rate => ErrorRate,
        status_distribution => StatusCounts
    }.

generate_analytics_report(ReportType, State) ->
    case ReportType of
        daily ->
            generate_daily_report(State);
        weekly ->
            generate_weekly_report(State);
        monthly ->
            generate_monthly_report(State);
        custom ->
            generate_custom_report(State)
    end.

generate_daily_report(State) ->
    Events = filter_events_by_time(State#{events}, 86400000),
    TotalRequests = length(Events),
    UniqueConsumers = length(lists:usort([E#event.consumer_id || E <- Events])),

    ResponseTimes = [E#event.response_time || E <- Events],
    AvgResponseTime = calculate_average(ResponseTimes),
    P95ResponseTime = calculate_percentile(ResponseTimes, 95),

    #{
        report_type => daily,
        total_requests => TotalRequests,
        unique_consumers => UniqueConsumers,
        avg_response_time => AvgResponseTime,
        p95_response_time => P95ResponseTime,
        generated_at => erlang:system_time(millisecond)
    }.

generate_weekly_report(State) ->
    Events = filter_events_by_time(State#{events}, 604800000),
    DailyStats = lists:map(fun(Day) ->
        DailyEvents = filter_events_by_day(Events, Day),
        length(DailyEvents)
    end, lists:seq(1, 7)),

    #{
        report_type => weekly,
        daily_requests => DailyStats,
        total_requests => lists:sum(DailyStats),
        generated_at => erlang:system_time(millisecond)
    }.

generate_monthly_report(State) ->
    Events = filter_events_by_time(State#{events}, 2592000000),

    DailyStats = lists:map(fun(Day) ->
        DailyEvents = filter_events_by_day(Events, Day),
        length(DailyEvents)
    end, lists:seq(1, 30)),

    WeeklyStats = lists:map(fun(Week) ->
        WeeklyEvents = lists:sublist(DailyStats, Week, 7),
        lists:sum(WeeklyEvents)
    end, lists:seq(1, 4, 7)),

    #{
        report_type => monthly,
        weekly_requests => WeeklyStats,
        total_requests => lists:sum(DailyStats),
        generated_at => erlang:system_time(millisecond)
    }.

generate_custom_report(State) ->
    Events = State#{events},

    APIStats = lists:map(fun({ApiId, Count}) ->
        ApiEvents = filter_events(ApiId, <<"all">>, Events),
        #{
            api_id => ApiId,
            requests => Count,
            avg_response_time => calculate_average([E#event.response_time || E <- ApiEvents])
        }
    end, lists:keysort(2, maps:to_list(lists:foldl(fun(E, Acc) ->
        maps:update_with(E#event.api_id, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Events)))),

    #{
        report_type => custom,
        top_apis => APIStats,
        total_events => length(Events),
        generated_at => erlang:system_time(millisecond)
    }.

check_alerts(Event, Alerts) ->
    maps:foreach(fun(_, Alert) ->
        check_alert_condition(Event, Alert)
    end, Alerts).

check_alert_condition(Event, Alert) ->
    #{metric := Metric, threshold := Threshold, operator := Op} = Alert,

    Value = case Metric of
        response_time -> Event#event.response_time;
        error_rate -> case Event#event.status >= 400 of true -> 100; false -> 0 end;
        requests_per_minute -> get_requests_per_minute(Event#event.api_id)
    end,

    Triggered = case Op of
        gt -> Value > Threshold;
        lt -> Value < Threshold;
        eq -> Value =:= Threshold;
        ne -> Value =/= Threshold
    end,

    if
        Triggered ->
            trigger_alert(Alert, Value);
        true ->
            ok
    end.

trigger_alert(Alert, Value) ->
    io:format("ALERT: ~p ~p ~p~n", [Alert#{value}, Value, erlang:system_time(millisecond)]).

filter_events(ApiId, Period, Events) ->
    Filtered = lists:filter(fun(E) ->
        E#event.api_id =:= ApiId
    end, Events),

    case Period of
        <<"1h">> -> filter_events_by_time(Filtered, 3600000);
        <<"24h">> -> filter_events_by_time(Filtered, 86400000);
        <<"7d">> -> filter_events_by_time(Filtered, 604800000);
        _ -> Filtered
    end.

filter_events_by_time(Events, TimeWindow) ->
    Now = erlang:system_time(millisecond),
    lists:filter(fun(E) ->
        E#event.timestamp >= Now - TimeWindow
    end, Events).

filter_events_by_day(Events, Day) ->
    StartOfDay = erlang:system_time(millisecond) - (Day * 86400000),
    EndOfDay = StartOfDay + 86400000,
    lists:filter(fun(E) ->
        E#event.timestamp >= StartOfDay andalso E#event.timestamp < EndOfDay
    end, Events).

filter_consumer_events(ConsumerId, Events) ->
    lists:filter(fun(E) ->
        E#event.consumer_id =:= ConsumerId
    end, Events).

cleanup_old_events(Events, MaxAge) ->
    Now = erlang:system_time(millisecond),
    lists:filter(fun(E) ->
        E#event.timestamp >= Now - MaxAge
    end, Events).

calculate_average(List) when length(List) =:= 0 -> 0;
calculate_average(List) ->
    lists:sum(List) / length(List).

calculate_percentile(List, Percentile) when length(List) =:= 0 -> 0;
calculate_percentile(List, Percentile) ->
    Sorted = lists:sort(List),
    Index = round((Percentile / 100) * length(Sorted)),
    lists:nth(Index, Sorted).

hour_from_timestamp(Timestamp) ->
    calendar:system_time_to_universal_time(Timestamp, millisecond).

get_requests_per_minute(ApiId) ->
    ThisMinute = erlang:system_time(millisecond) - (erlang:system_time(second) rem 60 * 1000),
    LastMinute = ThisMinute - 60000,

    {ok, Events} = get_events_in_minute_range(LastMinute, ThisMinute),

    length([E || E <- Events, E#event.api_id =:= ApiId]).

get_events_in_minute_range(Start, End) ->
    ok.