-module(erlmcp_api_gateway_monitor).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    record_request/3, record_error/3, get_metrics/1, get_health/0,
    record_rate/4, get_analytics/2
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{metrics => #{}, analytics => #{}, health => #{}}}.

record_request(ApiId, Method, ResponseTime) ->
    gen_server:cast(?MODULE, {record_request, ApiId, Method, ResponseTime}).

record_error(ApiId, Method, Error) ->
    gen_server:cast(?MODULE, {record_error, ApiId, Method, Error}).

record_rate(ApiId, ConsumerId, Window, Count) ->
    gen_server:cast(?MODULE, {record_rate, ApiId, ConsumerId, Window, Count}).

get_metrics(TimeWindow) ->
    gen_server:call(?MODULE, {get_metrics, TimeWindow}).

get_health() ->
    gen_server:call(?MODULE, get_health).

get_analytics(ApiId, Period) ->
    gen_server:call(?MODULE, {get_analytics, ApiId, Period}).

handle_cast({record_request, ApiId, Method, ResponseTime}, State) ->
    Now = erlang:system_time(millisecond),
    Key = request_key(ApiId, Method, Now),

    Metrics = State#{metrics},
    NewMetrics = maps:put(Key, #{
        timestamp => Now,
        response_time => ResponseTime,
        status => success
    }, Metrics),

    {noreply, State#{metrics => NewMetrics}};

handle_cast({record_error, ApiId, Method, Error}, State) ->
    Now = erlang:system_time(millisecond),
    Key = error_key(ApiId, Method, Now),

    Metrics = State#{metrics},
    NewMetrics = maps:put(Key, #{
        timestamp => Now,
        error => Error,
        status => error
    }, Metrics),

    {noreply, State#{metrics => NewMetrics}};

handle_cast({record_rate, ApiId, ConsumerId, Window, Count}, State) ->
    Now = erlang:system_time(millisecond),
    Key = rate_key(ApiId, ConsumerId, Window),

    Analytics = State#{analytics},
    NewAnalytics = maps:put(Key, #{
        timestamp => Now,
        count => Count,
        window => Window
    }, Analytics),

    {noreply, State#{analytics => NewAnalytics}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call({get_metrics, TimeWindow}, _From, State) ->
    Now = erlang:system_time(millisecond),
    StartTime = Now - TimeWindow,

    Metrics = filter_metrics_by_time(State#{metrics}, StartTime),

    Summary = calculate_metrics_summary(Metrics),
    {reply, {ok, Summary}, State};

handle_call(get_health, _From, State) ->
    Health = calculate_health(State),
    {reply, {ok, Health}, State};

handle_call({get_analytics, ApiId, Period}, _From, State) ->
    Now = erlang:system_time(millisecond),
    StartTime = Now - Period,

    Analytics = filter_analytics_by_time(State#{analytics}, ApiId, StartTime),

    Summary = calculate_analytics_summary(Analytics),
    {reply, {ok, Summary}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

request_key(ApiId, Method, Timestamp) ->
    list_to_binary([
        "request:",
        ApiId, ":", Method, ":",
        integer_to_binary(Timestamp div 60000)
    ]).

error_key(ApiId, Method, Timestamp) ->
    list_to_binary([
        "error:",
        ApiId, ":", Method, ":",
        integer_to_binary(Timestamp div 60000)
    ]).

rate_key(ApiId, ConsumerId, Window) ->
    list_to_binary([
        "rate:",
        ApiId, ":", ConsumerId, ":",
        Window
    ]).

filter_metrics_by_time(Metrics, StartTime) ->
    maps:filter(fun(_, Metric) ->
        maps:get(timestamp, Metric) >= StartTime
    end, Metrics).

filter_analytics_by_time(Analytics, ApiId, StartTime) ->
    maps:filter(fun(_, Data) ->
        maps:get(api_id, Data) =:= ApiId andalso
        maps:get(timestamp, Data) >= StartTime
    end, Analytics).

calculate_metrics_summary(Metrics) ->
    TotalRequests = maps:size(Metrics),

    SuccessMetrics = maps:filter(fun(_, Metric) ->
        maps:get(status, Metric) =:= success
    end, Metrics),

    SuccessCount = maps:size(SuccessMetrics),
    ErrorCount = TotalRequests - SuccessCount,

    ResponseTimes = [maps:get(response_time, Metric) || Metric <- maps:values(SuccessMetrics)],

    AvgResponseTime = case ResponseTimes of
        [] -> 0;
        _ -> lists:sum(ResponseTimes) / length(ResponseTimes)
    end,

    #{total_requests => TotalRequests,
      success_count => SuccessCount,
      error_count => ErrorCount,
      success_rate => SuccessCount / TotalRequests,
      avg_response_time => AvgResponseTime}.

calculate_analytics_summary(Analytics) ->
    AnalyticsList = maps:values(Analytics),

    TotalRequests = lists:sum([maps:get(count, Data) || Data <- AnalyticsList]),

    RequestsByWindow = lists:foldl(fun(Data, Acc) ->
        Window = maps:get(window, Data),
        maps:update_with(Window, fun(C) -> C + maps:get(count, Data) end, maps:get(count, Data), Acc)
    end, #{}, AnalyticsList),

    #{total_requests => TotalRequests,
      requests_by_window => RequestsByWindow}.

calculate_health(State) ->
    Metrics = maps:get(metrics, State),
    Analytics = maps:get(analytics, State),

    Now = erlang:system_time(millisecond),
    RecentMetrics = filter_metrics_by_time(Metrics, Now - 60000),

    TotalRequests = maps:size(RecentMetrics),
    ErrorCount = maps:size(maps:filter(fun(_, Metric) ->
        maps:get(status, Metric) =:= error
    end, RecentMetrics)),

    HealthScore = case TotalRequests of
        0 -> 100;
        _ -> (TotalRequests - ErrorCount) / TotalRequests * 100
    end,

    #{status => case HealthScore >= 95 of
        true -> healthy;
        true when HealthScore >= 90 -> degraded;
        true when HealthScore >= 80 -> warning;
        _ -> critical
    end,
     score => HealthScore,
     total_requests => TotalRequests,
     error_rate => case TotalRequests of
         0 -> 0;
         _ -> ErrorCount / TotalRequests
     end}.

-spec erlmcp_api_gateway_monitor:record_request(binary(), binary(), integer()) -> any().
-spec erlmcp_api_gateway_monitor:record_error(binary(), binary(), binary()) -> any().
-spec erlmcp_api_gateway_monitor:record_rate(binary(), binary(), binary(), integer()) -> any().
-spec erlmcp_api_gateway_monitor:get_metrics(integer()) -> {ok, map()} | {error, term()}.
-spec erlmcp_api_gateway_monitor:get_health() -> {ok, map()} | {error, term()}.
-spec erlmcp_api_gateway_monitor:get_analytics(binary(), integer()) -> {ok, map()} | {error, term()}.