-module(erlmcp_api_analytics).

-behaviour(gen_server).

%% API exports
-export([start_link/0, record_event/1, get_metrics/1, get_aggregates/1, get_trends/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp_api_gateway.hrl").

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

record_event(Event) ->
    gen_server:cast(?MODULE, {record_event, Event}).

get_metrics(TimeRange) ->
    gen_server:call(?MODULE, {get_metrics, TimeRange}).

get_aggregates(TimeRange) ->
    gen_server:call(?MODULE, {get_aggregates, TimeRange}).

get_trends(Metric, TimeRange) ->
    gen_server:call(?MODULE, {get_trends, Metric, TimeRange}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize metrics ETS table
    ets:new(api_metrics, [
        named_table,
        public,
        set,
        {keypos, 2},
        {write_concurrency, true}
    ]),

    %% Initialize aggregates ETS table
    ets:new(api_aggregates, [
        named_table,
        public,
        set,
        {keypos, 2},
        {write_concurrency, true}
    ]),

    %% Load configuration
    Config = load_analytics_config(),

    %% Start aggregation process
    erlang:send_after(60000, self(), aggregate_metrics),

    State = #{
        config => Config,
        metrics => ets:tab2list(api_metrics),
        aggregates => ets:tab2list(api_aggregates)
    },

    {ok, State}.

handle_call({get_metrics, TimeRange}, _From, State) ->
    %% Get metrics for time range
    Now = erlang:system_time(millisecond),
    StartTime = case TimeRange of
        {hours, N} ->
            Now - (N * 3600000);
        {days, N} ->
            Now - (N * 86400000);
        {minutes, N} ->
            Now - (N * 60000)
    end,

    %% Query metrics
    Metrics = ets:foldl(fun({Key, Timestamp, Metric}, Acc) ->
        if
            Timestamp >= StartTime ->
                [{Key, Metric} | Acc];
            true ->
                Acc
        end
    end, [], api_metrics),

    {reply, {ok, Metrics}, State};

handle_call({get_aggregates, TimeRange}, _From, State) ->
    %% Get aggregates for time range
    Now = erlang:system_time(millisecond),
    StartTime = case TimeRange of
        {hours, N} ->
            Now - (N * 3600000);
        {days, N} ->
            Now - (N * 86400000);
        {minutes, N} ->
            Now - (N * 60000)
    end,

    %% Query aggregates
    Aggregates = ets:foldl(fun({Key, Timestamp, Aggregate}, Acc) ->
        if
            Timestamp >= StartTime ->
                [{Key, Aggregate} | Acc];
            true ->
                Acc
        end
    end, [], api_aggregates),

    {reply, {ok, Aggregates}, State};

handle_call({get_trends, Metric, TimeRange}, _From, State) ->
    %% Get trends for specific metric
    Now = erlang:system_time(millisecond),
    StartTime = case TimeRange of
        {hours, N} ->
            Now - (N * 3600000);
        {days, N} ->
            Now - (N * 86400000);
        {minutes, N} ->
            Now - (N * 60000)
    end,

    %% Query metric history
    History = ets:foldl(fun({Key, Timestamp, Value}, Acc) ->
        case Key of
            {Metric, T} when T >= StartTime ->
                [{Timestamp, Value} | Acc];
            _ ->
                Acc
        end
    end, [], api_metrics),

    %% Sort by timestamp
    SortedHistory = lists:keysort(1, History),

    {reply, {ok, SortedHistory}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({record_event, Event}, State) ->
    %% Record analytics event
    Now = erlang:system_time(millisecond),
    Key = {Event#analytics_event.event_type, Now},
    true = ets:insert(api_metrics, {Key, Now, Event}),

    %% Update aggregates
    update_aggregates(Event, Now),

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(aggregate_metrics, State) ->
    %% Aggregate metrics
    Now = erlang:system_time(millisecond),
    Window = 60000, %% 1 minute

    %% Get all metrics
    AllMetrics = ets:tab2list(api_metrics),

    %% Aggregate by type and time window
    Aggregates = aggregate_by_window(AllMetrics, Now, Window),

    %% Store aggregates
    lists:foreach(fun({Key, Aggregate}) ->
        true = ets:insert(api_aggregates, {Key, Now, Aggregate})
    end, Aggregates),

    %% Schedule next aggregation
    erlang:send_after(60000, self(), aggregate_metrics),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

load_analytics_config() ->
    %% Load analytics configuration
    DefaultConfig = #{
        retention => 86400000, %% 24 hours
        aggregation_window => 60000, %% 1 minute
        metrics => [
            request_count,
            error_count,
            response_time,
            throughput,
            error_rate,
            consumer_activity
        ]
    },

    DefaultConfig.

record_analytics_event(Event) ->
    %% Record analytics event
    Now = erlang:system_time(millisecond),
    Key = {Event#analytics_event.event_type, Now},
    true = ets:insert(api_metrics, {Key, Now, Event}).

update_aggregates(Event, Now) ->
    %% Update aggregates based on event
    EventType = Event#analytics_event.event_type,

    %% Update request count
    update_counter(request_count, 1),

    %% Update error count if error response
    case Event#analytics_event.status_code >= 400 of
        true ->
            update_counter(error_count, 1);
        false ->
            ok
    end,

    %% Update response time
    update_counter(response_time, Event#analytics_event.response_time),

    %% Update consumer activity
    update_counter(consumer_activity, Event#analytics_event.consumer_id, 1).

update_counter(Name, Value) ->
    %% Update counter aggregate
    Key = {Name, erlang:system_time(millisecond)},
    true = ets:insert(api_aggregates, {Key, Value}).

update_counter(Name, Id, Value) ->
    %% Update counter with ID
    Key = {Name, Id, erlang:system_time(millisecond)},
    true = ets:insert(api_aggregates, {Key, Value}).

aggregate_by_window(Metrics, Now, Window) ->
    %% Aggregate metrics by time window
    lists:foldl(fun({Key, Timestamp, Value}, Acc) ->
        WindowStart = Timestamp - (Timestamp rem Window),
        WindowKey = {Key, WindowStart},

        case lists:keyfind(WindowKey, 1, Acc) of
            {WindowKey, Current} ->
                lists:keyreplace(WindowKey, 1, Acc, {WindowKey, Current + Value});
            false ->
                [{WindowKey, Value} | Acc]
        end
    end, [], Metrics).