%%%-------------------------------------------------------------------
%%% @doc TCPS Metrics Dashboard - Real-time visualization and monitoring
%%%
%%% Provides comprehensive dashboard for TCPS metrics including:
%%% - Real-time WIP tracking across Kanban buckets
%%% - Quality gates monitoring (test coverage, defect rates)
%%% - Andon alert system with severity tracking
%%% - Kaizen improvement metrics
%%% - Production flow visualization
%%% - SKU lifecycle tracking
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_dashboard).

-behaviour(gen_server).

%% API
-export([start_link/1, start_dashboard/1, stop_dashboard/0]).
-export([get_metrics_summary/0, get_real_time_metrics/0]).
-export([export_dashboard_data/1, generate_weekly_report/0]).
-export([notify_event/2, subscribe_events/1, unsubscribe_events/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Internal exports
-export([get_config/1]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8080).
-define(DEFAULT_REFRESH_INTERVAL, 5000).
-define(DEFAULT_HISTORY_DAYS, 30).

-type dashboard_config() :: #{
    port => pos_integer(),
    refresh_interval => pos_integer(),
    history_days => pos_integer(),
    enable_auth => boolean()
}.

-type metric_summary() :: #{
    wip => map(),
    andons => list(),
    recent_skus => list(),
    lead_time_trend => list(),
    quality_gates => map(),
    kaizen_metrics => map(),
    production_flow => map()
}.

-type event_type() :: andon_triggered | work_order_completed | sku_published
                    | quality_gate_failed | kaizen_improvement | wip_limit_exceeded.

-record(state, {
    config :: dashboard_config(),
    http_listener :: pid() | undefined,
    subscribers :: list(pid()),
    metrics_cache :: map(),
    last_update :: erlang:timestamp()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the dashboard server with configuration
-spec start_link(Config :: dashboard_config()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

%% @doc Start Cowboy HTTP server for dashboard
-spec start_dashboard(Port :: pos_integer()) -> {ok, pid()} | {error, term()}.
start_dashboard(Port) ->
    Config = #{port => Port,
               refresh_interval => ?DEFAULT_REFRESH_INTERVAL,
               history_days => ?DEFAULT_HISTORY_DAYS,
               enable_auth => false},
    start_link(Config).

%% @doc Stop the dashboard server
-spec stop_dashboard() -> ok.
stop_dashboard() ->
    gen_server:stop(?SERVER).

%% @doc Get current metrics summary across all categories
-spec get_metrics_summary() -> metric_summary().
get_metrics_summary() ->
    gen_server:call(?SERVER, get_metrics_summary).

%% @doc Get real-time metrics stream (returns SSE subscription)
-spec get_real_time_metrics() -> {ok, reference()} | {error, term()}.
get_real_time_metrics() ->
    gen_server:call(?SERVER, get_real_time_metrics).

%% @doc Export dashboard data in specified format
-spec export_dashboard_data(Format :: json | csv | pdf) -> binary() | {error, term()}.
export_dashboard_data(Format) ->
    gen_server:call(?SERVER, {export_data, Format}).

%% @doc Generate weekly HTML report
-spec generate_weekly_report() -> binary() | {error, term()}.
generate_weekly_report() ->
    gen_server:call(?SERVER, generate_weekly_report).

%% @doc Notify dashboard of an event (triggers SSE broadcast)
-spec notify_event(Type :: event_type(), Data :: map()) -> ok.
notify_event(Type, Data) ->
    gen_server:cast(?SERVER, {notify_event, Type, Data}).

%% @doc Subscribe to dashboard events
-spec subscribe_events(Pid :: pid()) -> ok.
subscribe_events(Pid) ->
    gen_server:cast(?SERVER, {subscribe, Pid}).

%% @doc Unsubscribe from dashboard events
-spec unsubscribe_events(Pid :: pid()) -> ok.
unsubscribe_events(Pid) ->
    gen_server:cast(?SERVER, {unsubscribe, Pid}).

%% @doc Get configuration value
-spec get_config(Key :: atom()) -> term() | undefined.
get_config(Key) ->
    gen_server:call(?SERVER, {get_config, Key}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Config]) ->
    ?LOG_INFO("Starting TCPS Dashboard with config: ~p", [Config]),

    %% Start dependent services
    start_dependent_services(),

    %% Start HTTP listener
    Port = maps:get(port, Config, ?DEFAULT_PORT),
    {ok, Listener} = start_http_listener(Port),

    %% Initialize metrics cache (uses aggregator now)
    MetricsCache = initialize_metrics_cache(),

    %% Schedule periodic metrics update
    RefreshInterval = maps:get(refresh_interval, Config, ?DEFAULT_REFRESH_INTERVAL),
    erlang:send_after(RefreshInterval, self(), refresh_metrics),

    State = #state{
        config = Config,
        http_listener = Listener,
        subscribers = [],
        metrics_cache = MetricsCache,
        last_update = erlang:timestamp()
    },

    {ok, State}.

handle_call(get_metrics_summary, _From, State) ->
    Summary = build_metrics_summary(State#state.metrics_cache),
    {reply, Summary, State};

handle_call(get_real_time_metrics, From, State) ->
    %% Add caller to subscribers
    {CallerPid, _Ref} = From,
    NewSubscribers = [CallerPid | State#state.subscribers],
    Ref = make_ref(),
    {reply, {ok, Ref}, State#state{subscribers = NewSubscribers}};

handle_call({export_data, Format}, _From, State) ->
    Result = export_metrics(Format, State#state.metrics_cache),
    {reply, Result, State};

handle_call(generate_weekly_report, _From, State) ->
    Report = generate_html_report(State#state.metrics_cache, State#state.config),
    {reply, Report, State};

handle_call({get_config, Key}, _From, State) ->
    Value = maps:get(Key, State#state.config, undefined),
    {reply, Value, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({notify_event, Type, Data}, State) ->
    %% Broadcast event to all subscribers
    Event = #{type => Type, data => Data, timestamp => erlang:timestamp()},
    broadcast_to_subscribers(Event, State#state.subscribers),
    {noreply, State};

handle_cast({subscribe, Pid}, State) ->
    monitor(process, Pid),
    NewSubscribers = [Pid | State#state.subscribers],
    {noreply, State#state{subscribers = NewSubscribers}};

handle_cast({unsubscribe, Pid}, State) ->
    NewSubscribers = lists:delete(Pid, State#state.subscribers),
    {noreply, State#state{subscribers = NewSubscribers}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(refresh_metrics, State) ->
    %% Refresh metrics cache
    NewCache = refresh_metrics_cache(State#state.metrics_cache),

    %% Broadcast updated metrics to subscribers
    Summary = build_metrics_summary(NewCache),
    Event = #{type => metrics_updated, data => Summary, timestamp => erlang:timestamp()},
    broadcast_to_subscribers(Event, State#state.subscribers),

    %% Schedule next refresh
    RefreshInterval = maps:get(refresh_interval, State#state.config, ?DEFAULT_REFRESH_INTERVAL),
    erlang:send_after(RefreshInterval, self(), refresh_metrics),

    {noreply, State#state{metrics_cache = NewCache, last_update = erlang:timestamp()}};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Remove dead subscriber
    NewSubscribers = lists:delete(Pid, State#state.subscribers),
    {noreply, State#state{subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Stop HTTP listener
    stop_http_listener(State#state.http_listener),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Start Cowboy HTTP listener
start_http_listener(Port) ->
    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, erlmcp, "dashboard/index.html"}},
            {"/dashboard", cowboy_static, {priv_file, erlmcp, "dashboard/index.html"}},
            {"/css/[...]", cowboy_static, {priv_dir, erlmcp, "dashboard/css"}},
            {"/js/[...]", cowboy_static, {priv_dir, erlmcp, "dashboard/js"}},
            {"/assets/[...]", cowboy_static, {priv_dir, erlmcp, "dashboard/assets"}},
            {"/api/metrics/summary", tcps_dashboard_handler, #{endpoint => summary}},
            {"/api/metrics/overview", tcps_dashboard_handler, #{endpoint => overview}},
            {"/api/metrics/kanban", tcps_dashboard_handler, #{endpoint => kanban}},
            {"/api/metrics/quality", tcps_dashboard_handler, #{endpoint => quality}},
            {"/api/metrics/andon", tcps_dashboard_handler, #{endpoint => andon}},
            {"/api/metrics/kaizen", tcps_dashboard_handler, #{endpoint => kaizen}},
            {"/api/metrics/flow", tcps_dashboard_handler, #{endpoint => flow}},
            {"/api/metrics/all", tcps_dashboard_handler, #{endpoint => all_metrics}},
            {"/api/work-orders", tcps_dashboard_handler, #{endpoint => work_orders}},
            {"/api/work-orders/:id", tcps_dashboard_handler, #{endpoint => work_order_detail}},
            {"/api/andon/:id", tcps_dashboard_handler, #{endpoint => andon_detail}},
            {"/api/andon/:id/resolve", tcps_dashboard_handler, #{endpoint => andon_resolve}},
            {"/api/sku/:id/receipts", tcps_dashboard_handler, #{endpoint => sku_receipts}},
            {"/api/health", tcps_dashboard_handler, #{endpoint => health}},
            {"/api/stream", tcps_dashboard_sse_handler, #{}},
            {"/ws", tcps_websocket_handler, #{}},
            {"/api/export/:format", tcps_dashboard_handler, #{endpoint => export}}
        ]}
    ]),

    %% Start Cowboy listener
    {ok, _} = cowboy:start_clear(tcps_dashboard_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),

    ?LOG_INFO("TCPS Dashboard HTTP server started on port ~p", [Port]),
    {ok, self()}.

%% @private Stop HTTP listener
stop_http_listener(_Listener) ->
    cowboy:stop_listener(tcps_dashboard_http_listener).

%% @private Initialize metrics cache (now uses aggregator)
initialize_metrics_cache() ->
    case whereis(tcps_metrics_aggregator) of
        undefined ->
            %% Fallback to mock data if aggregator not available
            #{
                wip => get_wip_metrics(),
                andons => get_andon_metrics(),
                recent_skus => get_recent_skus(),
                lead_time_trend => get_lead_time_trend(),
                quality_gates => get_quality_gates(),
                kaizen_metrics => get_kaizen_metrics(),
                production_flow => get_production_flow()
            };
        _Pid ->
            %% Use real aggregator
            tcps_metrics_aggregator:get_all_metrics()
    end.

%% @private Refresh metrics cache (now uses aggregator)
refresh_metrics_cache(_OldCache) ->
    initialize_metrics_cache().

%% @private Start dependent services
start_dependent_services() ->
    %% Start metrics cache
    tcps_metrics_cache:init_cache(),

    %% Start SSE manager if not already running
    case whereis(tcps_sse_manager) of
        undefined ->
            case tcps_sse_manager:start_link() of
                {ok, _SsePid} ->
                    ?LOG_INFO("Started TCPS SSE Manager"),
                    ok;
                {error, {already_started, _SsePid}} ->
                    ok;
                {error, SseReason} ->
                    ?LOG_ERROR("Failed to start SSE Manager: ~p", [SseReason]),
                    ok
            end;
        _SsePid ->
            ok
    end,

    %% Start metrics aggregator if not already running
    case whereis(tcps_metrics_aggregator) of
        undefined ->
            case tcps_metrics_aggregator:start_link() of
                {ok, _MetricsPid} ->
                    ?LOG_INFO("Started TCPS Metrics Aggregator"),
                    ok;
                {error, {already_started, _MetricsPid}} ->
                    ok;
                {error, MetricsReason} ->
                    ?LOG_ERROR("Failed to start Metrics Aggregator: ~p", [MetricsReason]),
                    ok
            end;
        _MetricsPid ->
            ok
    end,

    ok.

%% @private Build metrics summary from cache
build_metrics_summary(Cache) ->
    Cache.

%% @private Get WIP metrics by bucket
get_wip_metrics() ->
    Buckets = [backlog, ready, in_progress, review, done],
    lists:foldl(fun(Bucket, Acc) ->
        Count = case tcps_kanban:get_work_items_by_bucket(Bucket) of
            {ok, Items} -> length(Items);
            _ -> 0
        end,
        Limit = tcps_kanban:get_wip_limit(Bucket),
        Acc#{Bucket => #{count => Count, limit => Limit}}
    end, #{}, Buckets).

%% @private Get active Andon alerts
get_andon_metrics() ->
    %% Mock implementation - would integrate with actual Andon system
    [
        #{
            id => <<"andon-001">>,
            severity => critical,
            title => <<"Test failure in authentication module">>,
            affected_skus => [<<"SKU-2024-001">>],
            triggered_at => {{2024, 1, 26}, {10, 30, 0}},
            elapsed_seconds => 3600
        },
        #{
            id => <<"andon-002">>,
            severity => warning,
            title => <<"WIP limit exceeded in review bucket">>,
            affected_skus => [],
            triggered_at => {{2024, 1, 26}, {12, 0, 0}},
            elapsed_seconds => 1800
        }
    ].

%% @private Get recent SKUs published
get_recent_skus() ->
    %% Mock implementation - would query actual SKU database
    [
        #{
            sku_id => <<"SKU-2024-005">>,
            published_at => {{2024, 1, 26}, {14, 30, 0}},
            lead_time_hours => 48,
            quality_score => 0.95
        },
        #{
            sku_id => <<"SKU-2024-004">>,
            published_at => {{2024, 1, 26}, {10, 15, 0}},
            lead_time_hours => 36,
            quality_score => 0.98
        }
    ].

%% @private Get lead time trend data
get_lead_time_trend() ->
    %% Generate last 30 days of lead time data
    lists:map(fun(Day) ->
        #{
            date => adjust_date(erlang:date(), -Day),
            average_hours => 40 + rand:uniform(20),
            count => rand:uniform(5)
        }
    end, lists:seq(0, 29)).

%% @private Get quality gate metrics
get_quality_gates() ->
    #{
        test_pass_rate => #{value => 0.92, target => 0.80, status => pass},
        code_coverage => #{value => 0.85, target => 0.80, status => pass},
        defect_rate => #{value => 0.005, target => 0.01, status => pass},
        first_pass_yield => #{value => 0.96, target => 0.95, status => pass}
    }.

%% @private Get Kaizen improvement metrics
get_kaizen_metrics() ->
    #{
        week_over_week => #{
            lead_time_reduction => 0.12,
            defect_reduction => 0.08,
            throughput_increase => 0.15
        },
        top_waste_points => [
            #{category => waiting, percentage => 0.35},
            #{category => rework, percentage => 0.25},
            #{category => overprocessing, percentage => 0.20}
        ],
        improvement_proposals => [
            #{
                id => <<"kaizen-001">>,
                title => <<"Automate code review checks">>,
                estimated_roi => 0.30,
                status => in_progress
            }
        ],
        trend_lines => generate_trend_data()
    }.

%% @private Get production flow metrics
get_production_flow() ->
    #{
        active_skus => [
            #{sku_id => <<"SKU-2024-006">>, current_stage => in_progress, progress => 0.60},
            #{sku_id => <<"SKU-2024-007">>, current_stage => review, progress => 0.85}
        ],
        throughput_rate => #{
            skus_per_day => 3.5,
            trend => increasing
        },
        cycle_time_distribution => [
            #{bucket => <<"0-24h">>, count => 5},
            #{bucket => <<"24-48h">>, count => 12},
            #{bucket => <<"48-72h">>, count => 8},
            #{bucket => <<"72h+">>, count => 3}
        ]
    }.

%% @private Generate trend data for charts
generate_trend_data() ->
    Metrics = [lead_time, defect_rate, throughput],
    lists:map(fun(Metric) ->
        Data = lists:map(fun(Week) ->
            #{week => Week, value => rand:uniform() * 100}
        end, lists:seq(1, 12)),
        #{metric => Metric, data => Data}
    end, Metrics).

%% @private Broadcast event to all subscribers
broadcast_to_subscribers(Event, Subscribers) ->
    Message = {dashboard_event, Event},
    lists:foreach(fun(Pid) ->
        Pid ! Message
    end, Subscribers).

%% @private Export metrics in specified format
export_metrics(json, Cache) ->
    jsx:encode(Cache);

export_metrics(csv, Cache) ->
    %% Simple CSV export of WIP metrics
    WIP = maps:get(wip, Cache, #{}),
    Header = <<"Bucket,Count,Limit\n">>,
    Rows = maps:fold(fun(Bucket, #{count := Count, limit := Limit}, Acc) ->
        Row = io_lib:format("~s,~p,~p~n", [Bucket, Count, Limit]),
        [iolist_to_binary(Row) | Acc]
    end, [], WIP),
    iolist_to_binary([Header | lists:reverse(Rows)]);

export_metrics(pdf, _Cache) ->
    %% PDF export would require a PDF library (not implemented in this version)
    {error, pdf_export_not_implemented}.

%% @private Generate weekly HTML report
generate_html_report(Cache, Config) ->
    HistoryDays = maps:get(history_days, Config, ?DEFAULT_HISTORY_DAYS),

    WIP = maps:get(wip, Cache, #{}),
    QualityGates = maps:get(quality_gates, Cache, #{}),
    Andons = maps:get(andons, Cache, []),

    HTML = [
        <<"<!DOCTYPE html><html><head>">>,
        <<"<title>TCPS Weekly Report</title>">>,
        <<"<style>">>,
        <<"body { font-family: Arial, sans-serif; margin: 20px; }">>,
        <<"h1 { color: #333; }">>,
        <<"table { border-collapse: collapse; width: 100%; margin: 20px 0; }">>,
        <<"th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }">>,
        <<"th { background-color: #4CAF50; color: white; }">>,
        <<".pass { color: green; }">>,
        <<".fail { color: red; }">>,
        <<"</style></head><body>">>,
        <<"<h1>TCPS Weekly Report</h1>">>,
        io_lib:format("<p>Generated: ~s</p>", [format_timestamp(erlang:timestamp())]),
        io_lib:format("<p>History Period: ~p days</p>", [HistoryDays]),

        <<"<h2>Work In Progress</h2><table><tr><th>Bucket</th><th>Count</th><th>Limit</th></tr>">>,
        [io_lib:format("<tr><td>~s</td><td>~p</td><td>~p</td></tr>",
                       [Bucket, maps:get(count, Data), maps:get(limit, Data)])
         || {Bucket, Data} <- maps:to_list(WIP)],
        <<"</table>">>,

        <<"<h2>Quality Gates</h2><table><tr><th>Metric</th><th>Value</th><th>Target</th><th>Status</th></tr>">>,
        [io_lib:format("<tr><td>~s</td><td>~.2f</td><td>~.2f</td><td class='~s'>~s</td></tr>",
                       [Gate, maps:get(value, Data), maps:get(target, Data),
                        maps:get(status, Data), maps:get(status, Data)])
         || {Gate, Data} <- maps:to_list(QualityGates)],
        <<"</table>">>,

        <<"<h2>Active Andons</h2><table><tr><th>ID</th><th>Severity</th><th>Title</th><th>Elapsed</th></tr>">>,
        [io_lib:format("<tr><td>~s</td><td>~s</td><td>~s</td><td>~p seconds</td></tr>",
                       [maps:get(id, Andon), maps:get(severity, Andon),
                        maps:get(title, Andon), maps:get(elapsed_seconds, Andon)])
         || Andon <- Andons],
        <<"</table>">>,

        <<"</body></html>">>
    ],

    iolist_to_binary(HTML).

%% @private Helper functions
adjust_date({Y, M, D}, Days) ->
    %% Simple date adjustment (doesn't handle month/year boundaries)
    NewD = D + Days,
    {Y, M, max(1, NewD)}.

format_timestamp({MegaSecs, Secs, _MicroSecs}) ->
    DateTime = calendar:now_to_datetime({MegaSecs, Secs, 0}),
    {{Y, M, D}, {H, Min, S}} = DateTime,
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w", [Y, M, D, H, Min, S]).
