%%%-------------------------------------------------------------------
%%% @doc
%%% Comprehensive metrics collector for erlmcp v3
%%% Ensures all critical metrics are collected and exposed for Prometheus scraping
%%% Metrics: request rate, error rate, latency, memory usage, CPU usage, connection counts
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_metrics_collector).

-behaviour(gen_server).

%% API
-export([start_link/0,
         start_link/1,
         scrape_counter/2,
         scrape_gauge/2,
         scrape_histogram/2,
         record_request/2,
         record_response/2,
         record_error/1,
         record_latency/2,
         get_all_metrics/0,
         prometheus_export/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(METRICS_TABLE, erlmcp_prometheus_metrics).
-define(HISTOGRAM_BUCKETS, [0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]).

%% Metric types
-type metric_type() :: counter | gauge | histogram | summary.
-type metric_name() :: binary().
-type metric_labels() :: map().
-type metric_value() :: number().

%% Metric record
-record(metric, {
    name :: metric_name(),
    type :: metric_type(),
    value :: metric_value() | [metric_value()],
    labels :: metric_labels(),
    timestamp :: integer()
}).

-record(state, {
    scrape_interval :: pos_integer(),
    metrics_table :: ets:tid(),
    histogram_buckets :: [number()],
    last_scrape :: integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Start the metrics collector with default interval (5 seconds)
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(5000).

%%--------------------------------------------------------------------
%% @doc
%% Start the metrics collector with custom scrape interval
%% @end
%%--------------------------------------------------------------------
start_link(ScrapeInterval) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ScrapeInterval], []).

%%--------------------------------------------------------------------
%% @doc
%% Record a counter metric (monotonically increasing)
%% @end
%%--------------------------------------------------------------------
scrape_counter(Name, Value) ->
    scrape_counter(Name, Value, #{}).

scrape_counter(Name, Value, Labels) when is_binary(Name), is_number(Value) ->
    gen_server:cast(?SERVER, {counter, Name, Value, Labels}).

%%--------------------------------------------------------------------
%% @doc
%% Record a gauge metric (can go up or down)
%% @end
%%--------------------------------------------------------------------
scrape_gauge(Name, Value) ->
    scrape_gauge(Name, Value, #{}).

scrape_gauge(Name, Value, Labels) when is_binary(Name), is_number(Value) ->
    gen_server:cast(?SERVER, {gauge, Name, Value, Labels}).

%%--------------------------------------------------------------------
%% @doc
%% Record a histogram metric (distribution tracking)
%% @end
%%--------------------------------------------------------------------
scrape_histogram(Name, Value) ->
    scrape_histogram(Name, Value, #{}).

scrape_histogram(Name, Value, Labels) when is_binary(Name), is_number(Value) ->
    gen_server:cast(?SERVER, {histogram, Name, Value, Labels}).

%%--------------------------------------------------------------------
%% @doc
%% Record an incoming request (increments request counter)
%% @end
%%--------------------------------------------------------------------
record_request(Method, Endpoint) ->
    scrape_counter(<<"erlmcp_requests_total">>, 1, #{
        <<"method">> => to_binary(Method),
        <<"endpoint">> => to_binary(Endpoint)
    }).

%%--------------------------------------------------------------------
%% @doc
%% Record a response (tracks status codes)
%% @end
%%--------------------------------------------------------------------
record_response(StatusCode, Method) ->
    scrape_counter(<<"erlmcp_responses_total">>, 1, #{
        <<"status">> => to_binary(StatusCode),
        <<"method">> => to_binary(Method)
    }),
    case StatusCode of
        Code when Code >= 400 ->
            scrape_counter(<<"erlmcp_errors_total">>, 1, #{
                <<"error_type">> => <<"http_error">>,
                <<"status_code">> => to_binary(Code)
            });
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc
%% Record an error occurrence
%% @end
%%--------------------------------------------------------------------
record_error(ErrorType) ->
    scrape_counter(<<"erlmcp_errors_total">>, 1, #{
        <<"error_type">> => to_binary(ErrorType)
    }).

%%--------------------------------------------------------------------
%% @doc
%% Record request latency in histogram
%% @end
%%--------------------------------------------------------------------
record_latency(DurationSeconds, Labels) ->
    scrape_histogram(<<"erlmcp_request_duration_seconds">>, DurationSeconds, Labels).

%%--------------------------------------------------------------------
%% @doc
%% Get all current metrics as a map
%% @end
%%--------------------------------------------------------------------
get_all_metrics() ->
    gen_server:call(?SERVER, get_all_metrics).

%%--------------------------------------------------------------------
%% @doc
%% Export metrics in Prometheus text format
%% @end
%%--------------------------------------------------------------------
prometheus_export() ->
    gen_server:call(?SERVER, prometheus_export).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([ScrapeInterval]) ->
    %% Create ETS table for metrics storage
    ets:new(?METRICS_TABLE, [
        set,
        public,
        named_table,
        {write_concurrency, true},
        {read_concurrency, true}
    ]),

    %% Initialize standard metrics
    init_standard_metrics(),

    %% Schedule periodic metrics collection
    erlang:send_after(ScrapeInterval, self(), collect_system_metrics),

    {ok, #state{
        scrape_interval = ScrapeInterval,
        metrics_table = ?METRICS_TABLE,
        histogram_buckets = ?HISTOGRAM_BUCKETS,
        last_scrape = erlang:system_time(millisecond)
    }}.

handle_call(get_all_metrics, _From, State) ->
    Metrics = ets:foldl(fun({_Key, Metric}, Acc) ->
        [Metric | Acc]
    end, [], ?METRICS_TABLE),
    {reply, {ok, Metrics}, State};

handle_call(prometheus_export, _From, State) ->
    Export = generate_prometheus_export(State),
    {reply, Export, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({counter, Name, Value, Labels}, State) ->
    Key = {counter, Name, Labels},
    CurrentValue = case ets:lookup(?METRICS_TABLE, Key) of
        [{_, #metric{value = OldValue}}] -> OldValue;
        [] -> 0
    end,
    Metric = #metric{
        name = Name,
        type = counter,
        value = CurrentValue + Value,
        labels = Labels,
        timestamp = erlang:system_time(millisecond)
    },
    ets:insert(?METRICS_TABLE, {Key, Metric}),
    {noreply, State};

handle_cast({gauge, Name, Value, Labels}, State) ->
    Key = {gauge, Name, Labels},
    Metric = #metric{
        name = Name,
        type = gauge,
        value = Value,
        labels = Labels,
        timestamp = erlang:system_time(millisecond)
    },
    ets:insert(?METRICS_TABLE, {Key, Metric}),
    {noreply, State};

handle_cast({histogram, Name, Value, Labels}, State) ->
    Key = {histogram, Name, Labels},
    Buckets = State#state.histogram_buckets,
    CurrentData = case ets:lookup(?METRICS_TABLE, Key) of
        [{_, #metric{value = {Count, Sum, OldBuckets}}}] ->
            {Count, Sum, OldBuckets};
        [] ->
            {0, 0.0, maps:from_list([{B, 0} || B <- Buckets])}
    end,

    {OldCount, OldSum, OldBucketCounts} = CurrentData,
    NewCount = OldCount + 1,
    NewSum = OldSum + Value,
    NewBucketCounts = maps:map(fun(Bucket, AccCount) ->
        AccCount + case Value =< Bucket of
            true -> 1;
            false -> 0
        end
    end, OldBucketCounts),

    Metric = #metric{
        name = Name,
        type = histogram,
        value = {NewCount, NewSum, NewBucketCounts},
        labels = Labels,
        timestamp = erlang:system_time(millisecond)
    },
    ets:insert(?METRICS_TABLE, {Key, Metric}),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_system_metrics, State) ->
    collect_vm_metrics(State),
    collect_connection_metrics(State),
    collect_process_metrics(State),
    collect_gc_metrics(State),

    %% Schedule next collection
    erlang:send_after(State#state.scrape_interval, self(), collect_system_metrics),
    {noreply, State#state{last_scrape = erlang:system_time(millisecond)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% Initialize standard metrics that should always be present
%% @end
%%--------------------------------------------------------------------
init_standard_metrics() ->
    %% Connection metrics
    scrape_gauge(<<"erlmcp_connections_total">>, 0, #{
        <<"transport">> => <<"stdio">>
    }),
    scrape_gauge(<<"erlmcp_connections_total">>, 0, #{
        <<"transport">> => <<"tcp">>
    }),
    scrape_gauge(<<"erlmcp_connections_total">>, 0, #{
        <<"transport">> => <<"http">>
    }),
    scrape_gauge(<<"erlmcp_connections_total">>, 0, #{
        <<"transport">> => <<"ws">>
    }),

    %% Session metrics
    scrape_gauge(<<"erlmcp_active_sessions">>, 0, #{}),

    %% Queue metrics
    scrape_gauge(<<"erlmcp_queue_size">>, 0, #{
        <<"queue_type">> => <<"message">>
    }),

    %% Health metric
    scrape_gauge(<<"erlmcp_healthy">>, 1, #{}),

    ok.

%%--------------------------------------------------------------------
%% @private
%% Collect VM metrics (memory, scheduler)
%% @end
%%--------------------------------------------------------------------
collect_vm_metrics(State) ->
    %% Memory metrics
    MemoryTypes = [total, processes, system, atom, binary, code, ets],
    lists:foreach(fun(Type) ->
        Value = case erlang:memory(Type) of
            undefined -> 0;
            Mem -> Mem
        end,
        scrape_gauge(<<"erlmcp_memory_bytes">>, Value, #{
            <<"type">> => to_binary(Type)
        })
    end, MemoryTypes),

    %% CPU usage (scheduler utilization)
    TotalSchedulers = erlang:system_info(schedulers_online),
    BusySchedulers = length([S || S <- erlang:statistics(scheduler_wall_time), S =/= {0, 0}]),
    CPUUsage = case TotalSchedulers of
        0 -> 0;
        _ -> (BusySchedulers / TotalSchedulers) * 100
    end,
    scrape_gauge(<<"erlmcp_cpu_usage_percent">>, CPUUsage, #{}),

    %% I/O statistics
    {{_, InputBytes}, {_, OutputBytes}} = erlang:statistics(io),
    scrape_gauge(<<"erlmcp_io_bytes_total">>, InputBytes, #{
        <<"direction">> => <<"input">>
    }),
    scrape_gauge(<<"erlmcp_io_bytes_total">>, OutputBytes, #{
        <<"direction">> => <<"output">>
    }),

    ok.

%%--------------------------------------------------------------------
%% @private
%% Collect connection-related metrics
%% @end
%%--------------------------------------------------------------------
collect_connection_metrics(State) ->
    %% Get connection counts from gproc registry
    ConnectionCount = try
        gproc:lookup_value({l, erlmcp_connections})
    catch
        _:_ -> undefined
    end,
    ConnCount = case ConnectionCount of
        Val when is_integer(Val) -> Val;
        _ -> 0
    end,
    scrape_gauge(<<"erlmcp_connections_total">>, ConnCount, #{<<"transport">> => <<"all">>}),

    %% Transport-specific counts
    Transports = [stdio, tcp, http, ws, sse],
    lists:foreach(fun(Transport) ->
        RawCount = try
            gproc:lookup_value({l, {erlmcp_transport, Transport}})
        catch
            _:_ -> undefined
        end,
        Count = case RawCount of
            TransportCount when is_integer(TransportCount) -> TransportCount;
            _ -> 0
        end,
        scrape_gauge(<<"erlmcp_connections_total">>, Count, #{<<"transport">> => to_binary(Transport)})
    end, Transports),

    ok.

%%--------------------------------------------------------------------
%% @private
%% Collect process-related metrics
%% @end
%%--------------------------------------------------------------------
collect_process_metrics(State) ->
    %% Process count
    ProcessCount = erlang:system_info(process_count),
    scrape_gauge(<<"erlmcp_process_count">>, ProcessCount, #{}),

    %% Reductions
    {TotalReductions, _ReductionsSinceLastCall} = erlang:statistics(reductions),
    scrape_gauge(<<"erlmcp_reductions_total">>, TotalReductions, #{}),

    %% Context switches
    {ContextSwitches, _} = erlang:statistics(context_switches),
    scrape_gauge(<<"erlmcp_context_switches_total">>, ContextSwitches, #{}),

    %% Run queue
    RunQueue = erlang:statistics(run_queue),
    scrape_gauge(<<"erlmcp_run_queue_length">>, RunQueue, #{}),

    ok.

%%--------------------------------------------------------------------
%% @private
%% Collect garbage collection metrics
%% @end
%%--------------------------------------------------------------------
collect_gc_metrics(State) ->
    case erlang:statistics(garbage_collection) of
        {NumberOfGCs, WordsReclaimed, _} ->
            scrape_gauge(<<"erlmcp_gc_count">>, NumberOfGCs, #{}),
            scrape_gauge(<<"erlmcp_gc_words_reclaimed">>, WordsReclaimed, #{});
        _ ->
            ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% Generate Prometheus text format export
%% @end
%%--------------------------------------------------------------------
generate_prometheus_export(State) ->
    Lines = ets:foldl(fun({_Key, #metric{name = Name, type = Type, value = Value, labels = Labels}}, Acc) ->
        case Type of
            counter ->
                [format_metric(Name, Type, Value, Labels) | Acc];
            gauge ->
                [format_metric(Name, Type, Value, Labels) | Acc];
            histogram ->
                {Count, Sum, Buckets} = Value,
                BucketLines = format_histogram_buckets(Name, Labels, Buckets, State#state.histogram_buckets),
                SumLine = format_histogram_sum(Name, Labels, Sum),
                CountLine = format_histogram_count(Name, Labels, Count),
                [CountLine, SumLine | BucketLines] ++ Acc;
            summary ->
                [format_metric(Name, Type, Value, Labels) | Acc]
        end
    end, [], ?METRICS_TABLE),

    %% Add HELP and TYPE for each unique metric
    UniqueMetrics = lists:usort([Name || {_Key, #metric{name = Name}} <- ets:tab2list(?METRICS_TABLE)]),
    HeaderLines = lists:map(fun(Name) ->
        Type = case [M || {{_Type, N, _Labels}, #metric{type = T}} = M <- ets:tab2list(?METRICS_TABLE), N =:= Name] of
            [{_, #metric{type = T}} | _] -> T;
            _ -> gauge
        end,
        [
            io_lib:format("# HELP ~s ~s~n", [Name, get_metric_help(Name)]),
            io_lib:format("# TYPE ~s ~s~n", [Name, Type])
        ]
    end, UniqueMetrics),

    FinalLines = lists:flatten(HeaderLines) ++ lists:reverse(Lines),
    iolist_to_binary(FinalLines).

%%--------------------------------------------------------------------
%% @private
%% Format a single metric line
%% @end
%%--------------------------------------------------------------------
format_metric(Name, _Type, Value, Labels) ->
    LabelStr = format_labels(Labels),
    Suffix = case LabelStr of
        "" -> "";
        _ -> "{" ++ LabelStr ++ "}"
    end,
    io_lib:format("~s~s ~w~n", [Name, Suffix, round(Value * 1000) / 1000]).

%%--------------------------------------------------------------------
%% @private
%% Format histogram bucket lines
%% @end
%%--------------------------------------------------------------------
format_histogram_buckets(Name, Labels, BucketCounts, AllBuckets) ->
    LabelStr = format_labels(Labels),
    BaseLabel = case LabelStr of
        "" -> "";
        _ -> LabelStr
    end,

    %% Sort buckets
    SortedBuckets = lists:sort(AllBuckets),

    %% Generate bucket lines
    {Lines, _} = lists:mapfoldl(fun(Bucket, Acc) ->
        Count = maps:get(Bucket, BucketCounts, 0),
        BucketLabel = case BaseLabel of
            "" -> io_lib:format("le=\"~.3f\"", [Bucket]);
            _ -> io_lib:format("~s,le=\"~.3f\"", [BaseLabel, Bucket])
        end,
        Line = io_lib:format("~s_bucket{~s} ~w~n", [Name, BucketLabel, Count]),
        {Line, Acc + Count}
    end, 0, SortedBuckets),

    %% Add +Inf bucket
    InfLine = case BaseLabel of
        "" -> io_lib:format("~s_bucket{le=\"+Inf\" ~w~n", [Name, 0]);
        _ -> io_lib:format("~s_bucket{~s,le=\"+Inf\"} ~w~n", [Name, BaseLabel, 0])
    end,

    lists:reverse([InfLine | Lines]).

format_histogram_sum(Name, Labels, Sum) ->
    LabelStr = format_labels(Labels),
    Suffix = case LabelStr of
        "" -> "";
        _ -> "{" ++ LabelStr ++ "}"
    end,
    io_lib:format("~s_sum~s ~w~n", [Name, Suffix, round(Sum * 1000) / 1000]).

format_histogram_count(Name, Labels, Count) ->
    LabelStr = format_labels(Labels),
    Suffix = case LabelStr of
        "" -> "";
        _ -> "{" ++ LabelStr ++ "}"
    end,
    io_lib:format("~s_count~s ~w~n", [Name, Suffix, Count]).

%%--------------------------------------------------------------------
%% @private
%% Format labels as Prometheus label string
%% @end
%%--------------------------------------------------------------------
format_labels(Labels) when map_size(Labels) =:= 0 ->
    "";
format_labels(Labels) ->
    Pairs = maps:foldl(fun(Key, Value, Acc) ->
        EscapedKey = escape_label_value(to_binary(Key)),
        EscapedValue = escape_label_value(to_binary(Value)),
        [io_lib:format("~s=\"~s\"", [EscapedKey, EscapedValue]) | Acc]
    end, [], Labels),
    string:join(lists:reverse(Pairs), ",").

%%--------------------------------------------------------------------
%% @private
%% Escape special characters in label values
%% @end
%%--------------------------------------------------------------------
escape_label_value(Value) ->
    %% Escape backslashes, double quotes, and newlines
    Replacements = [
        {"\\", "\\\\"},
        {"\"", "\\\""},
        {"\n", "\\n"}
    ],
    lists:foldl(fun({From, To}, Acc) ->
        binary:replace(Acc, From, To, [global])
    end, Value, Replacements).

%%--------------------------------------------------------------------
%% @private
%% Get help text for a metric
%% @end
%%--------------------------------------------------------------------
get_metric_help(Name) ->
    HelpTexts = #{
        <<"erlmcp_requests_total">> => <<"Total number of requests processed">>,
        <<"erlmcp_responses_total">> => <<"Total number of responses sent">>,
        <<"erlmcp_errors_total">> => <<"Total number of errors">>,
        <<"erlmcp_request_duration_seconds">> => <<"Request processing duration in seconds">>,
        <<"erlmcp_connections_total">> => <<"Current number of active connections">>,
        <<"erlmcp_active_sessions">> => <<"Current number of active sessions">>,
        <<"erlmcp_memory_bytes">> => <<"Memory usage in bytes">>,
        <<"erlmcp_cpu_usage_percent">> => <<"CPU usage percentage">>,
        <<"erlmcp_process_count">> => <<"Number of Erlang processes">>,
        <<"erlmcp_queue_size">> => <<"Current queue size">>,
        <<"erlmcp_healthy">> => <<"Health status (1 = healthy, 0 = unhealthy)">>,
        <<"erlmcp_reductions_total">> => <<"Total reductions">>,
        <<"erlmcp_gc_count">> => <<"Garbage collection count">>,
        <<"erlmcp_gc_words_reclaimed">> => <<"Words reclaimed by garbage collection">>,
        <<"erlmcp_io_bytes_total">> => <<"Total I/O bytes">>,
        <<"erlmcp_run_queue_length">> => <<"Run queue length">>,
        <<"erlmcp_context_switches_total">> => <<"Total context switches">>
    },
    maps:get(Name, HelpTexts, <<"erlmcp metric">>).

%%--------------------------------------------------------------------
%% @private
%% Convert various types to binary
%% @end
%%--------------------------------------------------------------------
to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
to_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
to_binary(Value) when is_float(Value) -> float_to_binary(Value, [{decimals, 4}]);
to_binary(Value) when is_list(Value) -> list_to_binary(Value);
to_binary(Value) -> <<"unknown">>.
