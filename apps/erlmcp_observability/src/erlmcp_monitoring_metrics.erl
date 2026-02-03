%%%-------------------------------------------------------------------
%%% @doc
%%% Enterprise metrics collection module for erlmcp v3
%%% Collects comprehensive metrics including business, technical, and security metrics
%%% with proper cardinality management and high cardinality handling
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_monitoring_metrics).

-behaviour(gen_server).

%% API
-export([start_link/0,
         register_metric/1,
         record_counter/2,
         record_gauge/2,
         record_histogram/2,
         record_business_metric/3,
         record_error/2,
         record_security_event/2,
         get_metrics_summary/0,
         get_metric_history/2,
         export_metrics/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(METRICS_REGISTRY, erlmcp_metrics_registry).
-define(HIGH_CARDINALITY_LIMIT, 100).

-include("erlmcp_observability.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the monitoring metrics server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Register a new metric definition
%% @end
%%--------------------------------------------------------------------
register_metric(#metric_def{name = Name, type = Type, description = Desc, tags = Tags}) ->
    gen_server:call(?SERVER, {register_metric, Name, Type, Desc, Tags}).

%%--------------------------------------------------------------------
%% @doc
%% Record a counter metric value
%% @end
%%--------------------------------------------------------------------
record_counter(Name, Value) ->
    gen_server:cast(?SERVER, {record_counter, Name, Value, #{}}).

record_counter(Name, Value, Tags) ->
    gen_server:cast(?SERVER, {record_counter, Name, Value, Tags}).

%%--------------------------------------------------------------------
%% @doc
%% Record a gauge metric value
%% @end
%%--------------------------------------------------------------------
record_gauge(Name, Value) ->
    gen_server:cast(?SERVER, {record_gauge, Name, Value, #{}}).

record_gauge(Name, Value, Tags) ->
    gen_server:cast(?SERVER, {record_gauge, Name, Value, Tags}).

%%--------------------------------------------------------------------
%% @doc
%% Record a histogram metric value
%% @end
%%--------------------------------------------------------------------
record_histogram(Name, Value) ->
    gen_server:cast(?SERVER, {record_histogram, Name, Value, #{}}).

record_histogram(Name, Value, Tags) ->
    gen_server:cast(?SERVER, {record_histogram, Name, Value, Tags}).

%%--------------------------------------------------------------------
%% @doc
%% Record a business metric with automatic high cardinality handling
%% @end
%%--------------------------------------------------------------------
record_business_metric(Category, Metric, Value) ->
    record_business_metric(Category, Metric, Value, #{}).

record_business_metric(Category, Metric, Value, Tags) when is_binary(Category) ->
    SafeTags = maybe_sanitize_high_cardinality(Category, Tags),
    gen_server:cast(?SERVER, {record_business_metric, Category, Metric, Value, SafeTags}).

%%--------------------------------------------------------------------
%% @doc
%% Record an error with context and stack trace
%% @end
%%--------------------------------------------------------------------
record_error(Error, Context) ->
    gen_server:cast(?SERVER, {record_error, Error, Context}).

%%--------------------------------------------------------------------
%% @doc
%% Record a security event with severity and classification
%% @end
%%--------------------------------------------------------------------
record_security_event(Event, Severity) ->
    record_security_event(Event, Severity, #{}).

record_security_event(Event, Severity, Tags) when is_atom(Severity) ->
    SecurityTags = maps:put(<<"event_severity">>, atom_to_binary(Severity, utf8), Tags),
    gen_server:cast(?SERVER, {record_security_event, Event, SecurityTags}).

%%--------------------------------------------------------------------
%% @doc
%% Get metrics summary for all registered metrics
%% @end
%%--------------------------------------------------------------------
get_metrics_summary() ->
    gen_server:call(?SERVER, get_metrics_summary).

%%--------------------------------------------------------------------
%% @doc
%% Get historical data for a specific metric
%% @end
%%--------------------------------------------------------------------
get_metric_history(Name, TimeRange) ->
    gen_server:call(?SERVER, {get_metric_history, Name, TimeRange}).

%%--------------------------------------------------------------------
%% @doc
%% Export metrics in Prometheus format
%% @end
%%--------------------------------------------------------------------
export_metrics(OutputFormat) ->
    gen_server:call(?SERVER, {export_metrics, OutputFormat}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Initialize metrics registry
    ets:new(?METRICS_REGISTRY, [set, public, named_table]),

    %% Initialize metric storage
    ets:new(erlmcp_metrics_storage, [set, public, named_table]),

    %% Start metrics collection timer
    erlang:send_after(1000, self(), collect_metrics),

    %% Initialize default metrics
    init_default_metrics(),

    {ok, #{last_collection => erlang:system_time(millisecond),
           metrics_collected => 0,
           errors_detected => 0,
           security_events => 0}}.

handle_call({register_metric, Name, Type, Description, Tags}, _From, State) ->
    %% Register metric with validation
    MetricDef = #metric_def{
        name = Name,
        type = Type,
        description = Description,
        tags = Tags,
        registered_at = erlang:system_time(millisecond)
    },
    ets:insert(?METRICS_REGISTRY, {Name, MetricDef}),
    {reply, ok, State};

handle_call(get_metrics_summary, _From, State) ->
    Summary = generate_metrics_summary(),
    {reply, Summary, State};

handle_call({get_metric_history, Name, TimeRange}, _From, State) ->
    History = get_metric_data_history(Name, TimeRange),
    {reply, History, State};

handle_call({export_metrics, prometheus}, _From, State) ->
    Exported = export_prometheus_metrics(),
    {reply, Exported, State};

handle_call({export_metrics, json}, _From, State) ->
    Exported = export_json_metrics(),
    {reply, Exported, State}.

handle_cast({record_counter, Name, Value, Tags}, State) ->
    record_metric(Name, counter, Value, Tags),
    {noreply, State#{metrics_collected := State#metrics_collected + 1}};

handle_cast({record_gauge, Name, Value, Tags}, State) ->
    record_metric(Name, gauge, Value, Tags),
    {noreply, State#{metrics_collected := State#metrics_collected + 1}};

handle_cast({record_histogram, Name, Value, Tags}, State) ->
    record_metric(Name, histogram, Value, Tags),
    {noreply, State#{metrics_collected := State#metrics_collected + 1}};

handle_cast({record_business_metric, Category, Metric, Value, Tags}, State) ->
    FullName = <<Category/binary, <<"_">>/binary, Metric/binary>>,
    record_metric(FullName, gauge, Value, Tags),
    {noreply, State#{metrics_collected := State#metrics_collected + 1}};

handle_cast({record_error, Error, Context}, State) ->
    %% Record error metric
    ErrorTag = case Error of
                  {error, Reason} -> Reason;
                  {error, _Mod, Reason, _Stack} -> Reason;
                  Reason -> Reason
              end,
    ErrorTags = #{
        <<"error_type">> => atom_to_binary(ErrorTag, utf8),
        <<"context">> => jsx:encode(Context)
    },
    record_metric(<<"erlmcp_errors_total">>, counter, 1, ErrorTags),

    %% Store error details
    ErrorDetail = #{
        timestamp => erlang:system_time(millisecond),
        error => Error,
        context => Context,
        tags => ErrorTags
    },
    ets:insert(erlmcp_error_storage, {erlang:system_time(millisecond), ErrorDetail}),

    {noreply, State#{errors_detected := State#errors_detected + 1}};

handle_cast({record_security_event, Event, Tags}, State) ->
    SecurityMetric = #{
        timestamp => erlang:system_time(millisecond),
        event => Event,
        tags => Tags
    },
    ets:insert(erlmcp_security_storage, {erlang:system_time(millisecond), SecurityMetric}),

    {noreply, State#{security_events := State#security_events + 1}}.

handle_info(collect_metrics, State) ->
    collect_system_metrics(),
    erlang:send_after(1000, self(), collect_metrics),
    {noreply, State#{last_collection := erlang:system_time(millisecond)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

init_default_metrics() ->
    %% Core system metrics
    register_metric(#metric_def{
        name = <<"erlmcp_connections_total">>,
        type = counter,
        description = "Total number of connections established",
        tags = [<<"transport">>, <<"protocol">>]
    }),

    register_metric(#metric_def{
        name = <<"erlmcp_requests_total">>,
        type = counter,
        description = "Total number of requests processed",
        tags = [<<"service">>]
    }),

    register_metric(#metric_def{
        name = <<"erlmcp_responses_total">>,
        type = counter,
        description = "Total number of responses sent",
        tags = [<<"status">>]
    }),

    register_metric(#metric_def{
        name = <<"erlmcp_request_duration_seconds">>,
        type = histogram,
        description = "Request processing duration in seconds",
        tags = [<<"endpoint">>, <<"method">>]
    }),

    %% Performance metrics
    register_metric(#metric_def{
        name = <<"erlmcp_queue_size">>,
        type = gauge,
        description = "Current queue size",
        tags = [<<"queue_type">>]
    }),

    register_metric(#metric_def{
        name = <<"erlmcp_process_count">>,
        type = gauge,
        description = "Number of running processes",
        tags = [<<"process_type">>]
    }),

    %% Business metrics
    register_metric(#metric_def{
        name = <<"erlmcp_active_sessions">>,
        type = gauge,
        description = "Number of active sessions",
        tags = [<<"session_type">>]
    }),

    register_metric(#metric_def{
        name = <<"erlmcp_tools_invocations">>,
        type = counter,
        description = "Total tool invocations",
        tags = [<<"tool_name">>]
    }).

record_metric(Name, Type, Value, Tags) ->
    %% Add timestamp to tags
    TimestampedTags = maps:put(<<"timestamp">>, erlang:system_time(millisecond), Tags),

    %% Store metric data
    MetricData = #{
        name => Name,
        type => Type,
        value => Value,
        tags => TimestampedTags,
        timestamp => erlang:system_time(millisecond)
    },
    ets:insert(erlmcp_metrics_storage, {Name, TimestampedTags, MetricData}).

maybe_sanitize_high_cardinality(Category, Tags) ->
    %% Check for high cardinality tags
    TagValues = maps:values(Tags),
    if
        length(TagValues) > ?HIGH_CARDINALITY_LIMIT ->
            %% Sanitize by removing some tags or using hash
            Sanitized = maps:without(
                lists:nthtail(?HIGH_CARDINALITY_LIMIT - 1, maps:keys(Tags)),
                Tags
            ),
            maps:put(<<"sanitized">>, true, Sanitized);
        true ->
            Tags
    end.

collect_system_metrics() ->
    %% Memory metrics
    {memory, MemoryData} = erlang:memory(),
    record_gauge(<<"erlmcp_memory_bytes">>, MemoryData, #{
        <<"type">> => atom_to_binary(memory_type, utf8)
    }),

    %% Process metrics
    ProcessCount = erlang:system_info(process_count),
    record_gauge(<<"erlmcp_system_processes">>, ProcessCount),

    %% Queue metrics
    case whereis(erlmcp_registry) of
        undefined -> ok;
        _ ->
            RegistrySize = ets:info(erlmcp_registry, size),
            record_gauge(<<"erlmcp_registry_size">>, RegistrySize)
    end.

generate_metrics_summary() ->
    %% Count registered metrics
    MetricsCount = ets:info(?METRICS_REGISTRY, size),

    %% Count stored metrics
    StoredMetrics = ets:info(erlmcp_metrics_storage, size),

    %% Count errors
    ErrorsCount = ets:info(erlmcp_error_storage, size, 0),

    %% Count security events
    SecurityCount = ets:info(erlmcp_security_storage, size, 0),

    #{
        metrics_registered => MetricsCount,
        metrics_stored => StoredMetrics,
        errors_detected => ErrorsCount,
        security_events => SecurityCount,
        last_updated => erlang:system_time(millisecond)
    }.

get_metric_data_history(Name, TimeRange) ->
    Now = erlang:system_time(millisecond),
    StartTime = Now - TimeRange,

    ets:foldl(fun({_, _, Data} = Key, Acc) ->
        case maps:get(timestamp, Data) of
            T when T >= StartTime ->
                [Key | Acc];
            _ ->
                Acc
        end
    end, [], erlmcp_metrics_storage).

export_prometheus_metrics() ->
    %% Format metrics for Prometheus
    Lines = ets:foldl(fun({_, _, #{name := Name, type := Type, value := Value, tags := Tags}}, Acc) ->
        Line = format_prometheus_metric(Name, Type, Value, Tags),
        [Line | Acc]
    end, [], erlmcp_metrics_storage),

    lists:reverse(Lines).

format_prometheus_metric(Name, Type, Value, Tags) ->
    %% Format tags as Prometheus labels
    LabelPairs = maps:fold(fun(Key, Val, Acc) ->
        Acc ++ [Key, Val]
    end, [], Tags),

    Case = case Type of
              counter -> "_total";
              histogram -> "_sum";
              _ -> ""
          end,

    Line = io_lib:format("~s~s{~s} ~.2f", [
        binary_to_list(Name),
        Case,
        string:join(lists:map(fun(B) -> binary_to_list(B) end, LabelPairs), ", "),
        Value
    ]),

    lists:flatten(Line).

export_json_metrics() ->
    %% Format metrics as JSON
    Metrics = ets:foldl(fun({_, _, Data}, Acc) ->
        [Data | Acc]
    end, [], erlmcp_metrics_storage),

    jsx:encode(#{
        metrics => Metrics,
        timestamp => erlang:system_time(millisecond)
    }).