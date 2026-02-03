%% @doc Enterprise Monitoring Adapter
%% Integrates with monitoring systems (Splunk, Datadog, New Relic)
-module(erlmcp_monitoring_adapter).

-behaviour(gen_server).

-export([start_link/0, send_metric/3, send_log/3, send_trace/3,
         query_metrics/2, setup_alert/3, get_dashboard/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type provider() :: splunk | datadog | newrelic.
-type metric_type() :: counter | gauge | histogram | summary.
-type metric_data() :: map().
-type log_data() :: map().
-type trace_data() :: map().
-type query() :: map().

-record(state, {
    provider :: provider(),
    config :: map(),
    connection :: pid() | undefined,
    cache :: map(),
    metrics :: map()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Send a metric to the monitoring system
-spec send_metric(binary(), metric_type(), metric_data()) -> {ok, binary()} | {error, term()}.
send_metric(MetricName, MetricType, Data) ->
    gen_server:call(?MODULE, {send_metric, MetricName, MetricType, Data}).

%% Send a log entry to the monitoring system
-spec send_log(binary(), log_data(), map()) -> {ok, binary()} | {error, term()}.
send_log(LogCategory, LogData, Context) ->
    gen_server:call(?MODULE, {send_log, LogCategory, LogData, Context}).

%% Send a trace to the monitoring system
-spec send_trace(binary(), trace_data(), map()) -> {ok, binary()} | {error, term()}.
send_trace(TraceName, TraceData, Context) ->
    gen_server:call(?MODULE, {send_trace, TraceName, TraceData, Context}).

%% Query metrics from the monitoring system
-spec query_metrics(query(), map()) -> {ok, [metric_data()]} | {error, term()}.
query_metrics(Query, Context) ->
    gen_server:call(?MODULE, {query_metrics, Query, Context}).

%% Set up an alert rule
-spec setup_alert(binary(), map(), map()) -> {ok, binary()} | {error, term()}.
setup_alert(AlertName, AlertRule, Context) ->
    gen_server:call(?MODULE, {setup_alert, AlertName, AlertRule, Context}).

%% Get dashboard configuration
-spec get_dashboard(binary(), map()) -> {ok, map()} | {error, term()}.
get_dashboard(DashboardName, Context) ->
    gen_server:call(?MODULE, {get_dashboard, DashboardName, Context}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize state with configuration
    Config = load_config(),
    Provider = maps:get(provider, Config, undefined),

    State = #state{
        provider = Provider,
        config = Config,
        connection = undefined,
        cache = #{},
        metrics = #{}
    },

    %% Initialize provider-specific connection
    case Provider of
        splunk ->
            splunk_connection:start(State);
        datadog ->
            datadog_connection:start(State);
        newrelic ->
            newrelic_connection:start(State);
        _ ->
            throw({invalid_provider, Provider})
    end,

    {ok, State}.

handle_call({send_metric, MetricName, MetricType, Data}, _From, State) ->
    try
        %% Send metric based on provider
        case State#state.provider of
            splunk -> send_splunk_metric(MetricName, MetricType, Data, State);
            datadog -> send_datadog_metric(MetricName, MetricType, Data, State);
            newrelic -> send_newrelic_metric(MetricName, MetricType, Data, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to send metric: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({send_log, LogCategory, LogData, Context}, _From, State) ->
    try
        %% Send log based on provider
        case State#state.provider of
            splunk -> send_splunk_log(LogCategory, LogData, Context, State);
            datadog -> send_datadog_log(LogCategory, LogData, Context, State);
            newrelic -> send_newrelic_log(LogCategory, LogData, Context, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to send log: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({send_trace, TraceName, TraceData, Context}, _From, State) ->
    try
        %% Send trace based on provider
        case State#state.provider of
            splunk -> send_splunk_trace(TraceName, TraceData, Context, State);
            datadog -> send_datadog_trace(TraceName, TraceData, Context, State);
            newrelic -> send_newrelic_trace(TraceName, TraceData, Context, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to send trace: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({query_metrics, Query, Context}, _From, State) ->
    try
        %% Query metrics based on provider
        case State#state.provider of
            splunk -> query_splunk_metrics(Query, Context, State);
            datadog -> query_datadog_metrics(Query, Context, State);
            newrelic -> query_newrelic_metrics(Query, Context, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to query metrics: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({setup_alert, AlertName, AlertRule, Context}, _From, State) ->
    try
        %% Set up alert based on provider
        case State#state.provider of
            splunk -> setup_splunk_alert(AlertName, AlertRule, Context, State);
            datadog -> setup_datadog_alert(AlertName, AlertRule, Context, State);
            newrelic -> setup_newrelic_alert(AlertName, AlertRule, Context, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to set up alert: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({get_dashboard, DashboardName, Context}, _From, State) ->
    try
        %% Get dashboard based on provider
        case State#state.provider of
            splunk -> get_splunk_dashboard(DashboardName, Context, State);
            datadog -> get_datadog_dashboard(DashboardName, Context, State);
            newrelic -> get_newrelic_dashboard(DashboardName, Context, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Failed to get dashboard: ~p:~p", [Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Clean up resources
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

load_config() ->
    %% Load monitoring provider configuration
    case application:get_env(erlmcp_enterprise_integrations, monitoring_config) of
        undefined -> default_monitoring_config();
        {ok, Config} -> Config
    end.

default_monitoring_config() ->
    #{
        provider => splunk,
        endpoint => "https://your-splunk-instance:8088",
        api_key => undefined,
        username => undefined,
        password => undefined,
        app => "erlmcp",
        index => "main",
        timeout => 30000
    }.

%% Splunk Integration
send_splunk_metric(MetricName, MetricType, Data, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/services/collector/event",
    Headers = splunk_headers(State),
    Event = splunk_metric_event(MetricName, MetricType, Data),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(Event)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"id">>, Response)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {metric_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

send_splunk_log(LogCategory, LogData, Context, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/services/collector/event",
    Headers = splunk_headers(State),
    Event = splunk_log_event(LogCategory, LogData, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(Event)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"id">>, Response)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {log_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

send_splunk_trace(TraceName, TraceData, Context, State) ->
    %% Splunk doesn't have native trace support
    %% Send as logs with trace context
    send_splunk_log(<<"trace">>, TraceData, Context#{<<"trace_name">> => TraceName}, State).

query_splunk_metrics(Query, Context, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/services/search/jobs",
    Headers = splunk_headers(State),
    SearchQuery = splunk_search_query(Query, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(SearchQuery)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            JobResponse = jsx:decode(ResponseBody, [{labels, binary}]),
            JobId = proplists:get_value(<<"sid">>, JobResponse),
            wait_for_splunk_results(JobId, State);
        {ok, {{_, Code, _}, _, _}} ->
            {error, {query_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

setup_splunk_alert(AlertName, AlertRule, Context, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/services/alerts/saved/searches",
    Headers = splunk_headers(State),
    AlertConfig = splunk_alert_config(AlertName, AlertRule, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(AlertConfig)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"name">>, Response)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {alert_setup_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

get_splunk_dashboard(DashboardName, Context, State) ->
    Endpoint = maps:get(endpoint, State#state.config) ++ "/servicesNS/nobody/" ++
               maps:get(app, State#state.config) ++ "/data/ui/views/" ++
               DashboardName,
    Headers = splunk_headers(State),

    case httpc:request(get, {Endpoint, Headers},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(ResponseBody, [{labels, binary}]};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {dashboard_not_found, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

%% Datadog Integration
send_datadog_metric(MetricName, MetricType, Data, State) ->
    Endpoint = "https://api.datadoghq.com/api/v1/series",
    Headers = datadog_headers(State),
    Series = datadog_metric_series(MetricName, MetricType, Data),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(Series)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 202, _}, _, ResponseBody}} ->
            {ok, <<"metrics_sent">>};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {metric_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

send_datadog_log(LogCategory, LogData, Context, State) ->
    Endpoint = "https://http-intake.logs.datadoghq.com/api/v2/logs",
    Headers = datadog_headers(State),
    Log = datadog_log_entry(LogCategory, LogData, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(Log)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 202, _}, _, ResponseBody}} ->
            {ok, <<"logs_sent">>};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {log_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

send_datadog_trace(TraceName, TraceData, Context, State) ->
    Endpoint = "https://http-intake.trace.datadoghq.com/api/v2/traces",
    Headers = datadog_headers(State),
    Traces = datadog_traces(TraceName, TraceData, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(Traces)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 202, _}, _, ResponseBody}} ->
            {ok, <<"traces_sent">>};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {trace_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

query_datadog_metrics(Query, Context, State) ->
    Endpoint = "https://api.datadoghq.com/api/v1/query",
    Headers = datadog_headers(State),
    QueryParams = datadog_query_params(Query, Context),

    case httpc:request(get, {Endpoint, Headers ++ QueryParams},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"series">>, Response, [])};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {query_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

setup_datadog_alert(AlertName, AlertRule, Context, State) ->
    Endpoint = "https://api.datadoghq.com/api/v1/alerts",
    Headers = datadog_headers(State),
    Alert = datadog_alert_config(AlertName, AlertRule, Context),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(Alert)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"id">>, Response)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {alert_setup_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

get_datadog_dashboard(DashboardName, Context, State) ->
    Endpoint = "https://api.datadoghq.com/api/v1/dashboard/" ++ DashboardName,
    Headers = datadog_headers(State),

    case httpc:request(get, {Endpoint, Headers},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(ResponseBody, [{labels, binary}]};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {dashboard_not_found, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

%% New Relic Integration
send_newrelic_metric(MetricName, MetricType, Data, State) ->
    %% New Relic doesn't have a direct metrics API
    %% Send as custom events instead
    send_newrelic_event(<<"metric">>, Data#{<<"name">> => MetricName, <<"type">> => MetricType}, State).

send_newrelic_log(LogCategory, LogData, Context, State) ->
    Event = newrelic_log_event(LogCategory, LogData, Context),
    send_newrelic_event(<<"log">>, Event, State).

send_newrelic_trace(TraceName, TraceData, Context, State) ->
    Event = newrelic_trace_event(TraceName, TraceData, Context),
    send_newrelic_event(<<"trace">>, Event, State).

send_newrelic_event(EventType, EventData, State) ->
    Endpoint = "https://api.newrelic.com/v1/events",
    Headers = newrelic_headers(State),
    Event = newrelic_event(EventType, EventData),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(Event)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 202, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"success">>, Response)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {event_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

query_newrelic_metrics(Query, Context, State) ->
    %% New Relic uses NRQL for queries
    Endpoint = "https://api.newrelic.com/v1/accounts/" ++
               maps:get(account_id, State#state.config) ++
               "/nrql",
    Headers = newrelic_headers(State),
    NRQL = maps:get(query, Query),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(#{<<"nrql">> => NRQL})},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"results">>, Response, [])};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {query_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

setup_newrelic_alert(AlertName, AlertRule, Context, State) ->
    Endpoint = "https://api.newrelic.com/v1/alerts_conditions",
    Headers = newrelic_headers(State),
    Alert = newrelic_alert_config(AlertName, AlertRule, Context, State),

    case httpc:request(post, {Endpoint, Headers, "application/json", jsx:encode(Alert)},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 201, _}, _, ResponseBody}} ->
            Response = jsx:decode(ResponseBody, [{labels, binary}]),
            {ok, proplists:get_value(<<"id">>, Response)};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {alert_setup_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

get_newrelic_dashboard(DashboardName, Context, State) ->
    Endpoint = "https://api.newrelic.com/v1/dashboards/" ++ DashboardName,
    Headers = newrelic_headers(State),

    case httpc:request(get, {Endpoint, Headers},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(ResponseBody, [{labels, binary}]};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {dashboard_not_found, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

%% Helper Functions
splunk_headers(State) ->
    #{
        <<"Authorization">> => <<"Splunk ", (maps:get(api_key, State#state.config))/binary>>,
        <<"X-Splunk-Request-Id">> => generate_uuid(),
        <<"Content-Type">> => <<"application/json">>
    }.

splunk_metric_event(MetricName, MetricType, Data) ->
    #{
        <<"time">> => erlang:system_time(second),
        <<"host">> => list_to_binary(atom_to_list(node())),
        <<"source">> => <<"erlmcp">>,
        <<"sourcetype">> => <<"erlmcp:metric">>,
        <<"event">> => Data#{
            <<"name">> => MetricName,
            <<"type">> => MetricType,
            <<"value">> => maps:get(value, Data),
            <<"tags">> => maps:get(tags, Data, #{})
        }
    }.

splunk_log_event(LogCategory, LogData, Context) ->
    #{
        <<"time">> => erlang:system_time(second),
        <<"host">> => list_to_binary(atom_to_list(node())),
        <<"source">> => <<"erlmcp">>,
        <<"sourcetype">> => <<"erlmcp:log">>,
        <<"event">> => LogData#{
            <<"category">> => LogCategory,
            <<"context">> => Context,
            <<"level">> => maps:get(level, LogData, <<"info">>)
        }
    }.

splunk_search_query(Query, Context) ->
    Search = proplists:get_value(query, Query),
    #{
        <<"search">> => Search,
        <<"earliest_time">> => proplists:get_value(earliest_time, Query, "-5m"),
        <<"latest_time">> => proplists:get_value(latest_time, Query, "now"),
        <<"context">> => Context
    }.

splunk_alert_config(AlertName, AlertRule, Context) ->
    #{
        <<"name">> => AlertName,
        <<"search">> => proplists:get_value(query, AlertRule),
        <<"alert_condition">> => proplists:get_value(condition, AlertRule, ">0"),
        <<"alert_type">> => simple,
        <<"dispatch_email">> => proplists:get_value(email, Context, []),
        <<"dispatch_webhook">> => proplists:get_value(webhook, Context, [])
    }.

datadog_headers(State) ->
    case maps:get(api_key, State#state.config) of
        undefined ->
            #{
                <<"DD-API-KEY">> => <<"your-api-key">>,
                <<"Content-Type">> => <<"application/json">>
            };
        ApiKey ->
            #{
                <<"DD-API-KEY">> => ApiKey,
                <<"Content-Type">> => <<"application/json">>
            }
    end.

datadog_metric_series(MetricName, MetricType, Data) ->
    #{
        <<"series">> => [#{
            <<"metric">> => MetricName,
            <<"type">> => MetricType,
            <<"points">> => [[erlang:system_time(second), maps:get(value, Data)]],
            <<"tags">> => maps:get(tags, Data, #{})
        }]
    }.

datadog_log_entry(LogCategory, LogData, Context) ->
    #{
        <<"ddsource">> => <<"erlmcp">>,
        <<"ddtags">> => <<"category:" ++ binary_to_list(LogCategory)>>,
        <<"hostname">> => list_to_binary(atom_to_list(node())),
        <<"message">> => jsx:encode(LogData),
        <<"context">> => Context
    }.

datadog_traces(TraceName, TraceData, Context) ->
    #{
        <<"common">> => #{
            <<"service_name">> => <<"erlmcp">>,
            <<"environment">> => <<"production">>
        },
        <<"traces">> => [TraceData]
    }.

datadog_query_params(Query, Context) ->
    Params = #{
        <<"query">> => proplists:get_value(query, Query),
        <<"from">> => proplists:get_value(from, Query, now-5m),
        <<"to">> => proplists:get_value(to, Query, now),
        <<"timeframe">> => proplists:get_value(timeframe, Query, 5m)
    },
    uri_string:compose_query([{atom_to_binary(K, utf8), V} || {K, V} <- maps:to_list(Params)]).

datadog_alert_config(AlertName, AlertRule, Context) ->
    #{
        <<"name">> => AlertName,
        <<"type">> => query,
        <<"query">> => proplists:get_value(query, AlertRule),
        <<"message">> => proplists:get_value(message, AlertRule, AlertName),
        <<"tags">> => maps:get(tags, Context, []),
        <<"options">> => #{
            <<"thresholds">> => [proplists:get_value(threshold, AlertRule, 1)],
            <<"evaluation_window">> => proplists:get_value(window, AlertRule, 5m)
        }
    }.

newrelic_headers(State) =>
    #{
        <<"X-Api-Key">> => maps:get(api_key, State#state.config),
        <<"Content-Type">> => <<"application/json">>
    }.

newrelic_log_event(LogCategory, LogData, Context) =>
    #{
        <<"eventType">> => <<"Log">>,
        <<"category">> => LogCategory,
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"message">> => jsx:encode(LogData),
        <<"context">> => Context,
        <<"hostname">> => list_to_binary(atom_to_list(node()))
    }.

newrelic_trace_event(TraceName, TraceData, Context) =>
    #{
        <<"eventType">> => <<"Trace">>,
        <<"name">> => TraceName,
        <<"data">> => TraceData,
        <<"context">> => Context,
        <<"timestamp">> => erlang:system_time(millisecond)
    }.

newrelic_event(EventType, EventData) =>
    #{
        eventType => EventType,
        timestamp => erlang:system_time(millisecond),
        data => EventData
    }.

newrelic_alert_config(AlertName, AlertRule, Context, State) =>
    #{
        <<"name">> => AlertName,
        <<"type">> => static,
        <<"time_windows">> => [proplists:get_value(window, AlertRule, 60)],
        <<"evaluation_offset">> => proplists:get_value(offset, AlertRule, 0),
        <<"baseline_direction">> => proplists:get_value(direction, AlertRule, above),
        <<"critical">> => [proplists:get_value(critical, AlertRule, 1)],
        <<"description">> => proplists:get_value(description, AlertRule, AlertName),
        <<"nrql">> => proplists:get_value(query, AlertRule)
    }.

wait_for_splunk_results(JobId, State) ->
    %% Wait for Splunk job to complete
    Endpoint = maps:get(endpoint, State#state.config) ++
               "/services/search/jobs/" ++ JobId ++ "/results",
    Headers = splunk_headers(State),

    case httpc:request(get, {Endpoint, Headers},
                     [{timeout, maps:get(timeout, State#state.config)}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            {ok, jsx:decode(ResponseBody, [{labels, binary}]};
        {ok, {{_, Code, _}, _, _}} ->
            {error, {query_failed, Code}};
        {error, Reason} ->
            {error, {network_error, Reason}}
    end.

generate_uuid() ->
    %% Generate a UUID for request tracking
    binary_to_list(erlang:ref_to_list(make_ref())).