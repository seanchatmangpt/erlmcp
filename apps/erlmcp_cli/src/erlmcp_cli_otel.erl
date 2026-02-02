%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_otel - CLI OpenTelemetry Integration
%%%
%%% Provides comprehensive OpenTelemetry integration for CLI operations
%%% with JSON-RPC 2.0 protocol support and multi-transport
compatibility.

%%% Integrates with erlmcp_observability for distributed tracing.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_otel).

-behaviour(gen_server).

%% API
-export([start_link/0, with_span/3, with_span/4, record_metric/2, record_metric/3, record_error/2,
         record_event/3, inject_span/3, inject_span/4, extract_span/1, get_tracer/0, get_meter/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(otel_state,
        {tracer :: opentelemetry:tracer(),      % OpenTelemetry tracer
         meter :: opentelemetry:meter(),          % OpenTelemetry meter
         config :: map(),                         % OTEL configuration
         spans :: map(),                          % Active spans
         exporters :: list(),                     % Exporter configurations
         metrics :: map(),                        % Metrics registry
         exporters :: list(),                     % Exporter processes
         resource :: map(),                       % Resource attributes
         service_name :: binary()}).               % Service name

%% Default OTEL configuration
-define(DEFAULT_OTEL_CONFIG,
        #{<<"exporter">> => <<"otlp">>,              % OTLP exporter
          <<"endpoint">> => <<"http://localhost:4317">>,
          <<"headers">> => #{},
          <<"resource">> =>
              #{<<"service.name">> => <<"erlmcp_cli">>,
                <<"service.version">> => <<"2.1.0">>,
                <<"telemetry.sdk.name">> => <<"erlmcp">>,
                <<"telemetry.sdk.version">> => <<"2.1.0">>},
          <<"sampling">> => #{<<"type">> => <<"always_on">>, <<"trace_ratio">> => 1.0},
          <<"batch_size">> => 512,
          <<"batch_timeout">> => 5000}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the OTEL integration
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Execute code within a span
-spec with_span(binary(), fun(() -> T)) -> T.
with_span(Name, Fun) ->
    with_span(Name, #{}, Fun).

%% @doc Execute code within a span with attributes
-spec with_span(binary(), map(), fun(() -> T)) -> T.
with_span(Name, Attributes, Fun) ->
    with_span(Name, Attributes, #{}, Fun).

%% @doc Execute code within a span with attributes and links
-spec with_span(binary(), map(), map(), fun(() -> T)) -> T.
with_span(Name, Attributes, Links, Fun) ->
    try
        %% Start span
        SpanCtx = start_span(Name, Attributes, Links),

        try
            %% Execute function
            Result = Fun(),

            %% End span successfully
            end_span(SpanCtx, ok),

            Result
        catch
            Class:Reason:Stacktrace ->
                %% End span with error
                end_span(SpanCtx, {Class, Reason, Stacktrace}),
                erlang:raise(Class, Reason, Stacktrace)
        end
    catch
        Error:Reason ->
            lager:warning("Failed to create span ~p: ~p:~p", [Name, Error, Reason]),
            Fun()
    end.

%% @doc Record a metric
-spec record_metric(binary(), number()) -> ok.
record_metric(Name, Value) ->
    record_metric(Name, Value, #{}).

%% @doc Record a metric with attributes
-spec record_metric(binary(), number(), map()) -> ok.
record_metric(Name, Value, Attributes) ->
    gen_server:cast(?SERVER, {record_metric, Name, Value, Attributes}).

%% @doc Record an error
-spec record_error(term(), term()) -> ok.
record_error(SpanCtx, Error) ->
    gen_server:cast(?SERVER, {record_error, SpanCtx, Error}).

%% @doc Record an event
-spec record_event(term(), binary(), map()) -> ok.
record_event(SpanCtx, Name, Attributes) ->
    gen_server:cast(?SERVER, {record_event, SpanCtx, Name, Attributes}).

%% @doc Inject span context
-spec inject_span(binary(), map(), term()) -> term().
inject_span(Name, Attributes, SpanCtx) ->
    inject_span(Name, Attributes, #{}, SpanCtx).

%% @doc Inject span context with links
-spec inject_span(binary(), map(), map(), term()) -> term().
inject_span(Name, Attributes, Links, SpanCtx) ->
    try
        %% Create new span context
        NewSpanCtx = start_span(Name, Attributes, Links, SpanCtx),

        %% Add links to existing context
        case SpanCtx of
            undefined ->
                NewSpanCtx;
            _ ->
                link_spans(SpanCtx, NewSpanCtx)
        end
    catch
        Error:Reason ->
            lager:warning("Failed to inject span ~p: ~p:~p", [Name, Error, Reason]),
            SpanCtx
    end.

%% @doc Extract span context from headers
-spec extract_span(map()) -> term().
extract_span(Headers) ->
    try
        %% Extract trace context from headers
        TraceContext = extract_trace_context(Headers),
        otel_context:new(TraceContext)
    catch
        Error:Reason ->
            lager:warning("Failed to extract span context: ~p:~p", [Error, Reason]),
            undefined
    end.

%% @doc Get tracer
-spec get_tracer() -> opentelemetry:tracer().
get_tracer() ->
    gen_server:call(?SERVER, get_tracer, 5000).

%% @doc Get meter
-spec get_meter() -> opentelemetry:meter().
get_meter() ->
    gen_server:call(?SERVER, get_meter, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the OTEL integration
-spec init(term()) -> {ok, #otel_state{}} | {stop, term()}.
init(_Args) ->
    %% Create OTEL span for initialization
    erlmcp_otel:with_span("cli.otel.init",
                          #{<<"module">> => atom_to_binary(?MODULE, utf8)},
                          fun() ->
                             %% Initialize OpenTelemetry
                             case init_opentelemetry() of
                                 ok ->
                                     %% Load configuration
                                     Config = load_config(),

                                     %% Create resource
                                     Resource = create_resource(Config),

                                     %% Start exporters
                                     Exporters = start_exporters(Config, Resource),

                                     %% Get tracer and meter
                                     Tracer = opentelemetry:start_tracer("erlmcp_cli", Config),
                                     Meter = opentelemetry:start_meter("erlmcp_cli", Config),

                                     %% Initialize metrics registry
                                     Metrics = init_metrics(),

                                     State =
                                         #otel_state{tracer = Tracer,
                                                     meter = Meter,
                                                     config = Config,
                                                     spans = #{},
                                                     exporters = Exporters,
                                                     metrics = Metrics,
                                                     resource = Resource,
                                                     service_name = <<"erlmcp_cli">>},

                                     %% Initialize span cleanup timer
                                     erlang:send_after(30000, self(), cleanup_spans),

                                     erlmcp_metrics:record("cli.otel.initialized", 1),
                                     {ok, State};
                                 {error, Reason} ->
                                     {stop, Reason}
                             end
                          end).

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #otel_state{}) -> {reply, term(), #otel_state{}}.
handle_call(get_tracer, _From, State) ->
    {reply, State#otel_state.tracer, State};
handle_call(get_meter, _From, State) ->
    {reply, State#otel_state.meter, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #otel_state{}) -> {noreply, #otel_state{}}.
handle_cast({record_metric, Name, Value, Attributes}, State) ->
    %% Record metric with OTEL
    try
        Meter = State#otel_state.meter,
        opentelemetry:meter_create_counter(Meter, Name, #{description => "CLI metric"}),
        opentelemetry:meter_add_counter(Meter, Name, Value, Attributes)
    catch
        Error:Reason ->
            lager:warning("Failed to record metric ~p: ~p:~p", [Name, Error, Reason])
    end,

    %% Update local metrics registry
    Metrics = update_metric_registry(State#otel_state.metrics, Name, Value, Attributes),

    {noreply, State#otel_state{metrics = Metrics}};
handle_cast({record_error, SpanCtx, Error}, State) ->
    %% Record error on span
    try
        case SpanCtx of
            undefined ->
                lager:warning("No span context for error recording");
            _ ->
                otel_span:set_attribute(SpanCtx,
                                        <<"error.type">>,
                                        atom_to_binary(element(1, Error), utf8)),
                otel_span:set_attribute(SpanCtx,
                                        <<"error.message">>,
                                        format_error(element(2, Error))),
                otel_span:set_status(SpanCtx, opentelemetry:status(error, format_error(Error))),
                otel_span:add_event(SpanCtx,
                                    <<"error">>,
                                    #{<<"error.stack">> => format_stacktrace(element(3, Error))})
        end
    catch
        Error1:Reason1 ->
            lager:warning("Failed to record error: ~p:~p", [Error1, Reason1])
    end,

    {noreply, State};
handle_cast({record_event, SpanCtx, Name, Attributes}, State) ->
    %% Record event on span
    try
        otel_span:add_event(SpanCtx, Name, Attributes)
    catch
        Error:Reason ->
            lager:warning("Failed to record event ~p: ~p:~p", [Name, Error, Reason])
    end,

    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info( term( ) , #otel_state{ } ) -> { noreply , #otel_state{ } } } .

handle_info(cleanup_spans, State) ->
    %% Clean up completed spans
    Now = erlang:system_time(millisecond),
    ActiveSpans =
        maps:filter(fun(_SpanId, SpanCtx) ->
                       case otel_span:is_active(SpanCtx) of
                           true ->
                               true;
                           false ->
                               false
                       end
                    end,
                    State#otel_state.spans),

    %% Schedule next cleanup
    erlang:send_after(30000, self(), cleanup_spans),

    {noreply, State#otel_state{spans = ActiveSpans}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), #otel_state{}) -> ok.
terminate(_Reason, State) ->
    %% Create OTEL span for termination
    erlmcp_otel:with_span("cli.otel.terminate",
                          #{<<"service_name">> => State#otel_state.service_name},
                          fun() ->
                             %% End all active spans
                             lists:foreach(fun({_SpanId, SpanCtx}) -> otel_span:end_span(SpanCtx)
                                           end,
                                           maps:to_list(State#otel_state.spans)),

                             %% Shutdown exporters
                             lists:foreach(fun(Pid) ->
                                              case erlang:is_process_alive(Pid) of
                                                  true ->
                                                      erlang:exit(Pid, normal);
                                                  false ->
                                                      ok
                                              end
                                           end,
                                           State#otel_state.exporters),

                             %% Record final metrics
                             erlmcp_metrics:record("cli.otel.terminated", 1),

                             ok
                          end).

%% @doc Handle code changes
-spec code_change(term(), #otel_state{}, term()) -> {ok, #otel_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Initialize OpenTelemetry
-spec init_opentelemetry() -> ok | {error, term()}.
init_opentelemetry() ->
    try
        %% Initialize OpenTelemetry SDK
        ok = opentelemetry:start(),

        %% Configure exporters
        ok = configure_exporters(),

        %% Initialize context propagation
        ok = init_context_propagation(),

        ok
    catch
        Error:Reason ->
            {error, {otel_init_failed, Error, Reason}}
    end.

%% @doc Configure exporters
-spec configure_exporters() -> ok.
configure_exporters() ->
    try
        %% Configure OTLP exporter
        Config =
            otel_exporter_otlp:configure(#{<<"endpoint">> => <<"http://localhost:4317">>,
                                           <<"headers">> => #{}}),

        ok = opentelemetry:add_exporter(otel_exporter_otlp, Config),

        ok
    catch
        Error:Reason ->
            lager:warning("Failed to configure OTEL exporters: ~p:~p", [Error, Reason]),
            ok
    end.

%% @doc Initialize context propagation
-spec init_context_propagation() -> ok.
init_context_propagation() ->
    try
        %% Register text map propagator
        ok = otel_propagator:text_map(),

        %% Register trace context propagator
        ok = otel_propagator:trace_context(),

        ok
    catch
        Error:Reason ->
            lager:warning("Failed to init context propagation: ~p:~p", [Error, Reason]),
            ok
    end.

%% @doc Load configuration
-spec load_config() -> map().
load_config() ->
    %% Start with defaults
    DefaultConfig = ?DEFAULT_OTEL_CONFIG,

    %% Merge with environment variables
    EnvConfig = merge_env_config(DefaultConfig),

    %% Merge with file configuration
    FileConfig = load_file_config(),

    %% Final merge
    maps:merge(
        maps:merge(DefaultConfig, EnvConfig), FileConfig).

%% @doc Merge environment configuration
-spec merge_env_config(map()) -> map().
merge_env_config(Config) ->
    %% Environment variable mappings
    EnvMappings =
        #{<<"OTEL_EXPORTER_OTLP_ENDPOINT">> => <<"endpoint">>,
          <<"OTEL_EXPORTER_OTLP_HEADERS">> => <<"headers">>,
          <<"OTEL_SERVICE_NAME">> => <<"resource.service.name">>,
          <<"OTEL_TRACES_SAMPLER">> => <<"sampling.type">>,
          <<"OTEL_TRACES_SAMPLER_ARG">> => <<"sampling.trace_ratio">>,
          <<"OTEL_EXPORTER_OTLP_BATCH_SIZE">> => <<"batch_size">>,
          <<"OTEL_EXPORTER_OTLP_BATCH_TIMEOUT">> => <<"batch_timeout">>},

    lists:foldl(fun({EnvVar, ConfigKey}, Acc) ->
                   case os:getenv(binary_to_list(EnvVar)) of
                       false ->
                           Acc;
                       Value ->
                           set_nested_config(ConfigKey, Value, Acc)
                   end
                end,
                Config,
                EnvMappings).

%% @doc Load file configuration
-spec load_file_config() -> map().
load_file_config() ->
    try
        ConfigFile =
            filename:join(
                os:getenv("HOME", "."), ".erlmcp_otel.json"),
        case file:read_file(ConfigFile) of
            {ok, Content} ->
                jsx:decode(Content, [{labels, binary}, return_maps]);
            {error, enoent} ->
                #{};
            {error, Reason} ->
                lager:warning("Failed to read OTEL config file: ~p", [Reason]),
                #{}
        end
    catch
        Error:Reason ->
            lager:warning("Failed to parse OTEL config file: ~p:~p", [Error, Reason]),
            #{}
    end.

%% @doc Create resource
-spec create_resource(map()) -> map().
create_resource(Config) ->
    %% Start with default resource
    DefaultResource = maps:get(<<"resource">>, Config, #{}),

    %% Add host information
    HostInfo =
        #{<<"host.name">> => get_hostname(),
          <<"host.arch">> => get_arch(),
          <<"host.os">> => get_os()},

    %% Add process information
    ProcessInfo =
        #{<<"process.pid">> => os:getpid(),
          <<"process.executable.name">> => <<"erlmcp_cli">>,
          <<"process.executable.path">> => get_executable_path()},

    %% Merge all resources
    maps:merge(
        maps:merge(DefaultResource, HostInfo), ProcessInfo).

%% @doc Start exporters
-spec start_exporters(map(), map()) -> list().
start_exporters(Config, Resource) ->
    Exporters = maps:get(<<"exporter">>, Config, <<"otlp">>),

    case Exporters of
        <<"otlp">> ->
            start_otlp_exporter(Config, Resource);
        <<"console">> ->
            start_console_exporter(Config, Resource);
        <<"none">> ->
            [];
        _ ->
            lager:warning("Unknown exporter type: ~p", [Exporters]),
            []
    end.

%% @doc Start OTLP exporter
-spec start_otlp_exporter(map(), map()) -> list().
start_otlp_exporter(Config, Resource) ->
    try
        %% Configure OTLP exporter
        OtlpConfig =
            #{<<"endpoint">> => maps:get(<<"endpoint">>, Config, <<"http://localhost:4317">>),
              <<"headers">> => maps:get(<<"headers">>, Config, #{}),
              <<"resource">> => Resource,
              <<"batch_size">> => maps:get(<<"batch_size">>, Config, 512),
              <<"batch_timeout">> => maps:get(<<"batch_timeout">>, Config, 5000)},

        %% Start exporter
        {ok, Pid} = otel_exporter_otlp:start_link(OtlpConfig),

        [Pid]
    catch
        Error:Reason ->
            lager:warning("Failed to start OTLP exporter: ~p:~p", [Error, Reason]),
            []
    end.

%% @doc Start console exporter
-spec start_console_exporter(map(), map()) -> list().
start_console_exporter(_Config, _Resource) ->
    try
        %% Start console exporter for debugging
        {ok, Pid} = otel_exporter_console:start_link(),
        [Pid]
    catch
        Error:Reason ->
            lager:warning("Failed to start console exporter: ~p:~p", [Error, Reason]),
            []
    end.

%% @doc Initialize metrics registry
-spec init_metrics() -> map().
init_metrics() ->
    #{"spans.active" => 0,
      "spans.completed" => 0,
      "spans.errors" => 0,
      "metrics.recorded" => 0,
      "events.recorded" => 0}.

%% @doc Start span
-spec start_span(binary(), map(), map(), term()) -> term().
start_span(Name, Attributes, Links, ParentSpanCtx) ->
    try
        %% Create span context
        SpanCtx =
            otel_span:start_span(get_tracer(),
                                 Name,
                                 #{attributes => Attributes,
                                   links => Links,
                                   parent => ParentSpanCtx}),

        %% Add to active spans
        SpanId = generate_span_id(),
        erlang:put({active_span, SpanId}, SpanCtx),

        SpanCtx
    catch
        Error:Reason ->
            lager:warning("Failed to start span ~p: ~p:~p", [Name, Error, Reason]),
            undefined
    end.

%% @doc End span
-spec end_span(term(), term()) -> ok.
end_span(SpanCtx, Status) ->
    try
        case Status of
            ok ->
                otel_span:end_span(SpanCtx);
            {Class, Reason, Stacktrace} ->
                otel_span:set_status(SpanCtx,
                                     opentelemetry:status(error, format_error({Class, Reason}))),
                otel_span:set_attribute(SpanCtx, <<"error.stack">>, format_stacktrace(Stacktrace)),
                otel_span:end_span(SpanCtx)
        end
    catch
        Error:Reason ->
            lager:warning("Failed to end span: ~p:~p", [Error, Reason])
    end.

%% @doc Link spans
-spec link_spans(term(), term()) -> term().
link_spans(ParentSpanCtx, ChildSpanCtx) ->
    try
        otel_span:add_link(ChildSpanCtx, ParentSpanCtx)
    catch
        Error:Reason ->
            lager:warning("Failed to link spans: ~p:~p", [Error, Reason]),
            ChildSpanCtx
    end.

%% @doc Extract trace context
-spec extract_trace_context(map()) -> map().
extract_trace_context(Headers) ->
    try
        %% Extract W3C trace context
        TraceContext =
            #{<<"traceparent">> => maps:get(<<"traceparent">>, Headers, undefined),
              <<"tracestate">> => maps:get(<<"tracestate">>, Headers, undefined)},

        %% Convert to OTEL context
        otel_context:new(TraceContext)
    catch
        Error:Reason ->
            lager:warning("Failed to extract trace context: ~p:~p", [Error, Reason]),
            #{}
    end.

%% @doc Update metric registry
-spec update_metric_registry(map(), binary(), number(), map()) -> map().
update_metric_registry(Metrics, Name, Value, Attributes) ->
    %% Update total metrics count
    UpdatedMetrics = maps:update_with("metrics.recorded", fun(V) -> V + 1 end, Metrics),

    %% Update specific metric
    case maps:find(Name, UpdatedMetrics) of
        {ok, CurrentValue} ->
            maps:put(Name, CurrentValue + Value, UpdatedMetrics);
        error ->
            maps:put(Name, Value, UpdatedMetrics)
    end.

%% @doc Set nested configuration value
-spec set_nested_config(binary(), term(), map()) -> map().
set_nested_config(Key, Value, Config) ->
    case binary:split(Key, <<".">>, [global]) of
        [Key] ->
            maps:put(Key, Value, Config);
        Parts ->
            set_nested_config_parts(Parts, Value, Config)
    end.

%% @doc Set nested configuration parts
-spec set_nested_config_parts([binary()], term(), map()) -> map().
set_nested_config_parts([Part], Value, Map) ->
    maps:put(Part, Value, Map);
set_nested_config_parts([Part | Rest], Value, Map) ->
    SubMap =
        case maps:get(Part, Map, undefined) of
            undefined ->
                #{};
            Existing when is_map(Existing) ->
                Existing
        end,
    NewSubMap = set_nested_config_parts(Rest, Value, SubMap),
    maps:put(Part, NewSubMap, Map).

%% @doc Generate span ID
-spec generate_span_id() -> binary().
generate_span_id() ->
    Id = crypto:strong_rand_bytes(8),
    base64:encode(Id).

%% @doc Format error
-spec format_error(term()) -> binary().
format_error(Error) ->
    list_to_binary(io_lib:format("~p", [Error])).

%% @doc Format stacktrace
-spec format_stacktrace(list()) -> binary().
format_stacktrace(Stacktrace) ->
    list_to_binary(io_lib:format("~p", [Stacktrace])).

%% @doc Get hostname
-spec get_hostname() -> binary().
get_hostname() ->
    {ok, Hostname} = inet:gethostname(),
    list_to_binary(Hostname).

%% @doc Get architecture
-spec get_arch() -> binary().
get_arch() ->
    list_to_binary(string:to_lower(
                       erlang:system_info(system_architecture))).

%% @doc Get OS
-spec get_os() -> binary().
get_os() ->
    list_to_binary(string:to_lower(
                       os:type())).

%% @doc Get executable path
-spec get_executable_path() -> binary().
get_executable_path() ->
    case escript:script_name() of
        non_existing ->
            list_to_binary(code:where_is_file("erlmcp_cli"));
        Path ->
            list_to_binary(Path)
    end.
