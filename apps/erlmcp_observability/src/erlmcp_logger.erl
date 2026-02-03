%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_logger - OTP 26-28 Structured Logger for MCP
%%%
%%% This module provides structured logging capabilities using OTP 26-28
%%% logger improvements for Model Context Protocol operations.
%%%
%%% OTP 26-28 Features Used:
%%% - OTP 26: Logger domain support, metadata improvements
%%% - OTP 27: Logger formatter enhancements, native JSON module
%%% - OTP 28: Log compression, structured metadata
%%%
%%% Features:
%%% - JSON structured logging output for log aggregation (Loki, ELK)
%%% - Correlation ID tracking (trace_id, span_id, request_id)
%%% - Per-environment log levels
%%% - Request tracing across distributed systems
%%% - Automatic metadata propagation
%%%
%%% MCP Log Domains:
%%% - [mcp, tools]: Tool execution events
%%% - [mcp, sessions]: Session lifecycle events
%%% - [mcp, requests]: Request/response events
%%% - [mcp, errors]: Error and failure events
%%%
%%% Metadata Fields:
%%% - session_id: MCP session identifier
%%% - tool_id: Tool name/identifier
%%% - request_id: JSON-RPC request ID
%%% - trace_id: OpenTelemetry trace identifier
%%% - span_id: OpenTelemetry span identifier
%%% - correlation_id: Request correlation identifier
%%% - client_pid: Client process PID
%%% - component: Service component name
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_logger).

%% API
-export([configure_logger/0,
         configure_logger/1,
         configure_handler/2,
         configure_json_handler/2,
         log_tool_call/2,
         log_tool_result/2,
         log_session_event/2,
         log_request_event/2,
         log_error/2,
         set_log_level/1,
         get_log_level/0,
         add_metadata/2,
         get_metadata/0,
         with_correlation_id/2,
         new_correlation_id/0,
         get_correlation_id/0,
         set_trace_context/2,
         get_trace_context/0,
         get_environment/0,
         get_log_level_for_environment/1]).

%% Types
-type log_level() :: debug | info | notice | warning | error | critical | alert | emergency.
-type log_domain() :: [atom()].
-type log_metadata() :: #{session_id => binary(),
                          tool_id => binary(),
                          request_id => binary() | integer(),
                          trace_id => binary(),
                          span_id => binary(),
                          correlation_id => binary(),
                          client_pid => pid(),
                          component => binary(),
                          atom() => term()}.
-type tool_call_event() :: #{tool_name => binary(),
                             params => map(),
                             duration_ms => integer() | undefined}.
-type tool_result_event() :: #{tool_name => binary(),
                              result => term(),
                              duration_ms => integer()}.
-type session_event() :: #{event_type => session_start | session_end,
                          session_id => binary(),
                          reason => term() | undefined}.
-type request_event() :: #{method => binary(),
                          request_id => term(),
                          direction => request | response}.
-type error_event() :: #{category => atom(),
                        reason => term(),
                        stacktrace => list() | undefined}.
-type environment() :: development | testing | staging | production.
-type trace_context() :: #{trace_id => binary(),
                           span_id => binary()}.

%%%===================================================================
%%% Constants
%%%===================================================================

-define(DEFAULT_LOG_LEVEL, info).
-define(PROD_LOG_LEVEL, warning).
-define(DEV_LOG_LEVEL, debug).
-define(TEST_LOG_LEVEL, debug).
-define(STAGING_LOG_LEVEL, info).

-define(TRACE_ID_KEY, '$erlmcp_trace_id').
-define(SPAN_ID_KEY, '$erlmcp_span_id').
-define(CORRELATION_ID_KEY, '$erlmcp_correlation_id').

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Configure the primary logger for MCP operations.
%% Sets up logger with appropriate handlers, formatters, and filters.
%% Uses environment-specific log levels.
-spec configure_logger() -> ok.
configure_logger() ->
    configure_logger(#{}).

%% @doc Configure the primary logger with custom options.
%% Options:
%%   - format: json | text (default: text for console, json for file)
%%   - json_handler: Handler ID for JSON output (default: json_log)
%%   - enable_json: Boolean to enable JSON output (default: true)
-spec configure_logger(map()) -> ok.
configure_logger(Options) ->
    % Set primary log level based on environment
    LogLevel = get_env_log_level(),
    ok = logger:set_primary_config(level, LogLevel),

    % Add default handler if not exists
    case logger:get_handler_config(default) of
        {ok, _} ->
            ok;
        _ ->
            {ok, _} = logger:add_handler(default, logger_std_h, #{}),
            ok
    end,

    % Configure handler with MCP-specific formatter
    configure_handler(default, #{level => LogLevel}),

    % Configure JSON handler if enabled
    EnableJson = maps:get(enable_json, Options, true),
    JsonHandlerId = maps:get(json_handler, Options, json_log),
    case EnableJson of
        true ->
            configure_json_handler(JsonHandlerId, #{level => LogLevel});
        false ->
            ok
    end,

    % Add MCP-specific metadata extractors
    ok = configure_metadata_extractors(),

    logger:info(#{domain => [mcp, logger],
                  message => "MCP logger configured",
                  level => LogLevel,
                  environment => get_environment(),
                  json_enabled => EnableJson}),
    ok.

%% @doc Configure a specific logger handler with MCP formatters and filters.
-spec configure_handler(logger:handler_id(), map()) -> ok.
configure_handler(HandlerId, Config) ->
    Level = maps:get(level, Config, info),

    % OTP 27: Enhanced formatter with domain support
    FormatterConfig = #{
        template => [time, " ", level, " [",
                    domain, "] ",
                    message, "\n",
                    "  Metadata: ", metadata, "\n"],
        single_line => true,
        time_designator => $T,
        time_offset => "+00:00"
    },

    % Update handler configuration
    ok = logger:set_handler_config(HandlerId, formatter,
                                   {logger_formatter, FormatterConfig}),

    % Set handler level
    ok = logger:set_handler_config(HandlerId, level, Level),

    % Add MCP-specific filters
    ok = logger:add_handler_filter(HandlerId, mcp_domain_filter,
                                    {fun mcp_domain_filter/2, #{}}),

    ok.

%% @doc Configure a JSON log handler for structured logging output.
%% This handler outputs JSON-formatted logs suitable for aggregation
%% systems like Loki, ELK, or other JSON-based log processors.
-spec configure_json_handler(logger:handler_id(), map()) -> ok.
configure_json_handler(HandlerId, Config) ->
    Level = maps:get(level, Config, info),

    % Check if handler already exists
    case logger:get_handler_config(HandlerId) of
        {ok, _} ->
            % Remove existing handler to reconfigure
            logger:remove_handler(HandlerId);
        _ ->
            ok
    end,

    % Get log file path for JSON output
    LogFile = maps:get(file, Config, get_json_log_file()),

    % Add handler with JSON formatter
    FormatterConfig = #{
        time_offset => "+00:00",
        time_unit => millisecond,
        include_metadata => true,
        include_node => true,
        include_pid => true,
        pretty_print => maps:get(pretty_print, Config, false),
        field_map => #{
            timestamp => <<"@timestamp">>,
            level => <<"log.level">>,
            message => <<"message">>,
            node => <<"host.name">>,
            pid => <<"process.id">>,
            mfa => <<"code.namespace">>,
            line => <<"code.line">>,
            trace_id => <<"trace.id">>,
            span_id => <<"span.id">>,
            request_id => <<"request.id">>,
            session_id => <<"session.id">>,
            correlation_id => <<"correlation.id">>,
            domain => <<"log.domain">>,
            component => <<"service.name">>,
            metadata => <<"labels">>
        }
    },

    {ok, _} = logger:add_handler(HandlerId, logger_std_h, #{
        config => #{
            type => {file, LogFile},
            max_no_bytes => maps:get(max_no_bytes, Config, 104857600),  % 100MB
            max_no_files => maps:get(max_no_files, Config, 10),
            sync_mode_qlen => maps:get(sync_mode_qlen, Config, 100),
            drop_mode_qlen => maps:get(drop_mode_qlen, Config, 1000),
            flush_qlen => maps:get(flush_qlen, Config, 5000),
            compress_on_rotate => maps:get(compress_on_rotate, Config, true)
        },
        formatter => {erlmcp_json_log_formatter, FormatterConfig},
        level => Level,
        filters => [
            {mcp_domain_filter, {fun mcp_domain_filter/2, #{}}}
        ]
    }),

    logger:info(#{domain => [mcp, logger],
                  message => "JSON log handler configured",
                  handler => HandlerId,
                  log_file => LogFile,
                  level => Level}),
    ok.

%% @doc Log a tool call event with structured data.
%% Domain: [mcp, tools]
%% Metadata includes tool name and parameters.
%% Automatically includes trace context if available.
-spec log_tool_call(binary(), map()) -> ok.
log_tool_call(ToolName, Params) when is_binary(ToolName), is_map(Params) ->
    Metadata = enrich_with_trace_context(#{
        tool_name => ToolName,
        params => Params
    }),
    logger:info(#{domain => [mcp, tools],
                  message => <<"Tool call">>,
                  metadata => Metadata,
                  timestamp => erlang:system_time(millisecond)}),
    ok.

%% @doc Log a tool call event with custom correlation ID.
-spec log_tool_call(binary(), map(), binary()) -> ok.
log_tool_call(ToolName, Params, CorrelationId) when is_binary(ToolName), is_map(Params), is_binary(CorrelationId) ->
    Metadata = enrich_with_trace_context(#{
        tool_name => ToolName,
        params => Params,
        correlation_id => CorrelationId
    }),
    logger:info(#{domain => [mcp, tools],
                  message => <<"Tool call">>,
                  metadata => Metadata,
                  timestamp => erlang:system_time(millisecond)}),
    ok.

%% @doc Log a tool result event with duration and outcome.
%% Domain: [mcp, tools]
%% Metadata includes tool name, result, and execution time.
-spec log_tool_result(binary(), {ok, term()} | {error, term()} | term()) -> ok.
log_tool_result(ToolName, Result) ->
    Metadata = enrich_with_trace_context(#{
        tool_name => ToolName,
        result => format_result(Result)
    }),
    logger:info(#{domain => [mcp, tools],
                  message => <<"Tool result">>,
                  metadata => Metadata,
                  timestamp => erlang:system_time(millisecond)}),
    ok.

%% @doc Log a tool result event with explicit duration.
-spec log_tool_result(binary(), {ok, term()} | {error, term()} | term(), integer()) -> ok.
log_tool_result(ToolName, Result, DurationMs) when is_integer(DurationMs) ->
    Metadata = enrich_with_trace_context(#{
        tool_name => ToolName,
        result => format_result(Result),
        duration_ms => DurationMs
    }),
    logger:info(#{domain => [mcp, tools],
                  message => <<"Tool result">>,
                  metadata => Metadata,
                  timestamp => erlang:system_time(millisecond)}),
    ok.

%% @doc Log a tool result event with duration and correlation ID.
-spec log_tool_result(binary(), {ok, term()} | {error, term()} | term(), integer(), binary()) -> ok.
log_tool_result(ToolName, Result, DurationMs, CorrelationId) when is_integer(DurationMs), is_binary(CorrelationId) ->
    Metadata = enrich_with_trace_context(#{
        tool_name => ToolName,
        result => format_result(Result),
        duration_ms => DurationMs,
        correlation_id => CorrelationId
    }),
    logger:info(#{domain => [mcp, tools],
                  message => <<"Tool result">>,
                  metadata => Metadata,
                  timestamp => erlang:system_time(millisecond)}),
    ok.

%% @doc Log a session lifecycle event.
%% Domain: [mcp, sessions]
%% Events: session_start, session_end
-spec log_session_event(session_event(), log_metadata()) -> ok.
log_session_event(#{event_type := EventType, session_id := SessionId} = Event, Metadata) ->
    Level = case EventType of
        session_start -> info;
        session_end -> info
    end,

    EnrichedMetadata = enrich_with_trace_context(maps:merge(Metadata, #{
        session_id => SessionId,
        reason => maps:get(reason, Event, undefined)
    })),

    logger:log(Level, #{domain => [mcp, sessions],
                        message => format_session_event(EventType),
                        metadata => EnrichedMetadata,
                        timestamp => erlang:system_time(millisecond)}),
    ok.

%% @doc Log a request/response event.
%% Domain: [mcp, requests]
-spec log_request_event(request_event(), log_metadata()) -> ok.
log_request_event(#{method := Method, request_id := RequestId, direction := Direction}, Metadata) ->
    EnrichedMetadata = enrich_with_trace_context(maps:merge(Metadata, #{
        request_id => RequestId,
        method => Method,
        direction => Direction
    })),

    logger:debug(#{domain => [mcp, requests],
                   message => format_request_direction(Direction),
                   metadata => EnrichedMetadata,
                   timestamp => erlang:system_time(millisecond)}),
    ok.

%% @doc Log an error event with context.
%% Domain: [mcp, errors]
-spec log_error(error_event(), log_metadata()) -> ok.
log_error(#{category := Category, reason := Reason} = Event, Metadata) ->
    EnrichedMetadata = enrich_with_trace_context(maps:merge(Metadata, #{
        category => Category,
        reason => Reason,
        stacktrace => maps:get(stacktrace, Event, undefined)
    })),

    logger:error(#{domain => [mcp, errors],
                   message => <<"Error occurred">>,
                   metadata => EnrichedMetadata,
                   timestamp => erlang:system_time(millisecond)}),
    ok.

%% @doc Set the primary logger level.
-spec set_log_level(log_level()) -> ok.
set_log_level(Level) when Level =:= debug; Level =:= info;
                         Level =:= notice; Level =:= warning;
                         Level =:= error; Level =:= critical;
                         Level =:= alert; Level =:= emergency ->
    ok = logger:set_primary_config(level, Level),

    % Update all handlers
    Handlers = logger:get_handler_ids(),
    lists:foreach(fun(HandlerId) ->
        logger:set_handler_config(HandlerId, level, Level)
    end, Handlers),

    logger:info(#{domain => [mcp, logger],
                 message => "Log level changed",
                 level => Level}),
    ok.

%% @doc Get the current primary logger level.
-spec get_log_level() -> log_level().
get_log_level() ->
    {ok, Level} = logger:get_primary_config(level),
    Level.

%% @doc Add process-specific metadata that will be included in all log events.
-spec add_metadata(atom(), term()) -> ok.
add_metadata(Key, Value) when is_atom(Key) ->
    logger:update_process_metadata(#{Key => Value}),
    ok.

%% @doc Get current process metadata.
-spec get_metadata() -> logger:metadata().
get_metadata() ->
    logger:get_process_metadata().

%% @doc Execute a function with a correlation ID context.
%% The correlation ID will be automatically included in all log events.
-spec with_correlation_id(binary(), fun(() -> term())) -> term().
with_correlation_id(CorrelationId, Fun) when is_binary(CorrelationId), is_function(Fun, 0) ->
    put(?CORRELATION_ID_KEY, CorrelationId),
    try
        Fun()
    after
        erase(?CORRELATION_ID_KEY)
    end.

%% @doc Generate a new correlation ID.
%% Uses crypto:strong_rand_bytes for uniqueness.
-spec new_correlation_id() -> binary().
new_correlation_id() ->
    Id = crypto:strong_rand_bytes(16),
    binary:encode_hex(Id).

%% @doc Get the current correlation ID from process dictionary.
-spec get_correlation_id() -> binary() | undefined.
get_correlation_id() ->
    get(?CORRELATION_ID_KEY).

%% @doc Set the OpenTelemetry trace context for the current process.
%% This context will be included in all log events for correlation.
-spec set_trace_context(trace_context(), fun(() -> term())) -> term().
set_trace_context(#{trace_id := TraceId, span_id := SpanId}, Fun) when is_binary(TraceId), is_binary(SpanId), is_function(Fun, 0) ->
    put(?TRACE_ID_KEY, TraceId),
    put(?SPAN_ID_KEY, SpanId),
    try
        Fun()
    after
        erase(?TRACE_ID_KEY),
        erase(?SPAN_ID_KEY)
    end.

%% @doc Set just the trace ID (for use without span).
-spec set_trace_id(binary(), fun(() -> term())) -> term().
set_trace_id(TraceId, Fun) when is_binary(TraceId), is_function(Fun, 0) ->
    put(?TRACE_ID_KEY, TraceId),
    try
        Fun()
    after
        erase(?TRACE_ID_KEY)
    end.

%% @doc Get the current trace context from process dictionary.
-spec get_trace_context() -> trace_context() | undefined.
get_trace_context() ->
    TraceId = get(?TRACE_ID_KEY),
    SpanId = get(?SPAN_ID_KEY),
    case {TraceId, SpanId} of
        {undefined, undefined} -> undefined;
        {T, S} -> #{trace_id => T, span_id => S}
    end.

%% @doc Get the current environment.
%% Returns development, testing, staging, or production based on configuration.
-spec get_environment() -> environment().
get_environment() ->
    case application:get_env(erlmcp_observability, environment) of
        {ok, Env} when Env =:= development; Env =:= testing; Env =:= staging; Env =:= production ->
            Env;
        _ ->
            case os:getenv("ERLMCP_ENV") of
                "development" -> development;
                "dev" -> development;
                "testing" -> testing;
                "test" -> testing;
                "staging" -> staging;
                "stage" -> staging;
                "production" -> production;
                "prod" -> production;
                _ -> development  % Default
            end
    end.

%% @doc Get the appropriate log level for a given environment.
-spec get_log_level_for_environment(environment()) -> log_level().
get_log_level_for_environment(development) -> ?DEV_LOG_LEVEL;
get_log_level_for_environment(testing) -> ?TEST_LOG_LEVEL;
get_log_level_for_environment(staging) -> ?STAGING_LOG_LEVEL;
get_log_level_for_environment(production) -> ?PROD_LOG_LEVEL.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Get log level from environment configuration.
%% Uses environment-aware defaults if not explicitly configured.
-spec get_env_log_level() -> log_level().
get_env_log_level() ->
    case application:get_env(erlmcp_observability, log_level) of
        {ok, Level} when Level =:= debug; Level =:= info;
                        Level =:= notice; Level =:= warning;
                        Level =:= error; Level =:= critical;
                        Level =:= alert; Level =:= emergency ->
            Level;
        _ ->
            % Check environment variable first
            case os:getenv("ERLMCP_LOG_LEVEL") of
                "debug" -> debug;
                "info" -> info;
                "notice" -> notice;
                "warning" -> warning;
                "error" -> error;
                "critical" -> critical;
                "alert" -> alert;
                "emergency" -> emergency;
                _ ->
                    % Fall back to environment-based default
                    get_log_level_for_environment(get_environment())
            end
    end.

%% @doc Get the JSON log file path based on environment.
-spec get_json_log_file() -> file:filename_all().
get_json_log_file() ->
    Env = get_environment(),
    BaseDir = case os:getenv("ERLMCP_LOG_DIR") of
        false -> "log";
        Dir -> Dir
    end,
    filename:join([BaseDir, atom_to_list(Env), "erlmcp.json.log"]).

%% @doc Enrich metadata with trace context from process dictionary.
-spec enrich_with_trace_context(map()) -> map().
enrich_with_trace_context(Metadata) ->
    TraceId = get(?TRACE_ID_KEY),
    SpanId = get(?SPAN_ID_KEY),
    CorrelationId = get(?CORRELATION_ID_KEY),

    Metadata0 = case TraceId of
        undefined -> Metadata;
        _ -> maps:put(trace_id, TraceId, Metadata)
    end,
    Metadata1 = case SpanId of
        undefined -> Metadata0;
        _ -> maps:put(span_id, SpanId, Metadata0)
    end,
    Metadata2 = case CorrelationId of
        undefined -> Metadata1;
        _ -> maps:put(correlation_id, CorrelationId, Metadata1)
    end,

    % Add environment
    maps:put(environment, get_environment(), Metadata2).

%% @doc Configure OTP 28 metadata extractors for MCP context.
-spec configure_metadata_extractors() -> ok.
configure_metadata_extractors() ->
    % OTP 28: Add metadata extractors for automatic enrichment
    % These extractors run on every log event to add context

    Extractors = [
        {session_id_extractor, fun session_id_extractor/1},
        {tool_id_extractor, fun tool_id_extractor/1},
        {request_id_extractor, fun request_id_extractor/1},
        {client_pid_extractor, fun client_pid_extractor/1},
        {trace_id_extractor, fun trace_id_extractor/1},
        {span_id_extractor, fun span_id_extractor/1},
        {correlation_id_extractor, fun correlation_id_extractor/1},
        {environment_extractor, fun environment_extractor/1},
        {component_extractor, fun component_extractor/1}
    ],

    % Register extractors with logger (OTP 28 feature)
    lists:foreach(fun({Name, ExtractorFun}) ->
        case logger:add_primary_metadata(Name, ExtractorFun) of
            ok -> ok;
            {error, exists} -> ok;  % Already added
            _ -> ok
        end
    end, Extractors),

    ok.

%% @doc Filter log events based on MCP domain.
-spec mcp_domain_filter(logger:log_event(), map()) -> logger:filter_return().
mcp_domain_filter(#{meta := #{domain := Domain}}, _Config) when is_list(Domain) ->
    % Only allow MCP domain events
    case lists_prefix([mcp], Domain) of
        true -> ignore;
        false -> stop
    end;
mcp_domain_filter(_Event, _Config) ->
    % Allow non-domain events
    ignore.

%% @doc Extract session_id from process metadata or event metadata.
-spec session_id_extractor(logger:log_event()) -> term().
session_id_extractor(#{meta := #{session_id := SessionId}}) ->
    SessionId;
session_id_extractor(_Event) ->
    undefined.

%% @doc Extract tool_id from process metadata or event metadata.
-spec tool_id_extractor(logger:log_event()) -> term().
tool_id_extractor(#{meta := #{tool_id := ToolId}}) ->
    ToolId;
tool_id_extractor(_Event) ->
    undefined.

%% @doc Extract request_id from process metadata or event metadata.
-spec request_id_extractor(logger:log_event()) -> term().
request_id_extractor(#{meta := #{request_id := RequestId}}) ->
    RequestId;
request_id_extractor(_Event) ->
    undefined.

%% @doc Extract client_pid from process metadata or event metadata.
-spec client_pid_extractor(logger:log_event()) -> term().
client_pid_extractor(#{meta := #{client_pid := ClientPid}}) ->
    ClientPid;
client_pid_extractor(_Event) ->
    undefined.

%% @doc Extract trace_id from process dictionary or event metadata.
-spec trace_id_extractor(logger:log_event()) -> term().
trace_id_extractor(_Event) ->
    get(?TRACE_ID_KEY).

%% @doc Extract span_id from process dictionary or event metadata.
-spec span_id_extractor(logger:log_event()) -> term().
span_id_extractor(_Event) ->
    get(?SPAN_ID_KEY).

%% @doc Extract correlation_id from process dictionary or event metadata.
-spec correlation_id_extractor(logger:log_event()) -> term().
correlation_id_extractor(_Event) ->
    get(?CORRELATION_ID_KEY).

%% @doc Extract environment from application configuration.
-spec environment_extractor(logger:log_event()) -> term().
environment_extractor(_Event) ->
    get_environment().

%% @doc Extract component name (service name).
-spec component_extractor(logger:log_event()) -> term().
component_extractor(#{meta := #{component := Component}}) ->
    Component;
component_extractor(_Event) ->
    case application:get_application() of
        {ok, App} -> atom_to_binary(App);
        undefined -> <<"erlmcp">>
    end.

%% @doc Format result for logging.
-spec format_result(term()) -> binary().
format_result({ok, Data}) ->
    iolist_to_binary(["OK: ", format_data(Data)]);
format_result({error, Reason}) ->
    iolist_to_binary(["ERROR: ", format_data(Reason)]);
format_result(ok) ->
    <<"OK">>;
format_result({error, Reason, _Stack}) ->
    iolist_to_binary(["ERROR: ", format_data(Reason)]);
format_result(Result) ->
    format_data(Result).

%% @doc Format arbitrary data for logging.
-spec format_data(term()) -> binary().
format_data(Data) when is_binary(Data) ->
    Data;
format_data(Data) when is_atom(Data) ->
    atom_to_binary(Data, utf8);
format_data(Data) when is_integer(Data); is_float(Data) ->
    iolist_to_binary(io_lib:format("~p", [Data]));
format_data(Data) when is_map(Data); is_list(Data) ->
    try
        iolist_to_binary(io_lib:format("~p", [Data]))
    catch
        _:_ -> <<"[complex data]">>
    end;
format_data(Data) ->
    iolist_to_binary(io_lib:format("~p", [Data])).

%% @doc Format session event type to human-readable message.
-spec format_session_event(session_start | session_end) -> binary().
format_session_event(session_start) ->
    <<"Session started">>;
format_session_event(session_end) ->
    <<"Session terminated">>.

%% @doc Format request direction to human-readable message.
-spec format_request_direction(request | response) -> binary().
format_request_direction(request) ->
    <<"Request received">>;
format_request_direction(response) ->
    <<"Response sent">>.

%% @doc Check if a list starts with a given prefix.
-spec lists_prefix(list(), list()) -> boolean().
lists_prefix([], _List) ->
    true;
lists_prefix(_Prefix, []) ->
    false;
lists_prefix([H|T1], [H|T2]) ->
    lists_prefix(T1, T2);
lists_prefix(_, _) ->
    false.
