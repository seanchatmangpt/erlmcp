%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_logger - OTP 26-28 Structured Logger for MCP
%%%
%%% This module provides structured logging capabilities using OTP 26-28
%%% logger improvements for Model Context Protocol operations.
%%%
%%% OTP 26-28 Features Used:
%%% - OTP 26: Logger domain support, metadata improvements
%%% - OTP 27: Logger formatter enhancements
%%% - OTP 28: Log compression, structured metadata
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
%%% - client_pid: Client process PID
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_logger).

%% API
-export([configure_logger/0,
         configure_handler/2,
         log_tool_call/2,
         log_tool_result/2,
         log_session_event/2,
         log_request_event/2,
         log_error/2,
         set_log_level/1,
         get_log_level/0,
         add_metadata/2,
         get_metadata/0]).

%% Types
-type log_level() :: debug | info | notice | warning | error | critical | alert | emergency.
-type log_domain() :: [atom()].
-type log_metadata() :: #{session_id => binary(),
                          tool_id => binary(),
                          request_id => binary() | integer(),
                          client_pid => pid(),
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

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Configure the primary logger for MCP operations.
%% Sets up logger with appropriate handlers, formatters, and filters.
-spec configure_logger() -> ok.
configure_logger() ->
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

    % Add MCP-specific metadata extractors
    ok = configure_metadata_extractors(),

    logger:info(#{domain => [mcp, logger],
                 message => "MCP logger configured",
                 level => LogLevel}),
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
    Filters = [
        {mcp_domain_filter, {fun mcp_domain_filter/2, #{}}}
    ],
    ok = logger:add_handler_filter(HandlerId, mcp_domain_filter,
                                    {fun mcp_domain_filter/2, #{}}),

    ok.

%% @doc Log a tool call event with structured data.
%% Domain: [mcp, tools]
%% Metadata includes tool name and parameters.
-spec log_tool_call(binary(), map()) -> ok.
log_tool_call(ToolName, Params) when is_binary(ToolName), is_map(Params) ->
    logger:info(#{domain => [mcp, tools],
                  message => <<"Tool call">>,
                  tool_name => ToolName,
                  params => Params,
                  timestamp => erlang:system_time(millisecond)}),
    ok.

%% @doc Log a tool result event with duration and outcome.
%% Domain: [mcp, tools]
%% Metadata includes tool name, result, and execution time.
-spec log_tool_result(binary(), {ok, term()} | {error, term()} | term()) -> ok.
log_tool_result(ToolName, Result) ->
    logger:info(#{domain => [mcp, tools],
                  message => <<"Tool result">>,
                  tool_name => ToolName,
                  result => format_result(Result),
                  timestamp => erlang:system_time(millisecond)}),
    ok.

%% @doc Log a tool result event with explicit duration.
-spec log_tool_result(binary(), {ok, term()} | {error, term()} | term(), integer()) -> ok.
log_tool_result(ToolName, Result, DurationMs) when is_integer(DurationMs) ->
    logger:info(#{domain => [mcp, tools],
                  message => <<"Tool result">>,
                  tool_name => ToolName,
                  result => format_result(Result),
                  duration_ms => DurationMs,
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

    logger:log(Level, #{domain => [mcp, sessions],
                        message => format_session_event(EventType),
                        session_id => SessionId,
                        reason => maps:get(reason, Event, undefined),
                        metadata => Metadata,
                        timestamp => erlang:system_time(millisecond)}),
    ok.

%% @doc Log a request/response event.
%% Domain: [mcp, requests]
-spec log_request_event(request_event(), log_metadata()) -> ok.
log_request_event(#{method := Method, request_id := RequestId, direction := Direction}, Metadata) ->
    logger:debug(#{domain => [mcp, requests],
                   message => format_request_direction(Direction),
                   method => Method,
                   request_id => RequestId,
                   metadata => Metadata,
                   timestamp => erlang:system_time(millisecond)}),
    ok.

%% @doc Log an error event with context.
%% Domain: [mcp, errors]
-spec log_error(error_event(), log_metadata()) -> ok.
log_error(#{category := Category, reason := Reason} = Event, Metadata) ->
    logger:error(#{domain => [mcp, errors],
                   message => <<"Error occurred">>,
                   category => Category,
                   reason => Reason,
                   stacktrace => maps:get(stacktrace, Event, undefined),
                   metadata => Metadata,
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

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Get log level from environment configuration.
-spec get_env_log_level() -> log_level().
get_env_log_level() ->
    case application:get_env(erlmcp_observability, log_level) of
        {ok, Level} when Level =:= debug; Level =:= info;
                        Level =:= notice; Level =:= warning;
                        Level =:= error; Level =:= critical;
                        Level =:= alert; Level =:= emergency ->
            Level;
        _ ->
            % Check environment variable
            case os:getenv("ERLMCP_LOG_LEVEL") of
                "debug" -> debug;
                "info" -> info;
                "notice" -> notice;
                "warning" -> warning;
                "error" -> error;
                "critical" -> critical;
                "alert" -> alert;
                "emergency" -> emergency;
                _ -> info  % Default
            end
    end.

%% @doc Configure OTP 28 metadata extractors for MCP context.
-spec configure_metadata_extractors() -> ok.
configure_metadata_extractors() ->
    % OTP 28: Add metadata extractors for automatic enrichment
    % These extractors run on every log event to add context

    Extractors = [
        {session_id_extractor, fun session_id_extractor/1},
        {tool_id_extractor, fun tool_id_extractor/1},
        {request_id_extractor, fun request_id_extractor/1},
        {client_pid_extractor, fun client_pid_extractor/1}
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
