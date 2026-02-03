%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_json_log_formatter - Structured JSON Log Formatter
%%%
%%% This module provides a JSON formatter for OTP logger that outputs
%%% structured logs suitable for log aggregation systems like Loki, ELK,
%%% and other JSON-based log processors.
%%%
%%% Features:
%%% - JSON structured output
%%% - Correlation ID tracking (trace_id, span_id)
%%% - Request ID propagation
%%% - Environment-aware metadata
%%% - ISO 8601 timestamp formatting
%%% - Proper escaping of special characters
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_json_log_formatter).

-behaviour(logger_formatter).

%% logger_formatter callbacks
-export([format/2, check_config/1]).

%% Types
-type config() :: #{template => logger:formatter_template(),
                    single_line => boolean(),
                    time_offset => integer() | string(),
                    time_designator => byte(),
                    time_unit => nanosecond | microsecond | millisecond | second,
                    include_metadata => boolean(),
                    include_node => boolean(),
                    include_pid => boolean(),
                    pretty_print => boolean(),
                    field_map => map()}.

%% Use logger:metadata() instead of custom metadata type
-type log_metadata() :: logger:metadata().

-define(DEFAULT_TIME_OFFSET, "+00:00").
-define(DEFAULT_TIME_DESIGNATOR, $T).
-define(DEFAULT_TIME_UNIT, millisecond).

%%====================================================================
%% logger_formatter Callbacks
%%====================================================================

%% @doc Format a log event as JSON
-spec format(logger:log_event(), config()) -> iolist().
format(#{msg := Msg, meta := Meta} = Event, Config) ->
    %% Extract timestamp
    Time = maps:get(time, Event, erlang:system_time(?DEFAULT_TIME_UNIT)),

    %% Build JSON object
    JsonObj = build_json_object(Time, Msg, Meta, Config),

    %% Encode as JSON
    JsonBytes = encode_json(JsonObj, Config),

    %% Add newline
    [JsonBytes, $\n].

%% @doc Validate configuration
-spec check_config(logger:formatter_config()) -> {ok, config()} | {error, term()}.
check_config(Config) when is_map(Config) ->
    %% Validate known configuration keys
    ValidKeys = [template, single_line, time_offset, time_designator,
                 time_unit, include_metadata, include_node, include_pid,
                 pretty_print, field_map],

    UnknownKeys = maps:keys(Config) -- ValidKeys,
    case UnknownKeys of
        [] ->
            {ok, normalize_config(Config)};
        _ ->
            {error, {unknown_keys, UnknownKeys}}
    end;
check_config(_Config) ->
    {error, invalid_config}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Normalize configuration with defaults
-spec normalize_config(map()) -> config().
normalize_config(Config) ->
    Default = #{
        time_offset => ?DEFAULT_TIME_OFFSET,
        time_designator => ?DEFAULT_TIME_DESIGNATOR,
        time_unit => ?DEFAULT_TIME_UNIT,
        include_metadata => true,
        include_node => true,
        include_pid => true,
        pretty_print => false,
        field_map => #{
            timestamp => <<"timestamp">>,
            level => <<"level">>,
            message => <<"message">>,
            node => <<"node">>,
            pid => <<"pid">>,
            mfa => <<"mfa">>,
            line => <<"line">>,
            trace_id => <<"trace_id">>,
            span_id => <<"span_id">>,
            request_id => <<"request_id">>,
            session_id => <<"session_id">>,
            correlation_id => <<"correlation_id">>,
            domain => <<"domain">>,
            component => <<"component">>,
            metadata => <<"metadata">>
        }
    },
    maps:merge(Default, Config).

%% @private
%% Build JSON object from log event
-spec build_json_object(integer(), {io:format(), [term()]}, log_metadata(), config()) -> map().
build_json_object(Time, {Format, Args}, Meta, Config) ->
    %% Format message
    Message = format_message(Format, Args),

    %% Get field mapping
    FieldMap = maps:get(field_map, Config, #{}),

    %% Base fields
    Base = #{
        maps:get(timestamp, FieldMap, <<"timestamp">>) => format_timestamp_iso8601(Time, Config),
        maps:get(level, FieldMap, <<"level">>) => atom_to_binary(maps:get(level, Meta, info)),
        maps:get(message, FieldMap, <<"message">>) => Message
    },

    %% Add node if enabled
    Base1 = case maps:get(include_node, Config, true) of
        true ->
            NodeBin = atom_to_binary(node()),
            maps:put(maps:get(node, FieldMap, <<"node">>), NodeBin, Base);
        false ->
            Base
    end,

    %% Add PID if enabled
    Base2 = case maps:get(include_pid, Config, true) of
        true ->
            PidBin = list_to_binary(pid_to_list(maps:get(pid, Meta))),
            maps:put(maps:get(pid, FieldMap, <<"pid">>), PidBin, Base1);
        false ->
            Base1
    end,

    %% Add MFA if available
    Base3 = case maps:get(mfa, Meta, undefined) of
        undefined -> Base2;
        {M, F, A} ->
            MfaBin = format_mfa(M, F, A),
            maps:put(maps:get(mfa, FieldMap, <<"mfa">>), MfaBin, Base2)
    end,

    %% Add line number if available
    Base4 = case maps:get(line, Meta, undefined) of
        undefined -> Base3;
        Line -> maps:put(maps:get(line, FieldMap, <<"line">>), Line, Base3)
    end,

    %% Add trace/span correlation IDs if available
    Base5 = add_correlation_ids(Meta, FieldMap, Base4),

    %% Add domain if available
    Base6 = case maps:get(domain, Meta, undefined) of
        undefined -> Base5;
        Domain when is_list(Domain) ->
            DomainBin = list_to_binary(string:join([atom_to_list(D) || D <- Domain], ".")),
            maps:put(maps:get(domain, FieldMap, <<"domain">>), DomainBin, Base5)
    end,

    %% Add component if available
    Base7 = case maps:get(component, Meta, undefined) of
        undefined -> Base6;
        Component -> maps:put(maps:get(component, FieldMap, <<"component">>), Component, Base6)
    end,

    %% Add all metadata if enabled
    case maps:get(include_metadata, Config, true) of
        true ->
            %% Filter out already-added keys
            FilteredMeta = maps:without([time, level, pid, mfa, line, domain, component], Meta),
            case maps:size(FilteredMeta) of
                0 -> Base7;
                _ -> maps:put(maps:get(metadata, FieldMap, <<"metadata">>), filter_metadata(FilteredMeta), Base7)
            end;
        false ->
            Base7
    end.

%% @private
%% Add correlation IDs (trace_id, span_id, request_id, etc.)
-spec add_correlation_ids(log_metadata(), map(), map()) -> map().
add_correlation_ids(Meta, FieldMap, Acc) ->
    %% trace_id from OpenTelemetry
    Acc1 = case maps:get(trace_id, Meta, undefined) of
        undefined -> Acc;
        TraceId -> maps:put(maps:get(trace_id, FieldMap, <<"trace_id">>), format_trace_id(TraceId), Acc)
    end,

    %% span_id from OpenTelemetry
    Acc2 = case maps:get(span_id, Meta, undefined) of
        undefined -> Acc1;
        SpanId -> maps:put(maps:get(span_id, FieldMap, <<"span_id">>), format_span_id(SpanId), Acc1)
    end,

    %% request_id for request tracking
    Acc3 = case maps:get(request_id, Meta, undefined) of
        undefined -> Acc2;
        RequestId -> maps:put(maps:get(request_id, FieldMap, <<"request_id">>), format_request_id(RequestId), Acc2)
    end,

    %% session_id for session tracking
    Acc4 = case maps:get(session_id, Meta, undefined) of
        undefined -> Acc3;
        SessionId -> maps:put(maps:get(session_id, FieldMap, <<"session_id">>), format_session_id(SessionId), Acc3)
    end,

    %% correlation_id - explicit correlation
    Acc5 = case maps:get(correlation_id, Meta, undefined) of
        undefined -> Acc4;
        CorrelationId -> maps:put(maps:get(correlation_id, FieldMap, <<"correlation_id">>), format_correlation_id(CorrelationId), Acc4)
    end,

    Acc5.

%% @private
%% Format timestamp as ISO 8601
-spec format_timestamp_iso8601(integer(), config()) -> binary().
format_timestamp_iso8601(Time, Config) ->
    Unit = maps:get(time_unit, Config, ?DEFAULT_TIME_UNIT),
    TimeMs = case Unit of
        nanosecond -> Time div 1000000;
        microsecond -> Time div 1000;
        millisecond -> Time;
        second -> Time * 1000
    end,

    {{Year, Month, Day}, {Hour, Min, Sec}} = timestamp_to_datetime(TimeMs),

    %% Get offset
    Offset = case maps:get(time_offset, Config, ?DEFAULT_TIME_OFFSET) of
        "-00:00" -> "Z";
        "+00:00" -> "Z";
        O when is_list(O) -> O;
        O when is_binary(O) -> binary_to_list(O)
    end,

    DesignatorBin = case maps:get(time_designator, Config, ?DEFAULT_TIME_DESIGNATOR) of
        $T -> <<"T">>;
        _ -> <<"T">>
    end,

    FormatStr = <<"~4.10.0B-~2.10.0B-~2.10.0B", DesignatorBin/binary,
                  "~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B", Offset/binary>>,
    iolist_to_binary(io_lib:format(binary_to_list(FormatStr),
                                   [Year, Month, Day, Hour, Min, Sec, TimeMs rem 1000])).

%% @private
%% Convert millisecond timestamp to datetime
-spec timestamp_to_datetime(integer()) -> calendar:datetime().
timestamp_to_datetime(Ms) when Ms > 0 ->
    %% Epoch is 1970-01-01 00:00:00 GMT
    Epoch = {{1970, 1, 1}, {0, 0, 0}},
    SecondsSinceEpoch = Ms div 1000,
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Epoch) + SecondsSinceEpoch).

%% @private
%% Format log message
-spec format_message(io:format(), [term()]) -> binary().
format_message(Format, Args) ->
    try
        Msg = io_lib:format(Format, Args),
        iolist_to_binary(Msg)
    catch
        _:_ ->
            %% Fallback for unsafe format strings
            MsgBin = case Format of
                Bin when is_binary(Bin) -> Bin;
                Str when is_list(Str) -> list_to_binary(Str);
                _ -> <<"Unable to format message">>
            end,
            <<MsgBin/binary, " (args: ", (list_to_binary(io_lib:format("~p", [Args])))/binary, ")">>
    end.

%% @private
%% Format MFA tuple
-spec format_mfa(module(), atom(), arity()) -> binary().
format_mfa(M, F, A) ->
    MfaBin = iolist_to_binary(io_lib:format("~p:~p/~p", [M, F, A])),
    %% Clean up extra quotes from atom formatting
    binary:replace(MfaBin, <<"'">>, <<"">>, [global]).

%% @private
%% Format trace ID (16-byte hex string)
-spec format_trace_id(binary() | integer() | list()) -> binary().
format_trace_id(TraceId) when is_binary(TraceId) ->
    case byte_size(TraceId) of
        16 -> binary:encode_hex(TraceId);
        32 -> TraceId;  % Already hex
        _ -> TraceId
    end;
format_trace_id(TraceId) when is_integer(TraceId) ->
    integer_to_binary(TraceId, 16);
format_trace_id(TraceId) when is_list(TraceId) ->
    list_to_binary(TraceId).

%% @private
%% Format span ID (8-byte hex string)
-spec format_span_id(binary() | integer() | list()) -> binary().
format_span_id(SpanId) when is_binary(SpanId) ->
    case byte_size(SpanId) of
        8 -> binary:encode_hex(SpanId);
        16 -> SpanId;  % Already hex
        _ -> SpanId
    end;
format_span_id(SpanId) when is_integer(SpanId) ->
    integer_to_binary(SpanId, 16);
format_span_id(SpanId) when is_list(SpanId) ->
    list_to_binary(SpanId).

%% @private
%% Format request ID
-spec format_request_id(binary() | integer() | list()) -> binary().
format_request_id(RequestId) when is_binary(RequestId) -> RequestId;
format_request_id(RequestId) when is_integer(RequestId) -> integer_to_binary(RequestId);
format_request_id(RequestId) when is_list(RequestId) -> list_to_binary(RequestId).

%% @private
%% Format session ID
-spec format_session_id(binary() | integer() | list()) -> binary().
format_session_id(SessionId) when is_binary(SessionId) -> SessionId;
format_session_id(SessionId) when is_integer(SessionId) -> integer_to_binary(SessionId);
format_session_id(SessionId) when is_list(SessionId) -> list_to_binary(SessionId).

%% @private
%% Format correlation ID
-spec format_correlation_id(binary() | integer() | list()) -> binary().
format_correlation_id(CorrelationId) when is_binary(CorrelationId) -> CorrelationId;
format_correlation_id(CorrelationId) when is_integer(CorrelationId) -> integer_to_binary(CorrelationId);
format_correlation_id(CorrelationId) when is_list(CorrelationId) -> list_to_binary(CorrelationId).

%% @private
%% Filter metadata to JSON-serializable values only
-spec filter_metadata(log_metadata()) -> map().
filter_metadata(Meta) ->
    maps:fold(fun(K, V, Acc) ->
        case is_json_serializable(V) of
            true -> maps:put(K, sanitize_value(V), Acc);
            false -> Acc
        end
    end, #{}, Meta).

%% @private
%% Check if value is JSON serializable
-spec is_json_serializable(term()) -> boolean().
is_json_serializable(V) when is_binary(V); is_number(V); is_boolean(V) -> true;
is_json_serializable(V) when is_atom(V) -> true;
is_json_serializable(V) when is_list(V); is_map(V) ->
    try json_sanitize(V), true catch _:_ -> false end;
is_json_serializable(V) when is_pid(V) -> true;
is_json_serializable(V) when is_reference(V) -> true;
is_json_serializable(V) when is_port(V) -> true;
is_json_serializable(_) -> false.

%% @private
%% Sanitize value for JSON encoding
-spec sanitize_value(term()) -> term().
sanitize_value(V) when is_binary(V); is_number(V); is_boolean(V) -> V;
sanitize_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
sanitize_value(V) when is_pid(V) -> list_to_binary(pid_to_list(V));
sanitize_value(V) when is_reference(V) -> list_to_binary(io_lib:format("~p", [V]));
sanitize_value(V) when is_port(V) -> list_to_binary(erlang:port_to_list(V));
sanitize_value(V) when is_list(V) -> json_sanitize(V);
sanitize_value(V) when is_map(V) ->
    maps:map(fun(_K, Val) -> sanitize_value(Val) end, V);
sanitize_value(V) -> list_to_binary(io_lib:format("~p", [V])).

%% @private
%% Sanitize lists for JSON
-spec json_sanitize(list()) -> list() | binary().
json_sanitize([]) -> [];
json_sanitize([H | T]) when is_integer(H), H >= 0, H =< 255 ->
    %% Possibly a string (charlist)
    case io_lib:char_list([H | T]) of
        true -> list_to_binary([H | T]);
        false -> [sanitize_value(H) | json_sanitize(T)]
    end;
json_sanitize([H | T]) ->
    [sanitize_value(H) | json_sanitize(T)].

%% @private
%% Encode map as JSON (using OTP 27+ native json module)
-spec encode_json(map(), config()) -> iolist().
encode_json(JsonObj, Config) ->
    case maps:get(pretty_print, Config, false) of
        true ->
            %% Pretty print with indentation
            json_encode_pretty(JsonObj);
        false ->
            %% Compact JSON
            encode_json_compact(JsonObj)
    end.

%% @private
%% Compact JSON encoding using OTP 27+ native json
-spec encode_json_compact(map()) -> iolist().
encode_json_compact(Map) when is_map(Map) ->
    try
        %% Use OTP 27+ native json module
        json:encode(Map)
    catch
        error:undef ->
            %% Fallback for OTP < 27
            json_encode_fallback(Map)
    end.

%% @private
%% Pretty print JSON with indentation
-spec json_encode_pretty(map()) -> iolist().
json_encode_pretty(Map) when is_map(Map) ->
    try
        %% Try pretty print with native json
        case json:encode(Map, [{pretty, true}]) of
            Result when is_binary(Result) -> Result;
            _ -> encode_json_compact(Map)
        end
    catch
        error:undef ->
            encode_json_compact(Map);
        _:_ ->
            encode_json_compact(Map)
    end.

%% @private
%% Fallback JSON encoding for OTP < 27
-spec json_encode_fallback(map()) -> binary().
json_encode_fallback(Map) when is_map(Map) ->
    iolist_to_binary(encode_map(Map)).

%% @private
%% Encode map to JSON string
encode_map(Map) ->
    Items = maps:to_list(Map),
    EncodedItems = encode_items(Items),
    ["{", string:join(EncodedItems, ","), "}"].

encode_items([]) -> [];
encode_items([{K, V}]) -> [encode_value(K), ":", encode_value(V)];
encode_items([{K, V} | Rest]) ->
    [encode_value(K), ":", encode_value(V) | "," ++ encode_items(Rest)].

encode_value(V) when is_binary(V) ->
    ["\"", escape_json(V), "\""];
encode_value(V) when is_atom(V) ->
    ["\"", atom_to_list(V), "\""];
encode_value(V) when is_integer(V) ->
    integer_to_list(V);
encode_value(V) when is_float(V) ->
    float_to_list(V, [{scientific, 20}, {decimals, 10}]);
encode_value(V) when is_list(V) ->
    case io_lib:char_list(V) of
        true -> ["\"", escape_json(list_to_binary(V)), "\""];
        false -> encode_list(V)
    end;
encode_value(V) when is_map(V) ->
    encode_map(V);
encode_value(true) -> "true";
encode_value(false) -> "false";
encode_value(null) -> "null";
encode_value(undefined) -> "null";
encode_value(_) -> ["\"", "unknown", "\""].

encode_list([]) -> "[]";
encode_list(List) ->
    case lists:all(fun(E) -> is_map(E) orelse is_binary(E) orelse is_number(E) orelse
                              is_atom(E) orelse is_boolean(E) end, List) of
        true ->
            Items = [encode_value(E) || E <- List],
            ["[", string:join(Items, ","), "]"];
        false ->
            %% Treat as string
            ["\"", escape_json(list_to_binary(List)), "\""]
    end.

%% @private
%% Escape special characters in JSON strings
-spec escape_json(binary()) -> iolist().
escape_json(Bin) ->
    escape_json(Bin, []).

escape_json(<<>>, Acc) ->
    lists:reverse(Acc);
escape_json(<<$\\, Rest/binary>>, Acc) ->
    escape_json(Rest, ["\\\\" | Acc]);
escape_json(<<$", Rest/binary>>, Acc) ->
    escape_json(Rest, ["\\\"" | Acc]);
escape_json(<<$\n, Rest/binary>>, Acc) ->
    escape_json(Rest, ["\\n" | Acc]);
escape_json(<<$\r, Rest/binary>>, Acc) ->
    escape_json(Rest, ["\\r" | Acc]);
escape_json(<<$\t, Rest/binary>>, Acc) ->
    escape_json(Rest, ["\\t" | Acc]);
escape_json(<<$\b, Rest/binary>>, Acc) ->
    escape_json(Rest, ["\\b" | Acc]);
escape_json(<<$\f, Rest/binary>>, Acc) ->
    escape_json(Rest, ["\\f" | Acc]);
escape_json(<<C, Rest/binary>>, Acc) when C < 32 ->
    escape_json(Rest, [io_lib:format("\\u00~2.16.0B", [C]) | Acc]);
escape_json(<<C, Rest/binary>>, Acc) ->
    escape_json(Rest, [C | Acc]).
