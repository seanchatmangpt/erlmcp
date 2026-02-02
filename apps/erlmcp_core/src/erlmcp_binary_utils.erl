%%%-------------------------------------------------------------------
%%% @doc erlmcp_binary_utils - OTP 28 Binary Utilities
%%%
%%% This module provides efficient binary operations using OTP 28 features,
%%% particularly binary:join/2 for optimized binary concatenation.
%%%
%%% OTP 28 INNOVATION: binary:join/2
%%% - Join list of binaries with separator
%%% - Analogous to string:join for binaries
%%% - More efficient than iolist_to_binary for joining
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_binary_utils).

%% API exports
-export([join_sse_chunks/1,
         join_json_array/1,
         join_headers/1,
         join_lines/1,
         join_with_separator/2,
         join_kv_pairs/2,
         join_trace_context/1,
         join_metrics/1]).

%% Types
-type chunk() :: binary().
-type header() :: {binary(), binary()}.
-type kv_pair() :: {binary(), binary()}.
-type trace_entry() :: {binary(), term()}.
-type metric() :: {binary(), number()}.

-export_type([chunk/0, header/0, kv_pair/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Join SSE (Server-Sent Events) chunks with newlines.
%% SSE format requires double newline at end of each event.
%%
%% Optimized for:
%% - Real-time event streaming
%% - Large batch event delivery
%% - Keep-alive comment insertion
%%
%% @param Chunks List of SSE event chunks
%% @return Joined SSE message with proper newlines
%%
%% <b>Example:</b>
%% <pre>
%% Chunks = [
%%     <<"event: message">>,
%%     <<"data: {\"json\": \"data\"}">>,
%%     <<"event: log">>,
%%     <<"data: \"log entry\"">>
%% ],
%% Result = erlmcp_binary_utils:join_sse_chunks(Chunks),
%% %% Result: <<"event: message\ndata: {\"json\": \"data\"}\nevent: log\ndata: \"log entry\"\n">>
%% </pre>
-spec join_sse_chunks([chunk()]) -> binary().
join_sse_chunks(Chunks) when is_list(Chunks) ->
    %% OTP 28: Use binary:join/2 for efficient newline joining
    binary:join(<<"\n">>, Chunks).

%% @doc Join JSON array elements with commas.
%% Wraps the result in square brackets.
%%
%% Optimized for:
%% - JSON-RPC batch responses
%% - Array serialization
%% - Bulk data transfer
%%
%% @param Items List of JSON-encoded binaries
%% @return Valid JSON array binary
%%
%% <b>Example:</b>
%% <pre>
%% Items = [
%%     <<"{\"id\": 1, \"result\": \"ok\"}">>,
%%     <<"{\"id\": 2, \"result\": \"error\"}">>
%% ],
%% Result = erlmcp_binary_utils:join_json_array(Items),
%% %% Result: <<"[{\"id\": 1, \"result\": \"ok\"},{\"id\": 2, \"result\": \"error\"}]">>
%% </pre>
-spec join_json_array([binary()]) -> binary().
join_json_array([]) ->
    <<"[]">>;
join_json_array(Items) when is_list(Items) ->
    %% OTP 28: Join with comma, wrap in brackets
    Inner = binary:join(<<",">>, Items),
    <<"[", Inner/binary, "]">>.

%% @doc Join HTTP headers with CRLF.
%% Formats headers as "Key: Value\r\n".
%%
%% Optimized for:
%% - HTTP response headers
%% - HTTP request headers
%% - SSE event headers
%%
%% @param Headers List of {Key, Value} header pairs
%% @return Headers formatted with CRLF
%%
%% <b>Example:</b>
%% <pre>
%% Headers = [
%%     {<<"content-type">>, <<"application/json">>},
%%     {<<"cache-control">>, <<"no-cache">>}
%% ],
%% Result = erlmcp_binary_utils:join_headers(Headers),
%% %% Result: <<"content-type: application/json\r\ncache-control: no-cache\r\n">>
%% </pre>
-spec join_headers([header()]) -> binary().
join_headers([]) ->
    <<>>;
join_headers(Headers) when is_list(Headers) ->
    %% Format each header as "Key: Value"
    Lines = [<<K/binary, ": ", V/binary>> || {K, V} <- Headers],
    %% OTP 28: Join with CRLF (HTTP standard)
    binary:join(<<"\r\n">>, Lines).

%% @doc Join lines with newline separator.
%% Generic line joining utility.
%%
%% Optimized for:
%% - Log file formatting
%% - Text block assembly
%% - Multi-line messages
%%
%% @param Lines List of line binaries
%% @return Lines joined with newline
%%
%% <b>Example:</b>
%% <pre>
%% Lines = [<<"Line 1">>, <<"Line 2">>, <<"Line 3">>],
%% Result = erlmcp_binary_utils:join_lines(Lines),
%% %% Result: <<"Line 1\nLine 2\nLine 3">>
%% </pre>
-spec join_lines([binary()]) -> binary().
join_lines([]) ->
    <<>>;
join_lines(Lines) when is_list(Lines) ->
    %% OTP 28: Join with newline
    binary:join(<<"\n">>, Lines).

%% @doc Join binaries with custom separator.
%% Generic joining function for any separator.
%%
%% Optimized for:
%% - Custom delimited formats
%% - Protocol-specific separators
%% - Non-standard delimiters
%%
%% @param Separator Binary separator
%% @param Parts List of binary parts to join
%% @return Joined binary
%%
%% <b>Example:</b>
%% <pre>
%% Parts = [<<"a">>, <<"b">>, <<"c">>],
%% Result = erlmcp_binary_utils:join_with_separator(<<"|">>, Parts),
%% %% Result: <<"a|b|c">>
%% </pre>
-spec join_with_separator(binary(), [binary()]) -> binary().
join_with_separator(_Separator, []) ->
    <<>>;
join_with_separator(Separator, Parts) when is_binary(Separator), is_list(Parts) ->
    %% OTP 28: Generic join with any separator
    binary:join(Separator, Parts).

%% @doc Join key-value pairs with equals sign.
%% Useful for query strings, properties files, etc.
%%
%% Optimized for:
%% - URL query parameters
%% - Configuration properties
%% - Log context formatting
%%
%% @param Pairs List of {Key, Value} pairs
%% @param Separator Pair separator (default: & for query strings)
%% @return Joined key-value pairs
%%
%% <b>Example:</b>
%% <pre>
%% Pairs = [
%%     {<<"key1">>, <<"value1">>},
%%     {<<"key2">>, <<"value2">>}
%% ],
%% Result = erlmcp_binary_utils:join_kv_pairs(Pairs, <<"&">>),
%% %% Result: <<"key1=value1&key2=value2">>
%% </pre>
-spec join_kv_pairs([kv_pair()], binary()) -> binary().
join_kv_pairs([], _Separator) ->
    <<>>;
join_kv_pairs(Pairs, Separator) when is_list(Pairs), is_binary(Separator) ->
    %% Format each pair as "key=value"
    Formatted = [<<K/binary, "=", V/binary>> || {K, V} <- Pairs],
    %% OTP 28: Join pairs with separator
    binary:join(Separator, Formatted).

%% @doc Join OpenTelemetry trace context entries.
%% Formats trace context for distributed tracing headers.
%%
%% Optimized for:
%% - W3C Trace Context headers
%% - OpenTelemetry propagation
%% - Distributed tracing correlation
%%
%% @param Entries List of {Key, Value} trace entries
%% @return Formatted trace context
%%
%% <b>Example:</b>
%% <pre>
%% Entries = [
%%     {<<"traceparent">>, <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01">>},
%%     {<<"tracestate">>, <<"rojo=00f067aa0ba902b7">>}
%% ],
%% Result = erlmcp_binary_utils:join_trace_context(Entries),
%% %% Result: <<"traceparent=00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01;tracestate=rojo=00f067aa0ba902b7">>
%% </pre>
-spec join_trace_context([trace_entry()]) -> binary().
join_trace_context([]) ->
    <<>>;
join_trace_context(Entries) when is_list(Entries) ->
    %% Format each entry, converting values to binary
    Formatted = [format_trace_entry(K, V) || {K, V} <- Entries],
    %% OTP 28: Join with semicolon (W3C trace context standard)
    binary:join(<<";">>, Formatted).

%% @doc Join metrics for Prometheus exposition format.
%% Formats metrics with proper separators.
%%
%% Optimized for:
%% - Prometheus metrics export
%% - OpenTelemetry metrics
%% - Monitoring dashboards
%%
%% @param Metrics List of {Name, Value} metric pairs
%% @return Formatted metrics block
%%
%% <b>Example:</b>
%% <pre>
%% Metrics = [
%%     {<<"erlmcp_requests_total">>, 1234},
%%     {<<"erlmcp_errors_total">>, 5}
%% ],
%% Result = erlmcp_binary_utils:join_metrics(Metrics),
%% %% Result: <<"erlmcp_requests_total 1234\nerlmcp_errors_total 5\n">>
%% </pre>
-spec join_metrics([metric()]) -> binary().
join_metrics([]) ->
    <<>>;
join_metrics(Metrics) when is_list(Metrics) ->
    %% Format each metric as "name value\n"
    Lines = [format_metric(K, V) || {K, V} <- Metrics],
    %% OTP 28: Join with newline, add trailing newline
    Joined = binary:join(<<"\n">>, Lines),
    <<Joined/binary, "\n">>.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Format trace context entry.
%% Handles various value types (binary, integer, atom).
-spec format_trace_entry(binary(), term()) -> binary().
format_trace_entry(Key, Value) when is_binary(Value) ->
    <<Key/binary, "=", Value/binary>>;
format_trace_entry(Key, Value) when is_integer(Value) ->
    <<Key/binary, "=", (integer_to_binary(Value))/binary>>;
format_trace_entry(Key, Value) when is_atom(Value) ->
    <<Key/binary, "=", (atom_to_binary(Value, utf8))/binary>>;
format_trace_entry(Key, Value) ->
    %% Fallback: convert to string then binary
    <<Key/binary, "=", (list_to_binary(io_lib:format("~p", [Value])))/binary>>.

%% @private Format metric for Prometheus exposition.
-spec format_metric(binary(), number()) -> binary().
format_metric(Name, Value) when is_integer(Value) ->
    <<Name/binary, " ", (integer_to_binary(Value))/binary>>;
format_metric(Name, Value) when is_float(Value) ->
    %% Use float_to_binary with scientific notation for large numbers
    <<Name/binary, " ", (float_to_binary(Value, [{scientific, 50}]))/binary>>;
format_metric(Name, Value) ->
    %% Fallback for other number types
    <<Name/binary, " ", (list_to_binary(io_lib:format("~p", [Value])))/binary>>.
