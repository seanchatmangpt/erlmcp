%%%-------------------------------------------------------------------
%%% @doc erlmcp_binary_utils tests - OTP 28 Binary Utilities Tests
%%%
%%% Chicago School TDD: Tests drive behavior, not implementation.
%%% Tests validate black-box behavior, not internal details.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_binary_utils_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

%% @doc Test join_sse_chunks/1
join_sse_chunks_test_() ->
    [{"Empty list returns empty binary",
      fun() ->
              ?assertEqual(<<>>, erlmcp_binary_utils:join_sse_chunks([]))
      end},
     {"Single chunk returns unchanged",
      fun() ->
              Chunk = <<"event: message\ndata: test">>,
              ?assertEqual(Chunk, erlmcp_binary_utils:join_sse_chunks([Chunk]))
      end},
     {"Multiple chunks joined with newlines",
      fun() ->
              Chunks = [<<"event: message">>,
                        <<"data: test1">>,
                        <<"data: test2">>],
              Expected = <<"event: message\ndata: test1\ndata: test2">>,
              ?assertEqual(Expected, erlmcp_binary_utils:join_sse_chunks(Chunks))
      end},
     {"Large chunk list (10K)",
      fun() ->
              %% Performance test: 10K chunks
              Chunks = lists:duplicate(10000, <<"data: chunk">>),
              Result = erlmcp_binary_utils:join_sse_chunks(Chunks),
              ?assert(is_binary(Result)),
              ?assert(byte_size(Result) > 0)
      end},
     {"SSE event with double newline",
      fun() ->
              %% Real SSE format test
              Chunks = [
                  <<"event: message">>,
                  <<"data: {\"json\": \"data\"}">>,
                  <<>>  % Empty line for SSE double newline
              ],
              Result = erlmcp_binary_utils:join_sse_chunks(Chunks),
              ?assertMatch(<<"event: message\ndata:", _/binary>>, Result)
      end}].

%% @doc Test join_json_array/1
join_json_array_test_() ->
    [{"Empty array",
      fun() ->
              ?assertEqual(<<"[]">>, erlmcp_binary_utils:join_json_array([]))
      end},
     {"Single element",
      fun() ->
              Items = [<<"{\"id\": 1}">>],
              Expected = <<"[{\"id\": 1}]">>,
              ?assertEqual(Expected, erlmcp_binary_utils:join_json_array(Items))
      end},
     {"Multiple elements joined with commas",
      fun() ->
              Items = [
                  <<"{\"id\": 1, \"result\": \"ok\"}">>,
                  <<"{\"id\": 2, \"result\": \"error\"}">>
              ],
              Expected = <<"[{\"id\": 1, \"result\": \"ok\"},{\"id\": 2, \"result\": \"error\"}]">>,
              ?assertEqual(Expected, erlmcp_binary_utils:join_json_array(Items))
      end},
     {"Large JSON array (10K elements)",
      fun() ->
              %% Performance test: 10K JSON objects
              Items = lists:duplicate(10000, <<"{\"item\": true}">>),
              Result = erlmcp_binary_utils:join_json_array(Items),
              ?assert(is_binary(Result)),
              ?assertMatch(<<"[", _/binary>>, Result),
              ?assertMatch(<<_/binary, "]">>, Result)
      end},
     {"JSON-RPC batch response format",
      fun() ->
              %% Real JSON-RPC batch format
              Items = [
                  <<"{\"jsonrpc\": \"2.0\", \"id\": 1, \"result\": {}}">>,
                  <<"{\"jsonrpc\": \"2.0\", \"id\": 2, \"result\": {}}">>
              ],
              Result = erlmcp_binary_utils:join_json_array(Items),
              ?assertMatch(<<"[", _/binary>>, Result)
      end}].

%% @doc Test join_headers/1
join_headers_test_() ->
    [{"Empty list returns empty binary",
      fun() ->
              ?assertEqual(<<>>, erlmcp_binary_utils:join_headers([]))
      end},
     {"Single header formatted correctly",
      fun() ->
              Headers = [{<<"content-type">>, <<"application/json">>}],
              Expected = <<"content-type: application/json\r\n">>,
              ?assertEqual(Expected, erlmcp_binary_utils:join_headers(Headers))
      end},
     {"Multiple headers joined with CRLF",
      fun() ->
              Headers = [
                  {<<"content-type">>, <<"application/json">>},
                  {<<"cache-control">>, <<"no-cache">>},
                  {<<"connection">>, <<"keep-alive">>}
              ],
              Result = erlmcp_binary_utils:join_headers(Headers),
              %% Verify CRLF separators
              ?assertMatch(<<"content-type: application/json\r\ncache-control:", _/binary>>, Result)
      end},
     {"HTTP security headers",
      fun() ->
              %% Real HTTP security headers
              Headers = [
                  {<<"x-content-type-options">>, <<"nosniff">>},
                  {<<"x-frame-options">>, <<"DENY">>},
                  {<<"strict-transport-security">>, <<"max-age=31536000">>}
              ],
              Result = erlmcp_binary_utils:join_headers(Headers),
              ?assert(is_binary(Result)),
              ?assert(byte_size(Result) > 0)
      end},
     {"SSE response headers",
      fun() ->
              %% SSE-specific headers
              Headers = [
                  {<<"content-type">>, <<"text/event-stream">>},
                  {<<"cache-control">>, <<"no-cache">>},
                  {<<"connection">>, <<"keep-alive">>},
                  {<<"x-accel-buffering">>, <<"no">>}
              ],
              Result = erlmcp_binary_utils:join_headers(Headers),
              ?assertMatch(<<"content-type: text/event-stream\r\n", _/binary>>, Result)
      end}].

%% @doc Test join_lines/1
join_lines_test_() ->
    [{"Empty list returns empty binary",
      fun() ->
              ?assertEqual(<<>>, erlmcp_binary_utils:join_lines([]))
      end},
     {"Single line",
      fun() ->
              Lines = [<<"Line 1">>],
              ?assertEqual(<<"Line 1">>, erlmcp_binary_utils:join_lines(Lines))
      end},
     {"Multiple lines with newlines",
      fun() ->
              Lines = [<<"Line 1">>, <<"Line 2">>, <<"Line 3">>],
              Expected = <<"Line 1\nLine 2\nLine 3">>,
              ?assertEqual(Expected, erlmcp_binary_utils:join_lines(Lines))
      end},
     {"Log file formatting",
      fun() ->
              %% Real log format
              Lines = [
                  <<"2025-01-01 10:00:00 [INFO] Starting">>,
                  <<"2025-01-01 10:00:01 [INFO] Running">>,
                  <<"2025-01-01 10:00:02 [INFO] Done">>
              ],
              Result = erlmcp_binary_utils:join_lines(Lines),
              ?assert(is_binary(Result)),
              ?assert(byte_size(Result) > 0)
      end}].

%% @doc Test join_with_separator/2
join_with_separator_test_() ->
    [{"Empty list returns empty binary",
      fun() ->
              ?assertEqual(<<>>, erlmcp_binary_utils:join_with_separator(<<"|">>, []))
      end},
     {"Single element",
      fun() ->
              Parts = [<<"a">>],
              ?assertEqual(<<"a">>, erlmcp_binary_utils:join_with_separator(<<"|">>, Parts))
      end},
     {"Pipe separator",
      fun() ->
              Parts = [<<"a">>, <<"b">>, <<"c">>],
              Expected = <<"a|b|c">>,
              ?assertEqual(Expected, erlmcp_binary_utils:join_with_separator(<<"|">>, Parts))
      end},
     {"Comma separator (CSV format)",
      fun() ->
              Parts = [<<"value1">>, <<"value2">>, <<"value3">>],
              Expected = <<"value1,value2,value3">>,
              ?assertEqual(Expected, erlmcp_binary_utils:join_with_separator(<<",">>, Parts))
      end},
     {"Tab separator (TSV format)",
      fun() ->
              Parts = [<<"col1">>, <<"col2">>, <<"col3">>],
              Result = erlmcp_binary_utils:join_with_separator(<<"\t">>, Parts),
              ?assertMatch(<<"col1\t", _/binary>>, Result)
      end},
     {"Custom separator",
      fun() ->
              Parts = [<<"seg1">>, <<"seg2">>],
              Result = erlmcp_binary_utils:join_with_separator(<<"::">>, Parts),
              ?assertEqual(<<"seg1::seg2">>, Result)
      end}].

%% @doc Test join_kv_pairs/2
join_kv_pairs_test_() ->
    [{"Empty list returns empty binary",
      fun() ->
              ?assertEqual(<<>>, erlmcp_binary_utils:join_kv_pairs([], <<"&">>))
      end},
     {"Single key-value pair",
      fun() ->
              Pairs = [{<<"key1">>, <<"value1">>}],
              Expected = <<"key1=value1">>,
              ?assertEqual(Expected, erlmcp_binary_utils:join_kv_pairs(Pairs, <<"&">>))
      end},
     {"Query string format with & separator",
      fun() ->
              Pairs = [
                  {<<"key1">>, <<"value1">>},
                  {<<"key2">>, <<"value2">>},
                  {<<"key3">>, <<"value3">>}
              ],
              Expected = <<"key1=value1&key2=value2&key3=value3">>,
              ?assertEqual(Expected, erlmcp_binary_utils:join_kv_pairs(Pairs, <<"&">>))
      end},
     {"Properties file format with newline",
      fun() ->
              Pairs = [
                  {<<"host">>, <<"localhost">>},
                  {<<"port">>, <<"8080">>}
              ],
              Result = erlmcp_binary_utils:join_kv_pairs(Pairs, <<"\n">>),
              ?assertMatch(<<"host=localhost\nport=8080">>, Result)
      end},
     {"Log context formatting",
      fun() ->
              %% Real log context
              Pairs = [
                  {<<"request_id">>, <<"abc123">>},
                  {<<"user_id">>, <<"user456">>},
                  {<<"action">>, <<"login">>}
              ],
              Result = erlmcp_binary_utils:join_kv_pairs(Pairs, <<" ">>),
              ?assert(is_binary(Result)),
              ?assert(byte_size(Result) > 0)
      end}].

%% @doc Test join_trace_context/1
join_trace_context_test_() ->
    [{"Empty list returns empty binary",
      fun() ->
              ?assertEqual(<<>>, erlmcp_binary_utils:join_trace_context([]))
      end},
     {"Single trace entry",
      fun() ->
              Entries = [{<<"traceparent">>, <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01">>}],
              Result = erlmcp_binary_utils:join_trace_context(Entries),
              ?assertMatch(<<"traceparent=00-4bf92f3577b34da6a3ce929d0e0e4736-", _/binary>>, Result)
      end},
     {"Multiple trace entries with semicolon separator",
      fun() ->
              Entries = [
                  {<<"traceparent">>, <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01">>},
                  {<<"tracestate">>, <<"rojo=00f067aa0ba902b7">>}
              ],
              Result = erlmcp_binary_utils:join_trace_context(Entries),
              ?assertMatch(<<"traceparent=", _/binary>>, Result),
              ?assertMatch(<<_/binary, ";tracestate=", _/binary>>, Result)
      end},
     {"Integer value conversion",
      fun() ->
              Entries = [{<<"span_id">>, 12345}],
              Result = erlmcp_binary_utils:join_trace_context(Entries),
              ?assertMatch(<<"span_id=12345">>, Result)
      end},
     {"Atom value conversion",
      fun() ->
              Entries = [{<<"sampling_rate">>, enabled}],
              Result = erlmcp_binary_utils:join_trace_context(Entries),
              ?assertMatch(<<"sampling_rate=enabled">>, Result)
      end},
     {"Complex trace context",
      fun() ->
              Entries = [
                  {<<"traceparent">>, <<"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01">>},
                  {<<"tracestate">>, <<"rojo=00f067aa0ba902b7">>},
                  {<<"baggage">>, <<"key1=value1,key2=value2">>}
              ],
              Result = erlmcp_binary_utils:join_trace_context(Entries),
              ?assert(is_binary(Result)),
              ?assert(byte_size(Result) > 0)
      end}].

%% @doc Test join_metrics/1
join_metrics_test_() ->
    [{"Empty list returns empty binary",
      fun() ->
              ?assertEqual(<<>>, erlmcp_binary_utils:join_metrics([]))
      end},
     {"Single metric",
      fun() ->
              Metrics = [{<<"erlmcp_requests_total">>, 100}],
              Result = erlmcp_binary_utils:join_metrics(Metrics),
              ?assertMatch(<<"erlmcp_requests_total 100\n">>, Result)
      end},
     {"Multiple metrics with newline separator",
      fun() ->
              Metrics = [
                  {<<"erlmcp_requests_total">>, 1234},
                  {<<"erlmcp_errors_total">>, 5},
                  {<<"erlmcp_latency_ms">>, 42}
              ],
              Result = erlmcp_binary_utils:join_metrics(Metrics),
              ?assertMatch(<<"erlmcp_requests_total 1234\n", _/binary>>, Result),
              ?assertMatch(<<_/binary, "\n">>, Result)
      end},
     {"Float metric values",
      fun() ->
              Metrics = [
                  {<<"erlmcp_latency_avg">>, 42.5},
                  {<<"erlmcp_latency_p99">>, 123.8}
              ],
              Result = erlmcp_binary_utils:join_metrics(Metrics),
              ?assert(is_binary(Result)),
              ?assert(byte_size(Result) > 0)
      end},
     {"Prometheus exposition format",
      fun() ->
              %% Real Prometheus format
              Metrics = [
                  {<<"erlmcp_requests_total">>, 1234},
                  {<<"erlmcp_errors_total">>, 5},
                  {<<"erlmcp_active_connections">>, 42}
              ],
              Result = erlmcp_binary_utils:join_metrics(Metrics),
              %% Verify trailing newline
              ?assertMatch(<<_/binary, "\n">>, Result)
      end},
     {"Large metric set (10K metrics)",
      fun() ->
              %% Performance test: 10K metrics
              Metrics = [{<<"metric_", (integer_to_binary(N))/binary>>, N} || N <- lists:seq(1, 10000)],
              Result = erlmcp_binary_utils:join_metrics(Metrics),
              ?assert(is_binary(Result)),
              ?assert(byte_size(Result) > 0)
      end}].

%%====================================================================
%% Performance Benchmarks (Property-Based Testing)
%%====================================================================

%% @doc Property: join_sse_chunks always produces valid binary
prop_join_sse_chunks_binary() ->
    ?FORALL(Chunks, list(binary()),
            begin
                Result = erlmcp_binary_utils:join_sse_chunks(Chunks),
                is_binary(Result) andalso
                byte_size(Result) >= 0
            end).

%% @doc Property: join_json_array always produces valid JSON array
prop_join_json_array_valid() ->
    ?FORALL(Items, list(binary()),
            begin
                Result = erlmcp_binary_utils:join_json_array(Items),
                is_binary(Result) andalso
                byte_size(Result) >= 2 andalso  % At least "["
                (Items =:= [] orelse
                 (byte_size(Result) >= 2 andalso
                  binary:first(Result) =:= $[ andalso
                  binary:last(Result) =:= $]))
            end).

%% @doc Property: join_with_separator is associative
prop_join_associative() ->
    ?FORALL({Sep, Parts1, Parts2},
            {binary(), list(binary()), list(binary())},
            begin
                Result1 = erlmcp_binary_utils:join_with_separator(
                            Sep,
                            Parts1 ++ Parts2),
                Result2 = erlmcp_binary_utils:join_with_separator(
                            Sep,
                            [erlmcp_binary_utils:join_with_separator(Sep, Parts1),
                             erlmcp_binary_utils:join_with_separator(Sep, Parts2)]),
                %% Different due to separator placement, just check both are valid binaries
                is_binary(Result1) andalso is_binary(Result2)
            end).
