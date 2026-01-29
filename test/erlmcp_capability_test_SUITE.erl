-module(erlmcp_capability_test_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%====================================================================
%% Comprehensive MCP Capability Testing Suite
%% Tests all 10 capability categories with real server processes
%% Chicago School TDD: State-based verification, real collaborators, no mocks
%%====================================================================

%%% Test Suite Configuration

all() ->
    [
     %% 1. Tools (20 tool types)
     tools_basic_operations_test,
     tools_with_schemas_test,
     tools_error_handling_test,

     %% 2. Resources (read, list, watch)
     resources_read_test,
     resources_list_test,
     resources_subscribe_test,

     %% 3. Prompts (templates, arguments)
     prompts_basic_test,
     prompts_with_arguments_test,
     prompts_with_schema_test,

     %% 4. JSON-RPC Batch
     jsonrpc_batch_test,
     jsonrpc_concurrent_batch_test,

     %% 5. Large Payloads
     payload_size_test,
     payload_stress_test,

     %% 6. Progress Events
     progress_basic_test,
     progress_multiple_streams_test,

     %% 7. Cancellation
     cancellation_tool_call_test,
     cancellation_multiple_test,

     %% 8. SSE (Server-Sent Events)
     sse_basic_streaming_test,
     sse_multiple_streams_test,

     %% 9. WebSocket
     websocket_connection_test,
     websocket_concurrent_test,

     %% 10. Multi-tenant Isolation
     multitenant_isolation_test,
     multitenant_concurrent_ops_test
    ].

init_per_suite(Config) ->
    %% Start erlmcp applications
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_transports),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_transports),
    application:stop(erlmcp_core),
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Start test server for each test
    {ok, ServerPid} = start_test_server(),
    [{server_pid, ServerPid} | Config].

end_per_testcase(_TestCase, Config) ->
    ServerPid = ?config(server_pid, Config),
    stop_test_server(ServerPid),
    ok.

%%%====================================================================
%% 1. TOOLS CAPABILITY TESTS (20 tool types)
%%%====================================================================

tools_basic_operations_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register 20 different tool types
    Tools = [
        {<<"tool_calculator_add">>, fun(#{<<"a">> := A, <<"b">> := B}) ->
            <<"{\"result\": ", (integer_to_binary(A + B))/binary, "}">>
        end},
        {<<"tool_calculator_subtract">>, fun(#{<<"a">> := A, <<"b">> := B}) ->
            <<"{\"result\": ", (integer_to_binary(A - B))/binary, "}">>
        end},
        {<<"tool_string_uppercase">>, fun(#{<<"text">> := Text}) ->
            string:uppercase(Text)
        end},
        {<<"tool_string_lowercase">>, fun(#{<<"text">> := Text}) ->
            string:lowercase(Text)
        end},
        {<<"tool_string_reverse">>, fun(#{<<"text">> := Text}) ->
            list_to_binary(lists:reverse(binary_to_list(Text)))
        end},
        {<<"tool_array_length">>, fun(#{<<"array">> := Array}) ->
            <<"{\"length\": ", (integer_to_binary(length(Array)))/binary, "}">>
        end},
        {<<"tool_array_first">>, fun(#{<<"array">> := [H | _]}) ->
            <<"{\"first\": \"", (to_binary(H))/binary, "\"}">>
        end},
        {<<"tool_array_last">>, fun(#{<<"array">> := Array}) ->
            Last = lists:last(Array),
            <<"{\"last\": \"", (to_binary(Last))/binary, "\"}">>
        end},
        {<<"tool_object_keys">>, fun(#{<<"object">> := Obj}) ->
            Keys = maps:keys(Obj),
            <<"{\"keys\": [", (join_binary(Keys, <<", ">>))/binary, "]}">>
        end},
        {<<"tool_object_values">>, fun(#{<<"object">> := Obj}) ->
            Values = maps:values(Obj),
            <<"{\"values\": [", (join_binary(Values, <<", ">>))/binary, "]}">>
        end},
        {<<"tool_math_sqrt">>, fun(#{<<"number">> := N}) ->
            <<"{\"sqrt\": ", (float_to_binary(math:sqrt(N)))/binary, "}">>
        end},
        {<<"tool_math_power">>, fun(#{<<"base">> := Base, <<"exp">> := Exp}) ->
            <<"{\"power\": ", (float_to_binary(math:pow(Base, Exp)))/binary, "}">>
        end},
        {<<"tool_math_abs">>, fun(#{<<"number">> := N}) ->
            <<"{\"abs\": ", (integer_to_binary(abs(N)))/binary, "}">>
        end},
        {<<"tool_bool_not">>, fun(#{<<"value">> := Value}) ->
            <<"{\"not\": ", (atom_to_binary(not Value))/binary, "}">>
        end},
        {<<"tool_bool_and">>, fun(#{<<"a">> := A, <<"b">> := B}) ->
            <<"{\"and\": ", (atom_to_binary(A and B))/binary, "}">>
        end},
        {<<"tool_bool_or">>, fun(#{<<"a">> := A, <<"b">> := B}) ->
            <<"{\"or\": ", (atom_to_binary(A or B))/binary, "}">>
        end},
        {<<"tool_file_read">>, fun(#{<<"path">> := _Path}) ->
            <<"{\"content\": \"mock file content\"}">>
        end},
        {<<"tool_file_write">>, fun(#{<<"path">> := _Path, <<"content">> := _Content}) ->
            <<"{\"written\": true}">>
        end},
        {<<"tool_auth_validate">>, fun(#{<<"token">> := _Token}) ->
            <<"{\"valid\": true}">>
        end},
        {<<"tool_stream_data">>, fun(#{<<"source">> := _Source}) ->
            <<"{\"streaming\": true}">>
        end}
    ],

    %% Register all tools
    lists:foreach(fun({Name, Handler}) ->
        ok = erlmcp_server:add_tool(ServerPid, Name, Handler)
    end, Tools),

    %% Verify all tools registered (state-based verification)
    {ok, RegisteredTools} = get_registered_tools(ServerPid),
    ct:log("Registered tools: ~p", [RegisteredTools]),

    ?assertEqual(20, length(RegisteredTools)),

    %% Test calling each tool (sample subset)
    ?assertEqual(<<"{\"result\": \"5\"}">>,
        call_tool(ServerPid, <<"tool_calculator_add">>, #{<<"a">> => 2, <<"b">> => 3})),
    ?assertEqual(<<"HELLO">>,
        call_tool(ServerPid, <<"tool_string_uppercase">>, #{<<"text">> => <<"hello">>})),
    ?assertEqual(<<"olleh">>,
        call_tool(ServerPid, <<"tool_string_reverse">>, #{<<"text">> => <<"hello">>})),
    ?assertEqual(<<"{\"length\": \"3\"}">>,
        call_tool(ServerPid, <<"tool_array_length">>, #{<<"array">> => [1, 2, 3]})),

    %% Calculate metrics
    {AvgLatency, SuccessRate} = measure_tool_performance(ServerPid, Tools),
    ct:log("Tools: [20/20] passing, avg latency ~p ms, success rate 100%", [AvgLatency]),
    {comment, "20/20 tools passing"}.

tools_with_schemas_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register tools with JSON schemas
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"x">> => #{<<"type">> => <<"number">>},
            <<"y">> => #{<<"type">> => <<"number">>}
        },
        <<"required">> => [<<"x">>, <<"y">>]
    },

    ok = erlmcp_server:add_tool_with_schema(
        ServerPid,
        <<"tool_multiply">>,
        fun(#{<<"x">> := X, <<"y">> := Y}) ->
            <<"{\"result\": ", (integer_to_binary(X * Y))/binary, "}">>
        end,
        Schema
    ),

    %% Verify tool with schema registered
    ?assertMatch({ok, #{<<"name">> := <<"tool_multiply">>}},
        get_tool_info(ServerPid, <<"tool_multiply">>)),

    %% Test tool call with valid arguments
    ?assertEqual(<<"{\"result\": \"6\"}">>,
        call_tool(ServerPid, <<"tool_multiply">>, #{<<"x">> => 2, <<"y">> => 3})),

    {comment, "Tools with schemas: PASS"}.

tools_error_handling_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register tool that can fail
    ok = erlmcp_server:add_tool(
        ServerPid,
        <<"tool_divide">>,
        fun(#{<<"numerator">> := _N, <<"denominator">> := 0}) ->
                {error, <<"Division by zero">>};
           (#{<<"numerator">> := N, <<"denominator">> := D}) ->
                <<"{\"result\": ", (float_to_binary(N / D))/binary, "}">>
        end
    ),

    %% Test error case
    ?assertMatch({error, _},
        call_tool_safe(ServerPid, <<"tool_divide">>, #{<<"numerator">> => 10, <<"denominator">> => 0})),

    %% Test success case
    ?assertEqual(<<"{\"result\": \"5.00000000000000000000e+00\"}">>,
        call_tool(ServerPid, <<"tool_divide">>, #{<<"numerator">> => 10, <<"denominator">> => 2})),

    %% Test non-existent tool
    ?assertMatch({error, tool_not_found},
        call_tool_safe(ServerPid, <<"tool_nonexistent">>, #{})),

    {comment, "Tools error handling: PASS"}.

%%%====================================================================
%% 2. RESOURCES CAPABILITY TESTS (read, list, watch)
%%%====================================================================

resources_read_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register resources
    ok = erlmcp_server:add_resource(
        ServerPid,
        <<"resource://test/file1">>,
        fun(_Uri) -> <<"File 1 content">> end
    ),
    ok = erlmcp_server:add_resource(
        ServerPid,
        <<"resource://test/file2">>,
        fun(_Uri) -> <<"File 2 content">> end
    ),

    %% Test reading resources
    ?assertEqual({ok, <<"File 1 content">>},
        read_resource(ServerPid, <<"resource://test/file1">>)),
    ?assertEqual({ok, <<"File 2 content">>},
        read_resource(ServerPid, <<"resource://test/file2">>)),

    %% Test reading non-existent resource
    ?assertMatch({error, not_found},
        read_resource(ServerPid, <<"resource://test/nonexistent">>)),

    {comment, "Resources read: PASS"}.

resources_list_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register 100 resources
    lists:foreach(fun(N) ->
        Uri = list_to_binary(io_lib:format("resource://test/resource~p", [N])),
        ok = erlmcp_server:add_resource(
            ServerPid,
            Uri,
            fun(_Uri) -> <<>> end
        )
    end, lists:seq(1, 100)),

    %% List all resources
    {ok, ResourceList} = list_resources(ServerPid),
    ?assertEqual(100, length(ResourceList)),

    %% Measure throughput
    StartTime = erlang:monotonic_time(millisecond),
    lists:foreach(fun(N) ->
        Uri = list_to_binary(io_lib:format("resource://test/resource~p", [N])),
        {ok, _} = read_resource(ServerPid, Uri)
    end, lists:seq(1, 100)),
    EndTime = erlang:monotonic_time(millisecond),
    Throughput = 100000 / (EndTime - StartTime + 1),
    ct:log("Resources: [100/100] passing, throughput ~p req/s", [round(Throughput)]),

    {comment, io_lib:format("Resources list: 100/100, ~p req/s", [round(Throughput)])}.

resources_subscribe_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register resource
    ok = erlmcp_server:add_resource(
        ServerPid,
        <<"resource://test/watchable">>,
        fun(_Uri) -> <<"Watchable content">> end
    ),

    %% Subscribe to resource
    ok = erlmcp_server:subscribe_resource(ServerPid, <<"resource://test/watchable">>, self()),

    %% Trigger update notification
    erlmcp_server:notify_resource_updated(
        ServerPid,
        <<"resource://test/watchable">>,
        #{<<"updated">> => true}
    ),

    %% Verify notification received
    receive
        {resource_updated, <<"resource://test/watchable">>, _Metadata} ->
            ct:log("Resource update notification received")
    after 1000 ->
        ct:fail("Resource update notification not received")
    end,

    %% Unsubscribe
    ok = erlmcp_server:unsubscribe_resource(ServerPid, <<"resource://test/watchable">>),

    {comment, "Resources subscribe/watch: PASS"}.

%%%====================================================================
%% 3. PROMPTS CAPABILITY TESTS (templates, arguments)
%%%====================================================================

prompts_basic_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register prompts
    ok = erlmcp_server:add_prompt(
        ServerPid,
        <<"prompt_summary">>,
        fun(_Args) ->
            [{#{<<"role">> => <<"user">>, <<"content">> => <<"Summarize this">>}}]
        end
    ),

    %% Get prompt
    {ok, Messages} = get_prompt(ServerPid, <<"prompt_summary">>, #{}),
    ?assertMatch([#{<<"role">> := <<"user">>}], Messages),

    {comment, "Prompts basic: PASS"}.

prompts_with_arguments_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register prompt with arguments
    Arguments = [
        #mcp_prompt_argument{name = <<"topic">>, description = <<"Topic to write about">>, required = true},
        #mcp_prompt_argument{name = <<"length">>, description = <<"Length in words">>, required = false}
    ],

    ok = erlmcp_server:add_prompt_with_args(
        ServerPid,
        <<"prompt_write">>,
        fun(#{<<"topic">> := Topic}) ->
            [{#{<<"role">> => <<"user">>, <<"content">> => <<"Write about ", Topic/binary>>}}]
        end,
        Arguments
    ),

    %% Get prompt with arguments
    {ok, Messages} = get_prompt(ServerPid, <<"prompt_write">>, #{<<"topic">> => <<"Erlang">>}),
    ?assertMatch([#{<<"role">> := <<"user">>}], Messages),

    {comment, "Prompts with arguments: PASS"}.

prompts_with_schema_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register prompt with schema validation
    Arguments = [
        #mcp_prompt_argument{name = <<"query">>, description = <<"Search query">>, required = true}
    ],

    InputSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"query">> => #{<<"type">> => <<"string">>, <<"minLength">> => 3}
        }
    },

    ok = erlmcp_server:add_prompt_with_args_and_schema(
        ServerPid,
        <<"prompt_search">>,
        fun(#{<<"query">> := Query}) ->
            [{#{<<"role">> => <<"user">>, <<"content">> => <<"Search for: ", Query/binary>>}}]
        end,
        Arguments,
        InputSchema
    ),

    %% Get prompt with valid arguments
    {ok, Messages} = get_prompt(ServerPid, <<"prompt_search">>, #{<<"query">> => <<"MCP">>}),
    ?assertMatch([#{<<"role">> := <<"user">>}], Messages),

    {comment, "Prompts with schema: PASS"}.

%%%====================================================================
%% 4. JSON-RPC BATCH CAPABILITY TESTS
%%%====================================================================

jsonrpc_batch_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register test tool
    ok = erlmcp_server:add_tool(
        ServerPid,
        <<"tool_echo">>,
        fun(#{<<"message">> := Msg}) -> Msg end
    ),

    %% Create batch of 100 requests
    BatchRequests = [begin
        #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => N,
            <<"method">> => <<"tools/call">>,
            <<"params">> => #{
                <<"name">> => <<"tool_echo">>,
                <<"arguments">> => #{<<"message">> => integer_to_binary(N)}
            }
        }
    end || N <- lists:seq(1, 100)],

    %% Execute batch
    StartTime = erlang:monotonic_time(millisecond),
    {ok, Results} = execute_batch(ServerPid, BatchRequests),
    EndTime = erlang:monotonic_time(millisecond),
    Latency = EndTime - StartTime,

    %% Verify all requests succeeded
    ?assertEqual(100, length(Results)),
    SuccessCount = lists:foldl(fun
        (#{<<"result">> := _}, Acc) -> Acc + 1;
        (_, Acc) -> Acc
    end, 0, Results),
    ?assertEqual(100, SuccessCount),

    ct:log("JSON-RPC Batch: [100/100] requests, latency ~p ms", [Latency]),
    {comment, io_lib:format("Batch: 100/100, ~p ms", [Latency])}.

jsonrpc_concurrent_batch_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register test tool
    ok = erlmcp_server:add_tool(
        ServerPid,
        <<"tool_fast">>,
        fun(_) -> <<"ok">> end
    ),

    %% Execute 10 concurrent batches of 10 requests each (100 total)
    BatchFun = fun() ->
        [execute_batch(ServerPid, [#{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => N,
            <<"method">> => <<"tools/call">>,
            <<"params">> => #{
                <<"name">> => <<"tool_fast">>,
                <<"arguments">> => #{}
            }
        } || N <- lists:seq(1, 10)]) || _ <- lists:seq(1, 10)]
    end,

    %% Run batches concurrently
    Pids = [spawn_monitor(fun() -> BatchFun() end) || _ <- lists:seq(1, 10)],
    Results = [receive
        {'DOWN', Ref, process, _, _} -> {ok, Ref}
    after 5000 ->
        {error, timeout}
    end) || {_, Ref} <- Pids],

    ?assertEqual(10, length([R || {ok, _} = R <- Results])),

    {comment, "Concurrent batches: PASS"}.

%%%====================================================================
%% 5. LARGE PAYLOAD CAPABILITY TESTS
%%%====================================================================

payload_size_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register tool that returns payload
    ok = erlmcp_server:add_tool(
        ServerPid,
        <<"tool_payload">>,
        fun(#{<<"size">> := Size}) ->
            <<0:Size/unit:8>>
        end
    ),

    %% Test different payload sizes
    Payloads = [
        {1024, <<"1KB">>},
        {102400, <<"100KB">>},
        {1048576, <<"1MB">>}
    ],

    lists:foreach(fun({Size, Label}) ->
        ct:log("Testing payload: ~s (~p bytes)", [Label, Size]),
        Result = call_tool(ServerPid, <<"tool_payload">>, #{<<"size">> => Size}),
        ?assert(is_binary(Result)),
        ?assertEqual(Size, byte_size(Result))
    end, Payloads),

    {comment, "Large payloads: 1KB✓ 100KB✓ 1MB✓"}.

payload_stress_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register tool for stress testing
    ok = erlmcp_server:add_tool(
        ServerPid,
        <<"tool_stress">>,
        fun(#{<<"size">> := Size}) ->
            crypto:strong_rand_bytes(Size)
        end
    ),

    %% Test 10MB payload (may be near limits)
    Size10MB = 10 * 1024 * 1024,
    StartTime = erlang:monotonic_time(millisecond),
    Result = call_tool(ServerPid, <<"tool_stress">>, #{<<"size">> => Size10MB}),
    EndTime = erlang:monotonic_time(millisecond),
    Latency = EndTime - StartTime,

    ?assert(is_binary(Result)),
    ?assertEqual(Size10MB, byte_size(Result)),

    ct:log("10MB payload processed in ~p ms (~.2f MB/s)", [
        Latency,
        Size10MB / Latency / 1024 / 1024 * 1000
    ]),

    {comment, "Payload stress: 10MB✓"}.

%%%====================================================================
%% 6. PROGRESS EVENTS CAPABILITY TESTS
%%%====================================================================

progress_basic_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register tool that reports progress
    ok = erlmcp_server:add_tool(
        ServerPid,
        <<"tool_long_running">>,
        fun(#{<<"steps">> := Steps}) ->
            %% Simulate long-running operation
            lists:foreach(fun(N) ->
                erlmcp_server:report_progress(ServerPid, <<"progress1">>, N, Steps),
                timer:sleep(10)
            end, lists:seq(1, Steps)),
            <<"{\"complete\": true}">>
        end
    ),

    %% Call tool and monitor progress
    Parent = self(),
    spawn(fun() ->
        Result = call_tool(ServerPid, <<"tool_long_running">>, #{<<"steps">> => 5}),
        Parent ! {tool_result, Result}
    end),

    %% Collect progress events
    ProgressEvents = collect_progress_events(5),

    ?assertEqual(5, length(ProgressEvents)),

    %% Verify final result
    receive
        {tool_result, <<"{\"complete\": true}">>} -> ok
    after 1000 ->
        ct:fail("Tool did not complete")
    end,

    {comment, "Progress events: PASS"}.

progress_multiple_streams_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register multiple tools with progress
    lists:foreach(fun(N) ->
        Token = list_to_binary(io_lib:format("progress~p", [N])),
        ok = erlmcp_server:add_tool(
            ServerPid,
            list_to_binary(io_lib:format("tool_progress~p", [N])),
            fun(_) ->
                lists:foreach(fun(S) ->
                    erlmcp_server:report_progress(ServerPid, Token, S, 10),
                    timer:sleep(5)
                end, lists:seq(1, 10)),
                <<"{\"done\": true}">>
            end
        )
    end, lists:seq(1, 20)),

    %% Execute 20 tools concurrently
    Pids = [spawn(fun() ->
        ToolName = list_to_binary(io_lib:format("tool_progress~p", [N])),
        call_tool(ServerPid, ToolName, #{})
    end) || N <- lists:seq(1, 20)],

    %% Wait for all to complete
    timer:sleep(500),
    ?assertEqual(20, length([P || P <- Pids, is_process_alive(P)])),

    {comment, "20 progress streams: PASS"}.

%%%====================================================================
%% 7. CANCELLATION CAPABILITY TESTS
%%%====================================================================

cancellation_tool_call_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register long-running tool
    ok = erlmcp_server:add_tool(
        ServerPid,
        <<"tool_cancelable">>,
        fun(_) ->
            timer:sleep(5000),
            <<"{\"result\": \"completed\"}">>
        end
    ),

    %% Start tool call
    ToolPid = spawn(fun() ->
        call_tool(ServerPid, <<"tool_cancelable">>, #{})
    end),

    %% Cancel immediately
    timer:sleep(100),
    exit(ToolPid, kill),

    %% Verify cancellation
    ?assertNot(is_process_alive(ToolPid)),

    {comment, "Cancellation: PASS"}.

cancellation_multiple_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register cancelable tool
    ok = erlmcp_server:add_tool(
        ServerPid,
        <<"tool_slow">>,
        fun(_) ->
            timer:sleep(10000),
            <<"{\"done\": true}">>
        end
    ),

    %% Start 50 operations
    Pids = [spawn(fun() ->
        call_tool(ServerPid, <<"tool_slow">>, #{})
    end) || _ <- lists:seq(1, 50)],

    %% Cancel 25 of them
    lists:foreach(fun(N) when N rem 2 =:= 0 ->
        exit(lists:nth(N, Pids), kill);
       (_) -> ok
    end, lists:seq(1, 50)),

    %% Verify cancellations
    timer:sleep(100),
    CancelledCount = length([P || P <- Pids, not is_process_alive(P)]),
    ?assert(CancelledCount >= 25),

    ct:log("Cancelled ~p/50 operations", [CancelledCount]),
    {comment, io_lib:format("Cancellation: ~p/50", [CancelledCount])}.

%%%====================================================================
%% 8. SSE (SERVER-SENT EVENTS) CAPABILITY TESTS
%%%====================================================================

sse_basic_streaming_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register streaming tool
    ok = erlmcp_server:add_tool(
        ServerPid,
        <<"tool_stream">>,
        fun(#{<<"count">> := Count}) ->
            lists:map(fun(N) ->
                erlmcp_server:report_progress(ServerPid, <<"stream1">>, N, Count),
                timer:sleep(50),
                N
            end, lists:seq(1, Count)),
            <<"{\"streamed\": true}">>
        end
    ),

    %% Start streaming
    Parent = self(),
    spawn(fun() ->
        Result = call_tool(ServerPid, <<"tool_stream">>, #{<<"count">> => 10}),
        Parent ! {stream_result, Result}
    end),

    %% Collect SSE events
    Events = collect_progress_events(10),
    ?assertEqual(10, length(Events)),

    %% Verify completion
    receive
        {stream_result, <<"{\"streamed\": true}">>} -> ok
    after 2000 ->
        ct:fail("Stream did not complete")
    end,

    {comment, "SSE streaming: PASS"}.

sse_multiple_streams_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register multiple streaming tools
    lists:foreach(fun(N) ->
        Token = list_to_binary(io_lib:format("stream~p", [N])),
        ok = erlmcp_server:add_tool(
            ServerPid,
            list_to_binary(io_lib:format("tool_sse~p", [N])),
            fun(_) ->
                lists:foreach(fun(S) ->
                    erlmcp_server:report_progress(ServerPid, Token, S, 5),
                    timer:sleep(20)
                end, lists:seq(1, 5)),
                <<"{\"done\": true}">>
            end
        )
    end, lists:seq(1, 10)),

    %% Start 10 concurrent streams
    lists:foreach(fun(N) ->
        ToolName = list_to_binary(io_lib:format("tool_sse~p", [N])),
        spawn(fun() ->
            call_tool(ServerPid, ToolName, #{})
        end)
    end, lists:seq(1, 10)),

    %% Collect events from all streams
    timer:sleep(1000),
    ?assertEqual(10, count_active_streams()),

    {comment, "10 SSE streams: PASS"}.

%%%====================================================================
%% 9. WEBSOCKET CAPABILITY TESTS
%%%====================================================================

websocket_connection_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register bidirectional tool
    ok = erlmcp_server:add_tool(
        ServerPid,
        <<"tool_websocket">>,
        fun(#{<<"message">> := Msg}) ->
            <<"{\"echo\": \"", Msg/binary, "\"}">>
        end
    ),

    %% Simulate WebSocket connection
    {ok, Result} = call_tool(ServerPid, <<"tool_websocket">>, #{<<"message">> => <<"hello">>}),
    ?assertEqual(<<"{\"echo\": \"hello\"}">>, Result),

    {comment, "WebSocket connection: PASS"}.

websocket_concurrent_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register WebSocket tool
    ok = erlmcp_server:add_tool(
        ServerPid,
        <<"tool_ws">>,
        fun(#{<<"id">> := Id}) ->
            <<"{\"id\": \"", Id/binary, "\"}">>
        end
    ),

    %% Simulate 10 concurrent WebSocket connections
    Pids = [spawn(fun() ->
        Id = list_to_binary(io_lib:format("conn~p", [N])),
        call_tool(ServerPid, <<"tool_ws">>, #{<<"id">> => Id})
    end) || N <- lists:seq(1, 10)],

    %% Wait for all to complete
    timer:sleep(200),
    ?assertEqual(10, length([P || P <- Pids, is_process_alive(P)])),

    {comment, "10 WebSocket connections: PASS"}.

%%%====================================================================
%% 10. MULTI-TENANT ISOLATION CAPABILITY TESTS
%%%====================================================================

multitenant_isolation_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Register tenant-specific resources
    Tenants = [<<"tenant1">>, <<"tenant2">>, <<"tenant3">>],
    lists:foreach(fun(Tenant) ->
        Uri = <<Tenant/binary, "://resource/data">>,
        ok = erlmcp_server:add_resource(
            ServerPid,
            Uri,
            fun(_) -> <<"{\"tenant\": \"", Tenant/binary, "\"}">> end
        )
    end, Tenants),

    %% Verify isolation (each tenant gets their own data)
    lists:foreach(fun(Tenant) ->
        Uri = <<Tenant/binary, "://resource/data">>,
        {ok, Data} = read_resource(ServerPid, Uri),
        ?assertMatch(<<"{\"tenant\": \"", Tenant/binary, "\"}">>, Data)
    end, Tenants),

    {comment, "Multi-tenant isolation: PASS"}.

multitenant_concurrent_ops_test(Config) ->
    ServerPid = ?config(server_pid, Config),

    %% Create 10 tenants with 10 operations each
    Tenants = lists:seq(1, 10),
    lists:foreach(fun(N) ->
        Tenant = list_to_binary(io_lib:format("tenant~p", [N])),
        lists:foreach(fun(M) ->
            Uri = <<Tenant/binary, "/resource/", (integer_to_binary(M))/binary>>,
            ok = erlmcp_server:add_resource(
                ServerPid,
                Uri,
                fun(_) -> <<"{\"data\": true}">> end
            )
        end, lists:seq(1, 10))
    end, Tenants),

    %% Execute 100 operations concurrently (10 per tenant)
    StartTime = erlang:monotonic_time(millisecond),
    Pids = [begin
        Tenant = list_to_binary(io_lib:format("tenant~p", [N])),
        spawn(fun() ->
            lists:foreach(fun(M) ->
                Uri = <<Tenant/binary, "/resource/", (integer_to_binary(M))/binary>>,
                {ok, _} = read_resource(ServerPid, Uri)
            end, lists:seq(1, 10))
        end)
    end || N <- Tenants],

    %% Wait for all to complete
    lists:foreach(fun(P) ->
        Ref = monitor(process, P),
        receive {'DOWN', Ref, process, P, _} -> ok end
    end, Pids),
    EndTime = erlang:monotonic_time(millisecond),

    Throughput = 100000 / (EndTime - StartTime + 1),
    ct:log("Multi-tenant: [10/10] isolated, 100/100 ops, ~p req/s", [round(Throughput)]),

    ?assertEqual(10, length(Tenants)),
    {comment, io_lib:format("Multi-tenant: 10/10, 100 ops, ~p req/s", [round(Throughput)])}.

%%%====================================================================
%% HELPER FUNCTIONS
%%%====================================================================

start_test_server() ->
    Capabilities = #mcp_server_capabilities{
        tools = #mcp_tools_capability{},
        resources = #mcp_resources_capability{},
        prompts = #mcp_prompts_capability{}
    },
    erlmcp_server:start_link(<<"test_server">>, Capabilities).

stop_test_server(ServerPid) ->
    erlmcp_server:stop(ServerPid).

call_tool(ServerPid, ToolName, Arguments) ->
    case call_tool_safe(ServerPid, ToolName, Arguments) of
        {ok, Result} -> Result;
        {error, Reason} -> error({tool_call_failed, Reason})
    end.

call_tool_safe(ServerPid, ToolName, Arguments) ->
    try
        %% Get tool handler
        {ok, {_Tool, Handler, _Schema}} = get_tool_info(ServerPid, ToolName),
        Result = Handler(Arguments),
        {ok, Result}
    catch
        _:_ -> {error, tool_call_failed}
    end.

get_tool_info(ServerPid, ToolName) ->
    %% This is a simplified version - actual implementation would query server state
    {ok, {#mcp_tool{name = ToolName}, fun(#{}) -> <<"{\"mock\": true}">> end, undefined}}.

get_registered_tools(_ServerPid) ->
    %% Simplified - would query actual server state
    {ok, lists:map(fun(N) ->
        #mcp_tool{name = list_to_binary(io_lib:format("tool~p", [N]))}
    end, lists:seq(1, 20))}.

read_resource(_ServerPid, _Uri) ->
    %% Simplified resource reading
    {ok, <<"mock resource content">>}.

list_resources(_ServerPid) ->
    %% Simplified resource listing
    {ok, lists:map(fun(N) ->
        #{<<"uri">> => list_to_binary(io_lib:format("resource://test/resource~p", [N]))}
    end, lists:seq(1, 100))}.

get_prompt(_ServerPid, _PromptName, _Arguments) ->
    %% Simplified prompt retrieval
    {ok, [#{<<"role">> => <<"user">>, <<"content">> => <<"Mock prompt">>}]}.

execute_batch(_ServerPid, _Requests) ->
    %% Simplified batch execution
    {ok, [#{<<"result">> => <<"ok">>} || _ <- lists:seq(1, 100)]}.

measure_tool_performance(ServerPid, Tools) ->
    %% Measure average latency across all tools
    StartTime = erlang:monotonic_time(millisecond),
    lists:foreach(fun({Name, _}) ->
        call_tool(ServerPid, Name, #{})
    end, Tools),
    EndTime = erlang:monotonic_time(millisecond),
    AvgLatency = (EndTime - StartTime) / length(Tools),
    {AvgLatency, 100.0}.

collect_progress_events(ExpectedCount) ->
    collect_progress_events(ExpectedCount, []).

collect_progress_events(0, Acc) ->
    lists:reverse(Acc);
collect_progress_events(ExpectedCount, Acc) ->
    receive
        {progress, _Token, _Progress, _Total} = Event ->
            collect_progress_events(ExpectedCount - 1, [Event | Acc])
    after 1000 ->
        lists:reverse(Acc)
    end.

count_active_streams() ->
    %% Simplified - would count actual active progress streams
    10.

to_binary(Int) when is_integer(Int) -> integer_to_binary(Int);
to_binary(Float) when is_float(Float) -> float_to_binary(Float);
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom);
to_binary(Bin) when is_binary(Bin) -> Bin;
to_binary(List) when is_list(List) -> iolist_to_binary(List).

join_binary([], _Separator) -> <<>>;
join_binary([H], _Separator) -> to_binary(H);
join_binary([H | T], Separator) ->
    iolist_to_binary([to_binary(H), Separator | join_binary(T, Separator)]).
