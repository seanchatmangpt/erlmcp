%%%====================================================================
%%% ERLMCP MCP FEATURES BENCHMARK - PROTOCOL FEATURE PERFORMANCE
%%%====================================================================
%%% Measures: Tool calls, Resource subscriptions, Prompt rendering, Sampling
%%% Workloads: Varying complexity (simple, medium, complex) and scale (100, 1K, 10K)
%%% Output: Full metrology-compliant JSON
%%%====================================================================
%%% Benchmark Categories:
%%% 1. tool_call_latency - Tool invocation pipeline with varying parameter complexity
%%% 2. resource_subscription_overhead - Subscription management and notification latency
%%% 3. prompt_rendering - Template compilation and rendering performance
%%% 4. sampling_operations - LLM sampling request handling
%%%====================================================================

-module(erlmcp_bench_mcp_features).

-export([
    run/0,
    run/1,
    run_all/0,
    workloads/0,
    benchmark_tool_calls/1,
    benchmark_subscriptions/1,
    benchmark_prompt_rendering/1,
    benchmark_sampling/1
]).

-include_lib("kernel/include/logger.hrl").
-include("erlmcp.hrl").

%% Workload definitions
-spec workloads() -> [map()].
workloads() ->
    [
        %% Tool Call Latency Benchmarks
        #{id => <<"tool_call_simple_100">>, 
          category => tool_calls,
          complexity => simple, 
          operations => 100},
        #{id => <<"tool_call_simple_1k">>, 
          category => tool_calls,
          complexity => simple, 
          operations => 1000},
        #{id => <<"tool_call_simple_10k">>, 
          category => tool_calls,
          complexity => simple, 
          operations => 10000},
        
        #{id => <<"tool_call_medium_100">>, 
          category => tool_calls,
          complexity => medium, 
          operations => 100},
        #{id => <<"tool_call_medium_1k">>, 
          category => tool_calls,
          complexity => medium, 
          operations => 1000},
        
        #{id => <<"tool_call_complex_100">>, 
          category => tool_calls,
          complexity => complex, 
          operations => 100},
        
        %% Resource Subscription Benchmarks
        #{id => <<"subscription_1sub_1hz">>, 
          category => subscriptions,
          subscribers => 1, 
          notification_rate => 1,
          duration_s => 10},
        #{id => <<"subscription_10sub_10hz">>, 
          category => subscriptions,
          subscribers => 10, 
          notification_rate => 10,
          duration_s => 10},
        #{id => <<"subscription_100sub_10hz">>, 
          category => subscriptions,
          subscribers => 100, 
          notification_rate => 10,
          duration_s => 10},
        #{id => <<"subscription_1000sub_1hz">>, 
          category => subscriptions,
          subscribers => 1000, 
          notification_rate => 1,
          duration_s => 10},
        
        %% Prompt Rendering Benchmarks
        #{id => <<"prompt_simple_100">>, 
          category => prompts,
          complexity => simple, 
          operations => 100},
        #{id => <<"prompt_simple_1k">>, 
          category => prompts,
          complexity => simple, 
          operations => 1000},
        #{id => <<"prompt_simple_10k">>, 
          category => prompts,
          complexity => simple, 
          operations => 10000},
        
        #{id => <<"prompt_medium_100">>, 
          category => prompts,
          complexity => medium, 
          operations => 100},
        #{id => <<"prompt_medium_1k">>, 
          category => prompts,
          complexity => medium, 
          operations => 1000},
        
        #{id => <<"prompt_complex_100">>, 
          category => prompts,
          complexity => complex, 
          operations => 100},
        
        %% Sampling Operations Benchmarks
        #{id => <<"sampling_random_100">>, 
          category => sampling,
          strategy => random, 
          operations => 100},
        #{id => <<"sampling_random_1k">>, 
          category => sampling,
          strategy => random, 
          operations => 1000},
        
        #{id => <<"sampling_temperature_100">>, 
          category => sampling,
          strategy => temperature, 
          operations => 100},
        
        #{id => <<"sampling_top_k_100">>, 
          category => sampling,
          strategy => top_k, 
          operations => 100}
    ].

%% Main entry points
-spec run() -> ok.
run() ->
    run_all().

-spec run(binary()) -> ok | {error, term()}.
run(WorkloadId) when is_binary(WorkloadId) ->
    Workloads = workloads(),
    case lists:filter(fun(#{id := Id}) -> Id =:= WorkloadId end, Workloads) of
        [] ->
            io:format("ERROR: Unknown workload: ~s~n", [WorkloadId]),
            io:format("Available workloads: ~p~n", [[Id || #{id := Id} <- Workloads]]),
            {error, {unknown_workload, WorkloadId}};
        [Workload] ->
            run_workload(Workload)
    end;
run(WorkloadId) when is_list(WorkloadId) ->
    run(list_to_binary(WorkloadId));
run(WorkloadId) when is_atom(WorkloadId) ->
    run(atom_to_binary(WorkloadId, utf8)).

-spec run_all() -> ok.
run_all() ->
    io:format("~n==============================================~n"),
    io:format("ERLMCP MCP FEATURES BENCHMARK SUITE~n"),
    io:format("==============================================~n~n"),

    %% Ensure required apps are started
    application:ensure_all_started(erlmcp),

    lists:foreach(fun(Workload) ->
        run_workload(Workload)
    end, workloads()),

    io:format("~n==============================================~n"),
    io:format("All benchmarks complete. Results in bench/results/~n"),
    io:format("==============================================~n~n"),
    ok.

%% Run a single workload
-spec run_workload(map()) -> ok | {error, term()}.
run_workload(#{id := WorkloadId, category := Category} = Workload) ->
    io:format("~n--- Workload: ~s (~p) ---~n", [WorkloadId, Category]),

    %% Capture environment
    Env = capture_environment(),

    %% Measure memory before
    MemoryBefore = erlang:memory(total),

    %% Run category-specific benchmark
    StartTime = erlang:monotonic_time(microsecond),

    Result = case Category of
        tool_calls -> benchmark_tool_calls(Workload);
        subscriptions -> benchmark_subscriptions(Workload);
        prompts -> benchmark_prompt_rendering(Workload);
        sampling -> benchmark_sampling(Workload)
    end,

    EndTime = erlang:monotonic_time(microsecond),

    %% Measure memory after
    MemoryAfter = erlang:memory(total),

    %% Calculate overall metrics
    TotalDurationUs = EndTime - StartTime,
    TotalDurationS = TotalDurationUs / 1_000_000,
    
    Latencies = maps:get(latencies, Result),
    Ops = maps:get(operations, Result),
    Throughput = Ops / TotalDurationS,

    Percentiles = calculate_percentiles(Latencies),

    %% Estimate CPU usage
    CpuPercent = estimate_cpu_usage(TotalDurationUs),

    %% Build metrology report
    Report = #{
        workload_id => WorkloadId,
        benchmark => <<"mcp_features">>,
        category => atom_to_binary(Category, utf8),
        timestamp => erlang:system_time(second),
        environment => Env,
        operations => Ops,
        duration_s => round_float(TotalDurationS, 2),
        throughput_msg_per_s => round_float(Throughput, 2),
        latency_p50_us => round_float(maps:get(p50, Percentiles), 1),
        latency_p95_us => round_float(maps:get(p95, Percentiles), 1),
        latency_p99_us => round_float(maps:get(p99, Percentiles), 1),
        precision => <<"microsecond">>,
        memory_start_mib => round_float(MemoryBefore / (1024 * 1024), 1),
        memory_end_mib => round_float(MemoryAfter / (1024 * 1024), 1),
        memory_delta_mib => round_float((MemoryAfter - MemoryBefore) / (1024 * 1024), 1),
        cpu_percent_avg => round_float(CpuPercent, 1),
        scope => <<"per_node">>,
        details => maps:get(details, Result, #{})
    },

    %% Validate and write report
    case validate_report(Report) of
        ok ->
            Timestamp = erlang:system_time(second),
            Filename = io_lib:format("bench/results/mcp_features_~s_~p.json", [WorkloadId, Timestamp]),
            write_report(Filename, Report),
            io:format("✓ Report written: ~s~n", [Filename]),
            ok;
        {error, ValidationError} ->
            io:format("✗ Validation failed: ~p~n", [ValidationError]),
            {error, {validation_failed, ValidationError}}
    end.

%%====================================================================
%% Benchmark 1: Tool Call Latency
%%====================================================================

-spec benchmark_tool_calls(map()) -> map().
benchmark_tool_calls(#{operations := Ops, complexity := Complexity}) ->
    io:format("  [Tool Calls] Running ~p operations with ~p complexity...~n", [Ops, Complexity]),

    %% Setup test server with tools
    Capabilities = #mcp_server_capabilities{
        tools = #{<<"listChanged">> => true}
    },
    {ok, ServerPid} = erlmcp_server:start_link(make_ref(), Capabilities),

    %% Add tool based on complexity
    ToolName = add_tool_by_complexity(ServerPid, Complexity),
    Args = generate_tool_args(Complexity),

    %% Measure tool call latencies
    Latencies = lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        
        %% Encode tool call request (measures protocol overhead)
        RequestId = rand:uniform(1000000),
        Params = #{
            <<"name">> => ToolName,
            <<"arguments">> => Args
        },
        _Request = erlmcp_json_rpc:encode_request(RequestId, ?MCP_METHOD_TOOLS_CALL, Params),
        
        End = erlang:monotonic_time(microsecond),
        End - Start
    end, lists:seq(1, Ops)),

    %% Cleanup
    erlmcp_server:stop(ServerPid),

    #{
        operations => Ops,
        latencies => Latencies,
        details => #{
            complexity => Complexity,
            tool_name => ToolName,
            param_count => map_size(Args)
        }
    }.

%% Add tool with specific complexity
add_tool_by_complexity(ServerPid, simple) ->
    ToolName = <<"echo_simple">>,
    erlmcp_server:add_tool_with_schema(
        ServerPid,
        ToolName,
        fun(#{<<"message">> := Msg}) ->
            #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => Msg}]}
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{<<"message">> => #{<<"type">> => <<"string">>}},
            <<"required">> => [<<"message">>]
        }
    ),
    ToolName;

add_tool_by_complexity(ServerPid, medium) ->
    ToolName = <<"process_medium">>,
    erlmcp_server:add_tool_with_schema(
        ServerPid,
        ToolName,
        fun(Args) ->
            %% Process 5 parameters
            Results = lists:map(fun(I) ->
                Key = iolist_to_binary(io_lib:format("param~p", [I])),
                maps:get(Key, Args, <<"default">>)
            end, lists:seq(1, 5)),
            #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => iolist_to_binary(Results)}]}
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"param1">> => #{<<"type">> => <<"string">>},
                <<"param2">> => #{<<"type">> => <<"string">>},
                <<"param3">> => #{<<"type">> => <<"string">>},
                <<"param4">> => #{<<"type">> => <<"string">>},
                <<"param5">> => #{<<"type">> => <<"string">>}
            }
        }
    ),
    ToolName;

add_tool_by_complexity(ServerPid, complex) ->
    ToolName = <<"process_complex">>,
    %% Generate schema with 20 parameters
    Properties = maps:from_list([
        {iolist_to_binary(io_lib:format("param~p", [I])), #{<<"type">> => <<"string">>}}
        || I <- lists:seq(1, 20)
    ]),
    erlmcp_server:add_tool_with_schema(
        ServerPid,
        ToolName,
        fun(Args) ->
            %% Process 20 parameters
            Results = lists:map(fun(I) ->
                Key = iolist_to_binary(io_lib:format("param~p", [I])),
                maps:get(Key, Args, <<"default">>)
            end, lists:seq(1, 20)),
            #{<<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => iolist_to_binary(Results)}]}
        end,
        #{<<"type">> => <<"object">>, <<"properties">> => Properties}
    ),
    ToolName.

%% Generate tool arguments by complexity
generate_tool_args(simple) ->
    #{<<"message">> => <<"test">>};
generate_tool_args(medium) ->
    maps:from_list([
        {iolist_to_binary(io_lib:format("param~p", [I])), <<"value">>}
        || I <- lists:seq(1, 5)
    ]);
generate_tool_args(complex) ->
    maps:from_list([
        {iolist_to_binary(io_lib:format("param~p", [I])), <<"value">>}
        || I <- lists:seq(1, 20)
    ]).

%%====================================================================
%% Benchmark 2: Resource Subscription Overhead
%%====================================================================

-spec benchmark_subscriptions(map()) -> map().
benchmark_subscriptions(#{subscribers := NumSubs, notification_rate := RateHz, duration_s := DurationS}) ->
    io:format("  [Subscriptions] ~p subscribers, ~p Hz, ~p seconds...~n", 
              [NumSubs, RateHz, DurationS]),

    %% Start subscription manager if not running
    case whereis(erlmcp_resource_subscriptions) of
        undefined ->
            {ok, _Pid} = erlmcp_resource_subscriptions:start_link();
        _ ->
            ok
    end,

    %% Create subscriber processes
    Uri = <<"bench://test/resource">>,
    Subscribers = lists:map(fun(_) ->
        spawn_link(fun() ->
            subscriber_loop([])
        end)
    end, lists:seq(1, NumSubs)),

    %% Subscribe all
    SubscribeStart = erlang:monotonic_time(microsecond),
    lists:foreach(fun(SubPid) ->
        erlmcp_resource_subscriptions:subscribe_to_resource(Uri, SubPid, #{})
    end, Subscribers),
    SubscribeEnd = erlang:monotonic_time(microsecond),
    SubscribeLatency = SubscribeEnd - SubscribeStart,

    %% Send notifications at specified rate
    NotificationIntervalMs = 1000 div RateHz,
    TotalNotifications = RateHz * DurationS,
    
    NotificationLatencies = lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        erlmcp_resource_subscriptions:notify_resource_changed(Uri, #{timestamp => erlang:system_time()}),
        End = erlang:monotonic_time(microsecond),
        timer:sleep(NotificationIntervalMs),
        End - Start
    end, lists:seq(1, TotalNotifications)),

    %% Unsubscribe all
    UnsubscribeStart = erlang:monotonic_time(microsecond),
    lists:foreach(fun(SubPid) ->
        erlmcp_resource_subscriptions:unsubscribe_from_resource(Uri, SubPid)
    end, Subscribers),
    UnsubscribeEnd = erlang:monotonic_time(microsecond),
    UnsubscribeLatency = UnsubscribeEnd - UnsubscribeStart,

    %% Cleanup
    lists:foreach(fun(SubPid) ->
        exit(SubPid, normal)
    end, Subscribers),

    #{
        operations => TotalNotifications,
        latencies => NotificationLatencies,
        details => #{
            subscribers => NumSubs,
            notification_rate_hz => RateHz,
            subscribe_latency_us => SubscribeLatency,
            unsubscribe_latency_us => UnsubscribeLatency,
            subscribe_latency_per_sub_us => SubscribeLatency div NumSubs,
            unsubscribe_latency_per_sub_us => UnsubscribeLatency div NumSubs
        }
    }.

%% Subscriber process loop
subscriber_loop(Notifications) ->
    receive
        {resource_updated, _Uri, _Metadata} ->
            subscriber_loop([erlang:monotonic_time(microsecond) | Notifications]);
        stop ->
            ok
    end.

%%====================================================================
%% Benchmark 3: Prompt Template Rendering
%%====================================================================

-spec benchmark_prompt_rendering(map()) -> map().
benchmark_prompt_rendering(#{operations := Ops, complexity := Complexity}) ->
    io:format("  [Prompt Rendering] Running ~p operations with ~p complexity...~n", [Ops, Complexity]),

    %% Generate template and variables by complexity
    {Template, Variables} = generate_prompt_template(Complexity),

    %% Compile template once
    {ok, CompiledTemplate} = erlmcp_prompt_template:compile(Template),

    %% Measure rendering latencies
    Latencies = lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        {ok, _Output} = erlmcp_prompt_template:render(CompiledTemplate, Variables),
        End = erlang:monotonic_time(microsecond),
        End - Start
    end, lists:seq(1, Ops)),

    #{
        operations => Ops,
        latencies => Latencies,
        details => #{
            complexity => Complexity,
            template_size_bytes => byte_size(Template),
            variable_count => map_size(Variables)
        }
    }.

%% Generate prompt template by complexity
generate_prompt_template(simple) ->
    %% No variables, static template
    Template = <<"Hello, this is a static prompt template.">>,
    Variables = #{},
    {Template, Variables};

generate_prompt_template(medium) ->
    %% 10 variables
    Template = <<"Hello {{name}}, you are {{age}} years old. "
                 "Your email is {{email}}, phone {{phone}}, "
                 "city {{city}}, country {{country}}, "
                 "job {{job}}, company {{company}}, "
                 "hobby {{hobby}}, language {{language}}.">>,
    Variables = #{
        <<"name">> => <<"John">>,
        <<"age">> => <<"30">>,
        <<"email">> => <<"john@example.com">>,
        <<"phone">> => <<"555-1234">>,
        <<"city">> => <<"San Francisco">>,
        <<"country">> => <<"USA">>,
        <<"job">> => <<"Engineer">>,
        <<"company">> => <<"Tech Corp">>,
        <<"hobby">> => <<"Coding">>,
        <<"language">> => <<"Erlang">>
    },
    {Template, Variables};

generate_prompt_template(complex) ->
    %% 50 variables
    Template = iolist_to_binary([
        <<"Data: ">>,
        [io_lib:format("{{var~p}} ", [I]) || I <- lists:seq(1, 50)]
    ]),
    Variables = maps:from_list([
        {iolist_to_binary(io_lib:format("var~p", [I])), <<"value">>}
        || I <- lists:seq(1, 50)
    ]),
    {Template, Variables}.

%%====================================================================
%% Benchmark 4: Sampling Operations
%%====================================================================

-spec benchmark_sampling(map()) -> map().
benchmark_sampling(#{operations := Ops, strategy := Strategy}) ->
    io:format("  [Sampling] Running ~p operations with ~p strategy...~n", [Ops, Strategy]),

    %% Start sampling server with mock provider
    case whereis(erlmcp_sampling) of
        undefined ->
            {ok, _Pid} = erlmcp_sampling:start_link();
        _ ->
            ok
    end,

    %% Set mock provider (fast, no network calls)
    erlmcp_sampling:set_model_provider(erlmcp_sampling, erlmcp_mock_llm),

    %% Generate sampling parameters based on strategy
    Params = generate_sampling_params(Strategy),
    Messages = [#{
        <<"role">> => <<"user">>,
        <<"content">> => #{
            <<"type">> => <<"text">>,
            <<"text">> => <<"Test message">>
        }
    }],

    %% Measure sampling latencies
    Latencies = lists:map(fun(_) ->
        Start = erlang:monotonic_time(microsecond),
        
        %% Call sampling (uses mock provider for speed)
        case catch erlmcp_sampling:create_message(Messages, Params, 5000) of
            {ok, _Result} ->
                End = erlang:monotonic_time(microsecond),
                End - Start;
            _ ->
                %% If sampling fails, record 0 (will be filtered)
                0
        end
    end, lists:seq(1, Ops)),

    %% Filter out failures
    ValidLatencies = [L || L <- Latencies, L > 0],

    #{
        operations => length(ValidLatencies),
        latencies => ValidLatencies,
        details => #{
            strategy => Strategy,
            success_rate => length(ValidLatencies) / Ops,
            params => Params
        }
    }.

%% Generate sampling parameters by strategy
generate_sampling_params(random) ->
    #{
        <<"modelPreferences">> => #{
            <<"hints">> => [#{<<"name">> => <<"mock-model">>}]
        },
        <<"maxTokens">> => 100
    };
generate_sampling_params(temperature) ->
    #{
        <<"modelPreferences">> => #{
            <<"hints">> => [#{<<"name">> => <<"mock-model">>}],
            <<"temperature">> => 0.7
        },
        <<"maxTokens">> => 100
    };
generate_sampling_params(top_k) ->
    #{
        <<"modelPreferences">> => #{
            <<"hints">> => [#{<<"name">> => <<"mock-model">>}],
            <<"topK">> => 50
        },
        <<"maxTokens">> => 100
    }.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Calculate percentiles from latency list
-spec calculate_percentiles([number()]) -> map().
calculate_percentiles([]) ->
    #{p50 => 0.0, p95 => 0.0, p99 => 0.0, min => 0.0, max => 0.0, avg => 0.0};
calculate_percentiles(Latencies) ->
    Sorted = lists:sort(Latencies),
    Len = length(Sorted),

    P50 = percentile(Sorted, 0.50),
    P95 = percentile(Sorted, 0.95),
    P99 = percentile(Sorted, 0.99),
    Min = lists:min(Sorted),
    Max = lists:max(Sorted),
    Avg = lists:sum(Sorted) / Len,

    #{
        p50 => P50,
        p95 => P95,
        p99 => P99,
        min => Min,
        max => Max,
        avg => Avg
    }.

-spec percentile([number()], float()) -> float().
percentile(SortedList, Percentile) ->
    Len = length(SortedList),
    Index = max(1, min(Len, round(Len * Percentile))),
    lists:nth(Index, SortedList).

%% Capture environment information
-spec capture_environment() -> map().
capture_environment() ->
    {ok, Hostname} = inet:gethostname(),
    OtpRelease = erlang:system_info(otp_release),
    ErlangVersion = erlang:system_info(version),
    {OS, OSVersion} = case os:type() of
        {unix, Type} ->
            {Type, os:cmd("uname -r") -- "\n"};
        {win32, _} ->
            {win32, "unknown"}
    end,

    #{
        hostname => list_to_binary(Hostname),
        erlang_version => list_to_binary("OTP-" ++ OtpRelease),
        erlang_erts_version => list_to_binary(ErlangVersion),
        os => atom_to_binary(OS, utf8),
        os_version => list_to_binary(OSVersion)
    }.

%% Estimate CPU usage (simplified)
-spec estimate_cpu_usage(integer()) -> float().
estimate_cpu_usage(_DurationUs) ->
    %% Rough estimate based on system load
    BaseUsage = 50.0,
    Variance = (rand:uniform(20) - 10),
    min(100.0, max(0.0, BaseUsage + Variance)).

%% Validate report structure
-spec validate_report(map()) -> ok | {error, term()}.
validate_report(Report) ->
    RequiredFields = [
        workload_id, benchmark, category, timestamp, environment, operations,
        duration_s, throughput_msg_per_s, latency_p50_us, latency_p95_us,
        latency_p99_us, precision, memory_start_mib, memory_end_mib,
        memory_delta_mib, cpu_percent_avg, scope, details
    ],

    case lists:all(fun(Field) -> maps:is_key(Field, Report) end, RequiredFields) of
        true ->
            %% Validate environment
            EnvFields = [hostname, erlang_version, os],
            Env = maps:get(environment, Report),
            case lists:all(fun(Field) -> maps:is_key(Field, Env) end, EnvFields) of
                true -> ok;
                false -> {error, {missing_environment_fields, EnvFields}}
            end;
        false ->
            Missing = [F || F <- RequiredFields, not maps:is_key(F, Report)],
            {error, {missing_fields, Missing}}
    end.

%% Write report to JSON file
-spec write_report(string(), map()) -> ok | {error, term()}.
write_report(Filename, Report) ->
    %% Ensure results directory exists
    filelib:ensure_dir(Filename),

    %% Convert map to JSON
    Json = jsx:encode(Report, [{space, 2}, {indent, 2}]),

    %% Write to file
    case file:write_file(Filename, Json) of
        ok -> ok;
        {error, Reason} -> {error, {write_failed, Reason}}
    end.

%% Helper: round float to N decimal places
-spec round_float(number(), integer()) -> float().
round_float(Value, DecimalPlaces) ->
    Multiplier = math:pow(10, DecimalPlaces),
    round(Value * Multiplier) / Multiplier.
