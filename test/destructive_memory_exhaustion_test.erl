%%%-------------------------------------------------------------------
%%% @doc Destructive Memory Exhaustion Stress Test
%%%
%%% BREAKS THE MCP SERVER BY PUSHING MEMORY LIMITS
%%%
%%% Test Protocol:
%%% 1. Spawn MCP server with echo_data tool
%%% 2. Send increasingly large payloads (1KB -> 1GB)
%%% 3. Monitor memory at each size
%%% 4. Continue until OOM, crash, or unresponsive
%%% 5. Document exact breaking point
%%%
%%% NO MERCY - PUSH UNTIL CRASH
%%% @end
%%%-------------------------------------------------------------------
-module(destructive_memory_exhaustion_test).

-export([run/0, run/1, run_test/0]).

-include_lib("kernel/include/logger.hrl").

%% Test configuration
-define(SIZES, [
    {1 * 1024, <<"1KB">>},
    {10 * 1024, <<"10KB">>},
    {100 * 1024, <<"100KB">>},
    {1 * 1024 * 1024, <<"1MB">>},
    {10 * 1024 * 1024, <<"10MB">>},
    {100 * 1024 * 1024, <<"100MB">>},
    {256 * 1024 * 1024, <<"256MB">>},
    {512 * 1024 * 1024, <<"512MB">>},
    {1 * 1024 * 1024 * 1024, <<"1GB">>},
    {2 * 1024 * 1024 * 1024, <<"2GB">>}
]).

-define(REQUESTS_PER_SIZE, 3).
-define(TIMEOUT_MS, 60000).

-record(test_result, {
    size_label :: binary(),
    size_bytes :: non_neg_integer(),
    success_count = 0 :: non_neg_integer(),
    fail_count = 0 :: non_neg_integer(),
    memory_before_mb :: float(),
    memory_after_mb :: float(),
    memory_peak_mb :: float(),
    avg_latency_ms :: float(),
    status :: pass | crash | timeout | oom | unresponsive,
    error_message :: binary() | undefined,
    crashed :: boolean(),
    vm_terminated :: boolean()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Run the full destructive memory exhaustion test
-spec run() -> ok.
run() ->
    run(#{}).

%% @doc Run with custom config
-spec run(map()) -> ok.
run(_Config) ->
    logger:warning("=== DESTRUCTIVE MEMORY EXHAUSTION TEST STARTED ===", []),
    logger:warning("Objective: Find exact breaking point of MCP server", []),
    logger:warning("NO MERCY - PUSH UNTIL CRASH", []),
    
    %% Start applications
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_transports),
    
    %% Start server in separate process
    ServerPid = start_test_server(),
    
    %% Run test sequence
    Results = test_sequence(ServerPid, ?SIZES),
    
    %% Generate report
    Report = generate_report(Results),
    print_report(Report),
    
    %% Cleanup
    cleanup(ServerPid),
    
    logger:warning("=== DESTRUCTIVE TEST COMPLETE ===", []),
    ok.

%% @doc Quick test (for development)
-spec run_test() -> ok.
run_test() ->
    run(#{
        quick_test => true,
        max_size => 100 * 1024 * 1024  % Stop at 100MB
    }).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Start test server with echo_data tool
-spec start_test_server() -> pid().
start_test_server() ->
    Parent = self(),
    
    spawn(fun() ->
        %% Start stdio server
        {ok, ServerPid} = erlmcp_stdio:start_link(),
        
        %% Add echo_data tool
        ok = erlmcp_stdio:add_tool(
            <<"echo_data">>,
            <<"Echo back data for memory testing">>,
            fun(#{<<"data">> := Data}) -> Data end,
            #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"data">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"data">>]
            }
        ),
        
        Parent ! {server_ready, ServerPid},
        
        %% Keep alive
        receive
            stop -> ok
        end
    end),
    
    receive
        {server_ready, ServerPid} ->
            logger:info("Test server started: ~p", [ServerPid]),
            ServerPid
    after 5000 ->
        error("Server failed to start")
    end.

%% @doc Run test sequence through sizes
-spec test_sequence(pid(), [{non_neg_integer(), binary()}]) -> [#test_result{}].
test_sequence(ServerPid, Sizes) ->
    test_sequence(ServerPid, Sizes, []).

test_sequence(_ServerPid, [], Acc) ->
    lists:reverse(Acc);
test_sequence(ServerPid, [{Size, Label} | Rest], Acc) ->
    logger:warning("Testing size: ~s (~p bytes)", [Label, Size]),
    
    %% Check if server still alive
    case is_process_alive(ServerPid) of
        false ->
            logger:error("SERVER DEAD at ~s", [Label]),
            Result = #test_result{
                size_label = Label,
                size_bytes = Size,
                status = crash,
                crashed = true,
                vm_terminated = false,
                error_message = <<"Server process died">>
            },
            lists:reverse([Result | Acc]);
        true ->
            %% Run requests for this size
            Result = test_size(ServerPid, Size, Label),
            
            %% Stop if crashed or OOM
            case Result#test_result.status of
                crash ->
                    logger:error("CRASH DETECTED at ~s - STOPPING TEST", [Label]),
                    lists:reverse([Result | Acc]);
                oom ->
                    logger:error("OOM DETECTED at ~s - STOPPING TEST", [Label]),
                    lists:reverse([Result | Acc]);
                unresponsive ->
                    logger:error("UNRESPONSIVE at ~s - STOPPING TEST", [Label]),
                    lists:reverse([Result | Acc]);
                _ ->
                    test_sequence(ServerPid, Rest, [Result | Acc])
            end
    end.

%% @doc Test a specific payload size
-spec test_size(pid(), non_neg_integer(), binary()) -> #test_result{}.
test_size(ServerPid, Size, Label) ->
    MemoryBefore = get_memory_mb(),
    
    %% Generate payload
    Payload = generate_payload(Size),
    PayloadSize = byte_size(Payload),
    
    logger:info("Payload size: ~p bytes", [PayloadSize]),
    
    %% Run requests
    {SuccessCount, FailCount, Latencies, PeakMemory, Status, Error} = 
        run_requests(ServerPid, Payload, ?REQUESTS_PER_SIZE, MemoryBefore, []),
    
    MemoryAfter = get_memory_mb(),
    
    #test_result{
        size_label = Label,
        size_bytes = PayloadSize,
        success_count = SuccessCount,
        fail_count = FailCount,
        memory_before_mb = MemoryBefore,
        memory_after_mb = MemoryAfter,
        memory_peak_mb = PeakMemory,
        avg_latency_ms = avg(Latencies),
        status = Status,
        error_message = Error,
        crashed = Status =:= crash,
        vm_terminated = Status =:= oom
    }.

%% @doc Run multiple requests and monitor
-spec run_requests(pid(), binary(), non_neg_integer(), float(), [float()]) ->
    {non_neg_integer(), non_neg_integer(), [float()], float(), atom(), binary() | undefined}.
run_requests(ServerPid, Payload, Count, PeakMemory, Latencies) when Count > 0 ->
    case is_process_alive(ServerPid) of
        false ->
            {0, Count, Latencies, PeakMemory, crash, <<"Server died">>};
        true ->
            StartTime = erlang:monotonic_time(millisecond),
            
            try
                %% Call tool with large payload
                Result = erlmcp_stdio:call_tool(<<"echo_data">>, #{<<"data">> => Payload}, ?TIMEOUT_MS),
                
                EndTime = erlang:monotonic_time(millisecond),
                Latency = EndTime - StartTime,
                CurrentMemory = get_memory_mb(),
                NewPeak = max(PeakMemory, CurrentMemory),
                
                case Result of
                    {ok, #{<<"result">> := _}} ->
                        %% Success
                        run_requests(ServerPid, Payload, Count - 1, NewPeak, [Latency | Latencies]);
                    {error, Reason} ->
                        logger:error("Request failed: ~p", [Reason]),
                        {0, Count, Latencies, NewPeak, error, iolist_to_binary(io_lib:format("~p", [Reason]))}
                end
            catch
                error:ErrorReason:Stacktrace ->
                    logger:error("Request exception: ~p~n~p", [ErrorReason, Stacktrace]),
                    
                    %% Check if OOM
                    ErrorBin = iolist_to_binary(io_lib:format("~p", [ErrorReason])),
                    
                    case ErrorReason of
                        {system_memory_limit_exceeded, _} ->
                            {0, Count, Latencies, PeakMemory, oom, ErrorBin};
                        _ ->
                            {0, Count, Latencies, PeakMemory, crash, ErrorBin}
                    end;
                Class:ErrorReason:Stacktrace ->
                    logger:error("Request exception: ~p:~p~n~p", [Class, ErrorReason, Stacktrace]),
                    ErrorBin = iolist_to_binary(io_lib:format("~p:~p", [Class, ErrorReason])),
                    {0, Count, Latencies, PeakMemory, crash, ErrorBin}
            end
    end;
run_requests(_ServerPid, _Payload, 0, PeakMemory, Latencies) ->
    %% All requests succeeded
    TotalRequests = length(Latencies),
    {TotalRequests, 0, lists:reverse(Latencies), PeakMemory, pass, undefined}.

%% @doc Generate payload of specific size
-spec generate_payload(non_neg_integer()) -> binary().
generate_payload(Size) ->
    Pattern = <<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789">>,
    PatternSize = byte_size(Pattern),
    Repeats = (Size div PatternSize) + 1,
    Bin = binary:copy(Pattern, Repeats),
    binary:part(Bin, {0, Size}).

%% @doc Get current memory usage in MB
-spec get_memory_mb() -> float().
get_memory_mb() ->
    MemoryBytes = erlang:memory(total),
    MemoryBytes / (1024 * 1024).

%% @doc Calculate average
-spec avg([number()]) -> float().
avg([]) -> 0.0;
avg(List) -> lists:sum(List) / length(List).

%% @doc Generate comprehensive report
-spec generate_report([#test_result{}]) -> map().
generate_report(Results) ->
    TotalTests = length(Results),
    PassedTests = length([R || R <- Results, R#test_result.status =:= pass]),
    CrashedTests = length([R || R <- Results, R#test_result.status =:= crash]),
    OomTests = length([R || R <- Results, R#test_result.status =:= oom]),
    
    %% Find breaking point
    BreakingPoint = find_breaking_point(Results),
    
    %% Memory stats
    MaxMemory = case Results of
        [] -> 0.0;
        _ -> lists:max([R#test_result.memory_peak_mb || R <- Results])
    end,
    
    #{
        total_tests => TotalTests,
        passed => PassedTests,
        crashed => CrashedTests,
        oom => OomTests,
        breaking_point => BreakingPoint,
        max_memory_mb => MaxMemory,
        results => Results
    }.

%% @doc Find breaking point
-spec find_breaking_point([#test_result{}]) -> map() | undefined.
find_breaking_point([]) ->
    undefined;
find_breaking_point([#test_result{status = pass} | Rest]) ->
    find_breaking_point(Rest);
find_breaking_point([Result | _]) ->
    #{
        size_label => Result#test_result.size_label,
        size_bytes => Result#test_result.size_bytes,
        status => Result#test_result.status,
        error_message => Result#test_result.error_message,
        memory_mb => Result#test_result.memory_after_mb,
        crashed => Result#test_result.crashed,
        vm_terminated => Result#test_result.vm_terminated
    }.

%% @doc Print formatted report
-spec print_report(map()) -> ok.
print_report(Report) ->
    io:format("~n=== MEMORY EXHAUSTION CRASH TEST ===~n~n", []),
    
    %% Print test sequence
    Results = maps:get(results, Report),
    lists:foreach(fun(Result) ->
        StatusSymbol = case Result#test_result.status of
            pass -> <<"PASS">>;
            crash -> <<"CRASH">>;
            oom -> <<"OOM">>;
            timeout -> <<"TIMEOUT">>;
            unresponsive -> <<"UNRESPONSIVE">>
        end,
        
        io:format("- ~s: ~s (memory: ~.2f -> ~.2f MB, peak: ~.2f MB)~n", [
            Result#test_result.size_label,
            StatusSymbol,
            Result#test_result.memory_before_mb,
            Result#test_result.memory_after_mb,
            Result#test_result.memory_peak_mb
        ]),
        
        case Result#test_result.error_message of
            undefined -> ok;
            Error -> io:format("  ERROR: ~s~n", [Error])
        end
    end, Results),
    
    %% Print breaking point
    case maps:get(breaking_point, Report) of
        undefined ->
            io:format("~nNO BREAKING POINT FOUND (all tests passed)~n", []);
        BreakingPoint ->
            io:format("~nBREAKING POINT:~n", []),
            io:format("- Payload Size: ~s (~p bytes)~n", [
                maps:get(size_label, BreakingPoint),
                maps:get(size_bytes, BreakingPoint)
            ]),
            io:format("- Total Memory: ~.2f MB~n", [maps:get(memory_mb, BreakingPoint)]),
            io:format("- Error: ~s~n", [maps:get(error_message, BreakingPoint)]),
            io:format("- Crash Type: ~s~n", [maps:get(status, BreakingPoint)]),
            io:format("- Server Survived: ~s~n", [case maps:get(crashed, BreakingPoint) of true -> "no"; false -> "yes" end]),
            io:format("- VM Terminated: ~s~n", [case maps:get(vm_terminated, BreakingPoint) of true -> "yes"; false -> "no" end])
    end,
    
    %% Print summary
    io:format("~nSUMMARY:~n", []),
    io:format("- Total Tests: ~p~n", [maps:get(total_tests, Report)]),
    io:format("- Passed: ~p~n", [maps:get(passed, Report)]),
    io:format("- Crashed: ~p~n", [maps:get(crashed, Report)]),
    io:format("- OOM: ~p~n", [maps:get(oom, Report)]),
    io:format("- Peak Memory: ~.2f MB~n", [maps:get(max_memory_mb, Report)]),
    
    ok.

%% @doc Cleanup
-spec cleanup(pid()) -> ok.
cleanup(ServerPid) ->
    case is_process_alive(ServerPid) of
        true ->
            ServerPid ! stop,
            timer:sleep(100);
        false ->
            ok
    end.
