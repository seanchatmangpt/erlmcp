%%%-------------------------------------------------------------------
%%% @doc Standalone Destructive Memory Exhaustion Test
%%%
%%% Simple test that directly allocates memory until OOM
%%% NO MERCY - PUSH UNTIL CRASH
%%% @end
%%%-------------------------------------------------------------------
-module(destructive_memory_standalone).

-export([run/0, run/1, run_test/0]).

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
    {2 * 1024 * 1024 * 1024, <<"2GB">>},
    {4 * 1024 * 1024 * 1024, <<"4GB">>},
    {8 * 1024 * 1024 * 1024, <<"8GB">>},
    {16 * 1024 * 1024 * 1024, <<"16GB">>}
]).

-record(test_result, {
    size_label :: binary(),
    size_bytes :: non_neg_integer(),
    memory_before_mb :: float(),
    memory_after_mb :: float(),
    status :: pass | crash | oom | timeout,
    error_message :: binary() | undefined,
    allocation_time_ms :: float()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec run() -> ok.
run() ->
    run(#{}).

-spec run(map()) -> ok.
run(_Config) ->
    io:format("~n=== STANDALONE MEMORY EXHAUSTION TEST ===~n", []),
    io:format("Objective: Find exact breaking point~n", []),
    io:format("NO MERCY - PUSH UNTIL CRASH~n~n", []),
    
    Results = test_sequence(?SIZES, []),
    
    Report = generate_report(Results),
    print_report(Report),
    
    io:format("~n=== TEST COMPLETE ===~n~n", []),
    ok.

-spec run_test() -> ok.
run_test() ->
    run(#{}).

%%====================================================================
%% Internal Functions
%%====================================================================

-spec test_sequence([{non_neg_integer(), binary()}], [#test_result{}]) -> [#test_result{}].
test_sequence([], Acc) ->
    lists:reverse(Acc);
test_sequence([{Size, Label} | Rest], Acc) ->
    io:format("Testing size: ~s (~p bytes)~n", [Label, Size]),
    
    MemoryBefore = get_memory_mb(),
    
    Result = try
        StartTime = erlang:monotonic_time(millisecond),
        
        %% Allocate large binary
        Payload = generate_payload(Size),
        
        %% Force memory pressure by creating references
        _Refs = create_memory_pressure(Payload, 100),
        
        EndTime = erlang:monotonic_time(millisecond),
        MemoryAfter = get_memory_mb(),
        
        #test_result{
            size_label = Label,
            size_bytes = Size,
            memory_before_mb = MemoryBefore,
            memory_after_mb = MemoryAfter,
            status = pass,
            error_message = undefined,
            allocation_time_ms = EndTime - StartTime
        }
    catch
        error:ErrorReason ->
            MemAfter = get_memory_mb(),
            ErrorBin = iolist_to_binary(io_lib:format("~p", [ErrorReason])),
            Status = case ErrorReason of
                {system_memory_limit_exceeded, _} -> oom;
                _ -> crash
            end,
            #test_result{
                size_label = Label,
                size_bytes = Size,
                memory_before_mb = MemoryBefore,
                memory_after_mb = MemAfter,
                status = Status,
                error_message = ErrorBin,
                allocation_time_ms = 0.0
            };
        Class:ErrorReason:Stacktrace ->
            io:format("Exception: ~p:~p~n", [Class, ErrorReason]),
            MemAfter = get_memory_mb(),
            ErrorBin = iolist_to_binary(io_lib:format("~p:~p", [Class, ErrorReason])),
            #test_result{
                size_label = Label,
                size_bytes = Size,
                memory_before_mb = MemoryBefore,
                memory_after_mb = MemAfter,
                status = crash,
                error_message = ErrorBin,
                allocation_time_ms = 0.0
            }
    end,
    
    %% Stop if crashed or OOM
    case Result#test_result.status of
        crash ->
            io:format("CRASH at ~s - STOPPING~n", [Label]),
            lists:reverse([Result | Acc]);
        oom ->
            io:format("OOM at ~s - STOPPING~n", [Label]),
            lists:reverse([Result | Acc]);
        _ ->
            test_sequence(Rest, [Result | Acc])
    end.

-spec generate_payload(non_neg_integer()) -> binary().
generate_payload(Size) ->
    Pattern = <<"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789">>,
    PatternSize = byte_size(Pattern),
    Repeats = (Size div PatternSize) + 1,
    Bin = binary:copy(Pattern, Repeats),
    binary:part(Bin, {0, Size}).

-spec create_memory_pressure(binary(), non_neg_integer()) -> [binary()].
create_memory_pressure(_Payload, 0) ->
    [];
create_memory_pressure(Payload, N) ->
    %% Create copies to increase memory pressure
    [Payload | create_memory_pressure(Payload, N - 1)].

-spec get_memory_mb() -> float().
get_memory_mb() ->
    erlang:memory(total) / (1024 * 1024).

-spec generate_report([#test_result{}]) -> map().
generate_report(Results) ->
    TotalTests = length(Results),
    PassedTests = length([R || R <- Results, R#test_result.status =:= pass]),
    CrashedTests = length([R || R <- Results, R#test_result.status =:= crash]),
    OomTests = length([R || R <- Results, R#test_result.status =:= oom]),
    
    BreakingPoint = find_breaking_point(Results),
    
    MaxMemory = case Results of
        [] -> 0.0;
        _ -> lists:max([R#test_result.memory_after_mb || R <- Results])
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
        memory_mb => Result#test_result.memory_after_mb
    }.

-spec print_report(map()) -> ok.
print_report(Report) ->
    io:format("~n=== MEMORY EXHAUSTION CRASH TEST ===~n~n", []),
    
    %% Print test sequence
    Results = maps:get(results, Report),
    lists:foreach(fun(Result) ->
        StatusSymbol = case Result#test_result.status of
            pass -> "PASS";
            crash -> "CRASH";
            oom -> "OOM";
            timeout -> "TIMEOUT"
        end,
        
        LabelBin = Result#test_result.size_label,
        LabelStr = binary_to_list(LabelBin),
        
        io:format("- ~s: ~s (memory before: ~.2f MB, after: ~.2f MB, time: ~.2f ms)~n", [
            LabelStr,
            StatusSymbol,
            Result#test_result.memory_before_mb,
            Result#test_result.memory_after_mb,
            Result#test_result.allocation_time_ms
        ]),
        
        case Result#test_result.error_message of
            undefined -> ok;
            Error -> 
                ErrStr = binary_to_list(Error),
                io:format("  ERROR: ~s~n", [ErrStr])
        end
    end, Results),
    
    %% Print breaking point
    case maps:get(breaking_point, Report) of
        undefined ->
            io:format("~nNO BREAKING POINT (all tests passed)~n", []);
        BreakingPoint ->
            io:format("~nBREAKING POINT:~n", []),
            LabelBin = maps:get(size_label, BreakingPoint),
            LabelStr = binary_to_list(LabelBin),
            io:format("- Payload Size: ~s (~p bytes)~n", [
                LabelStr,
                maps:get(size_bytes, BreakingPoint)
            ]),
            io:format("- Total Memory: ~.2f MB~n", [maps:get(memory_mb, BreakingPoint)]),
            ErrBin = maps:get(error_message, BreakingPoint),
            ErrStr = binary_to_list(ErrBin),
            io:format("- Error: ~s~n", [ErrStr]),
            StatusAtom = maps:get(status, BreakingPoint),
            io:format("- Crash Type: ~p~n", [StatusAtom])
    end,
    
    %% Print summary
    io:format("~nSUMMARY:~n", []),
    io:format("- Total Tests: ~p~n", [maps:get(total_tests, Report)]),
    io:format("- Passed: ~p~n", [maps:get(passed, Report)]),
    io:format("- Crashed: ~p~n", [maps:get(crashed, Report)]),
    io:format("- OOM: ~p~n", [maps:get(oom, Report)]),
    io:format("- Peak Memory: ~.2f MB~n", [maps:get(max_memory_mb, Report)]),
    
    ok.
