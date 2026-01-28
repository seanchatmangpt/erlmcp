#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

-mode(compile).

main([]) ->
    io:format("~n=== erlmcp Profiler Demo ===~n~n"),
    
    %% Start applications
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_observability),
    
    io:format("1. Testing Memory Snapshot...~n"),
    test_memory_snapshot(),
    
    io:format("~n2. Testing Binary Leak Detection...~n"),
    test_binary_leaks(),
    
    io:format("~n3. Testing Process Memory Inspection...~n"),
    test_process_memory(),
    
    io:format("~n4. Testing ETS Table Analysis...~n"),
    test_ets_tables(),
    
    io:format("~n=== All Profiler Features Demonstrated ===~n~n"),
    halt(0).

test_memory_snapshot() ->
    case erlmcp_profiler:memory_snapshot(#{top => 5}) of
        {ok, Snapshot} ->
            io:format("   ✅ Captured ~p top processes~n", [length(Snapshot)]),
            case Snapshot of
                [First | _] ->
                    MemMB = maps:get(memory_mb, First),
                    io:format("   Top process: ~.2f MB~n", [MemMB]);
                [] -> ok
            end;
        Error ->
            io:format("   ❌ Error: ~p~n", [Error])
    end.

test_binary_leaks() ->
    %% Create process with large binary
    LeakyPid = spawn(fun() ->
        Bin = binary:copy(<<1>>, 5000000),
        receive stop -> ok end,
        _ = Bin
    end),
    
    timer:sleep(100),
    
    case erlmcp_profiler:binary_leaks() of
        {ok, Suspects} ->
            io:format("   ✅ Found ~p processes with high binary usage~n", [length(Suspects)]),
            case lists:filter(fun(#{pid := P}) -> P == LeakyPid end, Suspects) of
                [_] -> io:format("   ✓ Detected our test leak~n");
                [] -> io:format("   (Test leak below threshold)~n")
            end;
        Error ->
            io:format("   ❌ Error: ~p~n", [Error])
    end,
    
    LeakyPid ! stop.

test_process_memory() ->
    Pid = self(),
    case erlmcp_profiler:process_memory(Pid) of
        {ok, Info} ->
            MemMB = maps:get(memory_mb, Info),
            Frag = maps:get(heap_fragmentation_pct, Info),
            io:format("   ✅ Self memory: ~.2f MB, fragmentation: ~.1f%~n", [MemMB, Frag]);
        Error ->
            io:format("   ❌ Error: ~p~n", [Error])
    end.

test_ets_tables() ->
    %% Create test table
    Tab = ets:new(demo_table, [public]),
    [ets:insert(Tab, {N, N * 2}) || N <- lists:seq(1, 1000)],
    
    Tables = erlmcp_memory_analyzer:ets_tables(5),
    io:format("   ✅ Analyzed ~p ETS tables~n", [length(Tables)]),
    
    case lists:filter(fun(#{name := Name}) -> Name == demo_table end, Tables) of
        [TableInfo] ->
            Size = maps:get(size, TableInfo),
            io:format("   Found demo_table with ~p entries~n", [Size]);
        [] ->
            io:format("   (demo_table not in top 5)~n")
    end,
    
    ets:delete(Tab).
