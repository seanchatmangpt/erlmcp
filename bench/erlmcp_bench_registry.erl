%%%-------------------------------------------------------------------
%%% @doc Registry Performance Benchmark
%%% Compares registry operations against 553K msg/sec baseline
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_registry).
-export([run/0, run_all/0, bench_register/0, bench_lookup/0, 
         bench_unregister/0, bench_send/0, bench_mixed/0]).

-define(BASELINE, 553000).
-define(ITERATIONS, 100000).
-define(NUM_PROCS, 100).

%%====================================================================
%% Public API
%%====================================================================

run() ->
    io:format("=== ERLMCP Registry Benchmark ===~n"),
    io:format("Baseline Target: ~p msg/sec~n~n", [?BASELINE]),
    
    %% Start necessary apps
    application:ensure_all_started(gproc),
    
    %% Run tests
    Tests = [
        {"gproc:reg/2 (register)", fun bench_register/0},
        {"gproc:lookup_local_name/1 (lookup)", fun bench_lookup/0},
        {"gproc:unreg/1 (unregister)", fun bench_unregister/0},
        {"gproc:send/2 (send)", fun bench_send/0},
        {"Mixed Operations", fun bench_mixed/0}
    ],
    
    Results = lists:map(fun({Name, Fun}) ->
        io:format("Running: ~s...~n", [Name]),
        {OpsPerSec, _Microseconds} = Fun(),
        io:format("  Result: ~w msg/sec~n~n", [round(OpsPerSec)]),
        {Name, OpsPerSec}
    end, Tests),
    
    %% Calculate overall average
    AvgThroughput = lists:foldl(fun({_, Ops}, Acc) -> Acc + Ops end, 0, Results) / length(Results),
    
    io:format("~n=== SUMMARY ===~n"),
    lists:foreach(fun({Name, Ops}) ->
        Status = case Ops >= ?BASELINE of
            true -> "PASS";
            false -> "FAIL"
        end,
        io:format("~s: ~w msg/sec [~s]~n", [Name, round(Ops), Status])
    end, Results),
    
    io:format("~nAverage: ~w msg/sec~n", [round(AvgThroughput)]),
    
    OverallStatus = case AvgThroughput >= ?BASELINE of
        true -> io_lib:format("PASS >= ~pK", [?BASELINE div 1000]);
        false -> io_lib:format("FAIL: ~w msg/sec", [round(AvgThroughput)])
    end,
    
    io:format("~nOverall: ~s~n", [OverallStatus]),
    {ok, AvgThroughput, Results}.

run_all() ->
    {ok, Throughput, _Results} = run(),
    case Throughput >= ?BASELINE of
        true -> 
            io:format("~n~n*** PASS ***~n"),
            halt(0);
        false -> 
            io:format("~n~n*** FAIL ***~n"),
            halt(1)
    end.

%%====================================================================
%% Benchmark Functions
%%====================================================================

bench_register() ->
    Pids = spawn_test_procs(?NUM_PROCS),
    
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(N) ->
            Pid = lists:nth((N rem ?NUM_PROCS) + 1, Pids),
            Name = list_to_binary("test_" ++ integer_to_list(N)),
            try gproc:reg({n, l, Name}, Pid) catch _:_ -> ok end
        end, lists:seq(1, ?ITERATIONS))
    end),
    
    %% Cleanup
    lists:foreach(fun(Pid) -> catch gproc:goodbye(Pid) end, Pids),
    
    OpsPerSec = (?ITERATIONS * 1000000) / Time,
    {OpsPerSec, Time}.

bench_lookup() ->
    Pids = spawn_test_procs(?NUM_PROCS),
    
    %% Register names first
    lists:foreach(fun(N) ->
        Pid = lists:nth((N rem ?NUM_PROCS) + 1, Pids),
        Name = list_to_binary("test_" ++ integer_to_list(N)),
        try gproc:reg({n, l, Name}, Pid) catch _:_ -> ok end
    end, lists:seq(1, ?NUM_PROCS)),
    
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(N) ->
            Name = list_to_binary("test_" ++ integer_to_list((N rem ?NUM_PROCS) + 1)),
            gproc:lookup_local_name(Name)
        end, lists:seq(1, ?ITERATIONS))
    end),
    
    %% Cleanup
    lists:foreach(fun(Pid) -> catch gproc:goodbye(Pid) end, Pids),
    
    OpsPerSec = (?ITERATIONS * 1000000) / Time,
    {OpsPerSec, Time}.

bench_unregister() ->
    Pids = spawn_test_procs(?NUM_PROCS),
    
    %% Register and unregister repeatedly
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(N) ->
            Pid = lists:nth((N rem ?NUM_PROCS) + 1, Pids),
            Name = list_to_binary("test_" ++ integer_to_list(N)),
            try gproc:reg({n, l, Name}, Pid) catch _:_ -> ok end,
            try gproc:unreg({n, l, Name}) catch _:_ -> ok end
        end, lists:seq(1, ?ITERATIONS))
    end),
    
    %% Cleanup
    lists:foreach(fun(Pid) -> catch gproc:goodbye(Pid) end, Pids),
    
    OpsPerSec = (?ITERATIONS * 1000000) / Time,
    {OpsPerSec, Time}.

bench_send() ->
    Pids = spawn_test_procs(?NUM_PROCS),
    
    %% Register names first
    lists:foreach(fun(N) ->
        Pid = lists:nth((N rem ?NUM_PROCS) + 1, Pids),
        Name = list_to_binary("receiver_" ++ integer_to_list(N)),
        try gproc:reg({n, l, Name}, Pid) catch _:_ -> ok end
    end, lists:seq(1, ?NUM_PROCS)),
    
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(N) ->
            Name = list_to_binary("receiver_" ++ integer_to_list((N rem ?NUM_PROCS) + 1)),
            gproc:send({n, l, Name}, test_msg)
        end, lists:seq(1, ?ITERATIONS))
    end),
    
    %% Cleanup
    lists:foreach(fun(Pid) -> catch gproc:goodbye(Pid) end, Pids),
    
    OpsPerSec = (?ITERATIONS * 1000000) / Time,
    {OpsPerSec, Time}.

bench_mixed() ->
    Pids = spawn_test_procs(?NUM_PROCS),
    
    %% Pre-register some names
    lists:foreach(fun(N) ->
        Pid = lists:nth((N rem ?NUM_PROCS) + 1, Pids),
        Name = list_to_binary("mixed_" ++ integer_to_list(N)),
        try gproc:reg({n, l, Name}, Pid) catch _:_ -> ok end
    end, lists:seq(1, 20)),
    
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(N) ->
            Pid = lists:nth((N rem ?NUM_PROCS) + 1, Pids),
            Name = list_to_binary("mixed_" ++ integer_to_list(N)),
            case N rem 4 of
                0 -> try gproc:reg({n, l, Name}, Pid) catch _:_ -> ok end;
                1 -> gproc:lookup_local_name(Name);
                2 -> try gproc:send({n, l, Name}, test_msg) catch _:_ -> ok end;
                3 -> try gproc:unreg({n, l, Name}) catch _:_ -> ok end
            end
        end, lists:seq(1, ?ITERATIONS))
    end),
    
    %% Cleanup
    lists:foreach(fun(Pid) -> catch gproc:goodbye(Pid) end, Pids),
    
    OpsPerSec = (?ITERATIONS * 1000000) / Time,
    {OpsPerSec, Time}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Spawn test processes that will stay alive
spawn_test_procs(Count) ->
    Parent = self(),
    lists:map(fun(_) ->
        spawn(fun() ->
            link(Parent),
            receive
                stop -> ok
            end
        end)
    end, lists:seq(1, Count)).
