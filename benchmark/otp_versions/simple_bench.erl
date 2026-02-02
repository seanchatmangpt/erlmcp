%% Simple benchmark for OTP performance analysis
-module(simple_bench).
-export([main/0]).

main() ->
    io:format("Simple OTP Performance Benchmark~n", []),
    io:format("OTP Version: ~p~n", [erlang:system_info(otp_release)]),

    %% Test process creation
    ProcessTime = benchmark_process_creation(1000),
    io:format("Process creation (1000): ~p ms~n", [ProcessTime]),

    %% Test message passing
    MessageTime = benchmark_message_passing(1000),
    io:format("Message passing (1000): ~p ms~n", [MessageTime]),

    %% Test memory usage
    MemoryUsage = benchmark_memory_usage(100),
    io:format("Memory usage (100 processes): ~p bytes~n", [MemoryUsage]),

    ok.

benchmark_process_creation(N) ->
    Start = erlang:monotonic_time(millisecond),

    Pids = lists:foldl(fun(_, Acc) ->
        Pid = spawn(fun() ->
            %% Do some work
            lists:map(fun(_) -> crypto:strong_rand_bytes(1024) end, lists:seq(1, 10))
        end),
        [Pid | Acc]
    end, [], lists:seq(1, N)),

    %% Cleanup
    lists:foreach(fun(Pid) -> exit(Pid, normal) end, Pids),

    End = erlang:monotonic_time(millisecond),
    End - Start.

benchmark_message_passing(N) ->
    Self = self(),

    %% Create a receiver process
    {ok, Receiver} = spawn_opt(fun() -> receiver_loop(N, []) end, [monitor]),

    Start = erlang:monotonic_time(millisecond),

    %% Send N messages
    lists:foreach(fun(_) ->
        Receiver ! {data, crypto:strong_rand_bytes(100)}
    end, lists:seq(1, N)),

    %% Wait for all messages
    receive
        {done, Count} when Count =:= N ->
            ok
    after 10000 ->
        timeout
    end,

    exit(Receiver, normal),

    End = erlang:monotonic_time(millisecond),
    End - Start.

receiver_loop(0, _) ->
    self() ! {done, 0};
receiver_loop(Expected, Count) ->
    receive
        {data, _} ->
            receiver_loop(Expected - 1, Count + 1)
    after 1000 ->
        receiver_loop(Expected, Count)
    end.

benchmark_memory_usage(N) ->
    InitialMemory = erlang:memory(total),

    Pids = lists:foldl(fun(_, Acc) ->
        Pid = spawn(fun() ->
            %% Allocate some memory
            lists:map(fun(_) -> crypto:strong_rand_bytes(2048) end, lists:seq(1, 50))
        end),
        [Pid | Acc]
    end, [], lists:seq(1, N)),

    PeakMemory = erlang:memory(total),

    %% Cleanup
    lists:foreach(fun(Pid) -> exit(Pid, normal) end, Pids),

    FinalMemory = erlang:memory(total),

    PeakMemory - InitialMemory.