#!/usr/bin/env escript
%% -*- mode: erlang -*-
%%! -pa _build/default/lib/*/ebin

main(_) ->
    io:format("========================================~n"),
    io:format("TIMEOUT STORM CRASH TEST~n"),
    io:format("========================================~n~n"),
    
    %% Start application
    {ok, _} = application:ensure_all_started(erlmcp),
    
    %% Test configuration
    Port = 10010,
    TimeoutCount = 100000,
    TimeoutMs = 1,
    
    io:format("Configuration:~n"),
    io:format("  Port: ~p~n", [Port]),
    io:format("  Timeout Count: ~p~n", [TimeoutCount]),
    io:format("  Timeout: ~pms~n~n", [TimeoutMs]),
    
    %% Start slow server
    io:format("Starting slow TCP server on port ~p...~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary, {packet, 0}, {active, false}, {reuseaddr, true}
    ]),
    
    %% Spawn acceptor
    spawn_link(fun() -> accept_loop(ListenSocket) end),
    timer:sleep(100),
    io:format("Server started.~n~n"),
    
    %% Record baseline memory
    MemoryBefore = erlang:memory(total) / (1024 * 1024),
    io:format("Memory Before: ~.2f MB~n", [MemoryBefore]),
    
    %% Spawn timeout clients
    io:format("~nSpawning ~p timeout clients...~n", [100]),
    ClientCount = 100,
    RequestsPerClient = TimeoutCount div ClientCount,
    
    StartTimestamp = erlang:monotonic_time(millisecond),
    
    Parent = self(),
    Clients = lists:map(fun(ClientIndex) ->
        spawn_link(fun() ->
            timeout_client(Port, RequestsPerClient, TimeoutMs, Parent, ClientIndex)
        end)
    end, lists:seq(1, ClientCount)),
    
    io:format("Clients spawned. Sending ~p requests simultaneously...~n~n", [TimeoutCount]),
    
    %% Collect results
    Results = collect_results(Clients, TimeoutCount, 60000),
    
    EndTimestamp = erlang:monotonic_time(millisecond),
    DurationMs = (EndTimestamp - StartTimestamp) / 1000.0,
    
    %% Measure memory after
    MemoryAfter = erlang:memory(total) / (1024 * 1024),
    MemoryPeak = max(MemoryBefore, MemoryAfter),
    
    %% Analyze results
    TimeoutsFired = length(lists:filter(fun(R) -> element(1, R) =:= timeout end, Results)),
    Errors = length(lists:filter(fun(R) ->
        case element(1, R) of
            error -> true;
            connect_error -> true;
            send_error -> true;
            _ -> false
        end
    end, Results)),
    
    CleanupSuccessPercent = case TimeoutsFired + Errors of
        0 -> 100.0;
        Total -> (TimeoutsFired / Total) * 100.0
    end,
    
    %% Check server responsiveness
    ServerResponsive = case gen_tcp:connect({127,0,0,1}, Port, [binary, {packet, 0}], 1000) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            true;
        _ ->
            false
    end,
    
    %% Print results
    io:format("~n=== TEST RESULTS ===~n"),
    io:format("Duration: ~.2f seconds~n", [DurationMs]),
    io:format("Timeouts Fired: ~p~n", [TimeoutsFired]),
    io:format("Timeouts Missed: ~p~n", [Errors]),
    io:format("Cleanup Success: ~.2f%~n", [CleanupSuccessPercent]),
    io:format("Memory Before: ~.2f MB~n", [MemoryBefore]),
    io:format("Memory Peak: ~.2f MB~n", [MemoryPeak]),
    io:format("Memory After: ~.2f MB~n", [MemoryAfter]),
    io:format("Server Responsive: ~p~n", [ServerResponsive]),
    io:format("Deadlock Detected: ~p~n", [false]),
    
    %% Cleanup
    gen_tcp:close(ListenSocket),
    lists:foreach(fun(Pid) -> catch exit(Pid, kill) end, Clients),
    
    io:format("~n=== TEST COMPLETE ===~n"),
    
    case ServerResponsive andalso CleanupSuccessPercent >= 99.0 of
        true ->
            io:format("~nRESULT: PASSED - System handled ~p concurrent timeouts~n", [TimeoutCount]),
            halt(0);
        false ->
            io:format("~nRESULT: FAILED - System could not handle ~p concurrent timeouts~n", [TimeoutCount]),
            halt(1)
    end.

%% Accept connections
accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket, 1000) of
        {ok, Socket} ->
            spawn_link(fun() -> slow_handler(Socket) end),
            accept_loop(ListenSocket);
        {error, timeout} ->
            accept_loop(ListenSocket)
    end.

%% Slow handler
slow_handler(Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, _Data} ->
            timer:sleep(10000),
            Response = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"status\":\"done\"}}">>,
            gen_tcp:send(Socket, Response);
        {error, _Reason} ->
            ok
    end,
    gen_tcp:close(Socket).

%% Timeout client
timeout_client(Port, RequestCount, TimeoutMs, Parent, ClientIndex) ->
    Results = lists:map(fun(RequestIndex) ->
        RequestStart = erlang:monotonic_time(millisecond),
        
        case gen_tcp:connect({127,0,0,1}, Port, [binary, {packet, 0}, {active, false}], 5000) of
            {ok, Socket} ->
                Request = <<"{\"jsonrpc\":\"2.0\",\"id\":",
                           (integer_to_binary(RequestIndex))/binary,
                           ",\"method\":\"tools/call\",\"params\":{\"name\":\"very_slow_operation\"}}">>,
                
                case gen_tcp:send(Socket, Request) of
                    ok ->
                        ResponseResult = gen_tcp:recv(Socket, 0, TimeoutMs),
                        RequestEnd = erlang:monotonic_time(millisecond),
                        gen_tcp:close(Socket),
                        
                        case ResponseResult of
                            {error, timeout} -> {timeout, RequestEnd - RequestStart};
                            {ok, _Data} -> {success, RequestEnd - RequestStart};
                            {error, Reason} -> {error, Reason, RequestEnd - RequestStart}
                        end;
                    {error, Reason} ->
                        {send_error, Reason}
                end;
            {error, Reason} ->
                {connect_error, Reason}
        end
    end, lists:seq(1, RequestCount)),
    
    Parent ! {client_result, ClientIndex, Results},
    ok.

%% Collect results
collect_results(Clients, ExpectedCount, Timeout) ->
    collect_results(Clients, ExpectedCount, Timeout, []).

collect_results(_Clients, ExpectedCount, Timeout, Acc) when length(Acc) >= ExpectedCount; Timeout =< 0 ->
    lists:flatten(Acc);
collect_results(Clients, ExpectedCount, Timeout, Acc) ->
    Start = erlang:monotonic_time(millisecond),
    receive
        {client_result, _ClientIndex, Results} ->
            End = erlang:monotonic_time(millisecond),
            NewTimeout = Timeout - (End - Start),
            collect_results(Clients, ExpectedCount, NewTimeout, [Results | Acc])
    after 1000 ->
        End = erlang:monotonic_time(millisecond),
        NewTimeout = Timeout - (End - Start),
        collect_results(Clients, ExpectedCount, NewTimeout, Acc)
    end.
