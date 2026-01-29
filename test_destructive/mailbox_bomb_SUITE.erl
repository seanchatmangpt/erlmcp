%%% Mailbox Bombing Stress Test - Destructive Test #7
-module(mailbox_bomb_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").

all() -> [bomb_server, bomb_clients].

%%--------------------------------------------------------------------
%% Test Cases
%%--------------------------------------------------------------------

bomb_server(_Config) ->
    io:format("~n=== MAILBOX BOMBING CRASH TEST - SERVER ===~n~n"),
    
    %% Start application
    {ok, _} = application:ensure_all_started(erlmcp),
    timer:sleep(500),
    
    %% Get or start server
    ServerPid = case whereis(erlmcp_server) of
        undefined -> 
            {ok, P} = erlmcp_server:start_link(#{port => 10007}),
            P;
        P -> P
    end,
    
    io:format("Server Pid: ~p~n", [ServerPid]),
    
    %% Bomb server mailbox
    Msg = <<0:100>>,
    
    lists:foreach(fun(Scale) ->
        io:format("~n~p messages:~n", [Scale]),
        SendTime = os:system_time(microsecond),
        lists:foreach(fun(_) -> ServerPid ! {bomb, Msg} end, lists:seq(1, Scale)),
        SendElapsed = (os:system_time(microsecond) - SendTime) / 1000,
        
        QueueLen = get_queue_len(ServerPid),
        MemoryMB = get_memory_mb(ServerPid),
        
        io:format("  Send time: ~.2f ms~n", [SendElapsed]),
        io:format("  Queue Len: ~p~n", [QueueLen]),
        io:format("  Memory: ~.2f MB~n", [MemoryMB]),
        io:format("  Alive: ~p~n", [is_process_alive(ServerPid)]),
        
        %% Check if still alive
        case is_process_alive(ServerPid) of
            false -> ct:fail("Server died at ~p messages", [Scale]);
            true -> ok
        end,
        
        timer:sleep(100)
    end, [1000, 10000, 100000, 1000000, 10000000]),
    
    io:format("~nServer survived all bombing!~n"),
    ok.

bomb_clients(_Config) ->
    io:format("~n=== MAILBOX BOMBING CRASH TEST - CLIENTS ===~n~n"),
    
    %% Start application
    {ok, _} = application:ensure_all_started(erlmcp),
    timer:sleep(500),
    
    %% Spawn 10 clients
    Clients = lists:map(fun(N) ->
        {ok, Pid} = erlmcp_client:start_link(#{transport => stdio}),
        {N, Pid}
    end, lists:seq(1, 10)),
    
    io:format("Spawned ~p clients~n", [length(Clients)]),
    
    %% Bomb each client
    Msg = <<0:100>>,
    
    lists:foreach(fun({N, Pid}) ->
        io:format("~nClient ~p:~n", [N]),
        
        Qi = get_queue_len(Pid),
        lists:foreach(fun(_) -> Pid ! {bomb, Msg} end, lists:seq(1, 100000)),
        Qf = get_queue_len(Pid),
        MemoryKB = get_memory_kb(Pid),
        
        io:format("  Queue: ~p -> ~p (~p added)~n", [Qi, Qf, Qf - Qi]),
        io:format("  Memory: ~.2f KB~n", [MemoryKB]),
        io:format("  Alive: ~p~n", [is_process_alive(Pid)]),
        
        case is_process_alive(Pid) of
            false -> ct:fail("Client ~p died during bombing", [N]);
            true -> ok
        end
    end, Clients),
    
    io:format("~nAll clients survived!~n"),
    ok.

%%--------------------------------------------------------------------
%% Internal Functions
%%--------------------------------------------------------------------

get_queue_len(Pid) ->
    case process_info(Pid, message_queue_len) of
        {message_queue_len, Q} -> Q;
        _ -> -1
    end.

get_memory_kb(Pid) ->
    case process_info(Pid, memory) of
        {memory, Mem} -> Mem / 1024;
        _ -> -1
    end.

get_memory_mb(Pid) ->
    case process_info(Pid, memory) of
        {memory, Mem} -> Mem / (1024 * 1024);
        _ -> -1
    end.
