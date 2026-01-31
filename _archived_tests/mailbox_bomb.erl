%%% Mailbox Bombing Stress Test - Destructive Test #7
-module(mailbox_bomb).
-export([run_test/0, run_test/2]).

run_test() ->
    io:format("~n=== MAILBOX BOMBING CRASH TEST ===~n~n"),
    
    {ok, ServerPid} = start_server(),
    timer:sleep(1000),
    
    RegistryPid = whereis(erlmcp_registry),
    io:format("Server Pid: ~p~nRegistry Pid: ~p~n", [ServerPid, RegistryPid]),
    
    io:format("~nSpawning 10 client processes...~n"),
    Clients = spawn_clients(10),
    
    io:format("~n=== BASELINE METRICS ===~n"),
    print_metrics(ServerPid, "Server"),
    print_metrics(RegistryPid, "Registry"),
    print_client_metrics(Clients),
    
    io:format("~n=== PHASE 1: BOMBING SERVER MAILBOX ===~n"),
    bomb_process(ServerPid, "Server", 1000000),
    
    io:format("~n=== PHASE 2: BOMBING CLIENT MAILBOXES ===~n"),
    bomb_clients(Clients, 100000),
    
    io:format("~n=== FINAL STATUS ===~n"),
    print_final_status(ServerPid, RegistryPid, Clients),
    
    io:format("~nTest complete.~n"),
    {ok, server_bombed}.

run_test(ServerPid, Clients) ->
    io:format("~n=== QUICK MAILBOX BOMB TEST ===~n"),
    bomb_process(ServerPid, "Server", 100000),
    bomb_clients(Clients, 50000),
    print_final_status(ServerPid, whereis(erlmcp_registry), Clients),
    {ok, bombed}.

%% Internal functions
start_server() ->
    application:ensure_all_started(erlmcp),
    timer:sleep(500),
    case erlmcp_server:start_link(#{port => 10007}) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        Error -> Error
    end.

spawn_clients(Count) ->
    lists:map(fun(N) ->
        {ok, Pid} = erlmcp_client:start_link(#{transport => stdio}),
        {N, Pid}
    end, lists:seq(1, Count)).

get_queue_len(Pid) ->
    case process_info(Pid, message_queue_len) of
        {message_queue_len, QL} -> QL;
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

bomb_process(TargetPid, Name, _MaxMessages) ->
    io:format("~nTarget: ~s (Pid: ~p)~n", [Name, TargetPid]),
    
    case is_process_alive(TargetPid) of
        false -> io:format("ERROR: Process already dead!~n");
        true ->
            InitialPing = ping_process(TargetPid),
            io:format("Initial Ping: ~p ms~n", [InitialPing]),
            
            bomb_and_measure(TargetPid, 1000),
            bomb_and_measure(TargetPid, 10000),
            bomb_and_measure(TargetPid, 100000),
            bomb_and_measure(TargetPid, 1000000),
            
            case is_process_alive(TargetPid) of
                false -> io:format("~nPROCESS TERMINATED during bombing!~n");
                true ->
                    QFinal = get_queue_len(TargetPid),
                    io:format("~nProcess survived. Final queue: ~p~n", [QFinal])
            end
    end.

bomb_and_measure(TargetPid, Scale) ->
    io:format("~n~p messages:~n", [Scale]),
    
    case is_process_alive(TargetPid) of
        false ->
            io:format("  STATUS: Process already dead, skipping~n");
        true ->
            Msg = <<0:100>>,
            SendTime = os:system_time(microsecond),
            lists:foreach(fun(_) -> TargetPid ! {bomb, Msg} end, lists:seq(1, Scale)),
            SendElapsed = (os:system_time(microsecond) - SendTime) / 1000,
            
            io:format("  Send time: ~.2f ms~n", [SendElapsed]),
            
            Q = get_queue_len(TargetPid),
            M = get_memory_mb(TargetPid),
            
            io:format("  Queue Len: ~p~n", [Q]),
            io:format("  Memory: ~.2f MB~n", [M]),
            
            PingTime = ping_process(TargetPid),
            io:format("  Ping: ~p~n", [PingTime]),
            
            case is_process_alive(TargetPid) of
                false -> io:format("  STATUS: PROCESS DIED at ~p messages!~n", [Scale]);
                true when Q =:= -1 -> io:format("  STATUS: PROCESS INFO FAILED~n");
                true -> io:format("  STATUS: Alive~n")
            end,
            
            timer:sleep(100)
    end.

bomb_clients(Clients, MessagesPerClient) ->
    io:format("~nBombing ~p clients with ~p messages each...~n", 
              [length(Clients), MessagesPerClient]),
    
    DeadCount = lists:foldl(fun({N, Pid}, DeadAcc) ->
        io:format("~nClient ~p:~n", [N]),
        
        Qi = get_queue_len(Pid),
        
        Msg = <<0:100>>,
        SendTime = os:system_time(microsecond),
        lists:foreach(fun(_) -> Pid ! {bomb, Msg} end, lists:seq(1, MessagesPerClient)),
        SendElapsed = (os:system_time(microsecond) - SendTime) / 1000,
        
        io:format("  Send time: ~.2f ms~n", [SendElapsed]),
        
        Qf = get_queue_len(Pid),
        M = get_memory_kb(Pid),
        
        io:format("  Queue: ~p -> ~p (~p added)~n", [Qi, Qf, Qf - Qi]),
        io:format("  Memory: ~.2f KB~n", [M]),
        
        PingTime = ping_process(Pid),
        io:format("  Ping: ~p~n", [PingTime]),
        
        case is_process_alive(Pid) of
            false -> 
                io:format("  Status: dead~n"),
                DeadAcc + 1;
            true ->
                io:format("  Status: alive~n"),
                DeadAcc
        end
    end, 0, Clients),
    
    io:format("~nClient bomb summary: ~p/~p dead~n", [DeadCount, length(Clients)]).

ping_process(Pid) ->
    case is_process_alive(Pid) of
        false -> timeout;
        true ->
            Ref = monitor(process, Pid),
            Pid ! {ping, self(), Ref},
            receive
                {pong, Ref} ->
                    demonitor(Ref, [flush]),
                    1;
                {'DOWN', Ref, process, Pid, _Reason} ->
                    timeout
            after 5000 ->
                demonitor(Ref, [flush]),
                timeout
            end
    end.

print_metrics(Pid, Name) ->
    Q = get_queue_len(Pid),
    M = case process_info(Pid, memory) of
        {memory, Mem} -> Mem / 1024;
        _ -> -1
    end,
    H = case process_info(Pid, heap_size) of
        {heap_size, HS} -> HS / 1024;
        _ -> -1
    end,
    io:format("  ~s: Queue=~p Memory=~.2f KB Heap=~.2f KB~n", [Name, Q, M, H]).

print_client_metrics(Clients) ->
    lists:foreach(fun({N, Pid}) ->
        Q = get_queue_len(Pid),
        M = get_memory_kb(Pid),
        io:format("  Client ~p: Queue=~p Memory=~.2f KB~n", [N, Q, M])
    end, Clients).

print_final_status(ServerPid, RegistryPid, Clients) ->
    io:format("~nServer: "),
    case is_process_alive(ServerPid) of
        false -> io:format("DEAD~n");
        true -> 
            Q1 = get_queue_len(ServerPid),
            io:format("Alive (Queue: ~p)~n", [Q1])
    end,
    
    io:format("Registry: "),
    case RegistryPid of
        undefined ->
            io:format("Not running~n");
        _ when is_pid(RegistryPid) ->
            case is_process_alive(RegistryPid) of
                false -> io:format("DEAD~n");
                true -> 
                    Q2 = get_queue_len(RegistryPid),
                    io:format("Alive (Queue: ~p)~n", [Q2])
            end
    end,
    
    io:format("~nClients:~n"),
    AliveClients = [N || {N, Pid} <- Clients, is_process_alive(Pid)],
    DeadClients = [N || {N, Pid} <- Clients, not is_process_alive(Pid)],
    
    io:format("  Alive: ~p/~p~n", [length(AliveClients), length(Clients)]),
    io:format("  Dead: ~p/~p~n", [length(DeadClients), length(Clients)]),
    
    lists:foreach(fun({N, Pid}) ->
        Status = case is_process_alive(Pid) of
            true -> "alive";
            false -> "DEAD"
        end,
        Q = get_queue_len(Pid),
        io:format("    Client ~p: ~s (Queue: ~p)~n", [N, Status, Q])
    end, Clients).
