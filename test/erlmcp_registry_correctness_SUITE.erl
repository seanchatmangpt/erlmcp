-module(erlmcp_registry_correctness_SUITE).

%% Common Test Suite for registry correctness under extreme load
%% Validates: message delivery, no loss, no duplicates, concurrent subscribe/unsubscribe

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    groups/0
]).

-export([
    test_message_delivery_no_loss/1,
    test_routing_no_duplicates/1,
    test_concurrent_subscribe_unsubscribe/1,
    test_broadcast_delivery_correctness/1,
    test_binding_consistency/1,
    test_failure_recovery/1,
    test_partition_isolation_correctness/1,
    test_message_ordering/1,
    test_concurrent_binding_unbinding/1,
    test_registry_memory_safety/1
]).

-include_lib("common_test/include/ct.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Common Test Callbacks
%%====================================================================

all() ->
    [
        {group, message_delivery},
        {group, routing_correctness},
        {group, concurrent_operations},
        {group, failure_scenarios},
        {group, memory_safety}
    ].

groups() ->
    [
        {message_delivery, [], [
            test_message_delivery_no_loss,
            test_broadcast_delivery_correctness,
            test_message_ordering
        ]},
        {routing_correctness, [], [
            test_routing_no_duplicates,
            test_binding_consistency,
            test_partition_isolation_correctness
        ]},
        {concurrent_operations, [], [
            test_concurrent_subscribe_unsubscribe,
            test_concurrent_binding_unbinding
        ]},
        {failure_scenarios, [], [
            test_failure_recovery
        ]},
        {memory_safety, [], [
            test_registry_memory_safety
        ]}
    ].

init_per_suite(Config) ->
    ensure_gproc_started(),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    % Clean up registry entries before each test
    clear_all_registrations(),
    timer:sleep(50),

    % Start fresh registry
    {ok, Registry} = erlmcp_registry_sharded:start_link(16),
    [{registry, Registry} | Config].

end_per_testcase(_TestCase, Config) ->
    Registry = proplists:get_value(registry, Config),
    catch gen_server:stop(Registry, shutdown, 5000),
    clear_all_registrations(),
    timer:sleep(50),
    ok.

%%====================================================================
%% Test Cases: Message Delivery
%%====================================================================

test_message_delivery_no_loss(Config) ->
    %% Verify no messages are lost in routing
    MessageCount = 1000,

    % Create collector process
    Collector = start_collector(),

    % Create server that forwards messages to collector
    Server = spawn_link(fun() ->
        receive_and_forward(Collector, 0)
    end),

    Transport = spawn_link(fun() -> receive _ -> ok end end),

    % Register entities
    erlmcp_registry_sharded:register_server(test_server, Server, #{}),
    erlmcp_registry_sharded:register_transport(test_transport, Transport, #{}),
    erlmcp_registry_sharded:bind_transport_to_server(test_transport, test_server),

    % Send messages
    [erlmcp_registry_sharded:route_to_server(test_server, test_transport, {msg, N})
     || N <- lists:seq(1, MessageCount)],

    timer:sleep(1000),

    % Collect results
    Collector ! {get_all, self()},
    Collected = receive {all_messages, M} -> M after 2000 -> [] end,

    % Verify
    CollectedCount = length(Collected),
    ct:log("Sent ~p messages, received ~p~n", [MessageCount, CollectedCount]),

    % Cleanup
    unlink(Server), exit(Server, kill),
    unlink(Transport), exit(Transport, kill),
    unlink(Collector), exit(Collector, kill),

    CollectedCount =:= MessageCount.

test_broadcast_delivery_correctness(Config) ->
    %% Verify broadcast delivers to all bound transports
    NumTransports = 50,

    % Create server
    Server = spawn_link(fun() -> receive _ -> ok end end),
    erlmcp_registry_sharded:register_server(test_server_bc, Server, #{}),

    % Create and bind transports
    Collectors = [start_collector() || _ <- lists:seq(1, NumTransports)],
    TransportPids = [spawn_link(fun() ->
        receive
            {mcp_response, _, Message} -> C ! {received, Message}
        after 5000 -> ok
        end
    end) || C <- Collectors],

    lists:foreach(fun({N, Pid}) ->
        erlmcp_registry_sharded:register_transport(list_to_atom("trans_bc_" ++ integer_to_list(N)), Pid, {}),
        erlmcp_registry_sharded:bind_transport_to_server(list_to_atom("trans_bc_" ++ integer_to_list(N)), test_server_bc)
    end, lists:zip(lists:seq(1, NumTransports), TransportPids)),

    % Send broadcast
    BroadcastMessage = #{<<"type">> => <<"broadcast">>},
    erlmcp_registry_sharded:route_to_transport(broadcast, test_server_bc, BroadcastMessage),

    timer:sleep(500),

    % Verify all transports received
    ReceivedCounts = [
        case C of
            C when is_pid(C) ->
                C ! {get_count, self()},
                receive {count, Count} -> Count after 1000 -> 0 end
        end
        || C <- Collectors
    ],

    ReceivedAll = lists:all(fun(Count) -> Count >= 1 end, ReceivedCounts),
    SuccessCount = length([1 || C <- ReceivedCounts, C >= 1]),

    ct:log("Broadcast to ~p transports, received by ~p~n", [NumTransports, SuccessCount]),

    % Cleanup
    lists:foreach(fun(C) -> catch unlink(C), catch exit(C, kill) end, Collectors),
    lists:foreach(fun(P) -> catch unlink(P), catch exit(P, kill) end, TransportPids),
    unlink(Server), exit(Server, kill),

    ReceivedAll.

test_message_ordering(Config) ->
    %% Verify message order is preserved for single routing path
    MessageCount = 100,
    Collector = start_collector(),

    Server = spawn_link(fun() ->
        receive_and_forward_ordered(Collector, 0, [])
    end),

    Transport = spawn_link(fun() -> receive _ -> ok end end),

    erlmcp_registry_sharded:register_server(test_order_server, Server, #{}),
    erlmcp_registry_sharded:register_transport(test_order_transport, Transport, #{}),
    erlmcp_registry_sharded:bind_transport_to_server(test_order_transport, test_order_server),

    % Send ordered messages
    [erlmcp_registry_sharded:route_to_server(test_order_server, test_order_transport, N)
     || N <- lists:seq(1, MessageCount)],

    timer:sleep(500),

    Collector ! {get_all, self()},
    Received = receive {all_messages, M} -> M after 2000 -> [] end,

    % Verify order (messages should be received in order they were sent)
    IsOrdered = lists:all(fun({N, Msg}) ->
        Msg =:= N
    end, lists:zip(lists:seq(1, length(Received)), Received)),

    ct:log("Sent ~p ordered messages, received ~p in order: ~p~n",
        [MessageCount, length(Received), IsOrdered]),

    unlink(Server), exit(Server, kill),
    unlink(Transport), exit(Transport, kill),
    unlink(Collector), exit(Collector, kill),

    IsOrdered.

%%====================================================================
%% Test Cases: Routing Correctness
%%====================================================================

test_routing_no_duplicates(Config) ->
    %% Verify no message duplication in routing
    MessageCount = 500,
    Collector = start_collector(),

    Server = spawn_link(fun() ->
        receive_and_forward(Collector, 0)
    end),

    Transport = spawn_link(fun() -> receive _ -> ok end end),

    erlmcp_registry_sharded:register_server(test_no_dup_server, Server, #{}),
    erlmcp_registry_sharded:register_transport(test_no_dup_transport, Transport, #{}),

    % Send messages with tracking IDs
    [erlmcp_registry_sharded:route_to_server(test_no_dup_server, test_no_dup_transport, {msg, N})
     || N <- lists:seq(1, MessageCount)],

    timer:sleep(1000),

    Collector ! {get_all, self()},
    Received = receive {all_messages, M} -> M after 2000 -> [] end,

    % Check for duplicates
    ReceivedIds = [Id || {msg, Id} <- Received],
    UniqueIds = lists:usort(ReceivedIds),

    HasDuplicates = length(UniqueIds) =/= length(ReceivedIds),

    ct:log("Sent ~p messages, received ~p unique (duplicates: ~p)~n",
        [MessageCount, length(UniqueIds), HasDuplicates]),

    unlink(Server), exit(Server, kill),
    unlink(Transport), exit(Transport, kill),
    unlink(Collector), exit(Collector, kill),

    not HasDuplicates.

test_binding_consistency(Config) ->
    %% Verify transport-server bindings are consistent
    NumBindings = 100,

    % Create servers and transports
    Servers = [spawn_link(fun() -> receive _ -> ok end end) || _ <- lists:seq(1, 10)],
    Transports = [spawn_link(fun() -> receive _ -> ok end end) || _ <- lists:seq(1, 100)],

    lists:foreach(fun({N, Pid}) ->
        erlmcp_registry_sharded:register_server(list_to_atom("bind_server_" ++ integer_to_list(N)), Pid, {})
    end, lists:zip(lists:seq(1, length(Servers)), Servers)),

    lists:foreach(fun({N, Pid}) ->
        erlmcp_registry_sharded:register_transport(list_to_atom("bind_trans_" ++ integer_to_list(N)), Pid, {})
    end, lists:zip(lists:seq(1, length(Transports)), Transports)),

    % Perform bindings
    [begin
        TransIdx = (N rem 100) + 1,
        ServerIdx = (N rem 10) + 1,
        erlmcp_registry_sharded:bind_transport_to_server(
            list_to_atom("bind_trans_" ++ integer_to_list(TransIdx)),
            list_to_atom("bind_server_" ++ integer_to_list(ServerIdx))
        )
    end || N <- lists:seq(1, NumBindings)],

    % Verify all bindings
    AllBindingsValid = lists:all(fun(N) ->
        TransId = list_to_atom("bind_trans_" ++ integer_to_list(N)),
        case erlmcp_registry_sharded:get_server_for_transport(TransId) of
            {ok, _ServerId} -> true;
            {error, not_found} -> false
        end
    end, lists:seq(1, 100)),

    ct:log("Created ~p bindings, all valid: ~p~n", [NumBindings, AllBindingsValid]),

    % Cleanup
    lists:foreach(fun(P) -> catch unlink(P), catch exit(P, kill) end, Servers),
    lists:foreach(fun(P) -> catch unlink(P), catch exit(P, kill) end, Transports),

    AllBindingsValid.

test_partition_isolation_correctness(Config) ->
    %% Verify partitions don't interfere with each other
    NumPartitions = 16,
    EntriesPerPartition = 10,

    % Create entries that hash to specific partitions
    Entries = [
        {erlang:phash2(list_to_atom("entry_" ++ integer_to_list(N))) rem NumPartitions,
         list_to_atom("entry_" ++ integer_to_list(N))}
        || N <- lists:seq(1, EntriesPerPartition * NumPartitions)
    ],

    % Register all
    lists:foreach(fun({_Part, Id}) ->
        Pid = spawn_link(fun() -> receive _ -> ok end end),
        erlmcp_registry_sharded:register_server(Id, Pid, #{partition_test => true}),
        unlink(Pid), exit(Pid, kill)
    end, Entries),

    % Verify partition isolation: lookup should work correctly
    AllFound = lists:all(fun({_Part, Id}) ->
        case erlmcp_registry_sharded:find_server(Id) of
            {ok, {_, Config}} ->
                maps:get(partition_test, Config, false) =:= true;
            {error, not_found} ->
                false
        end
    end, Entries),

    ct:log("Verified partition isolation for ~p entries across ~p partitions~n",
        [length(Entries), NumPartitions]),

    AllFound.

%%====================================================================
%% Test Cases: Concurrent Operations
%%====================================================================

test_concurrent_subscribe_unsubscribe(Config) ->
    %% Stress test concurrent register/unregister
    NumWorkers = 50,
    OpsPerWorker = 100,

    Parent = self(),

    % Spawn workers that register/unregister
    Workers = [spawn_link(fun() ->
        worker_subscribe_unsubscribe(Parent, N, OpsPerWorker)
    end) || N <- lists:seq(1, NumWorkers)],

    % Collect results
    Results = [
        receive {worker_done, N, Result} -> Result end
        || _ <- Workers
    ],

    AllSuccess = lists:all(fun(R) -> R =:= ok end, Results),

    ct:log("Concurrent operations: ~p workers x ~p ops each, all success: ~p~n",
        [NumWorkers, OpsPerWorker, AllSuccess]),

    AllSuccess.

test_concurrent_binding_unbinding(Config) ->
    %% Stress test concurrent bind/unbind operations
    NumServers = 20,
    NumTransports = 100,
    NumBindOps = 500,

    Parent = self(),

    % Pre-create servers and transports
    Servers = [spawn_link(fun() -> receive _ -> ok end end) || _ <- lists:seq(1, NumServers)],
    Transports = [spawn_link(fun() -> receive _ -> ok end end) || _ <- lists:seq(1, NumTransports)],

    lists:foreach(fun({N, Pid}) ->
        erlmcp_registry_sharded:register_server(list_to_atom("server_" ++ integer_to_list(N)), Pid, {})
    end, lists:zip(lists:seq(1, NumServers), Servers)),

    lists:foreach(fun({N, Pid}) ->
        erlmcp_registry_sharded:register_transport(list_to_atom("transport_" ++ integer_to_list(N)), Pid, {})
    end, lists:zip(lists:seq(1, NumTransports), Transports)),

    % Spawn workers for concurrent bind/unbind
    Workers = [spawn_link(fun() ->
        worker_bind_unbind(Parent, N, NumBindOps, NumServers, NumTransports)
    end) || N <- lists:seq(1, 10)],

    Results = [
        receive {worker_done, N, Result} -> Result end
        || _ <- Workers
    ],

    AllSuccess = lists:all(fun(R) -> R =:= ok end, Results),

    ct:log("Concurrent binding: ~p workers x ~p ops each, all success: ~p~n",
        [length(Workers), NumBindOps, AllSuccess]),

    % Cleanup
    lists:foreach(fun(P) -> catch unlink(P), catch exit(P, kill) end, Servers),
    lists:foreach(fun(P) -> catch unlink(P), catch exit(P, kill) end, Transports),

    AllSuccess.

%%====================================================================
%% Test Cases: Failure Scenarios
%%====================================================================

test_failure_recovery(Config) ->
    %% Verify registry recovers from process deaths
    % Create and kill processes, verify cleanup
    Servers = [spawn(fun() -> receive _ -> ok end end) || _ <- lists:seq(1, 10)],
    Transports = [spawn(fun() -> receive _ -> ok end end) || _ <- lists:seq(1, 10)],

    lists:foreach(fun({N, Pid}) ->
        erlmcp_registry_sharded:register_server(list_to_atom("fail_server_" ++ integer_to_list(N)), Pid, {})
    end, lists:zip(lists:seq(1, length(Servers)), Servers)),

    lists:foreach(fun({N, Pid}) ->
        erlmcp_registry_sharded:register_transport(list_to_atom("fail_trans_" ++ integer_to_list(N)), Pid, {})
    end, lists:zip(lists:seq(1, length(Transports)), Transports)),

    InitialServerCount = length(erlmcp_registry_sharded:list_servers()),

    % Kill half the servers
    KilledServers = lists:sublist(Servers, 5),
    lists:foreach(fun(P) -> exit(P, kill) end, KilledServers),

    timer:sleep(500),

    % Verify cleanup (entries for dead processes should remain but be stale)
    FinalServerCount = length(erlmcp_registry_sharded:list_servers()),

    ct:log("Initial servers: ~p, after killing 5: ~p~n", [InitialServerCount, FinalServerCount]),

    % Should have same count (gproc handles monitoring)
    InitialServerCount =:= FinalServerCount.

%%====================================================================
%% Test Cases: Memory Safety
%%====================================================================

test_registry_memory_safety(Config) ->
    %% Verify no memory leaks with many register/unregister cycles
    Cycles = 1000,
    NumEntriesPerCycle = 100,

    InitialMemory = erlang:memory(total),

    [begin
        Servers = [spawn_link(fun() -> receive _ -> ok end end) || _ <- lists:seq(1, NumEntriesPerCycle)],
        lists:foreach(fun({N, Pid}) ->
            Id = list_to_atom("mem_test_" ++ integer_to_list(N) ++ "_" ++ integer_to_list(Cycle)),
            erlmcp_registry_sharded:register_server(Id, Pid, #{})
        end, lists:zip(lists:seq(1, NumEntriesPerCycle), Servers)),

        timer:sleep(10),

        lists:foreach(fun({N, Pid}) ->
            Id = list_to_atom("mem_test_" ++ integer_to_list(N) ++ "_" ++ integer_to_list(Cycle)),
            erlmcp_registry_sharded:unregister_server(Id),
            unlink(Pid),
            exit(Pid, kill)
        end, lists:zip(lists:seq(1, NumEntriesPerCycle), Servers))
    end || Cycle <- lists:seq(1, Cycles)],

    timer:sleep(500),

    FinalMemory = erlang:memory(total),
    MemoryGrowth = FinalMemory - InitialMemory,
    MemoryGrowthMB = MemoryGrowth / (1024 * 1024),

    ct:log("Memory growth after ~p cycles of ~p entries: ~.2f MB~n",
        [Cycles, NumEntriesPerCycle, MemoryGrowthMB]),

    % Memory growth should be reasonable (< 100MB for cleanup)
    MemoryGrowthMB < 100.

%%====================================================================
%% Helper Functions
%%====================================================================

ensure_gproc_started() ->
    case application:ensure_started(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end.

clear_all_registrations() ->
    ensure_gproc_started(),
    % Clear servers
    ServerPattern = [{{{n, l, {mcp, server, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    ServerEntries = gproc:select(ServerPattern),
    lists:foreach(fun({Id, Pid}) ->
        catch gproc:unreg_other({n, l, {mcp, server, Id}}, Pid)
    end, ServerEntries),

    % Clear transports
    TransportPattern = [{{{n, l, {mcp, transport, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    TransportEntries = gproc:select(TransportPattern),
    lists:foreach(fun({Id, Pid}) ->
        catch gproc:unreg_other({n, l, {mcp, transport, Id}}, Pid)
    end, TransportEntries),

    ok.

start_collector() ->
    spawn_link(fun() -> collector_loop([]) end).

collector_loop(Messages) ->
    receive
        {received, Message} ->
            collector_loop([Message | Messages]);
        {get_all, From} ->
            From ! {all_messages, lists:reverse(Messages)},
            collector_loop(Messages);
        {get_count, From} ->
            From ! {count, length(Messages)},
            collector_loop(Messages);
        _ ->
            collector_loop(Messages)
    after 10000 ->
        ok
    end.

receive_and_forward(Collector, Count) ->
    receive
        {mcp_message, _TransportId, Message} ->
            Collector ! {received, Message},
            receive_and_forward(Collector, Count + 1)
    after 5000 ->
        ok
    end.

receive_and_forward_ordered(Collector, Count, Acc) ->
    receive
        {mcp_message, _TransportId, Message} ->
            Collector ! {received, Message},
            receive_and_forward_ordered(Collector, Count + 1, [Message | Acc])
    after 5000 ->
        ok
    end.

worker_subscribe_unsubscribe(Parent, WorkerId, OpsPerWorker) ->
    try
        lists:foreach(fun(Op) ->
            ServerId = list_to_atom("worker_" ++ integer_to_list(WorkerId) ++ "_" ++ integer_to_list(Op)),
            Pid = spawn_link(fun() -> receive _ -> ok end end),
            erlmcp_registry_sharded:register_server(ServerId, Pid, #{}),
            timer:sleep(1),
            erlmcp_registry_sharded:unregister_server(ServerId),
            unlink(Pid),
            exit(Pid, kill)
        end, lists:seq(1, OpsPerWorker)),
        Parent ! {worker_done, WorkerId, ok}
    catch
        _:_ ->
            Parent ! {worker_done, WorkerId, error}
    end.

worker_bind_unbind(Parent, WorkerId, OpsPerBind, NumServers, NumTransports) ->
    try
        lists:foreach(fun(Op) ->
            TransIdx = (Op rem NumTransports) + 1,
            ServerIdx = (Op rem NumServers) + 1,
            erlmcp_registry_sharded:bind_transport_to_server(
                list_to_atom("transport_" ++ integer_to_list(TransIdx)),
                list_to_atom("server_" ++ integer_to_list(ServerIdx))
            ),
            timer:sleep(1),
            erlmcp_registry_sharded:unbind_transport(list_to_atom("transport_" ++ integer_to_list(TransIdx)))
        end, lists:seq(1, OpsPerBind)),
        Parent ! {worker_done, WorkerId, ok}
    catch
        _:_ ->
            Parent ! {worker_done, WorkerId, error}
    end.
