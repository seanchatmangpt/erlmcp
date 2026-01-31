-module(erlmcp_poc_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup/Teardown
%%====================================================================

%%====================================================================
%% erlmcp_telemetry_poc Tests
%%====================================================================

telemetry_poc_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_telemetry_poc:start_link(),
         Pid
     end,
     fun(Pid) ->
         erlmcp_telemetry_poc:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(telemetry_event_fires(Pid)),
          ?_test(telemetry_handler_receives(Pid)),
          ?_test(telemetry_multiple_handlers(Pid)),
          ?_test(telemetry_detach_handler(Pid))
         ]
     end}.

telemetry_event_fires(Pid) ->
    %% Setup: Create handler that sends message to self
    Self = self(),
    Handler = #{
        id => test_handler,
        module => erlmcp_poc_tests,
        function => telemetry_test_handler
    },

    %% Register handler
    ok = erlmcp_telemetry_poc:attach_handler(Pid, test_event, Handler),

    %% Exercise: Emit event
    Metadata = #{key => value, timestamp => erlang:system_time()},
    ok = erlmcp_telemetry_poc:emit_event(Pid, test_event, Metadata),

    %% Verify: Handler was called (observable behavior)
    receive
        {telemetry_event, test_event, Metadata} -> ok
    after 1000 ->
        ?assert(false)
    end.

telemetry_handler_receives(Pid) ->
    %% Setup: Attach handler
    Self = self(),
    Handler = #{id => handler1, module => erlmcp_poc_tests, function => telemetry_test_handler},
    ok = erlmcp_telemetry_poc:attach_handler(Pid, event1, Handler),

    %% Verify: Handler registered
    {ok, Handlers} = erlmcp_telemetry_poc:get_handlers(Pid, event1),
    ?assertEqual(1, length(Handlers)).

telemetry_multiple_handlers(Pid) ->
    %% Setup: Attach multiple handlers
    Handler1 = #{id => h1, module => erlmcp_poc_tests, function => telemetry_test_handler},
    Handler2 = #{id => h2, module => erlmcp_poc_tests, function => telemetry_test_handler},

    ok = erlmcp_telemetry_poc:attach_handler(Pid, multi_event, Handler1),
    ok = erlmcp_telemetry_poc:attach_handler(Pid, multi_event, Handler2),

    %% Verify: Both handlers registered
    {ok, Handlers} = erlmcp_telemetry_poc:get_handlers(Pid, multi_event),
    ?assertEqual(2, length(Handlers)).

telemetry_detach_handler(Pid) ->
    %% Setup: Attach handler
    Handler = #{id => detach_test, module => erlmcp_poc_tests, function => telemetry_test_handler},
    ok = erlmcp_telemetry_poc:attach_handler(Pid, detach_event, Handler),

    %% Exercise: Detach
    ok = erlmcp_telemetry_poc:detach_handler(Pid, detach_event),

    %% Verify: No handlers
    {ok, Handlers} = erlmcp_telemetry_poc:get_handlers(Pid, detach_event),
    ?assertEqual(0, length(Handlers)).

%% Telemetry test helper
telemetry_test_handler(EventName, Metadata) ->
    self() ! {telemetry_event, EventName, Metadata}.

%%====================================================================
%% erlmcp_pubsub_poc Tests
%%====================================================================

pubsub_poc_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_pubsub_poc:start_link(),
         Pid
     end,
     fun(Pid) ->
         erlmcp_pubsub_poc:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(pubsub_subscribe_and_publish(Pid)),
          ?_test(pubsub_fanout_multiple_subscribers(Pid)),
          ?_test(pubsub_unsubscribe(Pid)),
          ?_test(pubsub_subscriber_death_cleanup(Pid))
         ]
     end}.

pubsub_subscribe_and_publish(Pid) ->
    %% Setup: Subscribe self to topic
    ok = erlmcp_pubsub_poc:subscribe(Pid, test_topic, self()),

    %% Exercise: Publish message
    Message = {test, data, 123},
    ok = erlmcp_pubsub_poc:publish(Pid, test_topic, Message),

    %% Verify: Received message (observable behavior)
    receive
        {pubsub_message, test_topic, Message} -> ok
    after 1000 ->
        ?assert(false)
    end.

pubsub_fanout_multiple_subscribers(Pid) ->
    %% Setup: Create multiple subscriber processes
    Self = self(),
    Sub1 = spawn(fun() -> pubsub_subscriber_loop(Self, sub1) end),
    Sub2 = spawn(fun() -> pubsub_subscriber_loop(Self, sub2) end),
    Sub3 = spawn(fun() -> pubsub_subscriber_loop(Self, sub3) end),

    ok = erlmcp_pubsub_poc:subscribe(Pid, fanout_topic, Sub1),
    ok = erlmcp_pubsub_poc:subscribe(Pid, fanout_topic, Sub2),
    ok = erlmcp_pubsub_poc:subscribe(Pid, fanout_topic, Sub3),

    %% Exercise: Publish to all
    ok = erlmcp_pubsub_poc:publish(Pid, fanout_topic, broadcast_message),

    %% Verify: All subscribers received message (fan-out behavior)
    Received = lists:sort([
        receive {received, Id} -> Id after 1000 -> timeout end,
        receive {received, Id2} -> Id2 after 1000 -> timeout end,
        receive {received, Id3} -> Id3 after 1000 -> timeout end
    ]),

    ?assertEqual([sub1, sub2, sub3], Received),

    %% Cleanup
    Sub1 ! stop,
    Sub2 ! stop,
    Sub3 ! stop.

pubsub_unsubscribe(Pid) ->
    %% Setup: Subscribe
    ok = erlmcp_pubsub_poc:subscribe(Pid, unsub_topic, self()),

    %% Verify: Subscribed
    {ok, Subs} = erlmcp_pubsub_poc:get_subscribers(Pid, unsub_topic),
    ?assertEqual(1, length(Subs)),

    %% Exercise: Unsubscribe
    ok = erlmcp_pubsub_poc:unsubscribe(Pid, unsub_topic),

    %% Verify: No subscribers
    {ok, NoSubs} = erlmcp_pubsub_poc:get_subscribers(Pid, unsub_topic),
    ?assertEqual(0, length(NoSubs)).

pubsub_subscriber_death_cleanup(Pid) ->
    %% Setup: Create subscriber that will die
    Sub = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_pubsub_poc:subscribe(Pid, death_topic, Sub),

    %% Verify: Subscribed
    {ok, Subs1} = erlmcp_pubsub_poc:get_subscribers(Pid, death_topic),
    ?assertEqual(1, length(Subs1)),

    %% Exercise: Kill subscriber (real process death)
    exit(Sub, kill),
    timer:sleep(100),

    %% Verify: Auto-cleanup (observable behavior)
    {ok, Subs2} = erlmcp_pubsub_poc:get_subscribers(Pid, death_topic),
    ?assertEqual(0, length(Subs2)).

%% Pubsub test helper
pubsub_subscriber_loop(Parent, Id) ->
    receive
        {pubsub_message, _Topic, _Message} ->
            Parent ! {received, Id},
            pubsub_subscriber_loop(Parent, Id);
        stop ->
            ok
    end.

%%====================================================================
%% erlmcp_distributed_registry_poc Tests
%%====================================================================

distributed_registry_poc_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_distributed_registry_poc:start_link(),
         Pid
     end,
     fun(Pid) ->
         erlmcp_distributed_registry_poc:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(registry_register_and_lookup(Pid)),
          ?_test(registry_duplicate_name_error(Pid)),
          ?_test(registry_unregister(Pid)),
          ?_test(registry_process_death_cleanup(Pid)),
          ?_test(registry_get_all_names(Pid))
         ]
     end}.

registry_register_and_lookup(Pid) ->
    %% Setup: Spawn process to register
    Process = spawn(fun() -> receive stop -> ok end end),

    %% Exercise: Register
    ok = erlmcp_distributed_registry_poc:register_name(Pid, test_name, Process),

    %% Verify: Lookup returns process (observable state)
    {ok, FoundPid} = erlmcp_distributed_registry_poc:whereis_name(Pid, test_name),
    ?assertEqual(Process, FoundPid),

    %% Cleanup
    Process ! stop.

registry_duplicate_name_error(Pid) ->
    %% Setup: Register first process
    Proc1 = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_distributed_registry_poc:register_name(Pid, dup_name, Proc1),

    %% Exercise: Try to register another process with same name
    Proc2 = spawn(fun() -> receive stop -> ok end end),
    Result = erlmcp_distributed_registry_poc:register_name(Pid, dup_name, Proc2),

    %% Verify: Error returned (observable behavior)
    ?assertEqual({error, already_registered}, Result),

    %% Cleanup
    Proc1 ! stop,
    Proc2 ! stop.

registry_unregister(Pid) ->
    %% Setup: Register process
    Process = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_distributed_registry_poc:register_name(Pid, unreg_name, Process),

    %% Exercise: Unregister
    ok = erlmcp_distributed_registry_poc:unregister_name(Pid, unreg_name),

    %% Verify: Not found after unregister
    Result = erlmcp_distributed_registry_poc:whereis_name(Pid, unreg_name),
    ?assertEqual({error, not_found}, Result),

    %% Cleanup
    Process ! stop.

registry_process_death_cleanup(Pid) ->
    %% Setup: Register process
    Process = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_distributed_registry_poc:register_name(Pid, death_name, Process),

    %% Verify: Registered
    {ok, _} = erlmcp_distributed_registry_poc:whereis_name(Pid, death_name),

    %% Exercise: Kill registered process
    exit(Process, kill),
    timer:sleep(100),

    %% Verify: Auto-cleanup on death (observable behavior)
    Result = erlmcp_distributed_registry_poc:whereis_name(Pid, death_name),
    ?assertEqual({error, not_found}, Result).

registry_get_all_names(Pid) ->
    %% Setup: Register multiple names
    P1 = spawn(fun() -> receive stop -> ok end end),
    P2 = spawn(fun() -> receive stop -> ok end end),
    P3 = spawn(fun() -> receive stop -> ok end end),

    ok = erlmcp_distributed_registry_poc:register_name(Pid, name1, P1),
    ok = erlmcp_distributed_registry_poc:register_name(Pid, name2, P2),
    ok = erlmcp_distributed_registry_poc:register_name(Pid, name3, P3),

    %% Exercise: Get all names
    {ok, Names} = erlmcp_distributed_registry_poc:get_all_names(Pid),

    %% Verify: All names present
    ?assertEqual(3, length(Names)),
    ?assert(lists:member(name1, Names)),
    ?assert(lists:member(name2, Names)),
    ?assert(lists:member(name3, Names)),

    %% Cleanup
    P1 ! stop,
    P2 ! stop,
    P3 ! stop.

%%====================================================================
%% erlmcp_consensus_poc Tests
%%====================================================================

consensus_poc_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
          ?_test(consensus_single_node_becomes_leader()),
          ?_test(consensus_leader_election()),
          ?_test(consensus_leader_failure_reelection()),
          ?_test(consensus_get_state())
         ]
     end}.

consensus_single_node_becomes_leader() ->
    %% Setup: Start single node
    {ok, Node} = erlmcp_consensus_poc:start_link(node1),

    %% Wait for election timeout
    timer:sleep(1200),

    %% Verify: Single node becomes leader (observable behavior)
    {ok, State} = erlmcp_consensus_poc:get_state(Node),
    ?assertEqual(leader, State),

    %% Cleanup
    erlmcp_consensus_poc:stop(Node).

consensus_leader_election() ->
    %% Setup: Start multiple nodes
    {ok, Node1} = erlmcp_consensus_poc:start_link(node1),
    {ok, Node2} = erlmcp_consensus_poc:start_link(node2),
    {ok, Node3} = erlmcp_consensus_poc:start_link(node3),

    %% Form cluster
    ok = erlmcp_consensus_poc:join_cluster(Node1, [Node2, Node3]),
    ok = erlmcp_consensus_poc:join_cluster(Node2, [Node1, Node3]),
    ok = erlmcp_consensus_poc:join_cluster(Node3, [Node1, Node2]),

    %% Wait for election
    timer:sleep(1500),

    %% Verify: One leader elected (observable behavior)
    States = [
        erlmcp_consensus_poc:get_state(Node1),
        erlmcp_consensus_poc:get_state(Node2),
        erlmcp_consensus_poc:get_state(Node3)
    ],

    Leaders = [N || {ok, leader} = N <- States],
    ?assertEqual(1, length(Leaders)),

    %% Cleanup
    erlmcp_consensus_poc:stop(Node1),
    erlmcp_consensus_poc:stop(Node2),
    erlmcp_consensus_poc:stop(Node3).

consensus_leader_failure_reelection() ->
    %% Setup: Start nodes and elect leader
    {ok, Node1} = erlmcp_consensus_poc:start_link(node1),
    {ok, Node2} = erlmcp_consensus_poc:start_link(node2),

    ok = erlmcp_consensus_poc:join_cluster(Node1, [Node2]),
    ok = erlmcp_consensus_poc:join_cluster(Node2, [Node1]),

    timer:sleep(1500),

    %% Find leader
    {ok, State1} = erlmcp_consensus_poc:get_state(Node1),
    {ok, State2} = erlmcp_consensus_poc:get_state(Node2),

    {Leader, Follower} = case State1 of
        leader -> {Node1, Node2};
        _ -> {Node2, Node1}
    end,

    %% Exercise: Kill leader (real process death)
    exit(Leader, kill),
    timer:sleep(1500),

    %% Verify: Follower becomes new leader (observable behavior)
    {ok, NewState} = erlmcp_consensus_poc:get_state(Follower),
    ?assertEqual(leader, NewState),

    %% Cleanup
    erlmcp_consensus_poc:stop(Follower).

consensus_get_state() ->
    %% Setup: Start node
    {ok, Node} = erlmcp_consensus_poc:start_link(node_test),

    %% Verify: Initial state is follower
    {ok, State} = erlmcp_consensus_poc:get_state(Node),
    ?assertEqual(follower, State),

    %% Cleanup
    erlmcp_consensus_poc:stop(Node).

%%====================================================================
%% erlmcp_streaming_poc Tests
%%====================================================================

streaming_poc_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_streaming_poc:start_link(),
         Pid
     end,
     fun(Pid) ->
         erlmcp_streaming_poc:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(streaming_chunk_delivery(Pid)),
          ?_test(streaming_multiple_chunks(Pid)),
          ?_test(streaming_completion_notification(Pid)),
          ?_test(streaming_manual_get_chunk(Pid))
         ]
     end}.

streaming_chunk_delivery(Pid) ->
    %% Setup: Start stream with small data
    Data = <<"Hello, World!">>,
    StreamId = stream1,

    ok = erlmcp_streaming_poc:start_stream(Pid, StreamId, #{
        data => Data,
        chunk_size => 5,
        subscriber => self()
    }),

    %% Verify: Receive chunks (observable behavior)
    receive
        {stream_chunk, StreamId, Chunk1} ->
            ?assertEqual(<<"Hello">>, Chunk1)
    after 1000 ->
        ?assert(false)
    end.

streaming_multiple_chunks(Pid) ->
    %% Setup: Stream with multiple chunks
    Data = <<"1234567890">>,
    StreamId = stream2,

    ok = erlmcp_streaming_poc:start_stream(Pid, StreamId, #{
        data => Data,
        chunk_size => 3,
        subscriber => self()
    }),

    %% Verify: Receive all chunks in order
    Chunks = collect_chunks(StreamId, []),
    ExpectedChunks = [<<"123">>, <<"456">>, <<"789">>, <<"0">>],
    ?assertEqual(ExpectedChunks, Chunks).

streaming_completion_notification(Pid) ->
    %% Setup: Small stream
    Data = <<"ABC">>,
    StreamId = stream3,

    ok = erlmcp_streaming_poc:start_stream(Pid, StreamId, #{
        data => Data,
        chunk_size => 10,
        subscriber => self()
    }),

    %% Collect chunk
    receive {stream_chunk, StreamId, _} -> ok after 1000 -> ?assert(false) end,

    %% Verify: Completion notification
    receive
        {stream_complete, StreamId} -> ok
    after 1000 ->
        ?assert(false)
    end.

streaming_manual_get_chunk(Pid) ->
    %% Create stream but use manual get_chunk instead of auto-delivery
    Data = <<"ABCDEF">>,
    StreamId = manual_stream,

    %% Start stream with different subscriber (not self)
    DummySub = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_streaming_poc:start_stream(Pid, StreamId, #{
        data => Data,
        chunk_size => 2,
        subscriber => DummySub
    }),

    %% Wait for auto-delivery to subscriber
    timer:sleep(100),

    %% Exercise: Manual get should return error (stream complete)
    Result = erlmcp_streaming_poc:get_chunk(Pid, StreamId),
    ?assertEqual({error, stream_complete}, Result),

    %% Cleanup
    DummySub ! stop.

%% Streaming test helper
collect_chunks(StreamId, Acc) ->
    receive
        {stream_chunk, StreamId, Chunk} ->
            collect_chunks(StreamId, Acc ++ [Chunk]);
        {stream_complete, StreamId} ->
            Acc
    after 2000 ->
        Acc
    end.

%%====================================================================
%% erlmcp_circuit_breaker_poc Tests
%%====================================================================

circuit_breaker_poc_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_circuit_breaker_poc:start_link([
             {failure_threshold, 3},
             {success_threshold, 2},
             {timeout, 1000}
         ]),
         Pid
     end,
     fun(Pid) ->
         erlmcp_circuit_breaker_poc:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(circuit_breaker_closed_state(Pid)),
          ?_test(circuit_breaker_trip_to_open(Pid)),
          ?_test(circuit_breaker_half_open_recovery(Pid)),
          ?_test(circuit_breaker_stats(Pid)),
          ?_test(circuit_breaker_reset(Pid))
         ]
     end}.

circuit_breaker_closed_state(Pid) ->
    %% Verify: Initial state is closed
    {ok, State} = erlmcp_circuit_breaker_poc:get_state(Pid),
    ?assertEqual(closed, State),

    %% Exercise: Successful call
    Result = erlmcp_circuit_breaker_poc:call(Pid, fun() -> ok end),
    ?assertEqual({ok, ok}, Result).

circuit_breaker_trip_to_open(Pid) ->
    %% Setup: Make calls that fail (threshold = 3)
    erlmcp_circuit_breaker_poc:call(Pid, fun() -> error(fail) end),
    erlmcp_circuit_breaker_poc:call(Pid, fun() -> error(fail) end),
    erlmcp_circuit_breaker_poc:call(Pid, fun() -> error(fail) end),

    %% Verify: Circuit is now open (observable state transition)
    {ok, State} = erlmcp_circuit_breaker_poc:get_state(Pid),
    ?assertEqual(open, State),

    %% Verify: Calls rejected
    Result = erlmcp_circuit_breaker_poc:call(Pid, fun() -> ok end),
    ?assertEqual({error, circuit_open}, Result).

circuit_breaker_half_open_recovery(Pid) ->
    %% Setup: Trip circuit
    erlmcp_circuit_breaker_poc:call(Pid, fun() -> error(fail) end),
    erlmcp_circuit_breaker_poc:call(Pid, fun() -> error(fail) end),
    erlmcp_circuit_breaker_poc:call(Pid, fun() -> error(fail) end),

    {ok, open} = erlmcp_circuit_breaker_poc:get_state(Pid),

    %% Wait for timeout (transitions to half_open)
    timer:sleep(1200),

    %% Verify: State is half_open
    {ok, HalfOpenState} = erlmcp_circuit_breaker_poc:get_state(Pid),
    ?assertEqual(half_open, HalfOpenState),

    %% Exercise: Successful calls (threshold = 2)
    erlmcp_circuit_breaker_poc:call(Pid, fun() -> success1 end),
    erlmcp_circuit_breaker_poc:call(Pid, fun() -> success2 end),

    %% Verify: Circuit closed again (observable recovery)
    {ok, ClosedState} = erlmcp_circuit_breaker_poc:get_state(Pid),
    ?assertEqual(closed, ClosedState).

circuit_breaker_stats(Pid) ->
    %% Exercise: Get stats
    {ok, Stats} = erlmcp_circuit_breaker_poc:get_stats(Pid),

    %% Verify: Stats structure (observable behavior)
    ?assert(maps:is_key(circuit_state, Stats)),
    ?assert(maps:is_key(failure_count, Stats)),
    ?assert(maps:is_key(success_count, Stats)),
    ?assert(maps:is_key(failure_threshold, Stats)),
    ?assert(maps:is_key(success_threshold, Stats)).

circuit_breaker_reset(Pid) ->
    %% Setup: Make some failures
    erlmcp_circuit_breaker_poc:call(Pid, fun() -> error(fail) end),
    erlmcp_circuit_breaker_poc:call(Pid, fun() -> error(fail) end),

    %% Exercise: Reset
    ok = erlmcp_circuit_breaker_poc:reset(Pid),

    %% Verify: State reset to closed
    {ok, State} = erlmcp_circuit_breaker_poc:get_state(Pid),
    ?assertEqual(closed, State),

    %% Verify: Failure count reset
    {ok, Stats} = erlmcp_circuit_breaker_poc:get_stats(Pid),
    ?assertEqual(0, maps:get(failure_count, Stats)).

%%====================================================================
%% erlmcp_pool_poc Tests
%%====================================================================

pool_poc_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = erlmcp_pool_poc:start_link(5),
         Pid
     end,
     fun(Pid) ->
         erlmcp_pool_poc:stop(Pid)
     end,
     fun(Pid) ->
         [
          ?_test(pool_checkout_checkin(Pid)),
          ?_test(pool_exhaust_resources(Pid)),
          ?_test(pool_client_death_returns_resource(Pid)),
          ?_test(pool_stats(Pid)),
          ?_test(pool_queuing_waiters(Pid))
         ]
     end}.

pool_checkout_checkin(Pid) ->
    %% Exercise: Checkout resource
    {ok, Resource} = erlmcp_pool_poc:checkout(Pid),
    ?assert(is_integer(Resource)),

    %% Verify: Resource checked out
    {ok, Stats1} = erlmcp_pool_poc:get_stats(Pid),
    ?assertEqual(1, maps:get(checked_out, Stats1)),
    ?assertEqual(4, maps:get(available, Stats1)),

    %% Exercise: Checkin
    ok = erlmcp_pool_poc:checkin(Pid, Resource),

    %% Verify: Resource returned (observable state)
    {ok, Stats2} = erlmcp_pool_poc:get_stats(Pid),
    ?assertEqual(0, maps:get(checked_out, Stats2)),
    ?assertEqual(5, maps:get(available, Stats2)).

pool_exhaust_resources(Pid) ->
    %% Setup: Checkout all resources (pool size = 5)
    {ok, R1} = erlmcp_pool_poc:checkout(Pid),
    {ok, R2} = erlmcp_pool_poc:checkout(Pid),
    {ok, R3} = erlmcp_pool_poc:checkout(Pid),
    {ok, R4} = erlmcp_pool_poc:checkout(Pid),
    {ok, R5} = erlmcp_pool_poc:checkout(Pid),

    %% Verify: Pool exhausted
    {ok, Stats} = erlmcp_pool_poc:get_stats(Pid),
    ?assertEqual(0, maps:get(available, Stats)),
    ?assertEqual(5, maps:get(checked_out, Stats)),

    %% Cleanup
    ok = erlmcp_pool_poc:checkin(Pid, R1),
    ok = erlmcp_pool_poc:checkin(Pid, R2),
    ok = erlmcp_pool_poc:checkin(Pid, R3),
    ok = erlmcp_pool_poc:checkin(Pid, R4),
    ok = erlmcp_pool_poc:checkin(Pid, R5).

pool_client_death_returns_resource(Pid) ->
    %% Setup: Client process checks out resource
    Self = self(),
    Client = spawn(fun() ->
        {ok, Resource} = erlmcp_pool_poc:checkout(Pid),
        Self ! {checked_out, Resource},
        receive stop -> ok end
    end),

    %% Wait for checkout
    Resource = receive
        {checked_out, R} -> R
    after 1000 ->
        ?assert(false)
    end,

    %% Verify: Resource checked out
    {ok, Stats1} = erlmcp_pool_poc:get_stats(Pid),
    ?assertEqual(1, maps:get(checked_out, Stats1)),

    %% Exercise: Kill client (real process death)
    exit(Client, kill),
    timer:sleep(100),

    %% Verify: Resource auto-returned (observable cleanup)
    {ok, Stats2} = erlmcp_pool_poc:get_stats(Pid),
    ?assertEqual(0, maps:get(checked_out, Stats2)),
    ?assertEqual(5, maps:get(available, Stats2)).

pool_stats(Pid) ->
    %% Exercise: Get stats
    {ok, Stats} = erlmcp_pool_poc:get_stats(Pid),

    %% Verify: Stats structure (observable behavior)
    ?assertEqual(5, maps:get(pool_size, Stats)),
    ?assert(maps:is_key(available, Stats)),
    ?assert(maps:is_key(checked_out, Stats)),
    ?assert(maps:is_key(waiting, Stats)).

pool_queuing_waiters(Pid) ->
    %% Setup: Exhaust pool
    {ok, R1} = erlmcp_pool_poc:checkout(Pid),
    {ok, R2} = erlmcp_pool_poc:checkout(Pid),
    {ok, R3} = erlmcp_pool_poc:checkout(Pid),
    {ok, R4} = erlmcp_pool_poc:checkout(Pid),
    {ok, R5} = erlmcp_pool_poc:checkout(Pid),

    %% Create waiter process
    Self = self(),
    Waiter = spawn(fun() ->
        Result = erlmcp_pool_poc:checkout(Pid),
        Self ! {waiter_got, Result}
    end),

    timer:sleep(100),

    %% Verify: Waiter is queued
    {ok, Stats1} = erlmcp_pool_poc:get_stats(Pid),
    ?assertEqual(1, maps:get(waiting, Stats1)),

    %% Exercise: Return resource (should go to waiter)
    ok = erlmcp_pool_poc:checkin(Pid, R1),

    %% Verify: Waiter received resource
    receive
        {waiter_got, {ok, _Resource}} -> ok
    after 1000 ->
        ?assert(false)
    end,

    %% Cleanup
    ok = erlmcp_pool_poc:checkin(Pid, R2),
    ok = erlmcp_pool_poc:checkin(Pid, R3),
    ok = erlmcp_pool_poc:checkin(Pid, R4),
    ok = erlmcp_pool_poc:checkin(Pid, R5).
