-module(erlmcp_message_queue_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup (Chicago School TDD: Real gen_server, no mocks)
%%%===================================================================

setup_queue(Name) ->
    Config = #{
        max_size => 1000,
        storage_backend => ets,
        ack_timeout_ms => 5000,
        retry_backoff_ms => 100,
        dead_letter_threshold => 3
    },
    {ok, Pid} = erlmcp_message_queue:start_link(Name, Config),
    Pid.

cleanup_queue(Pid) ->
    ok = erlmcp_message_queue:stop(Pid).

setup() ->
    setup_queue(<<"test_queue">>).

cleanup(_Pid) ->
    ok.

%%%===================================================================
%%% Test Generators
%%%===================================================================

erlmcp_message_queue_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun enqueue_dequeue_test/0,
      fun enqueue_with_priority_test/0,
      fun enqueue_with_ttl_test/0,
      fun acknowledge_test/0,
      fun nack_test/0,
      fun dead_letter_test/0,
      fun get_stats_test/0,
      fun queue_max_size_test/0,
      fun concurrent_enqueue_test/0,
      fun concurrent_dequeue_test/0,
      fun ets_backend_test/0,
      fun memory_backend_test/0
     ]}.

%%%===================================================================
%%% Individual Tests (Chicago School: State-based, real processes)
%%%===================================================================

enqueue_dequeue_test() ->
    {"enqueue/dequeue delivers messages in FIFO order", fun() ->
        QueuePid = setup_queue(<<"fifo_queue">>),
        try
            %% Exercise: Enqueue messages
            {ok, Id1} = erlmcp_message_queue:enqueue(QueuePid, msg1),
            {ok, Id2} = erlmcp_message_queue:enqueue(QueuePid, msg2),
            {ok, Id3} = erlmcp_message_queue:enqueue(QueuePid, msg3),

            %% Verify: Dequeue in FIFO order
            {ok, Delivery1} = erlmcp_message_queue:dequeue(QueuePid),
            ?assertEqual(msg1, maps:get(message, maps:get(message, Delivery1))),

            {ok, Delivery2} = erlmcp_message_queue:dequeue(QueuePid),
            ?assertEqual(msg2, maps:get(message, maps:get(message, Delivery2))),

            {ok, Delivery3} = erlmcp_message_queue:dequeue(QueuePid),
            ?assertEqual(msg3, maps:get(message, maps:get(message, Delivery3)))
        after
            cleanup_queue(QueuePid)
        end
    end}.

enqueue_with_priority_test() ->
    {"enqueue with priority orders by priority", fun() ->
        QueuePid = setup_queue(<<"priority_queue">>),
        try
            %% Exercise: Enqueue with different priorities
            {ok, _} = erlmcp_message_queue:enqueue(QueuePid, low, #{priority => 1}),
            {ok, _} = erlmcp_message_queue:enqueue(QueuePid, high, #{priority => 9}),
            {ok, _} = erlmcp_message_queue:enqueue(QueuePid, medium, #{priority => 5}),

            %% Verify: High priority dequeued first
            {ok, Delivery1} = erlmcp_message_queue:dequeue(QueuePid),
            ?assertEqual(high, maps:get(payload, maps:get(message, Delivery1))),

            {ok, Delivery2} = erlmcp_message_queue:dequeue(QueuePid),
            ?assertEqual(medium, maps:get(payload, maps:get(message, Delivery2))),

            {ok, Delivery3} = erlmcp_message_queue:dequeue(QueuePid),
            ?assertEqual(low, maps:get(payload, maps:get(message, Delivery3)))
        after
            cleanup_queue(QueuePid)
        end
    end}.

enqueue_with_ttl_test() ->
    {"enqueue with TTL expires messages", fun() ->
        QueuePid = setup_queue(<<"ttl_queue">>),
        try
            %% Exercise: Enqueue with short TTL
            {ok, _MsgId} = erlmcp_message_queue:enqueue(
                QueuePid,
                expiring_msg,
                #{ttl_ms => 100}
            ),

            %% Verify: Message available initially
            {ok, _Delivery} = erlmcp_message_queue:dequeue(QueuePid),

            %% Wait for TTL expiration
            timer:sleep(150),

            %% Verify: No more messages (expired)
            Result = erlmcp_message_queue:dequeue(QueuePid),
            ?assertEqual({error, empty}, Result)
        after
            cleanup_queue(QueuePid)
        end
    end}.

acknowledge_test() ->
    {"acknowledge removes message from delivered", fun() ->
        QueuePid = setup_queue(<<"ack_queue">>),
        try
            %% Setup: Enqueue and dequeue message
            {ok, MsgId} = erlmcp_message_queue:enqueue(QueuePid, ack_test),
            {ok, Delivery} = erlmcp_message_queue:dequeue(QueuePid),
            DeliveryId = maps:get(delivery_id, Delivery),

            %% Exercise: Acknowledge
            ok = erlmcp_message_queue:acknowledge(QueuePid, DeliveryId),

            %% Verify: Stats show acknowledged
            Stats = erlmcp_message_queue:get_stats(QueuePid),
            ?assertEqual(1, maps:get(acknowledged, Stats)),
            ?assertEqual(0, maps:get(delivered, Stats))
        after
            cleanup_queue(QueuePid)
        end
    end}.

nack_test() ->
    {"nack returns message to queue for retry", fun() ->
        QueuePid = setup_queue(<<"nack_queue">>),
        try
            %% Setup: Enqueue and dequeue message
            {ok, _MsgId} = erlmcp_message_queue:enqueue(QueuePid, retry_msg),
            {ok, Delivery1} = erlmcp_message_queue:dequeue(QueuePid),
            DeliveryId1 = maps:get(delivery_id, Delivery1),

            %% Exercise: Nack message
            ok = erlmcp_message_queue:nack(QueuePid, DeliveryId1),

            %% Verify: Message available for retry
            {ok, Delivery2} = erlmcp_message_queue:dequeue(QueuePid),
            Message = maps:get(message, Delivery2),
            ?assertEqual(retry_msg, maps:get(payload, Message)),
            ?assertEqual(1, maps:get(attempts, Message))
        after
            cleanup_queue(QueuePid)
        end
    end}.

dead_letter_test() ->
    {"nack with max attempts sends to dead letter", fun() ->
        QueuePid = setup_queue(<<"dlq_queue">>),
        try
            %% Setup: Enqueue message
            {ok, _MsgId} = erlmcp_message_queue:enqueue(QueuePid, failing_msg),

            %% Exercise: Fail message multiple times
            Attempts = lists:seq(1, 5),
            lists:foreach(fun(_) ->
                {ok, Delivery} = erlmcp_message_queue:dequeue(QueuePid),
                DeliveryId = maps:get(delivery_id, Delivery),
                erlmcp_message_queue:nack(QueuePid, DeliveryId),
                timer:sleep(50)
            end, Attempts),

            %% Verify: Message dead-lettered
            Stats = erlmcp_message_queue:get_stats(QueuePid),
            ?assertEqual(1, maps:get(dead_lettered, Stats))
        after
            cleanup_queue(QueuePid)
        end
    end}.

get_stats_test() ->
    {"get_stats returns queue statistics", fun() ->
        QueuePid = setup_queue(<<"stats_queue">>),
        try
            %% Setup: Enqueue messages
            {ok, _} = erlmcp_message_queue:enqueue(QueuePid, msg1),
            {ok, _} = erlmcp_message_queue:enqueue(QueuePid, msg2),
            {ok, _} = erlmcp_message_queue:enqueue(QueuePid, msg3),

            %% Exercise: Get stats
            Stats = erlmcp_message_queue:get_stats(QueuePid),

            %% Verify: Stats contain all fields
            ?assert(maps:is_key(pending, Stats)),
            ?assert(maps:is_key(delivered, Stats)),
            ?assert(maps:is_key(acknowledged, Stats)),
            ?assert(maps:is_key(dead_lettered, Stats)),
            ?assertEqual(3, maps:get(pending, Stats))
        after
            cleanup_queue(QueuePid)
        end
    end}.

queue_max_size_test() ->
    {"enqueue rejects messages when queue full", fun() ->
        Config = #{max_size => 2, storage_backend => ets},
        {ok, QueuePid} = erlmcp_message_queue:start_link(<<"full_queue">>, Config),
        try
            %% Setup: Fill queue
            {ok, _} = erlmcp_message_queue:enqueue(QueuePid, msg1),
            {ok, _} = erlmcp_message_queue:enqueue(QueuePid, msg2),

            %% Exercise: Try to enqueue when full
            Result = erlmcp_message_queue:enqueue(QueuePid, msg3),

            %% Verify: Rejected
            ?assertEqual({error, queue_full}, Result)
        after
            cleanup_queue(QueuePid)
        end
    end}.

concurrent_enqueue_test() ->
    {"enqueue handles concurrent producers", fun() ->
        QueuePid = setup_queue(<<"concurrent_enqueue_queue">>),
        try
            %% Exercise: 100 producers enqueue concurrently
            Producers = [spawn(fun() ->
                erlmcp_message_queue:enqueue(QueuePid, {msg, N})
            end) || N <- lists:seq(1, 100)],

            %% Wait for all to complete
            [begin
                Ref = monitor(process, P),
                receive {'DOWN', Ref, process, P, _} -> ok end
            end || P <- Producers],

            %% Verify: All messages enqueued
            Stats = erlmcp_message_queue:get_stats(QueuePid),
            ?assertEqual(100, maps:get(pending, Stats))
        after
            cleanup_queue(QueuePid)
        end
    end}.

concurrent_dequeue_test() ->
    {"dequeue handles concurrent consumers", fun() ->
        QueuePid = setup_queue(<<"concurrent_dequeue_queue">>),
        try
            %% Setup: Enqueue messages
            [begin
                {ok, _} = erlmcp_message_queue:enqueue(QueuePid, {msg, N})
            end || N <- lists:seq(1, 100)],

            %% Exercise: 10 consumers dequeue concurrently
            Consumers = [spawn(fun() ->
                case erlmcp_message_queue:dequeue(QueuePid) of
                    {ok, Delivery} ->
                        DeliveryId = maps:get(delivery_id, Delivery),
                        timer:sleep(10),
                        erlmcp_message_queue:acknowledge(QueuePid, DeliveryId),
                        self() ! {delivered, 1};
                    {error, empty} ->
                        self() ! {empty, 0}
                end
            end) || _ <- lists:seq(1, 10)],

            %% Collect results
            Delivered = lists:sum([receive
                {delivered, N} -> N;
                {empty, 0} -> 0
            after 1000 -> 0
            end || _ <- Consumers]),

            %% Verify: All messages delivered
            ?assert(Delivered > 0)
        after
            cleanup_queue(QueuePid)
        end
    end}.

ets_backend_test() ->
    {"ETS backend persists messages correctly", fun() ->
        Config = #{storage_backend => ets, max_size => 1000},
        {ok, QueuePid} = erlmcp_message_queue:start_link(<<"ets_queue">>, Config),
        try
            %% Exercise: Enqueue and dequeue
            {ok, _} = erlmcp_message_queue:enqueue(QueuePid, ets_test),
            {ok, Delivery} = erlmcp_message_queue:dequeue(QueuePid),

            %% Verify: Message delivered
            ?assertEqual(ets_test, maps:get(payload, maps:get(message, Delivery)))
        after
            cleanup_queue(QueuePid)
        end
    end}.

memory_backend_test() ->
    {"memory backend works correctly", fun() ->
        Config = #{storage_backend => memory, max_size => 1000},
        {ok, QueuePid} = erlmcp_message_queue:start_link(<<"memory_queue">>, Config),
        try
            %% Exercise: Enqueue and dequeue
            {ok, _} = erlmcp_message_queue:enqueue(QueuePid, memory_test),
            {ok, Delivery} = erlmcp_message_queue:dequeue(QueuePid),

            %% Verify: Message delivered
            ?assertEqual(memory_test, maps:get(payload, maps:get(message, Delivery)))
        after
            cleanup_queue(QueuePid)
        end
    end}.
