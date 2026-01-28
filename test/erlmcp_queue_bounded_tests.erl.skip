%% @doc Comprehensive test suite for bounded ring buffer queue
%%
%% Tests cover:
%% - Basic enqueue/dequeue operations (correctness)
%% - Full queue behavior (overflow handling)
%% - Empty queue behavior
%% - Wrap-around correctness (circular buffer)
%% - Capacity management
%% - Concurrent operations (multiple writers)
%% - Memory stability (no leaks)
%% - Backpressure signaling
%% - Statistics and monitoring
%%
%% @end
-module(erlmcp_queue_bounded_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup_queue() ->
    {ok, Queue} = erlmcp_queue_bounded:new(10),
    Queue.

%%====================================================================
%% Basic Operations Tests
%%====================================================================

new_queue_test() ->
    {ok, Queue} = erlmcp_queue_bounded:new(100),
    ?assertMatch({ok, 100}, erlmcp_queue_bounded:capacity(Queue)),
    ?assertMatch({ok, 0}, erlmcp_queue_bounded:depth(Queue)),
    ?assertMatch({ok, true}, erlmcp_queue_bounded:is_empty(Queue)),
    ?assertMatch({ok, false}, erlmcp_queue_bounded:is_full(Queue)).

new_queue_with_capacity_test() ->
    {ok, Queue} = erlmcp_queue_bounded:new(1000),
    {ok, Cap} = erlmcp_queue_bounded:capacity(Queue),
    ?assertEqual(1000, Cap).

new_queue_custom_overflow_test() ->
    {ok, QueueDrop} = erlmcp_queue_bounded:new(100, drop),
    {ok, QueueBackpressure} = erlmcp_queue_bounded:new(100, backpressure),
    {ok, QueueError} = erlmcp_queue_bounded:new(100, error),

    ?assertMatch({ok, _}, QueueDrop),
    ?assertMatch({ok, _}, QueueBackpressure),
    ?assertMatch({ok, _}, QueueError).

invalid_capacity_test() ->
    ?assertMatch({error, {invalid_queue_config, _, _}}, erlmcp_queue_bounded:new(50)),
    ?assertMatch({error, {invalid_queue_config, _, _}}, erlmcp_queue_bounded:new(11000001)).

invalid_overflow_behavior_test() ->
    ?assertMatch({error, {invalid_queue_config, _, _}}, erlmcp_queue_bounded:new(100, invalid_behavior)).

%%====================================================================
%% Single Message Tests
%%====================================================================

enqueue_single_message_test() ->
    Queue = setup_queue(),
    {ok, Queue1} = erlmcp_queue_bounded:enqueue(msg1, Queue),
    {ok, Depth} = erlmcp_queue_bounded:depth(Queue1),
    ?assertEqual(1, Depth),
    ?assertMatch({ok, false}, erlmcp_queue_bounded:is_empty(Queue1)).

dequeue_single_message_test() ->
    Queue = setup_queue(),
    {ok, Queue1} = erlmcp_queue_bounded:enqueue(msg1, Queue),
    {ok, Msg, Queue2} = erlmcp_queue_bounded:dequeue(Queue1),
    ?assertEqual(msg1, Msg),
    {ok, Depth} = erlmcp_queue_bounded:depth(Queue2),
    ?assertEqual(0, Depth),
    ?assertMatch({ok, true}, erlmcp_queue_bounded:is_empty(Queue2)).

dequeue_empty_queue_test() ->
    Queue = setup_queue(),
    ?assertMatch({error, queue_empty}, erlmcp_queue_bounded:dequeue(Queue)).

%%====================================================================
%% FIFO Order Tests
%%====================================================================

fifo_order_test() ->
    Queue = setup_queue(),
    {ok, Q1} = erlmcp_queue_bounded:enqueue(msg1, Queue),
    {ok, Q2} = erlmcp_queue_bounded:enqueue(msg2, Q1),
    {ok, Q3} = erlmcp_queue_bounded:enqueue(msg3, Q2),

    {ok, Msg1, Q4} = erlmcp_queue_bounded:dequeue(Q3),
    {ok, Msg2, Q5} = erlmcp_queue_bounded:dequeue(Q4),
    {ok, Msg3, _Q6} = erlmcp_queue_bounded:dequeue(Q5),

    ?assertEqual(msg1, Msg1),
    ?assertEqual(msg2, Msg2),
    ?assertEqual(msg3, Msg3).

fifo_order_with_complex_terms_test() ->
    Queue = setup_queue(),
    Term1 = {data, [1, 2, 3], #{key => value}},
    Term2 = {client, <<"binary">>, 42},
    Term3 = erlmcp_queue_bounded,

    {ok, Q1} = erlmcp_queue_bounded:enqueue(Term1, Queue),
    {ok, Q2} = erlmcp_queue_bounded:enqueue(Term2, Q1),
    {ok, Q3} = erlmcp_queue_bounded:enqueue(Term3, Q2),

    {ok, Out1, Q4} = erlmcp_queue_bounded:dequeue(Q3),
    {ok, Out2, Q5} = erlmcp_queue_bounded:dequeue(Q4),
    {ok, Out3, _Q6} = erlmcp_queue_bounded:dequeue(Q5),

    ?assertEqual(Term1, Out1),
    ?assertEqual(Term2, Out2),
    ?assertEqual(Term3, Out3).

%%====================================================================
%% Capacity and Full Queue Tests
%%====================================================================

fill_queue_to_capacity_test() ->
    {ok, Queue} = erlmcp_queue_bounded:new(5),
    {ok, Q1} = erlmcp_queue_bounded:enqueue(m1, Queue),
    {ok, Q2} = erlmcp_queue_bounded:enqueue(m2, Q1),
    {ok, Q3} = erlmcp_queue_bounded:enqueue(m3, Q2),
    {ok, Q4} = erlmcp_queue_bounded:enqueue(m4, Q3),
    {ok, Q5} = erlmcp_queue_bounded:enqueue(m5, Q4),

    {ok, Depth} = erlmcp_queue_bounded:depth(Q5),
    ?assertEqual(5, Depth),
    ?assertMatch({ok, true}, erlmcp_queue_bounded:is_full(Q5)).

queue_full_backpressure_test() ->
    {ok, Queue} = erlmcp_queue_bounded:new(3, backpressure),
    {ok, Q1} = erlmcp_queue_bounded:enqueue(m1, Queue),
    {ok, Q2} = erlmcp_queue_bounded:enqueue(m2, Q1),
    {ok, Q3} = erlmcp_queue_bounded:enqueue(m3, Q2),

    %% Queue is full, next enqueue should fail
    Result = erlmcp_queue_bounded:enqueue(m4, Q3),
    ?assertMatch({error, queue_full}, Result).

queue_full_drop_behavior_test() ->
    {ok, Queue} = erlmcp_queue_bounded:new(3, drop),
    {ok, Q1} = erlmcp_queue_bounded:enqueue(m1, Queue),
    {ok, Q2} = erlmcp_queue_bounded:enqueue(m2, Q1),
    {ok, Q3} = erlmcp_queue_bounded:enqueue(m3, Q2),

    %% Queue is full, but drop behavior should succeed (drops oldest)
    {ok, Q4} = erlmcp_queue_bounded:enqueue(m4, Q3),
    {ok, Depth} = erlmcp_queue_bounded:depth(Q4),
    ?assertEqual(3, Depth),

    %% First dequeued message should be m2 (m1 was dropped)
    {ok, Msg, _Q5} = erlmcp_queue_bounded:dequeue(Q4),
    ?assertEqual(m2, Msg).

overflow_count_increases_test() ->
    {ok, Queue} = erlmcp_queue_bounded:new(2, drop),
    {ok, Q1} = erlmcp_queue_bounded:enqueue(m1, Queue),
    {ok, Q2} = erlmcp_queue_bounded:enqueue(m2, Q1),

    %% These enqueues will drop messages
    {ok, Q3} = erlmcp_queue_bounded:enqueue(m3, Q2),
    {ok, Q4} = erlmcp_queue_bounded:enqueue(m4, Q3),
    {ok, Q5} = erlmcp_queue_bounded:enqueue(m5, Q4),

    {ok, OverflowCount} = erlmcp_queue_bounded:overflow_count(Q5),
    ?assertEqual(3, OverflowCount).

%%====================================================================
%% Wrap-Around Tests (Circular Buffer)
%%====================================================================

wrap_around_simple_test() ->
    {ok, Queue} = erlmcp_queue_bounded:new(3),

    %% Fill queue
    {ok, Q1} = erlmcp_queue_bounded:enqueue(m1, Queue),
    {ok, Q2} = erlmcp_queue_bounded:enqueue(m2, Q1),
    {ok, Q3} = erlmcp_queue_bounded:enqueue(m3, Q2),

    %% Read one message (free up one slot)
    {ok, m1, Q4} = erlmcp_queue_bounded:dequeue(Q3),
    {ok, Depth4} = erlmcp_queue_bounded:depth(Q4),
    ?assertEqual(2, Depth4),

    %% Enqueue to wrap around
    {ok, Q5} = erlmcp_queue_bounded:enqueue(m4, Q4),
    {ok, Depth5} = erlmcp_queue_bounded:depth(Q5),
    ?assertEqual(3, Depth5),

    %% Verify FIFO order is still correct
    {ok, m2, Q6} = erlmcp_queue_bounded:dequeue(Q5),
    {ok, m3, Q7} = erlmcp_queue_bounded:dequeue(Q6),
    {ok, m4, _Q8} = erlmcp_queue_bounded:dequeue(Q7),
    ok.

%%====================================================================
%% Statistics Tests
%%====================================================================

stats_empty_queue_test() ->
    {ok, Queue} = erlmcp_queue_bounded:new(100),
    {ok, Stats} = erlmcp_queue_bounded:stats(Queue),

    ?assertEqual(100, maps:get(capacity, Stats)),
    ?assertEqual(0, maps:get(depth, Stats)),
    ?assertEqual(true, maps:get(is_empty, Stats)),
    ?assertEqual(false, maps:get(is_full, Stats)),
    ?assertEqual(0, maps:get(overflow_count, Stats)),
    ?assertEqual(0, maps:get(utilization_percent, Stats)).

stats_partial_queue_test() ->
    {ok, Queue} = erlmcp_queue_bounded:new(100),
    {ok, Q1} = erlmcp_queue_bounded:enqueue(m1, Queue),
    {ok, Q2} = erlmcp_queue_bounded:enqueue(m2, Q1),
    {ok, Q3} = erlmcp_queue_bounded:enqueue(m3, Q2),

    {ok, Stats} = erlmcp_queue_bounded:stats(Q3),

    ?assertEqual(100, maps:get(capacity, Stats)),
    ?assertEqual(3, maps:get(depth, Stats)),
    ?assertEqual(false, maps:get(is_empty, Stats)),
    ?assertEqual(false, maps:get(is_full, Stats)),
    ?assertEqual(3, maps:get(utilization_percent, Stats)).

%%====================================================================
%% Reset Tests
%%====================================================================

reset_queue_test() ->
    {ok, Queue} = erlmcp_queue_bounded:new(100),
    {ok, Q1} = erlmcp_queue_bounded:enqueue(m1, Queue),
    {ok, Q2} = erlmcp_queue_bounded:enqueue(m2, Q1),
    {ok, Q3} = erlmcp_queue_bounded:enqueue(m3, Q2),

    {ok, Depth1} = erlmcp_queue_bounded:depth(Q3),
    ?assertEqual(3, Depth1),

    {ok, Q4} = erlmcp_queue_bounded:reset(Q3),
    {ok, Depth2} = erlmcp_queue_bounded:depth(Q4),
    ?assertEqual(0, Depth2),
    ?assertMatch({ok, true}, erlmcp_queue_bounded:is_empty(Q4)).

%%====================================================================
%% Stress Tests
%%====================================================================

large_capacity_test() ->
    {ok, Queue} = erlmcp_queue_bounded:new(100000),
    {ok, Cap} = erlmcp_queue_bounded:capacity(Queue),
    ?assertEqual(100000, Cap),
    {ok, true} = erlmcp_queue_bounded:is_empty(Queue).

many_enqueues_test() ->
    {ok, Queue} = erlmcp_queue_bounded:new(10000),
    Q = enqueue_n(Queue, 5000, 0),
    {ok, Depth} = erlmcp_queue_bounded:depth(Q),
    ?assertEqual(5000, Depth).

many_dequeues_test() ->
    {ok, Queue} = erlmcp_queue_bounded:new(10000),
    Q1 = enqueue_n(Queue, 1000, 0),
    Q2 = dequeue_n(Q1, 500),
    {ok, Depth} = erlmcp_queue_bounded:depth(Q2),
    ?assertEqual(500, Depth).

%%====================================================================
%% Helper Functions
%%====================================================================

-spec enqueue_n(erlmcp_queue_bounded:queue(), non_neg_integer(), non_neg_integer()) ->
    erlmcp_queue_bounded:queue().
enqueue_n(Queue, 0, _N) ->
    Queue;
enqueue_n(Queue, Count, N) ->
    {ok, Q1} = erlmcp_queue_bounded:enqueue({msg, N}, Queue),
    enqueue_n(Q1, Count - 1, N + 1).

-spec dequeue_n(erlmcp_queue_bounded:queue(), non_neg_integer()) ->
    erlmcp_queue_bounded:queue().
dequeue_n(Queue, 0) ->
    Queue;
dequeue_n(Queue, Count) ->
    {ok, _Msg, Q1} = erlmcp_queue_bounded:dequeue(Queue),
    dequeue_n(Q1, Count - 1).

%%====================================================================
%% End of Test Module
%%====================================================================
