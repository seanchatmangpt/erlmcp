%% @doc Bounded Ring Buffer Queue for MCP High-Performance Message Processing
-module(erlmcp_queue_bounded).

-export([
    new/1, new/2,
    enqueue/2, dequeue/1,
    depth/1, is_full/1, is_empty/1,
    capacity/1, overflow_count/1,
    reset/1,
    stats/1, set_overflow_behavior/2,
    to_list/1
]).

-record(queue, {
    capacity :: pos_integer(),
    read_ptr :: non_neg_integer(),
    write_ptr :: non_neg_integer(),
    buffer :: tuple(),
    overflow_count :: non_neg_integer(),
    overflow_behavior :: drop | backpressure | error
}).

-type queue() :: #queue{}.
-type message() :: term().
-type overflow_behavior() :: drop | backpressure | error.

-export_type([queue/0, message/0, overflow_behavior/0]).

-define(DEFAULT_CAPACITY, 10000).
-define(MIN_CAPACITY, 100).
-define(MAX_CAPACITY, 10000000).

%% API Functions

-spec new(pos_integer()) -> {ok, queue()} | {error, term()}.
new(Capacity) ->
    new(Capacity, backpressure).

-spec new(pos_integer(), overflow_behavior()) -> {ok, queue()} | {error, term()}.
new(Capacity, OverflowBehavior) when
    is_integer(Capacity), Capacity >= ?MIN_CAPACITY, Capacity =< ?MAX_CAPACITY,
    (OverflowBehavior =:= drop orelse OverflowBehavior =:= backpressure orelse OverflowBehavior =:= error)
->
    Buffer = erlang:make_tuple(Capacity, '$empty'),
    Queue = #queue{
        capacity = Capacity,
        read_ptr = 0,
        write_ptr = 0,
        buffer = Buffer,
        overflow_count = 0,
        overflow_behavior = OverflowBehavior
    },
    {ok, Queue};

new(Capacity, OverflowBehavior) ->
    {error, {invalid_queue_config, {capacity, Capacity}, {overflow_behavior, OverflowBehavior}}}.

-spec enqueue(message(), queue()) -> {ok, queue()} | {error, term()}.
enqueue(Msg, Queue) when is_record(Queue, queue) ->
    NextWritePtr = (Queue#queue.write_ptr + 1) rem Queue#queue.capacity,

    case NextWritePtr =:= Queue#queue.read_ptr of
        true ->
            case Queue#queue.overflow_behavior of
                backpressure ->
                    {error, queue_full};
                error ->
                    {error, queue_full};
                drop ->
                    NextReadPtr = (Queue#queue.read_ptr + 1) rem Queue#queue.capacity,
                    NewBuffer = erlang:setelement(NextWritePtr + 1, Queue#queue.buffer, Msg),
                    NewQueue = Queue#queue{
                        read_ptr = NextReadPtr,
                        write_ptr = NextWritePtr,
                        buffer = NewBuffer,
                        overflow_count = Queue#queue.overflow_count + 1
                    },
                    {ok, NewQueue}
            end;
        false ->
            NewBuffer = erlang:setelement(Queue#queue.write_ptr + 1, Queue#queue.buffer, Msg),
            NewQueue = Queue#queue{
                write_ptr = NextWritePtr,
                buffer = NewBuffer
            },
            {ok, NewQueue}
    end;

enqueue(_Msg, Queue) ->
    {error, {invalid_queue, Queue}}.

-spec dequeue(queue()) -> {ok, message(), queue()} | {error, term()}.
dequeue(Queue) when is_record(Queue, queue) ->
    case Queue#queue.read_ptr =:= Queue#queue.write_ptr of
        true ->
            {error, queue_empty};
        false ->
            Msg = erlang:element(Queue#queue.read_ptr + 1, Queue#queue.buffer),
            NewBuffer = erlang:setelement(Queue#queue.read_ptr + 1, Queue#queue.buffer, '$empty'),
            NextReadPtr = (Queue#queue.read_ptr + 1) rem Queue#queue.capacity,
            NewQueue = Queue#queue{
                read_ptr = NextReadPtr,
                buffer = NewBuffer
            },
            {ok, Msg, NewQueue}
    end;

dequeue(Queue) ->
    {error, {invalid_queue, Queue}}.

-spec depth(queue()) -> {ok, non_neg_integer()} | {error, term()}.
depth(Queue) when is_record(Queue, queue) ->
    Depth = case Queue#queue.write_ptr >= Queue#queue.read_ptr of
        true ->
            Queue#queue.write_ptr - Queue#queue.read_ptr;
        false ->
            Queue#queue.capacity - Queue#queue.read_ptr + Queue#queue.write_ptr
    end,
    {ok, Depth};

depth(Queue) ->
    {error, {invalid_queue, Queue}}.

-spec is_full(queue()) -> {ok, boolean()} | {error, term()}.
is_full(Queue) when is_record(Queue, queue) ->
    NextWritePtr = (Queue#queue.write_ptr + 1) rem Queue#queue.capacity,
    {ok, NextWritePtr =:= Queue#queue.read_ptr};

is_full(Queue) ->
    {error, {invalid_queue, Queue}}.

-spec is_empty(queue()) -> {ok, boolean()} | {error, term()}.
is_empty(Queue) when is_record(Queue, queue) ->
    {ok, Queue#queue.read_ptr =:= Queue#queue.write_ptr};

is_empty(Queue) ->
    {error, {invalid_queue, Queue}}.

-spec capacity(queue()) -> {ok, pos_integer()} | {error, term()}.
capacity(Queue) when is_record(Queue, queue) ->
    {ok, Queue#queue.capacity};

capacity(Queue) ->
    {error, {invalid_queue, Queue}}.

-spec overflow_count(queue()) -> {ok, non_neg_integer()} | {error, term()}.
overflow_count(Queue) when is_record(Queue, queue) ->
    {ok, Queue#queue.overflow_count};

overflow_count(Queue) ->
    {error, {invalid_queue, Queue}}.

-spec reset(queue()) -> {ok, queue()} | {error, term()}.
reset(Queue) when is_record(Queue, queue) ->
    NewQueue = Queue#queue{
        read_ptr = 0,
        write_ptr = 0,
        overflow_count = 0
    },
    {ok, NewQueue};

reset(Queue) ->
    {error, {invalid_queue, Queue}}.

-spec stats(queue()) -> {ok, map()} | {error, term()}.
stats(Queue) when is_record(Queue, queue) ->
    {ok, Capacity} = capacity(Queue),
    {ok, Depth} = depth(Queue),
    {ok, IsFull} = is_full(Queue),
    {ok, IsEmpty} = is_empty(Queue),
    {ok, OverflowCount} = overflow_count(Queue),

    Utilization = case Capacity of
        0 -> 0;
        _ -> round((Depth / Capacity) * 100)
    end,

    Stats = #{
        capacity => Capacity,
        depth => Depth,
        is_full => IsFull,
        is_empty => IsEmpty,
        overflow_count => OverflowCount,
        utilization_percent => Utilization,
        overflow_behavior => Queue#queue.overflow_behavior
    },
    {ok, Stats};

stats(Queue) ->
    {error, {invalid_queue, Queue}}.

-spec set_overflow_behavior(overflow_behavior(), queue()) -> {ok, queue()} | {error, term()}.
set_overflow_behavior(OverflowBehavior, Queue) when
    is_record(Queue, queue),
    (OverflowBehavior =:= drop orelse OverflowBehavior =:= backpressure orelse OverflowBehavior =:= error)
->
    NewQueue = Queue#queue{overflow_behavior = OverflowBehavior},
    {ok, NewQueue};

set_overflow_behavior(_OverflowBehavior, Queue) ->
    {error, {invalid_queue, Queue}}.

-spec to_list(queue()) -> {ok, [message()]} | {error, term()}.
to_list(Queue) when is_record(Queue, queue) ->
    Messages = case Queue#queue.read_ptr =:= Queue#queue.write_ptr of
        true ->
            [];
        false ->
            collect_messages(Queue#queue.buffer, Queue#queue.read_ptr, Queue#queue.write_ptr, Queue#queue.capacity, [])
    end,
    {ok, Messages};

to_list(Queue) ->
    {error, {invalid_queue, Queue}}.

%% Internal Functions

collect_messages(Buffer, ReadPtr, WritePtr, Capacity, Acc) ->
    case ReadPtr =:= WritePtr of
        true ->
            lists:reverse(Acc);
        false ->
            Msg = erlang:element(ReadPtr + 1, Buffer),
            NextReadPtr = (ReadPtr + 1) rem Capacity,
            collect_messages(Buffer, NextReadPtr, WritePtr, Capacity, [Msg | Acc])
    end.
