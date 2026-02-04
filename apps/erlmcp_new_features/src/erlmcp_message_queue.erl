-module(erlmcp_message_queue).
-behaviour(gen_server).

%% API exports
-export([start_link/1, start_link/2,
         enqueue/2, enqueue/3,
         dequeue/1, dequeue/2,
         acknowledge/2,
         nack/2, nack/3,
         get_stats/1,
         stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Default configuration values (from queue.ttl ontology)
-define(DEFAULT_MAX_SIZE, 10000).
-define(DEFAULT_STORAGE_BACKEND, ets).
-define(DEFAULT_ACK_TIMEOUT_MS, 30000).
-define(DEFAULT_RETRY_BACKOFF_MS, 1000).
-define(DEFAULT_DEAD_LETTER_THRESHOLD, 5).
-define(HIBERNATE_AFTER_MS, 60000).

%%%===================================================================
%%% Types
%%%===================================================================

-type queue_name() :: binary().
-type message_id() :: binary().
-type payload() :: term().
-type priority() :: 0..9.

-type message() :: #{
    id => message_id(),
    payload => payload(),
    priority => priority(),
    attempts => non_neg_integer(),
    max_attempts => non_neg_integer(),
    created_at => integer(),
    expires_at => integer() | undefined
}.

-type delivery() :: #{
    message => message(),
    delivery_id => reference(),
    delivered_at => integer(),
    worker_pid => pid()
}.

-type storage_backend() :: memory | ets.

-type queue_config() :: #{
    max_size => non_neg_integer(),
    storage_backend => storage_backend(),
    ack_timeout_ms => pos_integer(),
    retry_backoff_ms => pos_integer(),
    dead_letter_threshold => pos_integer()
}.

-type queue_stats() :: #{
    pending => non_neg_integer(),
    delivered => non_neg_integer(),
    acknowledged => non_neg_integer(),
    dead_lettered => non_neg_integer()
}.

-record(state, {
    name :: queue_name(),
    config :: queue_config(),
    pending_queue :: queue:queue(message()) | ets:tid(),
    delivered :: #{message_id() => delivery()},
    stats :: queue_stats(),
    storage_backend :: storage_backend()
}).

-type state() :: #state{}.

-export_type([message/0, queue_config/0, queue_stats/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start a message queue with default configuration.
-spec start_link(queue_name()) -> {ok, pid()} | {error, term()}.
start_link(Name) when is_binary(Name) ->
    Config = default_config(),
    gen_server:start_link(?MODULE, [Name, Config], []).

%% @doc Start a message queue with custom configuration.
-spec start_link(queue_name(), queue_config()) -> {ok, pid()} | {error, term()}.
start_link(Name, Config) when is_binary(Name), is_map(Config) ->
    gen_server:start_link(?MODULE, [Name, Config], []).

%% @doc Enqueue a message with default priority (5).
-spec enqueue(pid(), payload()) -> {ok, message_id()} | {error, term()}.
enqueue(QueuePid, Payload) when is_pid(QueuePid) ->
    enqueue(QueuePid, Payload, 5).

%% @doc Enqueue a message with specified priority (0-9, higher = more urgent).
-spec enqueue(pid(), payload(), priority()) -> {ok, message_id()} | {error, term()}.
enqueue(QueuePid, Payload, Priority) when is_pid(QueuePid), is_integer(Priority), Priority >= 0, Priority =< 9 ->
    gen_server:call(QueuePid, {enqueue, Payload, Priority}, 5000).

%% @doc Dequeue a message for processing.
-spec dequeue(pid()) -> {ok, message_id(), payload(), reference()} | {error, empty}.
dequeue(QueuePid) when is_pid(QueuePid) ->
    dequeue(QueuePid, self()).

%% @doc Dequeue a message for a specific worker process.
-spec dequeue(pid(), pid()) -> {ok, message_id(), payload(), reference()} | {error, empty}.
dequeue(QueuePid, WorkerPid) when is_pid(QueuePid), is_pid(WorkerPid) ->
    gen_server:call(QueuePid, {dequeue, WorkerPid}, 5000).

%% @doc Acknowledge successful message processing.
-spec acknowledge(pid(), reference()) -> ok | {error, term()}.
acknowledge(QueuePid, DeliveryRef) when is_pid(QueuePid), is_reference(DeliveryRef) ->
    gen_server:call(QueuePid, {acknowledge, DeliveryRef}, 5000).

%% @doc Negative acknowledge (message processing failed).
-spec nack(pid(), reference()) -> ok | {error, term()}.
nack(QueuePid, DeliveryRef) when is_pid(QueuePid), is_reference(DeliveryRef) ->
    nack(QueuePid, DeliveryRef, false).

%% @doc Negative acknowledge with requeue option.
-spec nack(pid(), reference(), boolean()) -> ok | {error, term()}.
nack(QueuePid, DeliveryRef, Requeue) when is_pid(QueuePid), is_reference(DeliveryRef), is_boolean(Requeue) ->
    gen_server:call(QueuePid, {nack, DeliveryRef, Requeue}, 5000).

%% @doc Get queue statistics.
-spec get_stats(pid()) -> {ok, queue_stats()}.
get_stats(QueuePid) when is_pid(QueuePid) ->
    gen_server:call(QueuePid, get_stats, 5000).

%% @doc Stop the queue server.
-spec stop(pid()) -> ok.
stop(QueuePid) when is_pid(QueuePid) ->
    gen_server:stop(QueuePid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([queue_name() | queue_config()]) -> {ok, state()}.
init([Name, Config]) ->
    process_flag(trap_exit, true),

    %% Extract configuration with defaults
    MaxSize = maps:get(max_size, Config, ?DEFAULT_MAX_SIZE),
    StorageBackend = maps:get(storage_backend, Config, ?DEFAULT_STORAGE_BACKEND),
    AckTimeout = maps:get(ack_timeout_ms, Config, ?DEFAULT_ACK_TIMEOUT_MS),
    RetryBackoff = maps:get(retry_backoff_ms, Config, ?DEFAULT_RETRY_BACKOFF_MS),
    DLQThreshold = maps:get(dead_letter_threshold, Config, ?DEFAULT_DEAD_LETTER_THRESHOLD),

    %% Initialize storage based on backend type
    PendingQueue = init_storage(StorageBackend, Name),

    %% Set up ack timeout monitoring
    erlang:send_after(AckTimeout, self(), ack_timeout_check),

    State = #state{
        name = Name,
        config = #{
            max_size => MaxSize,
            storage_backend => StorageBackend,
            ack_timeout_ms => AckTimeout,
            retry_backoff_ms => RetryBackoff,
            dead_letter_threshold => DLQThreshold
        },
        pending_queue = PendingQueue,
        delivered = #{},
        stats = #{
            pending => 0,
            delivered => 0,
            acknowledged => 0,
            dead_lettered => 0
        },
        storage_backend = StorageBackend
    },

    ?LOG_INFO("Message queue ~p started (backend=~p, max_size=~p)",
              [Name, StorageBackend, MaxSize]),

    {ok, State, {hibernate_after, ?HIBERNATE_AFTER_MS}}.

handle_call({enqueue, Payload, Priority}, _From, State) ->
    #state{
        config = Config,
        pending_queue = PendingQueue,
        stats = Stats
    } = State,

    MaxSize = maps:get(max_size, Config, ?DEFAULT_MAX_SIZE),

    case queue_size(PendingQueue, State) >= MaxSize of
        true ->
            {reply, {error, queue_full}, State};
        false ->
            MessageId = generate_message_id(),
            Now = erlang:system_time(millisecond),

            Message = #{
                id => MessageId,
                payload => Payload,
                priority => Priority,
                attempts => 0,
                max_attempts => maps:get(dead_letter_threshold, Config, ?DEFAULT_DEAD_LETTER_THRESHOLD),
                created_at => Now,
                expires_at => undefined
            },

            NewPendingQueue = insert_message(Message, PendingQueue, Priority, State),

            NewState = State#state{
                pending_queue = NewPendingQueue,
                stats = Stats#{pending => maps_get_safe(pending, Stats, 0) + 1}
            },

            {reply, {ok, MessageId}, NewState}
    end;

handle_call({dequeue, WorkerPid}, _From, State) ->
    #state{
        pending_queue = PendingQueue,
        delivered = Delivered,
        stats = Stats
    } = State,

    case pop_message(PendingQueue, State) of
        {empty, _} ->
            {reply, {error, empty}, State};
        {{value, Message}, NewPendingQueue} ->
            DeliveryRef = make_ref(),
            Now = erlang:system_time(millisecond),

            Delivery = #{
                message => Message,
                delivery_id => DeliveryRef,
                delivered_at => Now,
                worker_pid => WorkerPid
            },

            MessageId = maps_get_safe(id, Message, undefined),
            NewDelivered = maps:put(MessageId, Delivery, Delivered),

            NewState = State#state{
                pending_queue = NewPendingQueue,
                delivered = NewDelivered,
                stats = Stats#{
                    pending => maps_get_safe(pending, Stats, 0) - 1,
                    delivered => maps_get_safe(delivered, Stats, 0) + 1
                }
            },

            {reply, {ok, MessageId, maps_get_safe(payload, Message, undefined), DeliveryRef}, NewState}
    end;

handle_call({acknowledge, DeliveryRef}, _From, State) ->
    #state{
        delivered = Delivered,
        stats = Stats
    } = State,

    case find_delivery_by_ref(DeliveryRef, Delivered) of
        {ok, MessageId, _Delivery} ->
            NewDelivered = maps:remove(MessageId, Delivered),
            NewState = State#state{
                delivered = NewDelivered,
                stats = Stats#{
                    delivered => maps_get_safe(delivered, Stats, 0) - 1,
                    acknowledged => maps_get_safe(acknowledged, Stats, 0) + 1
                }
            },
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({nack, DeliveryRef, Requeue}, _From, State) ->
    #state{
        pending_queue = PendingQueue,
        delivered = Delivered,
        config = Config,
        stats = Stats
    } = State,

    case find_delivery_by_ref(DeliveryRef, Delivered) of
        {ok, MessageId, Delivery} ->
            Message = maps_get_safe(message, Delivery, #{}),
            Attempts = maps_get_safe(attempts, Message, 0) + 1,
            MaxAttempts = maps_get_safe(max_attempts, Message, 5),

            NewDelivered = maps:remove(MessageId, Delivered),

            if
                Attempts >= MaxAttempts ->
                    %% Move to dead letter queue (log for now)
                    ?LOG_WARNING("Message ~p exceeded max attempts (~p), dead-lettered",
                                [MessageId, MaxAttempts]),
                    NewState = State#state{
                        delivered = NewDelivered,
                        stats = Stats#{
                            delivered => maps_get_safe(delivered, Stats, 0) - 1,
                            dead_lettered => maps_get_safe(dead_lettered, Stats, 0) + 1
                        }
                    },
                    {reply, ok, NewState};
                Requeue =:= true ->
                    %% Requeue with updated attempts
                    BackoffMs = maps_get_safe(retry_backoff_ms, Config, ?DEFAULT_RETRY_BACKOFF_MS),
                    UpdatedMessage = Message#{attempts => Attempts},
                    NewPendingQueue = insert_message_simple(UpdatedMessage, PendingQueue, State),
                    NewState = State#state{
                        pending_queue = NewPendingQueue,
                        delivered = NewDelivered
                    },
                    %% Schedule delivery after backoff
                    erlang:send_after(BackoffMs, self(), {process_backoff, MessageId}),
                    {reply, ok, NewState};
                true ->
                    %% Don't requeue, just remove
                    NewState = State#state{
                        delivered = NewDelivered,
                        stats = Stats#{
                            delivered => maps_get_safe(delivered, Stats, 0) - 1
                        }
                    },
                    {reply, ok, NewState}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_stats, _From, State) ->
    #state{
        pending_queue = PendingQueue,
        stats = Stats
    } = State,

    PendingCount = queue_size(PendingQueue, State),

    FullStats = Stats#{pending => PendingCount},

    {reply, {ok, FullStats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(ack_timeout_check, State) ->
    #state{
        delivered = Delivered,
        config = Config,
        stats = Stats
    } = State,

    AckTimeoutMs = maps_get_safe(ack_timeout_ms, Config, ?DEFAULT_ACK_TIMEOUT_MS),
    Now = erlang:system_time(millisecond),

    %% Check for timed out deliveries
    {TimeoutDeliveries, ActiveDeliveries} =
        maps:fold(fun(_MessageId, Delivery, {TimeoutAcc, ActiveAcc}) ->
            DeliveredAt = maps_get_safe(delivered_at, Delivery, 0),
            case Now - DeliveredAt >= AckTimeoutMs of
                true ->
                    {[Delivery | TimeoutAcc], ActiveAcc};
                false ->
                    {TimeoutAcc, [Delivery | ActiveAcc]}
            end
        end, {[], []}, Delivered),

    %% Requeue timed out messages
    NewState = lists:foldl(fun(Delivery, AccState) ->
        Message = maps_get_safe(message, Delivery, #{}),
        MessageId = maps_get_safe(id, Message, undefined),
        ?LOG_WARNING("Message ~p ack timeout, requeueing", [MessageId]),
        %% Send nack for timeout
        {ok, NackState} = handle_nack_internal(Delivery, true, AccState),
        NackState
    end, State, TimeoutDeliveries),

    %% Reschedule next check
    erlang:send_after(AckTimeoutMs, self(), ack_timeout_check),

    {noreply, NewState};

handle_info({process_backoff, MessageId}, State) ->
    %% Backoff timer fired, message is already in queue
    ?LOG_DEBUG("Backoff complete for message ~p", [MessageId]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{storage_backend = ets, pending_queue = Queue}) ->
    %% Clean up ETS table
    ets:delete(Queue),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Get default configuration from ontology (queue:DefaultConfig).
-spec default_config() -> queue_config().
default_config() ->
    #{
        max_size => ?DEFAULT_MAX_SIZE,
        storage_backend => ?DEFAULT_STORAGE_BACKEND,
        ack_timeout_ms => ?DEFAULT_ACK_TIMEOUT_MS,
        retry_backoff_ms => ?DEFAULT_RETRY_BACKOFF_MS,
        dead_letter_threshold => ?DEFAULT_DEAD_LETTER_THRESHOLD
    }.

%% @doc Initialize storage backend.
-spec init_storage(storage_backend(), queue_name()) -> queue:queue(_) | ets:tid().
init_storage(memory, _Name) ->
    queue:new();
init_storage(ets, Name) ->
    TableId = ets:new(queue_table_name(Name), [ordered_set, private]),
    TableId.

%% @doc Get ETS table name from queue name.
-spec queue_table_name(queue_name()) -> atom().
queue_table_name(Name) ->
    BinaryName = <<"erlmcp_queue_", Name/binary>>,
    binary_to_atom(BinaryName, utf8).

%% @doc Insert message into queue (priority-aware for ETS).
-spec insert_message(message(), queue:queue(message()) | ets:tid(), priority(), state()) ->
    queue:queue(message()) | ets:tid().
insert_message(Message, PendingQueue, _Priority, #state{storage_backend = memory}) ->
    %% Simple FIFO for in-memory (queue module doesn't support priority)
    queue:in(Message, PendingQueue);
insert_message(Message, PendingQueue, Priority, #state{storage_backend = ets}) ->
    %% Priority-based insert for ETS (use key = {Priority, Timestamp})
    Now = erlang:system_time(millisecond),
    Key = {10 - Priority, Now},  % Invert priority so higher = first
    ets:insert(PendingQueue, {Key, Message}),
    PendingQueue.

%% @doc Simple message insert without priority (for requeue).
-spec insert_message_simple(message(), queue:queue(message()) | ets:tid(), state()) ->
    queue:queue(message()) | ets:tid().
insert_message_simple(Message, PendingQueue, #state{storage_backend = memory}) ->
    queue:in(Message, PendingQueue);
insert_message_simple(Message, PendingQueue, #state{storage_backend = ets}) ->
    Priority = maps_get_safe(priority, Message, 5),
    Now = erlang:system_time(millisecond),
    Key = {10 - Priority, Now},
    ets:insert(PendingQueue, {Key, Message}),
    PendingQueue.

%% @doc Pop message from queue.
-spec pop_message(queue:queue(message()) | ets:tid(), state()) ->
    {{value, message()}, queue:queue(message()) | ets:tid()} | {empty, queue:queue(message()) | ets:tid()}.
pop_message(Queue, #state{storage_backend = memory}) ->
    queue:out(Queue);
pop_message(Queue, #state{storage_backend = ets}) ->
    case ets:first(Queue) of
        '$end_of_table' ->
            {empty, Queue};
        Key ->
            [{Key, Message}] = ets:lookup(Queue, Key),
            ets:delete(Queue, Key),
            {{value, Message}, Queue}
    end.

%% @doc Get queue size.
-spec queue_size(queue:queue(_) | ets:tid(), state()) -> non_neg_integer().
queue_size(Queue, #state{storage_backend = memory}) ->
    queue:len(Queue);
queue_size(Queue, #state{storage_backend = ets}) ->
    ets:info(Queue, size).

%% @doc Find delivery by reference.
-spec find_delivery_by_ref(reference(), #{message_id() => delivery()}) ->
    {ok, message_id(), delivery()} | error.
find_delivery_by_ref(DeliveryRef, Delivered) ->
    try
        maps:fold(fun(MessageId, Delivery, _Acc) ->
            case maps_get_safe(delivery_id, Delivery, undefined) of
                DeliveryRef ->
                    throw({found, MessageId, Delivery});
                _ ->
                    ok
            end
        end, ok, Delivered),
        error
    catch
        throw:{found, MessageId, Delivery} ->
            {ok, MessageId, Delivery};
        _:_ ->
            error
    end.

%% @doc Handle nack internally (helper function).
-spec handle_nack_internal(delivery(), boolean(), state()) -> {ok, state()}.
handle_nack_internal(Delivery, Requeue, State) ->
    #state{
        pending_queue = PendingQueue,
        delivered = Delivered,
        config = Config,
        stats = Stats
    } = State,

    Message = maps_get_safe(message, Delivery, #{}),
    MessageId = maps_get_safe(id, Message, undefined),
    Attempts = maps_get_safe(attempts, Message, 0) + 1,
    MaxAttempts = maps_get_safe(max_attempts, Message, 5),

    NewDelivered = maps:remove(MessageId, Delivered),

    if
        Attempts >= MaxAttempts ->
            %% Dead letter
            {ok, State#state{
                delivered = NewDelivered,
                stats = Stats#{
                    delivered => maps_get_safe(delivered, Stats, 0) - 1,
                    dead_lettered => maps_get_safe(dead_lettered, Stats, 0) + 1
                }
            }};
        Requeue =:= true ->
            %% Requeue
            UpdatedMessage = Message#{attempts => Attempts},
            NewPendingQueue = insert_message_simple(UpdatedMessage, PendingQueue, State),
            {ok, State#state{
                pending_queue = NewPendingQueue,
                delivered = NewDelivered
            }};
        true ->
            %% Just remove
            {ok, State#state{
                delivered = NewDelivered,
                stats = Stats#{
                    delivered => maps_get_safe(delivered, Stats, 0) - 1
                }
            }}
    end.

%% @doc Safely get value from map with default.
-spec maps_get_safe(term(), map(), term()) -> term().
maps_get_safe(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.

%% @doc Generate unique message ID.
-spec generate_message_id() -> message_id().
generate_message_id() ->
    UniqueInt = erlang:unique_integer([positive]),
    <<UniqueInt:64/integer-unsigned-little, (erlang:timestamp()):64/integer-unsigned-little>>.
