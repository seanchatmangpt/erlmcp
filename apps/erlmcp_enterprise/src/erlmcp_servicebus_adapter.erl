%% @doc Enterprise Service Bus Adapter
%% Integrates with Kafka, ESB for message routing
-module(erlmcp_servicebus_adapter).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    system :: kafka | esb,
    config :: map(),
    connection :: pid() | undefined,
    consumer :: pid() | undefined,
    producer :: pid() | undefined,
    topics :: [binary()],
    subscriptions :: map()
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    System = maps:get(system, Config),
    gen_server:start_link({local, servicebus_name(System)}, ?MODULE, [System, Config], []).

-spec stop(Ref :: pid() | atom()) -> ok.
stop(Ref) ->
    gen_server:stop(Ref).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom(), map()]) -> {ok, #state{}} | {stop, term()}.
init([System, Config]) ->
    process_flag(trap_exit, true),
    State = #state{system = System, config = Config, topics = [], subscriptions = #{}},

    %% Initialize connection and components
    case init_servicebus(System, Config) of
        {ok, Connection, Consumer, Producer} ->
            {ok, State#state{connection = Connection, consumer = Consumer, producer = Producer}};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(send_message, {From, Message}, State) ->
    case send_message(State, Message) of
        ok ->
            Metrics = update_metric(State#state.metrics, messages_sent, 1),
            {reply, ok, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(publish_message, {From, Topic, Message}, State) ->
    case publish_message(State, Topic, Message) of
        ok ->
            Metrics = update_metric(State#state.metrics, messages_published, 1),
            {reply, ok, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(subscribe, {From, Topic, GroupId}, State) ->
    case subscribe_topic(State, Topic, GroupId) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(unsubscribe, {From, Topic, GroupId}, State) ->
    case unsubscribe_topic(State, Topic, GroupId) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_topics, _From, State) ->
    {reply, {ok, State#state.topics}, State};

handle_call(get_subscriptions, _From, State) ->
    {reply, {ok, State#state.subscriptions}, State};

handle_call(get_message, {From, Timeout}, State) ->
    case receive_message(State, Timeout) of
        {ok, Message} ->
            {reply, {ok, Message}, State};
        {error, timeout} ->
            {reply, {error, timeout}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({message, Message}, State) ->
    %% Process incoming message
    case process_message(Message) of
        ok ->
            Metrics = update_metric(State#state.metrics, messages_received, 1),
            {noreply, State#state{metrics = Metrics}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to process message: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast({batch_messages, Messages}, State) ->
    %% Process batch of messages
    lists:foreach(fun(Message) ->
        case process_message(Message) of
            ok -> ok;
            {error, Reason} -> ?LOG_ERROR("Failed to process batch message: ~p", [Reason])
        end
    end, Messages),
    Metrics = update_metric(State#state.metrics, batch_messages, length(Messages)),
    {noreply, State#state{metrics = Metrics}};

handle_cast({update_config, NewConfig}, State) ->
    case reconnect_servicebus(State, NewConfig) of
        {ok, NewState} ->
            {noreply, NewState#state{config = NewConfig}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to reconnect: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info({servicebus_event, Event}, State) ->
    case handle_servicebus_event(Event, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Failed to handle event: ~p", [Reason]),
            {noreply, State}
    end;

handle_info({consumer_error, Error}, State) ->
    ?LOG_ERROR("Consumer error: ~p", [Error]),
    case recover_consumer(State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Failed to recover consumer: ~p", [Reason]),
            {noreply, State}
    end;

handle_info({producer_error, Error}, State) ->
    ?LOG_ERROR("Producer error: ~p", [Error]),
    case recover_producer(State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Failed to recover producer: ~p", [Reason]),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    %% Cleanup consumer and producer
    case State#state.consumer of
        undefined -> ok;
        Consumer -> erlmcp_servicebus_consumer:stop(Consumer)
    end,
    case State#state.producer of
        undefined -> ok;
        Producer -> erlmcp_servicebus_producer:stop(Producer)
    end,
    case State#state.connection of
        undefined -> ok;
        Connection -> erlmcp_servicebus_connection:close(Connection)
    end.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec servicebus_name(atom()) -> atom().
servicebus_name(System) ->
    list_to_atom("servicebus_" ++ atom_to_list(System) ++ "_adapter").

-spec init_servicebus(atom(), map()) -> {ok, pid(), pid(), pid()} | {error, term()}.
init_servicebus(kafka, Config) ->
    case erlmcp_kafka_connection:start(Config) of
        {ok, Connection} ->
            case erlmcp_kafka_consumer:start(Connection, Config) of
                {ok, Consumer} ->
                    case erlmcp_kafka_producer:start(Connection, Config) of
                        {ok, Producer} ->
                            {ok, Connection, Consumer, Producer};
                        {error, Reason} ->
                            erlmcp_kafka_consumer:stop(Consumer),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;

init_servicebus(esb, Config) ->
    case erlmcp_esb_connection:start(Config) of
        {ok, Connection} ->
            case erlmcp_esb_consumer:start(Connection, Config) of
                {ok, Consumer} ->
                    case erlmcp_esb_producer:start(Connection, Config) of
                        {ok, Producer} ->
                            {ok, Connection, Consumer, Producer};
                        {error, Reason} ->
                            erlmcp_esb_consumer:stop(Consumer),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec send_message(#state{}, map()) -> ok | {error, term()}.
send_message(State, Message) ->
    case State#state.system of
        kafka -> erlmcp_kafka_producer:send(State#state.producer, Message);
        esb -> erlmcp_esb_producer:send(State#state.producer, Message)
    end.

-spec publish_message(#state{}, binary(), map()) -> ok | {error, term()}.
publish_message(State, Topic, Message) ->
    case State#state.system of
        kafka -> erlmcp_kafka_producer:publish(State#state.producer, Topic, Message);
        esb -> erlmcp_esb_producer:publish(State#state.producer, Topic, Message)
    end.

-spec subscribe_topic(#state{}, binary(), binary()) -> ok | {error, term()}.
subscribe_topic(State, Topic, GroupId) ->
    case State#state.system of
        kafka -> erlmcp_kafka_consumer:subscribe(State#state.consumer, Topic, GroupId);
        esb -> erlmcp_esb_consumer:subscribe(State#state.consumer, Topic, GroupId)
    end.

-spec unsubscribe_topic(#state{}, binary(), binary()) -> ok | {error, term()}.
unsubscribe_topic(State, Topic, GroupId) ->
    case State#state.system of
        kafka -> erlmcp_kafka_consumer:unsubscribe(State#state.consumer, Topic, GroupId);
        esb -> erlmcp_esb_consumer:unsubscribe(State#state.consumer, Topic, GroupId)
    end.

-spec receive_message(#state(), integer()) -> {ok, map()} | {error, term()}.
receive_message(State, Timeout) ->
    case State#state.system of
        kafka -> erlmcp_kafka_consumer:receive(State#state.consumer, Timeout);
        esb -> erlmcp_esb_consumer:receive(State#state.consumer, Timeout)
    end.

-spec process_message(map()) -> ok | {error, term()}.
process_message(Message) ->
    %% Route message to appropriate handler
    erlmcp_servicebus_router:route(Message).

-spec handle_servicebus_event(map(), #state{}) -> {ok, #state{}} | {error, term()}.
handle_servicebus_event(Event, State) ->
    case Event of
        {topic_created, Topic} ->
            Topics = [Topic | State#state.topics],
            {ok, State#state{topics = Topics}};
        {topic_deleted, Topic} ->
            Topics = lists:delete(Topic, State#state.topics),
            {ok, State#state{topics = Topics}};
        _ ->
            {ok, State}
    end.

-spec recover_consumer(#state{}) -> {ok, #state{}} | {error, term()}.
recover_consumer(State) ->
    case State#state.consumer of
        undefined ->
            case init_servicebus_consumer(State) of
                {ok, Consumer} ->
                    {ok, State#state{consumer = Consumer}};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, consumer_not_stopped}
    end.

-spec recover_producer(#state{}) -> {ok, #state{}} | {error, term()}.
recover_producer(State) ->
    case State#state.producer of
        undefined ->
            case init_servicebus_producer(State) of
                {ok, Producer} ->
                    {ok, State#state{producer = Producer}};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, producer_not_stopped}
    end.

-spec init_servicebus_consumer(#state{}) -> {ok, pid()} | {error, term()}.
init_servicebus_consumer(State) ->
    case State#state.system of
        kafka -> erlmcp_kafka_consumer:start(State#state.connection, State#state.config);
        esb -> erlmcp_esb_consumer:start(State#state.connection, State#state.config)
    end.

-spec init_servicebus_producer(#state{}) -> {ok, pid()} | {error, term()}.
init_servicebus_producer(State) ->
    case State#state.system of
        kafka -> erlmcp_kafka_producer:start(State#state.connection, State#state.config);
        esb -> erlmcp_esb_producer:start(State#state.connection, State#state.config)
    end.

-spec reconnect_servicebus(#state{}, map()) -> {ok, #state{}} | {error, term()}.
reconnect_servicebus(State, NewConfig) ->
    %% Stop existing components
    _ = case State#state.consumer of
        undefined -> ok;
        Consumer -> erlmcp_servicebus_consumer:stop(Consumer)
    end,
    _ = case State#state.producer of
        undefined -> ok;
        Producer -> erlmcp_servicebus_producer:stop(Producer)
    end,
    _ = case State#state.connection of
        undefined -> ok;
        Connection -> erlmcp_servicebus_connection:close(Connection)
    end,

    %% Reinitialize
    case init_servicebus(State#state.system, NewConfig) of
        {ok, Connection, Consumer, Producer} ->
            {ok, State#state{connection = Connection, consumer = Consumer, producer = Producer}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec update_metric(map(), atom(), integer()) -> map().
update_metric(Metrics, Key, Inc) ->
    Current = maps:get(Key, Metrics, 0),
    Metrics#{Key => Current + Inc}.