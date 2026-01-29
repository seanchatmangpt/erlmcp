%%%-------------------------------------------------------------------
%% @doc GCP Pub/Sub Simulator
%%
%% Simulates Google Cloud Pub/Sub for testing:
%% - Topic operations (create, get, list, delete)
%% - Subscription operations (create, get, list, delete)
%% - Publishing messages
%% - Pulling and acknowledging messages
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_pubsub_sim).
-behaviour(gen_server).

-include("gcp_simulator.hrl").

%% API
-export([
    start_link/0,
    stop/0
]).

%% Topic Operations
-export([
    create_topic/2,
    create_topic/3,
    get_topic/1,
    list_topics/1,
    delete_topic/1
]).

%% Subscription Operations
-export([
    create_subscription/3,
    create_subscription/4,
    get_subscription/1,
    list_subscriptions/1,
    list_subscriptions_for_topic/1,
    delete_subscription/1,
    update_subscription/2
]).

%% Message Operations
-export([
    publish/2,
    publish/3,
    pull/2,
    acknowledge/2,
    modify_ack_deadline/3,
    seek/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(pending_message, {
    received_message :: #gcp_received_message{},
    expires_at :: timestamp_ms(),
    subscription :: binary()
}).

-record(state, {
    topics = #{} :: #{binary() => #gcp_topic{}},
    subscriptions = #{} :: #{binary() => #gcp_subscription{}},
    messages = #{} :: #{binary() => [#gcp_pubsub_message{}]},  %% topic -> messages
    pending = #{} :: #{ack_id() => #pending_message{}},
    message_id_counter = 1 :: pos_integer()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?SERVER).

%% @doc Create a topic.
-spec create_topic(project_id(), binary()) ->
    {ok, #gcp_topic{}} | {error, #gcp_error{}}.
create_topic(ProjectId, TopicId) ->
    create_topic(ProjectId, TopicId, #{}).

-spec create_topic(project_id(), binary(), map()) ->
    {ok, #gcp_topic{}} | {error, #gcp_error{}}.
create_topic(ProjectId, TopicId, Opts) ->
    gen_server:call(?SERVER, {create_topic, ProjectId, TopicId, Opts}).

%% @doc Get topic.
-spec get_topic(binary()) ->
    {ok, #gcp_topic{}} | {error, #gcp_error{}}.
get_topic(TopicName) ->
    gen_server:call(?SERVER, {get_topic, TopicName}).

%% @doc List topics in a project.
-spec list_topics(project_id()) -> {ok, [#gcp_topic{}]}.
list_topics(ProjectId) ->
    gen_server:call(?SERVER, {list_topics, ProjectId}).

%% @doc Delete a topic.
-spec delete_topic(binary()) -> ok | {error, #gcp_error{}}.
delete_topic(TopicName) ->
    gen_server:call(?SERVER, {delete_topic, TopicName}).

%% @doc Create a subscription.
-spec create_subscription(project_id(), binary(), binary()) ->
    {ok, #gcp_subscription{}} | {error, #gcp_error{}}.
create_subscription(ProjectId, SubscriptionId, TopicName) ->
    create_subscription(ProjectId, SubscriptionId, TopicName, #{}).

-spec create_subscription(project_id(), binary(), binary(), map()) ->
    {ok, #gcp_subscription{}} | {error, #gcp_error{}}.
create_subscription(ProjectId, SubscriptionId, TopicName, Opts) ->
    gen_server:call(?SERVER, {create_subscription, ProjectId, SubscriptionId,
                              TopicName, Opts}).

%% @doc Get subscription.
-spec get_subscription(binary()) ->
    {ok, #gcp_subscription{}} | {error, #gcp_error{}}.
get_subscription(SubscriptionName) ->
    gen_server:call(?SERVER, {get_subscription, SubscriptionName}).

%% @doc List subscriptions in a project.
-spec list_subscriptions(project_id()) -> {ok, [#gcp_subscription{}]}.
list_subscriptions(ProjectId) ->
    gen_server:call(?SERVER, {list_subscriptions, ProjectId}).

%% @doc List subscriptions for a topic.
-spec list_subscriptions_for_topic(binary()) -> {ok, [#gcp_subscription{}]}.
list_subscriptions_for_topic(TopicName) ->
    gen_server:call(?SERVER, {list_subscriptions_for_topic, TopicName}).

%% @doc Delete a subscription.
-spec delete_subscription(binary()) -> ok | {error, #gcp_error{}}.
delete_subscription(SubscriptionName) ->
    gen_server:call(?SERVER, {delete_subscription, SubscriptionName}).

%% @doc Update subscription.
-spec update_subscription(binary(), map()) ->
    {ok, #gcp_subscription{}} | {error, #gcp_error{}}.
update_subscription(SubscriptionName, Updates) ->
    gen_server:call(?SERVER, {update_subscription, SubscriptionName, Updates}).

%% @doc Publish messages to a topic.
-spec publish(binary(), [map()]) ->
    {ok, [binary()]} | {error, #gcp_error{}}.
publish(TopicName, Messages) ->
    publish(TopicName, Messages, #{}).

-spec publish(binary(), [map()], map()) ->
    {ok, [binary()]} | {error, #gcp_error{}}.
publish(TopicName, Messages, _Opts) ->
    gen_server:call(?SERVER, {publish, TopicName, Messages}).

%% @doc Pull messages from a subscription.
-spec pull(binary(), pos_integer()) ->
    {ok, [#gcp_received_message{}]} | {error, #gcp_error{}}.
pull(SubscriptionName, MaxMessages) ->
    gen_server:call(?SERVER, {pull, SubscriptionName, MaxMessages}).

%% @doc Acknowledge messages.
-spec acknowledge(binary(), [ack_id()]) -> ok | {error, #gcp_error{}}.
acknowledge(SubscriptionName, AckIds) ->
    gen_server:call(?SERVER, {acknowledge, SubscriptionName, AckIds}).

%% @doc Modify acknowledgment deadline.
-spec modify_ack_deadline(binary(), [ack_id()], pos_integer()) ->
    ok | {error, #gcp_error{}}.
modify_ack_deadline(SubscriptionName, AckIds, DeadlineSeconds) ->
    gen_server:call(?SERVER, {modify_ack_deadline, SubscriptionName, AckIds,
                              DeadlineSeconds}).

%% @doc Seek to a timestamp or snapshot.
-spec seek(binary(), timestamp_ms() | binary()) -> ok | {error, #gcp_error{}}.
seek(SubscriptionName, Target) ->
    gen_server:call(?SERVER, {seek, SubscriptionName, Target}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Start timer for expiring acks
    erlang:send_after(1000, self(), expire_acks),
    {ok, #state{}}.

handle_call({create_topic, ProjectId, TopicId, Opts}, _From, State) ->
    TopicName = format_topic_name(ProjectId, TopicId),
    case maps:is_key(TopicName, State#state.topics) of
        true ->
            Error = #gcp_error{
                code = ?GCP_CONFLICT,
                message = <<"Topic already exists">>,
                status = <<"ALREADY_EXISTS">>
            },
            {reply, {error, Error}, State};
        false ->
            Now = erlang:system_time(millisecond),
            Topic = #gcp_topic{
                name = TopicName,
                project_id = ProjectId,
                labels = maps:get(labels, Opts, #{}),
                message_retention_duration = maps:get(retention, Opts, undefined),
                created_at = Now
            },
            NewTopics = maps:put(TopicName, Topic, State#state.topics),
            NewMessages = maps:put(TopicName, [], State#state.messages),
            NewState = State#state{topics = NewTopics, messages = NewMessages},
            {reply, {ok, Topic}, NewState}
    end;

handle_call({get_topic, TopicName}, _From, State) ->
    case maps:get(TopicName, State#state.topics, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Topic">>)}, State};
        Topic ->
            {reply, {ok, Topic}, State}
    end;

handle_call({list_topics, ProjectId}, _From, State) ->
    Topics = maps:fold(
        fun(_Name, Topic, Acc) ->
            case Topic#gcp_topic.project_id of
                ProjectId -> [Topic | Acc];
                _ -> Acc
            end
        end,
        [],
        State#state.topics
    ),
    {reply, {ok, Topics}, State};

handle_call({delete_topic, TopicName}, _From, State) ->
    case maps:get(TopicName, State#state.topics, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Topic">>)}, State};
        _Topic ->
            NewTopics = maps:remove(TopicName, State#state.topics),
            NewMessages = maps:remove(TopicName, State#state.messages),
            NewState = State#state{topics = NewTopics, messages = NewMessages},
            {reply, ok, NewState}
    end;

handle_call({create_subscription, ProjectId, SubscriptionId, TopicName, Opts},
            _From, State) ->
    SubName = format_subscription_name(ProjectId, SubscriptionId),
    case maps:is_key(SubName, State#state.subscriptions) of
        true ->
            Error = #gcp_error{
                code = ?GCP_CONFLICT,
                message = <<"Subscription already exists">>,
                status = <<"ALREADY_EXISTS">>
            },
            {reply, {error, Error}, State};
        false ->
            case maps:is_key(TopicName, State#state.topics) of
                false ->
                    {reply, {error, not_found_error(<<"Topic">>)}, State};
                true ->
                    Now = erlang:system_time(millisecond),
                    Sub = #gcp_subscription{
                        name = SubName,
                        project_id = ProjectId,
                        topic = TopicName,
                        ack_deadline_seconds = maps:get(ack_deadline, Opts, 10),
                        labels = maps:get(labels, Opts, #{}),
                        filter = maps:get(filter, Opts, undefined),
                        created_at = Now
                    },
                    NewSubs = maps:put(SubName, Sub, State#state.subscriptions),
                    {reply, {ok, Sub}, State#state{subscriptions = NewSubs}}
            end
    end;

handle_call({get_subscription, SubscriptionName}, _From, State) ->
    case maps:get(SubscriptionName, State#state.subscriptions, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Subscription">>)}, State};
        Sub ->
            {reply, {ok, Sub}, State}
    end;

handle_call({list_subscriptions, ProjectId}, _From, State) ->
    Subs = maps:fold(
        fun(_Name, Sub, Acc) ->
            case Sub#gcp_subscription.project_id of
                ProjectId -> [Sub | Acc];
                _ -> Acc
            end
        end,
        [],
        State#state.subscriptions
    ),
    {reply, {ok, Subs}, State};

handle_call({list_subscriptions_for_topic, TopicName}, _From, State) ->
    Subs = maps:fold(
        fun(_Name, Sub, Acc) ->
            case Sub#gcp_subscription.topic of
                TopicName -> [Sub | Acc];
                _ -> Acc
            end
        end,
        [],
        State#state.subscriptions
    ),
    {reply, {ok, Subs}, State};

handle_call({delete_subscription, SubscriptionName}, _From, State) ->
    case maps:get(SubscriptionName, State#state.subscriptions, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Subscription">>)}, State};
        _Sub ->
            NewSubs = maps:remove(SubscriptionName, State#state.subscriptions),
            %% Remove pending messages for this subscription
            NewPending = maps:filter(
                fun(_AckId, PM) ->
                    PM#pending_message.subscription =/= SubscriptionName
                end,
                State#state.pending
            ),
            NewState = State#state{subscriptions = NewSubs, pending = NewPending},
            {reply, ok, NewState}
    end;

handle_call({update_subscription, SubscriptionName, Updates}, _From, State) ->
    case maps:get(SubscriptionName, State#state.subscriptions, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Subscription">>)}, State};
        Sub ->
            UpdatedSub = apply_subscription_updates(Sub, Updates),
            NewSubs = maps:put(SubscriptionName, UpdatedSub, State#state.subscriptions),
            {reply, {ok, UpdatedSub}, State#state{subscriptions = NewSubs}}
    end;

handle_call({publish, TopicName, Messages}, _From, State) ->
    case maps:get(TopicName, State#state.topics, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Topic">>)}, State};
        _Topic ->
            Now = erlang:system_time(millisecond),
            {PublishedMessages, NewCounter} = lists:foldl(
                fun(MsgMap, {Acc, Counter}) ->
                    MessageId = integer_to_binary(Counter),
                    Msg = #gcp_pubsub_message{
                        message_id = MessageId,
                        data = maps:get(data, MsgMap, <<>>),
                        attributes = maps:get(attributes, MsgMap, #{}),
                        publish_time = Now,
                        ordering_key = maps:get(ordering_key, MsgMap, undefined)
                    },
                    {[Msg | Acc], Counter + 1}
                end,
                {[], State#state.message_id_counter},
                Messages
            ),
            ExistingMessages = maps:get(TopicName, State#state.messages, []),
            NewMessages = maps:put(TopicName,
                                   lists:reverse(PublishedMessages) ++ ExistingMessages,
                                   State#state.messages),
            MessageIds = [M#gcp_pubsub_message.message_id || M <- PublishedMessages],
            NewState = State#state{
                messages = NewMessages,
                message_id_counter = NewCounter
            },
            {reply, {ok, lists:reverse(MessageIds)}, NewState}
    end;

handle_call({pull, SubscriptionName, MaxMessages}, _From, State) ->
    case maps:get(SubscriptionName, State#state.subscriptions, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Subscription">>)}, State};
        Sub ->
            TopicName = Sub#gcp_subscription.topic,
            TopicMessages = maps:get(TopicName, State#state.messages, []),

            %% Get messages not already pending for this subscription
            PendingMsgIds = get_pending_message_ids(SubscriptionName, State#state.pending),
            AvailableMessages = lists:filter(
                fun(M) -> not lists:member(M#gcp_pubsub_message.message_id, PendingMsgIds) end,
                TopicMessages
            ),

            %% Take up to MaxMessages
            ToDeliver = lists:sublist(AvailableMessages, MaxMessages),

            %% Create received messages with ack IDs
            Now = erlang:system_time(millisecond),
            AckDeadline = Sub#gcp_subscription.ack_deadline_seconds * 1000,

            {ReceivedMessages, NewPending} = lists:foldl(
                fun(Msg, {RecvAcc, PendAcc}) ->
                    AckId = generate_ack_id(),
                    RecvMsg = #gcp_received_message{
                        ack_id = AckId,
                        message = Msg,
                        delivery_attempt = 1
                    },
                    PendMsg = #pending_message{
                        received_message = RecvMsg,
                        expires_at = Now + AckDeadline,
                        subscription = SubscriptionName
                    },
                    {[RecvMsg | RecvAcc], maps:put(AckId, PendMsg, PendAcc)}
                end,
                {[], State#state.pending},
                ToDeliver
            ),

            NewState = State#state{pending = NewPending},
            {reply, {ok, lists:reverse(ReceivedMessages)}, NewState}
    end;

handle_call({acknowledge, SubscriptionName, AckIds}, _From, State) ->
    case maps:get(SubscriptionName, State#state.subscriptions, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Subscription">>)}, State};
        Sub ->
            TopicName = Sub#gcp_subscription.topic,
            TopicMessages = maps:get(TopicName, State#state.messages, []),

            %% Remove acknowledged messages from topic and pending
            AckedMsgIds = lists:foldl(
                fun(AckId, Acc) ->
                    case maps:get(AckId, State#state.pending, undefined) of
                        undefined -> Acc;
                        PM ->
                            MsgId = (PM#pending_message.received_message)#gcp_received_message.message#gcp_pubsub_message.message_id,
                            [MsgId | Acc]
                    end
                end,
                [],
                AckIds
            ),

            NewTopicMessages = lists:filter(
                fun(M) -> not lists:member(M#gcp_pubsub_message.message_id, AckedMsgIds) end,
                TopicMessages
            ),

            NewMessages = maps:put(TopicName, NewTopicMessages, State#state.messages),
            NewPending = maps:without(AckIds, State#state.pending),

            NewState = State#state{messages = NewMessages, pending = NewPending},
            {reply, ok, NewState}
    end;

handle_call({modify_ack_deadline, _SubscriptionName, AckIds, DeadlineSeconds}, _From, State) ->
    Now = erlang:system_time(millisecond),
    NewExpiry = Now + (DeadlineSeconds * 1000),
    NewPending = lists:foldl(
        fun(AckId, Pending) ->
            case maps:get(AckId, Pending, undefined) of
                undefined -> Pending;
                PM -> maps:put(AckId, PM#pending_message{expires_at = NewExpiry}, Pending)
            end
        end,
        State#state.pending,
        AckIds
    ),
    {reply, ok, State#state{pending = NewPending}};

handle_call({seek, SubscriptionName, Timestamp}, _From, State) when is_integer(Timestamp) ->
    case maps:get(SubscriptionName, State#state.subscriptions, undefined) of
        undefined ->
            {reply, {error, not_found_error(<<"Subscription">>)}, State};
        _Sub ->
            %% Remove pending messages older than timestamp
            NewPending = maps:filter(
                fun(_AckId, PM) ->
                    PM#pending_message.subscription =/= SubscriptionName orelse
                    (PM#pending_message.received_message)#gcp_received_message.message#gcp_pubsub_message.publish_time >= Timestamp
                end,
                State#state.pending
            ),
            {reply, ok, State#state{pending = NewPending}}
    end;

handle_call({seek, _SubscriptionName, _Snapshot}, _From, State) ->
    %% Snapshot seek not implemented in simulator
    {reply, {error, #gcp_error{code = 501, message = <<"Snapshot seek not implemented">>,
                               status = <<"UNIMPLEMENTED">>}}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(expire_acks, State) ->
    Now = erlang:system_time(millisecond),
    %% Find expired pending messages and make them available again
    NewPending = maps:filter(
        fun(_AckId, PM) -> PM#pending_message.expires_at > Now end,
        State#state.pending
    ),
    erlang:send_after(1000, self(), expire_acks),
    {noreply, State#state{pending = NewPending}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

format_topic_name(ProjectId, TopicId) ->
    <<"projects/", ProjectId/binary, "/topics/", TopicId/binary>>.

format_subscription_name(ProjectId, SubscriptionId) ->
    <<"projects/", ProjectId/binary, "/subscriptions/", SubscriptionId/binary>>.

generate_ack_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

not_found_error(Resource) ->
    #gcp_error{
        code = ?GCP_NOT_FOUND,
        message = <<Resource/binary, " not found">>,
        status = <<"NOT_FOUND">>
    }.

get_pending_message_ids(SubscriptionName, Pending) ->
    maps:fold(
        fun(_AckId, PM, Acc) ->
            case PM#pending_message.subscription of
                SubscriptionName ->
                    MsgId = (PM#pending_message.received_message)#gcp_received_message.message#gcp_pubsub_message.message_id,
                    [MsgId | Acc];
                _ ->
                    Acc
            end
        end,
        [],
        Pending
    ).

apply_subscription_updates(Sub, Updates) ->
    Sub1 = case maps:get(ack_deadline, Updates, undefined) of
        undefined -> Sub;
        AD -> Sub#gcp_subscription{ack_deadline_seconds = AD}
    end,
    Sub2 = case maps:get(labels, Updates, undefined) of
        undefined -> Sub1;
        L -> Sub1#gcp_subscription{labels = L}
    end,
    Sub2.
