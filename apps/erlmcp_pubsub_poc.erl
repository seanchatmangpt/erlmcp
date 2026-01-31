%% @doc PubSub POC - Publish/Subscribe Pattern
%%
%% Proof-of-concept publish/subscribe system with fanout messaging,
%% topic-based routing, and automatic cleanup on subscriber death.
%%
%% API:
%% - start_link/0: Start the pubsub server
%% - subscribe/3: Subscribe a process to a topic
%% - publish/3: Publish a message to a topic
%% - unsubscribe/2: Unsubscribe from a topic
%% - get_subscribers/2: Get all subscribers for a topic
%% - stop/1: Stop the server
-module(erlmcp_pubsub_poc).
-behaviour(gen_server).

%% API
-export([start_link/0, subscribe/3, publish/3, unsubscribe/2, get_subscribers/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    topics :: map()  %% Topic -> [SubscriberPid]
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the pubsub server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Subscribe a process to a topic
-spec subscribe(pid(), term(), pid()) -> ok.
subscribe(ServerPid, Topic, SubscriberPid) ->
    gen_server:call(ServerPid, {subscribe, Topic, SubscriberPid}).

%% @doc Publish a message to a topic
-spec publish(pid(), term(), term()) -> ok.
publish(ServerPid, Topic, Message) ->
    gen_server:cast(ServerPid, {publish, Topic, Message}).

%% @doc Unsubscribe from a topic
-spec unsubscribe(pid(), term()) -> ok.
unsubscribe(ServerPid, Topic) ->
    gen_server:call(ServerPid, {unsubscribe, Topic, self()}).

%% @doc Get all subscribers for a topic
-spec get_subscribers(pid(), term()) -> {ok, [pid()]}.
get_subscribers(ServerPid, Topic) ->
    gen_server:call(ServerPid, {get_subscribers, Topic}).

%% @doc Stop the server
-spec stop(pid()) -> ok.
stop(ServerPid) ->
    gen_server:stop(ServerPid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{topics = #{}}}.

handle_call({subscribe, Topic, SubscriberPid}, _From, State = #state{topics = Topics}) ->
    %% Monitor the subscriber
    MonitorRef = monitor(process, SubscriberPid),
    %% Add to subscribers list
    CurrentSubscribers = maps:get(Topic, Topics, []),
    %% Store with monitor ref
    NewSubscribers = [{SubscriberPid, MonitorRef} | CurrentSubscribers],
    NewTopics = maps:put(Topic, NewSubscribers, Topics),
    {reply, ok, State#state{topics = NewTopics}};

handle_call({unsubscribe, Topic, SubscriberPid}, _From, State = #state{topics = Topics}) ->
    case maps:get(Topic, Topics, undefined) of
        undefined ->
            {reply, ok, State};
        Subscribers ->
            %% Find and remove subscriber
            NewSubscribers = lists:filter(fun({Pid, _Ref}) ->
                Pid =/= SubscriberPid
            end, Subscribers),
            NewTopics = case NewSubscribers of
                [] -> maps:remove(Topic, Topics);
                _ -> maps:put(Topic, NewSubscribers, Topics)
            end,
            {reply, ok, State#state{topics = NewTopics}}
    end;

handle_call({get_subscribers, Topic}, _From, State = #state{topics = Topics}) ->
    Subscribers = case maps:get(Topic, Topics, undefined) of
        undefined -> [];
        SubList -> [Pid || {Pid, _Ref} <- SubList]
    end,
    {reply, {ok, Subscribers}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({publish, Topic, Message}, State = #state{topics = Topics}) ->
    case maps:get(Topic, Topics, undefined) of
        undefined ->
            ok;
        Subscribers ->
            %% Send message to all subscribers
            lists:foreach(fun({SubscriberPid, _Ref}) ->
                SubscriberPid ! {pubsub_message, Topic, Message}
            end, Subscribers)
    end,
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, Pid, _Reason}, State = #state{topics = Topics}) ->
    %% Subscriber died - cleanup from all topics
    NewTopics = maps:map(fun(_Topic, Subscribers) ->
        lists:filter(fun({SubPid, SubRef}) ->
            SubPid =/= Pid andalso SubRef =/= MonitorRef
        end, Subscribers)
    end, Topics),
    %% Remove empty topics
    NewTopicsFiltered = maps:filter(fun(_Topic, Subscribers) ->
        Subscribers =/= []
    end, NewTopics),
    {noreply, State#state{topics = NewTopicsFiltered}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
